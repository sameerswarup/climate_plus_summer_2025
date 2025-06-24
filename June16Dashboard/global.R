# global.R - Using dedicated polygon datasets
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(tidyverse)
library(qs)
library(pryr)
library(rnaturalearth)

print(">>> global.R is running <<<")

# Load the original point-level data
df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

# Load the pre-created polygon datasets (run create_polygon_dataset.R first!)
country_polygons_with_data <- tryCatch({
  readRDS("data/country_polygons_with_data.rds")
}, error = function(e) {
  stop("Please run create_polygon_dataset.R first to create the polygon datasets!")
})

country_centroids_with_data <- tryCatch({
  readRDS("data/country_centroids_with_data.rds") 
}, error = function(e) {
  stop("Please run create_polygon_dataset.R first to create the centroid datasets!")
})

# Load additional data
countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))

print(paste("Loaded data for", nrow(country_polygons_with_data), "countries"))

# Now we have perfectly aligned datasets:
# - df: original point-level data for detailed country analysis
# - country_polygons_with_data: country-level polygons with aggregated data
# - country_centroids_with_data: country-level centroids with aggregated data

# Set up the main datasets for the app
combined_scores_global <- country_centroids_with_data  # Use centroids for markers
combined_scores_global_polygons <- country_polygons_with_data  # Use polygons for choropleth

average_country_nogeo <- country_centroids_with_data  # Use centroids for markers  
average_country_polygons <- country_polygons_with_data  # Use polygons for choropleth

# Define indicators and choices
indicator_map <- list(
  "Socio-Ecological Vulnerability" = "vulnerab.score.rank",
  "Social Inequality" = "ineq.score.rank",
  "Weak Governance" = "gov.score.rank"
)

composite_choices <- names(indicator_map)
indicator_choices <- composite_choices
composite_arith_list <- c("vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")
indicator_arith_map <- indicator_map

# Indicator descriptions
indicator_descriptions <- list(
  "Socio-Ecological Vulnerability" = "Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels.",
  "Social Inequality" = "Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups.",
  "Weak Governance" = "Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation in decision-making."
)

# Helper function for flag paths
findPNGpath <- function(name_en, countryCodes) {
  pngDefaultPath <- "www/flags/"
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull(Alpha.2.code)
  
  if (length(alpha2) == 0) return("www/globe.png")  # Fallback
  
  alpha2 <- substring(alpha2, 3, 4)
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  return(pngFinal)
}

# Global level choices for analysis
global_level_choices <- c(
  "Nutritional Dependence" = "Nutritional.dependence.sc",
  "Economic Dependence" = "Economic.dependence.sc",
  "Low Voice and Accountability" = "Voice_account.sc", 
  "Political Instability" = "Political_stab.sc",
  "Government Ineffectiveness" = "Gov_effect.sc",
  "Poor Regulatory Quality" = "Reg_quality.sc",
  "Weak Rule of Law" = "Rule_law.sc",
  "Weak Control of Corruption" = "control_corr.sc",
  "Gender Inequality" = "gender.ineq.sc",
  "Income Inequality" = "income.ineq.sc",
  "Inequality Adjusted Life Expectancy" = "le.ineq.log.sc"
)

# Load data descriptions with fallback
inequity_data_descriptions <- tryCatch({
  read.csv("data/inequity_data_descriptions.csv")
}, error = function(e) {
  data.frame(
    variable_name = c(
      "Nutritional.dependence.sc", "Economic.dependence.sc", "Voice_account.sc",
      "Political_stab.sc", "Gov_effect.sc", "Reg_quality.sc", "Rule_law.sc", 
      "control_corr.sc", "gender.ineq.sc", "income.ineq.sc", "le.ineq.log.sc",
      "distance_to_coast_km", "mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc",
      "perc.pop.world.coastal.merit.10m.log.sc"
    ),
    description = c(
      "Nutritional dependence on marine resources",
      "Economic dependence on marine sectors",
      "Voice and accountability in governance", 
      "Political stability and absence of violence",
      "Government effectiveness",
      "Regulatory quality",
      "Rule of law",
      "Control of corruption",
      "Gender inequality index",
      "Income inequality measure", 
      "Life expectancy inequality",
      "Distance from coast in kilometers",
      "Degraded ecosystem indicator",
      "Relative deprivation index",
      "Coastal vulnerability percentage"
    )
  )
})

# Create country centroids for zooming (extract coordinates)
country_centroids <- country_centroids_with_data %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(COUNTRY = country_centroids_with_data$COUNTRY) %>%
  select(COUNTRY, X, Y)

composite_score_list <- c("vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")

# Define global variables for backward compatibility
use_polygons <- TRUE

# Legacy function for backward compatibility (though not needed anymore)
aggregate_country <- function(data, use_polygons = TRUE) {
  if (use_polygons) {
    return(country_polygons_with_data)
  } else {
    return(country_centroids_with_data)
  }
}

# Print diagnostic information
print("=== DATA LOADING SUMMARY ===")
print(paste("Point-level data (df):", nrow(df), "rows"))
print(paste("Country polygons:", nrow(country_polygons_with_data), "countries"))
print(paste("Country centroids:", nrow(country_centroids_with_data), "countries")) 
print(paste("Countries for zooming:", nrow(country_centroids), "countries"))

# Verify data quality
complete_composite_scores <- country_polygons_with_data %>%
  st_drop_geometry() %>%
  filter(!is.na(vulnerab.score.rank) & !is.na(ineq.score.rank) & !is.na(gov.score.rank)) %>%
  nrow()

print(paste("Countries with complete composite scores:", complete_composite_scores))

# Check for any missing critical columns
required_columns <- c("COUNTRY", "vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")
missing_columns <- required_columns[!required_columns %in% names(country_polygons_with_data)]

if (length(missing_columns) > 0) {
  warning(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
} else {
  print("All required columns present ✓")
}

print("global.R completed successfully!")
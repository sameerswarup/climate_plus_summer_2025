# global.R - Improved Version
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
print(list.files())

# Load main data first
df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

# Check if df already contains proper country polygons
if ("COUNTRY" %in% names(df)) {
  # Get unique countries from your actual data
  data_countries <- unique(df$COUNTRY)
  print(paste("Countries in data:", length(data_countries)))
}

# Load external polygon data as backup
country_polygons_external <- ne_countries(scale = "medium", returnclass = "sf")

# Create a name mapping function to handle country name differences
fix_country_names <- function(country_name) {
  name_fix <- c(
    "United States"    = "United States of America",
    "México"           = "Mexico", 
    "Côte d'Ivoire"    = "Ivory Coast",
    "Tanzania"         = "United Republic of Tanzania",
    "Timor-Leste"      = "East Timor",
    "Congo"            = "Republic of the Congo",
    "Democratic Republic of the Congo" = "Democratic Republic of the Congo"
  )
  
  ifelse(country_name %in% names(name_fix), name_fix[country_name], country_name)
}

# Check if your data already has country-level polygons
has_country_polygons <- df %>% 
  group_by(COUNTRY) %>% 
  summarise(
    unique_geoms = n_distinct(st_as_text(geometry)),
    total_points = n()
  ) %>%
  filter(unique_geoms == 1 & total_points > 1) %>%
  nrow() > 0

if (has_country_polygons) {
  print("Data appears to contain country-level polygons")
  # Use the polygons from your data
  country_polygons <- df %>%
    group_by(COUNTRY) %>%
    slice(1) %>%
    select(COUNTRY, geometry) %>%
    ungroup()
} else {
  print("Data contains point-level data, using external polygons")
  # Use external polygons and match with your data
  country_polygons <- country_polygons_external %>%
    mutate(COUNTRY_FIXED = fix_country_names(admin)) %>%
    filter(COUNTRY_FIXED %in% data_countries) %>%
    select(COUNTRY = COUNTRY_FIXED, geometry = geometry)
}

# Create centroids for point data
country_centroids_sf <- country_polygons %>%
  mutate(geometry = st_centroid(geometry))

# Load additional data
countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))

# Define global variables
use_polygons <- TRUE

# Function to create country-aggregated datasets
aggregate_country <- function(data, use_polygons = TRUE) {
  # Aggregate data by country
  aggregated_data <- data %>%
    st_drop_geometry() %>%  # Remove geometry temporarily
    filter(!is.na(COUNTRY)) %>%
    mutate(COUNTRY = fix_country_names(COUNTRY)) %>%
    group_by(COUNTRY) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = 'drop')
  
  # Add appropriate geometry
  if (use_polygons) {
    result <- aggregated_data %>%
      left_join(
        country_polygons %>% st_drop_geometry() %>% mutate(polygon_geom = country_polygons$geometry),
        by = "COUNTRY"
      ) %>%
      filter(!is.na(polygon_geom)) %>%
      st_sf(geometry = .$polygon_geom)
  } else {
    result <- aggregated_data %>%
      left_join(
        country_centroids_sf %>% st_drop_geometry() %>% mutate(centroid_geom = country_centroids_sf$geometry),
        by = "COUNTRY"  
      ) %>%
      filter(!is.na(centroid_geom)) %>%
      st_sf(geometry = .$centroid_geom)
  }
  
  return(result)
}

# Create combined scores data - properly aggregated
combined_scores <- df %>%
  st_drop_geometry() %>%
  filter(!is.na(COUNTRY)) %>%
  mutate(COUNTRY = fix_country_names(COUNTRY)) %>%
  group_by(COUNTRY) %>%
  summarise(
    vulnerab.score.rank = mean(vulnerab.score.rank, na.rm = TRUE),
    ineq.score.rank = mean(ineq.score.rank, na.rm = TRUE), 
    gov.score.rank = mean(gov.score.rank, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    country_centroids_sf %>% st_drop_geometry() %>% mutate(geom = country_centroids_sf$geometry),
    by = "COUNTRY"
  ) %>%
  filter(!is.na(geom)) %>%
  st_sf(geometry = .$geom)

combined_scores_global <- combined_scores

# Create polygon version
combined_scores_global_polygons <- df %>%
  st_drop_geometry() %>%
  filter(!is.na(COUNTRY)) %>%
  mutate(COUNTRY = fix_country_names(COUNTRY)) %>%
  group_by(COUNTRY) %>%
  summarise(
    vulnerab.score.rank = mean(vulnerab.score.rank, na.rm = TRUE),
    ineq.score.rank = mean(ineq.score.rank, na.rm = TRUE),
    gov.score.rank = mean(gov.score.rank, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    country_polygons %>% st_drop_geometry() %>% mutate(poly_geom = country_polygons$geometry),
    by = "COUNTRY"
  ) %>%
  filter(!is.na(poly_geom)) %>%
  st_sf(geometry = .$poly_geom)

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

# Create average country data with proper geometry handling
average_country_nogeo <- df %>%
  st_drop_geometry() %>%
  filter(!is.na(COUNTRY)) %>%
  mutate(COUNTRY = fix_country_names(COUNTRY)) %>%
  group_by(COUNTRY) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
  left_join(
    country_centroids_sf %>% st_drop_geometry() %>% mutate(geom = country_centroids_sf$geometry),
    by = "COUNTRY"
  ) %>%
  filter(!is.na(geom)) %>%
  st_sf(geometry = .$geom)

# Create polygon version for average_country_nogeo
average_country_polygons <- df %>%
  st_drop_geometry() %>%
  filter(!is.na(COUNTRY)) %>%
  mutate(COUNTRY = fix_country_names(COUNTRY)) %>%
  group_by(COUNTRY) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = 'drop') %>%
  left_join(
    country_polygons %>% st_drop_geometry() %>% mutate(poly_geom = country_polygons$geometry),
    by = "COUNTRY"
  ) %>%
  filter(!is.na(poly_geom)) %>%
  st_sf(geometry = .$poly_geom)

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

# Create country centroids for zooming
country_centroids <- country_centroids_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(COUNTRY = country_centroids_sf$COUNTRY)

composite_score_list <- c("vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")

# Print diagnostic information
print(paste("Total countries in polygon data:", nrow(country_polygons)))
print(paste("Total countries in centroid data:", nrow(country_centroids_sf)))
print(paste("Countries with complete combined scores:", nrow(combined_scores_global)))
print(paste("Countries with complete average data:", nrow(average_country_nogeo)))
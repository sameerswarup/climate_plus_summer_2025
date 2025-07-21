# global.R
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

country_polygons <- ne_countries(scale = "medium", returnclass = "sf")

country_centroids_sf <- country_polygons %>%
  select(admin, geometry) %>%
  mutate(geometry = st_centroid(geometry))




df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

df_country <- df %>%
  mutate(
    geometry = country_centroids_sf[match(COUNTRY, country_centroids_sf$admin), ]$geometry
  )

countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))

# Load country polygons - these will be used for borders and highlighting

# Create centroids for point data

# Load original data
# gov <- readRDS("data/governance_scores.rds")
# ineq <- readRDS("data/inequality_scores.rds")
# eco <- readRDS("data/ecological_scores.rds")
# dep <- readRDS("data/deprivation_scores.rds")
# exp <- readRDS("data/exposure_scores.rds")
# # 
# gov <- gov %>% slice_sample(n = 10000)
# ineq <- ineq %>% slice_sample(n = 10000)
# eco <- eco %>% slice_sample(n = 10000)
# dep <- dep %>% slice_sample(n = 10000)
# exp <- exp %>% slice_sample(n = 10000)

# Function to create country-aggregated datasets with centroid geometries
aggregate_country <- function(data) {
  
  name_fix <- c(
    "United States"    = "United States of America",
    "México"           = "Mexico",
    "Côte d'Ivoire"    = "Ivory Coast",
    "Tanzania"         = "United Republic of Tanzania",
    "Timor-Leste"      = "East Timor"
  )
  
  data <- data %>%
    filter(!is.na(COUNTRY), !st_is_empty(geometry)) %>%
    mutate(
      COUNTRY_fixed = ifelse(COUNTRY %in% names(name_fix), name_fix[COUNTRY], COUNTRY)
    ) %>%
    group_by(COUNTRY_fixed) %>%
    summarise(across(ends_with("_arith") | ends_with("_geom"), \(x) mean(x, na.rm = TRUE))) %>%
    ungroup() %>%
    rename(COUNTRY = COUNTRY_fixed)
  
  # Choose geometry type based on parameter
  if (use_polygons) {
    data <- data %>%
      mutate(
        geometry = country_polygons$geometry[match(COUNTRY, country_polygons$admin)]
      ) %>%
      st_as_sf()
  } else {
    data <- data %>%
      mutate(
        geometry = country_centroids_sf$geometry[match(COUNTRY, country_centroids_sf$admin)]
      ) %>%
      st_as_sf()
  }
  
  return(data)
}



# Create polygon version for combined_scores_global - ensure this exists
combined_scores_global_polygons <- tryCatch({
  aggregate_country(combined_scores, use_polygons = TRUE)
}, error = function(e) {
  # Fallback: create manually if aggregate_country fails
  combined_scores_global %>%
    mutate(
      geometry = country_polygons$geometry[match(COUNTRY, country_polygons$admin)]
    ) %>%
    st_as_sf()
})

indicator_map <- list(
  "Socio-Ecological Vulnerability" = "vulnerab.score.rank",
  "Social Inequality" = "ineq.score.rank",
  "Weak Governance" = "gov.score.rank"
)


# mean_type_suffix <- list(
#   "Arithmetic Mean" = "_arith",
#   "Geometric Mean" = "_geom"
# )

composite_choices <- names(indicator_map)

# Indicator descriptions
indicator_descriptions <- list(
  "Socio-Ecological Vulnerability" = "Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels.",
  "Social Inequality" = "Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups.",
  "Weak Governance" = "Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation in decision-making."
  
)

# FROM ETHAN'S FILES


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

# GLOBAL-LEVEL VARIABLES (UNCHANGING)
global_level_variables <- names(df)[8:20]

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

average_country_nogeo <- df |>
  group_by(iso_a3.x) |>
  summarize(
    COUNTRY = first(COUNTRY),
    name_en = first(name_en),
    across(5:24, ~mean(.x, na.rm = TRUE))
  )

average_country_nogeo <- average_country_nogeo %>%
  mutate(
    geometry = country_centroids_sf[match(COUNTRY, country_centroids_sf$admin), ]$geometry
  )

# Create polygon version for average_country_nogeo - ensure this exists
average_country_polygons <- tryCatch({
  average_country_nogeo %>%
    mutate(
      geometry = country_polygons$geometry[match(COUNTRY, country_polygons$admin)]
    ) %>%
    st_as_sf()
}, error = function(e) {
  # Fallback to centroids if polygons fail
  average_country_nogeo %>% st_as_sf()
})

inequity_data_descriptions <- read.csv("data/inequity_data_descriptions.csv")

country_centroids <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()
country_centroids$COUNTRY <- ne_countries(scale = "medium", returnclass = "sf")$admin

composite_score_list <- c("vulnerab.score.rank",
                          "ineq.score.rank",
                          "gov.score.rank")

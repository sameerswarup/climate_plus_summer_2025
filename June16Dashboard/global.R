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


# Load the original point-level data
df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

# Load the pre-created polygon datasets
country_polygons_with_data <- readRDS("data/country_polygons_with_data.rds")
country_centroids_with_data <- readRDS("data/country_centroids_with_data.rds")

# Load additional data
countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))

# Set up the main datasets for the app
combined_scores_global <- country_centroids_with_data
combined_scores_global_polygons <- country_polygons_with_data
average_country_nogeo <- country_centroids_with_data
average_country_polygons <- country_polygons_with_data

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
  
  if (length(alpha2) == 0) return("www/globe.png")
  
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

# Load data descriptions
inequity_data_descriptions <- read.csv("data/inequity_data_descriptions.csv")

# Create country centroids for zooming
country_centroids <- country_centroids_with_data %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(COUNTRY = country_centroids_with_data$COUNTRY) %>%
  select(COUNTRY, X, Y)

composite_score_list <- c("vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")

# Define global variables
use_polygons <- TRUE

# Legacy function for backward compatibility
aggregate_country <- function(data, use_polygons = TRUE) {
  if (use_polygons) {
    return(country_polygons_with_data)
  } else {
    return(country_centroids_with_data)
  }
}
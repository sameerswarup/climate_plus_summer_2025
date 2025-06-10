library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(tidyverse)
library(qs)
library(rnaturalearth)

country_polygons <- ne_countries(scale = "medium", returnclass = "sf")
country_centroids_sf <- country_polygons %>%
  select(admin, geometry) %>%
  mutate(geometry = st_centroid(geometry))  # Geometric center of polygon

# Load original data
gov <- readRDS("data/governance_scores.rds")
ineq <- readRDS("data/inequality_scores.rds")
eco <- readRDS("data/ecological_scores.rds")
dep <- readRDS("data/deprivation_scores.rds")
exp <- readRDS("data/exposure_scores.rds")

# Subset for performance
gov <- gov %>% slice_sample(n = 200000)
ineq <- ineq %>% slice_sample(n = 200000)
eco <- eco %>% slice_sample(n = 200000)
dep <- dep %>% slice_sample(n = 200000)
exp <- exp %>% slice_sample(n = 200000)

# Function to create country-aggregated datasets with centroid geometries
aggregate_country <- function(data) {
  data %>%
    filter(!is.na(COUNTRY), !st_is_empty(geometry)) %>%
    group_by(COUNTRY) %>%
    summarise(across(ends_with("_arith") | ends_with("_geom"), mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      geometry = country_centroids_sf[match(COUNTRY, country_centroids_sf$admin), ]$geometry
    ) %>%
    st_as_sf()
}

# Create both full and global (aggregated) datasets
gov_global <- aggregate_country(gov)
ineq_global <- aggregate_country(ineq)
eco_global <- aggregate_country(eco)
dep_global <- aggregate_country(dep)
exp_global <- aggregate_country(exp)

data_list <- list(
  "Governance" = list(full = gov, global = gov_global),
  "Inequality" = list(full = ineq, global = ineq_global),
  "Ecological" = list(full = eco, global = eco_global),
  "Deprivation" = list(full = dep, global = dep_global),
  "Exposure" = list(full = exp, global = exp_global)
)

indicator_prefix_map <- c(
  "Governance" = "gov",
  "Inequality" = "ineq",
  "Ecological" = "eco",
  "Deprivation" = "dep",
  "Exposure" = "exp"
)

mean_type_suffix <- c(
  "Arithmetic Mean" = "_arith",
  "Geometric Mean" = "_geom"
)

indicator_choices <- names(data_list)
mean_choices <- names(mean_type_suffix)
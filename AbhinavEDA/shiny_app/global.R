# global.R
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(tidyverse)
library(qs)

print(">>> global.R is running <<<")
print(list.files())

# Load original data
gov <- readRDS("data/governance_scores.rds")
ineq <- readRDS("data/inequality_scores.rds")
eco <- readRDS("data/ecological_scores.rds")
dep <- readRDS("data/deprivation_scores.rds")
exp <- readRDS("data/exposure_scores.rds")

# Sample smaller subsets for testing
gov <- gov %>% slice_sample(n = 40000)
ineq <- ineq %>% slice_sample(n = 40000)
eco <- eco %>% slice_sample(n = 40000)
dep <- dep %>% slice_sample(n = 40000)
exp <- exp %>% slice_sample(n = 40000)

data_list <- list(
  "Governance" = gov,
  "Inequality" = ineq,
  "Ecological" = eco,
  "Deprivation" = dep,
  "Exposure" = exp
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

# library(shiny)
# library(leaflet)
# library(dplyr)
# library(sf)
# library(viridis)
# library(tidyverse)
# 
# print(">>> global.R is running <<<")
# print(list.files("data"))

# # Load full datasets and simplify geometries in memory
# gov <- st_simplify(readRDS("data/governance_scores.rds"), dTolerance = 0.25, preserveTopology = TRUE)
# ineq <- st_simplify(readRDS("data/inequality_scores.rds"), dTolerance = 0.25, preserveTopology = TRUE)
# eco  <- st_simplify(readRDS("data/ecological_scores.rds"), dTolerance = 0.25, preserveTopology = TRUE)
# dep  <- st_simplify(readRDS("data/deprivation_scores.rds"), dTolerance = 0.25, preserveTopology = TRUE)
# exp  <- st_simplify(readRDS("data/exposure_scores.rds"), dTolerance = 0.25, preserveTopology = TRUE)
# 
# # Combine into list for use in server
# data_list <- list(
#   "Governance" = gov,
#   "Inequality" = ineq,
#   "Ecological" = eco,
#   "Deprivation" = dep,
#   "Exposure" = exp
# )
# 
# # Define input options
# mean_type_suffix <- c(
#   "Arithmetic Mean" = "_arith",
#   "Geometric Mean" = "_geom",
#   "Harmonic Mean" = "_harm"
# )
# 
# indicator_choices <- names(data_list)
# mean_choices <- names(mean_type_suffix)


# library(shiny)
# library(leaflet)
# library(dplyr)
# library(sf)
# library(viridis)
# library(tidyverse)
# library(qs)
# 
# print(">>> global.R is running <<<")
# print(list.files())
# 
# # Load original data
# gov <- readRDS("data/governance_scores.rds") 
# ineq <- readRDS("data/inequality_scores.rds") 
# eco <- readRDS("data/ecological_scores.rds")
# dep <- readRDS("data/deprivation_scores.rds")
# exp <- readRDS("data/exposure_scores.rds")
# 
# # Function to group by COUNTRY, average indicators, and compute a centroid
# aggregate_country <- function(data) {
#   data <- data %>%
#     filter(!is.na(COUNTRY), !st_is_empty(geometry)) %>%
#     group_by(COUNTRY) %>%
#     summarise(across(ends_with("_arith") | ends_with("_geom") | ends_with("_harm"), mean, na.rm = TRUE),
#               geometry = st_centroid(st_union(geometry))) %>%
#     ungroup() %>%
#     st_as_sf()
#   
#   return(data)
# }
# 
# # Apply aggregation to each dataset
# gov <- aggregate_country(gov)
# ineq <- aggregate_country(ineq)
# eco <- aggregate_country(eco)
# dep <- aggregate_country(dep)
# exp <- aggregate_country(exp)
# 
# # Combine into list for use in server
# data_list <- list(
#   "Governance" = gov,
#   "Inequality" = ineq,
#   "Ecological" = eco,
#   "Deprivation" = dep,
#   "Exposure" = exp
# )
# 
# # Define input options
# mean_type_suffix <- c(
#   "Arithmetic Mean" = "_arith",
#   "Geometric Mean" = "_geom"
# )
# 
# indicator_choices <- names(data_list)
# mean_choices <- names(mean_type_suffix)


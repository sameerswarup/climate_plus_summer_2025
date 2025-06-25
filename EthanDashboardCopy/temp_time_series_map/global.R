# global.R

library(tidyverse)
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)

# Load the filtered points data from 1990 to 2025

wide <- readRDS("data/wide_global_temp_anomaly_1990_2025 copy.rds")

# column names

wideCol <- setdiff(colnames(wide), "geometry")
date_labels <- format(as.Date(wideCol, format = "%Y-%m-%d"), "%b %Y")
names(wideCol) <- date_labels

# Extract all month columns from wide as a vector
all_values <- unlist(wide[ , wideCol], use.names = FALSE)

# Remove NA if any
all_values <- all_values[!is.na(all_values)]

# Calculate min and max
min_val <- min(all_values)
max_val <- max(all_values)

wide <- wide %>%
  mutate(
    geom_id = as.character(st_as_text(geometry))  # WKT string
  )

long <- readRDS("data/long_global_temp_anomaly_1990_2025 copy.rds")
long <- long %>%
  mutate(
    geom_id = as.character(st_as_text(geometry))  # WKT string
  )

# ND Gain Data
gain <- readRDS("data/filteredNDGainData copy.rds")
  country_names <- unique(gain$Name)

# ND Gain Columns
  
gainVars <- list(
  "Projected Change of Biome Distribution" = "Value..ecos_01_score",
  "Projected Change of Marine Biodiversity" = "Value..ecos_02_score",
  "Projected Change of Warm Periods" = "Value..habi_01_score",
  "Projected Change of Deaths from Climate Change Induced Diseases" = "Value..heal_01_score",
  "Projected Change in Vector-Borne Diseases" = "Value..heal_02_score",
  "Dependency on External Resource for Health Services" = "Value..heal_03_score",
  "Medical Staff" = "Value..heal_05_score",
  "Projected Change of Sea Level Rise Impacts" = "Value..infr_02_score",
  "Population Living Under 5m Above Sea Level" = "Value..infr_04_score",
  "Economic Readiness" = "Value..economic",
  "Ecosystem Vulnerability" = "Value..ecosystems",
  "Food Vulnerability" = "Value..food",
  "Governance Readiness" = "Value..governance",
  "GAIN Score" = "Value..gain",
  "Health Vulnerability" = "Value..health",
  "Infrastructure Vulnerability" = "Value..infrastructure"
)
gainVarsNames <- names(gainVars)

# Get min_val and max_val of all values across time


ndNamedCols <- unlist(gainVars, use.names = FALSE)

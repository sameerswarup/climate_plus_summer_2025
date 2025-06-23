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


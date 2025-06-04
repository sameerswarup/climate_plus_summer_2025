library(readxl)
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rnaturalearth)
library(scales)
library(DT)
library(tidyr)
# Joining GPWV4 and Inequity for Vietnam

# Read GPW Data
csv_folder <- "/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files"
csv_files <- list.files(path = csv_folder, pattern = "\\.csv$", full.names = TRUE)
gpw <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)
gpw <- st_as_sf(gpw,
                      coords = c("CENTROID_X", "CENTROID_Y"),
                      crs = 4326)

# Read LECZ Data
lecz <- read_excel("/Users/student/Desktop/Data+/climate_plus_summer_2025/Data/Coastal Climate Vulnerability/LECZ_Data/lecz-v3.xlsm", sheet = "Raw-Combined-Data")

# Filter to Only Vietnam
lecz_vnm <- filter(lecz, ISO3 == "VNM")
gpw_vnm <- filter(gpw, ISOALPHA == "VNM")


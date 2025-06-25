library(shiny)
library(leaflet)
library(terra)
library(bslib)

# Expanded climate variable options
climate_data_options <- list(
  "Ocean pH" = list(
    "Change (pH)" = list(
      "Near Term (2021–2040)" = "../IPCC_data/pH_change_near_term.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/pH_change_medium_term.tif",
      "Long Term (2081–2100)" = "../IPCC_data/pH_change_long_term.tif"
    ),
    "Value (pH)" = list(
      "Near Term (2021–2040)" = "../IPCC_data/pH_value_near_term.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/pH_value_medium_term.tif",
      "Long Term (2081–2100)" = "../IPCC_data/pH_value_long_term.tif"
    )
  ),
  "Sea Level Rise" = list(
    "Change (meters)" = list(
      "Near Term (2021–2040)" = "../IPCC_data/SLR_near_term.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/SLR_medium_term.tif",
      "Long Term (2081–2100)" = "../IPCC_data/SLR_long_term.tif"
    )
  ),
  "Heating Degree Days" = list(
    "Degree Days" = list(
      "Near Term (2021–2040)" = "../IPCC_data/HDD_near_term.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/HDD_medium_term.tif",
      "Long Term (2081–2100)" = "../IPCC_data/HDD_long_term.tif"
    )
  )
)

# Variable metadata for display
variable_metadata <- list(
  "Ocean pH" = list(
    unit = "pH units",
    description = "Ocean acidity levels",
    baseline = "N/A",
    color_palette = "viridis"
  ),
  "Sea Level Rise" = list(
    unit = "meters",
    description = "Sea level change",
    baseline = "1995-2014",
    color_palette = "plasma"
  ),
  "Heating Degree Days" = list(
    unit = "degree days",
    description = "Heating degree days",
    baseline = "1995-2014",
    color_palette = "inferno"
  )
)
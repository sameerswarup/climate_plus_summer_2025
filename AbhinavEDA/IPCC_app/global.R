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
  
  "Coral Bleaching Heat" = list(
    "Degree Days" = list(
      "1-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250401.tif",
      "2-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250402.tif",
      "3-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250403.tif",
      "4-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250404.tif",
      "5-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250405.tif",
      "6-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250406.tif",
      "7-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250407.tif",
      "8-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250408.tif",
      "9-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250409.tif",
      "10-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250410.tif",
      "11-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250411.tif",
      "12-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250412.tif",
      "13-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250413.tif",
      "14-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250414.tif",
      "15-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250415.tif",
      "16-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250416.tif",
      "17-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250417.tif",
      "18-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250418.tif",
      "19-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250419.tif",
      "20-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250420.tif",
      "21-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250421.tif",
      "22-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250422.tif",
      "23-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250423.tif",
      "24-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250424.tif",
      "25-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250425.tif",
      "26-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250426.tif",
      "27-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250427.tif",
      "28-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250428.tif",
      "29-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250429.tif",
      "30-April-2025" = "../Coral_Bleaching_data/ct5km_dhw_v3.1_20250430.tif"
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
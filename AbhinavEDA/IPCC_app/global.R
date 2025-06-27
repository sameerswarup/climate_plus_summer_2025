library(shiny)
library(leaflet)
library(terra)
library(bslib)

# Composite score options

composite_data_options <- list(
  "Climate Risk" = climate_data_options,
  "ND Gain" = gainVars
)

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
    "Degree Heating Weeks" = list(
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
    "Change" = list(
      "Near Term (2021–2040)" = "../IPCC_data/DH_Near_Term_Change.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/DH_Medium_Term_Change.tif",
      "Long Term (2081–2100)" = "../IPCC_data/DH_Long_Term_Change.tif"
    ),
    "Value" = list(
      "Near Term (2021–2040)" = "../IPCC_data/DH_Near_Term_Value.tif",
      "Medium Term (2041–2060)" = "../IPCC_data/DH_Medium_Term_Value.tif",
      "Long Term (2081–2100)" = "../IPCC_data/DH_Long_Term_Value.tif"
    )
  )
)

# Variable metadata for display
variable_metadata <- list(
  "Ocean pH" = list(
    unit = "pH units",
    description = "Ocean Acidity Levels",
    baseline = "1850-1900",
    color_palette = "viridis"
  ),
  "Sea Level Rise" = list(
    unit = "meters",
    description = "Sea Level Change",
    baseline = "1995-2014",
    color_palette = "viridis"
  ),
  "Heating Degree Days" = list(
    "Change" = list(
      unit = "Degree Days",
      description = "Heating Degree Days Change from Baseline",
      baseline = "1850-1900",
      color_palette = "viridis"
    ),
    "Value" = list(
      unit = "degree days",
      description = "Heating Degree Days Value",
      baseline = "1995–2014",
      color_palette = "viridis"
    )
  ),
  "Coral Bleaching Heat" = list(
    unit = "Degree Heating Weeks",
    description = "Coral Bleaching Heat Stress Index",
    baseline = "Rolling 12-week window",
    color_palette = "viridis"
  )
)

# ND Gain Data
gain <- readRDS("data/gain_coastal_filtered.rds")
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
  "GAIN" = "Value..gain",
  "Health Vulnerability" = "Value..health",
  "Infrastructure Vulnerability" = "Value..infrastructure"
)
gainVarsNames <- names(gainVars)

# Get min_val and max_val of all values across time

ndNamedCols <- unlist(gainVars, use.names = FALSE)

# Indicator Descriptions

ndGainDescriptions <- read.csv("data/ndgain_indicator_descriptions.csv")
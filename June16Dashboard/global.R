# global.R - Simplified version
library(shiny)
library(leaflet)
library(terra)
library(bslib)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(tidyverse)
library(qs)
library(pryr)
library(rnaturalearth)
library(shinyjs)
library(ggthemes)
library(plotly)

# Load the main datasets
df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

country_polygons_with_data <- readRDS("data/country_polygons_with_data.rds")
country_centroids_with_data <- readRDS("data/country_centroids_with_data.rds")

df_regional <- readRDS("data/regional_scores_of_inequity_filtered_5k.rds")

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
composite_arith_list <- c("vulnerab.score.rank", "ineq.score.rank", "gov.score.rank")

# ND GAIN Columns

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

# Indicator descriptions
indicator_descriptions <- list(
  "Socio-Ecological Vulnerability" = "Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels.",
  "Social Inequality" = "Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups.",
  "Weak Governance" = "Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation in decision-making."
)

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

# Global level choices for analysis
global_level_choices <- list(
  "Inequity" = list(
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
  ),
  "ND GAIN" = gainVars
  
)

global_level_name_key_value <- c(
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
  "Infrastructure Vulnerability" = "Value..infrastructure",
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

# Create country centroids for zooming (filter out invalid coordinates)
country_centroids <- country_centroids_with_data %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(COUNTRY = country_centroids_with_data$COUNTRY) %>%
  select(COUNTRY, X, Y) %>%
  filter(!is.na(X) & !is.na(Y) & is.finite(X) & is.finite(Y))

inequity_data_descriptions <- read.csv("data/inequity_data_descriptions.csv")

indicator_choice_list <- list(
  "Socio-Ecological Vulnerability" = c("Socio-Ecological Vulnerability (Composite)" = "vulnerab.score.rank",
                                       "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                       "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                       "Coastal Climate Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc",
                                       "Nutritional Dependence" = "Nutritional.dependence.sc"),
  "Social Inequality" = c("Social Inequality (Composite)" = "ineq.score.rank",
                          "Gender Inequality" = "gender.ineq.sc",
                          "Income Inequality" = "income.ineq.sc",
                          "Inequality Adjusted Life Expectancy" = "le.ineq.log.sc"),
  "Weak Governance" = c("Weak Governance (Composite)" = "gov.score.rank",
                        "Government Ineffectiveness" = "Gov_effect.sc",
                        "Poor Regulatory Quality" = "Reg_quality.sc",
                        "Weak Rule of Law" = "Rule_law.sc",
                        "Weak Control of Corruption" = "control_corr.sc",
                        "Low Voice and Accountability" = "Voice_account.sc",
                        "Political Instability" = "Political_stab.sc")
)


# IPCC and ND GAIN

world_sf <- ne_countries(scale = "medium", returnclass = "sf")

world_sf <- world_sf[world_sf$continent != "Antarctica", ]


# Expanded climate variable options
climate_data_options <- list(
  
  "Ocean pH" = list(
    "Change (pH)" = list(
      "Near Term (2021–2040)" = "data/IPCC_data/pH_change_near_term.tif",
      "Medium Term (2041–2060)" = "data/IPCC_data/pH_change_medium_term.tif",
      "Long Term (2081–2100)" = "data/IPCC_data/pH_change_long_term.tif"
    ),
    "Value (pH)" = list(
      "Near Term (2021–2040)" = "data/IPCC_data/pH_value_near_term.tif",
      "Medium Term (2041–2060)" = "data/IPCC_data/pH_value_medium_term.tif",
      "Long Term (2081–2100)" = "data/IPCC_data/pH_value_long_term.tif"
    )
  ),
  
  "Coral Bleaching Heat" = list(
    "Degree Heating Weeks" = list(
      "1-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250401.tif",
      "2-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250402.tif",
      "3-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250403.tif",
      "4-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250404.tif",
      "5-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250405.tif",
      "6-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250406.tif",
      "7-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250407.tif",
      "8-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250408.tif",
      "9-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250409.tif",
      "10-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250410.tif",
      "11-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250411.tif",
      "12-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250412.tif",
      "13-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250413.tif",
      "14-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250414.tif",
      "15-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250415.tif",
      "16-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250416.tif",
      "17-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250417.tif",
      "18-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250418.tif",
      "19-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250419.tif",
      "20-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250420.tif",
      "21-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250421.tif",
      "22-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250422.tif",
      "23-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250423.tif",
      "24-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250424.tif",
      "25-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250425.tif",
      "26-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250426.tif",
      "27-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250427.tif",
      "28-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250428.tif",
      "29-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250429.tif",
      "30-April-2025" = "data/Coral_Bleaching_data/ct5km_dhw_v3.1_20250430.tif"
    )
  ),
  "Sea Level Rise" = list(
    "Change (meters)" = list(
      "Near Term (2021–2040)" = "data/IPCC_data/SLR_near_term.tif",
      "Medium Term (2041–2060)" = "data/IPCC_data/SLR_medium_term.tif",
      "Long Term (2081–2100)" = "data/IPCC_data/SLR_long_term.tif"
    )
  ),
  
  "Heating Degree Days" = list(
    "Change" = list(
      "Near Term (2021–2040)" = "data/IPCC_data/DH_Near_Term_Change.tif",
      "Medium Term (2041–2060)" = "data/IPCC_data/DH_Medium_Term_Change.tif",
      "Long Term (2081–2100)" = "data/IPCC_data/DH_Long_Term_Change.tif"
    ),
    "Value" = list(
      "Near Term (2021–2040)" = "data/IPCC_data/DH_Near_Term_Value.tif",
      "Medium Term (2041–2060)" = "data/IPCC_data/DH_Medium_Term_Value.tif",
      "Long Term (2081–2100)" = "data/IPCC_data/DH_Long_Term_Value.tif"
    )
  )
)

# Composite score options

composite_data_options <- list(
  "Inequity" = c("Socio-Ecological Vulnerability (Composite)" = "vulnerab.score.rank",
                 "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                 "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                 "Coastal Climate Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc",
                 "Nutritional Dependence" = "Nutritional.dependence.sc"),
  "Climate Risk" = names(climate_data_options),
  "ND GAIN" = gainVars
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
    unit = "Meters",
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
      unit = "Degree days",
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

# ND GAIN Data
gain <- readRDS("data/gain_coastal_filtered.rds")

acn_country_iso <- average_country_nogeo %>%
  select(COUNTRY, iso_a3) %>%
  st_drop_geometry()

gain <- gain %>%
  left_join(acn_country_iso, by = c("ISO3" = "iso_a3")) %>%
  mutate(Name = COUNTRY) %>%   # Replace Name with the joined COUNTRY
  select(-COUNTRY)

country_names <- unique(gain$Name)
gainVarsNames <- names(gainVars)

# Get min_val and max_val of all values across time

ndNamedCols <- unlist(gainVars, use.names = FALSE)

# Indicator Descriptions

ndGainDescriptions <- read.csv("data/ndgain_indicator_descriptions.csv")

# Icons
ndGainIcons <- list(
  "Projected Change of Biome Distribution" = "mountain-sun",
  "Projected Change of Marine Biodiversity" = "fish",
  "Projected Change of Warm Periods" = "temperature-high",
  "Projected Change of Deaths from Climate Change Induced Diseases" = "disease",
  "Projected Change in Vector-Borne Diseases" = "square-virus",
  "Dependency on External Resource for Health Services" = "kit-medical",
  "Medical Staff" = "user-nurse",
  "Projected Change of Sea Level Rise Impacts" = "water",
  "Population Living Under 5m Above Sea Level" = "people-group",
  "Economic Readiness" = "money-bill-transfer",
  "Ecosystem Vulnerability" = "seedling",
  "Food Vulnerability" = "wheat-awn-circle-exclamation",
  "Governance Readiness" = "person-chalkboard",
  "GAIN" = "tree",
  "Health Vulnerability" = "virus-covid",
  "Infrastructure Vulnerability" = "building-circle-exclamation"
)

gain_wide_points <- readRDS("data/gain_wide_points.rds")

world_average <- average_country_nogeo %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

gain_wide_points <- gain_wide_points %>%
  left_join(acn_country_iso, by = c("iso_a3.x" = "iso_a3")) %>%
  mutate(Name = COUNTRY) %>%
  select(-COUNTRY)
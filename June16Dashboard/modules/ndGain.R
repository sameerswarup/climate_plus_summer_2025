# ND Gain Server File

world_sf <- ne_countries(scale = "medium", returnclass = "sf")

world_sf <- world_sf[world_sf$continent != "Antarctica", ]

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

# ND Gain Data
gain <- readRDS("data/gain_coastal_filtered.rds")
country_names <- unique(gain$Name)
gainVarsNames <- names(gainVars)

# Get min_val and max_val of all values across time

ndNamedCols <- unlist(gainVars, use.names = FALSE)

# Indicator Descriptions

ndGainDescriptions <- read.csv("data/ndgain_indicator_descriptions.csv")
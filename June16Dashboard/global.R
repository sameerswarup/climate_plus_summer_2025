# global.R
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(tidyverse)
library(qs)
library(pryr)
library(rnaturalearth)


print(">>> global.R is running <<<")
print(list.files())


countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))


#regionCodes <- suppressWarnings(readRDS("/Users/student/Desktop/regions 1.rds"))


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

# Sample smaller subsets for testing

gov <- gov %>% slice_sample(n = 10000)
ineq <- ineq %>% slice_sample(n = 10000)
eco <- eco %>% slice_sample(n = 10000)
dep <- dep %>% slice_sample(n = 10000)
exp <- exp %>% slice_sample(n = 10000)

# Function to create country-aggregated datasets with centroid geometries
aggregate_country <- function(data) {
  name_fix <- c(
    "United States"    = "United States of America",
    "México"           = "Mexico",
    "Côte d'Ivoire"    = "Ivory Coast",
    "Tanzania"         = "United Republic of Tanzania",
    "Timor-Leste"      = "East Timor"
  )
  
  data <- data %>%
    filter(!is.na(COUNTRY), !st_is_empty(geometry)) %>%
    mutate(
      COUNTRY_fixed = ifelse(COUNTRY %in% names(name_fix), name_fix[COUNTRY], COUNTRY)
    ) %>%
    group_by(COUNTRY_fixed) %>%
    summarise(across(ends_with("_arith") | ends_with("_geom"), mean, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(COUNTRY = COUNTRY_fixed) %>%  # <-- THIS LINE restores the column name
    mutate(
      geometry = country_centroids_sf$geometry[match(COUNTRY, country_centroids_sf$admin)]
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
  "Governance Weakness" = list(full = gov, global = gov_global),
  "Social Inequality Risk" = list(full = ineq, global = ineq_global),
  "Ecological Risk" = list(full = eco, global = eco_global),
  "Deprivation Risk" = list(full = dep, global = dep_global),
  "Exposure Risk" = list(full = exp, global = exp_global)
)

indicator_prefix_map <- list(
  "Governance Weakness" = "gov",
  "Social Inequality Risk" = "ineq",
  "Ecological Risk" = "eco",
  "Deprivation Risk" = "dep",
  "Exposure Risk" = "exp"
)

indicator_arith_map <- list(
  "Governance Weakness" = "gov_arith",
  "Social Inequality Risk" = "ineq_arith",
  "Ecological Risk" = "eco_arith",
  "Deprivation Risk" = "dep_arith",
  "Exposure Risk" = "exp_arith"
)


mean_type_suffix <- list(
  "Arithmetic Mean" = "_arith",
  "Geometric Mean" = "_geom"
)

indicator_choices <- names(data_list)

mean_choices <- names(mean_type_suffix)

# Indicator descriptions
indicator_descriptions <- list(
  "Governance Weakness" = "Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation in decision-making.",
  "Social Inequality Risk" = "Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups.",
  "Ecological Risk" = "Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels.",
  "Deprivation Risk" = "Measures poverty levels through multiple factors including child welfare, infant health, education and living standards, infrastructure development, and economic opportunities.",
  "Exposure Risk" = "Measures coastal population vulnerability to climate impacts, specifically the proportion of populations in low-elevation coastal zones within 10 meters of sea level facing sea-level rise exposure."
)

# -----------------------------------------------------------------------------


# FROM ETHAN'S FILES

df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

df_country <- df %>%
  mutate(
      geometry = country_centroids_sf[match(COUNTRY, country_centroids_sf$admin), ]$geometry
    )
  

# df is now inequity_filtered5k.rds which is smaller

findPNGpath <- function(name_en) {
  pngDefaultPath <- "www/flags/"
  countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull(Alpha.2.code)
  alpha2<- substring(alpha2, 3, 4)
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  return(pngFinal)
}

# -----------------------------------------------------------------------------

# GLOBAL-LEVEL VARIABLES (UNCHANGING)

global_level_variables <- names(df)[8:20]

global_level_choices <- c(
  "Nutritional Dependence" = "Nutritional.dependence.sc" ,
  "Economic Dependence" = "Economic.dependence.sc"    ,
  "Low Voice and Accountability" = "Voice_account.sc"        ,  
  "Political Instability" = "Political_stab.sc"     ,   
  "Government Ineffectiveness" = "Gov_effect.sc"          ,   
  "Poor Regulatory Quality" = "Reg_quality.sc"          ,  
  "Weak Rule of Law" = "Rule_law.sc"              , 
  "Weak Control of Corruption" = "control_corr.sc"          ,
  "Gender Inequality" = "gender.ineq.sc"            ,
  "Income Inequality" = "income.ineq.sc"            ,
  "Inequality Adjusted Life Expectancy" = "le.ineq.log.sc"            
  
)

average_country_nogeo <- df |>
  group_by(iso_a3.x) |>
  summarize (
    COUNTRY = first(COUNTRY),
    name_en = first(name_en),
    across(5:24, ~mean(.x, na.rm = TRUE))
  )

average_country_nogeo <- average_country_nogeo %>%
  mutate(
    geometry = country_centroids_sf[match(COUNTRY, country_centroids_sf$admin), ]$geometry
  )



inequity_data_descriptions <- read.csv("data/inequity_data_descriptions.csv")

country_centroids <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()
country_centroids$COUNTRY <- ne_countries(scale = "medium", returnclass = "sf")$admin

composite_score_list <- c("governance_composite", "inequality_composite", "ecological_composite", "deprivation_composite", "exposure_composite")
composite_arith_list <- c("gov_arith", "ineq_arith", "eco_arith", "dep_arith", "exp_arith")
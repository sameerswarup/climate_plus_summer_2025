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

countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))

regionCodes <- suppressWarnings(readRDS("/Users/student/Downloads/regions 1.rds"))

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

indicator_prefix_map <- c(
  "Governance" = "gov",
  "Inequality" = "ineq",
  "Ecological" = "eco",
  "Deprivation" = "dep",
  "Exposure" = "exp"
)

data_list <- list(
  "Governance" = gov,
  "Inequality" = ineq,
  "Ecological" = eco,
  "Deprivation" = dep,
  "Exposure" = exp
)

mean_type_suffix <- c(
  "Arithmetic Mean" = "_arith",
  "Geometric Mean" = "_geom"
)

indicator_choices <- names(data_list)

mean_choices <- names(mean_type_suffix)


# -----------------------------------------------------------------------------


# FROM ETHAN'S FILES

df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

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
    name_en = first(name_en),
    across(5:24, ~mean(.x, na.rm = TRUE))
  )

average_country_nogeo <- select(average_country_nogeo, iso_a3.x, name_en, all_of(global_level_variables))

inequity_data_descriptions <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/June16Dashboard/data/inequity_data_descriptions.csv")
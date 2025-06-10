library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(data.table)

df <- readRDS("data copy/gpw_inequity_average copy.rds") %>%
  st_transform(4326)


df_population <- df |>
  st_drop_geometry() |>
  group_by(COUNTRYNM) |>
  summarise(
    population = sum(UN_2020_E),
    ISOALPHA = first(ISOALPHA)
  )

world <- ne_countries(returnclass = "sf")
world_joined <- world |>
  left_join(df_population, by = c("adm0_a3" = "ISOALPHA"))

findPNGpath <- function(name_en) {
  pngDefaultPath <- "www/flags/"
  countryCodes <- suppressWarnings(read.csv("data copy/countries_codes_and_coordinates.csv"))
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

global_level_variables <- names(df)[104:116]

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
  group_by(ISOALPHA) |>
  summarize (
    COUNTRYNM = first(COUNTRYNM),
    across(99:119, ~mean(.x, na.rm = TRUE))
  )

average_country_nogeo <- select(average_country_nogeo, all_of(global_level_variables))

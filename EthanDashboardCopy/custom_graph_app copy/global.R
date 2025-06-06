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
  pngDefaultPath <- "flags/"
  countryCodes <- suppressWarnings(read.csv("data copy/countries_codes_and_coordinates.csv"))
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull(Alpha.2.code)
  alpha2<- substring(alpha2, 3, 4)
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  return(pngFinal)
}
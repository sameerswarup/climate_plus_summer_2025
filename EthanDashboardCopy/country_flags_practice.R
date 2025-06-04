library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(bslib)


findPNGpath <- function(name_en) {
  pngDefaultPath <- "../data copy/160x120/"
  countryCodes <- suppressWarnings(read.csv("/Users/student/Desktop/Data+/Shiny R Learning/PracticePages/data copy/countries_codes_and_coordinates.csv"))
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull(Alpha.2.code)
  alpha2<- substring(alpha2, 2)
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  return(pngFinal)
}


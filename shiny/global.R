library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)

df <- readRDS("data/inequity_filtered5k.rds") %>%
  st_transform(4326)

regionCodes <- suppressWarnings(readRDS("data/regions.rds"))

inequity_data_descriptions <- read.csv("data/inequity_data_descriptions.csv")

countryCodes <- suppressWarnings(read.csv("data/countries_codes_and_coordinates.csv"))


df_country <- df |>
  st_drop_geometry() |>
  group_by(name_en) |>
  summarise(
    governance_score = mean(c(
      Voice_account.sc, Gov_effect.sc, Reg_quality.sc,
      Rule_law.sc, control_corr.sc, Political_stab.sc
    ), na.rm = TRUE),
    
    inequality_score = mean(c(
      gender.ineq.sc, income.ineq.sc, le.ineq.log.sc,
      ineq.score.rank, hierachical.score.rank.ineq
    ), na.rm = TRUE),
    
    economic_dependence_score = mean(c(
      Nutritional.dependence.sc, Economic.dependence.sc,
      income.ineq.change.sc, le.ineq.change.sc
    ), na.rm = TRUE),
    
    coastal_exposure_score = mean(c(
      mean.count.grav.V2.log.sc, povmap.grdi.v1.sc,
      perc.pop.world.coastal.merit.10m.log.sc, vulnerab.score.rank
    ), na.rm = TRUE)
  )

world <- ne_countries(returnclass = "sf")
world_joined <- world |>
  left_join(df_country, by = c("name" = "name_en"))





findPNGpath <- function(name_en) {
  pngDefaultPath <- "data/flags/flags/"
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull("Alpha-2 code")
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  print(pngFinal)
  return(pngFinal)
}


bounding_boxes <- do.call(rbind, lapply(1:nrow(world), function(i) {
  bb <- st_bbox(world[i, ])
  data.frame(
    name = world$admin[i],
    xmin = bb["xmin"],
    ymin = bb["ymin"],
    xmax = bb["xmax"],
    ymax = bb["ymax"]
  )
}))

#
#csv_folder <- "../misc/Filtered_Files"

#csv_files <- list.files(path = csv_folder, pattern = "\\.csv$", full.names = TRUE)

#worldData <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

#worldData <- st_as_sf(worldData,
#                      coords = c("CENTROID_X", "CENTROID_Y"),
#                      crs = 4326)
#worldData_pop_2020 <- dplyr::select(worldData, "ISOALPHA", "COUNTRYNM", 
#                               "NAME1", "NAME2", "NAME3", "NAME4",
#                               "NAME5", "NAME6", 
#                               "UN_2020_E")
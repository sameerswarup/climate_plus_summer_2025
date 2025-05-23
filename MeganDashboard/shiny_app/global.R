library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)

df <- readRDS("../data/df.cont.inequity.compo.coastal.scores_sr.rds") |>
  st_transform(4326)

df_country <- df |>
  st_drop_geometry() |>
  group_by(name_en) |>
  summarise(
    mean_ineq = mean(ineq.score.rank, na.rm = TRUE),
    mean_gov = mean(Gov_effect.sc, na.rm = TRUE),
    mean_voice = mean(Voice_account.sc, na.rm = TRUE)
  )

world <- ne_countries(returnclass = "sf")
world_joined <- world |>
  left_join(df_country, by = c("name" = "name_en"))

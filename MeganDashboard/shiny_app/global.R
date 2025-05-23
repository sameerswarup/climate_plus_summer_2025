library(shiny)
library(leaflet)
library(dplyr)
library(sf)

df <- readRDS("../data/df.cont.inequity.compo.coastal.scores_sr.rds")
df <- sf::st_transform(df, crs = 4326)

set.seed(42)  
df_sample <- df |> sample_n(1000)
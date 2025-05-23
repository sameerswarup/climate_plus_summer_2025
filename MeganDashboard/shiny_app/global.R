library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)

df <- readRDS("../data/df.cont.inequity.compo.coastal.scores_sr.rds") %>%
  st_transform(4326)

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

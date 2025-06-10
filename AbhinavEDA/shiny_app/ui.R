library(shiny)
library(leaflet)
library(bslib)

ui <- page_sidebar(
  title = "Inequity Indicators Map",
  sidebar = sidebar(
    width = 400,
    tags$h2("Inequity Indicators Map"),
    selectInput("indicator_category", "Choose Indicator Category:",
                choices = indicator_choices, selected = "Governance"),
    selectInput("mean_type", "Choose Mean Type:",
                choices = mean_choices, selected = "Arithmetic Mean"),
    selectizeInput("country_search", "Jump to Country:", choices = NULL, selected = NULL),
    actionButton("zoom_button", "Zoom to Selected Country")
  ),
  leafletOutput("map", height = 600)
)


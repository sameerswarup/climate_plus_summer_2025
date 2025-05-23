library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Megan's Coastal Map"),
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -122.4, lat = 47.6, zoom = 10)
  })
}

shinyApp(ui, server)
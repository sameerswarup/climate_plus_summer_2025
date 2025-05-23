server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # OpenStreetMap basemap
      setView(lng = 0, lat = 0, zoom = 2)  # Centered at the equator
  })
}

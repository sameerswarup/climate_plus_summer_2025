server <- function(input, output, session) {
  
  pal_reactive <- reactive({
    colorNumeric("YlOrRd", world_joined[[input$score_type]], na.color = "transparent")
  })
  
  output$map <- renderLeaflet({
    pal <- pal_reactive()
    
    leaflet(data = world_joined) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(world_joined[[input$score_type]]),
        weight = 1,
        color = "grey",
        fillOpacity = 0.8,
        label = ~paste0(
          name, ": ", round(world_joined[[input$score_type]], 2)
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal, values = ~world_joined[[input$score_type]],
        title = input$score_type
      )
  })
}

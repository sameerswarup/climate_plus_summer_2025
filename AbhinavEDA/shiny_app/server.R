# server.R
server <- function(input, output, session) {
  selected_data <- reactive({
    req(input$indicator_category)
    data_list[[input$indicator_category]]
  })
  
  selected_var <- reactive({
    paste0(tolower(substr(input$indicator_category, 1, 3)),
           mean_type_suffix[[input$mean_type]])
  })
  
  output$map <- renderLeaflet({
    data <- selected_data()
    var <- selected_var()
    
    req(var %in% colnames(data))
    
    pal <- colorNumeric("viridis", domain = data[[var]], na.color = "transparent")
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "white",
        weight = 0.5,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3))
      ) %>%
      addLegend(
        pal = pal,
        values = data[[var]],
        opacity = 0.8,
        title = paste(input$indicator_category, "(", input$mean_type, ")"),
        position = "bottomright"
      )
  })
}

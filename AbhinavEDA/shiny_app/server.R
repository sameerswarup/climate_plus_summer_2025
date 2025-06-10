server <- function(input, output, session) {
  # Reactive to get the selected dataset from the list
  selected_data <- reactive({
    req(input$indicator_category)
    data_list[[input$indicator_category]]
  })
  
  # Reactive to get the correct variable name for the selected dataset and mean type
  selected_var <- reactive({
    req(input$indicator_category, input$mean_type)
    prefix <- indicator_prefix_map[[input$indicator_category]]
    paste0(prefix, mean_type_suffix[[input$mean_type]])
  })
  
  # Render Leaflet map based on user selection
  output$map <- renderLeaflet({
    data <- selected_data()
    var <- selected_var()
    
    # Confirm the variable exists in the data
    req(var %in% colnames(data))
    
    # Define color palette
    pal <- colorNumeric("viridis", domain = data[[var]], na.color = "transparent")
    
    # Build map
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
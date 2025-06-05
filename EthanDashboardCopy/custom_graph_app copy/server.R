server <- function(input, output, session) {
  
  
  chosen_country <- reactiveVal(NULL)
  
  observeEvent(input$country_select, {
    click<-input$country_select
    chosen_country(click)
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    chosen_country(click$id) 
  })
  
  observe({ # makes it reactive, and chosen_country() is inside a reactive function
    req(chosen_country()) # waits for chosen_country to have something selected
    updateSelectInput(
      session,
      inputId = "country_select",
      selected = chosen_country()
    )
  })
  
  country_dataset <- reactiveVal(NULL)
  
  observeEvent(input$country_select, {
    country <-input$country_select
    dataset <- filter(df, COUNTRYNM == country)
    country_dataset(dataset)
  })
  
  output$countryDisplay <- renderText({
    if (is.null(chosen_country())) {
      paste("Choose a country")
    } else {
      paste0("You chose ",chosen_country(), "!")
    }
  })
  
  
  
  pal_reactive <- reactive({
    colorNumeric("YlGnBu", world_joined$population, na.color = "transparent")
  })
  
  output$map <- renderLeaflet({
    pal <- pal_reactive()
    
    
    leaflet(data = world_joined) %>%
      addTiles() %>%
      addPolygons (
        fillColor = ~pal(population),
        layerId = ~name,
        # layerId is an identifier you assign 
        # to each shape (polygon, marker, etc.) 
        # you add to a Leaflet map in Shiny.
        weight = 1,
        color = "grey",
        fillOpacity = 0.8,
        label = ~paste0(
          name, ": ", round(population, 2)
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal, values = ~population,
        title = "Coastal Population"
      ) 
    
  })
  
  # Zooming in on the selected country
  # 
  # observeEvent(input$country_select, {
  #   req(input$country_select)
  #   
  #   if (input$country_select %in% world_joined$name) {
  #     selected_country <- dplyr::filter(world_joined, name == input$country_select)
  #     bbox <- st_bbox(selected_country)
  #     
  #     leafletProxy("map") %>%
  #       fitBounds(
  #         lng1 = bbox["xmin"],
  #         lat1 = bbox["ymin"],
  #         lng2 = bbox["xmax"],
  #         lat2 = bbox["ymax"]
  #       )
  #   }
  # })
  # 
  # 
  # CUSTOM BIVARIATE SCATTERPLOT
  

  
  output$custom_scatter <- renderPlot({
    data <- country_dataset() # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    # reverse so that i can find the labels for each choice
    
    indicator_choices <- c(
      "Distance to Coast (km)" = "distance_to_coast_km", 
      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
    )
    
    x_label <- names(indicator_choices)[indicator_choices == x_col]
    y_label <- names(indicator_choices)[indicator_choices == y_col]
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    
    data1 <- data[[x_col]]
    data2 <- data[[y_col]]
    
    # Check if the data vectors have at least some non-NA numeric values
    if (all(is.na(data1)) || all(is.na(data2))) return()
    
    plot(data1, data2, 
         main = chosen_country(),
         xlab = x_label,
         ylab = y_label)
  
  })
  
  output$correlation <- renderText({
    data <- country_dataset() # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    cor = cor(data[[x_col]], data[[y_col]])
    
    cor = round(cor, 4)
  

    spr_cor = cor(data[[x_col]], data[[y_col]], method = "spearman")
    spr_cor = round(spr_cor, 4)
    
    paste("Pearson Coefficient (r) = ", cor,
          "\nSpearman Coefficient (rho) = ", spr_cor)
    
  })

  output$country_flag <- renderImage({
    # before click
    if (is.null(chosen_country())) {
      list(src = "www/globe.png",
           contentType = "image/png",
           alt = "Globe",
           width = 120,
           height = 120
      )
      # after click
    } else {
      filename <- findPNGpath(chosen_country())
      # Return a list containing the filename
      list(src = filename,
           contentType = "image/png",
           alt = "Country flag",
           width = 160,
           height = 120
      )
    }
  }, deleteFile = FALSE)
  
}
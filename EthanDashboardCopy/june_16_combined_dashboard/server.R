server <- function(input, output, session) {
  
  pal_reactive <- reactive({
    colorNumeric("Blues", world_joined[[input$score_type]], na.color = "transparent")
  })
  
  output$map <- renderLeaflet({
    pal <- pal_reactive()
    
    
    leaflet(data = world_joined) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(world_joined[[input$score_type]]),
        layerId = ~name,
        # layerId is an identifier you assign 
        # to each shape (polygon, marker, etc.) 
        # you add to a Leaflet map in Shiny.
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
  
  clicked_country <- reactiveVal(NULL);
  # reactive value holder
  
  # when country is clicked store the name in click
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    clicked_country(click$id)  # This is the "name" of the country
  })
  
  # Display clicked country name
  output$clicked_country <- renderText({
    if (is.null(clicked_country())) {
      paste("Click a country on the map.")
    } else {
      paste(clicked_country())
    }
  })
  
  # Display that country's scores
  output$country_scores <- renderTable({
    req(clicked_country())  # Wait for click
    table <- df_country %>%
      filter(name_en == clicked_country())
  })
  
  output$country_flag <- renderImage({
    # before click
    if (is.null(clicked_country())) {
      list(src = "www/globe.png",
           contentType = "image/png",
           alt = "Globe",
           width = 120,
           height = 120
          )
    # after click
    } else {
      filename <- findPNGpath(clicked_country())
      # Return a list containing the filename
      list(src = filename,
           contentType = "image/png",
           alt = "Country flag",
           width = 160,
           height = 120
      )
    }
  }, deleteFile = FALSE)
  
  # renderImage() function must return a 
  # list with specific named elements that 
  # tell Shiny how to display the image
  
  output$scoreInQuestion <- renderText({
    req(clicked_country())  # Wait for click
    score <- df_country %>%
      filter(name_en == clicked_country()) %>%
      pull(input$score_type)
    
    if(length(score) == 0 || is.na(score)) {
      "No Data"
    } else {
      paste0(round(score, 2))
    }
  })
  
  # reactive value for selected indicator
  clicked_indicator <- reactiveVal(NULL)
  
  observeEvent(input$score_type, {
    click <- input$score_type
    clicked_indicator(click)  # id of indicator
  })
  
  output$data_descriptions <- renderTable({
    req(clicked_indicator)
    score <- descriptions %>%
      filter(id == clicked_indicator())
  })
  
  output$description <- renderText({
    req(clicked_indicator)
    description <- descriptions %>%
      filter(id == clicked_indicator()) %>%
      pull(description)
  })
  
  output$label <- renderText({
    req(clicked_indicator)
    description <- descriptions %>%
      filter(id == clicked_indicator()) %>%
      pull(label)
  })
  
  output$histogram <- renderPlot({
    req(clicked_indicator)
    label <- descriptions %>%
      filter(id == clicked_indicator()) %>%
      pull(label)
    label <- paste(label, "Score", sep = " ")
    label1 <- paste(label, "Distribution", sep = " ")
    score <- input$score_type
    # ggplot(df_country, aes (x = score)) +
    #   geom_histogram(binwidth =0.1)
    histogram <- hist(x = df_country[[score]], # have to use [[]] to access a column by name stored in a variable
                      main = label1,
                      xlab = label)
  })
  
  pal_reactive_pop <- reactive({
    colorNumeric("Blues", world_joined_pop$population, na.color = "transparent")
  })
  
  output$popmap <- renderLeaflet({
    pal <- pal_reactive_pop()
    
    
    leaflet(data = world_joined_pop) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(population),
        layerId = ~name,
        # layerId is an identifier you assign 
        # to each shape (polygon, marker, etc.) 
        # you add to a Leaflet map in Shiny.
        weight = 1,
        color = "grey",
        fillOpacity = 0.8,
        label = ~paste0(
          name, ": ", world_joined_pop$population
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal, values = ~world_joined_pop$population,
        title = "Population"
      )
  })
  
  # Action Button for searching for countries 
  # in "Individual Country Inspection
  
  observeEvent(input$submit_search, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  output$indCountryMap <- renderPlot({
    countryName <- input$search
    bbox <- bounding_boxes %>% filter(name == input$search)
    
    req(nrow(bbox) > 0)
    data<-filter(worldData_pop_2020, COUNTRYNM == countryName)
    
    xmin <- bbox$xmin
    ymin <- bbox$ymin
    xmax <- bbox$xmax
    ymax <- bbox$ymax
    
    ggplot(data) +
      geom_sf(data = world, fill = "#ADD8E6", color = "white") +
      geom_sf(aes(color = UN_2020_E)) +
      scale_color_viridis_c() +
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      theme_minimal() 
  })
  
}
  
  # map_shape_click is a built-in Shiny 
  # input that captures click events on 
  # shapes (like polygons) in a leaflet map.


# make your own graph tab

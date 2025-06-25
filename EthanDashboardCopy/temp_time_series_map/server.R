# server.R

server <- function(input, output) {
  chosenMonth <- reactiveVal(NULL)
  observeEvent(input$month_slider, {
    req(input$month_slider)
    month <- input$month_slider
    chosenMonth(month)
  })
  
  year_data <- reactiveVal(NULL) 
  observeEvent(input$month_slider, {
    req(input$month_slider)
    col <- chosenMonth()
    data <- wide %>% 
      mutate(value = .data[[col]],
             geom_id = as.character(st_as_text(geometry))
             )
    year_data(data)
  }
  )
  
  observe({
    req(input$map_type == "global_temp_anomaly")
    req(input$month_slider)
    data <- year_data()
    pal <- colorNumeric(
      palette = "RdYlBu",  
      domain = data$value,
      reverse = TRUE
    )
    leafletProxy("my_map", data = data) |>
      clearMarkers() |>
      addCircleMarkers(
        radius = 8,
        label = ~paste0("Value: ", round(value, 2)),
        color = ~pal(value),
        fillOpacity = 1,
        layerId = ~geom_id
      ) |>
      addLegend(
        pal = pal,
        values = c(min_val, max_val),
        opacity = 0.9,
        title = "Global Temperature Anomaly",
        position = "bottomright"
      )
  })
  
  output$histogram <- renderPlot ({
    req(input$month_slider)
    col <- input$month_slider
    hist <- hist(wide[[col]],
                 xlab = col)
  })
  
  clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click$id
    # sfc_geom <- st_as_sfc(click, crs = st_crs(wide))
    # coords <- st_coordinates(sfc_geom)
    clicked_point(click)
    print(input$map_marker_click)
  })
  
  output$time_graph <- renderPlot ({
    req(clicked_point())
    
    filtered <- long %>%
      filter(geom_id == clicked_point()) %>%
      mutate(date = ymd(paste(year, month, 1, sep = "-")))
    
    ggplot(filtered, aes(x = date, y = Anomaly)) +
      geom_line() +              # line plot over time
      geom_point() +             # points for each month
      labs(title = "Anomaly over Time",
           x = "Date",
           y = "Anomaly") +
      theme_bw()
    
  })
  
  # TRY TO DISPLAY GRAPH NEXT TIME
  
  # mean median mode
  
  output$mmm <- renderText({
    col <- input$month_slider
    data <- wide[[col]]
    mean <- mean(data)
    mean <- round(mean, 3)
    median <- median(data)
    median <- round(median, 3)
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    mode <- get_mode(data)
    mode <- round(mode,3)
    paste0("Mean: ", mean, "\nMedian: ", median, "\nMode: ", mode)
    
  })
  
  # ----------------------------------------------------------------------
  
  # For ND Gain Data interactive map
  
  nd_year_data <- reactiveVal(NULL) 
  observeEvent(c(input$nd_year,
               input$variable_nd), {
    req(input$nd_year)
    req(input$variable_nd)
    
    data <- gain %>%
      select(ISO3, Name, Year, input$variable_nd) %>%
      filter(Year == input$nd_year)
    
    nd_year_data(data)
    
  }
  )
  
  output$my_map <- renderLeaflet({
    
    if (input$map_type == "global_temp_anomaly") {
      pal <- colorNumeric(
        palette = "RdYlBu",  
        domain = c(min_val, max_val),
        reverse = TRUE
      )
      leaflet() %>% 
        addTiles() %>%
        setView(lng = 2.5, lat = 7.5, zoom = 2) 
        
    } else if (input$map_type == "nd_gain") { # this is if input$map_type == "nd_gain"
      
      year <- input$nd_year
      year_data <- gain %>%
        filter(Year == year)
      ndVar <- input$variable_nd
      

      pal <- colorNumeric(
        palette = "RdYlBu",  
        domain = c(min_val_nd, max_val_nd),
        reverse = TRUE
      )
      
      leaflet() %>% 
        addTiles() %>%
        setView(lng = 2.5, lat = 7.5, zoom = 2)
      
      
    }
    
    
    
  }
  )

  
  observe({
    req(input$map_type == "nd_gain")
    req(input$nd_year)
    data <- left_join(world_sf, nd_year_data(), by = c("iso_a3" = "ISO3"))
    
    valid_vals <- na.omit(data[[input$variable_nd]])
    req(length(valid_vals) > 0)  # Make sure there's data
    
    min_val_nd <- min(valid_vals)
    max_val_nd <- max(valid_vals)
    
    pal <- colorNumeric(
      palette = "RdYlBu",  
      domain = data$value,
      reverse = TRUE
    )
    
    label <- gainVarsNames[gainVars == input$variable_nd]
    
    leafletProxy("my_map", data = data) |>
      clearMarkers() |>
      addPolygons(
        fillColor = ~pal(get(input$variable_nd)),  # use tidy eval
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        smoothFactor = 0.5,
        label = ~paste0(input$variable_nd, ": ", round(get(input$variable_nd), 4)),
        layerId = ~iso_a3
      ) |>
      addLegend(
        pal = pal,
        values = c(min_val_nd, max_val_nd),
        opacity = 0.9,
        title = ~paste0(label, " Score"),
        position = "bottomright"
      )
  })
  
  countryND <- reactiveVal(NULL)
  
  observeEvent(input$country_nd, {
    req(input$country_nd)
    country <- input$country_nd
    countryND(country)
  })
  
  varND <- reactiveVal(NULL)
  
  observeEvent(input$variable_nd, {
    req(input$variable_nd)
    var <- input$variable_nd
    varND(var)
  })
  
  output$nd_graph <- renderPlot({
    filtered <- gain %>%
      filter(Name == countryND())
    
    label <- gainVarsNames[gainVars == varND()]
    
    ggplot(filtered, aes(x = Year, .data[[varND()]])) +
      geom_line() +              # line plot over time
      geom_point() +             # points for each month
      labs(title = paste0(label, " for ", countryND(), " (1995-2022)"),
           x = "Date",
           y = label) +
      theme_bw()
    
  })
  
  
  
}
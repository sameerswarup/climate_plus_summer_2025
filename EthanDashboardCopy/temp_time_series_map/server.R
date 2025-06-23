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
      ) 
  })
  
  output$my_map <- renderLeaflet({
    pal <- colorNumeric(
      palette = "RdYlBu",  
      domain = c(min_val, max_val),
      reverse = TRUE
    )
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 2.5, lat = 7.5, zoom = 2) %>%
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
}
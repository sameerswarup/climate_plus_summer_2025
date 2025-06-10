library(sf)
library(rnaturalearth)

country_centroids <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()
country_centroids$COUNTRY <- ne_countries(scale = "medium", returnclass = "sf")$admin

server <- function(input, output, session) {
  
  selected_country <- reactiveVal(NULL)
  
  selected_var <- reactive({
    req(input$indicator_category, input$mean_type)
    prefix <- indicator_prefix_map[[input$indicator_category]]
    paste0(prefix, mean_type_suffix[[input$mean_type]])
  })
  
  observe({
    global_data <- data_list[[input$indicator_category]]$global
    choices <- c("Global (Default)", sort(unique(global_data$COUNTRY)))
    updateSelectizeInput(session, "country_search", choices = choices, server = TRUE)
  })
  
  observeEvent(input$country_search, {
    if (input$country_search == "Global (Default)") {
      selected_country(NULL)
    } else {
      selected_country(input$country_search)
    }
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- input$map_marker_click$id
    selected_country(clicked_country)
  })
  
  output$map <- renderLeaflet({
    var <- selected_var()
    global_data <- data_list[[input$indicator_category]]$global
    req(var %in% colnames(global_data))
    
    pal <- colorNumeric("viridis", domain = global_data[[var]], na.color = "transparent")
    
    leaflet(global_data) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "white",
        weight = 0.5,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
        layerId = ~COUNTRY
      ) %>%
      addLegend(
        pal = pal,
        values = global_data[[var]],
        opacity = 0.8,
        title = paste(input$indicator_category, "(", input$mean_type, ")"),
        position = "bottomright"
      )
  })
  
  observeEvent(selected_country(), {
    country <- selected_country()
    var <- selected_var()
    full_data <- data_list[[input$indicator_category]]$full
    global_data <- data_list[[input$indicator_category]]$global
    
    if (is.null(country)) {
      pal <- colorNumeric("viridis", domain = global_data[[var]], na.color = "transparent")
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          data = global_data,
          radius = 6,
          fillColor = ~pal(get(var)),
          fillOpacity = 0.8,
          stroke = TRUE,
          color = "white",
          weight = 0.5,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
          layerId = ~COUNTRY
        ) %>%
        addLegend(
          pal = pal,
          values = global_data[[var]],
          opacity = 0.8,
          title = paste(input$indicator_category, "(", input$mean_type, ")"),
          position = "bottomright"
        )
      return()
    }
    
    country_data <- full_data %>% filter(COUNTRY == country)
    req(nrow(country_data) > 0)
    
    pal <- colorNumeric("viridis", domain = country_data[[var]], na.color = "transparent")
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = country_data,
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.9,
        stroke = TRUE,
        color = "black",
        weight = 0.7,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3))
      ) %>%
      addLegend(
        pal = pal,
        values = country_data[[var]],
        opacity = 0.9,
        title = paste0(country, " (", input$mean_type, ")"),
        position = "bottomright"
      )
  })
  
  observeEvent(input$zoom_button, {
    country <- selected_country()
    if (is.null(country)) {
      leafletProxy("map") %>%
        setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      unlist()
    
    leafletProxy("map") %>%
      setView(lng = zoom_coords["X"], lat = zoom_coords["Y"], zoom = 5)
  })
}

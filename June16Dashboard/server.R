# server.R - Streamlined version with global default
server <- function(input, output, session) {
  
source("modules/countryAnalysisModule.R", local = TRUE)
source("modules/countryComparison.R", local = TRUE)
source("modules/ipcc.R", local = TRUE)
source("modules/ndGain.R", local = TRUE)

countryND <- reactiveVal(NULL)
  
  #COMPARISON MAP OUTPUTS
  output$map1 <- renderLeaflet({
    tiles <- providers$Esri.WorldStreetMap
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(tiles) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$map2 <- renderLeaflet({
    tiles <- providers$Esri.WorldStreetMap
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(tiles) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  
  selected_country <- reactiveVal(NULL)  # Start with global view
  country_dataset <- reactiveVal(NULL)
  map_initialized <- reactiveVal(FALSE)
  
  
  select_country <- function(country) {
    if (is.null(country) || country == "" || country == "Global (Default)") {
      selected_country(NULL)
      country_dataset(NULL)
      updateTextInput(session, "country_search", value = "")
      updateTextInput(session, "country_search_graphs", value = "")
      zoom_to_country("map", NULL)
    } else {
      selected_country(country)
      country_dataset(filter(df, COUNTRY == country))
      updateTextInput(session, "country_search", value = country)
      updateTextInput(session, "country_search_graphs", value = country)
      zoom_to_country("map", country)
    }
    update_map_layers_only()
  }
  
  should_show_points <- function(var) {
    # Show points for Socio-Ecological Vulnerability individual indicators when a country is selected
    socio_ecological_vars <- c("mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc", 
                               "perc.pop.world.coastal.merit.10m.log.sc", "Nutritional.dependence.sc")
    
    # Only show points if: 1) a country is selected AND 2) it's a socio-ecological variable (not composite)
    !is.null(selected_country()) && var %in% socio_ecological_vars
  }
  
  zoom_to_country <- function(map_id, country, zoom_val = 5) {
    coords <- if (is.null(country) || country == "Global (Default)") {
      list(X = 0, Y = 20, zoom = 2)
    } else {
      zoom_coords <- country_centroids %>% filter(COUNTRY == country) %>% select(X, Y) %>% as.list()
      if (length(zoom_coords$X) > 0) c(zoom_coords, zoom = zoom_val) else list(X = 0, Y = 20, zoom = 2)
    }
    leafletProxy(map_id) %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
  }
  
  create_base_map <- function(satellite = FALSE) {
    map <- leaflet(options = leafletOptions(
      zoomControl = FALSE,
      maxBounds = list(list(-90, -180), list(90, 180)),
      maxBoundsViscosity = 1.0,
      minZoom = 2,
      maxZoom = 18,
      worldCopyJump = FALSE
    ))
    if (satellite) {
      map %>% addProviderTiles(providers$Esri.WorldImagery) %>% htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
    } else {
      map %>% addProviderTiles(providers$Esri.WorldStreetMap) %>% htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
    }
  }
  
  
  
  update_map_layers_only <- function() {
    if (!map_initialized() || is.null(input$variable_choice) || is.null(input$indicator_category)) return()
    
    var <- input$variable_choice
    country <- selected_country()
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    # Check if variable exists and has valid data
    if (!(var %in% names(global_data)) || all(is.na(global_data[[var]]))) return()
    
    # Create legend title based on whether composite score is selected
    legend_title <- if (var %in% composite_arith_list) {
      paste(input$indicator_category)
    } else {
      # Find the variable name for display - show ONLY the variable name
      var_display_name <- names(indicator_choice_list[[input$indicator_category]])[
        indicator_choice_list[[input$indicator_category]] == var
      ]
      var_display_name
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    leafletProxy("map") %>%
      clearMarkers() %>% clearShapes() %>% clearControls()
    
    if (is.null(country)) {
      # Global view
      leafletProxy("map") %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY,           label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
        ) %>%
        {if (should_show_points(var)) {
          addCircleMarkers(., 
                           data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                           stroke = TRUE, color = "white", weight = 1,
                           label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))),
                           layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
        } else . } %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = legend_title, position = "bottomright")
    } else {
      # Country-specific view
      if (!should_show_points(var)) {
        selected_country_data <- polygon_data %>% filter(COUNTRY == country)
        other_countries_data <- polygon_data %>% filter(COUNTRY != country)
        
        if (nrow(other_countries_data) > 0) {
          leafletProxy("map") %>%
            addPolygons(
              data = other_countries_data,
              fillColor = "transparent", fillOpacity = 0, 
              color = ~pal(get(var)), weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY,               label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        if (nrow(selected_country_data) > 0) {
          leafletProxy("map") %>%
            addPolygons(
              data = selected_country_data,
              fillColor = ~pal(get(var)), fillOpacity = 0.8,
              color = ~pal(get(var)), weight = 3, opacity = 1,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        leafletProxy("map") %>%
          addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                    title = paste0("<b>", country, "</b><br>", legend_title),
                    position = "bottomright")
      } else {
        # Point data for country
        country_data <- df %>% filter(COUNTRY == country)
        if (nrow(country_data) > 0) {
          use_local <- isTRUE(input$use_country_specific_scale)
          domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
          
          pal_country <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
          border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
          
          leafletProxy("map") %>%
            addPolygons(
              data = polygon_data,
              fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
              weight = 1, opacity = 0.4,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY,               label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            ) %>%
            addCircleMarkers(
              data = country_data, radius = 6, fillColor = ~pal_country(get(var)), fillOpacity = 0.9,
              stroke = TRUE, color = "black", weight = 0.7,
              label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            ) %>%
            addLegend(pal = pal_country, values = domain_data, opacity = 0.9,
                      title = paste0("<b>", country, "</b><br>", legend_title, "<br><i>(", if (use_local) "Local" else "Global", ")</i>"),
                      position = "bottomright")
        }
      }
    }
  }
  
  # # Update dropdown choices based on indicator category
  # observeEvent({input$indicator_category}, {
  #   if (input$indicator_category == "Social Inequality" || input$indicator_category == "Weak Governance") {
  #     updateSelectInput(session, "variable_choice", choices = indicator_choice_list[[input$indicator_category]])
  #   } else {
  #     updateSelectInput(session, "composite_choice", choices = names(composite_data_options))
  #   }
  # })
  # 
  # observeEvent({input$composite_choice}, {
  #   if (input$indicator_category == "Social Inequality" || input$indicator_category == "Weak Governance") {
  #     updateSelectInput(session, "variable_choice", choices = indicator_choice_list[[input$indicator_category]])
  # 
  #   } else {
  #     updateSelectInput(session, "variable_choice", choices = composite_data_options[[input$composite_choice]])
  #   
  #   }
  #   
  # })
  # 
  # When category changes
  observeEvent(input$indicator_category, {
    if (input$indicator_category %in% c("Social Inequality", "Weak Governance")) {
      # Hide composite_choice UI if needed
      updateSelectInput(session, "variable_choice", 
                        choices = indicator_choice_list[[input$indicator_category]],
                        selected = indicator_choice_list[[input$indicator_category]][[1]])
    } else {
      updateSelectInput(session, "composite_choice", 
                        choices = names(composite_data_options),
                        selected = names(composite_data_options)[1])
      
      # Preload variable choices for first composite (e.g., ND Gain)
      first_composite <- names(composite_data_options)[1]
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[first_composite]],
                        selected = composite_data_options[[first_composite]][[1]])
    }
  })
  
  # When composite changes
  observeEvent(input$composite_choice, {
    req(input$indicator_category)  # just in case
    
    if (!(input$indicator_category %in% c("Social Inequality", "Weak Governance"))) {
      # Update variable_choice based on selected composite
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[input$composite_choice]],
                        selected = composite_data_options[[input$composite_choice]][[1]])
    }
  })
  
  # Sync map variable with histogram variable selection
  observeEvent(input$country_histogram_indicator, {
    req(input$country_histogram_indicator)
    
    for (category in names(indicator_choice_list)) {
      if (input$country_histogram_indicator %in% indicator_choice_list[[category]]) {
        updateSelectInput(session, "indicator_category", selected = category)
        updateSelectInput(session, "variable_choice", selected = input$country_histogram_indicator)
        break
      }
    }
  })
  
  # Initialize country choices including comparison dropdowns
  observe({
    countries_list <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "comparison_country_search", choices = countries_list, server = TRUE)
    updateSelectizeInput(session, "map_2_country_search", choices = countries_list, server = TRUE)
    session$sendCustomMessage("updateCountriesList", countries_list)
  })
  
  # Event handlers for country selection
  observeEvent(input$country_search_graphs_selected, {
    req(input$country_search_graphs_selected)
    select_country(input$country_search_graphs_selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$country_search_selected, {
    req(input$country_search_selected)
    select_country(input$country_search_selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_shape_click, {
    select_country(input$map_shape_click$id)
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- gsub("^marker_", "", input$map_marker_click$id)
    select_country(clicked_country)
  })
  
  observeEvent(input$global_view_button, {
    select_country(NULL)
  })

  # Main map output
  output$map <- renderLeaflet({
    var <- "gov.score.rank"
    
    # Ensure data is loaded
    req(combined_scores_global, combined_scores_global_polygons, average_country_nogeo, average_country_polygons)
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    # Check if variable exists and has valid data
    if (!(var %in% names(global_data)) || all(is.na(global_data[[var]]))) {
      map_initialized(TRUE)
      return(create_base_map(FALSE))
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    map <- create_base_map(FALSE)
    
    result <- map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
      ) %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = "Weak Governance", position = "bottomright")
    
    map_initialized(TRUE)
    shinyjs::hide("comparison-maps")
    return(result)
  })
  
  # Update map when variable choices change
  observeEvent({
    input$use_country_specific_scale; input$variable_choice
  }, {
    update_map_layers_only()
  })
  
  # Satellite view toggle
  observeEvent(input$satellite_view, {
    tiles <- if (input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  output$countryDisplay <- renderText({
    country <- selected_country()
    if (is.null(country)) {
      "Global view - Click on a country to analyze specific data"
    } else {
      paste("Currently analyzing:", country, "- Map automatically zoomed to this country")
    }
  })
}
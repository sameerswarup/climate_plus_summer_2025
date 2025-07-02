# server.R - Streamlined version with global default
server <- function(input, output, session) {
  
  selected_country <- reactiveVal(NULL)  # Start with global view
  country_dataset <- reactiveVal(NULL)
  map_initialized <- reactiveVal(FALSE)
  
  indicator_choice_list <- list(
    "Socio-Ecological Vulnerability" = c("Socio-Ecological Vulnerability (Composite)" = "vulnerab.score.rank",
                                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                         "Coastal Climate Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc",
                                         "Nutritional Dependence" = "Nutritional.dependence.sc"),
    "Social Inequality" = c("Social Inequality (Composite)" = "ineq.score.rank",
                            "Gender Inequality" = "gender.ineq.sc",
                            "Income Inequality" = "income.ineq.sc",
                            "Inequality Adjusted Life Expectancy" = "le.ineq.log.sc"),
    "Weak Governance" = c("Weak Governance (Composite)" = "gov.score.rank",
                          "Government Ineffectiveness" = "Gov_effect.sc",
                          "Poor Regulatory Quality" = "Reg_quality.sc",
                          "Weak Rule of Law" = "Rule_law.sc",
                          "Weak Control of Corruption" = "control_corr.sc",
                          "Low Voice and Accountability" = "Voice_account.sc",
                          "Political Instability" = "Political_stab.sc")
  )
  
  should_show_points <- function(var) {
    # Show points for Socio-Ecological Vulnerability individual indicators when a country is selected
    socio_ecological_vars <- c("mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc", 
                               "perc.pop.world.coastal.merit.10m.log.sc", "Nutritional.dependence.sc")
    
    # Only show points if: 1) a country is selected AND 2) it's a socio-ecological variable (not composite)
    !is.null(selected_country()) && var %in% socio_ecological_vars
  }
  
  zoom_to_country <- function(map_id, country) {
    coords <- if (is.null(country) || country == "Global (Default)") {
      list(X = 0, Y = 20, zoom = 2)
    } else {
      zoom_coords <- country_centroids %>% filter(COUNTRY == country) %>% select(X, Y) %>% as.list()
      if (length(zoom_coords$X) > 0) c(zoom_coords, zoom = 5) else list(X = 0, Y = 20, zoom = 2)
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
  
  select_country <- function(country) {
    if (is.null(country) || country == "" || country == "Global (Default)") {
      selected_country(NULL)
      country_dataset(NULL)
      updateTextInput(session, "country_search", value = "")
      updateTextInput(session, "country_search_graphs", value = "")
      zoom_to_country("map", NULL)
      #zoom_to_country("map1", NULL)
      #zoom_to_country("map2", NULL)
    } else {
      selected_country(country)
      country_dataset(filter(df, COUNTRY == country))
      updateTextInput(session, "country_search", value = country)
      updateTextInput(session, "country_search_graphs", value = country)
      zoom_to_country("map", country)
      #zoom_to_country("map1", country)
      #zoom_to_country("map2", country)
      zoom_to_country("map2", country)
      print("zooming")
    }
    update_map_layers_only()
    #update_comparison_map_1_layers_only()
    #update_comparison_map_2_layers_only()
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
  
  # Update dropdown choices based on indicator category
  observeEvent(input$indicator_category, {
    updateSelectInput(session, "variable_choice", choices = indicator_choice_list[[input$indicator_category]])
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
  
  # Initialize country choices
  observe({
    countries_list <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "comparison_country_search", choices = countries_list, server = TRUE)
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
  
  
  
  
  #COMPARISON MAPS
  update_comparison_map_1_layers_only <- function() {
    
    if (!map_initialized()) return()
    
    if (is.null(input$map_1_variable_choice) || is.null(input$map_1_indicator_category)) return()
    
    var <- input$map_1_variable_choice
    
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
      paste(input$map_1_indicator_category)
    } else {
      # Find the variable name for display - show ONLY the variable name
      var_display_name <- names(indicator_choice_list[[input$map_1_indicator_category]])[
        indicator_choice_list[[input$map_1_indicator_category]] == var
      ]
      var_display_name
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    leafletProxy("map1") %>%
      clearMarkers() %>% clearShapes() %>% clearControls()
    
    if (is.null(country)) {
      leafletProxy("map1") %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
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
      if (!should_show_points(var)) {
        selected_country_data <- polygon_data %>% filter(COUNTRY == country)
        other_countries_data <- polygon_data %>% filter(COUNTRY != country)
        
        if (nrow(other_countries_data) > 0) {
          leafletProxy("map1") %>%
            addPolygons(
              data = other_countries_data,
              fillColor = "transparent", fillOpacity = 0, 
              color = ~pal(get(var)), weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        if (nrow(selected_country_data) > 0) {
          leafletProxy("map1") %>%
            addPolygons(
              data = selected_country_data,
              fillColor = ~pal(get(var)), fillOpacity = 0.8,
              color = ~pal(get(var)), weight = 3, opacity = 1,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        leafletProxy("map1") %>%
          addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                    title = paste0("<b>", country, "</b><br>", legend_title),
                    position = "bottomright")
      } else {
        country_data <- df %>% filter(COUNTRY == country)
        if (nrow(country_data) > 0) {
          use_local <- isTRUE(input$use_country_specific_scale)
          domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
          
          pal_country <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
          border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
          
          leafletProxy("map1") %>%
            addPolygons(
              data = polygon_data,
              fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
              weight = 1, opacity = 0.4,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
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
    
    zoom_to_country("map1", selected_country())
  }
  
  
  
  update_comparison_map_2_layers_only <- function() {
    if (!map_initialized()) return()
    
    if (is.null(input$map_2_variable_choice) || is.null(input$map_2_indicator_category)) return()
    
    var <- input$map_2_variable_choice
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
      paste(input$map_2_indicator_category)
    } else {
      # Find the variable name for display - show ONLY the variable name
      var_display_name <- names(indicator_choice_list[[input$map_2_indicator_category]])[
        indicator_choice_list[[input$map_2_indicator_category]] == var
      ]
      var_display_name
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    leafletProxy("map2") %>%
      clearMarkers() %>% clearShapes() %>% clearControls()
    
    if (is.null(country)) {
      leafletProxy("map2") %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
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
      if (!should_show_points(var)) {
        selected_country_data <- polygon_data %>% filter(COUNTRY == country)
        other_countries_data <- polygon_data %>% filter(COUNTRY != country)
        
        if (nrow(other_countries_data) > 0) {
          leafletProxy("map2") %>%
            addPolygons(
              data = other_countries_data,
              fillColor = "transparent", fillOpacity = 0, 
              color = ~pal(get(var)), weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        if (nrow(selected_country_data) > 0) {
          leafletProxy("map2") %>%
            addPolygons(
              data = selected_country_data,
              fillColor = ~pal(get(var)), fillOpacity = 0.8,
              color = ~pal(get(var)), weight = 3, opacity = 1,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        leafletProxy("map2") %>%
          addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                    title = paste0("<b>", country, "</b><br>", legend_title),
                    position = "bottomright")
      } else {
        country_data <- df %>% filter(COUNTRY == country)
        if (nrow(country_data) > 0) {
          use_local <- isTRUE(input$use_country_specific_scale)
          domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
          
          pal_country <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
          border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
          
          leafletProxy("map2") %>%
            addPolygons(
              data = polygon_data,
              fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
              weight = 1, opacity = 0.4,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
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
    
    zoom_to_country("map2", selected_country())
    
  }
  
  
  #COMPARISON VARIABLE SELECTION
  observeEvent(input$map_1_indicator_category, {
    updateSelectInput(session, "map_1_variable_choice", choices = indicator_choice_list[[input$map_1_indicator_category]])
  })
  
  observeEvent(input$map_2_indicator_category, {
    updateSelectInput(session, "map_2_variable_choice", choices = indicator_choice_list[[input$map_2_indicator_category]])
  })
  
  
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
  
  #COMPARISON OBSERVER
  observeEvent({
    input$comparison_country_search; input$map_1_indicator_category; input$map_2_indicator_category; input$map_1_variable_choice; input$map_2_variable_choice
  }, {
    req(input$map_1_indicator_category)
    req(input$map_1_variable_choice)
    req(map_initialized())
    
    selected_country(input$comparison_country_search)
    
    update_comparison_map_1_layers_only()
    update_comparison_map_2_layers_only()
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
  
  # Plotting functions
  create_scatter_plot <- function(data, x_col, y_col, choices, title) {
    if (is.null(data) || !(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    if (all(is.na(data[[x_col]])) || all(is.na(data[[y_col]]))) return()
    
    subtitle <- paste0(names(choices)[choices == x_col], " vs. ", names(choices)[choices == y_col])
    
    ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point() +
      labs(title = title, subtitle = subtitle,
           x = names(choices)[choices == x_col],
           y = names(choices)[choices == y_col]) +
      theme_hc() +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10, l = 10))
      )
  }
  
  calculate_correlation <- function(data, x_col, y_col) {
    if (is.null(data) || nrow(data) == 0) return("No data available")
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return("Selected variables not available")
    
    x_data <- data[[x_col]]
    y_data <- data[[y_col]]
    
    if (!is.numeric(x_data) || !is.numeric(y_data)) return("Selected variables are not numeric")
    if (sum(complete.cases(x_data, y_data)) < 2) return("Insufficient data for correlation analysis")
    
    cor_result <- tryCatch(cor(x_data, y_data, use = "complete.obs"), error = function(e) NA)
    spr_cor_result <- tryCatch(cor(x_data, y_data, method = "spearman", use = "complete.obs"), error = function(e) NA)
    
    if (is.na(cor_result) || is.na(spr_cor_result)) return("Could not calculate correlation")
    
    paste("Pearson Coefficient (r) =", round(cor_result, 4),
          "\nSpearman Coefficient (rho) =", round(spr_cor_result, 4))
  }
  
  # Global analysis modal
  observeEvent(input$global_scale_button, {
    showModal(modalDialog(
      title = "Global Scale Analysis",
      size = "l",
      fluidRow(
        column(6, selectInput("first_indicator_global", "First indicator:", 
                              choices = global_level_choices, selected = "Gov_effect.sc")),
        column(6, selectInput("second_indicator_global", "Second indicator:", 
                              choices = global_level_choices, selected = "le.ineq.log.sc"))
      ),
      plotOutput("global_custom_scatter", height = "400px"),
      verbatimTextOutput("global_correlation"),
      downloadButton("downloadGlobalCustomScatter", "Download Plot"),
      footer = modalButton("Close")
    ))
  })
  
  # Reactive plot functions
  REAcustom_scatter <- reactive({
    country_choices <- c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    create_scatter_plot(country_dataset(), input$first_indicator, input$second_indicator, country_choices, selected_country())
  })
  
  REAglobal_custom_scatter <- reactive({
    create_scatter_plot(average_country_nogeo, input$first_indicator_global, input$second_indicator_global, global_level_choices, "Global")
  })
  
  REArenderHistogram <- reactive({
    data <- country_dataset()
    if (is.null(data) || nrow(data) == 0) return() 
    
    chi <- input$country_histogram_indicator
    if (!(chi %in% names(data))) return()
    
    col <- data[[chi]][!is.na(data[[chi]])]
    if (length(col) <= 1 || !is.numeric(col)) return()
    
    country_choices <- c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    label <- names(country_choices)[country_choices == chi]
    
    ggplot(data, aes(x = .data[[chi]])) +
      geom_histogram(bins = 30, fill = "#00539B", color = "white") +
      labs(title = paste0(label, " for ", selected_country()),
           subtitle = paste0("Each Point Is a Point Within ", selected_country()),
           x = label, y = "Frequency") +
      theme_hc() + 
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10))
      ) 
  })
  
  # Plot outputs
  output$custom_scatter <- renderPlot({
    req(REAcustom_scatter())
    REAcustom_scatter()
  })
  
  output$custom_scatter_zoom <- renderPlot({
    req(REAcustom_scatter())
    REAcustom_scatter()
  })
  
  output$global_custom_scatter <- renderPlot({
    req(REAglobal_custom_scatter())
    REAglobal_custom_scatter()
  })
  
  renderHistogram <- renderPlot({
    req(REArenderHistogram())
    REArenderHistogram()
  })
  
  output$country_histogram_zoom <- renderHistogram
  output$country_histogram <- renderHistogram
  
  output$correlation <- renderText({
    calculate_correlation(country_dataset(), input$first_indicator, input$second_indicator)
  })
  
  output$global_correlation <- renderText({
    calculate_correlation(average_country_nogeo, input$first_indicator_global, input$second_indicator_global)
  })
  
  # Modal dialogs for zoomed plots
  observeEvent(input$scatter_zoom, {
    showModal(modalDialog(
      title = "Country Scale Scatterplot",
      size = "l",
      plotOutput("custom_scatter_zoom", height = "400px"),
      downloadButton("downloadCustomScatter", "Download Plot"),
      verbatimTextOutput("correlation"),
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$histogram_zoom, {
    showModal(modalDialog(
      title = "Country Scale Histogram",
      size = "l",
      plotOutput("country_histogram_zoom", height = "400px"),
      downloadButton("downloadHistogram", "Download Plot"),
      footer = modalButton("Close")
    ))
  })
  
  # Download handlers
  output$downloadCustomScatter <- downloadHandler(
    filename = function() { paste('scatter-plot-', Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(filename = file, plot = REAcustom_scatter(), device = "png", width = 8, height = 6)
    }
  )
  
  output$downloadGlobalCustomScatter <- downloadHandler(
    filename = function() { paste('global-scatter-plot-', Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(filename = file, plot = REAglobal_custom_scatter(), device = "png", width = 8, height = 6)
    }
  )
  
  output$downloadHistogram <- downloadHandler(
    filename = function() { paste('histogram-', Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(filename = file, plot = REArenderHistogram(), device = "png", width = 8, height = 6)
    }
  )
  
  # Descriptions of indicators
  clicked_scores <- list(
    first_global = reactiveVal(NULL),
    second_global = reactiveVal(NULL),
    first_country = reactiveVal(NULL),
    second_country = reactiveVal(NULL)
  )
  clicked_score_country_histogram = reactiveVal(NULL)
  
  observe_map <- list(
    first_indicator_global = "first_global",
    second_indicator_global = "second_global",
    first_indicator = "first_country",
    second_indicator = "second_country"
  )
  
  lapply(names(observe_map), function(id) {
    observeEvent(input[[id]], {
      clicked_scores[[observe_map[[id]]]](input[[id]])
    })
  })
  
  description_output <- function(score_reactive) {
    renderText({
      req(score_reactive())
      inequity_data_descriptions %>%
        filter(variable_name == score_reactive()) %>%
        pull(description)
    })
  }
  
  output$first_indicator_country_description <- description_output(clicked_scores$first_country)
  output$second_indicator_country_description <- description_output(clicked_scores$second_country)
  output$first_indicator_global_description <- description_output(clicked_scores$first_global)
  output$second_indicator_global_description <- description_output(clicked_scores$second_global)
  output$country_histogram_description <- description_output(clicked_score_country_histogram)
  
  observeEvent(input$country_histogram_indicator, {
    clicked_score_country_histogram(input$country_histogram_indicator)
  })
}
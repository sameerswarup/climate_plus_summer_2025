



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
    paste(input$indicator_category)
  } else {
    # Find the variable name for display - show ONLY the variable name
    var_display_name <- names(indicator_choice_list[[input$indicator_category]])[
      indicator_choice_list[[input$indicator_category]] == var
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
  
  if (is.null(input$variable_choice) || is.null(input$indicator_category)) return()
  
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

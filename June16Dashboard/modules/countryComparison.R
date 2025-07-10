# Data type selector (second level dropdown)
output$data_type_selector_map_1 <- renderUI({
  req(input$climate_variable_map_1)
  cat("Selected climate variable:", input$climate_variable_map_1, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable_map_1]])
  cat("Available data types:", paste(choices, collapse = ", "), "\n")
  
  selectInput("data_type_map_1", "Select Data Type:", choices = choices)
})

# Time period selector (third level dropdown)
output$time_period_selector_map_1 <- renderUI({
  req(input$climate_variable_map_1, input$data_type_map_1)
  cat("Selected data type:", input$data_type_map_1, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable_map_1]][[input$data_type_map_1]])
  cat("Available time periods:", paste(choices, collapse = ", "), "\n")
  
  selectInput("time_period_map_1", "Select Time Range:", choices = choices)
})

# Dynamic range slider based on current data and variable
output$value_range_slider_map_1 <- renderUI({
  req(original_raster(), input$climate_variable_map_1)
  
  output$manual_min_input_map_1 <- renderUI({
    req(input$value_range_map_1)
    numericInput(
      "manual_min_map_1",
      "Minimum Value",
      value = input$value_range_map_1[1]
    )
  })
  
  output$manual_max_input_map_1 <- renderUI({
    req(input$value_range_map_1)
    numericInput(
      "manual_max_map_1",
      "Maximum Value",
      value = input$value_range_map_1[2]
    )
  })
  
  r <- original_raster()
  raster_values_map_1 <- values(r, na.rm = TRUE)
  
  if (length(raster_values_map_1) == 0) return(NULL)
  
  value_range_map_1 <- range(raster_values_map_1, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]])) {
    metadata <- variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]]
  } else {
    metadata <- variable_metadata[[input$climate_variable_map_1]]
  }
  
  # Determine step size based on variable type
  if(input$climate_variable_map_1 == "Ocean pH") {
    step_size <- 0.01
    decimals <- 2
  } else if(input$climate_variable_map_1  == "Heating Degree Days") {
    step_size <- 1
    decimals <- 0
  } else {
    step_size <- 0.001
    decimals <- 3
  }
  
  sliderInput("value_range_map_1 ", 
              paste0(metadata$description_map_1, " Range (", metadata$unit_map_1, "):"),
              min = floor(value_range_map_1[1] * (10^decimals)) / (10^decimals),
              max = ceiling(value_range_map_1[2] * (10^decimals)) / (10^decimals),
              value = value_range_map_1,
              step = step_size,
              round = decimals)
})





#INEQUITY
update_comparison_map_1_layers_only <- function() {
  
  #if (!map_initialized()) return()
  
  #if (is.null(input$variable_choice_map_1) || is.null(input$indicator_category_map_1)) return()
  # 
  # req(input$indicator_category_map_1)
  # req(input$variable_choice_map_1)
  # req(map_initialized())
  
  
  var <- input$variable_choice_map_1
  

  country <- selected_country()
  
  if (var %in% composite_arith_list) {
    global_data <- combined_scores_global
    polygon_data <- combined_scores_global_polygons
  } else {
    global_data <- average_country_nogeo
    polygon_data <- average_country_polygons
  }
  
  # Create legend title based on whether composite score is selected
  legend_title <- if (var %in% composite_arith_list) {
    paste(input$indicator_category_map_1)
  } else {
    # Find the variable name for display - show ONLY the variable name
    var_display_name <- names(indicator_choice_list[[input$indicator_category_map_1]])[
      indicator_choice_list[[input$indicator_category_map_1]] == var
    ]
    var_display_name
  }
  
  pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent", reverse = FALSE)
  
  leafletProxy("map1") %>%
    clearMarkers() %>% clearShapes() %>% clearControls()
  
  if (is.null(country)) {
    # Global view - always show polygons
    leafletProxy("map1") %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
      ) %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = legend_title, position = "bottomright")
  } else {
    # Country-specific view
    nd_gain_vars <- unlist(gainVars, use.names = FALSE)
    climate_vars <- unlist(climate_data_options, recursive = TRUE, use.names = FALSE)
    is_special_module_var <- (var %in% nd_gain_vars) || (var %in% climate_vars)
    
    if (is_special_module_var) {
      # Special module variables - use polygon rendering
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
      # Check if variable exists in country-level data (df) - if so, show points
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0 && var %in% names(country_data)) {
        # Show points for variables that exist in df
        use_local <- isTRUE(input$use_country_specific_scale)
        domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
        
        pal_country <- colorNumeric("Purples", domain = domain_data, na.color = "transparent", reverse = FALSE)
        border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent", reverse = FALSE)
        
        leafletProxy("map1") %>%
          # Add background polygons for all countries (for clicking)
          addPolygons(
            data = polygon_data,
            fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
            weight = 1, opacity = 0.4,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          ) %>%
          # Add points on top
          addCircleMarkers(
            data = country_data, radius = 6, fillColor = ~pal_country(get(var)), fillOpacity = 0.9,
            stroke = FALSE,
            label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          ) %>%
          addLegend(pal = pal_country, values = domain_data, opacity = 0.9,
                    title = paste0("<b>", country, "</b><br>", legend_title, "<br><i>(", if (use_local) "Local" else "Global", ")</i>"),
                    position = "bottomright")
      } else {
        # Show highlighted country polygon for other variables
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
      }
    }
  }
  
  zoom_to_country("map1", selected_country(), 5)
}

update_comparison_map_2_layers_only <- function() {
  
  var <- input$variable_choice_map_2
  country <- selected_country()
  
  if (var %in% composite_arith_list) {
    global_data <- combined_scores_global
    polygon_data <- combined_scores_global_polygons
  } else {
    global_data <- average_country_nogeo
    polygon_data <- average_country_polygons
  }
  
  # Create legend title based on whether composite score is selected
  legend_title <- if (var %in% composite_arith_list) {
    paste(input$indicator_category_map_2)
  } else {
    # Find the variable name for display - show ONLY the variable name
    var_display_name <- names(indicator_choice_list[[input$indicator_category_map_2]])[
      indicator_choice_list[[input$indicator_category_map_2]] == var
    ]
    var_display_name
  }
  
  pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent", reverse = FALSE)
  
  leafletProxy("map2") %>%
    clearMarkers() %>% clearShapes() %>% clearControls()
  
  if (is.null(country)) {
    # Global view - always show polygons
    leafletProxy("map2") %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
      ) %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = legend_title, position = "bottomright")
  } else {
    # Country-specific view
    nd_gain_vars <- unlist(gainVars, use.names = FALSE)
    climate_vars <- unlist(climate_data_options, recursive = TRUE, use.names = FALSE)
    is_special_module_var <- (var %in% nd_gain_vars) || (var %in% climate_vars)
    
    if (is_special_module_var) {
      # Special module variables - use polygon rendering
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
      # Check if variable exists in country-level data (df) - if so, show points
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0 && var %in% names(country_data)) {
        # Show points for variables that exist in df
        use_local <- isTRUE(input$use_country_specific_scale)
        domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
        
        pal_country <- colorNumeric("Purples", domain = domain_data, na.color = "transparent", reverse = FALSE)
        border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent", reverse = FALSE)
        
        leafletProxy("map2") %>%
          # Add background polygons for all countries (for clicking)
          addPolygons(
            data = polygon_data,
            fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
            weight = 1, opacity = 0.4,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          ) %>%
          # Add points on top
          addCircleMarkers(
            data = country_data, radius = 6, fillColor = ~pal_country(get(var)), fillOpacity = 0.9,
            stroke = FALSE,
            label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          ) %>%
          addLegend(pal = pal_country, values = domain_data, opacity = 0.9,
                    title = paste0("<b>", country, "</b><br>", legend_title, "<br><i>(", if (use_local) "Local" else "Global", ")</i>"),
                    position = "bottomright")
      } else {
        # Show highlighted country polygon for other variables
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
      }
    }
  }
  
  zoom_to_country("map2", selected_country(), 5)
}

#COMPARISON VARIABLE SELECTION
observeEvent(input$indicator_category_map_1, {
  updateSelectInput(session, "variable_choice_map_1", choices = indicator_choice_list[[input$indicator_category_map_1]])
})

observeEvent(input$indicator_category_map_2, {
  updateSelectInput(session, "variable_choice_map_2", choices = indicator_choice_list[[input$indicator_category_map_2]])
})

#COMPARISON OBSERVER
observeEvent({
  input$comparison_country_search_map_1; input$indicator_category_map_1; input$variable_choice_map_1; 
}, {
  req(input$indicator_category_map_1)
  req(input$variable_choice_map_1)
  req(map_initialized())
  
  selected_country(input$comparison_country_search_map_1)
  update_comparison_map_1_layers_only()
})

observeEvent({
  input$country_search_map_2; input$indicator_category_map_2; input$variable_choice_map_2
}, {
  req(input$indicator_category_map_2)
  req(input$variable_choice_map_2)
  req(map_initialized())
  
  selected_country(input$country_search_map_2)
  
  update_comparison_map_2_layers_only()
})

observeEvent(input$map1_shape_click, {
  select_country(input$map1_shape_click$id)
  update_comparison_map_1_layers_only()
})

observeEvent(input$map1_marker_click, {
  clicked_country <- gsub("^marker_", "", input$map1_marker_click$id)
  select_country(clicked_country)
  update_comparison_map_1_layers_only()
})

observeEvent(input$map2_shape_click, {
  select_country(input$map2_shape_click$id)
  update_comparison_map_2_layers_only()
})

observeEvent(input$map2_marker_click, {
  clicked_country <- gsub("^marker_", "", input$map2_marker_click$id)
  select_country(clicked_country)
  update_comparison_map_2_layers_only()
})






## ND GAIN


observeEvent(input$country_nd, {
  req(input$country_nd)
  country <- input$country_nd
  countryND(country)
  zoom_to_country_nd("nd_gain_map", country)
})

observeEvent(c(input$nd_year,
               input$variable_nd,
               input$country_nd), {
                 req(input$nd_year)
                 req(input$variable_nd)
                 req(countryND())
                 country<-countryND()
                 data <- gain %>%
                   select(ISO3, Name, Year, input$variable_nd) %>%
                   filter(Year == input$nd_year)
                 
                 score <- data %>%
                   filter(Name == country) %>%
                   pull(input$variable_nd)
                 
                 # add it here
                 year <- as.character(input$nd_year)
                 pointData <- gain_wide_points %>%
                   filter(Name == country) %>%
                   select(name_en, iso_a3.x, matches(year))
                 
                 nd_year_score(score)
                 nd_year_data(data)
                 year(input$nd_year)
                 point_data(pointData)
               }
)

observeEvent(c(input$manual_min, input$manual_max), {
  req(input$manual_min, input$manual_max)
  updateSliderInput(
    session,
    "value_range",
    value = c(input$manual_min, input$manual_max)
  )
})

output$variableNameAndYearOutput <- renderText({
  req(varND())
  req(year())
  var <- varND()
  year <- year()
  country <- countryND()
  
  label <- gainVarsNames[gainVars == var]
  label <- paste(label, "for", country, "in", year)
  return(label)
})

output$nd_gain_map_1 <- renderLeaflet({
  year <- input$nd_year
  year_data <- gain %>%
    filter(Year == year)
  ndVar <- input$variable_nd
  pal <- colorNumeric(
    palette = "YlGn",  
    domain = c(min_val_nd(), max_val_nd()),
    reverse = TRUE
  )
  
  leaflet(options = leafletOptions(
    worldCopyJump = FALSE,
    maxBounds = world_bounds,
    maxBoundsViscosity = 1.0
  )) %>% 
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)
})


observe({
  req(input$nd_year)
  nd_data <- nd_year_data()
  req(!is.null(nd_data), nrow(nd_data) > 0)
  
  data <- left_join(world_sf, nd_data, by = c("iso_a3" = "ISO3"))
  
  valid_vals <- na.omit(data[[input$variable_nd]])
  req(length(valid_vals) > 0)  # Make sure there's data
  
  min_val_nd(min(valid_vals))
  max_val_nd(max(valid_vals))
  
  pal <- colorNumeric(
    palette = "YlGn",  
    domain = data$value,
    reverse = TRUE
  )
  
  label <- gainVarsNames[gainVars == input$variable_nd]
  
  leafletProxy("nd_gain_map", data = data) |>
    clearMarkers() |>
    addPolygons(
      fillColor = ~pal(get(input$variable_nd)),  # use tidy eval
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      smoothFactor = 0.5,
      label = ~paste0(Name, ": ", round(get(input$variable_nd), 4)),
      layerId = ~iso_a3
    ) |>
    addLegend(
      pal = pal,
      values = c(min_val_nd(), max_val_nd()),
      opacity = 0.9,
      title = ~paste0(label, " Score"),
      position = "bottomright"
    )
})



observeEvent(input$variable_nd, {
  req(input$variable_nd)
  var <- input$variable_nd
  varND(var)
})


## IPCC


# IPCC MAP 1 Server File

# Reactive values to store current raster
current_raster_map_2 <- reactiveVal(NULL)
original_raster_map_2 <- reactiveVal(NULL)
current_metadata_map_2 <- reactiveVal(NULL)
chosenMonth_map_2 <- reactiveVal(NULL)
min_val_nd_map_2 <- reactiveVal(NULL)
max_val_nd_map_2 <- reactiveVal(NULL)
year_data_map_2 <- reactiveVal(NULL) 
clicked_point_map_2 <- reactiveVal(NULL)
nd_year_data_map_2 <- reactiveVal(NULL) 
nd_year_score_map_2 <- reactiveVal(NULL)
indicator_desc_map_2 <- reactiveVal(NULL)
countryND_map_2 <- reactiveVal(NULL)
varND_map_2 <- reactiveVal(NULL)
year_map_2 <- reactiveVal(NULL)
world_polygons_map_2 <- reactiveVal(world_sf)
filtered_world_polygons_map_2 <- reactive({
  req(world_polygons_map_2())
  world_polygons_map_2() %>%
    filter(continent != "Antarctica")
})
point_data_map_2 <- reactiveVal(NULL)
ca_nd_year_data1_map_2 <- reactiveVal(NULL)
ca_nd_year_data2_map_2 <- reactiveVal(NULL)


# Data type selector (second level dropdown)
output$data_type_selector_map_2 <- renderUI({
  req(input$climate_variable_map_2)
  cat("Selected climate variable:", input$climate_variable_map_2, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable_map_2]])
  cat("Available data types:", paste(choices, collapse = ", "), "\n")
  
  selectInput("data_type_map_2", "Select Data Type:", choices = choices)
})

# Time period selector (third level dropdown)
output$time_period_selector_map_2 <- renderUI({
  req(input$climate_variable_map_2, input$data_type_map_2)
  cat("Selected data type:", input$data_type_map_2, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable_map_2]][[input$data_type_map_2]])
  cat("Available time periods:", paste(choices, collapse = ", "), "\n")
  
  selectInput("time_period_map_2", "Select Time Range:", choices = choices)
})

# Variable information display
output$variable_info_map_2 <- renderUI({
  req(input$climate_variable_map_2)
  
  if (!is.null(variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]][[paste0("'",input$data_type_map_2,"'")]])) {
    metadata <- variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]][[paste0("'",input$data_type_map_2,"'")]]
  } else {
    metadata <- variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]]
  }
  
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
    tags$small(
      tags$strong("Variable: "), metadata$description_map_2, tags$br(),
      tags$strong("Units: "), metadata$unit_map_2, tags$br(),
      tags$strong("Baseline: "), metadata$baseline_map_2
    )
  )
})

# Dynamic range slider based on current data and variable
output$value_range_slider_map_2 <- renderUI({
  req(original_raster_map_2(), input$climate_variable_map_2)
  
  output$manual_min_input_map_2 <- renderUI({
    req(input$value_range_map_2)
    numericInput(
      "manual_min_map_2",
      "Minimum Value",
      value = input$value_range_map_2[1]
    )
  })
  
  output$manual_max_input_map_2 <- renderUI({
    req(input$value_range_map_2)
    numericInput(
      "manual_max_map_2",
      "Maximum Value",
      value = input$value_range_map_2[2]
    )
  })
  
  r <- original_raster_map_2()
  raster_values_map_2 <- values(r, na.rm = TRUE)
  
  if (length(raster_values_map_2) == 0) return(NULL)
  
  value_range_map_2 <- range(raster_values_map_2, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[input$climate_variable_map_2]][[input$data_type_map_2]])) {
    metadata_map_2 <- variable_metadata[[input$climate_variable_map_2]][[input$data_type_map_2]]
  } else {
    metadata_map_2 <- variable_metadata[[input$climate_variable_map_2]]
  }
  
  # Determine step size based on variable type
  if(input$climate_variable_map_2 == "Ocean pH") {
    step_size <- 0.01
    decimals <- 2
  } else if(input$climate_variable_map_2 == "Heating Degree Days") {
    step_size <- 1
    decimals <- 0
  } else {
    step_size <- 0.001
    decimals <- 3
  }
  
  sliderInput("value_range", 
              paste0(metadata_map_2$description, " Range (", metadata_map_2$unit, "):"),
              min = floor(value_range_map_2[1] * (10^decimals)) / (10^decimals),
              max = ceiling(value_range_map_2[2] * (10^decimals)) / (10^decimals),
              value = value_range_map_2,
              step = step_size,
              round = decimals)
})

# Data summary output
output$data_info_map_2 <- renderText({
  req(current_raster_map_2(), input$climate_variable_map_2)
  
  r <- current_raster_map_2()
  values_data_map_2 <- values(r, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]][[paste0("'",input$data_type_map_2,"'")]])) {
    metadata_map_2 <- variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]][[paste0("'",input$data_type_map_2,"'")]]
  } else {
    metadata_map_2 <- variable_metadata[[paste0("'",input$climate_variable_map_2,"'")]]
  }
  
  if (length(values_data_map_2) == 0) {
    return("No data to display")
  }
  
  # Format precision based on variable type
  precision <- if(input$climate_variable_map_2 == "Heating Degree Days") 0 else 3
  
  paste0(
    "Variable: ", input$climate_variable_map_2, "\n",
    "Type: ", input$data_type_map_2, "\n",
    "Period: ", input$time_period_map_2, "\n\n",
    "Cells displayed: ", format(length(values_data_map_2), big.mark = ","), "\n",
    "Value range: ", round(min(values_data_map_2), precision), " to ", round(max(values_data_map_2), precision), " ", metadata_map_2$unit, "\n",
    "Mean: ", round(mean(values_data_map_2), precision), " ", metadata_map_2$unit, "\n",
    "Std Dev: ", round(sd(values_data_map_2), precision), " ", metadata_map_2$unit
  )
})

#Click Reaction
output$click_info_map_2 <- renderText({
  info <- clicked_point_map_2()

  if (is.null(info)) return("Click on the map to see details.")
  
  variable_label_map_2 <- if (!is.null(current_metadata_map_2())) {
    current_metadata_map_2()$description
  } else {
    "Value"
  }
  
  val_text <- if (!is.null(info$value) && !is.na(info$value)) {
    print(info$value)
    round(info$value, 3)
  } else {
    "NA"
  }

  paste0(
    "Latitude: ", round(info$lat, 5), "\n",
    "Longitude: ", round(info$lng, 5), "\n",
    variable_label_map_2, ": ", val_text
  )
})

# # Initial blank map with world bounds restriction
# output$climate_map_2 <- renderLeaflet({
#   leaflet(options = leafletOptions(
#     worldCopyJump = FALSE,
#     maxBounds = world_bounds,
#     maxBoundsViscosity = 1.0
#   )) %>%
#     addTiles() %>%
#     setView(lng = 0, lat = 0, zoom = 3)
# })

# Load raster when selections change
observe({
  req(input$climate_variable_map_2, input$data_type_map_2, input$time_period_map_2)
  
  cat("🌎 Loading selection:\n")
  cat("Climate Variable:", input$climate_variable_map_2, "\n")
  cat("Data Type:", input$data_type_map_2, "\n") 
  cat("Time Period:", input$time_period_map_2, "\n")
  
  tiff_path <- NULL
  tryCatch({
    # Get the normal (default) raster path
    tiff_path_map_2 <- climate_data_options[[input$climate_variable_map_2]][[input$data_type_map_2]][[input$time_period_map_2]]

    # If user selected masked version, adjust path
    if (!is.null(tiff_path_map_2) && input$use_masked_raster_map_2) {
      tiff_path_map_2 <- sub("\\.tif$", "_masked.tif", tiff_path_map_2)
    }
    
    cat("File path:", tiff_path_map_2, "\n")
    
  }, error = function(e) {
    showNotification(paste("⚠️ Error resolving file path:",e), type = "error")
    return()
  })

  tryCatch({
    # showNotification(
    #   paste("Loading raster file:", basename(tiff_path_map_2)),
    #   type = "message", duration = 4
    # )
    
    r <- rast(tiff_path_map_2)
    
    if (is.na(crs(r))) {
      crs(r) <- "EPSG:4326"
    }
    
    r <- crop(r, ext(-180, 180, -85, 85))
    
    gauss_kernel_map_2 <- matrix(c(1,2,1,2,4,2,1,2,1), nrow = 3) / 16
    r <- focal(r, w = gauss_kernel_map_2, fun = sum, na.policy = "omit")
    
    # Store after fully processed
    original_raster_map_2(r)
    current_raster_map_2(r)

    if (!is.null(variable_metadata[[input$climate_variable_map_2]][[input$data_type_map_2]])) {
      current_metadata_map_2(variable_metadata[[input$climate_variable_map_2]][[input$data_type_map_2]])
    } else {
      current_metadata_map_2(variable_metadata[[input$climate_variable_map_2]])
    }
    
  }, error = function(e) {
    print(e)
  })
})

# Apply filters when filter controls change
observe({
  req(original_raster_map_2(), input$filter_mode_map_2)
  
  r <- original_raster_map_2()
  
  if (input$filter_mode_map_2 != "none" && !is.null(input$value_range_map_2)) {
    r_filtered_map_2 <- r
    
    if (input$filter_mode_map_2 == "range") {
      r_filtered_map_2[r_filtered_map_2 < input$value_range_map_2[1] | r_filtered_map_2 > input$value_range_map_2[2]] <- NA
      
    } else if (input$filter_mode_map_2 == "above") {
      r_filtered_map_2[r_filtered_map_2 <= input$value_range_map_2[2]] <- NA
      
    } else if (input$filter_mode_map_2 == "below") {
      r_filtered_map_2[r_filtered_map_2 >= input$value_range_map_2[1]] <- NA
    }
    
    current_raster_map_2(r_filtered_map_2)
  } else {
    current_raster_map_2(r)
  }
})

# Update map when current_raster changes
observe({

  req(current_raster_map_2(), current_metadata_map_2())
  
  r <- current_raster_map_2()
  metadata_map_2 <- current_metadata_map_2()
  
  raster_values_map_2 <- values(r, na.rm = TRUE)
  if (length(raster_values_map_2) == 0) {
    return()
  }
  
  original_values_map_2 <- values(original_raster_map_2(), na.rm = TRUE)
  color_range_map_2 <- range(original_values_map_2, na.rm = TRUE)
  
  if (input$climate_variable_map_2 == "Heating Degree Days") {
    # For Heating Degree Days ONLY, bake in transparency to the color scale itself
    pal_colors_map_2 <- viridisLite::viridis(256, alpha = 0.4)
    pal_map_2 <- colorNumeric(
      palette = pal_colors_map_2,
      domain = color_range_map_2,
      na.color = "transparent"
    )
  } else {
    # All other variables use normal palette (no alpha)
    pal_map_2 <- colorNumeric(
      palette = metadata_map_2$color_palette,
      domain = color_range_map_2,
      na.color = "transparent"
    )
  }
  
  legend_title_map_2 <- paste0(
    "Variable: ", input$climate_variable_map_2, "<br>",
    "Type: ", input$data_type_map_2, "<br>",
    "Term: ", input$time_period_map_2, "<br>",
    "Units: ", metadata_map_2$unit
  )
  
  map_proxy_map_2 <- leafletProxy("climate_map_2") %>%
    clearImages() %>%
    clearControls() %>%
    clearShapes() %>%
    addRasterImage(
      r, 
      colors = pal_map_2, 
      opacity = 0.8,
      project = TRUE,
      group = "raster"
    )
  
  if (input$climate_variable_map_2 == "Heating Degree Days") {
    map_proxy_map_2 <- map_proxy_map_2 %>%
      addPolygons(
        data = filtered_world_polygons_map_2(),
        color = "black",
        weight = 1,
        fill = FALSE,
        opacity = 1
      )
  }
  
  map_proxy_map_2 %>%
    addLegend(
      position = "bottomright",
      pal = pal_map_2, 
      values = original_values_map_2,
      title = legend_title_map_2,
      opacity = 1
    ) %>%
    fitBounds(
      lng1 = xmin(ext(r)), 
      lat1 = ymin(ext(r)), 
      lng2 = xmax(ext(r)), 
      lat2 = ymax(ext(r))
    )
})

# --- NEW CODE START ---
observeEvent(input$climate_map_click_map_2, {
  click_map_2 <- input$climate_map_click_map_2
  
  print(click_map_2)
  
  if (is.null(click_map_2)) return()
  
  lat_map_2 <- click$lat_map_2
  lng_map_2 <- click$lng_map_2
  
  val_map_2 <- NULL
  if (!is.null(original_raster_map_2())) {
    val_extracted_map_2 <- terra::extract(original_raster_map_2(), matrix(c(lng_map_2, lat_map_2), ncol = 2))
    print(val_extracted_map_2)  # optional for debugging
    if (!is.null(val_extracted_map_2) && nrow(val_extracted_map_2) >= 1) {
      val_map_2 <- val_extracted_map_2[1, ncol(val_extracted_map_2)]
    }
  }
  
  # Store in reactive for UI if needed
  clicked_point_map_2(list(lat = lat_map_2, lng = lng_map_2, value = val_map_2))
  
  # Get variable description for label
  variable_label_map_2 <- if (!is.null(current_metadata_map_2())) {
    current_metadata_map_2()$description
  } else {
    "Value"
  }
  
  # Create text for popup
  popup_text <- paste0(
    "<strong>Latitude:</strong> ", round(lat_map_2, 5), "<br>",
    "<strong>Longitude:</strong> ", round(lng_map_2, 5), "<br>",
    "<strong>", variable_label_map_2, ":</strong> ", 
    ifelse(is.na(val_map_2), "NA", round(val_map_2, 3))
  )
  
  # Add popup to map
  leafletProxy("climate_map_2") %>%
    clearPopups_map_2() %>%
    addPopups_map_2(lng = lng_map_2, lat = lat_map_2, popup = popup_text_map_2)
})
# --- NEW CODE END ---

# Reset filters
observeEvent(input$reset_filters_map_2, {
  updateRadioButtons(session, "filter_mode_map_2", selected = "none")
  req(original_raster_map_2())
  
  r <- original_raster_map_2()
  raster_values_map_2 <- values(r, na.rm = TRUE)
  value_range_map_2 <- range(raster_values_map_2, na.rm = TRUE)
  
  updateSliderInput(session, "value_range_map_2", value = value_range_map_2)
})


output$histogram_plot_map_2 <- renderPlotly({
  req(current_raster_map_2(), input$climate_variable_map_2)
  r <- current_raster_map_2()
  vals_map_2 <- as.vector(values(r, na.rm = TRUE))
  vals_map_2 <- vals[!is.na(vals_map_2)]
  req(length(vals_map_2) > 0)
  
  df <- data.frame(Value = vals_map_2)
  
  p <- ggplot(df, aes(x = Value)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = 0.8) +
    labs(
      title = paste("Histogram of", input$climate_variable_map_2),
      x = "Value",
      y = "Count"
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(
        margin = margin(t = 15),
        face = "bold",
        family = "Arial"
      ),
      axis.title.y = element_text(
        margin = margin(r = 15),
        face = "bold",
        family = "Arial"
      ),
      plot.title = element_text(
        size = 11,
        hjust = 0.5,
        family = "Arial"
      ),
      text = element_text(
        family = "Arial"
      )
    )
  
  ggplotly(p)
})

output$data_summary_vb <- renderUI({
  var_map_2 <- varND_map_2()
  varName_map_2 <- gainVarsNames[gainVars == var_map_2]
  iconName_map_2 <- ndGainIcons[[paste0("'", varName_map_2, "'")]]

  value_box(
    title = textOutput("variableNameAndYearOutput"),
    showcase = icon(iconName_map_2),
    value = textOutput("nd_year_score_map_2")
  )
})
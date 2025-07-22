# IPCC MAP 1 Server File

# Reactive values to store current raster
current_raster_map_1 <- reactiveVal(NULL)
original_raster_map_1 <- reactiveVal(NULL)
current_metadata_map_1 <- reactiveVal(NULL)
chosenMonth_map_1 <- reactiveVal(NULL)
min_val_nd_map_1 <- reactiveVal(NULL)
max_val_nd_map_1 <- reactiveVal(NULL)
year_data_map_1 <- reactiveVal(NULL) 
clicked_point_map_1 <- reactiveVal(NULL)
nd_year_data_map_1 <- reactiveVal(NULL) 
nd_year_score_map_1 <- reactiveVal(NULL)
indicator_desc_map_1 <- reactiveVal(NULL)
countryND_map_1 <- reactiveVal(NULL)
varND_map_1 <- reactiveVal(NULL)
year_map_1 <- reactiveVal(NULL)
world_polygons_map_1 <- reactiveVal(world_sf)
filtered_world_polygons_map_1 <- reactive({
  req(world_polygons_map_1())
  world_polygons_map_1() %>%
    filter(continent != "Antarctica")
})
point_data_map_1 <- reactiveVal(NULL)
ca_nd_year_data1_map_1 <- reactiveVal(NULL)
ca_nd_year_data2_map_1 <- reactiveVal(NULL)


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

# Variable information display
output$variable_info_map_1 <- renderUI({
  req(input$climate_variable_map_1)
  
  if (!is.null(variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]][[paste0("'",input$data_type_map_1,"'")]])) {
    metadata <- variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]][[paste0("'",input$data_type_map_1,"'")]]
  } else {
    metadata <- variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]]
  }
  
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
    tags$small(
      tags$strong("Variable: "), metadata$description_map_1, tags$br(),
      tags$strong("Units: "), metadata$unit_map_1, tags$br(),
      tags$strong("Baseline: "), metadata$baseline_map_1
    )
  )
})

# Dynamic range slider based on current data and variable
output$value_range_slider_map_1 <- renderUI({
  req(original_raster_map_1(), input$climate_variable_map_1)
  
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
  
  r <- original_raster_map_1()
  raster_values_map_1 <- values(r, na.rm = TRUE)
  
  if (length(raster_values_map_1) == 0) return(NULL)
  
  value_range_map_1 <- range(raster_values_map_1, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]])) {
    metadata_map_1 <- variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]]
  } else {
    metadata_map_1 <- variable_metadata[[input$climate_variable_map_1]]
  }
  
  # Determine step size based on variable type
  if(input$climate_variable_map_1 == "Ocean pH") {
    step_size <- 0.01
    decimals <- 2
  } else if(input$climate_variable_map_1 == "Heating Degree Days") {
    step_size <- 1
    decimals <- 0
  } else {
    step_size <- 0.001
    decimals <- 3
  }
  
  sliderInput("value_range", 
              paste0(metadata_map_1$description, " Range (", metadata_map_1$unit, "):"),
              min = floor(value_range_map_1[1] * (10^decimals)) / (10^decimals),
              max = ceiling(value_range_map_1[2] * (10^decimals)) / (10^decimals),
              value = value_range_map_1,
              step = step_size,
              round = decimals)
})

# Data summary output
output$data_info_map_1 <- renderText({
  req(current_raster_map_1(), input$climate_variable_map_1)
  
  r <- current_raster_map_1()
  values_data_map_1 <- values(r, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]][[paste0("'",input$data_type_map_1,"'")]])) {
    metadata_map_1 <- variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]][[paste0("'",input$data_type_map_1,"'")]]
  } else {
    metadata_map_1 <- variable_metadata[[paste0("'",input$climate_variable_map_1,"'")]]
  }
  
  if (length(values_data_map_1) == 0) {
    return("No data to display")
  }
  
  # Format precision based on variable type
  precision <- if(input$climate_variable_map_1 == "Heating Degree Days") 0 else 3
  
  paste0(
    "Variable: ", input$climate_variable_map_1, "\n",
    "Type: ", input$data_type_map_1, "\n",
    "Period: ", input$time_period_map_1, "\n\n",
    "Cells displayed: ", format(length(values_data_map_1), big.mark = ","), "\n",
    "Value range: ", round(min(values_data_map_1), precision), " to ", round(max(values_data_map_1), precision), " ", metadata_map_1$unit, "\n",
    "Mean: ", round(mean(values_data_map_1), precision), " ", metadata_map_1$unit, "\n",
    "Std Dev: ", round(sd(values_data_map_1), precision), " ", metadata_map_1$unit
  )
})

#Click Reaction
output$click_info_map_1 <- renderText({
  info <- clicked_point_map_1()
  print("clicked")
  if (is.null(info)) return("Click on the map to see details.")
  
  variable_label_map_1 <- if (!is.null(current_metadata_map_1())) {
    current_metadata_map_1()$description
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
    variable_label_map_1, ": ", val_text
  )
})

# # Initial blank map with world bounds restriction
# output$climate_map_1 <- renderLeaflet({
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
  req(input$climate_variable_map_1, input$data_type_map_1, input$time_period_map_1)
  
  cat("🌎 Loading selection:\n")
  cat("Climate Variable:", input$climate_variable_map_1, "\n")
  cat("Data Type:", input$data_type_map_1, "\n") 
  cat("Time Period:", input$time_period_map_1, "\n")
  
  tiff_path <- NULL
  tryCatch({
    # Get the normal (default) raster path
    tiff_path_map_1 <- climate_data_options[[input$climate_variable_map_1]][[input$data_type_map_1]][[input$time_period_map_1]]

    # If user selected masked version, adjust path
    if (!is.null(tiff_path_map_1) && input$use_masked_raster_map_1) {
      tiff_path_map_1 <- sub("\\.tif$", "_masked.tif", tiff_path_map_1)
    }
    
    cat("File path:", tiff_path_map_1, "\n")
    
  }, error = function(e) {
    showNotification(paste("⚠️ Error resolving file path:",e), type = "error")
    return()
  })
  
  tiles <- if (input$climate_variable_map_1 != "Coral Bleaching Heat") providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
  leafletProxy("climate_map_1") %>% clearTiles() %>% addProviderTiles(tiles)

  tryCatch({
    # showNotification(
    #   paste("Loading raster file:", basename(tiff_path_map_1)),
    #   type = "message", duration = 4
    # )
    
    r <- rast(tiff_path_map_1)
    
    if (is.na(crs(r))) {
      crs(r) <- "EPSG:4326"
    }
    
    r <- crop(r, ext(-180, 180, -85, 85))
    
    gauss_kernel_map_1 <- matrix(c(1,2,1,2,4,2,1,2,1), nrow = 3) / 16
    r <- focal(r, w = gauss_kernel_map_1, fun = sum, na.policy = "omit")
    
    # Store after fully processed
    original_raster_map_1(r)
    current_raster_map_1(r)

    if (!is.null(variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]])) {
      current_metadata_map_1(variable_metadata[[input$climate_variable_map_1]][[input$data_type_map_1]])
    } else {
      current_metadata_map_1(variable_metadata[[input$climate_variable_map_1]])
    }
    
  }, error = function(e) {
    print(e)
  })
})

# Apply filters when filter controls change
observe({
  req(original_raster_map_1(), input$filter_mode_map_1)
  
  r <- original_raster_map_1()
  
  if (input$filter_mode_map_1 != "none" && !is.null(input$value_range_map_1)) {
    r_filtered_map_1 <- r
    
    if (input$filter_mode_map_1 == "range") {
      r_filtered_map_1[r_filtered_map_1 < input$value_range_map_1[1] | r_filtered_map_1 > input$value_range_map_1[2]] <- NA
      
    } else if (input$filter_mode_map_1 == "above") {
      r_filtered_map_1[r_filtered_map_1 <= input$value_range_map_1[2]] <- NA
      
    } else if (input$filter_mode_map_1 == "below") {
      r_filtered_map_1[r_filtered_map_1 >= input$value_range_map_1[1]] <- NA
    }
    
    current_raster_map_1(r_filtered_map_1)
  } else {
    current_raster_map_1(r)
  }
})

# Update map when current_raster changes
observe({

  req(current_raster_map_1(), current_metadata_map_1())
  
  r <- current_raster_map_1()
  metadata_map_1 <- current_metadata_map_1()
  
  raster_values_map_1 <- values(r, na.rm = TRUE)
  if (length(raster_values_map_1) == 0) {
    return()
  }
  
  original_values_map_1 <- values(original_raster_map_1(), na.rm = TRUE)
  color_range_map_1 <- range(original_values_map_1, na.rm = TRUE)
  
  if (input$climate_variable_map_1 == "Heating Degree Days") {
    # For Heating Degree Days ONLY, bake in transparency to the color scale itself
    pal_colors_map_1 <- viridisLite::viridis(256, alpha = 0.4)
    pal_map_1 <- colorNumeric(
      palette = pal_colors_map_1,
      domain = color_range_map_1,
      na.color = "transparent"
    )
  } else {
    # All other variables use normal palette (no alpha)
    pal_map_1 <- colorNumeric(
      palette = metadata_map_1$color_palette,
      domain = color_range_map_1,
      na.color = "transparent"
    )
  }
  
  legend_title_map_1 <- paste0(
    "Variable: ", input$climate_variable_map_1, "<br>",
    "Type: ", input$data_type_map_1, "<br>",
    "Term: ", input$time_period_map_1, "<br>",
    "Units: ", metadata_map_1$unit
  )
  
  map_proxy_map_1 <- leafletProxy("climate_map_1") %>%
    clearImages() %>%
    clearControls() %>%
    clearShapes() %>%
    addRasterImage(
      r, 
      colors = pal_map_1, 
      opacity = 0.8,
      project = TRUE,
      group = "raster"
    )
  
  if (input$climate_variable_map_1 == "Heating Degree Days") {
    map_proxy_map_1 <- map_proxy_map_1 %>%
      addPolygons(
        data = filtered_world_polygons_map_1(),
        color = "black",
        weight = 1,
        fill = FALSE,
        opacity = 1
      )
  }
  
  map_proxy_map_1 %>%
    addLegend(
      position = "bottomright",
      pal = pal_map_1, 
      values = original_values_map_1,
      title = legend_title_map_1,
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
observeEvent(input$climate_map_1_click, {
  click_map_1 <- input$climate_map_1_click
  
  if (is.null(click_map_1)) return()
  
  lat_map_1 <- click_map_1$lat
  lng_map_1 <- click_map_1$lng
  
  val_map_1 <- NULL
  if (!is.null(original_raster_map_1())) {
    val_extracted_map_1 <- terra::extract(original_raster_map_1(), matrix(c(lng_map_1, lat_map_1), ncol = 2))
    print(val_extracted_map_1)  # optional for debugging
    if (!is.null(val_extracted_map_1) && nrow(val_extracted_map_1) >= 1) {
      val_map_1 <- val_extracted_map_1[1, ncol(val_extracted_map_1)]
    }
  }
  
  # Store in reactive for UI if needed
  clicked_point_map_1(list(lat = lat_map_1, lng = lng_map_1, value = val_map_1))
  
  # Get variable description for label
  variable_label_map_1 <- if (!is.null(current_metadata_map_1())) {
    current_metadata_map_1()$description
  } else {
    "Value"
  }
  
  # Create text for popup
  popup_text <- paste0(
    "<strong>Latitude:</strong> ", round(lat_map_1, 5), "<br>",
    "<strong>Longitude:</strong> ", round(lng_map_1, 5), "<br>",
    "<strong>", variable_label_map_1, ":</strong> ", 
    ifelse(is.na(val_map_1), "NA", round(val_map_1, 3))
  )
  
  # Add popup to map
  leafletProxy("climate_map_1") %>%
    clearPopups() %>%
    addPopups(lng = lng_map_1, lat = lat_map_1, popup = popup_text)
})
# --- NEW CODE END ---

# Reset filters
observeEvent(input$reset_filters_map_1, {
  updateRadioButtons(session, "filter_mode_map_1", selected = "none")
  req(original_raster_map_1())
  
  r <- original_raster_map_1()
  raster_values_map_1 <- values(r, na.rm = TRUE)
  value_range_map_1 <- range(raster_values_map_1, na.rm = TRUE)
  
  updateSliderInput(session, "value_range_map_1", value = value_range_map_1)
})


output$histogram_plot_map_1 <- renderPlotly({
  req(current_raster_map_1(), input$climate_variable_map_1)
  r <- current_raster_map_1()
  vals_map_1 <- as.vector(values(r, na.rm = TRUE))
  vals_map_1 <- vals[!is.na(vals_map_1)]
  req(length(vals_map_1) > 0)
  
  df <- data.frame(Value = vals_map_1)
  
  p <- ggplot(df, aes(x = Value)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = 0.8) +
    labs(
      title = paste("Histogram of", input$climate_variable_map_1),
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
  var_map_1 <- varND_map_1()
  varName_map_1 <- gainVarsNames[gainVars == var_map_1]
  iconName_map_1 <- ndGainIcons[[paste0("'", varName_map_1, "'")]]

  value_box(
    title = textOutput("variableNameAndYearOutput"),
    showcase = icon(iconName_map_1),
    value = textOutput("nd_year_score_map_1")
  )
})
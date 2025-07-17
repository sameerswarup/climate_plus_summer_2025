# IPCC Server File

world_bounds <- list(
  list(-85, -180),
  list(85, 180)
)

# Reactive values to store current raster
current_raster <- reactiveVal(NULL)
original_raster <- reactiveVal(NULL)
current_metadata <- reactiveVal(NULL)
chosenMonth <- reactiveVal(NULL)
min_val_nd <- reactiveVal(NULL)
max_val_nd <- reactiveVal(NULL)
year_data <- reactiveVal(NULL) 
clicked_point <- reactiveVal(NULL)
nd_year_data <- reactiveVal(NULL) 
nd_year_score <- reactiveVal(NULL)
indicator_desc <- reactiveVal(NULL)
countryND <- reactiveVal(NULL)
varND <- reactiveVal(NULL)
year <- reactiveVal(NULL)
world_polygons <- reactiveVal(world_sf)
filtered_world_polygons <- reactive({
  req(world_polygons())
  world_polygons() %>%
    filter(continent != "Antarctica")
})
point_data <- reactiveVal(NULL)
ca_nd_year_data1 <- reactiveVal(NULL)
ca_nd_year_data2 <- reactiveVal(NULL)

varND <- reactive({
  req(input$variable_nd)
  input$variable_nd
})



# Data type selector (second level dropdown)
output$data_type_selector <- renderUI({
  req(input$climate_variable)
  cat("Selected climate variable:", input$climate_variable, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable]])
  cat("Available data types:", paste(choices, collapse = ", "), "\n")
  
  selectInput("data_type", "Select Data Type:", choices = choices)
})

# Time period selector (third level dropdown)
output$time_period_selector <- renderUI({
  req(input$climate_variable, input$data_type)
  cat("Selected data type:", input$data_type, "\n")
  
  choices <- names(climate_data_options[[input$climate_variable]][[input$data_type]])
  cat("Available time periods:", paste(choices, collapse = ", "), "\n")
  
  selectInput("time_period", "Select Time Range:", choices = choices)
})

# Variable information display
output$variable_info <- renderUI({
  req(input$climate_variable)
  
  metadata <- NULL
  
  if (!is.null(variable_metadata[[input$climate_variable]][[input$data_type]])) {
    metadata <- variable_metadata[[input$climate_variable]][[input$data_type]]
  } else if (!is.null(variable_metadata[[input$climate_variable]])) {
    metadata <- variable_metadata[[input$climate_variable]]
  }
  
  if (is.null(metadata)) {
    return(NULL)
  }
  
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
    tags$small(
      tags$strong("Variable: "), metadata$description, tags$br(),
      tags$strong("Units: "), metadata$unit, tags$br(),
      tags$strong("Baseline: "), metadata$baseline
    )
  )
})

# Dynamic range slider based on current data and variable
output$value_range_slider <- renderUI({
  req(original_raster(), input$climate_variable)
  
  output$manual_min_input <- renderUI({
    req(input$value_range)
    numericInput(
      "manual_min",
      "Minimum Value",
      value = input$value_range[1]
    )
  })
  
  output$manual_max_input <- renderUI({
    req(input$value_range)
    numericInput(
      "manual_max",
      "Maximum Value",
      value = input$value_range[2]
    )
  })
  
  r <- original_raster()
  raster_values <- values(r, na.rm = TRUE)
  
  if (length(raster_values) == 0) return(NULL)
  
  value_range <- range(raster_values, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[input$climate_variable]][[input$data_type]])) {
    metadata <- variable_metadata[[input$climate_variable]][[input$data_type]]
  } else {
    metadata <- variable_metadata[[input$climate_variable]]
  }
  
  # Determine step size based on variable type
  if(input$climate_variable == "Ocean pH") {
    step_size <- 0.01
    decimals <- 2
  } else if(input$climate_variable == "Heating Degree Days") {
    step_size <- 1
    decimals <- 0
  } else {
    step_size <- 0.001
    decimals <- 3
  }
  
  sliderInput("value_range", 
              paste0(metadata$description, " Range (", metadata$unit, "):"),
              min = floor(value_range[1] * (10^decimals)) / (10^decimals),
              max = ceiling(value_range[2] * (10^decimals)) / (10^decimals),
              value = value_range,
              step = step_size,
              round = decimals)
})

# Data summary output
output$data_info <- renderText({
  req(current_raster(), input$climate_variable)
  
  r <- current_raster()
  values_data <- values(r, na.rm = TRUE)
  
  if (!is.null(variable_metadata[[input$climate_variable]][[input$data_type]])) {
    metadata <- variable_metadata[[input$climate_variable]][[input$data_type]]
  } else {
    metadata <- variable_metadata[[input$climate_variable]]
  }
  
  if (length(values_data) == 0) {
    return("No data to display")
  }
  
  # Format precision based on variable type
  precision <- if(input$climate_variable == "Heating Degree Days") 0 else 3
  
  paste0(
    "Variable: ", input$climate_variable, "\n",
    "Type: ", input$data_type, "\n",
    "Period: ", input$time_period, "\n\n",
    "Cells displayed: ", format(length(values_data), big.mark = ","), "\n",
    "Value range: ", round(min(values_data), precision), " to ", round(max(values_data), precision), " ", metadata$unit, "\n",
    "Mean: ", round(mean(values_data), precision), " ", metadata$unit, "\n",
    "Std Dev: ", round(sd(values_data), precision), " ", metadata$unit
  )
})

#Click Reaction
output$click_info <- renderText({
  info <- clicked_point()
  if (is.null(info)) return("Click on the map to see details.")
  
  variable_label <- if (!is.null(current_metadata())) {
    current_metadata()$description
  } else {
    "Value"
  }
  
  val_text <- if (!is.null(info$value) && !is.na(info$value)) {
    round(info$value, 3)
  } else {
    "NA"
  }
  
  paste0(
    "Latitude: ", round(info$lat, 5), "\n",
    "Longitude: ", round(info$lng, 5), "\n",
    variable_label, ": ", val_text
  )
})

# Initial blank map with world bounds restriction
output$climate_map <- renderLeaflet({
  leaflet(options = leafletOptions(
    zoomControl = FALSE,
    worldCopyJump = FALSE,
    maxBounds = world_bounds,
    maxBoundsViscosity = 1.0
  )) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ")
})

# Load raster when selections change
observe({
  req(input$climate_variable, input$data_type, input$time_period)
  
  cat("🌎 Loading selection:\n")
  cat("Climate Variable:", input$climate_variable, "\n")
  cat("Data Type:", input$data_type, "\n") 
  cat("Time Period:", input$time_period, "\n")
  
  tiff_path <- NULL
  tryCatch({
    # Get the normal (default) raster path
    tiff_path <- climate_data_options[[input$climate_variable]][[input$data_type]][[input$time_period]]
    
    # If user selected masked version, adjust path
    if (!is.null(tiff_path) && input$use_masked_raster) {
      tiff_path <- sub("\\.tif$", "_masked.tif", tiff_path)
    }
    
    cat("File path:", tiff_path, "\n")
    
    # if (is.null(tiff_path) || is.na(tiff_path) || !file.exists(tiff_path)) {
    #   showNotification(
    #     paste("⚠️ File not found:", tiff_path),
    #     type = "error", duration = 5
    #   )
    #   return()
    # }
  }, error = function(e) {
    print(e)
    #showNotification("⚠️ Error resolving file path.", type = "error")
    return()
  })

  tryCatch({
    # showNotification(
    #   paste("Loading raster file:", basename(tiff_path)),
    #   type = "message", duration = 4
    # )
    
    r <- rast(tiff_path)
    
    if (is.na(crs(r))) {
      crs(r) <- "EPSG:4326"
    }
    
    r <- crop(r, ext(-180, 180, -85, 85))
    
    gauss_kernel <- matrix(c(1,2,1,2,4,2,1,2,1), nrow = 3) / 16
    r <- focal(r, w = gauss_kernel, fun = sum, na.policy = "omit")
    
    # if (all(is.na(values(r, na.rm = FALSE)))) {
    #   showNotification("⚠️ Raster contains only NA values!", type = "warning")
    #   return()
    # }
    
    # Store after fully processed
    original_raster(r)
    current_raster(r)
    
    if (!is.null(variable_metadata[[input$climate_variable]][[input$data_type]])) {
      current_metadata(variable_metadata[[input$climate_variable]][[input$data_type]])
    } else {
      current_metadata(variable_metadata[[input$climate_variable]])
    }
    
    cat("✅ COMPLETED LOAD of raster file:", tiff_path, "\n")
    # showNotification(
    #   paste("✅ Finished loading:", basename(tiff_path)),
    #   type = "message", duration = 6
    # )
    
  }, error = function(e) {
    #showNotification("❌ Error loading raster file.", type = "error")
    cat("❌ Error loading raster:", conditionMessage(e), "\n")
  })
})

# Apply filters when filter controls change
observe({
  req(original_raster(), input$filter_mode)
  
  r <- original_raster()
  
  if (input$filter_mode != "none" && !is.null(input$value_range)) {
    r_filtered <- r
    
    if (input$filter_mode == "range") {
      r_filtered[r_filtered < input$value_range[1] | r_filtered > input$value_range[2]] <- NA
      
    } else if (input$filter_mode == "above") {
      r_filtered[r_filtered <= input$value_range[2]] <- NA
      
    } else if (input$filter_mode == "below") {
      r_filtered[r_filtered >= input$value_range[1]] <- NA
    }
    
    current_raster(r_filtered)
  } else {
    current_raster(r)
  }
})

# Update map when current_raster changes
observe({
  req(current_raster(), current_metadata())
  
  r <- current_raster()
  metadata <- current_metadata()
  
  raster_values <- values(r, na.rm = TRUE)
  if (length(raster_values) == 0) {
    return()
  }
  
  original_values <- values(original_raster(), na.rm = TRUE)
  color_range <- range(original_values, na.rm = TRUE)
  
  if (input$climate_variable == "Heating Degree Days") {
    # For Heating Degree Days ONLY, bake in transparency to the color scale itself
    pal_colors <- viridisLite::viridis(256, alpha = 0.4)
    pal <- colorNumeric(
      palette = pal_colors,
      domain = color_range,
      na.color = "transparent"
    )
  } else {
    # All other variables use normal palette (no alpha)
    pal <- colorNumeric(
      palette = metadata$color_palette,
      domain = color_range,
      na.color = "transparent"
    )
  }
  
  legend_title <- paste0(
    "Variable: ", input$climate_variable, "<br>",
    "Type: ", input$data_type, "<br>",
    "Term: ", input$time_period, "<br>",
    "Units: ", metadata$unit
  )
  
  map_proxy <- leafletProxy("climate_map") %>%
    clearImages() %>%
    clearControls() %>%
    clearShapes() %>%
    addRasterImage(
      r, 
      colors = pal, 
      opacity = 0.8,
      project = TRUE,
      group = "raster"
    )
  
  if (input$climate_variable == "Heating Degree Days") {
    map_proxy <- map_proxy %>%
      addPolygons(
        data = filtered_world_polygons(),
        color = "black",
        weight = 1,
        fill = FALSE,
        opacity = 1
      )
  }
  
  map_proxy %>%
    addLegend(
      position = "bottomright",
      pal = pal, 
      values = original_values,
      title = legend_title,
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
observeEvent(input$climate_map_click, {
  click <- input$climate_map_click
  if (is.null(click)) return()
  
  lat <- click$lat
  lng <- click$lng
  
  val <- NULL
  if (!is.null(original_raster())) {
    val_extracted <- terra::extract(original_raster(), matrix(c(lng, lat), ncol = 2))
    print(val_extracted)  # optional for debugging
    if (!is.null(val_extracted) && nrow(val_extracted) >= 1) {
      val <- val_extracted[1, ncol(val_extracted)]
    }
  }
  
  # Store in reactive for UI if needed
  clicked_point(list(lat = lat, lng = lng, value = val))
  
  # Get variable description for label
  variable_label <- if (!is.null(current_metadata())) {
    current_metadata()$description
  } else {
    "Value"
  }
  
  # Create text for popup
  popup_text <- paste0(
    "<strong>Latitude:</strong> ", round(lat, 5), "<br>",
    "<strong>Longitude:</strong> ", round(lng, 5), "<br>",
    "<strong>", variable_label, ":</strong> ", 
    ifelse(is.na(val), "NA", round(val, 3))
  )
  
  # Add popup to map
  leafletProxy("climate_map") %>%
    clearPopups() %>%
    addPopups(lng = lng, lat = lat, popup = popup_text)
})
# --- NEW CODE END ---

# Reset filters
observeEvent(input$reset_filters, {
  updateRadioButtons(session, "filter_mode", selected = "none")
  req(original_raster())
  
  r <- original_raster()
  raster_values <- values(r, na.rm = TRUE)
  value_range <- range(raster_values, na.rm = TRUE)
  
  updateSliderInput(session, "value_range", value = value_range)
})


output$histogram_plot <- renderPlotly({
  req(current_raster(), input$climate_variable)
  r <- current_raster()
  vals <- as.vector(values(r, na.rm = TRUE))
  vals <- vals[!is.na(vals)]
  req(length(vals) > 0)
  
  df <- data.frame(Value = vals)
  
  p <- ggplot(df, aes(x = Value)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = 0.8) +
    labs(
      title = paste("Histogram of", input$climate_variable),
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

output$boxplot_plot <- renderPlotly({
  req(current_raster(), input$climate_variable)
  r <- current_raster()
  vals <- as.vector(values(r, na.rm = TRUE))
  vals <- vals[!is.na(vals)]
  req(length(vals) > 0)
  
  df <- data.frame(Value = vals)
  
  p <- ggplot(df, aes(y = Value)) +
    geom_boxplot(fill = "steelblue", color = "black", alpha = 0.8) +
    labs(
      title = paste("Boxplot of", input$climate_variable),
      y = "Value"
    ) +
    theme_fivethirtyeight() +
    theme(
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

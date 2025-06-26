server <- function(input, output, session) {
  
  # Reactive values to store current raster
  current_raster <- reactiveVal(NULL)
  original_raster <- reactiveVal(NULL)
  current_metadata <- reactiveVal(NULL)
  
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
    metadata <- variable_metadata[[input$climate_variable]]
    
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
    
    r <- original_raster()
    raster_values <- values(r, na.rm = TRUE)
    
    if (length(raster_values) == 0) return(NULL)
    
    value_range <- range(raster_values, na.rm = TRUE)
    metadata <- variable_metadata[[input$climate_variable]]
    
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
    metadata <- variable_metadata[[input$climate_variable]]
    
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
  
  # Initial blank map with world bounds restriction
  output$climate_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      maxBounds = list(
        list(-180, -85),
        list(180, 85)
      ),
      maxBoundsViscosity = 1.0
    )) %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Load raster when selections change
  observe({
    req(input$climate_variable, input$data_type, input$time_period)
    
    # Debug print statements
    cat("Climate Variable:", input$climate_variable, "\n")
    cat("Data Type:", input$data_type, "\n") 
    cat("Time Period:", input$time_period, "\n")
    
    # Get file path with error checking
    tryCatch({
      tiff_path <- climate_data_options[[input$climate_variable]][[input$data_type]][[input$time_period]]
      cat("File path:", tiff_path, "\n")
      
      if (is.null(tiff_path) || is.na(tiff_path)) {
        return()
      }
      
      if (!file.exists(tiff_path)) {
        return()
      }
    }, error = function(e) {
      return()
    })
    
    # Load and process raster
    r <- rast(tiff_path)
    
    # Check and fix CRS if needed
    if (is.na(crs(r))) {
      crs(r) <- "EPSG:4326"
    }
    
    # Crop polar edges to avoid projection issues
    r <- crop(r, ext(-180, 180, -85, 85))
    
    gauss_kernel <- matrix(c(1,2,1,2,4,2,1,2,1), nrow = 3) / 16
    r <- focal(r, w = gauss_kernel, fun = sum, na.policy = "omit")
    
    # Check for data
    if (all(is.na(values(r, na.rm = FALSE)))) {
      return()
    }
    
    # Store original raster and metadata
    original_raster(r)
    current_raster(r)
    current_metadata(variable_metadata[[input$climate_variable]])
  })
  
  # Apply filters when filter controls change
  observe({
    req(original_raster(), input$filter_mode)
    
    r <- original_raster()
    
    # Apply filtering based on mode
    if (input$filter_mode != "none" && !is.null(input$value_range)) {
      r_filtered <- r
      
      if (input$filter_mode == "range") {
        # Show only values within range
        r_filtered[r_filtered < input$value_range[1] | r_filtered > input$value_range[2]] <- NA
        
      } else if (input$filter_mode == "above") {
        # Show only values above threshold
        r_filtered[r_filtered <= input$value_range[2]] <- NA
        
      } else if (input$filter_mode == "below") {
        # Show only values below threshold  
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
    
    # Get value range for color palette (excluding NAs)
    raster_values <- values(r, na.rm = TRUE)
    if (length(raster_values) == 0) {
      return()
    }
    
    # Use original data range for consistent color scale
    original_values <- values(original_raster(), na.rm = TRUE)
    color_range <- range(original_values, na.rm = TRUE)
    
    # Create color palette based on variable type
    pal <- colorNumeric(
      palette = metadata$color_palette, 
      domain = color_range,
      na.color = "transparent"
    )
    
    # Create legend title
    legend_title <- paste0(
      input$climate_variable, "<br>",
      input$data_type, "<br>",
      input$time_period, "<br>",
      "(", metadata$unit, ")"
    )
    
    # Update map
    leafletProxy("climate_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(
        r, 
        colors = pal, 
        opacity = 0.8,
        project = TRUE,
        group = "raster"
      ) %>%
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
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateRadioButtons(session, "filter_mode", selected = "none")
    req(original_raster())
    
    # Reset slider to full range
    r <- original_raster()
    raster_values <- values(r, na.rm = TRUE)
    value_range <- range(raster_values, na.rm = TRUE)
    
    updateSliderInput(session, "value_range", value = value_range)
  })
}
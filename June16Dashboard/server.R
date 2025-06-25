# server.R
server <- function(input, output, session) {
  
  # Reactive values for tracking state
  selected_country <- reactiveVal(NULL)
  current_map_for_country <- reactiveVal("map")
  last_zoomed_country <- reactiveVal(NULL)
  hovered_country <- reactiveVal(NULL)
  chosen_country <- reactiveVal(NULL)
  country_dataset <- reactiveVal(NULL)
  
  # Data description reactives
  description_reactives <- list(
    first_global = reactiveVal(NULL),
    second_global = reactiveVal(NULL),
    first_country = reactiveVal(NULL),
    second_country = reactiveVal(NULL),
    histogram = reactiveVal(NULL)
  )
  
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
  
  # Reactive expressions
  selected_var <- reactive(req(input$variable_choice))
  map_1_selected_var <- reactive(req(input$map_1_variable_choice))
  map_2_selected_var <- reactive(req(input$map_2_variable_choice))
  
  # Helper functions
  should_show_points <- function(var) {
    var %in% c("vulnerab.score.rank", "mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc", 
               "perc.pop.world.coastal.merit.10m.log.sc", "Nutritional.dependence.sc")
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
    map <- leaflet()
    if (satellite) {
      map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
  }
  
  # Update variable choices - consolidated
  observeEvent(input$indicator_category, {
    updateSelectInput(session, "variable_choice", choices = indicator_choice_list[[input$indicator_category]])
  })
  
  observeEvent(input$map_1_indicator_category, {
    updateSelectInput(session, "map_1_variable_choice", choices = indicator_choice_list[[input$map_1_indicator_category]])
  })
  
  observeEvent(input$map_2_indicator_category, {
    updateSelectInput(session, "map_2_variable_choice", choices = indicator_choice_list[[input$map_2_indicator_category]])
  })
  
  # Update country search choices - unified search approach
  observe({
    choices <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "country_search", choices = choices, server = TRUE)
    updateSelectizeInput(session, "comparison_country_search", choices = choices, server = TRUE)
  })
  
  # Main country search handler - primary search for interactive map
  observeEvent(input$country_search, {
    current_map_for_country("map")
    
    if (input$country_search != "Global (Default)" && !is.null(input$country_search)) {
      updateSelectInput(session, "country_select", selected = input$country_search)
      selected_country(input$country_search)
      # Auto-zoom to selected country
      zoom_to_country("map", input$country_search)
    } else {
      selected_country(NULL)
      # Return to global view
      zoom_to_country("map", NULL)
      # Re-render global map
      var <- selected_var()
      
      if (var %in% composite_arith_list) {
        global_data <- combined_scores_global
        polygon_data <- combined_scores_global_polygons
      } else {
        global_data <- average_country_nogeo
        polygon_data <- average_country_polygons
      }
      
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      leafletProxy("map") %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        ) %>%
        {if (should_show_points(var)) {
          addCircleMarkers(., 
                           data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                           stroke = TRUE, color = "white", weight = 1,
                           label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                           layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
        } else . } %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
    }
  })
  
  # Comparison country search handler - applies to both comparison maps
  observeEvent(input$comparison_country_search, {
    current_map_for_country("compare_map_1")
    
    country <- input$comparison_country_search
    
    # Zoom both comparison maps to the selected country
    zoom_to_country("compare_map_1", country)
    zoom_to_country("compare_map_2", country)
    
    # Update both comparison maps with the selected country
    if (country == "Global (Default)" || is.null(country)) {
      selected_country(NULL)
    } else {
      selected_country(country)
    }
  })
  
  observeEvent(input$country_select, {
    current_map_for_country("map")
    if (!is.null(input$country_select)) {
      updateSelectizeInput(session, "country_search", selected = input$country_select)
    }
  })
  
  # Click handlers - updated for new search system
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!is.null(clicked_country)) {
      selected_country(clicked_country)
      updateSelectizeInput(session, "country_search", selected = clicked_country)
      updateSelectInput(session, "country_select", selected = clicked_country)
      zoom_to_country("map", clicked_country)
    }
  })
  
  # Comparison map click handlers - updated to use new search
  observeEvent(input$compare_map_1_shape_click, {
    clicked_country <- input$compare_map_1_shape_click$id
    if (!is.null(clicked_country)) {
      updateSelectizeInput(session, "comparison_country_search", selected = clicked_country)
    }
  })
  
  observeEvent(input$compare_map_2_shape_click, {
    clicked_country <- input$compare_map_2_shape_click$id
    if (!is.null(clicked_country)) {
      updateSelectizeInput(session, "comparison_country_search", selected = clicked_country)
    }
  })
  
  # Marker click handlers - updated for new search system
  observeEvent(input$map_marker_click, {
    clicked_country <- gsub("^marker_", "", input$map_marker_click$id)
    selected_country(clicked_country)
    updateSelectizeInput(session, "country_search", selected = clicked_country)
    zoom_to_country("map", clicked_country)
  })
  
  observeEvent(input$compare_map_1_marker_click, {
    clicked_country <- gsub("^marker_", "", input$compare_map_1_marker_click$id)
    updateSelectizeInput(session, "comparison_country_search", selected = clicked_country)
  })
  
  observeEvent(input$compare_map_2_marker_click, {
    clicked_country <- gsub("^marker_", "", input$compare_map_2_marker_click$id)
    updateSelectizeInput(session, "comparison_country_search", selected = clicked_country)
  })
  
  # Mouse events
  observeEvent(input$map_shape_mouseover, {
    hovered_country(input$map_shape_mouseover$id)
  })
  
  observeEvent(input$map_shape_mouseout, {
    hovered_country(NULL)
  })
  
  # Main map rendering
  output$map <- renderLeaflet({
    
    # if (input$composite_category == inequity_composite) {
    
    
    # } else if (input$composite_category == nd_gain) {
    
    
    # } else if (input$composite_category == climate_risk)
    
    var <- selected_var()
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    req(var %in% colnames(global_data))
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    map <- create_base_map(input$satellite_view)
    
    map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
      ) %>%
      {if (should_show_points(var)) {
        addCircleMarkers(., 
                         data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                         stroke = TRUE, color = "white", weight = 1,
                         label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                         layerId = ~paste0("marker_", COUNTRY), group = "markers"
        )
      } else . } %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = paste(input$indicator_category), position = "bottomright")
  })
  
  # Update map tiles when satellite view changes
  observeEvent(input$satellite_view, {
    tiles <- if (input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  # Update main map when selections change - updated for new search system
  observeEvent({
    input$comparison_country_search; input$country_search; input$use_country_specific_scale
  }, {
    req(input$indicator_category)
    var <- selected_var()
    country <- selected_country()
    
    if (is.null(country)) {
      # GLOBAL VIEW
      if (var %in% composite_arith_list) {
        global_data <- combined_scores_global
        polygon_data <- combined_scores_global_polygons
      } else {
        global_data <- average_country_nogeo
        polygon_data <- average_country_polygons
      }
      
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      leafletProxy(current_map_for_country()) %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        ) %>%
        {if (should_show_points(var)) {
          addCircleMarkers(., 
                           data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                           stroke = TRUE, color = "white", weight = 1,
                           label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                           layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
        } else . } %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
      return()
    }
    
    # COUNTRY SPECIFIC LOGIC
    if (!should_show_points(var)) {
      # For governance/inequality: highlight selected country, make others transparent
      if (var %in% composite_arith_list) {
        global_data <- combined_scores_global
        polygon_data <- combined_scores_global_polygons
      } else {
        global_data <- average_country_nogeo
        polygon_data <- average_country_polygons
      }
      
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      
      # Get the selected country's data for highlighting
      selected_country_data <- polygon_data %>% filter(COUNTRY == country)
      other_countries_data <- polygon_data %>% filter(COUNTRY != country)
      
      leafletProxy(current_map_for_country()) %>%
        clearMarkers() %>% clearShapes() %>% clearControls()
      
      # Add other countries as transparent with borders only
      if (nrow(other_countries_data) > 0) {
        leafletProxy(current_map_for_country()) %>%
          addPolygons(
            data = other_countries_data,
            fillColor = "transparent", fillOpacity = 0, 
            color = ~pal(get(var)), weight = 2, opacity = 0.5,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "other_polygons"
          )
      }
      
      # Add selected country as highlighted/shaded
      if (nrow(selected_country_data) > 0) {
        leafletProxy(current_map_for_country()) %>%
          addPolygons(
            data = selected_country_data,
            fillColor = ~pal(get(var)), fillOpacity = 0.8,
            color = ~pal(get(var)), weight = 3, opacity = 1,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "selected_polygon"
          )
      }
      
      leafletProxy(current_map_for_country()) %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
      return()
    }
    
    # Only for socio-ecological vulnerability: show individual country points
    country_data <- df %>% filter(COUNTRY == country)
    req(nrow(country_data) > 0)
    
    use_local <- isTRUE(input$use_country_specific_scale)
    domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
    border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    leafletProxy(current_map_for_country()) %>%
      clearMarkers() %>% clearShapes() %>% clearControls() %>%
      addPolygons(
        data = polygon_data,
        fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
        weight = 1, opacity = 0.4,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
      ) %>%
      addCircleMarkers(
        data = country_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
        stroke = TRUE, color = "black", weight = 0.7,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "markers"
      ) %>%
      addLegend(pal = pal, values = domain_data, opacity = 0.9,
                title = paste0(country, if (use_local) " (Local Scale)" else " (Global Scale)"),
                position = "bottomright")
  })
  
  # Comparison map update logic - updated for unified search
  update_comparison_map <- function(map_id, var_func, use_local) {
    var <- var_func()
    country <- selected_country()  # Use the same selected country from comparison search
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    if (is.null(country)) {
      # Global view for comparison maps
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      leafletProxy(map_id) %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        ) %>%
        {if (should_show_points(var)) {
          addCircleMarkers(., 
                           data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                           stroke = TRUE, color = "white", weight = 1,
                           label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                           layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
        } else . } %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste("Map", substr(map_id, nchar(map_id), nchar(map_id))), position = "bottomright")
    } else if (!should_show_points(var)) {
      # Highlight selected country for governance/inequality
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      
      selected_country_data <- polygon_data %>% filter(COUNTRY == country)
      other_countries_data <- polygon_data %>% filter(COUNTRY != country)
      
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearControls()
      
      if (nrow(other_countries_data) > 0) {
        leafletProxy(map_id) %>%
          addPolygons(
            data = other_countries_data,
            fillColor = "transparent", fillOpacity = 0,
            color = ~pal(get(var)), weight = 2, opacity = 0.5,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          )
      }
      
      if (nrow(selected_country_data) > 0) {
        leafletProxy(map_id) %>%
          addPolygons(
            data = selected_country_data,
            fillColor = ~pal(get(var)), fillOpacity = 0.8,
            color = ~pal(get(var)), weight = 3, opacity = 1,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          )
      }
      
      leafletProxy(map_id) %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste("Map", substr(map_id, nchar(map_id), nchar(map_id))), position = "bottomright")
    } else {
      # Show individual points for socio-ecological
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0) {
        domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
        pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
        border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
        
        leafletProxy(map_id) %>%
          clearMarkers() %>% clearShapes() %>% clearControls() %>%
          addPolygons(
            data = polygon_data, fillColor = "transparent", fillOpacity = 0,
            color = ~border_pal(get(var)), weight = 1, opacity = 0.4,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          ) %>%
          addCircleMarkers(
            data = country_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
            stroke = TRUE, color = "black", weight = 0.7,
            label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          ) %>%
          addLegend(pal = pal, values = domain_data, opacity = 0.9,
                    title = paste0(country, if (use_local) " (Local)" else " (Global)"),
                    position = "bottomright")
      }
    }
  }
  
  # Handle comparison map updates - simplified with unified search
  observeEvent({
    input$use_comparison_country_scale; input$map_1_variable_choice; input$map_2_variable_choice;
    input$comparison_country_search
  }, {
    req(input$indicator_category)
    use_local <- isTRUE(input$use_comparison_country_scale)
    update_comparison_map("compare_map_1", map_1_selected_var, use_local)
    update_comparison_map("compare_map_2", map_2_selected_var, use_local)
  })
  
  # Scale change handlers - simplified
  observeEvent({input$use_country_specific_scale}, {
    current_map_for_country("map")
    selected_country(input$country_search)
  })
  
  observeEvent({input$use_comparison_country_scale}, {
    current_map_for_country("compare_map_1")
  })
  
  # Country selection and dataset management
  observeEvent(input$country_select, {
    chosen_country(input$country_select)
    country_dataset(filter(df, COUNTRY == input$country_select))
  })
  
  observe({
    req(chosen_country())
    updateSelectInput(session, inputId = "country_select", selected = chosen_country())
  })
  
  output$countryDisplay <- renderText({
    if (is.null(chosen_country())) "No country selected" else chosen_country()
  })
  
  # UI components - simplified
  output$global_or_country_components <- renderUI({
    if (input$global_or_country == "global") {
      create_global_analysis_ui()
    } else if (input$global_or_country == "country") {
      create_country_analysis_ui()
    }
  })
  
  create_global_analysis_ui <- function() {
    tagList(
      tags$h4("Global Bivariate Analysis Setup", style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
      create_indicator_input("first_indicator_global", "Choose your first indicator", global_level_choices, "Gov_effect.sc", "first_indicator_global_description"),
      create_indicator_input("second_indicator_global", "Choose your second indicator", global_level_choices, "le.ineq.log.sc", "second_indicator_global_description")
    )
  }
  
  create_country_analysis_ui <- function() {
    country_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    tagList(
      tags$h4("Country-Level Analysis Setup", style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
      tags$div(style = "margin-bottom: 20px;",
               selectInput("country_select", "Select a Country to Investigate", choices = sort(unique(countryCodes$Country)), selected = "Japan"),
               tags$small("This selection is synchronized with 'Jump to Country' above.", style = "font-style: italic; color: #666;")
      ),
      tags$div(style = "margin-bottom: 15px;",
               tags$h5("Histogram Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
               create_indicator_input("country_histogram_indicator", "Choose an indicator for distribution analysis", country_choices, "povmap.grdi.v1.sc", "country_histogram_indicator_description")
      ),
      tags$div(style = "margin-bottom: 15px;",
               tags$h5("Bivariate Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
               create_indicator_input("first_indicator", "Choose your first indicator", country_choices, "povmap.grdi.v1.sc", "first_indicator_country_description"),
               create_indicator_input("second_indicator", "Choose your second indicator", country_choices, "perc.pop.world.coastal.merit.10m.log.sc", "second_indicator_country_description")
      ),
      tags$p("Results will appear in the 'Custom Graphs' tab.", style = "font-style: italic; text-align: center; margin-top: 20px; color: #666;")
    )
  }
  
  create_indicator_input <- function(input_id, label, choices, selected, description_id) {
    tags$div(style = "margin-bottom: 15px;",
             selectInput(input_id, label, choices = choices, selected = selected),
             tags$small(textOutput(description_id), style = "font-style: italic; color: #666;")
    )
  }
  
  # Plot outputs - simplified
  create_scatter_plot <- function(data, x_col, y_col, choices, title) {
    if (is.null(data) || !(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    if (all(is.na(data[[x_col]])) || all(is.na(data[[y_col]]))) return()
    
    plot(data[[x_col]], data[[y_col]], main = title,
         xlab = names(choices)[choices == x_col],
         ylab = names(choices)[choices == y_col])
  }
  
  output$custom_scatter <- renderPlot({
    country_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    create_scatter_plot(country_dataset(), input$first_indicator, input$second_indicator, country_choices, chosen_country())
  })
  
  output$global_custom_scatter <- renderPlot({
    create_scatter_plot(average_country_nogeo, input$first_indicator_global, input$second_indicator_global, global_level_choices, "Global")
  })
  
  # Correlation calculation - unified
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
  
  output$correlation <- renderText({
    calculate_correlation(country_dataset(), input$first_indicator, input$second_indicator)
  })
  
  output$global_correlation <- renderText({
    calculate_correlation(average_country_nogeo, input$first_indicator_global, input$second_indicator_global)
  })
  
  # Histogram output
  output$country_histogram <- renderPlot({
    data <- country_dataset()
    if (is.null(data) || nrow(data) == 0) return() 
    
    chi <- input$country_histogram_indicator
    if (!(chi %in% names(data))) return()
    
    col <- data[[chi]][!is.na(data[[chi]])]
    if (length(col) <= 1 || !is.numeric(col)) return()
    
    country_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    label <- names(country_choices)[country_choices == chi]
    hist(col, main = paste0("Histogram of ", label, " for ", chosen_country()), xlab = label)
  })
  
  # Country flag output
  output$country_flag <- renderImage({
    flag_config <- if (is.null(chosen_country())) {
      list(src = "www/globe.png", width = 120, height = 120, alt = "Globe")
    } else {
      list(src = findPNGpath(chosen_country(), countryCodes), width = 160, height = 120, alt = "Country flag")
    }
    c(flag_config, list(contentType = "image/png"))
  }, deleteFile = FALSE)
  
  # Data description management - simplified
  description_mappings <- list(
    list("second_indicator_global", description_reactives$second_global),
    list("first_indicator_global", description_reactives$first_global),
    list("first_indicator", description_reactives$first_country),
    list("second_indicator", description_reactives$second_country),
    list("country_histogram_indicator", description_reactives$histogram)
  )
  
  lapply(description_mappings, function(mapping) {
    observeEvent(input[[mapping[[1]]]], {
      mapping[[2]](input[[mapping[[1]]]])
    })
  })
  
  get_description <- function(reactive_var) {
    req(reactive_var())
    inequity_data_descriptions %>%
      filter(variable_name == reactive_var()) %>%
      pull(description)
  }
  
  # Description outputs
  output$first_indicator_country_description <- renderText(get_description(description_reactives$first_country))
  output$second_indicator_country_description <- renderText(get_description(description_reactives$second_country))
  output$first_indicator_global_description <- renderText(get_description(description_reactives$first_global))
  output$second_indicator_global_description <- renderText(get_description(description_reactives$second_global))
  output$country_histogram_indicator_description <- renderText(get_description(description_reactives$histogram))
  
  # Static outputs
  output$dataplus_logo <- renderImage({
    list(src = "www/data-plus-logo.png", contentType = "image/png", alt = "data_plus", width = 300, height = 120)
  }, deleteFile = FALSE)
  
  # Comparison maps
  output$compare_map_1 <- renderLeaflet({
    var <- map_1_selected_var()
    req(var %in% colnames(average_country_nogeo))
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    map <- create_base_map()
    
    map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
      ) %>%
      {if (should_show_points(var)) {
        addCircleMarkers(., 
                         data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                         stroke = TRUE, color = "white", weight = 1,
                         label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                         layerId = ~paste0("marker_", COUNTRY), group = "markers"
        )
      } else . } %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = paste(input$map_1_indicator_category), position = "bottomright")
  })
  
  output$compare_map_2 <- renderLeaflet({
    var <- map_2_selected_var()
    req(var %in% colnames(average_country_nogeo))
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    map <- create_base_map()
    
    map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
      ) %>%
      {if (should_show_points(var)) {
        addCircleMarkers(., 
                         data = global_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
                         stroke = TRUE, color = "white", weight = 1,
                         label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
                         layerId = ~paste0("marker_", COUNTRY), group = "markers"
        )
      } else . } %>%
      addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                title = paste(input$map_2_indicator_category), position = "bottomright")
  })
}
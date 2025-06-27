# server.R
server <- function(input, output, session) {
  
  # Reactive values for tracking state
  selected_country <- reactiveVal(NULL)
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
    var %in% c("mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc", 
               "perc.pop.world.coastal.merit.10m.log.sc", "Nutritional.dependence.sc",
               "vulnerab.score.rank")
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
      maxBounds = list(list(-90, -180), list(90, 180)),
      maxBoundsViscosity = 1.0,
      minZoom = 2,
      maxZoom = 18,
      worldCopyJump = FALSE
    ))
    if (satellite) {
      map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
  }
  
  # Update variable choices
  observeEvent(input$indicator_category, {
    updateSelectInput(session, "variable_choice", choices = indicator_choice_list[[input$indicator_category]])
  })
  
  observeEvent(input$map_1_indicator_category, {
    updateSelectInput(session, "map_1_variable_choice", choices = indicator_choice_list[[input$map_1_indicator_category]])
  })
  
  observeEvent(input$map_2_indicator_category, {
    updateSelectInput(session, "map_2_variable_choice", choices = indicator_choice_list[[input$map_2_indicator_category]])
  })
  
  # Update country search choices
  observe({
    countries_list <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "comparison_country_search", choices = countries_list, server = TRUE)
    
    # Send countries list to JavaScript for search functionality
    session$sendCustomMessage("updateCountriesList", countries_list)
  })
  
  # Unified function to handle country selection from any source
  select_country <- function(country) {
    if (is.null(country) || country == "" || country == "Global (Default)") {
      selected_country(NULL)
      updateTextInput(session, "country_search", value = "")
      zoom_to_country("map", NULL)
      render_global_map()
    } else {
      selected_country(country)
      updateTextInput(session, "country_search", value = country)
      zoom_to_country("map", country)
      render_country_map(country)
    }
  }
  
  # Helper function to render global map
  render_global_map <- function() {
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
  
  # Helper function to render country-specific map
  render_country_map <- function(country) {
    var <- selected_var()
    
    if (!should_show_points(var)) {
      # For governance/inequality: highlight selected country
      if (var %in% composite_arith_list) {
        global_data <- combined_scores_global
        polygon_data <- combined_scores_global_polygons
      } else {
        global_data <- average_country_nogeo
        polygon_data <- average_country_polygons
      }
      
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      
      selected_country_data <- polygon_data %>% filter(COUNTRY == country)
      other_countries_data <- polygon_data %>% filter(COUNTRY != country)
      
      leafletProxy("map") %>%
        clearMarkers() %>% clearShapes() %>% clearControls()
      
      if (nrow(other_countries_data) > 0) {
        leafletProxy("map") %>%
          addPolygons(
            data = other_countries_data,
            fillColor = "transparent", fillOpacity = 0, 
            color = ~pal(get(var)), weight = 2, opacity = 0.5,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          )
      }
      
      if (nrow(selected_country_data) > 0) {
        leafletProxy("map") %>%
          addPolygons(
            data = selected_country_data,
            fillColor = ~pal(get(var)), fillOpacity = 0.8,
            color = ~pal(get(var)), weight = 3, opacity = 1,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          )
      }
      
      leafletProxy("map") %>%
        addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
    } else {
      # For socio-ecological: show individual country points
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0) {
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
        
        leafletProxy("map") %>%
          clearMarkers() %>% clearShapes() %>% clearControls() %>%
          addPolygons(
            data = polygon_data,
            fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
            weight = 1, opacity = 0.4,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          ) %>%
          addCircleMarkers(
            data = country_data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
            stroke = TRUE, color = "black", weight = 0.7,
            label = ~paste0(COUNTRY, ": ", round(get(var), 3))
          ) %>%
          addLegend(pal = pal, values = domain_data, opacity = 0.9,
                    title = paste0(country, if (use_local) " (Local Scale)" else " (Global Scale)"),
                    position = "bottomright")
      }
    }
  }
  
  # Handle text search input and selection
  observeEvent(input$country_search_selected, {
    select_country(input$country_search_selected)
  })
  
  # Global view button handler
  observeEvent(input$global_view_button, {
    select_country(NULL)
  })
  
  # Click handlers - now use unified selection
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!is.null(clicked_country)) {
      select_country(clicked_country)
    }
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- gsub("^marker_", "", input$map_marker_click$id)
    select_country(clicked_country)
  })
  
  # Main map rendering
  output$map <- renderLeaflet({
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
  
  # Comparison Map 1
  output$comp_map1 <- renderLeaflet({
    req(input$map_1_variable_choice)
    var <- input$map_1_variable_choice
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    req(var %in% colnames(global_data))
    
    pal <- colorNumeric("Blues", domain = global_data[[var]], na.color = "transparent")
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
                title = "Map 1", position = "bottomright")
  })
  
  # Comparison Map 2
  output$comp_map2 <- renderLeaflet({
    req(input$map_2_variable_choice)
    var <- input$map_2_variable_choice
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    req(var %in% colnames(global_data))
    
    pal <- colorNumeric("Reds", domain = global_data[[var]], na.color = "transparent")
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
                title = "Map 2", position = "bottomright")
  })
  
  # Update map tiles when satellite view changes
  observeEvent(input$satellite_view, {
    tiles <- if (input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  # Update main map when variable changes
  observeEvent({
    input$use_country_specific_scale; input$variable_choice
  }, {
    req(input$indicator_category)
    country <- selected_country()
    
    if (is.null(country)) {
      render_global_map()
    } else {
      render_country_map(country)
    }
  })
  
  # Update comparison maps when variables change
  observeEvent(input$map_1_variable_choice, {
    req(input$map_1_variable_choice)
    var <- input$map_1_variable_choice
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    if (var %in% colnames(global_data)) {
      pal <- colorNumeric("Blues", domain = global_data[[var]], na.color = "transparent")
      
      leafletProxy("comp_map1") %>%
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
                  title = "Map 1", position = "bottomright")
    }
  })
  
  observeEvent(input$map_2_variable_choice, {
    req(input$map_2_variable_choice)
    var <- input$map_2_variable_choice
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    if (var %in% colnames(global_data)) {
      pal <- colorNumeric("Reds", domain = global_data[[var]], na.color = "transparent")
      
      leafletProxy("comp_map2") %>%
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
                  title = "Map 2", position = "bottomright")
    }
  })
  
  # Country selection and dataset management
  observeEvent(input$country_select, {
    if (!is.null(input$country_select)) {
      chosen_country(input$country_select)
      country_dataset(filter(df, COUNTRY == input$country_select))
    }
  })
  
  # Sync country selection
  observe({
    if (!is.null(selected_country())) {
      chosen_country(selected_country())
      country_dataset(filter(df, COUNTRY == selected_country()))
    }
  })
  
  output$countryDisplay <- renderText({
    if (is.null(chosen_country())) "No country selected" else chosen_country()
  })
  
  # UI components
  output$global_or_country_components <- renderUI({
    if (input$global_or_country == "global") {
      create_global_analysis_ui()
    } else if (input$global_or_country == "country") {
      create_country_analysis_ui()
    }
  })
  
  create_global_analysis_ui <- function() {
    tagList(
      selectInput("first_indicator_global", "First indicator:", 
                  choices = global_level_choices, selected = "Gov_effect.sc"),
      selectInput("second_indicator_global", "Second indicator:", 
                  choices = global_level_choices, selected = "le.ineq.log.sc")
    )
  }
  
  create_country_analysis_ui <- function() {
    country_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    tagList(
      selectInput("country_select", "Select Country:", 
                  choices = sort(unique(countryCodes$Country)), selected = "Japan"),
      selectInput("country_histogram_indicator", "Histogram variable:", 
                  choices = country_choices, selected = "povmap.grdi.v1.sc"),
      selectInput("first_indicator", "First indicator:", 
                  choices = country_choices, selected = "povmap.grdi.v1.sc"),
      selectInput("second_indicator", "Second indicator:", 
                  choices = country_choices, selected = "perc.pop.world.coastal.merit.10m.log.sc")
    )
  }
  
  # Plot outputs
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
  
  # Correlation calculation
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
}
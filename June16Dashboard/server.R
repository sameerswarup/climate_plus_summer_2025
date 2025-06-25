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
  
  get_map_data <- function(var) {
    if (var %in% composite_arith_list) {
      list(global = combined_scores_global, polygons = combined_scores_global_polygons)
    } else {
      list(global = average_country_nogeo, polygons = average_country_polygons)
    }
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
  
  add_polygons_to_map <- function(proxy, polygon_data, var, pal, opacity_fill = 0.7, weight = 2, group = "polygons") {
    proxy %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)), fillOpacity = opacity_fill, color = ~pal(get(var)),
        weight = weight, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = group
      )
  }
  
  add_markers_to_map <- function(proxy, data, var, pal, group = "markers") {
    proxy %>%
      addCircleMarkers(
        data = data, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
        stroke = TRUE, color = "white", weight = 1,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
        layerId = ~paste0("marker_", COUNTRY), group = group
      )
  }
  
  render_country_highlighting <- function(map_id, map_data, var, selected_country_name) {
    pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
    
    leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    # Add other countries as transparent
    other_countries <- map_data$polygons %>% filter(COUNTRY != selected_country_name)
    if (nrow(other_countries) > 0) {
      leafletProxy(map_id) %>%
        addPolygons(
          data = other_countries, fillColor = "transparent", fillOpacity = 0,
          color = ~pal(get(var)), weight = 2, opacity = 0.5,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        )
    }
    
    # Add selected country as highlighted
    selected_data <- map_data$polygons %>% filter(COUNTRY == selected_country_name)
    if (nrow(selected_data) > 0) {
      leafletProxy(map_id) %>%
        addPolygons(
          data = selected_data, fillColor = ~pal(get(var)), fillOpacity = 0.8,
          color = ~pal(get(var)), weight = 3, opacity = 1,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        )
    }
    
    leafletProxy(map_id) %>%
      addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                title = paste(ifelse(grepl("compare", map_id), paste("Map", substr(map_id, nchar(map_id), nchar(map_id))), input$indicator_category)),
                position = "bottomright")
  }
  
  # Update variable choices - consolidated
  map(c("indicator_category", "map_1_indicator_category", "map_2_indicator_category"), function(input_id) {
    output_id <- case_when(
      input_id == "indicator_category" ~ "variable_choice",
      input_id == "map_1_indicator_category" ~ "map_1_variable_choice",
      input_id == "map_2_indicator_category" ~ "map_2_variable_choice"
    )
    observeEvent(input[[input_id]], {
      updateSelectInput(session, output_id, choices = indicator_choice_list[[input[[input_id]]]])
    })
  })
  
  # Update country search choices - consolidated
  observe({
    choices <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    map(c("country_search", "map_1_country_search", "map_2_country_search"), function(input_id) {
      updateSelectizeInput(session, input_id, choices = choices, server = TRUE)
    })
  })
  
  # Country search handlers with zoom - consolidated
  map(list(
    list(input = "map_1_country_search", map = "compare_map_1"),
    list(input = "map_2_country_search", map = "compare_map_2")
  ), function(config) {
    observeEvent(input[[config$input]], {
      current_map_for_country(config$map)
      country <- input[[config$input]]
      zoom_to_country(config$map, country)
      selected_country(if (country == "Global (Default)") NULL else country)
    })
  })
  
  # Main country selection handler
  observeEvent(input$country_search, {
    current_map_for_country("map")
    
    if (input$country_search != "Global (Default)" && !is.null(input$country_search)) {
      updateSelectInput(session, "country_select", selected = input$country_search)
      selected_country(input$country_search)
    } else {
      selected_country(NULL)
      var <- selected_var()
      map_data <- get_map_data(var)
      pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
      
      proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes() %>% clearControls()
      add_polygons_to_map(proxy, map_data$polygons, var, pal)
      
      if (should_show_points(var)) {
        add_markers_to_map(proxy, map_data$global, var, pal)
      }
      
      proxy %>% addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                          title = paste(input$indicator_category), position = "bottomright")
    }
  })
  
  observeEvent(input$country_select, {
    current_map_for_country("map")
    if (!is.null(input$country_select)) {
      updateSelectizeInput(session, "country_search", selected = input$country_select)
    }
  })
  
  # Click handlers - consolidated with map function
  click_handlers <- list(
    list(event = "map_shape_click", map = "map", search = "country_search", select = "country_select"),
    list(event = "compare_map_1_shape_click", map = "compare_map_1", search = "map_1_country_search", select = NULL),
    list(event = "compare_map_2_shape_click", map = "compare_map_2", search = "map_2_country_search", select = NULL)
  )
  
  map(click_handlers, function(handler) {
    observeEvent(input[[handler$event]], {
      clicked_country <- input[[handler$event]]$id
      if (!is.null(clicked_country)) {
        if (handler$map == "map") {
          selected_country(clicked_country)
          if (!is.null(handler$select)) updateSelectInput(session, handler$select, selected = clicked_country)
        }
        updateSelectizeInput(session, handler$search, selected = clicked_country)
        zoom_to_country(handler$map, clicked_country)
      }
    })
  })
  
  # Marker click handlers
  marker_handlers <- list(
    list(event = "map_marker_click", target = "selected_country"),
    list(event = "compare_map_1_marker_click", target = "map_1_country_search"),
    list(event = "compare_map_2_marker_click", target = "map_2_country_search")
  )
  
  map(marker_handlers, function(handler) {
    observeEvent(input[[handler$event]], {
      clicked_country <- gsub("^marker_", "", input[[handler$event]]$id)
      if (handler$target == "selected_country") {
        selected_country(clicked_country)
      } else {
        updateSelectizeInput(session, handler$target, selected = clicked_country)
        zoom_to_country(sub("_country_search", "", handler$target), clicked_country)
      }
    })
  })
  
  # Mouse events
  observeEvent(input$map_shape_mouseover, hovered_country(input$map_shape_mouseover$id))
  observeEvent(input$map_shape_mouseout, hovered_country(NULL))
  
  # Main map rendering
  output$map <- renderLeaflet({
    var <- selected_var()
    map_data <- get_map_data(var)
    req(var %in% colnames(map_data$global))
    
    pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
    map <- create_base_map(input$satellite_view)
    add_polygons_to_map(map, map_data$polygons, var, pal)
    
    if (should_show_points(var)) {
      add_markers_to_map(map, map_data$global, var, pal)
    }
    
    map %>% addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                      title = paste(input$indicator_category), position = "bottomright")
  })
  
  # Update map tiles when satellite view changes
  observeEvent(input$satellite_view, {
    tiles <- if (input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  # Unified map update logic
  update_map_display <- function(map_id, country, var, use_local = FALSE) {
    map_data <- get_map_data(var)
    
    if (is.null(country) || country == "Global (Default)") {
      pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
      proxy <- leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearControls()
      add_polygons_to_map(proxy, map_data$polygons, var, pal)
      
      if (should_show_points(var)) {
        add_markers_to_map(proxy, map_data$global, var, pal)
      }
      
      proxy %>% addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                          title = paste(input$indicator_category), position = "bottomright")
    } else if (!should_show_points(var)) {
      render_country_highlighting(map_id, map_data, var, country)
    } else {
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0) {
        domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
        pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
        border_pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
        
        leafletProxy(map_id) %>%
          clearMarkers() %>% clearShapes() %>% clearControls() %>%
          addPolygons(
            data = map_data$polygons, fillColor = "transparent", fillOpacity = 0,
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
                    title = paste0(country, if (use_local) " (Local Scale)" else " (Global Scale)"),
                    position = "bottomright")
      }
    }
  }
  
  # Update main map when selections change
  observeEvent({
    input$map_1_country_search; input$map_2_country_search; input$country_search; input$use_country_specific_scale
  }, {
    req(input$indicator_category)
    update_map_display("map", selected_country(), selected_var(), isTRUE(input$use_country_specific_scale))
  })
  
  # Comparison map update logic
  update_comparison_map <- function(map_id, var_func, search_input, use_local) {
    var <- var_func()
    country <- if (is.null(search_input) || search_input == "Global (Default)") NULL else search_input
    update_map_display(map_id, country, var, use_local)
  }
  
  # Handle comparison map updates
  observeEvent({
    input$use_comparison_country_scale; input$map_1_variable_choice; input$map_2_variable_choice;
    input$map_1_country_search; input$map_2_country_search
  }, {
    req(input$indicator_category)
    use_local <- isTRUE(input$use_comparison_country_scale)
    update_comparison_map("compare_map_1", map_1_selected_var, input$map_1_country_search, use_local)
    update_comparison_map("compare_map_2", map_2_selected_var, input$map_2_country_search, use_local)
  })
  
  # Scale change handlers
  observeEvent({input$use_country_specific_scale}, {
    current_map_for_country("map")
    selected_country(input$country_search)
  })
  
  observeEvent({input$use_comparison_country_scale}, {
    current_map_for_country("compare_map_1")
  })
  
  observeEvent(input$zoom_button, {
    zoom_to_country("map", if (input$country_search == "Global (Default)") NULL else selected_country())
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
  
  map(description_mappings, function(mapping) {
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
    
    pal <- colorNumeric("Purples", domain = average_country_nogeo[[var]], na.color = "transparent")
    map <- create_base_map()
    add_polygons_to_map(map, average_country_polygons, var, pal)
    
    if (should_show_points(var)) {
      add_markers_to_map(map, average_country_nogeo, var, pal)
    }
    
    map %>% addLegend(pal = pal, values = average_country_nogeo[[var]], opacity = 0.8,
                      title = paste(input$map_1_indicator_category), position = "bottomright")
  })
  
  output$compare_map_2 <- renderLeaflet({
    var <- map_2_selected_var()
    req(var %in% colnames(average_country_nogeo))
    
    pal <- colorNumeric("Purples", domain = average_country_nogeo[[var]], na.color = "transparent")
    map <- create_base_map()
    add_polygons_to_map(map, average_country_polygons, var, pal)
    
    if (should_show_points(var)) {
      add_markers_to_map(map, average_country_nogeo, var, pal)
    }
    
    map %>% addLegend(pal = pal, values = average_country_nogeo[[var]], opacity = 0.8,
                      title = paste(input$map_2_indicator_category), position = "bottomright")
  })
}
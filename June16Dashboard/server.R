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
  clicked_score_first_global <- reactiveVal(NULL)
  clicked_score_second_global <- reactiveVal(NULL)
  clicked_score_first_country <- reactiveVal(NULL)
  clicked_score_second_country <- reactiveVal(NULL)
  clicked_score_country_histogram <- reactiveVal(NULL)
  
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
  selected_var <- reactive({
    req(input$variable_choice)
    input$variable_choice
  })
  
  map_1_selected_var <- reactive({
    req(input$map_1_variable_choice)
    input$map_1_variable_choice
  })
  
  map_2_selected_var <- reactive({
    req(input$map_2_variable_choice)
    input$map_2_variable_choice
  })
  
  # Helper function to get data and polygons based on variable type
  get_map_data <- function(var) {
    if (var %in% composite_arith_list) {
      list(global = combined_scores_global, polygons = combined_scores_global_polygons)
    } else {
      list(global = average_country_nogeo, polygons = average_country_polygons)
    }
  }
  
  # Helper function to zoom to country
  zoom_to_country <- function(map_id, country) {
    if (is.null(country) || country == "Global (Default)") {
      leafletProxy(map_id) %>% setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      as.list()
    
    if (length(zoom_coords$X) > 0) {
      leafletProxy(map_id) %>% setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
    }
  }
  
  # Helper function to determine if points should be shown
  should_show_points <- function(var) {
    var %in% c("vulnerab.score.rank", "mean.count.grav.V2.log.sc", "povmap.grdi.v1.sc", 
               "perc.pop.world.coastal.merit.10m.log.sc", "Nutritional.dependence.sc")
  }
  
  # Helper function to create leaflet map with conditional layers
  create_map_with_layers <- function(global_data, polygon_data, var, title, satellite = FALSE) {
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    map <- leaflet()
    
    if (satellite) {
      map <- map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
    
    map <- map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.7,
        color = ~pal(get(var)),
        weight = 2,
        opacity = 0.9,
        highlightOptions = highlightOptions(
          color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8
        ),
        layerId = ~COUNTRY,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
        group = "polygons"
      )
    
    # Only add circle markers for Socio-Ecological Vulnerability variables
    if (should_show_points(var)) {
      map <- map %>%
        addCircleMarkers(
          data = global_data,
          radius = 6,
          fillColor = ~pal(get(var)),
          fillOpacity = 0.9,
          stroke = TRUE,
          color = "white",
          weight = 1,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
          layerId = ~paste0("marker_", COUNTRY),
          group = "markers"
        )
    }
    
    map %>%
      addLegend(
        pal = pal,
        values = global_data[[var]],
        opacity = 0.8,
        title = title,
        position = "bottomright"
      )
  }
  
  # Update variable choices - consolidated
  lapply(c("indicator_category", "map_1_indicator_category", "map_2_indicator_category"), function(input_id) {
    output_id <- switch(input_id,
                        "indicator_category" = "variable_choice",
                        "map_1_indicator_category" = "map_1_variable_choice", 
                        "map_2_indicator_category" = "map_2_variable_choice")
    
    observeEvent(input[[input_id]], {
      updateSelectInput(session, output_id, choices = indicator_choice_list[[input[[input_id]]]])
    })
  })
  
  # Update country search choices - consolidated
  observe({
    choices <- list("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "country_search", choices = choices, server = TRUE)
    updateSelectizeInput(session, "map_1_country_search", choices = choices, server = TRUE)
    updateSelectizeInput(session, "map_2_country_search", choices = choices, server = TRUE)
  })
  
  # Country search handlers - consolidated
  lapply(list(
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
  
  # Handle country selection synchronization
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
      leafletProxy("map") %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = map_data$polygons,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        )
      
      # Only add markers for Socio-Ecological Vulnerability variables
      if (should_show_points(var)) {
        leafletProxy("map") %>%
          addCircleMarkers(
            data = map_data$global, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
            stroke = TRUE, color = "white", weight = 1,
            label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
            layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
      }
      
      leafletProxy("map") %>%
        addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
    }
  })
  
  observeEvent(input$country_select, {
    current_map_for_country("map")
    if (!is.null(input$country_select)) {
      updateSelectizeInput(session, "country_search", selected = input$country_select)
    }
  })
  
  # Handle polygon and marker clicks - expanded for comparison maps
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!is.null(clicked_country)) {
      selected_country(clicked_country)
      updateSelectizeInput(session, "country_search", selected = clicked_country)
      updateSelectInput(session, "country_select", selected = clicked_country)
      zoom_to_country("map", clicked_country)
    }
  })
  
  # Add click handlers for comparison maps
  observeEvent(input$compare_map_1_shape_click, {
    clicked_country <- input$compare_map_1_shape_click$id
    if (!is.null(clicked_country)) {
      updateSelectizeInput(session, "map_1_country_search", selected = clicked_country)
      zoom_to_country("compare_map_1", clicked_country)
    }
  })
  
  observeEvent(input$compare_map_2_shape_click, {
    clicked_country <- input$compare_map_2_shape_click$id
    if (!is.null(clicked_country)) {
      updateSelectizeInput(session, "map_2_country_search", selected = clicked_country)
      zoom_to_country("compare_map_2", clicked_country)
    }
  })
  
  # Add marker click handlers for comparison maps
  observeEvent(input$compare_map_1_marker_click, {
    clicked_country <- input$compare_map_1_marker_click$id
    if (!is.null(clicked_country)) {
      # Remove "marker_" prefix if present
      country_name <- gsub("^marker_", "", clicked_country)
      updateSelectizeInput(session, "map_1_country_search", selected = country_name)
      zoom_to_country("compare_map_1", country_name)
    }
  })
  
  observeEvent(input$compare_map_2_marker_click, {
    clicked_country <- input$compare_map_2_marker_click$id
    if (!is.null(clicked_country)) {
      # Remove "marker_" prefix if present
      country_name <- gsub("^marker_", "", clicked_country)
      updateSelectizeInput(session, "map_2_country_search", selected = country_name)
      zoom_to_country("compare_map_2", country_name)
    }
  })
  
  observeEvent(input$map_shape_mouseover, {
    hovered_country(input$map_shape_mouseover$id)
  })
  
  observeEvent(input$map_shape_mouseout, {
    hovered_country(NULL)
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- input$map_marker_click$id
    selected_country(clicked_country)
  })
  
  # Main map rendering
  output$map <- renderLeaflet({
    var <- selected_var()
    map_data <- get_map_data(var)
    req(var %in% colnames(map_data$global))
    
    create_map_with_layers(
      map_data$global, 
      map_data$polygons, 
      var, 
      paste(input$indicator_category), 
      input$satellite_view
    )
  })
  
  # Update map tiles when satellite view changes
  observeEvent(input$satellite_view, {
    if (input$satellite_view) {
      leafletProxy("map") %>% clearTiles() %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      leafletProxy("map") %>% clearTiles() %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
  })
  
  # Function for drawing maps with proper layering
  draw_map <- function(current_map_for_country, domain_data, use_local, country_data, var, country) {
    pal <- colorNumeric(palette = "Purples", domain = domain_data, na.color = "transparent")
    map_data <- get_map_data(var)
    border_pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
    
    if (country == "Global (Default)" || is.null(country) || country == "") {
      # Global view with shaded countries and markers
      leafletProxy(current_map_for_country) %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = map_data$polygons,
          fillColor = ~border_pal(get(var)), fillOpacity = 0.7, color = ~border_pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        ) %>%
        addCircleMarkers(
          data = map_data$global, radius = 6, fillColor = ~border_pal(get(var)), fillOpacity = 0.9,
          stroke = TRUE, color = "white", weight = 1,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
          layerId = ~paste0("marker_", COUNTRY), group = "markers"
        ) %>%
        addLegend(pal = border_pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
    } else {
      # Country-specific view with transparent borders and country points
      leafletProxy(current_map_for_country) %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = map_data$polygons,
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
    }
  }
  
  # Update map when selections change
  observeEvent({
    input$map_1_country_search; input$map_2_country_search; input$country_search; input$use_country_specific_scale
  }, {
    req(input$indicator_category)
    country <- selected_country()
    var <- selected_var()
    
    if (is.null(country)) {
      map_data <- get_map_data(var)
      pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
      leafletProxy(current_map_for_country()) %>%
        clearMarkers() %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = map_data$polygons,
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        )
      
      # Only add markers for Socio-Ecological Vulnerability variables
      if (should_show_points(var)) {
        leafletProxy(current_map_for_country()) %>%
          addCircleMarkers(
            data = map_data$global, radius = 6, fillColor = ~pal(get(var)), fillOpacity = 0.9,
            stroke = TRUE, color = "white", weight = 1,
            label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
            layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
      }
      
      leafletProxy(current_map_for_country()) %>%
        addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
      return()
    }
    
    # When a country is selected, check if we should show points
    if (!should_show_points(var)) {
      # For governance/inequality: highlight selected country, make others transparent with borders
      map_data <- get_map_data(var)
      pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
      
      # Get the selected country's data for highlighting
      selected_country_data <- map_data$polygons %>% filter(COUNTRY == country)
      other_countries_data <- map_data$polygons %>% filter(COUNTRY != country)
      
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
        addLegend(pal = pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste(input$indicator_category), position = "bottomright")
      return()
    }
    
    # Only for socio-ecological vulnerability: show individual country points
    country_data <- df %>% filter(COUNTRY == country)
    req(nrow(country_data) > 0)
    
    use_local <- isTRUE(input$use_country_specific_scale)
    domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
    draw_map(current_map_for_country(), domain_data, use_local, country_data, var, country)
  })
  
  # Handle comparison map updates - consolidated
  observeEvent({
    input$use_comparison_country_scale; input$map_1_variable_choice; input$map_2_variable_choice;
    input$map_1_country_search; input$map_2_country_search
  }, {  
    req(input$indicator_category)
    
    use_local <- isTRUE(input$use_comparison_country_scale)
    
    get_country_data <- function(search_input) {
      if (is.null(search_input) || search_input == "Global (Default)" || search_input == "") {
        average_country_nogeo
      } else {
        df %>% filter(COUNTRY == search_input)
      }
    }
    
    country_1_data <- get_country_data(input$map_1_country_search)
    country_2_data <- get_country_data(input$map_2_country_search)
    
    domain_data <- if (use_local && !is.null(country_1_data) && !is.null(country_2_data)) {
      c(country_1_data[[map_1_selected_var()]], country_2_data[[map_2_selected_var()]])
    } else {
      c(average_country_nogeo[[map_1_selected_var()]], average_country_nogeo[[map_2_selected_var()]])
    }
    
    # Update both comparison maps with new logic
    update_comparison_map("compare_map_1", domain_data, use_local, country_1_data, map_1_selected_var(), input$map_1_country_search)
    update_comparison_map("compare_map_2", domain_data, use_local, country_2_data, map_2_selected_var(), input$map_2_country_search)
  })
  
  # New function to handle comparison map updates with highlighting
  update_comparison_map <- function(map_id, domain_data, use_local, country_data, var, selected_country_name) {
    map_data <- get_map_data(var)
    pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
    border_pal <- colorNumeric("Purples", domain = map_data$global[[var]], na.color = "transparent")
    
    # Clear existing layers
    leafletProxy(map_id) %>%
      clearMarkers() %>% clearShapes() %>% clearControls()
    
    # If no country selected or "Global (Default)", show all countries
    if (is.null(selected_country_name) || selected_country_name == "Global (Default)" || selected_country_name == "") {
      leafletProxy(map_id) %>%
        addPolygons(
          data = map_data$polygons,
          fillColor = ~border_pal(get(var)), fillOpacity = 0.7, color = ~border_pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "polygons"
        )
      
      # Only add markers for Socio-Ecological Vulnerability variables
      if (should_show_points(var)) {
        leafletProxy(map_id) %>%
          addCircleMarkers(
            data = map_data$global, radius = 6, fillColor = ~border_pal(get(var)), fillOpacity = 0.9,
            stroke = TRUE, color = "white", weight = 1,
            label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
            layerId = ~paste0("marker_", COUNTRY), group = "markers"
          )
      }
      
      leafletProxy(map_id) %>%
        addLegend(pal = border_pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste("Map", substr(map_id, nchar(map_id), nchar(map_id))), position = "bottomright")
      return()
    }
    
    # Country is selected - check if we should show points or highlighting
    if (!should_show_points(var)) {
      # For governance/inequality: highlight selected country, make others transparent
      selected_country_data <- map_data$polygons %>% filter(COUNTRY == selected_country_name)
      other_countries_data <- map_data$polygons %>% filter(COUNTRY != selected_country_name)
      
      # Add other countries as transparent with borders only
      if (nrow(other_countries_data) > 0) {
        leafletProxy(map_id) %>%
          addPolygons(
            data = other_countries_data,
            fillColor = "transparent", fillOpacity = 0,
            color = ~border_pal(get(var)), weight = 2, opacity = 0.5,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "other_polygons"
          )
      }
      
      # Add selected country as highlighted/shaded
      if (nrow(selected_country_data) > 0) {
        leafletProxy(map_id) %>%
          addPolygons(
            data = selected_country_data,
            fillColor = ~border_pal(get(var)), fillOpacity = 0.8,
            color = ~border_pal(get(var)), weight = 3, opacity = 1,
            highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
            layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", round(get(var), 3)), group = "selected_polygon"
          )
      }
      
      leafletProxy(map_id) %>%
        addLegend(pal = border_pal, values = map_data$global[[var]], opacity = 0.8,
                  title = paste("Map", substr(map_id, nchar(map_id), nchar(map_id))), position = "bottomright")
    } else {
      # For socio-ecological vulnerability: show individual points with transparent borders
      leafletProxy(map_id) %>%
        addPolygons(
          data = map_data$polygons,
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
                  title = paste0(selected_country_name, if (use_local) " (Local Scale)" else " (Global Scale)"),
                  position = "bottomright")
    }
  }
  
  # Handle scale changes - simplified
  observeEvent(input$use_comparison_country_scale, {
    req(input$indicator_category)
    
    if (current_map_for_country() == "map") {
      country <- if (input$country_search == "Global (Default)") NULL else input$country_search
      if (is.null(country)) {
        draw_map(current_map_for_country(), average_country_nogeo[[selected_var()]], FALSE, average_country_nogeo, selected_var(), "Global (Default)")
        return()
      }
      country_data <- df %>% filter(COUNTRY == country)
      req(nrow(country_data) > 0)
      use_local <- isTRUE(input$use_country_specific_scale)
      domain_data <- if (use_local) country_data[[selected_var()]] else average_country_nogeo[[selected_var()]]
      draw_map(current_map_for_country(), domain_data, use_local, country_data, selected_var(), country)
    }
  })
  
  observeEvent({input$use_country_specific_scale}, {
    current_map_for_country("map")
    selected_country(input$country_search)
    if (!is.null(selected_country())) {
      selected_country(selected_country())
    }
  })
  
  observeEvent({input$use_comparison_country_scale}, {
    current_map_for_country("compare_map_1")
  })
  
  observeEvent(input$zoom_button, {
    which_country <- if (input$country_search == "Global (Default)") NULL else selected_country()
    zoom_to_country("map", which_country)
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
  
  # Output for country display
  output$countryDisplay <- renderText({
    if (is.null(chosen_country())) "No country selected" else chosen_country()
  })
  
  # UI components
  output$global_or_country_components <- renderUI({
    if (input$global_or_country == "global") {
      tagList(
        tags$h4("Global Bivariate Analysis Setup", style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
        tags$div(style = "margin-bottom: 15px;",
                 selectInput("first_indicator_global", "Choose your first indicator", choices = global_level_choices, selected = "Gov_effect.sc"),
                 tags$small(textOutput("first_indicator_global_description"), style = "font-style: italic; color: #666;")
        ),
        tags$div(style = "margin-bottom: 15px;",
                 selectInput("second_indicator_global", "Choose your second indicator", choices = global_level_choices, selected = "le.ineq.log.sc"),
                 tags$small(textOutput("second_indicator_global_description"), style = "font-style: italic; color: #666;")
        )
      ) 
    } else if (input$global_or_country == "country") {
      tagList(
        tags$h4("Country-Level Analysis Setup", style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
        tags$div(style = "margin-bottom: 20px;",
                 selectInput("country_select", "Select a Country to Investigate", choices = sort(unique(countryCodes$Country)), selected = "Japan"),
                 tags$small("This selection is synchronized with 'Jump to Country' above.", style = "font-style: italic; color: #666;")
        ),
        tags$div(style = "margin-bottom: 15px;",
                 tags$h5("Histogram Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
                 selectInput("country_histogram_indicator", "Choose an indicator for distribution analysis",
                             choices = c("Distance to Coast (km)" = "distance_to_coast_km", 
                                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                         "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"),
                             selected = "povmap.grdi.v1.sc"),
                 tags$small(textOutput("country_histogram_indicator_description"), style = "font-style: italic; color: #666;")
        ),
        tags$div(style = "margin-bottom: 15px;",
                 tags$h5("Bivariate Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
                 tags$div(style = "margin-bottom: 10px;",
                          selectInput("first_indicator", "Choose your first indicator",
                                      choices = c("Distance to Coast (km)" = "distance_to_coast_km", 
                                                  "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                                  "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                                  "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"),
                                      selected = "povmap.grdi.v1.sc"),
                          tags$small(textOutput("first_indicator_country_description"), style = "font-style: italic; color: #666;")
                 ),
                 tags$div(style = "margin-bottom: 10px;",
                          selectInput("second_indicator", "Choose your second indicator",
                                      choices = c("Distance to Coast (km)" = "distance_to_coast_km", 
                                                  "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                                  "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                                  "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"),
                                      selected = "perc.pop.world.coastal.merit.10m.log.sc"),
                          tags$small(textOutput("second_indicator_country_description"), style = "font-style: italic; color: #666;")
                 )
        ),
        tags$p("Results will appear in the 'Custom Graphs' tab.", 
               style = "font-style: italic; text-align: center; margin-top: 20px; color: #666;")
      )
    }
  })
  
  # Plot outputs - simplified
  output$custom_scatter <- renderPlot({
    data <- country_dataset()
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    indicator_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                           "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                           "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                           "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    if (all(is.na(data[[x_col]])) || all(is.na(data[[y_col]]))) return()
    
    plot(data[[x_col]], data[[y_col]], 
         main = chosen_country(),
         xlab = names(indicator_choices)[indicator_choices == x_col],
         ylab = names(indicator_choices)[indicator_choices == y_col])
  })
  
  output$global_custom_scatter <- renderPlot({
    data <- average_country_nogeo
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator_global
    y_col <- input$second_indicator_global
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    if (all(is.na(data[[x_col]])) || all(is.na(data[[y_col]]))) return()
    
    plot(data[[x_col]], data[[y_col]], 
         main = "Global",
         xlab = names(global_level_choices)[global_level_choices == x_col],
         ylab = names(global_level_choices)[global_level_choices == y_col])
  })
  
  # Correlation outputs - consolidated
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
    
    indicator_choices <- c("Distance to Coast (km)" = "distance_to_coast_km", 
                           "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                           "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                           "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
    
    label <- names(indicator_choices)[indicator_choices == chi]
    hist(col, main = paste0("Histogram of ", label, " for ", chosen_country()), xlab = label)
  })
  
  # Country flag output
  output$country_flag <- renderImage({
    if (is.null(chosen_country())) {
      list(src = "www/globe.png", contentType = "image/png", alt = "Globe", width = 120, height = 120)
    } else {
      filename <- findPNGpath(chosen_country(), countryCodes)
      list(src = filename, contentType = "image/png", alt = "Country flag", width = 160, height = 120)
    }
  }, deleteFile = FALSE)
  
  # Data description observers - consolidated
  description_observers <- list(
    list("second_indicator_global", clicked_score_second_global),
    list("first_indicator_global", clicked_score_first_global),
    list("first_indicator", clicked_score_first_country),
    list("second_indicator", clicked_score_second_country),
    list("country_histogram_indicator", clicked_score_country_histogram)
  )
  
  lapply(description_observers, function(config) {
    observeEvent(input[[config[[1]]]], {
      config[[2]](input[[config[[1]]]])
    })
  })
  
  # Description outputs - consolidated
  get_description <- function(clicked_score) {
    req(clicked_score())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score()) %>%
      pull(description)
    return(descriptions)
  }
  
  output$first_indicator_country_description <- renderText({
    get_description(clicked_score_first_country)
  })
  
  output$second_indicator_country_description <- renderText({
    get_description(clicked_score_second_country)
  })
  
  output$first_indicator_global_description <- renderText({
    get_description(clicked_score_first_global)
  })
  
  output$second_indicator_global_description <- renderText({
    get_description(clicked_score_second_global)
  })
  
  output$country_histogram_indicator_description <- renderText({
    get_description(clicked_score_country_histogram)
  })
  
  # Data+ logo
  output$dataplus_logo <- renderImage({
    list(src = "www/data-plus-logo.png", contentType = "image/png", alt = "data_plus", width = 300, height = 120)
  }, deleteFile = FALSE)
  
  # Comparison maps with shaded countries and borders
  output$compare_map_1 <- renderLeaflet({
    var <- map_1_selected_var()
    req(var %in% colnames(average_country_nogeo))
    
    create_map_with_layers(
      average_country_nogeo, 
      average_country_polygons, 
      var, 
      paste(input$map_1_indicator_category)
    )
  })
  
  output$compare_map_2 <- renderLeaflet({
    var <- map_2_selected_var()
    req(var %in% colnames(average_country_nogeo))
    
    create_map_with_layers(
      average_country_nogeo, 
      average_country_polygons, 
      var, 
      paste(input$map_2_indicator_category)
    )
  })
}
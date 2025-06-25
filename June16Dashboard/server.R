# server.R
server <- function(input, output, session) {
  
  # Reactive values for tracking state
  selected_country <- reactiveVal(NULL)
  current_map_for_country <- reactiveVal("map")
  last_zoomed_country <- reactiveVal(NULL)
  hovered_country <- reactiveVal(NULL)
  
  indicator_choice_list <- list(
    "Socio-Ecological Vulnerability" = c("Socio-Ecological Vulnerability (Composite)" = "vulnerab.score.rank",
                                         "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                         "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                         "Coastal Climate Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc",
                                         "Nutritional Dependence" = "Nutritional.dependence.sc"
    ),
    
    "Social Inequality" = c("Social Inequality (Composite)" = "ineq.score.rank",
                            "Gender Inequality" = "gender.ineq.sc",
                            "Income Inequality" = "income.ineq.sc",
                            "Inequality Adjusted Life Expectancy" = "le.ineq.log.sc"
    ),
    
    "Weak Governance" = c("Weak Governance (Composite)" = "gov.score.rank",
                          "Government Ineffectiveness" = "Gov_effect.sc",
                          "Poor Regulatory Quality" = "Reg_quality.sc",
                          "Weak Rule of Law" = "Rule_law.sc",
                          "Weak Control of Corruption" = "control_corr.sc",
                          "Low Voice and Accountability" = "Voice_account.sc",
                          "Political Instability" = "Political_stab.sc"
    )
  )
  
  # Update variable choices based on composite score selection
  observeEvent(input$indicator_category, {
    updateSelectInput(session, "variable_choice",
                      choices = indicator_choice_list[[input$indicator_category]])
  })
  
  observeEvent(input$map_1_indicator_category, {
    updateSelectInput(session, "map_1_variable_choice",
                      choices = indicator_choice_list[[input$map_1_indicator_category]])
  })
  
  observeEvent(input$map_2_indicator_category, {
    updateSelectInput(session, "map_2_variable_choice",
                      choices = indicator_choice_list[[input$map_2_indicator_category]])
  })
  
  # For interactive Map
  selected_var <- reactive({
    req(input$variable_choice)
    variable <- input$variable_choice
    return(variable)
  })
  
  # Update country search choices
  observe({
    global_data <- average_country_nogeo
    choices <- list("Global (Default)", sort(unique(global_data$COUNTRY)))
    updateSelectizeInput(session, "country_search", choices = choices, server = TRUE)
  })
  
  # For Map 1
  map_1_selected_var <- reactive({
    req(input$map_1_variable_choice)
    return(input$map_1_variable_choice)
  })
  
  observeEvent(input$map_1_country_search, {
    current_map_for_country("compare_map_1")
    
    country <- input$map_1_country_search
    if (is.null(country)) {
      leafletProxy("compare_map_1") %>%
        setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      as.list()
    
    leafletProxy("compare_map_1") %>%
      setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
    
    if (input$map_1_country_search == "Global (Default)") {
      selected_country(NULL)
    } else {
      selected_country(input$map_1_country_search)
    }
  })
  
  observe({
    map_1_global_data <- average_country_nogeo
    map_1_choices <- list("Global (Default)", sort(unique(map_1_global_data$COUNTRY)))
    updateSelectizeInput(session, "map_1_country_search", choices = map_1_choices, server = TRUE)
  })
  
  # For Map 2
  map_2_selected_var <- reactive({
    req(input$map_2_variable_choice)
    return(input$map_2_variable_choice)
  })
  
  observeEvent(input$map_2_country_search, {
    current_map_for_country("compare_map_2")
    
    country <- input$map_2_country_search
    if (is.null(country)) {
      leafletProxy("compare_map_2") %>%
        setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      as.list()
    
    leafletProxy("compare_map_2") %>%
      setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
    
    if (input$map_2_country_search == "Global (Default)") {
      selected_country(NULL)
    } else {
      selected_country(input$map_2_country_search)
    }
  })
  
  observe({
    map_2_global_data <- average_country_nogeo
    map_2_choices <- list("Global (Default)", sort(unique(map_2_global_data$COUNTRY)))
    updateSelectizeInput(session, "map_2_country_search", choices = map_2_choices, server = TRUE)
  })
  
  # Handle country selection synchronization
  observeEvent(input$country_search, {
    current_map_for_country("map")
    
    if (input$country_search != "Global (Default)" && !is.null(input$country_search)) {
      updateSelectInput(session, "country_select", selected = input$country_search)
      selected_country(input$country_search)
    } else {
      selected_country(NULL)
      # When returning to global view, re-render the map with shaded countries
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
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~pal(get(var)),
          fillOpacity = 0.7,
          color = ~pal(get(var)),
          weight = 2,
          opacity = 0.9,
          highlightOptions = highlightOptions(
            color = "#FFFFFF",
            weight = 4,
            bringToFront = TRUE,
            opacity = 1,
            fillOpacity = 0.8
          ),
          layerId = ~COUNTRY,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        ) %>%
        addLegend(
          pal = pal,
          values = global_data[[var]],
          opacity = 0.8,
          title = paste(input$indicator_category),
          position = "bottomright"
        )
    }
  })
  
  observeEvent(input$country_select, {
    current_map_for_country("map")
    
    if (!is.null(input$country_select)) {
      updateSelectizeInput(session, "country_search", selected = input$country_select)
    }
  })
  
  # Handle country polygon clicks
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    if (!is.null(clicked_country)) {
      selected_country(clicked_country)
      updateSelectizeInput(session, "country_search", selected = clicked_country)
      updateSelectInput(session, "country_select", selected = clicked_country)
      
      # Zoom to clicked country
      zoom_coords <- country_centroids %>%
        filter(COUNTRY == clicked_country) %>%
        select(X, Y) %>%
        as.list()
      
      if (length(zoom_coords$X) > 0) {
        leafletProxy("map") %>%
          setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
      }
    }
  })
  
  # Handle country polygon hover
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
  
  # Main map rendering with country polygons
  output$map <- renderLeaflet({
    
    # if (input$composite_category == inequity_composite) {
    
    
    # } else if (input$composite_category == nd_gain) {
    
    
    # } else if (input$composite_category == climate_risk)
    
    var <- selected_var()
    
    global_data <- average_country_nogeo
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    }
    
    req(var %in% colnames(global_data))
    
    # Create map with conditional tile layer
    map <- leaflet()
    
    if (input$satellite_view) {
      map <- map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
    
    map %>%
      addPolygons(
        data = polygon_data,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.7,
        color = ~pal(get(var)),
        weight = 2,
        opacity = 0.9,
        highlightOptions = highlightOptions(
          color = "#FFFFFF",
          weight = 4,
          bringToFront = TRUE,
          opacity = 1,
          fillOpacity = 0.8
        ),
        layerId = ~COUNTRY,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3))
      ) %>%
      addLegend(
        pal = pal,
        values = global_data[[var]],
        opacity = 0.8,
        title = paste(input$indicator_category),
        position = "bottomright"
      )
  })
  
  # Update map tiles when satellite view changes
  observeEvent(input$satellite_view, {
    if (input$satellite_view) {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$Esri.WorldImagery)
    } else {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$Esri.WorldStreetMap)
    }
  })
  
  # Update map when selections change
  observeEvent({
    input$map_1_country_search
    input$map_2_country_search
    input$country_search
    input$use_country_specific_scale
  }, {
    req(input$indicator_category)
    var <- selected_var()
    global_data <- df
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    country <- selected_country()
    var <- selected_var()
    full_data <- df
    global_data <- average_country_nogeo
    
    if (is.null(country)) {
      # Get the appropriate polygon data
      if (var %in% composite_arith_list) {
        polygon_data <- combined_scores_global_polygons
        global_data <- combined_scores_global
      } else {
        polygon_data <- average_country_polygons
        global_data <- average_country_nogeo
      }
      
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      leafletProxy(current_map_for_country()) %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = "transparent",
          fillOpacity = 0,
          color = ~pal(get(var)),
          weight = 2,
          opacity = 0.8,
          highlightOptions = highlightOptions(
            color = "#FFFFFF",
            weight = 4,
            bringToFront = TRUE,
            opacity = 1
          ),
          layerId = ~COUNTRY,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        ) %>%
        addCircleMarkers(
          data = global_data,
          radius = 6,
          fillColor = ~pal(get(var)),
          fillOpacity = 0.8,
          stroke = TRUE,
          color = "white",
          weight = 0.5,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
          layerId = ~paste0("marker_", COUNTRY)
        ) %>%
        addLegend(
          pal = pal,
          values = global_data[[var]],
          opacity = 0.8,
          title = paste(input$indicator_category),
          position = "bottomright"
        )
      return()
    }
    
    country_data <- full_data %>% filter(COUNTRY == country)
    req(nrow(country_data) > 0)
    
    use_local <- isTRUE(input$use_country_specific_scale)
    domain_data <- if (use_local) country_data[[var]] else global_data[[var]]
    pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
    draw_map(current_map_for_country(), domain_data, use_local, country_data, var, country)
  })
  
  observeEvent({input$use_comparison_country_scale
    input$map_1_variable_choice
    input$map_2_variable_choice
    input$map_1_country_search
    input$map_2_country_search}, {  
      req(input$indicator_category)
      
      use_local <- isTRUE(input$use_comparison_country_scale)
      
      map_1_full_data <- df
      map_1_global_data <- average_country_nogeo
      
      map_2_full_data <- df
      map_2_global_data <- average_country_nogeo
      
      country_1_data <- if (is.null(input$map_1_country_search) || input$map_1_country_search == "Global (Default)" || input$map_1_country_search == "") map_1_global_data else map_1_full_data %>% filter(COUNTRY == input$map_1_country_search)
      country_2_data <- if (is.null(input$map_2_country_search) || input$map_2_country_search == "Global (Default)" || input$map_2_country_search == "") map_2_global_data else map_2_full_data %>% filter(COUNTRY == input$map_2_country_search)
      
      domain_data <- NULL
      if (use_local && !is.null(country_1_data) && !is.null(country_2_data)) {
        domain_data <- c(country_1_data[[map_1_selected_var()]], country_2_data[[map_2_selected_var()]])
      } else {
        if (!is.null(map_1_selected_var()) && !is.null(map_2_selected_var())) {
          domain_data <- c(map_1_global_data[[map_1_selected_var()]], map_2_global_data[[map_2_selected_var()]])
        } else if (!is.null(map_1_selected_var())) {
          domain_data <- map_1_global_data[[map_1_selected_var()]]
        } else {
          domain_data <- map_2_global_data[[map_2_selected_var()]]
        }
      }
      
      draw_map("compare_map_1", domain_data, use_local, if (!is.null(country_1_data)) country_1_data else domain_data, map_1_selected_var(), "")
      draw_map("compare_map_2", domain_data, use_local, if (!is.null(country_2_data)) country_2_data else domain_data, map_2_selected_var(), "")
    })
  
  # Handle comparison country scale changes
  observeEvent(input$use_comparison_country_scale, {
    req(input$indicator_category)
    
    if (current_map_for_country() == "map") {
      full_data <- df
      global_data <- average_country_nogeo
      
      country <- if (input$country_search == "Global (Default)") NULL else input$country_search
      if (is.null(country)) {
        draw_map(current_map_for_country(), global_data[[selected_var()]], FALSE, global_data, selected_var(), "Global (Default)")
        return()
      }
      country_data <- full_data %>% filter(COUNTRY == country)
      req(nrow(country_data) > 0)
      use_local <- isTRUE(input$use_country_specific_scale)
      domain_data <- if (use_local) country_data[[selected_var()]] else global_data[[selected_var()]]
      draw_map(current_map_for_country(), domain_data, use_local, country_data, selected_var(), country)
    } else if (current_map_for_country() == "compare_map_1" || current_map_for_country() == "compare_map_2") {
      use_local <- isTRUE(input$use_comparison_country_scale)
      
      map_1_full_data <- df
      map_1_global_data <- average_country_nogeo
      
      map_2_full_data <- df
      map_2_global_data <- average_country_nogeo
      
      country_1_data <- if (is.null(input$map_1_country_search) || input$map_1_country_search == "Global (Default)" || input$map_1_country_search == "") map_1_global_data else map_1_full_data %>% filter(COUNTRY == input$map_1_country_search)
      country_2_data <- if (is.null(input$map_2_country_search) || input$map_2_country_search == "Global (Default)" || input$map_2_country_search == "") map_2_global_data else map_2_full_data %>% filter(COUNTRY == input$map_2_country_search)
      
      if (input$use_comparison_country_scale && !is.null(country_1_data) && !is.null(country_2_data)) {
        domain_data <- c(country_1_data[[map_1_selected_var()]], country_2_data[[map_2_selected_var()]])
      } else {
        if (!is.null(map_1_selected_var()) && !is.null(map_2_selected_var())) {
          domain_data <- c(map_1_global_data[[map_1_selected_var()]], map_2_global_data[[map_2_selected_var()]])
        } else if (!is.null(map_1_selected_var())) {
          domain_data <- map_1_global_data[[map_1_selected_var()]]
        } else {
          domain_data <- map_2_global_data[[map_2_selected_var()]]
        }
      }
      
      draw_map("compare_map_1", domain_data, use_local, if (!is.null(country_1_data)) country_1_data else domain_data, map_1_selected_var(), "")
      draw_map("compare_map_2", domain_data, use_local, if (!is.null(country_2_data)) country_2_data else domain_data, map_2_selected_var(), "")
    }
  })
  
  # Function for drawing point maps with country borders always visible
  draw_map <- function(current_map_for_country, domain_data, use_local, country_data, var, country) {
    pal <- colorNumeric(palette = "Purples", domain = domain_data, na.color = "transparent")
    
    # Get the appropriate polygon data
    if (var %in% composite_arith_list) {
      polygon_data <- combined_scores_global_polygons
      global_data <- combined_scores_global
    } else {
      polygon_data <- average_country_polygons
      global_data <- average_country_nogeo
    }
    
    # Create color palette for borders (use global data for consistent coloring)
    border_pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    legendPosition = "bottomright"
    
    # Check if we're showing global view or country-specific view
    if (country == "Global (Default)" || is.null(country) || country == "") {
      # GLOBAL VIEW: Show filled countries (choropleth)
      leafletProxy(current_map_for_country) %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = polygon_data,
          fillColor = ~border_pal(get(var)),
          fillOpacity = 0.7,
          color = ~border_pal(get(var)),
          weight = 2,
          opacity = 0.9,
          highlightOptions = highlightOptions(
            color = "#FFFFFF",
            weight = 4,
            bringToFront = TRUE,
            opacity = 1,
            fillOpacity = 0.8
          ),
          layerId = ~COUNTRY,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        ) %>%
        addLegend(
          pal = border_pal,
          values = global_data[[var]],
          opacity = 0.8,
          title = paste(input$indicator_category),
          position = legendPosition
        )
    } else {
      # COUNTRY-SPECIFIC VIEW: Show individual points with transparent country borders
      leafletProxy(current_map_for_country) %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls() %>%
        # Add transparent country polygons with borders only
        addPolygons(
          data = polygon_data,
          fillColor = "transparent",
          fillOpacity = 0,
          color = ~border_pal(get(var)),
          weight = 1,
          opacity = 0.4,
          highlightOptions = highlightOptions(
            color = "#FFFFFF",
            weight = 3,
            bringToFront = TRUE,
            opacity = 1
          ),
          layerId = ~COUNTRY,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        ) %>%
        # Add markers for the selected country
        addCircleMarkers(
          data = country_data,
          radius = 6,
          fillColor = ~pal(get(var)),
          fillOpacity = 0.9,
          stroke = TRUE,
          color = "black",
          weight = 0.7,
          label = ~paste0(COUNTRY, ": ", round(get(var), 3))
        ) %>%
        addLegend(
          pal = pal,
          values = domain_data,
          opacity = 0.9,
          title = paste0(country, if (use_local) " (Local Scale)" else " (Global Scale)"),
          position = legendPosition
        )
    }
  }
  
  safe_round <- function(x, digits = 3) {
    tryCatch({
      round(x, digits)
    }, error = function(e) {
      0
    })
  }
  
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
    
    if (is.null(which_country)) {
      leafletProxy("map") %>%
        setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == which_country) %>%
      select(X, Y) %>%
      as.list()
    
    leafletProxy("map") %>%
      setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
  })
  
  # Country selection reactives
  chosen_country <- reactiveVal(NULL)
  observeEvent(input$country_select, {
    click <- input$country_select
    chosen_country(click)
  })
  
  observe({
    req(chosen_country())
    updateSelectInput(
      session,
      inputId = "country_select",
      selected = chosen_country()
    )
  })
  
  country_dataset <- reactiveVal(NULL)
  
  observeEvent(input$country_select, {
    country <- input$country_select
    dataset <- filter(df, COUNTRY == country)
    country_dataset(dataset)
  })
  
  # Output for country display
  output$countryDisplay <- renderText({
    if (is.null(chosen_country())) {
      "No country selected"
    } else {
      chosen_country()
    }
  })
  
  # Global or country components UI
  output$global_or_country_components <- renderUI({
    
    # GLOBAL COMPARISON
    if (input$global_or_country == "global") {
      tagList(
        tags$h4("Global Bivariate Analysis Setup", 
                style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
        
        # First indicator selection with better spacing
        tags$div(
          style = "margin-bottom: 15px;",
          selectInput("first_indicator_global", 
                      "Choose your first indicator",
                      choices = global_level_choices,
                      selected = "Gov_effect.sc"),
          tags$small(textOutput("first_indicator_global_description"),
                     style = "font-style: italic; color: #666;")
        ),
        
        # Second indicator selection with better spacing
        tags$div(
          style = "margin-bottom: 15px;",
          selectInput("second_indicator_global", 
                      "Choose your second indicator",
                      choices = global_level_choices,
                      selected = "le.ineq.log.sc"),
          tags$small(textOutput("second_indicator_global_description"),
                     style = "font-style: italic; color: #666;")
        ),
        
      ) 
      
      # COUNTRY-LEVEL COMPARISON  
    } else if (input$global_or_country == "country") {
      tagList(
        tags$h4("Country-Level Analysis Setup", 
                style = "color: var(--bs-primary, #003087); margin-bottom: 15px;"),
        
        # Country selection - now synchronized with country_search
        tags$div(
          style = "margin-bottom: 20px;",
          selectInput("country_select", "Select a Country to Investigate",
                      choices = sort(unique(countryCodes$Country)),
                      selected = "Japan"),
          tags$small("This selection is synchronized with 'Jump to Country' above.", 
                     style = "font-style: italic; color: #666;")
        ),
        
        # Histogram indicator selection
        tags$div(
          style = "margin-bottom: 15px;",
          tags$h5("Histogram Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
          selectInput("country_histogram_indicator", 
                      "Choose an indicator for distribution analysis",
                      choices = c(
                        "Distance to Coast (km)" = "distance_to_coast_km", 
                        "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                        "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                        "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                      ),
                      selected = "povmap.grdi.v1.sc"),
          tags$small(textOutput("country_histogram_indicator_description"),
                     style = "font-style: italic; color: #666;")
        ),
        
        # Bivariate analysis setup
        tags$div(
          style = "margin-bottom: 15px;",
          tags$h5("Bivariate Analysis:", style = "margin-bottom: 10px; color: var(--bs-primary, #003087);"),
          
          # First indicator
          tags$div(
            style = "margin-bottom: 10px;",
            selectInput("first_indicator", 
                        "Choose your first indicator",
                        choices = c(
                          "Distance to Coast (km)" = "distance_to_coast_km", 
                          "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                          "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                          "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                        ),
                        selected = "povmap.grdi.v1.sc"),
            tags$small(textOutput("first_indicator_country_description"),
                       style = "font-style: italic; color: #666;")
          ),
          
          # Second indicator
          tags$div(
            style = "margin-bottom: 10px;",
            selectInput("second_indicator", 
                        "Choose your second indicator",
                        choices = c(
                          "Distance to Coast (km)" = "distance_to_coast_km", 
                          "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                          "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                          "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                        ),
                        selected = "perc.pop.world.coastal.merit.10m.log.sc"),
            tags$small(textOutput("second_indicator_country_description"),
                       style = "font-style: italic; color: #666;")
          )
        ),
        
        tags$p("Results will appear in the 'Custom Graphs' tab.", 
               style = "font-style: italic; text-align: center; margin-top: 20px; color: #666;")
      )
    }
  })
  
  # CUSTOM BIVARIATE SCATTERPLOT
  output$custom_scatter <- renderPlot({
    data <- country_dataset()
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    indicator_choices <- c(
      "Distance to Coast (km)" = "distance_to_coast_km", 
      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
    )
    
    x_label <- names(indicator_choices)[indicator_choices == x_col]
    y_label <- names(indicator_choices)[indicator_choices == y_col]
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    
    data1 <- data[[x_col]]
    data2 <- data[[y_col]]
    
    if (all(is.na(data1)) || all(is.na(data2))) return()
    
    plot(data1, data2, 
         main = chosen_country(),
         xlab = x_label,
         ylab = y_label)
  })
  
  output$global_custom_scatter <- renderPlot({
    data <- average_country_nogeo
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator_global
    y_col <- input$second_indicator_global
    
    indicator_choices <- global_level_choices
    
    x_label <- names(indicator_choices)[indicator_choices == x_col]
    y_label <- names(indicator_choices)[indicator_choices == y_col]
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    
    data1 <- data[[x_col]]
    data2 <- data[[y_col]]
    
    if (all(is.na(data1)) || all(is.na(data2))) return()
    
    plot(data1, data2, 
         main = "Global",
         xlab = x_label,
         ylab = y_label)
  })
  
  # Outputs the Pearson and Spearman Coefficients
  output$correlation <- renderText({
    data <- country_dataset()
    if (is.null(data) || nrow(data) == 0) return("No data available") 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    # Check if columns exist
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) {
      return("Selected variables not available for this country")
    }
    
    # Get data and remove NA pairs
    x_data <- data[[x_col]]
    y_data <- data[[y_col]]
    
    # Check if we have valid numeric data
    if (!is.numeric(x_data) || !is.numeric(y_data)) {
      return("Selected variables are not numeric")
    }
    
    # Check for complete pairs
    complete_pairs <- complete.cases(x_data, y_data)
    if (sum(complete_pairs) < 2) {
      return("Insufficient data for correlation analysis")
    }
    
    cor_result <- tryCatch({
      cor(x_data, y_data, use = "complete.obs")
    }, error = function(e) {
      return(NA)
    })
    
    spr_cor_result <- tryCatch({
      cor(x_data, y_data, method = "spearman", use = "complete.obs")
    }, error = function(e) {
      return(NA)
    })
    
    if (is.na(cor_result) || is.na(spr_cor_result)) {
      return("Could not calculate correlation")
    }
    
    cor_result <- round(cor_result, 4)
    spr_cor_result <- round(spr_cor_result, 4)
    
    paste("Pearson Coefficient (r) = ", cor_result,
          "\nSpearman Coefficient (rho) = ", spr_cor_result)
  })
  
  output$global_correlation <- renderText({
    data <- average_country_nogeo
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator_global
    y_col <- input$second_indicator_global
    
    cor = cor(data[[x_col]], data[[y_col]], use = "complete.obs")
    cor = round(cor, 4)
    
    spr_cor = cor(data[[x_col]], data[[y_col]], method = "spearman", use = "complete.obs")
    spr_cor = round(spr_cor, 4)
    
    paste("Pearson Coefficient (r) = ", cor,
          "\nSpearman Coefficient (rho) = ", spr_cor)
  })
  
  # Displays a histogram for a COUNTRY
  output$country_histogram <- renderPlot({
    chi <- input$country_histogram_indicator
    
    data <- country_dataset()
    if (is.null(data) || nrow(data) == 0) return() 
    
    # Check if the column exists and has valid data
    if (!(chi %in% names(data))) return()
    
    col <- data[[chi]]
    
    # Remove NA values and check if we have numeric data
    col <- col[!is.na(col)]
    if (length(col) == 0 || !is.numeric(col)) return()
    
    indicator_choices <- c(
      "Distance to Coast (km)" = "distance_to_coast_km", 
      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
    )
    
    label <- names(indicator_choices)[indicator_choices == chi]
    countryname <- chosen_country()
    
    # Only create histogram if we have enough data points
    if (length(col) > 1) {
      hist(col,
           main = paste0("Histogram of ", label, " for ", countryname),
           xlab = label)
    }
  })
  
  # Displays the country flag image
  output$country_flag <- renderImage({
    if (is.null(chosen_country())) {
      list(src = "www/globe.png",
           contentType = "image/png",
           alt = "Globe",
           width = 120,
           height = 120
      )
    } else {
      filename <- findPNGpath(chosen_country(), countryCodes)
      list(src = filename,
           contentType = "image/png",
           alt = "Country flag",
           width = 160,
           height = 120
      )
    }
  }, deleteFile = FALSE)
  
  # Data Descriptions
  clicked_score_first_global <- reactiveVal(NULL)
  clicked_score_second_global <- reactiveVal(NULL)
  clicked_score_first_country <- reactiveVal(NULL)
  clicked_score_second_country <- reactiveVal(NULL)
  clicked_score_country_histogram <- reactiveVal(NULL)
  
  observeEvent(input$second_indicator_global, {
    click <- input$second_indicator_global
    clicked_score_second_global(click)
  })
  
  observeEvent(input$first_indicator_global, {
    click <- input$first_indicator_global
    clicked_score_first_global(click)
  })
  
  observeEvent(input$first_indicator, {
    click <- input$first_indicator
    clicked_score_first_country(click)
  })
  
  observeEvent(input$second_indicator, {
    click <- input$second_indicator
    clicked_score_second_country(click)
  })
  
  output$first_indicator_country_description <- renderText({
    req(clicked_score_first_country())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score_first_country()) %>%
      pull(description)
    return(descriptions)
  })
  
  output$second_indicator_country_description <- renderText({
    req(clicked_score_second_country())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score_second_country()) %>%
      pull(description)
    return(descriptions)
  })
  
  output$first_indicator_global_description <- renderText({
    req(clicked_score_first_global())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score_first_global()) %>%
      pull(description)
    return(descriptions)
  })
  
  output$second_indicator_global_description <- renderText({
    req(clicked_score_second_global())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score_second_global()) %>%
      pull(description)
    return(descriptions)
  })
  
  observeEvent(input$country_histogram_indicator, {
    click <- input$country_histogram_indicator
    clicked_score_country_histogram(click)
  })
  
  output$country_histogram_indicator_description <- renderText({
    req(clicked_score_country_histogram())
    descriptions <- inequity_data_descriptions %>%
      filter(variable_name == clicked_score_country_histogram()) %>%
      pull(description)
    return(descriptions)
  })
  
  # Data+ logo
  output$dataplus_logo <- renderImage({
    list(src = "www/data-plus-logo.png",
         contentType = "image/png",
         alt = "data_plus",
         width = 300,
         height = 120
    )
  }, deleteFile = FALSE)
  
  # Comparison maps
  output$compare_map_1 <- renderLeaflet({
    var <- map_1_selected_var()
    map_1_global_data <- average_country_nogeo
    
    req(var %in% colnames(map_1_global_data))
    
    pal <- colorNumeric("Purples", domain = map_1_global_data[[var]], na.color = "transparent")
    
    map_1 <- leaflet(map_1_global_data)
    
    if (FALSE) {
      map_1 <- map_1 %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map_1 <- map_1 %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
    
    map_1 %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "white",
        weight = 0.5,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
        layerId = ~COUNTRY
      ) %>%
      addLegend(
        pal = pal,
        values = map_1_global_data[[var]],
        opacity = 0.8,
        title = paste(input$map_1_indicator_category),
        position = "bottomright"
      )
  })
  
  output$compare_map_2 <- renderLeaflet({
    var <- map_2_selected_var()
    map_2_global_data <- average_country_nogeo
    
    req(var %in% colnames(map_2_global_data))
    
    pal <- colorNumeric("Purples", domain = map_2_global_data[[var]], na.color = "transparent")
    
    map_2 <- leaflet(map_2_global_data)
    
    if (FALSE) {
      map_2 <- map_2 %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map_2 <- map_2 %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
    
    map_2 %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "white",
        weight = 0.5,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3)),
        layerId = ~COUNTRY
      ) %>%
      addLegend(
        pal = pal,
        values = map_2_global_data[[var]],
        opacity = 0.8,
        title = paste(input$map_2_indicator_category),
        position = "bottomright"
      )
  })
}
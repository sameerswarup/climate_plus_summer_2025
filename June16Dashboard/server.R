# server.R
server <- function(input, output, session) {
  
  #rv <- reactiveValues(zoom = 4)
  
  # ----------------------------------------------------------------------------
  selected_country <- reactiveVal(NULL)
  current_map_for_country <- "map"
  
  # For interactive Map
  selected_var <- reactive({
    req(input$indicator_category)#, input$mean_type)
    prefix <- indicator_prefix_map[[input$indicator_category]]
    paste0(prefix, "_arith") # mean_type_suffix[[input$mean_type]])
  })
  
  observe({
    global_data <- data_list[[input$indicator_category]]$global
    choices <- list("Global (Default)", sort(unique(global_data$COUNTRY)))
    updateSelectizeInput(session, "country_search", choices = choices, server = TRUE)
  })
  
  
  
  # For Map 1
  map_1_selected_var <- reactive({
    req(input$map_1_country_search)
    map_1_prefix <- indicator_prefix_map[[input$map_1_indicator_category]]
    paste0(map_1_prefix, mean_type_suffix[[input$map_1_mean_type]])
  })

  observeEvent(input$map_1_country_search, {
    current_map_for_country <<- "compare_map_1" 
    
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
    map_1_global_data <- data_list[[input$map_1_indicator_category]]$global
    map_1_choices <- list("Global (Default)", sort(unique(map_1_global_data$COUNTRY)))
    updateSelectizeInput(session, "map_1_country_search", choices = map_1_choices, server = TRUE)
  })

  # For Map 2
  map_2_selected_var <- reactive({
    req(input$map_2_indicator_category, input$map_2_mean_type)
    map_2_prefix <- indicator_prefix_map[[input$map_2_indicator_category]]
    paste0(map_2_prefix, mean_type_suffix[[input$map_2_mean_type]])
  })

  observeEvent(input$map_2_country_search, {
    current_map_for_country <<- "compare_map_2" 
    
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
    map_2_global_data <- data_list[[input$map_2_indicator_category]]$global
    map_2_choices <- list("Global (Default)", sort(unique(map_2_global_data$COUNTRY)))
    updateSelectizeInput(session, "map_2_country_search", choices = map_2_choices, server = TRUE)
  })
  
  
  
  
  # Standardized country selection - sync between country_search and country_select
  observeEvent(input$country_search, {
    current_map_for_country <<- "map"
    
    if (input$country_search != "Global (Default)" && !is.null(input$country_search)) {
      # Update country_select to match country_search
      updateSelectInput(session, "country_select", selected = input$country_search)
    }
  })
  
  observeEvent(input$country_select, {
    current_map_for_country <<- "map"
    
    if (!is.null(input$country_select)) {
      # Update country_search to match country_select
      updateSelectizeInput(session, "country_search", selected = input$country_select)
    }
  })
  
  observeEvent(input$country_search, {
    current_map_for_country <<- "map"
    
    if (input$country_search == "Global (Default)") {
      selected_country(NULL)
    } else {
      selected_country(input$country_search)
    }
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- input$map_marker_click$id
    selected_country(clicked_country)
  })
  
  output$map <- renderLeaflet({
    var <- selected_var()
    global_data <- data_list[[input$indicator_category]]$global
    req(var %in% colnames(global_data))
    
    pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
    
    # Create map with conditional tile layer
    map <- leaflet(global_data)
    
    if (input$satellite_view) {
      map <- map %>% addProviderTiles(providers$Esri.WorldImagery)
    } else {
      map <- map %>% addProviderTiles(providers$Esri.WorldStreetMap)
    }
    
    map %>%
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
        values = global_data[[var]],
        opacity = 0.8,
        title = paste(input$indicator_category, "(", input$mean_type, ")"),
        position = "bottomright"
      )
  })
  
  # Update map tiles when satellite view changes - preserve zoom and center
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
  
  observeEvent({
    selected_country()
    input$use_country_specific_scale
  }, {
    req(input$indicator_category, input$mean_type)
    
    country <- selected_country()
    var <- selected_var()
    full_data <- data_list[[input$indicator_category]]$full
    global_data <- data_list[[input$indicator_category]]$global
    
    if (is.null(country)) {
      pal <- colorNumeric("Purples", domain = global_data[[var]], na.color = "transparent")
      leafletProxy(current_map_for_country) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          data = global_data,
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
          values = global_data[[var]],
          opacity = 0.8,
          title = paste(input$indicator_category, "(", input$mean_type, ")"),
          position = "bottomright"
        )
      return()
    }
    
    country_data <- full_data %>% filter(COUNTRY == country)
    req(nrow(country_data) > 0)
    
    use_local <- isTRUE(input$use_country_specific_scale)
    domain_data <- if (use_local) country_data[[var]] else global_data[[var]]
    pal <- colorNumeric("Purples", domain = domain_data, na.color = "transparent")
    
    leafletProxy(current_map_for_country) %>%
      clearMarkers() %>%
      clearControls() %>%
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
        title = paste0(country, " (", if (use_local) "Local" else "Global", " Scale, ", input$mean_type, ")"),
        position = "bottomright"
      )
  })
  
  observeEvent(input$use_country_specific_scale, {
    if (!is.null(selected_country())) {
      selected_country(selected_country())
    }
  })
  
  observeEvent(input$zoom_button, {
    country <- selected_country()
    if (is.null(country)) {
      leafletProxy("map") %>%
        setView(lng = 0, lat = 20, zoom = 2)
      return()
    }
    
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      as.list()
    
    leafletProxy("map") %>%
      setView(lng = zoom_coords$X, lat = zoom_coords$Y, zoom = 5)
  })
  
  # -----------------------------------------------------------------------------
  
  # reactiveVals
  # Observe for whether a country is chosen using the dropdown menu or
  # chosen directly from the map.
  
  chosen_country <- reactiveVal(NULL)
  observeEvent(input$country_select, {
    click<-input$country_select
    chosen_country(click)
  })
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    chosen_country(click$id) 
  })
  
  observe({ # makes it reactive, and chosen_country() is inside a reactive function
    req(chosen_country()) # waits for chosen_country to have something selected
    updateSelectInput(
      session,
      inputId = "country_select",
      selected = chosen_country()
    )
  })
  
  # -----------------------------------------------------------------------------
  
  country_dataset <- reactiveVal(NULL)
  
  observeEvent(input$country_select, {
    country <-input$country_select
    dataset <- filter(df, COUNTRY == country)
    country_dataset(dataset)
  })
  
  # -----------------------------------------------------------------------------
  
  # renderUI outputs. These are used to change the UI based on whether "Global"
  # or "Country" is chosen.
  
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
                      selected = "povmap.grdi.v1.sc")
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
  
  # -----------------------------------------------------------------------------
  
  # CUSTOM BIVARIATE SCATTERPLOT
  
  output$custom_scatter <- renderPlot({
    data <- country_dataset() # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    # reverse so that i can find the labels for each choice
    
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
    
    # Check if the data vectors have at least some non-NA numeric values
    if (all(is.na(data1)) || all(is.na(data2))) return()
    
    plot(data1, data2, 
         main = chosen_country(),
         xlab = x_label,
         ylab = y_label)
    
  })
  
  
  output$global_custom_scatter <- renderPlot({
    data <- average_country_nogeo # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator_global
    y_col <- input$second_indicator_global
    
    # reverse so that i can find the labels for each choice
    
    indicator_choices <- global_level_choices
    
    x_label <- names(indicator_choices)[indicator_choices == x_col]
    y_label <- names(indicator_choices)[indicator_choices == y_col]
    
    if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return()
    
    data1 <- data[[x_col]]
    data2 <- data[[y_col]]
    
    # Check if the data vectors have at least some non-NA numeric values
    if (all(is.na(data1)) || all(is.na(data2))) return()
    
    plot(data1, data2, 
         main = "Global",
         xlab = x_label,
         ylab = y_label)
    
  })
  
  # -----------------------------------------------------------------------------
  
  # Outputs the Pearson and Spearman Coefficients
  
  output$correlation <- renderText({
    data <- country_dataset() # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator
    y_col <- input$second_indicator
    
    cor = cor(data[[x_col]], data[[y_col]], use = "complete.obs")
    
    cor = round(cor, 4)
    
    
    spr_cor = cor(data[[x_col]], data[[y_col]], method = "spearman", use = "complete.obs")
    spr_cor = round(spr_cor, 4)
    
    paste("Pearson Coefficient (r) = ", cor,
          "\nSpearman Coefficient (rho) = ", spr_cor)
    
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
  
  # -----------------------------------------------------------------------------
  
  # Displays a histogram for a COUNTRY
  
  output$country_histogram <- renderPlot({
    
    chi <- input$country_histogram_indicator
    
    data <- country_dataset() # set it here because country_dataset is a function
    if (is.null(data)) return() 
    
    col = data[[chi]]
    
    indicator_choices <- c(
      "Distance to Coast (km)" = "distance_to_coast_km", 
      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
    )
    
    label <- names(indicator_choices)[indicator_choices == chi]
    
    countryname <- chosen_country()
    
    hist(col,
         main = paste0("Histogram of ", label, " for ", countryname),
         xlab = label)
    
  })
  
  # -----------------------------------------------------------------------------
  
  # Displays the country flag image
  
  output$country_flag <- renderImage({
    # before click
    if (is.null(chosen_country())) {
      list(src = "www/globe.png",
           contentType = "image/png",
           alt = "Globe",
           width = 120,
           height = 120
      )
      # after click
    } else {
      filename <- findPNGpath(chosen_country())
      # Return a list containing the filename
      list(src = filename,
           contentType = "image/png",
           alt = "Country flag",
           width = 160,
           height = 120
      )
    }
  }, deleteFile = FALSE)
  
  # -----------------------------------------------------------------------------
  
  # Data Descriptions
  
  # reactive value for selected indicator
  clicked_score_first_global <- reactiveVal(NULL)
  clicked_score_second_global <- reactiveVal(NULL)
  clicked_score_first_country <- reactiveVal(NULL)
  clicked_score_second_country <- reactiveVal(NULL)
  
  observeEvent(input$second_indicator_global, {
    click <- input$second_indicator_global
    clicked_score_second_global(click)  # id of indicator
  })
  
  observeEvent(input$first_indicator_global, {
    click <- input$first_indicator_global
    clicked_score_first_global(click)  # id of indicator
  })
  
  observeEvent(input$first_indicator, {
    click <- input$first_indicator
    clicked_score_first_country(click)  # id of indicator
  })
  
  observeEvent(input$second_indicator, {
    click <- input$second_indicator
    clicked_score_second_country(click)  # id of indicator
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
  
  # Data+ logo
  
  output$dataplus_logo <- renderImage({
    list(src = "www/data-plus-logo.png",
         contentType = "image/png",
         alt = "data_plus",
         width = 300,
         height = 120
    )
  }, deleteFile = FALSE)
  
  output$compare_map_1 <- renderLeaflet({

    var <- map_1_selected_var()
    map_1_global_data <- data_list[[input$map_1_indicator_category]]$global

    req(var %in% colnames(map_1_global_data))
    
    pal <- colorNumeric("Purples", domain = map_1_global_data[[var]], na.color = "transparent")

    # Create map with conditional tile layer
    map_1 <- leaflet(map_1_global_data)

    if (FALSE) { #input$map_1_satellite_view
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
    map_2_global_data <- data_list[[input$map_2_indicator_category]]$global
    req(var %in% colnames(map_2_global_data))

    pal <- colorNumeric("Purples", domain = map_2_global_data[[var]], na.color = "transparent")

    # Create map with conditional tile layer
    map_2 <- leaflet(map_2_global_data)

    if (FALSE) { #input$map_1_satellite_view
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
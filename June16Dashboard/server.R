# server.R
server <- function(input, output, session) {
  
  #rv <- reactiveValues(zoom = 4)
  
  output$country_selector <- renderUI({
    choices <- countryCodes %>%
      filter(!is.na(Alpha.3.code), !is.na(Country)) %>%
      distinct(Alpha.3.code, Country) %>%
      { setNames(.$Alpha.3.code, .$Country) }
    
    selectInput("selected_country", "Select Country", choices = choices)
  })
  
  output$region_selector <- renderUI({
    req(input$selected_country)  # wait for a country selection
    country <- gsub("\"", "", trimws(input$selected_country))
    
    choices <- regionCodes %>%
      filter(iso_a3 == country) %>%
      distinct(GID_2, NAME_2) %>%
      { setNames(.$GID_2, .$NAME_2) }
    
    # Handle empty choices
    if (length(choices) == 0) {
      choices <- c("No regions available" = "")
    }
    
    selectInput("selected_region", "Select District", choices = choices)
  })
  
  # Zoom Button
  observeEvent(input$zoom_button, {
    country <- gsub("\"", "", trimws(input$selected_country))
    
    new_zoom <- 6
    #if(input$map_zoom > 3) new_zoom <- 3
    
    country_details <- countryCodes %>% filter(Alpha.3.code == input$selected_country)
    
    leafletProxy("map") %>%
      setView(lng = gsub("\"", "", trimws(country_details$Longitude..average.)), lat = gsub("\"", "", trimws(country_details$Latitude..average.)), zoom = new_zoom)
  })
  
  # ----------------------------------------------------------------------------
  # Reactive to get the selected dataset from the list
  selected_data <- reactive({
    req(input$indicator_category)
    data_list[[input$indicator_category]]
  })
  
  # Reactive to get the correct variable name for the selected dataset and mean type
  selected_var <- reactive({
    req(input$indicator_category, input$mean_type)
    prefix <- indicator_prefix_map[[input$indicator_category]]
    paste0(prefix, mean_type_suffix[[input$mean_type]])
  })
  
  # Render Leaflet map based on user selection
  output$map <- renderLeaflet({
    data <- selected_data()
    var <- selected_var()
    
    # Confirm the variable exists in the data
    req(var %in% colnames(data))
    
    # Define color palette
    pal <- colorNumeric("viridis", domain = data[[var]], na.color = "transparent")
    
    # Build map
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 6,
        fillColor = ~pal(get(var)),
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "white",
        weight = 0.5,
        label = ~paste0(COUNTRY, ": ", round(get(var), 3))
      ) %>%
      addLegend(
        pal = pal,
        values = data[[var]],
        opacity = 0.8,
        title = paste(input$indicator_category, "(", input$mean_type, ")"),
        position = "bottomright"
      )
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
    dataset <- filter(countryCodes, Country == country)
    country_dataset(dataset)
  })
  
  # -----------------------------------------------------------------------------
  
  # renderUI outputs. These are used to change the UI based on whether "Global"
  # or "Country" is chosen.
  
  output$global_or_country_components <- renderUI ({
    
    
    # GLOBAL COMPARISON
    
    
    if (input$global_or_country == "global") {
      tagList(
        
        # DISPLAY BIVARIATE SCATTER PLOTS
        
        tags$h4("Bivariate Graphs!",
                style = "color: #003087; text-align: center;
              font-style: italic"),
        
        
        
        # CHOOSE FIRST INDICATOR (GLOBAL)
        
        selectInput("first_indicator_global", 
                    "Choose your first indicator",
                    choices = global_level_choices,
                    selected = "Gov_effect.sc"
                    
        ),
        
        # DESCRIPTION OF FIRST INDICATOR WILL GO HERE
        
        tags$small(textOutput("first_indicator_global_description"),
                style = "font-style: italic"),


        
        # CHOOSE SECOND INDICATOR (GLOBAL)
        
        selectInput("second_indicator_global", 
                    "Choose your second indicator",
                    choices = global_level_choices,
                    selected = "le.ineq.log.sc"
                    
        ),
        
        # DESCRIPTION OF SECOND INDICATOR WILL GO HERE
        
        tags$small(textOutput("second_indicator_global_description"),
                style = "font-style: italic"),
        
        
        
        # DISPLAYS CUSTOM SCATTERPLOT (GLOBAL-LEVEL)
        
        plotOutput("global_custom_scatter"),
        
        # DISPLAYS PEARSON AND SPEARMAN CORRELATION COEFFICIENTS
        
        verbatimTextOutput("global_correlation")
        
      ) 
      
      
      # -----------------------------------------------------------------------------
      
      
      # COUNTRY-LEVEL COMPARISON  
      
    } else if (input$global_or_country == "country") {
      tagList(
        
        # SELECT COUNTRY
        
        selectInput("country_select", "Select a Country to Investigate",
                    choices = sort(unique(countryCodes$Country)),
                    selected = "Japan"),
        
        #DISPLAYS WHAT COUNTRY WAS CHOSEN
        
        textOutput("countryDisplay"),
        
        # DISPLAYS COUNTRY FLAG
        
        tags$div(
          style = "text-align: center;",
          imageOutput("country_flag", height = "120px")
        ),
        
        # DISPLAY HISTOGRAMS HERE
        
        tags$h4("Histograms!",
                style = "color: #003087; text-align: center;
              font-style: italic"),
        
        selectInput("country_histogram_indicator", 
                    "Choose an indicator",
                    choices = c(
                      "Distance to Coast (km)" = "distance_to_coast_km", 
                      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                    ),
                    selected = "povmap.grdi.v1.sc"),
        
        plotOutput("country_histogram"),
        
        
        # DISPLAY BIVARIATE SCATTER PLOTS
        
        tags$h4("Bivariate Graphs!",
                style = "color: #003087; text-align: center;
              font-style: italic"),
        
        # CHOOSE FIRST INDICATOR (COUNTRY-LEVEL)
        
        selectInput("first_indicator", 
                    "Choose your first indicator",
                    choices = c(
                      "Distance to Coast (km)" = "distance_to_coast_km", 
                      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                    ),
                    selected = "povmap.grdi.v1.sc"
                    
        ),
        
        # DESCRIPTION OF FIRST INDICATOR (COUNTRY-LEVEL)
        
        tags$small(textOutput("first_indicator_country_description"),
                   style = "font-style: italic"),
        
        # CHOOSE SECOND INDICATOR (COUNTRY-LEVEL)
        
        selectInput("second_indicator", 
                    "Choose your second indicator",
                    choices = c(
                      "Distance to Coast (km)" = "distance_to_coast_km", 
                      "Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                      "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                      "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"
                    ),
                    selected = "perc.pop.world.coastal.merit.10m.log.sc"
                    
        ),
        
        # DESCRIPTION OF SECOND INDICATOR (COUNTRY-LEVEL)
        
        tags$small(textOutput("second_indicator_country_description"),
                   style = "font-style: italic"),
        
        # DISPLAYS CUSTOM SCATTERPLOT (COUNTRY-LEVEL)
        
        plotOutput("custom_scatter"),
        
        # DISPLAYS PEARSON AND SPEARMAN CORRELATION COEFFICIENTS
        
        verbatimTextOutput("correlation")
        
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
}

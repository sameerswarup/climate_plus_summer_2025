server <- function(input, output, session) {
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
  
  
  pal_reactive <- reactive({
      colorNumeric("YlGnBu", world_joined$population, na.color = "transparent")
    })
  
  # -----------------------------------------------------------------------------
  
  country_dataset <- reactiveVal(NULL)
  
  observeEvent(input$country_select, {
    country <-input$country_select
    dataset <- filter(df, COUNTRYNM == country)
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
        
        
        
        # CHOOSE SECOND INDICATOR (GLOBAL)
        
        selectInput("second_indicator_global", 
                    "Choose your second indicator",
                    choices = global_level_choices,
                    selected = "le.ineq.log.sc"
                    
        ),
        
        
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
                    choices = sort(unique(df_population$COUNTRYNM)),
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
        
        # DISPLAYS CUSTOM SCATTERPLOT (COUNTRY-LEVEL)
        
        plotOutput("custom_scatter"),
        
        # DISPLAYS PEARSON AND SPEARMAN CORRELATION COEFFICIENTS
        
        verbatimTextOutput("correlation")
        
      )
      
    }
  })
  
  
  # -----------------------------------------------------------------------------
  
  
  # Output interactive map in terms of POPULATION
  
  output$map <- renderLeaflet({
    pal <- pal_reactive()
    
    
    leaflet(data = world_joined) %>%
      addTiles() %>%
      addPolygons (
        fillColor = ~pal(population),
        layerId = ~name,
        # layerId is an identifier you assign 
        # to each shape (polygon, marker, etc.) 
        # you add to a Leaflet map in Shiny.
        weight = 1,
        color = "grey",
        fillOpacity = 0.8,
        label = ~paste0(
          name, ": ", round(population, 2)
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal, values = ~population,
        title = "Coastal Population"
      ) 
    
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
    
    cor = cor(data[[x_col]], data[[y_col]])
    
    cor = round(cor, 4)
    
    
    spr_cor = cor(data[[x_col]], data[[y_col]], method = "spearman")
    spr_cor = round(spr_cor, 4)
    
    paste("Pearson Coefficient (r) = ", cor,
          "\nSpearman Coefficient (rho) = ", spr_cor)
    
  })
  
  
  output$global_correlation <- renderText({
    data <- average_country_nogeo
    if (is.null(data)) return() 
    
    x_col <- input$first_indicator_global
    y_col <- input$second_indicator_global
    
    cor = cor(data[[x_col]], data[[y_col]])
    
    cor = round(cor, 4)
    
    
    spr_cor = cor(data[[x_col]], data[[y_col]], method = "spearman")
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
  
  
}

# NOTE: Could have the side bar show one MAIN indicator they want to see that is displayed on the
# map, and that displays the histogram. Below that we could have them choose another indicator they'd
# like it to compare to
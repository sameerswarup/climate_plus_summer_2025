# ui.R

ui <- bslib::page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  tags$h2("Time Series Maps of Global Temperature Anomaly (1990-2025)",
          style = "font-weight: bold; color: var(--bs-primary, #003087)"),
  
  sidebar = sidebar(
    width = 400,
    
    tags$h3("What is global temperature anomaly?"
    ),
    
    tags$small(
      style = "font-style: italic;",
      "The term temperature anomaly means a departure from a reference value or long-term average. A positive anomaly indicates that the observed temperature was warmer than the reference value, while a negative anomaly indicates that the observed temperature was cooler than the reference value."
    ),
    
    tags$h4("Choose a month!"),
    
    # Slider to choose the year
    
    sliderTextInput(
      inputId = "month_slider",
      label = "Select month:",
      choices = wideCol,   # Keep the original names here
      selected = wideCol[1],  # Default selection
      grid = FALSE,
      animate = animationOptions(interval = 200, loop = TRUE)
    ),
    
    plotOutput("histogram"),
    verbatimTextOutput("mmm"),
    
    plotOutput("time_graph")
  ),
  leafletOutput("my_map", height = "600px")
  
  
)

# ui.R

ui <- bslib::page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Time Series Maps of Global Temperature Anomaly and ND GAIN",
  
  sidebar = sidebar(
    width = 450,
    
    selectInput(inputId = "map_type",
                label = "Choose a map type:",
                choices = c("Global Temperature Anomaly" = "global_temp_anomaly",
                            "ND Gain Data" = "nd_gain"),
                selected = "global_temp_anomaly"),
    
    # Conditional Panel for Global Temperature Anomaly
    
    conditionalPanel(
      condition = "input.map_type == 'global_temp_anomaly'",
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
    
    # Conditional Panel for ND Gain Data
    
    conditionalPanel(
      condition = "input.map_type == 'nd_gain'",
      tags$h3("What is ND Gain?"
      ),
      tags$small(
        style = "font-style: italic;",
        "All countries, to different extents, are facing the challenges of adaptation. Due to
        geographical location or socio-economic condition, some countries are more vulnerable
        to the impacts of climate change than others. Further, some countries are more ready to
        take on adaptation actions by leveraging public and private sector investments, through
        government action, community awareness, and the ability to facilitate private sector
        responses. ND-GAIN measures both of these dimensions: vulnerability and readiness."
      ),
      selectInput(inputId = "country_nd",
                  label = "Choose a country:",
                  choices = country_names,
                  selected = "Afghanistan"),
      selectInput(inputId = "variable_nd",
                  label = "Choose a variable/indicator:",
                  choices = gainVars,
                  selected = "Value..gain"),
      sliderInput(inputId = "nd_year",
                  label = "Choose a year:",
                  min = 1995,
                  max = 2022,
                  value = 1995,
                  sep = "",
                  animate = TRUE),
      card(
        card_header("Data Summary")
      ),
      plotOutput("nd_graph")
      
    )
    
    
  ),
  
  leafletOutput("my_map", height = 600)
  
)

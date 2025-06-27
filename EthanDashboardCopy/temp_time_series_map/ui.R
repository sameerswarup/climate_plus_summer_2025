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
                selected = "nd_gain"),
    
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
        "The Notre Dame Global Adaptation Initiative’s (ND-GAIN) Country Index is a free, open
        source index that shows a country’s current vulnerability to climate disruptions. It also
        assesses a country’s readiness to leverage private and public sector investment for
        adaptive actions. The ND-GAIN Country Index brings together more than 40 core
        indicators to measure vulnerability and readiness of 182 UN countries from 1995 to the
        present (10 countries only have readiness scores)."
      ),
      selectInput(inputId = "country_nd",
                  label = "Choose a country:",
                  choices = country_names,
                  selected = "Afghanistan"),
      selectInput(inputId = "variable_nd",
                  label = "Choose a variable/indicator:",
                  choices = gainVars,
                  selected = "Value..gain"),
      
      tags$small(
        style = "font-style: italic;",
        textOutput("indDescOutput")
      ),
      sliderInput(inputId = "nd_year",
                  label = "Choose a year:",
                  min = 1995,
                  max = 2022,
                  value = 1995,
                  sep = "",
                  animate = TRUE),
      
      card(
        card_header("Data Summary"),
        tags$div(
          style = "text-align: center;",
          tags$h4(textOutput("variableNameAndYearOutput")        
          )
          
        ),
        value_box(
          title = "Hello",
          value = textOutput("nd_year_score")
        )
      ),
      plotOutput("nd_graph")
      
    )
    
    
  ),
  
  leafletOutput("my_map", height = 600)
  
)

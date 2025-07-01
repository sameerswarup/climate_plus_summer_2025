ui <- page_sidebar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 400,
    
    tags$div(
      style = "margin-bottom: 20px;",
      tags$h3("Climate Variable Projection Map", style = "color: var(--bs-primary, #003087); font-weight: bold; margin:0px;")
    ),
    
    # Composite score selector
    
    selectInput("composite_score",
                "Select Composite Score:",
                choices = names(composite_data_options),
    ),
    
    # Conditional Panel for Climate Risk
    
    conditionalPanel(
      condition = "input.composite_score == 'Climate Risk'",
      card(
        card_header("Climate Variable Controls"),
        
        
        # Climate variable selector (top level)
        selectInput("climate_variable", 
                    "Select Climate Variable:", 
                    choices = names(climate_data_options)),
        
        # Data type selector (second level)
        uiOutput("data_type_selector"),
        
        # Time period selector (third level)
        uiOutput("time_period_selector"),
        
        # Variable info display
        uiOutput("variable_info")
      ),
      
      # Filter controls card
      card(
        card_header("Data Filters"),
        
        # Dynamic range slider based on data
        uiOutput("value_range_slider"),
        
        # Filter options
        radioButtons("filter_mode", "Filter Mode:",
                     choices = list(
                       "Show All Data" = "none",
                       "Show Values in Range" = "range",
                       "Show Above Threshold" = "above", 
                       "Show Below Threshold" = "below"
                     ),
                     selected = "none"
        ),
        
        # Reset button
        actionButton("reset_filters", "Reset Filters", 
                     class = "btn-outline-secondary btn-sm",
                     style = "margin-top: 10px;")
      ),
      
      # Info display
      card(
        card_header("Data Summary"),
        verbatimTextOutput("data_info")
      )
    ),
    # Conditional Panel for ND Gain
    
    conditionalPanel(
      condition = "input.composite_score == 'ND Gain",
      card(
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
        )
      ),
      card(
        card_header("Map Controls"),
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
                    animate = TRUE)
      ),
      card(
        card_header("Data Summary"),
        
        value_box(
          title = textOutput("variableNameAndYearOutput"),
          value = textOutput("nd_year_score")
        )
      ),
      plotOutput("nd_graph"),
      
      card(
        card_header("Data Source"),
        tags$a(href = "https://gain.nd.edu/our-work/country-index/download-data/",
               "Notre Dame Global Adaptation Initiative",
               target = "_blank"),
        tags$small("Download up to half a million data points for more than 180 UN countries. Data is updated annually, but includes all ND-GAIN indicators across 20+ years. Data is provided as separate CSV files in a single compressed file.",
                   style = "font-style: italic")
      )
      
    )
    ),
    
    
    
  
  navset_card_tab(
    id = "tabset",
    
    # Add Conditional Panels Here
    
    
    nav_panel("Interactive Map", 
              # climate_maps
              conditionalPanel(
                condition = "input.composite_score == 'Climate Risk'" ,
                leafletOutput("climate_map", height = 600))
              ),
              
              #ND Gain 
              conditionalPanel(
                condition = "input.composite_score == 'ND Gain'",
                leafletOutput("nd_gain_map", height = 600)
              )
              
  )
)
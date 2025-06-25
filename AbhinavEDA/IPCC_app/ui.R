ui <- page_sidebar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 400,
    
    tags$div(
      style = "margin-bottom: 20px;",
      tags$h3("Climate Variable Projection Map", style = "color: var(--bs-primary, #003087); font-weight: bold; margin:0px;")
    ),
    
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
  
  navset_card_tab(
    id = "tabset",
    nav_panel("Interactive Map", leafletOutput("climate_map", height = 600))
  )
)
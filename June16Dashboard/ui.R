library(shiny)
library(leaflet)
library(bslib)

ui <- page_sidebar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  # input_dark_mode(id = "light"), 
  sidebar = sidebar(
    width = 400,
    # imageOutput("dataplus_logo"),
    tags$h1("Inequity Indicators Map", style = "color: #003087;
            font-weight: bold"),
    selectInput("indicator_category", "Choose Indicator Category:", 
                choices = indicator_choices, selected = "Ecological"),
    selectInput("mean_type", "Choose Mean Type:", 
                choices = mean_choices, selected = "Arithmetic Mean"),
    
    hr(), # HORIZONAL DIVIDING LINE
    
    tags$h3("Zoom", style = "color: #003087;
            font-weight: bold"),
    
    uiOutput("country_selector"),
    
    uiOutput("region_selector"),
    
    actionButton("zoom_button", "Zoom"),
    
    hr(), # HORIZONAL DIVIDING LINE
    
    tags$h3("Make Your Own CUSTOM GRAPH!",
            style = "color: #003087;
            font-weight: bold"),
    
    tags$h6("Global: Each data point represents the average score of a country." ,
            style = "font-style: italic"),
    
    
    tags$h6("Country: Each data point represents the score of a district within a chosen country." ,
            style = "font-style: italic"),
    
    
    # GLOBAL OR COUNTRY-LEVEL
    
    selectInput("global_or_country", "Select level of investigation:",
                choices = c("Global" = "global",
                            "Country" = "country"),
                selected = "Global"),
    
    
    # REACTIVE FOR WHETHER GLOBAL OR COUNTRY IS SELECTED
    
    uiOutput("global_or_country_components")
    
  ),
  leafletOutput("map", height = 600)
)

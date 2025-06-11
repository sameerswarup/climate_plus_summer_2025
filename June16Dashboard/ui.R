library(shiny)
library(leaflet)
library(bslib)

ui <- page_sidebar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 400,
    
    # Better spacing and styling for header
    tags$div(
      style = "margin-bottom: 20px;",
      # imageOutput("dataplus_logo"),
      tags$h1("Inequity Indicators Map", 
              style = "color: var(--bs-primary, #003087); font-weight: bold; margin-bottom: 20px;")
    ),
    
    # Map controls - only show on Interactive Map tab
    conditionalPanel(
      condition = "input.tabset == 'Interactive Map'",
      card(
        card_header("Map Controls"),
        selectInput("indicator_category", "Choose Indicator Category:", 
                    choices = indicator_choices, selected = "Ecological"),
        selectInput("mean_type", "Choose Mean Type:", 
                    choices = mean_choices, selected = "Arithmetic Mean"),
        
        selectizeInput("country_search", "Jump to Country:", 
                       choices = NULL, selected = NULL),
        actionButton("zoom_button", "Zoom to Selected Country", 
                     style = "width: 100%; margin-top: 10px;"),
        
        # Satellite view toggle
        tags$div(
          style = "margin-top: 15px;",
          checkboxInput("satellite_view", "Satellite View", value = FALSE)
        )
      )
    ),
    
    # Custom graph controls - only show on Custom Graphs tab
    conditionalPanel(
      condition = "input.tabset == 'Custom Graphs'",
      card(
        card_header(
          tags$h3("Make Your Own CUSTOM GRAPH!", 
                  style = "color: var(--bs-primary, #003087); font-weight: bold; margin: 0;")
        ),
        
        # Better spacing for explanatory text
        tags$div(
          style = "margin-bottom: 15px;",
          tags$h6("Global: Each data point represents the average score of a country.", 
                  style = "font-style: italic; margin-bottom: 5px;"),
          tags$h6("Country: Each data point represents the score of a district within a chosen country.", 
                  style = "font-style: italic; margin-bottom: 0;")
        ),
        
        # Analysis level selection
        selectInput("global_or_country", "Select level of investigation:",
                    choices = c("Global" = "global", "Country" = "country"),
                    selected = "Global"),
        
        # Dynamic components
        uiOutput("global_or_country_components")
      )
    )
  ),
  
  # Main content area with tabs
  navset_card_tab(
    id = "tabset",
    nav_panel("Interactive Map", 
              leafletOutput("map", height = 600)),
    
    nav_panel("Custom Graphs",
              # Global analysis results
              conditionalPanel(
                condition = "input.global_or_country == 'global'",
                tags$div(
                  style = "padding: 20px;",
                  tags$h3("Global Analysis Results", 
                          style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                  
                  card(
                    card_header("Bivariate Scatter Plot"),
                    plotOutput("global_custom_scatter")
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  card(
                    card_header("Correlation Analysis"),
                    verbatimTextOutput("global_correlation")
                  )
                )
              ),
              
              # Country analysis results
              conditionalPanel(
                condition = "input.global_or_country == 'country'",
                tags$div(
                  style = "padding: 20px;",
                  tags$h3("Country Analysis Results", 
                          style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                  
                  # Country info section
                  card(
                    card_header("Selected Country"),
                    tags$div(
                      style = "text-align: center; padding: 15px;",
                      textOutput("countryDisplay"),
                      tags$div(style = "margin: 10px 0;"),
                      imageOutput("country_flag", height = "120px")
                    )
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  # Histogram section
                  card(
                    card_header("Distribution Analysis"),
                    plotOutput("country_histogram")
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  # Bivariate analysis section
                  card(
                    card_header("Bivariate Analysis"),
                    plotOutput("custom_scatter"),
                    tags$div(
                      style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
                      verbatimTextOutput("correlation")
                    )
                  )
                )
              )
    ),
    
    nav_panel("Country Comparison",
              tags$div(
                style = "padding: 20px; text-align: center;",
                tags$h3("Country Comparison Tool", 
                        style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                tags$p("This feature will be implemented soon! yay!", 
                       style = "font-size: 18px; color: #666; font-style: italic;"),
                
                
                fluidRow(
                  column(width = 6,
                         style = "height: 30vh;",  # 100% viewport height, light background
                         div(style = "width: 100%; height: 10%;",
                             leafletOutput("compare_map_1", width = "100%", height = 250)
                         )
                  ),
                  column(width = 6,
                         leafletOutput("compare_map_2", width = "100%", height = 250)
                  )
                ),
                
                
                fluidRow(
                  column(width = 6,
                         h5("Summary Statistics"),
                  ),
                  column(width = 6,
                         h5("Summary Statistics"),
                  )
                )
              )
    )
  )
)
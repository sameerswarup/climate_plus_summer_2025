library(shiny)
library(leaflet)
library(bslib)

ui <- page_navbar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  # input_dark_mode(id = "light"), 
  
<<<<<<< HEAD
  nav_panel("Map",
            page_sidebar(sidebar = sidebar(
              width = 400,
              # imageOutput("dataplus_logo"),
              tags$h1("Composite Score Map", style = "color: #003087;
            font-weight: bold"),
              selectInput("indicator_category", "Choose Composite Score:", 
                          choices = indicator_choices, selected = "Ecological"),
              selectInput("mean_type", "Choose Mean Type:", 
                          choices = mean_choices, selected = "Arithmetic Mean"),
              
              selectizeInput("country_search", "Jump to Country:", choices = NULL, selected = NULL),
              actionButton("zoom_button", "Zoom to Selected Country"),
              
              hr(), # HORIZONAL DIVIDING LINE
              # 
              # tags$h3("Zoom", style = "color: #003087;
              #         font-weight: bold"),
              # 
              # uiOutput("country_selector"),
              # 
              # uiOutput("region_selector"),
              # 
              # actionButton("zoom_button", "Zoom"),
              # 
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
            ),
  nav_panel("Country Comparison", 
            page_fillable(
              tags$h1("Country Comparison", style = "color: #003087;
            font-weight: bold"),
              layout_columns(
                
                card(
                  
                ),
                
                card(
                  
                  
                )
                
              )
              
              
            )
  )
  )
=======
  sidebar = sidebar(
    width = 400,
    
    # Better spacing and styling for header
    tags$div(
      style = "margin-bottom: 20px;",
      # imageOutput("dataplus_logo"),
      tags$h1("Inequity Indicators Map", 
              style = "color: #003087; font-weight: bold; margin-bottom: 20px;")
    ),
    
    # Card for map controls
    card(
      card_header("Map Controls"),
      selectInput("indicator_category", "Choose Indicator Category:", 
                  choices = indicator_choices, selected = "Ecological"),
      selectInput("mean_type", "Choose Mean Type:", 
                  choices = mean_choices, selected = "Arithmetic Mean"),
      
      selectizeInput("country_search", "Jump to Country:", 
                     choices = NULL, selected = NULL),
      actionButton("zoom_button", "Zoom to Selected Country", 
                   style = "width: 100%; margin-top: 10px;")
    ),
    
    # Spacing between cards
    tags$div(style = "margin: 20px 0;"),
    
    # Card for custom analysis
    card(
      card_header(
        tags$h3("Make Your Own CUSTOM GRAPH!", 
                style = "color: #003087; font-weight: bold; margin: 0;")
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
  ),
  
  # Main content area with tabs
  navset_card_tab(
    nav_panel("Interactive Map", 
              leafletOutput("map", height = 600)),
    
    nav_panel("Analysis Results",
              # Global analysis results
              conditionalPanel(
                condition = "input.global_or_country == 'global'",
                tags$div(
                  style = "padding: 20px;",
                  tags$h3("Global Analysis Results", 
                          style = "color: #003087; margin-bottom: 20px;"),
                  
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
                          style = "color: #003087; margin-bottom: 20px;"),
                  
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
                        style = "color: #003087; margin-bottom: 20px;"),
                tags$p("This feature will be implemented soon!", 
                       style = "font-size: 18px; color: #666; font-style: italic;"),
                tags$p("soon features:", style = "margin-top: 30px; font-weight: bold;"),
                tags$ul(
                  style = "text-align: left; max-width: 400px; margin: 0 auto;",
                  tags$li("Side-by-side country comparison"),
                  tags$li("more features")
                )
              )
    )
  )
)
>>>>>>> 2a36dbc46e7afb0b3a91c0479c5d3e181d463f7a

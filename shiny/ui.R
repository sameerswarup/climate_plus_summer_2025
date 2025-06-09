ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center {
        text-align: center;
      }
      .font {
        font-family: Verdana;
      }
      .header_color {
        color: #003087;
      }
      .bold {
        font-weight: bold;
      }
    "))
  ),
  titlePanel(
    title=tags$h2(div("High Stakes Socio-Economic Mapping of Coastal Zones", class = "center font header_color bold")), 
    windowTitle = "High Stakes Socio-Economic Mapping of Coastal Zones"),
  
  
    tabsetPanel(
      tabPanel(
        title = tags$h5(div("Dashboard", class = "font header_color center")),
        
      
        page_sidebar(
          title = tags$h3(div("Main Dashboard", class = "font header_color center")),
          
          sidebar = sidebar(
            width = 500,

            uiOutput("country_selector"),
            
            uiOutput("region_selector"),
            
            actionButton("zoom_button", "Zoom"),
            
            
            selectInput("score_type", "Select Indicator to Display",
                                        choices = c(
                                          "Governance Quality" = "governance_score",
                                          "Social Inequality" = "inequality_score",
                                          "Economic Dependence" = "economic_dependence_score",
                                          "Coastal Exposure" = "coastal_exposure_score"
                                        ),
                                        selected = "inequality_score"
                            ),
          # "Here will go the descriptions
          # of the data!",
          # tableOutput("data_descriptions"),
          tags$h2("Indicator Description", 
                  style = "color: #003087; 
                  font-weight: bold;"),
          textOutput("description"),
        
          
          tags$div(
            style = "text-align: center;",
            tags$h3(textOutput("clicked_country"))
          ),
      
          # Could include a short description of the country here
          
          tags$div(
            style = "text-align: center;",
            imageOutput("country_flag", height = "120px")
          ),
          value_box(
            title = textOutput("label"),
            value = uiOutput("scoreInQuestion"),
            showcase = bs_icon("person-arms-up"),
            theme = value_box_theme(fg = "white", bg = "#003087")
          ),
          
          # could use ggplot for histogram to make prettier
          
          plotOutput("histogram"),
          
          # NOTE: could be cool to show a histogram in the
          # value box that shows where the country lies
          # in comparison to every other country
          
          tags$h3("Factor Descriptions",
                  style = "color: #003087;"),
          
          "Here we can put the descriptions of the
          factors to calculate this indicator",
          
          # selectInput(inputID = "factorSelect",
          #             label = "Select a factor!",
          #             choices = c("A" = "a",
          #                         "B" = "b",
          #                         "C" = "c"),
          #             selected = "a"),
          
          tableOutput("country_scores"),
      
        ),
        leafletOutput("map", width = "100%", height = 200)
        #find a way so that it doesn't scroll with
      )
    
    ),

    
    
    tabPanel(
      title = tags$h5(div("Compare Indicators", class = "font header_color")),
      tags$h3("Compare indicators", class="font header_color", style="margin-top: 2px"),
      tags$hr(style="margin-top: 3px;margin-bottom: 3px; border: 0; border-top: 1px  solid grey "),
      fluidRow(
        column(width = 6,
               style = "height: 25vh;",  # 100% viewport height, light background
               div(style = "width: 100%; height: 10%;",
                   h5("Search"),
                   textInput("search", label = h5("Country Inspection"), value = "Enter a country..."),
                   actionButton("submit_search", "Search")
               )
        ),
        column(width = 6,
               h5("Search"),
               textInput("search", label = h5("Country Inspection"), value = "Enter a country..."),
               actionButton("submit_search", "Search")
               )
      ),
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

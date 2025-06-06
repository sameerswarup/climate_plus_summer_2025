ui <- page_sidebar(
    sidebar = sidebar(
      width = 400,
      tags$h1("Make Your Own CUSTOM GRAPH!",
              style = "color: #003087;
            font-weight: bold"),
      selectInput("country_select", "Select a Country to Compare",
                  choices = sort(unique(df_population$COUNTRYNM)),
                  selected = "Japan"),
      
      textOutput("countryDisplay"),
      
      tags$div(
        style = "text-align: center;",
        imageOutput("country_flag", height = "120px")
      ),
      
      # selectInput("score_type", "Select Indicator to Display:",
      #             choices = c(
      #               "Governance Quality" = "governance_score",
      #               "Social Inequality" = "inequality_score",
      #               "Economic Dependence" = "economic_dependence_score",
      #               "Coastal Exposure" = "coastal_exposure_score"
      #             ),
      #             selected = "inequality_score"
      # ),
      
      # "Do you want to create your own graph? Here you can make it for yourself!",
      
      tags$h4("Bivariate Graphs!",
              style = "color: #003087;
            font-style: italic"),
      
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
      
      plotOutput("custom_scatter"),
      
      verbatimTextOutput("correlation")
      
  ),
  leafletOutput("map", width = "90%", height = 200)
  
  
)
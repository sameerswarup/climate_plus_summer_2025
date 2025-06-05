ui <- fluidPage(
  titlePanel("Generalized Coastal Inequity Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("score_type", "Select Score to Display:",
                  choices = c(
                    "Governance Quality" = "governance_score",
                    "Social Inequality" = "inequality_score",
                    "Economic Dependence" = "economic_dependence_score",
                    "Coastal Exposure" = "coastal_exposure_score"
                  ),
                  selected = "inequality_score"
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = 800)
    )
  )
)
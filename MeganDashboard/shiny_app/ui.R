ui <- fluidPage(
  titlePanel("Generalized Coastal Inequity Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("score_type", "Select Score to Display:",
                  choices = c("Mean Inequality" = "mean_ineq",
                              "Government Effectiveness" = "mean_gov",
                              "Voice & Accountability" = "mean_voice"),
                  selected = "mean_ineq")
    ),
    
    mainPanel(
      leafletOutput("map", height = 800)
    )
  )
)

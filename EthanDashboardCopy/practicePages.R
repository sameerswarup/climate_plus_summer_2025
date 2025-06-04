library(shiny)
library(bs4Dash)

ui <- fluidPage(
  titlePanel("Practice Dashboard Adding Pages"),
  sidebarLayout(
    sidebarPanel(width = 250,
                 selectInput(
                   "score_type",
                   "Select Score to Display:",
                   choices = c(
                     "Governance Quality" = "governance_score",
                     "Social Inequality" = "inequality_score",
                     "Economic Dependence" = "economic_dependence_score",
                     "Coastal Exposure" = "coastal_exposure_score"
                   ),
                   selected = "inequality_score"
                 ),
                 sliderInput("obs",
                             "Number of observations:",
                             min = 0,
                             max = 1000,
                             value = 500)
                 ),
    mainPanel(
      textOutput("selectedOutput"),
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot ({
    hist(rnorm(input$obs))
  })
  
  output$selectedOutput <- renderText ({
    paste0("Histogram of ", input$score_type)
  })
}

shinyApp(ui, server)
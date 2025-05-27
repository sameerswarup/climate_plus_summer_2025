ui <- page_sidebar(
  title = "Generalized Coastal Inequity Dashboard",
  sidebar = sidebar(
    width = 500,
    selectInput("score_type", "Select Score to Display:",
                                  choices = c(
                                    "Governance Quality" = "governance_score",
                                    "Social Inequality" = "inequality_score",
                                    "Economic Dependence" = "economic_dependence_score",
                                    "Coastal Exposure" = "coastal_exposure_score"
                                  ),
                                  selected = "inequality_score"
                      ),
    tags$h2(textOutput("clicked_country")),
    "Here will go the descriptions
    of the data!",
    tableOutput("data_descriptions"),
    #tags#
    tags$h2("Indicator Description", style = "color: blue;"),
    textOutput("description"),
    
    tags$div(
      style = "text-align: center;",
      imageOutput("country_flag", height = 120)
    ),
    value_box(
      title = textOutput("label"),
      value = uiOutput("scoreInQuestion"),
      theme = "teal"
    ),
    
    # NOTE: could be cool to show a histogram in the
    # value box that shows where the country lies
    # in comparison to every other country
    
    tableOutput("country_scores"),
    
  ),
  leafletOutput("map", height = 800)
    
)

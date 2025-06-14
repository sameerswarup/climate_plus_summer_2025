ui <- fluidPage(
  title = "Mapping High Stakes Coastal Zones",
  tabsetPanel(
  
  tabPanel(
    title = tags$h6("Dashboard",
                    style = "font-family: Papyrus; color: #003087;"),
    
  
  page_sidebar(
    title = tags$h1("Generalized Coastal Inequity Dashboard",
                    style = "color: #003087;
                  font-weight: bold;"),
    
    tags$h4("A nice little description of the dashboard and what we are
            trying to do with it!",
            style = "color: #003087;
            font-style: italic"),
  sidebar = sidebar(
    width = 500,
    selectInput("score_type", "Select Indicator to Display:",
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
  leafletOutput("map", width = "90%", height = 200)
  #find a way so that it doesn't scroll with
)

),

tabPanel(
  title = tags$h6("Custom Graphs",
                  style = "font-family: Courier New;
                  color: #003087;"),
  
  leafletOutput("popmap", width = "90%", height = 500),
  
  tags$div(
    tags$h2("Hi, do you want a custom graph?
            Here we will give you one!",
            style = "font-family: Courier New;
                  color: #003087;"),
    
    )
  
),

tabPanel(
  title = tags$h6("Country Comparison",
                  style = "font-famiily: Verdana;
                  color: #003087;"),
),

tabPanel(
  title = tags$h6("Individual Country Inspection",
                  style = "font-family: Lucida Handwriting;
                  color: #003087;"),
  
  page_sidebar(
    sidebar = sidebar(
      width = 500,
      textInput("search", label = h3("Country Inspection"), value = "Enter a country..."),
      actionButton("submit_search", "Search")
    ),
    
    plotOutput("indCountryMap")
  )
  
)

)

)

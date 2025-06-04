library(shiny)
library(bs4Dash)

ui <- dashboardPage(
  title = "Basic Dashboard",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Data+ Dashboard",
      color = "navy",
      image = "/Users/student/Desktop/data-plus-logo copy.png"
    ),
    status = "white"
  ),
  sidebar = dashboardSidebar(
    skin = "light", # light or dark (night or day etc.)
    status = "primary", # customize colors of the buttons
    elevation = 3,
    sidebarUserPanel( # can add an iamge if you want to, or a nice little greeting or subtitle
      image = NULL,
      name = "Choose a country!"
    ),
    sidebarMenu(
      sidebarHeader("Select:"),
      menuItem(
        "South Korea",
        tabName = "rok",
        icon = icon("sliders")
      ),
      menuItem(
        "United States of America",
        tabName = "usa",
        icon = icon("chart-simple")
      )
    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "rok",
        box(
          title = "South Korea"
          )
        ),
      tabItem(tabName = "usa",
              box(
                title = "United States of America"
              )
      )
      
              
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)
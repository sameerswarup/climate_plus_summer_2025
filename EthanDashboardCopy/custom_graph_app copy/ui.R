ui <- page_sidebar(
    input_dark_mode(id = "light"), 
    sidebar = sidebar(
      width = 450,
      tags$h1("Make Your Own CUSTOM GRAPH!",
              style = "color: #003087;
            font-weight: bold"),
      
      "It's important to be able to visualize data, and we wanted to make this data easily 
      accessible for you! Select TWO INDICATORS and see how these indicators compare across
      countries and within countries",
      
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
  
  # DISPLAYS WORLD MAP
  
  leafletOutput("map", width = "90%", height = 200)
  
  
)
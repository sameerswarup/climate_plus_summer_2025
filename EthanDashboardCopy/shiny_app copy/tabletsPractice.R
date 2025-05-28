library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rnaturalearth)
library(scales)
library(DT)
library(tidyr)

df <- readRDS("data copy/df.cont.inequity.compo.coastal.scores_sr.rds") %>%
  st_transform(4326)

df_country <- df |>
  st_drop_geometry() |>
  group_by(name_en) |>
  summarise(
    governance_score = mean(c(
      Voice_account.sc, Gov_effect.sc, Reg_quality.sc,
      Rule_law.sc, control_corr.sc, Political_stab.sc
    ), na.rm = TRUE),
    
    inequality_score = mean(c(
      gender.ineq.sc, income.ineq.sc, le.ineq.log.sc,
      ineq.score.rank, hierachical.score.rank.ineq
    ), na.rm = TRUE),
    
    economic_dependence_score = mean(c(
      Nutritional.dependence.sc, Economic.dependence.sc,
      income.ineq.change.sc, le.ineq.change.sc
    ), na.rm = TRUE),
    
    coastal_exposure_score = mean(c(
      mean.count.grav.V2.log.sc, povmap.grdi.v1.sc,
      perc.pop.world.coastal.merit.10m.log.sc, vulnerab.score.rank
    ), na.rm = TRUE)
  )

world <- ne_countries(returnclass = "sf")
world_joined <- world |>
  left_join(df_country, by = c("name" = "name_en"))

descriptions <- suppressWarnings(read.csv("data copy/dataDescriptions.csv"))

countryCodes <- suppressWarnings(read.csv("data copy/countries_codes_and_coordinates.csv"))


findPNGpath <- function(name_en) {
  pngDefaultPath <- "www/flags/"
  countryCodes <- suppressWarnings(read.csv("data copy/countries_codes_and_coordinates.csv"))
  alpha2 <- countryCodes %>%
    filter(Country == name_en) %>%
    pull(Alpha.2.code)
  alpha2<- substring(alpha2, 3, 4)
  alpha2 <- paste0(tolower(alpha2))
  pngFinal <- paste0(pngDefaultPath, alpha2, ".png")
  return(pngFinal)
}

ui <- fluidPage(
  titlePanel(tags$h1("Generalized Coastal Inequity Dashboard",
                     style = "color: #003087;
                  font-weight: bold;")),
  tabsetPanel(
    tabPanel(
      "Map",
      page_sidebar(
        title = tags$h1("Generalized Coastal Inequity Dashboard",
                        style = "color: #003087;
                  font-weight: bold;"),
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
        leafletOutput("map", height = 800)
        
      )
      
    ),
    tabPanel (
      "Bye!"
      
    )
  )
)

server <- function(input, output) {
  server <- function(input, output, session) {
    
    pal_reactive <- reactive({
      colorNumeric("Blues", world_joined[[input$score_type]], na.color = "transparent")
    })
    
    output$map <- renderLeaflet({
      pal <- pal_reactive()
      
      
      leaflet(data = world_joined) |>
        addTiles() |>
        addPolygons(
          fillColor = ~pal(world_joined[[input$score_type]]),
          layerId = ~name,
          # layerId is an identifier you assign 
          # to each shape (polygon, marker, etc.) 
          # you add to a Leaflet map in Shiny.
          weight = 1,
          color = "grey",
          fillOpacity = 0.8,
          label = ~paste0(
            name, ": ", round(world_joined[[input$score_type]], 2)
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          pal = pal, values = ~world_joined[[input$score_type]],
          title = input$score_type
        )
    })
    
    clicked_country <- reactiveVal(NULL);
    # reactive value holder
    
    # when country is clicked store the name in click
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      clicked_country(click$id)  # This is the "name" of the country
    })
    
    # Display clicked country name
    output$clicked_country <- renderText({
      if (is.null(clicked_country())) {
        paste("Click a country on the map.")
      } else {
        paste(clicked_country())
      }
    })
    
    # Display that country's scores
    output$country_scores <- renderTable({
      req(clicked_country())  # Wait for click
      table <- df_country %>%
        filter(name_en == clicked_country())
    })
    
    output$country_flag <- renderImage({
      # before click
      if (is.null(clicked_country())) {
        list(src = "www/globe.png",
             contentType = "image/png",
             alt = "Globe",
             width = 120,
             height = 120
        )
        # after click
      } else {
        filename <- findPNGpath(clicked_country())
        # Return a list containing the filename
        list(src = filename,
             contentType = "image/png",
             alt = "Country flag",
             width = 160,
             height = 120
        )
      }
    }, deleteFile = FALSE)
    
    # renderImage() function must return a 
    # list with specific named elements that 
    # tell Shiny how to display the image
    
    output$scoreInQuestion <- renderText({
      req(clicked_country())  # Wait for click
      score <- df_country %>%
        filter(name_en == clicked_country()) %>%
        pull(input$score_type)
      
      if(length(score) == 0 || is.na(score)) {
        "No Data"
      } else {
        paste0(round(score, 2))
      }
    })
    
    # reactive value for selected indicator
    clicked_indicator <- reactiveVal(NULL)
    
    observeEvent(input$score_type, {
      click <- input$score_type
      clicked_indicator(click)  # id of indicator
    })
    
    output$data_descriptions <- renderTable({
      req(clicked_indicator)
      score <- descriptions %>%
        filter(id == clicked_indicator())
    })
    
    output$description <- renderText({
      req(clicked_indicator)
      description <- descriptions %>%
        filter(id == clicked_indicator()) %>%
        pull(description)
    })
    
    output$label <- renderText({
      req(clicked_indicator)
      description <- descriptions %>%
        filter(id == clicked_indicator()) %>%
        pull(label)
    })
    
    output$histogram <- renderPlot({
      req(clicked_indicator)
      label <- descriptions %>%
        filter(id == clicked_indicator()) %>%
        pull(label)
      label <- paste(label, "Score", sep = " ")
      label1 <- paste(label, "Distribution", sep = " ")
      score <- input$score_type
      # ggplot(df_country, aes (x = score)) +
      #   geom_histogram(binwidth =0.1)
      histogram <- hist(x = df_country[[score]], # have to use [[]] to access a column by name stored in a variable
                        main = label1,
                        xlab = label)
    })
    
  }
  
  # map_shape_click is a built-in Shiny 
  # input that captures click events on 
  # shapes (like polygons) in a leaflet map.
  
  
  
  # make your own graph tab
  
}

shinyApp(ui, server)
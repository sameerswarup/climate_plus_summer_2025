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

<<<<<<< HEAD
# Load data once at start
data <- read_excel("/Users/student/Desktop/Data+/climate_plus_summer_2025/Data/Coastal Climate Vulnerability/$lecz-v3.xlsm", sheet = "Raw-Combined-Data")
=======
# Load and process data
data <- read_excel("../Data/Coastal Climate Vulnerability/LECZ_data/lecz-v3.xlsm", sheet = "Raw-Combined-Data")
>>>>>>> 5f5bec581aab93e4ce480c971c5b523090d90e4d
world <- ne_countries(scale = "medium", returnclass = "sf")

total_2015 <- data |>
  filter(Year == "2015") |>
  group_by(ISO3) |>
  summarise(Total_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")

lecz_2015 <- data |>
  filter(`LECZ Description` == "0 to 5 Meters", Year == "2015") |>
  group_by(ISO3) |>
  summarise(LECZ_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")

map_data <- left_join(lecz_2015, total_2015, by = "ISO3") |>
  mutate(PERCENT_EXPOSED = ifelse(Total_POP > 0, 100 * (LECZ_POP / Total_POP), NA))

world_exposed <- left_join(world, map_data, by = c("iso_a3" = "ISO3"))

lecz_levels <- c("0 to 5 Meters", "6 to 10 Meters", "Over 10 Meters")

# UI
ui <- fluidPage(
  titlePanel("Low Elevation Coastal Zone Analysis"),
  tabsetPanel(
    tabPanel("Map (2015)", leafletOutput("lecz_map", height = 600)),
    tabPanel("Country Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country1", "Select First Country:", choices = unique(data$Country), selected = "Bangladesh"),
                 selectInput("country2", "Select Second Country:", choices = unique(data$Country), selected = "Vietnam"),
                 selectInput("lecz_level", "Select LECZ Level:", choices = lecz_levels, selected = "0 to 5 Meters")
               ),
               mainPanel(
                 fluidRow(
                   column(6, leafletOutput("map1", height = 300)),
                   column(6, leafletOutput("map2", height = 300))
                 ),
                 DTOutput("comparison_table")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  color_pal <- colorQuantile("YlOrRd", map_data$PERCENT_EXPOSED, n = 5, na.color = "#cccccc")
  
  output$lecz_map <- renderLeaflet({
    leaflet(world_exposed) |>
      addTiles() |>
      addPolygons(
        fillColor = ~color_pal(PERCENT_EXPOSED),
        fillOpacity = 0.8,
        color = "#444444",
        weight = 0.5,
        label = ~paste(name, "<br>% Exposed:", round(PERCENT_EXPOSED, 1), "%")
      ) |>
      addLegend(
        pal = color_pal,
        values = world_exposed$PERCENT_EXPOSED,
        title = "% in 0–5m LECZ (2015)",
        position = "bottomright"
      )
  })
  
  render_comparison_map <- function(country_input) {
    iso <- unique(data$ISO3[data$Country == country_input])
    shape <- world[world$iso_a3 == iso, ]
    stats <- map_data[map_data$ISO3 == iso, ]
    
    if (nrow(shape) == 0 || nrow(stats) == 0) return(NULL)
    
    leaflet(shape) |>
      addTiles() |>
      addPolygons(
        fillColor = color_pal(stats$PERCENT_EXPOSED),
        fillOpacity = 0.8,
        color = "#333333",
        weight = 1,
        label = paste(country_input, "<br>% Exposed:", round(stats$PERCENT_EXPOSED, 1), "%")
      )
  }
  
  output$map1 <- renderLeaflet({
    render_comparison_map(input$country1)
  })
  
  output$map2 <- renderLeaflet({
    render_comparison_map(input$country2)
  })
  
  output$comparison_table <- renderDT({
    req(input$country1, input$country2, input$lecz_level)
    
    exposed_df <- data |>
      filter(Year == 2015, `LECZ Description` == input$lecz_level, Country %in% c(input$country1, input$country2)) |>
      group_by(Country) |>
      summarise(LECZ_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")
    
    total_df <- data |>
      filter(Year == 2015, Country %in% c(input$country1, input$country2)) |>
      group_by(Country) |>
      summarise(Total_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")
    
    combined <- left_join(total_df, exposed_df, by = "Country") |>
      mutate(
        `% Exposed` = round(100 * (LECZ_POP / Total_POP), 1),
        `LECZ Level` = input$lecz_level
      ) |>
      rename(
        `Total Coastal Population` = Total_POP,
        `Population in LECZ Level` = LECZ_POP
      ) |>
      select(`LECZ Level`, Country, `Total Coastal Population`, `Population in LECZ Level`, `% Exposed`)
    
    datatable(combined, rownames = FALSE, options = list(dom = 't'))
  })
}

shinyApp(ui, server)
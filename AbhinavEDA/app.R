library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rnaturalearth)
library(scales)

# Load data once at start
data <- read_excel("../Data/Coastal Climate Vulnerability/lecz-v3.xlsm", sheet = "Raw-Combined-Data")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Preprocess: 2015 exposure choropleth
total_2015 <- data |>
  filter(Year == "2015") |>
  group_by(ISO3) |>
  summarise(Total_POP = sum(`GHS-POP`, na.rm = TRUE))

lecz_2015 <- data |>
  filter(`LECZ Description` == "0 to 5 Meters", Year == "2015") |>
  group_by(ISO3) |>
  summarise(LECZ_POP = sum(`GHS-POP`, na.rm = TRUE))

map_data <- left_join(lecz_2015, total_2015, by = "ISO3") |>
  mutate(PERCENT_EXPOSED = 100 * (LECZ_POP / Total_POP))

world_exposed <- left_join(world, map_data, by = c("iso_a3" = "ISO3"))

# Preprocess: Time trend for top 5 countries
total_pop <- data |>
  group_by(ISO3, Country, Year) |>
  summarise(Total_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")

lecz_pop <- data |>
  filter(`LECZ Description` == "0 to 5 Meters") |>
  group_by(ISO3, Country, Year) |>
  summarise(LECZ_POP = sum(`GHS-POP`, na.rm = TRUE), .groups = "drop")

lecz_percent_over_time <- left_join(lecz_pop, total_pop, by = c("ISO3", "Country", "Year")) |>
  mutate(PERCENT_EXPOSED = 100 * (LECZ_POP / Total_POP)) |>
  filter(!is.na(PERCENT_EXPOSED), Total_POP > 0)

growth_stats <- lecz_percent_over_time |>
  group_by(Country) |>
  summarise(growth = last(PERCENT_EXPOSED) - first(PERCENT_EXPOSED)) |>
  arrange(desc(growth)) |>
  slice_head(n = 5)

top_countries <- growth_stats$Country

lecz_percent_over_time <- lecz_percent_over_time |>
  mutate(ColorGroup = ifelse(Country %in% top_countries, Country, "Other"))

highlight_colors <- hue_pal()(5)
palette_colors <- c("Other" = "gray70", setNames(highlight_colors, top_countries))

# Define UI
ui <- fluidPage(
  titlePanel("Low Elevation Coastal Zone Analysis"),
  tabsetPanel(
    tabPanel("Map (2015)", leafletOutput("lecz_map", height = 600)),
    tabPanel("Trend Over Time", plotOutput("trend_plot", height = 500))
  )
)

# Define server logic
server <- function(input, output, session) {
  output$lecz_map <- renderLeaflet({
    leaflet(world_exposed) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", PERCENT_EXPOSED)(PERCENT_EXPOSED),
        fillOpacity = 0.8,
        color = "#444444",
        weight = 0.5,
        label = ~paste(name, "<br>% Exposed:", round(PERCENT_EXPOSED, 1), "%")
      ) |>
      addLegend(
        pal = colorQuantile("YlOrRd", world_exposed$PERCENT_EXPOSED, n = 5),
        values = world_exposed$PERCENT_EXPOSED,
        title = "% in 0–5m LECZ (2015)",
        position = "bottomright"
      )
  })
  
  output$trend_plot <- renderPlot({
    ggplot() +
      geom_line(data = filter(lecz_percent_over_time, ColorGroup == "Other"),
                aes(x = as.numeric(Year), y = PERCENT_EXPOSED, group = Country, color = ColorGroup),
                size = 1, alpha = 0.6) +
      geom_line(data = filter(lecz_percent_over_time, ColorGroup != "Other"),
                aes(x = as.numeric(Year), y = PERCENT_EXPOSED, group = Country, color = ColorGroup),
                size = 1.2) +
      scale_color_manual(values = palette_colors, name = NULL) +
      labs(
        title = "Exposure to Sea-Level Rise (0–5m LECZ)",
        subtitle = "Top 5 Countries with Highest Growth in Exposure (1990–2015)",
        x = "Year",
        y = "% Population in 0–5m LECZ"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom"
      )
  })
}

# Run the app
shinyApp(ui, server)
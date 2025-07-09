zoom_to_country_nd <- function(map_id, country, zoom_val = 5) {
  coords <- if (is.null(country) || country == "" || country == "Global (Default)") {
    list(X = 0, Y = 0, zoom = 1)
  } else {
    zoom_coords <- country_centroids %>%
      filter(COUNTRY == country) %>%
      select(X, Y) %>%
      as.list()
    
    if (length(zoom_coords$X) > 0) c(zoom_coords, zoom = zoom_val) else list(X = 0, Y = 0, zoom = 1)
  }
  
  leafletProxy(map_id) %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
}

observeEvent(input$country_search, {
  req(input$country_search)
  country <- input$country_search
  countryND(country)
  zoom_to_country_nd("nd_gain_map", country)
})

observeEvent(c(input$nd_year,
               input$variable_nd,
               input$country_search), {
                 req(input$nd_year)
                 req(input$variable_nd)
                 req(input$country_search)
                 country <- input$country_search
                 countryND(country)
                 data <- gain %>%
                   select(ISO3, Name, Year, input$variable_nd) %>%
                   filter(Year == input$nd_year)
                 
                 score <- data %>%
                   filter(Name == country) %>%
                   pull(input$variable_nd)
                 
                 # add it here
                 year <- as.character(input$nd_year)
                 pointData <- gain_wide_points %>%
                   filter(Name == country) %>%
                   select(name_en, iso_a3.x, matches(year))
                 
                 nd_year_score(score)
                 nd_year_data(data)
                 year(input$nd_year)
                 point_data(pointData)
               }
)

observeEvent(c(input$manual_min, input$manual_max), {
  req(input$manual_min, input$manual_max)
  updateSliderInput(
    session,
    "value_range",
    value = c(input$manual_min, input$manual_max)
  )
})

output$variableNameAndYearOutput <- renderText({
  req(varND())
  req(year())
  var <- varND()
  year <- year()
  country <- countryND()
  
  label <- gainVarsNames[gainVars == var]
  label <- paste(label, "for", country, "in", year)
  return(label)
})

output$nd_gain_map <- renderLeaflet({
  year <- input$nd_year
  year_data <- gain %>%
    filter(Year == year)
  ndVar <- input$variable_nd
  pal <- colorNumeric(
    palette = "YlGn",  
    domain = c(min_val_nd(), max_val_nd()),
    reverse = TRUE
  )
  
  leaflet(options = leafletOptions(
    worldCopyJump = FALSE,
    maxBounds = world_bounds,
    maxBoundsViscosity = 1.0
  )) %>% 
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)
})


observe({
  req(input$nd_year)
  nd_data <- nd_year_data()
  req(!is.null(nd_data), nrow(nd_data) > 0)
  
  data <- left_join(world_sf, nd_data, by = c("iso_a3" = "ISO3"))
  
  valid_vals <- na.omit(data[[input$variable_nd]])
  req(length(valid_vals) > 0)  # Make sure there's data
  
  min_val_nd(min(valid_vals))
  max_val_nd(max(valid_vals))
  
  pal <- colorNumeric(
    palette = "YlGn",  
    domain = data$value,
    reverse = TRUE
  )
  
  label <- gainVarsNames[gainVars == input$variable_nd]
  
  leafletProxy("nd_gain_map", data = data) |>
    clearMarkers() |>
    addPolygons(
      fillColor = ~pal(get(input$variable_nd)),  # use tidy eval
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      smoothFactor = 0.5,
      label = ~paste0(Name, ": ", round(get(input$variable_nd), 4)),
      layerId = ~iso_a3
    ) |>
    addLegend(
      pal = pal,
      values = c(min_val_nd(), max_val_nd()),
      opacity = 0.9,
      title = ~paste0(label, " Score"),
      position = "bottomright"
    )
})



observeEvent(input$variable_nd, {
  req(input$variable_nd)
  var <- input$variable_nd
  varND(var)
})


output$nd_graph <- renderPlot({
  req(countryND(), nzchar(countryND()))
  
  filtered <- gain %>%
    filter(Name == countryND())
  
  label <- gainVarsNames[gainVars == varND()]
  
  ggplot(filtered, aes(x = Year, .data[[varND()]])) +
    geom_line(
      size = 1.2,
      alpha = 0.8
    ) +              # line plot over time
    geom_point(
      size = 3
    ) +             # points for each month
    labs(title = paste0(label, " for ", countryND(), " (1995-2022)"),
         subtitle = "Data Sourced from the University of Notre Dame Global Adaptation Initiative",
         x = "Date",
         y = label) +
    theme_hc() +
    theme( # modifies any visual things
      
      axis.title.x = element_text(
        margin = margin(t = 15),
        face = "bold"
      ),
      axis.title.y = element_text(
        margin = margin (r = 15),
        face = "bold"
      ),
      plot.title = element_text(
        size = 15,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0.5),
      text = element_text(
        family = "Sans"
      )
    )
  
})

# Reactive value for indicator descriptions

observeEvent(input$variable_nd, {
  req(varND())
  var <- varND()
  desc <- ndGainDescriptions %>%
    filter(variable_name == var) %>%
    pull(description)
  
  indicator_desc(desc) 
})

output$indDescOutput <- renderText({
  desc <- indicator_desc()
  return(desc)
})


output$nd_year_score <- renderText({
  return(nd_year_score())
})

# Extracting point data


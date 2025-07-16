

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
                 
                 print(score)
                 
                 # add it here
                 year <- as.character(input$nd_year)
                 pointData <- gain_wide_points %>%
                   filter(Name == country) %>%
                   select(name_en, iso_a3.x, matches(year))
                 
                 
                 nd_year_score(score)
                 print(nd_year_score())
                 print(varND())
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
    palette = "Purples",  
    domain = c(min_val_nd(), max_val_nd())
  )
  
  leaflet(options = leafletOptions(
    zoomControl = FALSE,
    worldCopyJump = FALSE,
    maxBounds = world_bounds,
    maxBoundsViscosity = 1.0
  )) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ")
})

observe({
  req(input$nd_year)
  nd_data <- nd_year_data()
  req(!is.null(nd_data), nrow(nd_data) > 0)
  
  # Join world polygons with ND-GAIN data
  data <- left_join(world_sf, nd_data, by = c("iso_a3" = "ISO3"))
  valid_vals <- na.omit(data[[input$variable_nd]])
  req(length(valid_vals) > 0)
  
  min_val_nd(min(valid_vals))
  max_val_nd(max(valid_vals))
  
  pal <- colorNumeric("Purples", domain = data[[input$variable_nd]])
  label <- gainVarsNames[gainVars == input$variable_nd]
  
  proxy <- leafletProxy("nd_gain_map", data = data)
  proxy %>% clearShapes() %>% clearMarkers() %>% clearControls()
  
  # Add this: Re-zoom to country after clearing if country is selected
  if (!is.null(countryND()) && countryND() != "" && countryND() != "Global (Default)") {
    zoom_to_country_nd("nd_gain_map", countryND())
  }
  
  selected_iso <- if (!is.null(countryND())) {
    iso3 <- gain %>%
      filter(Name == countryND()) %>%
      pull(ISO3) %>%
      unique()
    if (length(iso3) > 0) iso3 else NULL
  } else NULL
  
  # Draw polygons — omit selected country
  polygons_to_draw <- if (!is.null(selected_iso)) {
    data %>% filter(iso_a3 != selected_iso)
  } else data
  
  proxy %>% addPolygons(
    data = polygons_to_draw,
    fillColor = ~pal(get(input$variable_nd)),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    smoothFactor = 0.5,
    label = ~paste0(Name, ": ", round(get(input$variable_nd), 4)),
    layerId = ~iso_a3,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )
  
  # Add main legend
  proxy %>% addLegend(
    pal = pal,
    values = valid_vals,
    opacity = 0.9,
    title = paste0(label, " Score"),
    position = "bottomright"
  )
  
  # --- POINTS ---
  if (!is.null(countryND())) {
    country <- countryND()
    var <- varND()
    year_str <- as.character(year())
    
    # Filter relevant point data
    filtered_year_and_country_data <- gain_wide_points %>%
      filter(Name == country)
    
    # Match column like "Value..economic__2009"
    cols_to_keep <- grep(paste0(var, ".*", year_str), colnames(filtered_year_and_country_data), value = TRUE)
    if (length(cols_to_keep) == 0) return()
    
    value_column <- cols_to_keep[1]
    
    filtered_year_and_country_data <- filtered_year_and_country_data %>%
      select(Name, all_of(value_column), geometry) %>%
      sf::st_as_sf()
    
    
    coords <- sf::st_coordinates(filtered_year_and_country_data)
    if (!is.numeric(coords[, 1]) || !is.numeric(coords[, 2])) return()
    
    filtered_year_and_country_data$val_col <- filtered_year_and_country_data[[value_column]]
    
    
    # Plot circle markers
    proxy %>% addCircleMarkers(
      data = filtered_year_and_country_data,
      lng = coords[, 1],
      lat = coords[, 2],
      radius = 6,
      fillColor = ~pal(val_col), 
      fillOpacity = 0.8,
      stroke = FALSE,
      label = ~paste0(Name, ": ", round(val_col, 3))
    )
  }
})

output$nd_graph <- renderPlot({
  req(countryND(), nzchar(countryND()))
  
  filtered <- gain %>%
    filter(Name == countryND())
  
  label <- gainVarsNames[gainVars == varND()]
  
  ggplot(filtered, aes(x = Year, y = .data[[varND()]])) +
    geom_line(
      size = 1.2,
      alpha = 0.8
    ) +              # line plot over time
    geom_point(
      size = 3
    ) +             # points for each month
    labs(title = paste0(label, " for ", countryND(), " (1995-2022)"),
         subtitle = "Data Sourced from the University of Notre Dame",
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
  print(nd_year_score())
  return(nd_year_score())
})

# Extracting point data

observeEvent(input$nd_gain_map_shape_click, {
  clicked_iso <- input$nd_gain_map_shape_click$id
  req(clicked_iso)
  
  clicked_country <- gain %>%
    filter(ISO3 == clicked_iso) %>%
    distinct(Name) %>%
    pull(Name) %>%
    first()
  
  req(!is.null(clicked_country))
  
  updateTextInput(session, "country_search", value = clicked_country)
})
output$summary_title <- renderText({
  req(varND(), year(), countryND())
  
  var <- varND()
  varName <- gainVarsNames[gainVars == var]
  paste(varName, "for", countryND(), "in", year())
})

output$summary_score <- renderText({
  req(nd_year_score())
  round(nd_year_score(), 3)
})
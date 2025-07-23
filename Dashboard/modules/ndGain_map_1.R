
observeEvent(input$comparison_country_search_map_1, {
  req(input$comparison_country_search_map_1)
  country <- input$comparison_country_search_map_1
  countryND_map_1(country)
  zoom_to_country_nd("nd_gain_map_1", country)
})

observeEvent(c(input$nd_year_map_1,
               input$variable_nd_map_1,
               input$comparison_country_search_map_1), {
                 req(input$nd_year_map_1)
                 req(input$variable_nd_map_1)
                 req(input$comparison_country_search_map_1)
                 country <- input$comparison_country_search_map_1
                 countryND_map_1(country)
                 data <- gain %>%
                   select(ISO3, Name, Year, input$variable_nd_map_1) %>%
                   filter(Year == input$nd_year_map_1)
                 
                 score <- data %>%
                   filter(Name == country) %>%
                   pull(input$variable_nd_map_1)
                 
                 # add it here
                 year <- as.character(input$nd_year_map_1)
                 pointData <- gain_wide_points %>%
                   filter(Name == country) %>%
                   select(name_en, iso_a3.x, matches(year))
                 
                 nd_year_score_map_1(score)
                 nd_year_data_map_1(data)
                 year_map_1(input$nd_year_map_1)
                 point_data_map_1(pointData)
               }
)

observeEvent(c(input$manual_min_input_map_1, input$manual_max_input_map_1), {
  req(input$manual_min_input_map_1, input$manual_max_input_map_1)
  updateSliderInput(
    session,
    "value_range_map_1",
    value = c(input$manual_min_input_map_1, input$manual_max_input_map_1)
  )
})

output$variableNameAndYearOutput <- renderText({
  req(varND_map_1())
  req(year_map_1())
  var <- varND_map_1()
  year <- year_map_1()
  country <- countryND_map_1()
  
  label <- gainVarsNames[gainVars == var]
  label <- paste(label, "for", country, "in", year)
  return(label)
})

output$nd_gain_map_1 <- renderLeaflet({
  
  year <- input$nd_year_map_1
  year_data <- gain %>%
    filter(Year == year)
  ndVar <- input$variable_nd_map_1
  pal <- colorNumeric(
    palette = "Purples",  
    domain = c(min_val_nd_map_1(), max_val_nd_map_1())
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
  req(input$nd_year_map_1)
  #req(input$nd_gain_map_1) 
  nd_data <- nd_year_data_map_1()
  req(!is.null(nd_data), nrow(nd_data) > 0)
  
  # Join world polygons with ND-GAIN data
  data <- left_join(world_sf, nd_data, by = c("iso_a3" = "ISO3"))
  valid_vals <- na.omit(data[[input$variable_nd_map_1]])
  req(length(valid_vals) > 0)
  
  min_val_nd_map_1(min(valid_vals))
  max_val_nd_map_1(max(valid_vals))
  
  pal <- colorNumeric("Purples", domain = data[[input$variable_nd_map_1]])
  label <- gainVarsNames[gainVars == input$variable_nd_map_1]
  
  proxy <- leafletProxy("nd_gain_map_1", data = data)
  proxy %>% clearShapes() %>% clearMarkers() %>% clearControls()
  
  # Add this: Re-zoom to country after clearing if country is selected
  if (!is.null(countryND_map_1()) && countryND_map_1() != "" && countryND_map_1() != "Global (Default)") {
    zoom_to_country_nd("nd_gain_map_1", countryND_map_1())
  }
  
  selected_iso <- if (!is.null(countryND_map_1())) {
    iso3 <- gain %>%
      filter(Name == countryND_map_1()) %>%
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
    fillColor = ~pal(get(input$variable_nd_map_1)),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    smoothFactor = 0.5,
    label = ~paste0(Name, ": ", round(get(input$variable_nd_map_1), 4)),
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
  if (!is.null(countryND_map_1())) {
    country <- countryND_map_1()
    var <- varND_map_1()
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
    
    print(filtered_year_and_country_data$val_col)
    
    
    # Plot circle markers
    proxy %>% addCircleMarkers(
      data = filtered_year_and_country_data,
      lng = coords[, 1],
      lat = coords[, 2],
      radius = 6,
      fillColor = ~pal(as.numeric(val_col)), 
      fillOpacity = 0.8,
      stroke = FALSE,
      label = ~paste0(Name, ": ",round(as.numeric(val_col), 3))
    )
  }
})

# output$nd_graph <- renderPlot({
#   req(countryND_map_1(), nzchar(countryND_map_1()))
#   
#   filtered <- gain %>%
#     filter(Name == countryND_map_1())
#   
#   label <- gainVarsNames[gainVars == varND()]
#   
#   ggplot(filtered, aes(x = Year, .data[[varND()]])) +
#     geom_line(
#       size = 1.2,
#       alpha = 0.8
#     ) +              # line plot over time
#     geom_point(
#       size = 3
#     ) +             # points for each month
#     labs(title = paste0(label, " for ", countryND_map_1(), " (1995-2022)"),
#          subtitle = "Data Sourced from the University of Notre Dame Global Adaptation Initiative",
#          x = "Date",
#          y = label) +
#     theme_hc() +
#     theme( # modifies any visual things
#       
#       axis.title.x = element_text(
#         margin = margin(t = 15),
#         face = "bold"
#       ),
#       axis.title.y = element_text(
#         margin = margin (r = 15),
#         face = "bold"
#       ),
#       plot.title = element_text(
#         size = 15,
#         hjust = 0.5
#       ),
#       plot.subtitle = element_text(
#         size = 10,
#         hjust = 0.5),
#       text = element_text(
#         family = "Sans"
#       )
#     )
#   
# })

# Reactive value for indicator descriptions

observeEvent(input$variable_nd_map_1, {
  req(varND_map_1())
  var <- varND_map_1()
  desc <- ndGainDescriptions %>%
    filter(variable_name == var) %>%
    pull(description)
  
  indicator_desc(desc) 
})

output$indDescOutput <- renderText({
  desc <- indicator_desc()
  return(desc)
})


output$nd_year_map_1_score <- renderText({
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
  
  updateTextInput(session, "comparison_country_search_map_1", value = clicked_country)
})
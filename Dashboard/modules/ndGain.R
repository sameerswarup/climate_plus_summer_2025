# ND GAIN Module

# This module is entirely for the server functions pertaining to the ND GAIN
# portion of the Interactive Map page. When the composite score is changed to
# ND GAIN, the entire map and sidebar changes to only display components and
# maps relating to ND GAIN.

# Function to zoom into chosen country polygon

zoom_to_country_nd <- function(map_id, country, zoom_val = 5) {
  
  # Zooms out to whole world view if country is null, empty, or set to "Global (Default)"
  
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

# Observes when the country search text input changes, and when it does,
# changes the countryND() reactiveVal to that country, and also zooms into
# that country using the zoom_to_country_nd function
observeEvent(input$country_search, {
  req(input$country_search)
  country <- input$country_search
  countryND(country)
  zoom_to_country_nd("nd_gain_map", country)
})

# Observes when the year, variable, or country search changes, and initializes
# reactiveVals accordingly.
observeEvent(c(input$nd_year,
               input$variable_nd,
               input$country_search), {
                 req(input$nd_year)
                 req(input$variable_nd)
                 req(input$country_search)
                 
                 # Changes countryND() RV when input$country_search changes
                 country <- input$country_search
                 countryND(country)
                 
                 # Filters GAIN data to only include the selected variable's column
                 # and only country data from the specified year
                 data <- gain %>%
                   select(ISO3, Name, Year, input$variable_nd) %>%
                   filter(Year == input$nd_year)
                 
                 # From the filtered data, pulls the specific variable score for
                 # a specific country to display in the value box
                 score <- data %>%
                   filter(Name == country) %>%
                   pull(input$variable_nd)
                 
                 #print(score)
                 
                 # Get year as a character vector
                 year <- as.character(input$nd_year)
                 
                 # Filter the 800,000+ wide version of GAIN to only include
                 # the points in a certain country from a certain year
                 pointData <- gain_wide_points %>%
                   filter(Name == country) %>%
                   select(name_en, iso_a3.x, matches(year))
                 
                 # Assign all the scores and filtered datasets to their 
                 # respective reactive values
                 nd_year_score(score)
                 nd_year_data(data)
                 year(input$nd_year)
                 point_data(pointData)
               }
)

# Observes the manual minimum and maximum values from the 
# slider for climate risk data
observeEvent(c(input$manual_min, input$manual_max), {
  req(input$manual_min, input$manual_max)
  updateSliderInput(
    session,
    "value_range",
    value = c(input$manual_min, input$manual_max)
  )
})

# Output for variableNameAndYearOutput, which is used for the ggplots and
# general sidebar displays. Just displays something like "Variable Score for
# a Country in 2019" --> eventual textOutput in ui.R
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

# Main code for the output of the nd_gain_map, rendered using Leaflet. The nd_gain_map
# functions similar to the main interactive map, but just uses entirely different code
# due to the time-series nature of the ND GAIN dataset.
output$nd_gain_map <- renderLeaflet({
  year <- input$nd_year
  year_data <- gain %>%
    filter(Year == year)
  ndVar <- input$variable_nd
  
  # Color palette for map and country polygon shading
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

# Observes when the year slider changes
# This chunk is mainly for changing the nd_gain_map based on the different sliders
# using leafletProxy. 
observe({
  req(input$nd_year)
  nd_data <- nd_year_data()
  req(!is.null(nd_data), nrow(nd_data) > 0)
  year <- input$nd_year 
  
  # Join world polygons with ND-GAIN data
  data <- left_join(world_sf, nd_data, by = c("iso_a3" = "ISO3"))
  
  valid_vals <- na.omit(data[[input$variable_nd]])
  req(length(valid_vals) > 0)
  
  # Change the min and max values for the map legend
  min_val_nd(min(valid_vals))
  max_val_nd(max(valid_vals))
  
  # Color palette that dynamically changes based on the chosen variable
  pal <- colorNumeric("Purples", domain = data[[input$variable_nd]])
  
  # Label for legend
  label <- gainVarsNames[gainVars == input$variable_nd]
  
  # Proxy map that will dynamically change
  proxy <- leafletProxy("nd_gain_map", data = data)
  proxy %>% clearShapes() %>% clearMarkers() %>% clearControls()
  
  # Re-zoom to country after clearing if country is selected
  if (!is.null(countryND()) && countryND() != "" && countryND() != "Global (Default)") {
    zoom_to_country_nd("nd_gain_map", countryND())
  }
  
  # Get the selected_iso of the chosen country
  selected_iso <- if (!is.null(countryND())) {
    iso3 <- gain %>%
      filter(Name == countryND()) %>%
      pull(ISO3) %>%
      unique()
    if (length(iso3) > 0) iso3 else NULL
  } else NULL
  
  # Draw polygons but omit selected country so that points will show
  polygons_to_draw <- if (!is.null(selected_iso)) {
    data %>% filter(iso_a3 != selected_iso)
  } else data
  
  # This function is what adds the country polygons onto the map, with the 
  # color palette changing based on the chosen variable.
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
    title = paste0(label, " Score", " (", year, ")"),
    position = "bottomright"
  )
  
  # For point-level resolution --> only show the points for the country chosen.
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
    
    # Coordinates for each of the points.
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

# Output of the time-series graph that shows the variation in the ND GAIN score
# or readiness/vulnerability indicators over time for a specified country.
output$nd_graph <- renderPlot({
  req(countryND(), nzchar(countryND()))
  
  # Filter the GAIN data for all the observations within a country
  filtered <- gain %>%
    filter(Name == countryND())
  
  label <- gainVarsNames[gainVars == varND()]
  
  # ggplot was used for the graph.
  # x-axis is time, y-axis is the column data from varND() which is the chosen
  # variable for ND GAIN.
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

# Reactive value for indicator descriptions that appear below the dropdowns.
observeEvent(input$variable_nd, {
  req(varND())
  var <- varND()
  desc <- ndGainDescriptions %>%
    filter(variable_name == var) %>%
    pull(description)
  
  # Assigns this description to the indicator_desc() reactive value
  indicator_desc(desc) 
})

# Renders the indicator description as a text that will appear in the main UI. This
# indicator description is just a short description of the ND GAIN score or its
# individual vulnerability/readiness indicators.
output$indDescOutput <- renderText({
  desc <- indicator_desc()
  return(desc)
})

# Prints the specific score for a country in a specific year. This is a single
# value that will appear in the value box.
output$nd_year_score <- renderText({
  #print(nd_year_score())
  return(nd_year_score())
})

# Extracting point data based on map clicks. Observes whether the country
# polygons are being clicked on or not, and if they are clicked, then the necessary
# reactive values are also updated.
observeEvent(input$nd_gain_map_shape_click, {
  # input$nd_gain_map_shape_click$id is just the id attached to each of the country 
  # polygons, which were assigned in the addPolygons() function in the ND GAIN map.
  clicked_iso <- input$nd_gain_map_shape_click$id
  req(clicked_iso)
  
  # Get the clicked country's named.
  clicked_country <- gain %>%
    filter(ISO3 == clicked_iso) %>%
    distinct(Name) %>%
    pull(Name) %>%
    first()
  
  req(!is.null(clicked_country))
  
  # Updates the text input box to the clicked country.
  updateTextInput(session, "country_search", value = clicked_country)
})

# The title text for the data summary, which is just the value box that displays
# the score of a country in a year. This, again, is a singular value.
output$summary_title <- renderText({
  req(varND(), year(), countryND())
  
  var <- varND()
  varName <- gainVarsNames[gainVars == var]
  paste(varName, "for", countryND(), "in", year())
})

# RenderText for the nd_year_score(), which is the singular value that will be
# displayed in the value box. The value box's main function is just so the user
# can clearly see a single value/number rather than on a map or on a graph.
output$summary_score <- renderText({
  req(nd_year_score())
  round(nd_year_score(), 3)
})
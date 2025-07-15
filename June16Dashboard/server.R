server <- function(input, output, session) {
  
  source("modules/countryAnalysisModule.R", local = TRUE)
  source("modules/ipcc.R", local = TRUE)
  source("modules/ndGain.R", local = TRUE)
  
  source("modules/ipcc_map_1.R", local = TRUE)
  source("modules/ndGain_map_1.R", local = TRUE)
  source("modules/ipcc_map_2.R", local = TRUE)
  source("modules/ndGain_map_2.R", local = TRUE)
  source("modules/countryComparison.R", local = TRUE)
  
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$nd_gain_map_1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$climate_map_1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$nd_gain_map_2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  output$climate_map_2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      htmlwidgets::onRender("
      function(el, x) {
        this.attributionControl.setPosition('topright');
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  })
  
  
  
  
  selected_country <- reactiveVal(NULL)
  country_dataset <- reactiveVal(NULL)
  map_initialized <- reactiveVal(FALSE)
  
  select_country <- function(country) {
    if (is.null(country) || country == "" || country == "Global (Default)") {
      selected_country(NULL)
      country_dataset(NULL)
      updateTextInput(session, "country_search", value = "")
      updateTextInput(session, "country_search_graphs", value = "")
      zoom_to_country("map", NULL)
    } else {
      selected_country(country)
      country_dataset(filter(df, COUNTRY == country))
      updateTextInput(session, "country_search", value = country)
      updateTextInput(session, "country_search_graphs", value = country)
      zoom_to_country("map", country)
    }
    update_map_layers_only()
  }
  
  zoom_to_country <- function(map_id, country, zoom_val = 5) {
    coords <- if (is.null(country) || country == "Global (Default)") {
      list(X = 0, Y = 20, zoom = 2)
    } else {
      zoom_coords <- country_centroids %>% filter(COUNTRY == country) %>% select(X, Y) %>% as.list()
      if (length(zoom_coords$X) > 0) c(zoom_coords, zoom = zoom_val) else list(X = 0, Y = 20, zoom = 2)
    }
    leafletProxy(map_id) %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
  }
  
  create_base_map <- function(satellite = TRUE) {
    map <- leaflet(options = leafletOptions(
      zoomControl = FALSE,
      maxBounds = list(list(-90, -180), list(90, 180)),
      maxBoundsViscosity = 1.0,
      minZoom = 2,
      maxZoom = 18,
      worldCopyJump = FALSE
    ))
    tiles <- if (satellite) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    map %>% addProviderTiles(tiles) %>% htmlwidgets::onRender("
      function(el, x) {
        this.zoomControl = L.control.zoom({ position: 'topright' }).addTo(this);
      }
    ") 
  }
  
  update_map_layers_only <- function() {
    if (!map_initialized()) return()
    
    var <- if(is.null(input$variable_choice)) "gov.score.rank" else input$variable_choice
    category <- if(is.null(input$indicator_category)) "Weak Governance" else input$indicator_category
    
    country <- selected_country()
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    if (!(var %in% names(global_data)) || all(is.na(global_data[[var]]))) return()
    
    legend_title <- if (var %in% composite_arith_list) {
      category
    } else {
      if (!is.null(indicator_choice_list[[category]]) && var %in% indicator_choice_list[[category]]) {
        names(indicator_choice_list[[category]])[indicator_choice_list[[category]] == var]
      } else {
        "Weak Governance" # fallback
      }
    }
    
    pal <- colorNumeric("Purples", domain = NULL, na.color = "#FFFFFF", reverse = FALSE)
    
    leafletProxy("map") %>%
      clearMarkers() %>% clearShapes() %>% clearControls()
    
    
    if (is.null(country)) {
      leafletProxy("map") %>%
        addPolygons(
          data = polygon_data, labelOptions = labelOptions(
            noHide = FALSE,
            direction = "auto",
            sticky = TRUE
          ),
          fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
          weight = 2, opacity = 0.9,
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
          layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
        ) %>%
        addLegend(pal = pal, values = global_data[[var]][!is.na(global_data[[var]])], opacity = 0.8, title = legend_title, position = "bottomright")
    } else {
      nd_gain_vars <- unlist(gainVars, use.names = FALSE)
      climate_vars <- unlist(climate_data_options, recursive = TRUE, use.names = FALSE)
      is_special_module_var <- (var %in% nd_gain_vars) || (var %in% climate_vars)
      
      if (is_special_module_var) {
        selected_country_data <- polygon_data %>% filter(COUNTRY == country)
        other_countries_data <- polygon_data %>% filter(COUNTRY != country)
        
        if (nrow(other_countries_data) > 0) {
          leafletProxy("map") %>%
            addPolygons(
              data = other_countries_data, labelOptions = labelOptions(
                noHide = FALSE,
                direction = "auto",
                sticky = TRUE
              ),
              fillColor = "transparent", fillOpacity = 0, color = ~pal(get(var)), weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        if (nrow(selected_country_data) > 0) {
          leafletProxy("map") %>%
            addPolygons(
              data = selected_country_data, labelOptions = labelOptions(
                noHide = FALSE,
                direction = "auto",
                sticky = TRUE
              ),
              fillColor = ~pal(get(var)), fillOpacity = 0.8, color = ~pal(get(var)), weight = 3, opacity = 1,
              highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
              layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
            )
        }
        
        leafletProxy("map") %>%
          addLegend(pal = pal, values = global_data[[var]][!is.na(global_data[[var]])], opacity = 0.8,
                    title = paste0("<b>", country, "</b><br>", legend_title), position = "bottomright")
      } else {
        country_data <- df %>% filter(COUNTRY == country)
        if (nrow(country_data) > 0 && var %in% names(country_data)) {
          # Filter out rows with NA values for the variable
          country_data <- country_data %>% filter(!is.na(.data[[var]]))
          
          if (nrow(country_data) > 0) {
            use_local <- isTRUE(input$use_country_specific_scale)
            domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
            
            # Sort data by variable value so darker points (higher values) are drawn last (on top)
            country_data <- country_data %>% arrange(.data[[var]])
            
            pal_country <- colorNumeric("Purples", domain = NULL, na.color = "#FFFFFF", reverse = FALSE)
            border_pal <- colorNumeric("Purples", domain = NULL, na.color = "#FFFFFF", reverse = FALSE)
            
            leafletProxy("map") %>%
              addPolygons(
                data = polygon_data, labelOptions = labelOptions(
                  noHide = FALSE,
                  direction = "auto",
                  sticky = TRUE
                ),
                fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
                weight = 1, opacity = 0.4,
                highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
                layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
              ) %>%
              addCircleMarkers(
                data = country_data, radius = 6, fillColor = ~pal_country(get(var)), fillOpacity = 0.9,
                stroke = FALSE,
                label = ~paste0(COUNTRY, ": ", round(get(var), 3))
              ) %>%
              addLegend(pal = pal_country, values = country_data[[var]], opacity = 0.9,
                        title = paste0("<b>", country, "</b><br>", legend_title, "<br><i>(", if (use_local) "Local" else "Global", ")</i>"),
                        position = "bottomright")
          }
        } else {
          selected_country_data <- polygon_data %>% filter(COUNTRY == country)
          other_countries_data <- polygon_data %>% filter(COUNTRY != country)
          
          if (nrow(other_countries_data) > 0) {
            leafletProxy("map") %>%
              addPolygons(
                data = other_countries_data, labelOptions = labelOptions(
                  noHide = FALSE,
                  direction = "auto",
                  sticky = TRUE
                ),
                fillColor = "transparent", fillOpacity = 0, color = ~pal(get(var)), weight = 2, opacity = 0.5,
                highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
                layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
              )
          }
          
          if (nrow(selected_country_data) > 0) {
            leafletProxy("map") %>%
              addPolygons(
                data = selected_country_data, labelOptions = labelOptions(
                  noHide = FALSE,
                  direction = "auto",
                  sticky = TRUE
                ),
                fillColor = ~pal(get(var)), fillOpacity = 0.8, color = ~pal(get(var)), weight = 3, opacity = 1,
                highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
                layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
              )
          }
          
          leafletProxy("map") %>%
            addLegend(pal = pal, values = global_data[[var]][!is.na(global_data[[var]])], opacity = 0.8,
                      title = paste0("<b>", country, "</b><br>", legend_title), position = "bottomright")
        }
      }
    }
  }
  
  observeEvent(input$indicator_category, {
    if (input$indicator_category %in% c("Social Inequality", "Weak Governance")) {
      updateSelectInput(session, "variable_choice", 
                        choices = indicator_choice_list[[input$indicator_category]],
                        selected = indicator_choice_list[[input$indicator_category]][[1]])
    } else {
      updateSelectInput(session, "composite_choice", 
                        choices = names(composite_data_options),
                        selected = names(composite_data_options)[1])
      
      first_composite <- names(composite_data_options)[1]
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[first_composite]],
                        selected = composite_data_options[[first_composite]][[1]])
    }
  })
  
  observeEvent(input$composite_choice, {
    req(input$indicator_category)
    if (!(input$indicator_category %in% c("Social Inequality", "Weak Governance"))) {
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[input$composite_choice]],
                        selected = composite_data_options[[input$composite_choice]][[1]])
    }
  })
  
  observeEvent(input$country_histogram_indicator, {
    req(input$country_histogram_indicator)
    for (category in names(indicator_choice_list)) {
      if (input$country_histogram_indicator %in% indicator_choice_list[[category]]) {
        updateSelectInput(session, "indicator_category", selected = category)
        updateSelectInput(session, "variable_choice", selected = input$country_histogram_indicator)
        break
      }
    }
  })
  
  observe({
    countries_list <- c("Global (Default)", sort(unique(average_country_nogeo$COUNTRY)))
    updateSelectizeInput(session, "comparison_country_search_map_1", choices = countries_list, selected = "Global (Default)", server = TRUE)
    updateSelectizeInput(session, "comparison_country_search_map_2", choices = countries_list, selected = "Global (Default)", server = TRUE)
    session$sendCustomMessage("updateCountriesList", countries_list)
  })
  
  observeEvent(input$country_search_graphs_selected, {
    req(input$country_search_graphs_selected)
    select_country(input$country_search_graphs_selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$country_search_selected, {
    req(input$country_search_selected)
    select_country(input$country_search_selected)
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_shape_click, {
    select_country(input$map_shape_click$id)
  })
  
  observeEvent(input$map_marker_click, {
    clicked_country <- gsub("^marker_", "", input$map_marker_click$id)
    select_country(clicked_country)
  })
  
  observeEvent(input$global_view_button, {
    select_country(NULL)
  })
  
  output$map <- renderLeaflet({
    var <- "gov.score.rank"
    req(combined_scores_global, combined_scores_global_polygons, average_country_nogeo, average_country_polygons)
    
    if (var %in% composite_arith_list) {
      global_data <- combined_scores_global
      polygon_data <- combined_scores_global_polygons
    } else {
      global_data <- average_country_nogeo
      polygon_data <- average_country_polygons
    }
    
    if (!(var %in% names(global_data)) || all(is.na(global_data[[var]]))) {
      map_initialized(TRUE)
      return(create_base_map(TRUE))
    }
    
    pal <- colorNumeric("Purples", domain = NULL, na.color = "#FFFFFF", reverse = FALSE)
    
    result <- create_base_map(TRUE) %>%
      addPolygons(
        data = polygon_data, labelOptions = labelOptions(
          noHide = FALSE,
          direction = "auto",
          sticky = TRUE
        ),
        fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
        weight = 2, opacity = 0.9,
        highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
        layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
      ) %>%
      addLegend(pal = pal, values = global_data[[var]][!is.na(global_data[[var]])], opacity = 0.8, title = "Weak Governance", position = "bottomright")
    
    map_initialized(TRUE)
    shinyjs::hide("comparison-maps")
    return(result)
  })
  
  observeEvent({
    input$use_country_specific_scale; input$variable_choice
  }, {
    update_map_layers_only()
  })
  
  observeEvent(input$satellite_view, {
    tiles <- if (!input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  observeEvent(input$satellite_view_comparison, {
    # When checkbox is unchecked (FALSE), show satellite view
    # When checkbox is checked (TRUE), show street map
    tiles <- if (!input$satellite_view_comparison) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    
    # Update all comparison maps
    leafletProxy("map1") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("map2") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("nd_gain_map_1") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("nd_gain_map_2") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("climate_map_1") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("climate_map_2") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  output$countryDisplay <- renderText({
    country <- selected_country()
    if (is.null(country)) {
      "Global view - Click on a country to analyze specific data"
    } else {
      paste("Currently analyzing:", country, "- Map automatically zoomed to this country")
    }
  })
}
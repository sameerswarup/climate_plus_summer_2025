server <- function(input, output, session) {
  
  source("modules/countryAnalysisModule.R", local = TRUE)
  source("modules/ipcc.R", local = TRUE)
  source("modules/ndGain.R", local = TRUE)
  source("modules/ipcc_map_1.R", local = TRUE)
  source("modules/ndGain_map_1.R", local = TRUE)
  source("modules/ipcc_map_2.R", local = TRUE)
  source("modules/ndGain_map_2.R", local = TRUE)
  source("modules/countryComparison.R", local = TRUE)
  source("modules/dataOverview.R", local = TRUE)
  
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

  
  output$nd_gain_map <- renderLeaflet({
    create_base_map(TRUE) %>% 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  output$climate_map <- renderLeaflet({
    create_base_map(TRUE) %>% 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  

  selected_country <- reactiveVal(NULL)
  country_dataset <- reactiveVal(NULL)
  map_initialized <- reactiveVal(FALSE)
  
  app_initialized <- reactiveVal(FALSE)
  inputs_initialized <- reactiveVal(FALSE)
  
  observe({
    if (!inputs_initialized()) {
      isolate({
        updateSelectInput(session, "indicator_category", selected = "Socio-Ecological Vulnerability")
        updateSelectInput(session, "composite_choice", selected = "Inequity")
        updateSelectInput(session, "variable_choice", 
                          choices = indicator_choice_list[["Socio-Ecological Vulnerability"]],
                          selected = "povmap.grdi.v1.sc")
      })
      inputs_initialized(TRUE)
    }
  }, priority = 1000)
  
  observe({
    if (!app_initialized() && inputs_initialized()) {
      app_initialized(TRUE)
    }
  }, priority = 999)
  
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
    if (app_initialized()) {
      update_map_layers_only()
    }
  }
  
  zoom_to_country <- function(map_id, country, zoom_val = 5) {
    coords <- if (is.null(country) || country == "Global (Default)") {
      list(X = 0, Y = 20, zoom = 2)
    } else {
      zoom_coords <- country_centroids %>% filter(COUNTRY == country) %>% select(X, Y) %>% as.list()
      if (length(zoom_coords$X) > 0) c(zoom_coords, zoom = zoom_val) else list(X = 0, Y = 20, zoom = 2)
    }
    
    leafletProxy(map_id) %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
    leafletProxy("nd_gain_map") %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
    leafletProxy("climate_map") %>% setView(lng = coords$X, lat = coords$Y, zoom = coords$zoom)
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

    if (!map_initialized() || !app_initialized()) return()
    cat("Country:", selected_country(), "\n")
    cat("Variable:", input$variable_choice, "\n")
    
    # 
    # if (is.null(input$variable_choice) || input$variable_choice == "") {
    #   cat("Skipping - variable_choice not ready\n")
    #   
    #   return()
    # }
    # if (is.null(input$indicator_category) || input$indicator_category == "") {
    #   return()
    # }
    
    var <- input$variable_choice
    category <- if(is.null(input$indicator_category)) "Socio-Ecological Vulnerability" else input$indicator_category
    
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
        "Socio-Ecological Vulnerability"
      }
    }
    
    pal <- colorNumeric("Purples", domain = NULL, na.color = "#FFFFFF", reverse = FALSE)

    print("leaflet proxy about to run")
    
    leafletProxy("map") %>%
      clearMarkers() %>% clearControls()
    
    
    if (is.null(country) || country == "" || country == "Global (Default)") {
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
          country_data <- country_data %>% filter(!is.na(.data[[var]]))
          
          if (nrow(country_data) > 0) {
            use_local <- isTRUE(input$use_country_specific_scale)
            domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
            
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
    if (!app_initialized()) return()
    
    cat("indicator_category changed to:", input$indicator_category, "\n")
    
    if (input$indicator_category %in% c("Social Inequality", "Weak Governance")) {
      cat("Going to Social Inequality/Weak Governance branch\n")
      
      updateSelectInput(session, "variable_choice", 
                        choices = indicator_choice_list[[input$indicator_category]],
                        selected = indicator_choice_list[[input$indicator_category]][[1]])
    } else {
      cat("Going to else branch (Socio-Ecological Vulnerability)\n")
      
      updateSelectInput(session, "composite_choice", 
                        choices = names(composite_data_options),
                        selected = names(composite_data_options)[1])
      
      first_composite <- names(composite_data_options)[1]
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[first_composite]],
                        selected = composite_data_options[[first_composite]][[1]])
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$composite_choice, {
    if (!app_initialized()) return()
    req(input$indicator_category)
    if (!(input$indicator_category %in% c("Social Inequality", "Weak Governance"))) {
      updateSelectInput(session, "variable_choice", 
                        choices = composite_data_options[[input$composite_choice]],
                        selected = composite_data_options[[input$composite_choice]][[1]])
    }

  }, ignoreInit = TRUE)
  
  observeEvent(input$country_histogram_indicator, {
    if (!app_initialized()) return()
    req(input$country_histogram_indicator)
    for (category in names(indicator_choice_list)) {
      if (input$country_histogram_indicator %in% indicator_choice_list[[category]]) {
        updateSelectInput(session, "indicator_category", selected = category)
        updateSelectInput(session, "variable_choice", selected = input$country_histogram_indicator)
        break
      }
    }
  }, ignoreInit = TRUE)

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
    req(inputs_initialized())
    req(input$variable_choice)
    
    var <- input$variable_choice
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
    
    category <- input$indicator_category %||% "Socio-Ecological Vulnerability"
    legend_title <- if (var %in% composite_arith_list) {
      category
    } else {
      if (!is.null(indicator_choice_list[[category]]) && var %in% indicator_choice_list[[category]]) {
        names(indicator_choice_list[[category]])[indicator_choice_list[[category]] == var]
      } else {
        "Socio-Ecological Vulnerability"
      }
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
      addLegend(pal = pal, values = global_data[[var]][!is.na(global_data[[var]])], 
                opacity = 0.8, title = legend_title, position = "bottomright")
    
    map_initialized(TRUE)
    shinyjs::hide("comparison-maps")
    return(result)
  })
  
  observeEvent({
    input$use_country_specific_scale; input$variable_choice
  }, {
    if (app_initialized()) {
      update_map_layers_only()
    }
  })
  
  observeEvent(input$satellite_view, {
    tiles <- if (!input$satellite_view) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("nd_gain_map") %>% clearTiles() %>% addProviderTiles(tiles)
    # leafletProxy("climate_map") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  observeEvent(input$satellite_view_comparison, {
    tiles <- if (!input$satellite_view_comparison) providers$Esri.WorldImagery else providers$Esri.WorldStreetMap
    
    leafletProxy("map1") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("map2") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("nd_gain_map_1") %>% clearTiles() %>% addProviderTiles(tiles)
    leafletProxy("nd_gain_map_2") %>% clearTiles() %>% addProviderTiles(tiles)
    # leafletProxy("climate_map_1") %>% clearTiles() %>% addProviderTiles(tiles)
    # leafletProxy("climate_map_2") %>% clearTiles() %>% addProviderTiles(tiles)
  })
  
  output$countryDisplay <- renderText({
    country <- selected_country()
    if (is.null(country) || country == "Global (Default)") {
      "Global view - Click on a country to analyze specific data"
    } else {
      paste("Currently analyzing:", country, "- Map automatically zoomed to this country")
    }
  })
  
  output$dynamic_about_section <- renderUI({
    if (is.null(input$variable_choice)) {
      return(NULL)
    }
    
    var <- input$variable_choice
    nd_gain_vars <- unlist(gainVars, use.names = FALSE)
    
    if (var %in% nd_gain_vars) {
      return(NULL)
    }
    
    description <- get_variable_description(var)
    
    if (is.null(description)) {
      return(NULL)
    }
    
    tags$div(
      class = "control-group",
      tags$div(class = "control-title", "About Selected Variable"),
      tags$div(
        style = "font-size: 12px; line-height: 1.4; color: #6c757d;",
        description
      )
    )
  })
  
  get_variable_description <- function(var) {
    descriptions <- list(
      "povmap.grdi.v1.sc" = "Relative Deprivation Index: Assesses multidimensional poverty including limited access to essential services such as healthcare, education, clean water, and sanitation in coastal communities. Higher values indicate greater socioeconomic disadvantage.",
      
      "mean.count.grav.V2.log.sc" = "Degraded Ecosystems: Quantifies the extent of environmental degradation in coastal marine environments due to pollution, overfishing, habitat destruction, and climate change impacts. Higher values reflect more severely compromised ecosystem health.",
      
      "perc.pop.world.coastal.merit.10m.log.sc" = "Coastal Climate Vulnerability: Measures the proportion of population residing in low-elevation coastal zones (below 10 meters) that face heightened exposure to sea level rise, storm surges, and extreme weather events. Higher values indicate greater climate risk exposure.",
      
      "Nutritional.dependence.sc" = "Nutritional Dependence: Evaluates the degree to which coastal communities rely on marine resources for protein intake and nutritional security. Higher values signify greater dependency on ocean-based food systems and increased vulnerability to marine ecosystem disruption.",
      
      "vulnerab.score.rank" = "Socio-Ecological Vulnerability (Composite): Comprehensive ranking that integrates environmental degradation, socioeconomic deprivation, coastal climate risks, and food system dependencies. Higher scores represent greater overall vulnerability to environmental and social stressors.",
      
      "gender.ineq.sc" = "Gender Inequality: Measures disparities between men and women across dimensions of reproductive health, educational attainment, political representation, and economic participation. Higher values indicate more pronounced gender-based inequalities in opportunities and outcomes.",
      
      "income.ineq.sc" = "Income Inequality: Quantifies the distribution of income across population segments, primarily using measures such as the Gini coefficient. Higher values reflect greater concentration of wealth among fewer individuals and wider income disparities.",
      
      "le.ineq.log.sc" = "Health Inequality: Assesses disparities in life expectancy and health outcomes between different socioeconomic groups within a population. Higher values indicate larger health gaps between advantaged and disadvantaged populations.",
      
      "ineq.score.rank" = "Social Inequality (Composite): Aggregate measure combining gender, income, and health inequalities to provide an overall assessment of social stratification. Higher rankings indicate more pervasive inequality across multiple social dimensions.",
      
      "Gov_effect.sc" = "Government Effectiveness: Evaluates the quality of public service delivery, civil service competence, policy formulation and implementation, and institutional capacity. Higher values indicate weaker governmental performance and reduced public sector effectiveness.",
      
      "Reg_quality.sc" = "Regulatory Quality: Assesses the government's capacity to develop and implement sound policies that facilitate private sector development and economic growth. Higher values reflect poor regulatory frameworks that hinder business operations and investment.",
      
      "Rule_law.sc" = "Rule of Law: Measures the extent to which institutional frameworks support contract enforcement, property rights protection, judicial independence, and legal system integrity. Higher values indicate weaker legal institutions and reduced confidence in legal processes.",
      
      "control_corr.sc" = "Control of Corruption: Evaluates the prevalence of corruption in public institutions and the misuse of public authority for private benefit. Higher values indicate more widespread corruption and weaker anti-corruption mechanisms.",
      
      "Voice_account.sc" = "Voice and Accountability: Assesses citizens' capacity to participate in political processes, hold government accountable, and exercise fundamental freedoms including expression and association. Higher values reflect more restricted democratic participation and civic engagement.",
      
      "Political_stab.sc" = "Political Stability: Measures the likelihood of political instability, violence, and unconstitutional government changes including terrorism and civil unrest. Higher values indicate elevated risks of political disruption and governmental instability.",
      
      "gov.score.rank" = "Governance (Composite): Comprehensive assessment integrating all governance dimensions including effectiveness, regulatory quality, rule of law, corruption control, accountability, and political stability. Higher rankings indicate weaker overall governance performance across institutional domains."
    )
    
    description <- descriptions[[var]]
    if (is.null(description)) {
      return(paste("This indicator provides analytical data for:", var, ". Detailed methodology and interpretation guidelines are available in the technical documentation."))
    }
    
    return(description)
  }
  
  output$simple_variable_description <- renderText({
    if (is.null(input$variable_choice)) {
      return("Select a variable to see its description.")
    }
    
    var <- input$variable_choice
    cat("Variable changed to:", var, "\n")
    
    nd_gain_vars <- unlist(gainVars, use.names = FALSE)
    if (var %in% nd_gain_vars) {
      return(NULL)
    }
    
    return(get_variable_description(var))
  })
  
  output$selected_variable_description <- renderText({
    req(input$variable_choice, input$indicator_category)
    
    var <- input$variable_choice
    category <- input$indicator_category
    
    variable_descriptions <- list(
      "vulnerab.score.rank" = "Overall composite ranking that combines environmental degradation, social deprivation, coastal vulnerability, and nutritional dependence into a single vulnerability score. Higher values indicate greater socio-ecological vulnerability in coastal regions.",
      
      "mean.count.grav.V2.log.sc" = "Measures the extent of environmental degradation in coastal marine ecosystems, including coral bleaching, pollution levels, overfishing impacts, and biodiversity loss. Higher values indicate more severely degraded coastal environments that threaten local livelihoods and food security.",
      
      "povmap.grdi.v1.sc" = "Composite measure of poverty and social deprivation including lack of access to healthcare, education, clean water, sanitation, and basic services in coastal communities. Higher values indicate greater social and economic disadvantage among coastal populations.",
      
      "perc.pop.world.coastal.merit.10m.log.sc" = "Percentage of population living in low-elevation coastal zones (under 10 meters above sea level) that are highly vulnerable to sea level rise, storm surges, and extreme weather events. Higher values indicate greater exposure to climate-related coastal hazards.",
      
      "Nutritional.dependence.sc" = "Measures how heavily coastal communities rely on marine resources (fish, seafood, seaweed) for protein and essential nutrients. Higher values indicate greater dependence on ocean-based food sources, making communities more vulnerable to marine ecosystem changes.",
      
      "ineq.score.rank" = "Overall composite ranking combining gender inequality, income distribution disparities, and unequal health outcomes across different population groups. Higher values indicate greater social inequality and disparities within the country.",
      
      "gender.ineq.sc" = "Measures disparities between men and women in reproductive health (maternal mortality), empowerment (education levels, political participation), and economic participation (labor force participation, wage gaps). Higher values indicate greater gender-based inequalities.",
      
      "income.ineq.sc" = "Based on the Gini coefficient and other measures of how income and wealth are distributed within a population. Higher values indicate greater economic inequality, where income is concentrated among fewer people while many have significantly less.",
      
      "le.ineq.log.sc" = "Life expectancy adjusted for inequality in the distribution of expected years of life across different population groups (by income, education, location). Higher values indicate greater health disparities and unequal access to healthcare and healthy living conditions.",
      
      "gov.score.rank" = "Overall composite ranking from the World Bank Worldwide Governance Indicators, combining government effectiveness, regulatory quality, rule of law, corruption control, voice & accountability, and political stability. Higher values indicate weaker governance institutions.",
      
      "Gov_effect.sc" = "Measures the quality of public services, civil service competence and independence, policy formulation and implementation quality, and government commitment to policies. Higher values indicate less effective government institutions and public service delivery.",
      
      "Reg_quality.sc" = "Assesses the government's ability to formulate and implement sound policies and regulations that promote private sector development, business creation, and economic growth. Higher values indicate poorer regulatory frameworks and business environments.",
      
      "Rule_law.sc" = "Measures confidence in and compliance with societal rules, including contract enforcement, property rights protection, police effectiveness, and court quality. Higher values indicate weaker legal institutions and law enforcement.",
      
      "control_corr.sc" = "Captures the extent to which public power is exercised for private gain, including both petty and grand forms of corruption, as well as state capture by elites and private interests. Higher values indicate higher levels of corruption.",
      
      "Voice_account.sc" = "Measures citizens' ability to participate in selecting their government, freedom of expression, freedom of association, and free media access. Higher values indicate more restricted political rights and civil liberties.",
      
      "Political_stab.sc" = "Assesses the likelihood of political instability and/or politically motivated violence, including terrorism, coups, and civil unrest. Higher values indicate greater political instability and violence risk."
    )
    
    description <- variable_descriptions[[var]]
    
    if (is.null(description)) {
      if (var %in% names(gainVars)) {
        paste("ND-GAIN indicator:", names(gainVars)[gainVars == var], ". This measures climate vulnerability and adaptation readiness. Refer to the ND-GAIN documentation for detailed methodology.")
      } else {
        paste("Climate risk indicator showing", gsub("_", " ", var), ". This variable represents projected climate changes based on IPCC models and current monitoring data.")
      }
    } else {
      description
    }
  })
}
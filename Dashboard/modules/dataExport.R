observeEvent({
  #input$comparison_country_search_map_2; input$indicator_category_map_2; input$variable_choice_map_2  # Fixed this line
  input$country_search_data_export; input$indicator_category_data_export; input$variable_choice_data_export  # Fixed this line
}, {
  req(input$indicator_category_data_export)
  req(input$variable_choice_data_export)

  selected_country(input$country_search_data_export)  # And this line
  
  #get_selected_export_data()
})

# Data type selector (second level dropdown)
output$data_type_selector_data_export <- renderUI({
  req(input$variable_choice_data_export)
  choices <- names(climate_data_options[[input$variable_choice_data_export]])
  selectInput("variable_choice_data_export", "Select Data Type:", choices = choices)
})

# Time period selector (third level dropdown)
output$time_period_selector_data_export <- renderUI({
  req(input$climate_variable_data_export, input$data_type_selector_data_export)
  choices <- names(climate_data_options[[input$climate_variable_data_export]][[input$data_type_selector_data_export]])
  selectInput("time_period_selector_data_export", "Select Time Range:", choices = choices)
})

# VARIABLE SELECTION
observeEvent(input$indicator_category_data_export, {
  updateSelectInput(session, "variable_choice_data_export", choices = indicator_choice_list[[input$indicator_category_data_export]])
})


data_to_export <- reactiveVal(average_country_nogeo)

# Export the data
observe( {
  print("inside")
  country <- input$country_search_data_export
  unused_inequity_columns <- c("Economic.dependence.sc","income.ineq.change.sc","le.ineq.change.sc")

  if (is.null(country) || country == "" || country == "Global (Default)") {
    filtered_data <- average_country_nogeo
  }
  else
  {
    filtered_data <- df %>% filter(COUNTRY == country)
  }


  # Pick Columns
  if (nrow(filtered_data) > 0) {

    if(input$indicator_category_data_export == "All Themes"){
      data_to_export(filtered_data)
    }
    else if(input$indicator_category_data_export == "Socio-Ecological Vulnerability" && input$composite_choice_data_export == "Inequity"){
      data_to_export(filtered_data[ , !(names(filtered_data) %in% c(unname(indicator_choice_list[["Social Inequality"]]), unname(indicator_choice_list[["Weak Governance"]]), unused_inequity_columns))])
    }
    else if(input$indicator_category_data_export == "Social Inequality"){
      data_to_export(filtered_data[ , !(names(filtered_data) %in% c(unname(indicator_choice_list[["Socio-Ecological Vulnerability"]]), unname(indicator_choice_list[["Weak Governance"]]), unused_inequity_columns))])
    }
    else if(input$indicator_category_data_export == "Weak Governance"){
      data_to_export(filtered_data[ , !(names(filtered_data) %in% c(unname(indicator_choice_list[["Social Inequality"]]), unname(indicator_choice_list[["Socio-Ecological Vulnerability"]]), unused_inequity_columns))])
    }
  }

  # if(!is.null(data_to_export)){
  #   save_path <- tclvalue(tkgetSaveFile(
  #     filetypes = "{{CSV Files} {.csv}} {{All files} *}",
  #     defaultextension = ".csv"
  #   ))
  #
  #   if (nzchar(save_path)) {
  #     write.csv(data_to_export, save_path, row.names = FALSE)
  #   }
  # }

  
})


# Download handlers
output$export_data_handler <- downloadHandler(
  filename = function() {
    paste('Data-', Sys.time(), '.csv', sep='')
  },
  content = function(con) {
      #if (nzchar(con)) {
        write.csv(data_to_export(), con, row.names = FALSE)
      #}
  }
)






# Get the data to export
get_selected_export_data <- function() {
  
  var <- input$variable_choice_data_export
  country <- selected_country()
  
  if (var %in% composite_arith_list) {
    global_data <- combined_scores_global
    polygon_data <- combined_scores_global_polygons
  } else {
    global_data <- average_country_nogeo
    polygon_data <- average_country_polygons
  }
  
  # Create legend title based on whether composite score is selected
  legend_title <- if (var %in% composite_arith_list) {
    paste(input$indicator_category_data_export)
  } else {
    # Find the variable name for display - show ONLY the variable name
    var_display_name <- names(indicator_choice_list[[input$indicator_category_data_export]])[
      indicator_choice_list[[input$indicator_category_data_export]] == var
    ]
    var_display_name
  }
  
  if (is.null(country) || country == "" || country == "Global (Default)") {
    data_to_export <<- polygon_data

    # Global view - always show polygons
    # leafletProxy("map2") %>%
    #   addPolygons(
    #     data = polygon_data,
    #     fillColor = ~pal(get(var)), fillOpacity = 0.7, color = ~pal(get(var)),
    #     weight = 2, opacity = 0.9,
    #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1, fillOpacity = 0.8),
    #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3))), group = "polygons"
    #   ) %>%
    #   addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
    #             title = legend_title, position = "bottomright")
  } else {
    # Country-specific view
    nd_gain_vars <- unlist(gainVars, use.names = FALSE)
    climate_vars <- unlist(climate_data_options, recursive = TRUE, use.names = FALSE)
    is_special_module_var <- (var %in% nd_gain_vars) || (var %in% climate_vars)
    
    if (is_special_module_var) {
      # Special module variables - use polygon rendering
      selected_country_data <- polygon_data %>% filter(COUNTRY == country)
      other_countries_data <- polygon_data %>% filter(COUNTRY != country)
      
      
      
      
      
      data_to_export <<- selected_country_data
      
      
      
      
      if (nrow(other_countries_data) > 0) {
        # leafletProxy("map2") %>%
        #   addPolygons(
        #     data = other_countries_data,
        #     fillColor = "transparent", fillOpacity = 0, 
        #     color = ~pal(get(var)), weight = 2, opacity = 0.5,
        #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
        #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
        #   )
      }
      
      if (nrow(selected_country_data) > 0) {
        # leafletProxy("map2") %>%
        #   addPolygons(
        #     data = selected_country_data,
        #     fillColor = ~pal(get(var)), fillOpacity = 0.8,
        #     color = ~pal(get(var)), weight = 3, opacity = 1,
        #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
        #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
        #   )
      }
      
      # leafletProxy("map2") %>%
      #   addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
      #             title = paste0("<b>", country, "</b><br>", legend_title),
      #             position = "bottomright")
    } else {
      # Check if variable exists in country-level data (df) - if so, show points
      country_data <- df %>% filter(COUNTRY == country)
      if (nrow(country_data) > 0 && var %in% names(country_data)) {
        
        
        
        data_to_export <<- country_data
        print("here")
        if(input$indicator_category_data_export == "Socio-Ecological Vulnerability"){
          print(c(unname(indicator_choice_list[["Social Inequality"]]), unname(indicator_choice_list[["Weak Governance"]])))
          print("*****************************")
          print(!(names(data_to_export) %in% c(unname(indicator_choice_list[["Social Inequality"]]), unname(indicator_choice_list[["Weak Governance"]]))))
          data_to_export <- data_to_export[ , !(names(data_to_export) %in% c(unname(indicator_choice_list[["Social Inequality"]]), unname(indicator_choice_list[["Weak Governance"]])))]
        }
        else if(input$indicator_category_data_export == ""){
          
        }
        else if(input$indicator_category_data_export == ""){
          
        }
        
        
        
        
        
        # Show points for variables that exist in df
        #use_local <- isTRUE(input$use_country_specific_scale)
        #domain_data <- if (use_local) country_data[[var]] else average_country_nogeo[[var]]
        
        # leafletProxy("map2") %>%
        #   # Add background polygons for all countries (for clicking)
        #   addPolygons(
        #     data = polygon_data,
        #     fillColor = "transparent", fillOpacity = 0, color = ~border_pal(get(var)),
        #     weight = 1, opacity = 0.4,
        #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 3, bringToFront = TRUE, opacity = 1),
        #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
        #   ) %>%
        #   # Add points on top
        #   addCircleMarkers(
        #     data = country_data, radius = 6, fillColor = ~pal_country(get(var)), fillOpacity = 0.9,
        #     stroke = FALSE,
        #     label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
        #   ) %>%
        #   addLegend(pal = pal_country, values = domain_data, opacity = 0.9,
        #             title = paste0("<b>", country, "</b><br>", legend_title, "<br><i>(", if (use_local) "Local" else "Global", ")</i>"),
        #             position = "bottomright")
      } else {
        # Show highlighted country polygon for other variables
        selected_country_data <- polygon_data %>% filter(COUNTRY == country)
        other_countries_data <- polygon_data %>% filter(COUNTRY != country)
        
        if (nrow(other_countries_data) > 0) {
          # leafletProxy("map2") %>%
          #   addPolygons(
          #     data = other_countries_data,
          #     fillColor = "transparent", fillOpacity = 0, 
          #     color = ~pal(get(var)), weight = 2, opacity = 0.5,
          #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 4, bringToFront = TRUE, opacity = 1),
          #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          #   )
        }
        
        if (nrow(selected_country_data) > 0) {
          # leafletProxy("map2") %>%
          #   addPolygons(
          #     data = selected_country_data,
          #     fillColor = ~pal(get(var)), fillOpacity = 0.8,
          #     color = ~pal(get(var)), weight = 3, opacity = 1,
          #     highlightOptions = highlightOptions(color = "#FFFFFF", weight = 5, bringToFront = TRUE, opacity = 1, fillOpacity = 0.9),
          #     layerId = ~COUNTRY, label = ~paste0(COUNTRY, ": ", ifelse(is.na(get(var)), "No data", round(get(var), 3)))
          #   )
        }
        
        # leafletProxy("map2") %>%
        #   addLegend(pal = pal, values = global_data[[var]], opacity = 0.8,
        #             title = paste0("<b>", country, "</b><br>", legend_title),
        #             position = "bottomright")
      }
    }
  }
  
}


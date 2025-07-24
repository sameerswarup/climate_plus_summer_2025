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

outputOptions(output, "export_data_handler", suspendWhenHidden = FALSE)

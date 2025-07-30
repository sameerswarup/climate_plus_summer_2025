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

  country <- input$country_search_data_export
  
  # Gather data for ND GAIN
  if(input$indicator_category_data_export == "Socio-Ecological Vulnerability" && input$composite_choice_data_export == "ND GAIN")
  {
    if (is.null(country) || country == "" || country == "Global (Default)") {
      filtered_data <- gain
    }
    else
    {
      filtered_data <- gain %>% filter(Name == country)
    }
    
    # Filter Year
    if(input$variable_nd_data_export == "Specific Year"){
      filtered_data <- filtered_data %>% filter(Year == input$nd_year_data_export)
    }
    
    data_to_export(filtered_data)
  }
  
  
  else # Gather data for Inequity
  {
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


# Allow user to choose a folder
roots <- c(Home = normalizePath("~"))
shinyDirChoose(input, "folder", roots = roots)

# Reactive folder path
folder_path <- reactive({
  req(input$folder)
  parseDirPath(roots, input$folder)
})

output$folder_path <- renderPrint({
  folder_path()
})

observeEvent(input$folder, {#input$export_btn


  # Try to parse the selected folder path safely
  dest_dir <- tryCatch({
    path <- folder_path()
    if (length(path) == 0 || is.null(path) || is.na(path)) return(NULL)
    normalizePath(path, mustWork = FALSE)
  }, error = function(e) NULL)

  # Choose Source Path
  if (input$climate_variable_data_export == "Ocean pH"){
    source_dir <- "data/IPCC_data/"
    files_pattern <- "^pH"
  }
  else if (input$climate_variable_data_export == "Coral Bleaching Heat"){
    source_dir <- "data/Coral_Bleaching_data/"
    files_pattern <- "^ct"
  }
  else if (input$climate_variable_data_export == "Sea Level Rise"){
    source_dir <- "data/IPCC_data/"
    files_pattern <- "^SLR"
  }
  else if (input$climate_variable_data_export == "Heating Degree Days"){
    source_dir <- "data/IPCC_data/"
    files_pattern <- "^DH"
  }
  
  # List files
  files_to_copy <- list.files(source_dir, pattern = files_pattern, full.names = TRUE)

  if (length(files_to_copy) > 0){
    
    # Copy each file to the selected folder
    for (file in files_to_copy) {
      file.copy(file, file.path(dest_dir, basename(file)), overwrite = TRUE)
    }
    
    showNotification(paste(length(files_to_copy), "files exported to", dest_dir))
  }
})


outputOptions(output, "export_data_handler", suspendWhenHidden = FALSE)

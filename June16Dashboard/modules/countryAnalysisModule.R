# COUNTRY ANALYSIS MODULE

CA_country_search <- reactiveVal(NULL)

observeEvent(input$country_search_graphs, {
  CA_country_search(input$country_search_graphs)
})

output$CA_country <- renderText({
  text <- CA_country_search()
  return(paste0("Target Country: ", text)
)
})

# Plotting functions
create_scatter_plot <- function(data, x_col, y_col, choices, title) {
  if (is.null(data) || !(x_col %in% names(data)) || !(y_col %in% names(data))) return()
  if (all(is.na(data[[x_col]])) || all(is.na(data[[y_col]]))) return()
  

  subtitle <- paste0(names(choices)[choices == x_col], " vs. ", names(choices)[choices == y_col])
  
  if (title == "Global") {
    data$Highlight <- ifelse(data$COUNTRY == CA_country_search(), "Target Country", "Other")
    
    ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = Highlight, size = Highlight)) +
      geom_point() +
      scale_color_manual(values = c("Target Country" = "black", "Other" = "#A9A9A9")) +
      scale_size_manual(values = c("Target Country" = 3, "Other" = 2)) +
      labs(title = title, subtitle = subtitle,
           x = names(choices)[choices == x_col],
           y = names(choices)[choices == y_col]) +
      theme_hc() +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10, l = 10))
      )
  } else {
    ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point() +
      labs(title = title, subtitle = subtitle,
           x = names(choices)[choices == x_col],
           y = names(choices)[choices == y_col]) +
      theme_hc() +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10, l = 10))
      )
  }

  
}

calculate_correlation <- function(data, x_col, y_col) {
  
  if (identical(data, average_country_nogeo)) {
    gainVars_values <- unlist(gainVars, use.names = FALSE)
    
    first_is_gain <- input$first_indicator_global %in% gainVars_values
    second_is_gain <- input$second_indicator_global %in% gainVars_values
    
    # If either indicator is ND Gain, join in corresponding data (if available)
    if (first_is_gain) {
      gain_data1 <- ca_nd_year_data1()
      if (!is.null(gain_data1)) {
        data <- left_join(data, gain_data1, by = c("iso_a3" = "ISO3"))
      }
    }
    
    if (second_is_gain) {
      gain_data2 <- ca_nd_year_data2()
      if (!is.null(gain_data2)) {
        data <- left_join(data, gain_data2, by = c("iso_a3" = "ISO3"))
      }
    }
  }
  
  if (is.null(data) || nrow(data) == 0) return("No data available")
  if (!(x_col %in% names(data)) || !(y_col %in% names(data))) return("Selected variables not available")
  
  x_data <- data[[x_col]]
  y_data <- data[[y_col]]
  
  if (!is.numeric(x_data) || !is.numeric(y_data)) return("Selected variables are not numeric")
  if (sum(complete.cases(x_data, y_data)) < 2) return("Insufficient data for correlation analysis")
  
  cor_result <- tryCatch(cor(x_data, y_data, use = "complete.obs"), error = function(e) NA)
  spr_cor_result <- tryCatch(cor(x_data, y_data, method = "spearman", use = "complete.obs"), error = function(e) NA)
  
  if (is.na(cor_result) || is.na(spr_cor_result)) return("Could not calculate correlation")
  
  paste("Pearson Coefficient (r) =", round(cor_result, 4),
        "\nSpearman Coefficient (rho) =", round(spr_cor_result, 4))
}

# Global analysis modal
observeEvent(input$global_scale_button, {
  showModal(modalDialog(
    title = "Global Scale Analysis",
    size = "l",
    tags$div(
      style = "text-align: center",
      tags$h4(
        textOutput("CA_country")
      )
    ),
    fluidRow(
      column(6, selectInput("first_indicator_global", "First indicator:",
                            choices = global_level_choices, selected = "Gov_effect.sc"),
             sliderInput(inputId = "ca_nd_year1",
                         label = "Choose a year:",
                         min = 1995,
                         max = 2022,
                         value = 1995,
                         sep = "",
                         animate = TRUE),),
      column(6, selectInput("second_indicator_global", "Second indicator:",
                            choices = global_level_choices, selected = "le.ineq.log.sc"),
             sliderInput(inputId = "ca_nd_year2",
                         label = "Choose a year:",
                         min = 1995,
                         max = 2022,
                         value = 1995,
                         sep = "",
                         animate = TRUE),)
    ),
    
    plotOutput("global_custom_scatter", height = "400px"),
    verbatimTextOutput("global_correlation"),
    tags$div(
      style = "text-align: center;",
      downloadButton("downloadGlobalCustomScatter", "Download Plot")
    ),
    tags$br(),
    tags$small(style = "font-style: italic",
               "Here will go some analysis of like where the selected country lies ykyk"),
    footer = modalButton("Close")
  ))
})

# Reactive plot functions
REAcustom_scatter <- reactive({
  if (is.null(selected_country())) {
    return(NULL)
  }
  country_choices <- c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                       "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                       "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
  create_scatter_plot(country_dataset(), input$first_indicator, input$second_indicator, country_choices, selected_country())
})

REAglobal_custom_scatter <- reactive({
  req(input$first_indicator_global, input$second_indicator_global)
  
  gainVars_values <- unlist(gainVars, use.names = FALSE)
  
  first_is_gain <- input$first_indicator_global %in% gainVars_values
  second_is_gain <- input$second_indicator_global %in% gainVars_values
  
  # Start from a base dataset
  data <- average_country_nogeo
  
  # If either indicator is ND Gain, join in corresponding data (if available)
  if (first_is_gain) {
    gain_data1 <- ca_nd_year_data1()
    if (!is.null(gain_data1)) {
      data <- left_join(data, gain_data1, by = c("iso_a3" = "ISO3"))
    }
  }
  
  if (second_is_gain) {
    gain_data2 <- ca_nd_year_data2()
    if (!is.null(gain_data2)) {
      data <- left_join(data, gain_data2, by = c("iso_a3" = "ISO3"))
    }
  }
  
  # If neither is ND Gain, data remains just average_country_nogeo
  # Then create the plot with selected indicators
  create_scatter_plot(
    data,
    input$first_indicator_global,
    input$second_indicator_global,
    global_level_name_key_value,
    "Global"
  )
})

output$global_custom_scatter <- renderPlot({
  req(REAglobal_custom_scatter)
  REAglobal_custom_scatter()
})

output$correlation <- renderText({
  calculate_correlation(country_dataset(), input$first_indicator, input$second_indicator)
})

output$global_correlation <- renderText({
  calculate_correlation(average_country_nogeo, input$first_indicator_global, input$second_indicator_global)
})



REArenderHistogram <- reactive({
  if (is.null(selected_country())) {
    return(NULL)
  }
  
  data <- country_dataset()
  if (is.null(data) || nrow(data) == 0) return() 
  
  chi <- input$country_histogram_indicator
  if (!(chi %in% names(data))) return()
  
  col <- data[[chi]][!is.na(data[[chi]])]
  if (length(col) <= 1 || !is.numeric(col)) return()
  
  country_choices <- c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                       "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                       "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc")
  
  label <- names(country_choices)[country_choices == chi]
  
  ggplot(data, aes(x = .data[[chi]])) +
    geom_histogram(bins = 30, fill = "#00539B", color = "white") +
    labs(title = paste0(label, " for ", selected_country()),
         subtitle = paste0("Each Point Is a Point Within ", selected_country()),
         x = label, y = "Frequency") +
    theme_hc() + 
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10))
    ) 
})

# Plot outputs
output$custom_scatter <- renderPlot({
  req(REAcustom_scatter())
  REAcustom_scatter()
})

output$custom_scatter_zoom <- renderPlot({
  req(REAcustom_scatter())
  REAcustom_scatter()
})

output$global_custom_scatter <- renderPlot({
  req(REAglobal_custom_scatter())
  REAglobal_custom_scatter()
})

renderHistogram <- renderPlot({
  req(REArenderHistogram())
  REArenderHistogram()
})

output$country_histogram_zoom <- renderHistogram
output$country_histogram <- renderHistogram

output$correlation <- renderText({
  calculate_correlation(country_dataset(), input$first_indicator, input$second_indicator)
})

output$global_correlation <- renderText({
  calculate_correlation(average_country_nogeo, input$first_indicator_global, input$second_indicator_global)
})

# Modal dialogs for zoomed plots
observeEvent(input$scatter_zoom, {
  showModal(modalDialog(
    title = "Country Scale Scatterplot",
    size = "l",
    plotOutput("custom_scatter_zoom", height = "400px"),
    tags$br(),
    
    tags$div(style = "text-align: center;",
             downloadButton("downloadCustomScatter", "Download Plot")
             
    ),      
    tags$br(),
    verbatimTextOutput("correlation"),
    footer = modalButton("Close")
  ))
})

observeEvent(input$histogram_zoom, {
  showModal(modalDialog(
    title = "Country Scale Histogram",
    size = "l",
    plotOutput("country_histogram_zoom", height = "400px"),
    tags$br(),
    
    tags$div(style = "text-align: center;",
             downloadButton("downloadHistogram", "Download Plot")
             
    ),
    tags$br(),
    textOutput("country_histogram_description_zoom"),
    footer = modalButton("Close")
  ))
})

# Download handlers
output$downloadCustomScatter <- downloadHandler(
  filename = function() {
    paste('plot-', Sys.time(), '.png', sep='')
  },
  content = function(con) {
    ggsave(
      filename = con,
      plot = REAcustom_scatter(),
      device = "png",
      width = 14,
      height = 6
    )
    
  }
)

output$downloadGlobalCustomScatter <- downloadHandler(
  filename = function() {
    paste('plot-', Sys.time(), '.png', sep='')
  },
  content = function(con) {
    ggsave(
      filename = con,
      plot = REAglobal_custom_scatter(),
      device = "png",
      width = 14,
      height = 6
    )
  },
  contentType = "image/png"
  
)

output$downloadHistogram <- downloadHandler(
  filename = function() {
    paste('plot-', Sys.Date(), '.png', sep='')
  },
  content = function(con) {
    ggsave(
      filename = con,
      plot = REArenderHistogram(),
      device = "png",
      width = 14,
      height = 6
    )
  },
  contentType = "image/png"
)



# Descriptions of indicators
clicked_scores <- list(
  first_global = reactiveVal(NULL),
  second_global = reactiveVal(NULL),
  first_country = reactiveVal(NULL),
  second_country = reactiveVal(NULL)
)
clicked_score_country_histogram = reactiveVal(NULL)

observe({
  clicked_scores$first_country(input$first_indicator)
  clicked_scores$second_country(input$second_indicator)
  clicked_score_country_histogram(input$country_histogram_indicator)
})

observe_map <- list(
  first_indicator_global = "first_global",
  second_indicator_global = "second_global",
  first_indicator = "first_country",
  second_indicator = "second_country"
)

lapply(names(observe_map), function(id) {
  observeEvent(input[[id]], {
    clicked_scores[[observe_map[[id]]]](input[[id]])
  })
})

description_output <- function(score_reactive) {
  renderText({
    req(score_reactive())
    inequity_data_descriptions %>%
      filter(variable_name == score_reactive()) %>%
      pull(description)
  })
}

output$first_indicator_country_description <- description_output(clicked_scores$first_country)
output$second_indicator_country_description <- description_output(clicked_scores$second_country)
output$first_indicator_global_description <- description_output(clicked_scores$first_global)
output$second_indicator_global_description <- description_output(clicked_scores$second_global)
output$country_histogram_description <- description_output(clicked_score_country_histogram)
output$country_histogram_description_zoom <- description_output(clicked_score_country_histogram)


observeEvent(input$country_histogram_indicator, {
  clicked_score_country_histogram(input$country_histogram_indicator)
})

outputOptions(output, "country_histogram", suspendWhenHidden = FALSE)
outputOptions(output, "custom_scatter", suspendWhenHidden = FALSE)
outputOptions(output, "first_indicator_country_description", suspendWhenHidden = FALSE)
outputOptions(output, "second_indicator_country_description", suspendWhenHidden = FALSE)
outputOptions(output, "country_histogram_description", suspendWhenHidden = FALSE)

output$ca_nd_gain_slider <- renderUI({
  
  first <- input$first_indicator_global
  second <- input$second_indicator_global
  
  
})

observeEvent(c(input$first_indicator_global,
               input$second_indicator_global,
               input$ca_nd_year2,
               input$ca_nd_year1), {
                 
                 req(input$ca_nd_year1)
                 req(input$ca_nd_year2)
                 
                 gainVars_values <- unlist(gainVars, use.names = FALSE)
                 
                 if (input$first_indicator_global %in% gainVars_values) {
                   data1 <- gain %>%
                     select(ISO3, Name, Year, input$first_indicator_global) %>%
                     filter(Year == input$ca_nd_year1)
                   
                   ca_nd_year_data1(data1)
                 } else {
                   ca_nd_year_data1(NULL)
                 }
                 
                 if (input$second_indicator_global %in% gainVars_values) {
                   data2 <- gain %>%
                     select(ISO3, Name, Year, input$second_indicator_global) %>%
                     filter(Year == input$ca_nd_year2)
                   
                   ca_nd_year_data2(data2)
                 } else {
                   ca_nd_year_data2(NULL)
                 }
               })


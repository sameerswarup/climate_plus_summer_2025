library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

ui <- page_sidebar(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  theme = bs_theme(bootswatch = "flatly"),
  # input_dark_mode(id = "light"), 
  
  # Initialize shinyjs
  useShinyjs(), 
  
  sidebar = sidebar(
    width = 400,
    
    # Better spacing and styling for header
    tags$div(
      style = "margin-bottom: 20px;",
      # imageOutput("dataplus_logo"),
      tags$h3("Composite Scores Map", style = "color: var(--bs-primary, #003087); font-weight: bold; margin:0px;")
    ),
    
    # Map controls - only show on Interactive Map tab
    conditionalPanel(
      condition = "input.tabset == 'Interactive Map'",
      
      # Professional search bar section
      card(
        card_header(
          tags$div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            tags$span("Country Search", style = "font-weight: 600;"),
            tags$i(class = "fas fa-search", style = "color: #6c757d;")
          )
        ),
        tags$div(
          style = "padding: 15px 10px;",
          
          # True search input with autocomplete suggestions
          tags$div(
            style = "position: relative; margin-bottom: 15px;",
            tags$div(
              style = "position: absolute; left: 12px; top: 50%; transform: translateY(-50%); z-index: 10; color: #6c757d;",
              tags$i(class = "fas fa-globe", style = "font-size: 14px;")
            ),
            textInput(
              "country_search", 
              label = NULL,
              value = "",
              placeholder = "Search for a country...",
              width = "100%"
            ),
            # Custom suggestion dropdown
            tags$div(
              id = "country_suggestions",
              style = "position: absolute; top: 100%; left: 0; right: 0; background: white; 
                     border: 1px solid #dee2e6; border-top: none; border-radius: 0 0 8px 8px; 
                     box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); z-index: 1000; display: none; 
                     max-height: 200px; overflow-y: auto;",
              # Suggestions will be populated by JavaScript
            ),
            tags$style(HTML("
              #country_search {
                padding-left: 35px !important;
                border: 2px solid #e9ecef;
                border-radius: 8px;
                font-size: 14px;
                background: #fff;
                transition: all 0.2s ease;
              }
              #country_search:focus {
                border-color: #0d6efd;
                box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);
                outline: none;
              }
              .country-suggestion {
                padding: 8px 12px;
                border-bottom: 1px solid #f0f0f0;
                cursor: pointer;
                font-size: 14px;
                transition: background-color 0.2s ease;
              }
              .country-suggestion:hover {
                background-color: #f8f9fa;
              }
              .country-suggestion:last-child {
                border-bottom: none;
              }
              .country-suggestion.highlighted {
                background-color: #e3f2fd;
              }
            "))
          ),
          
          # Action buttons row
          tags$div(
            style = "display: flex; gap: 8px;",
            
            # Global view button
            actionButton(
              "global_view_button", 
              tags$div(
                style = "display: flex; align-items: center; gap: 6px;",
                tags$i(class = "fas fa-globe-americas", style = "font-size: 12px;"),
                "Global View"
              ),
              style = "flex: 1; background: linear-gradient(135deg, #6c757d, #5a6268); 
                     color: white; border: none; border-radius: 6px; padding: 8px 12px; 
                     font-size: 13px; font-weight: 500; transition: all 0.2s ease;",
              onclick = "this.style.transform='scale(0.98)'; setTimeout(() => this.style.transform='scale(1)', 100);"
            ),
            
            # Zoom to country button  
            actionButton(
              "zoom_button", 
              tags$div(
                style = "display: flex; align-items: center; gap: 6px;",
                tags$i(class = "fas fa-search-location", style = "font-size: 12px;"),
                "Zoom In"
              ),
              style = "flex: 1; background: linear-gradient(135deg, #0d6efd, #0b5ed7); 
                     color: white; border: none; border-radius: 6px; padding: 8px 12px; 
                     font-size: 13px; font-weight: 500; transition: all 0.2s ease;",
              onclick = "this.style.transform='scale(0.98)'; setTimeout(() => this.style.transform='scale(1)', 100);"
            )
          ),
          
          # Current selection indicator
          tags$div(
            id = "current_selection_indicator",
            style = "margin-top: 12px; padding: 8px 12px; background: #f8f9fa; 
                   border-radius: 6px; border-left: 4px solid #0d6efd; display: none;",
            tags$small(
              style = "color: #495057; font-weight: 500;",
              tags$span("Selected: "),
              tags$span(id = "selected_country_name", style = "font-weight: 600; color: #0d6efd;")
            )
          )
        )
      ),
      
      # Map controls card
      card(
        card_header("Map Controls"),
        selectInput("indicator_category", "Choose Composite Score:", 
                    choices = composite_choices, selected = "Weak Governance"),
        
        selectInput("variable_choice", "Choose a Variable:", choices = NULL),
        
        # Enhanced checkbox with better styling
        tags$div(
          style = "margin-top: 15px; padding: 12px; background: #f8f9fa; border-radius: 6px;",
          checkboxInput(
            inputId = "use_country_specific_scale",
            label = tags$span(
              style = "font-weight: 500; color: #495057;",
              "Use country-specific color scale"
            ),
            value = FALSE
          )
        ),
        
        # Satellite view toggle with better styling
        tags$div(
          style = "margin-top: 15px; padding: 12px; background: #f8f9fa; border-radius: 6px;",
          checkboxInput(
            "satellite_view", 
            label = tags$span(
              style = "font-weight: 500; color: #495057;",
              "Satellite View"
            ), 
            value = FALSE
          )
        )
      ),
      
      # Composite Score descriptions card
      card(
        card_header("Composite Score Descriptions"),
        tags$div(
          style = "padding: 10px;",
          
          tags$div(
            style = "margin-bottom: 15px; padding: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); 
                   border-radius: 8px; border-left: 4px solid #6f42c1;",
            tags$h6("Weak Governance", style = "font-weight: bold; margin-bottom: 8px; color: #2c3e50;"),
            tags$p("Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation in decision-making.", 
                   style = "margin: 0; font-size: 12px; line-height: 1.5; color: #495057;")
          ),
          
          tags$div(
            style = "margin-bottom: 15px; padding: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); 
                   border-radius: 8px; border-left: 4px solid #dc3545;",
            tags$h6("Social Inequality", style = "font-weight: bold; margin-bottom: 8px; color: #2c3e50;"),
            tags$p("Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups.", 
                   style = "margin: 0; font-size: 12px; line-height: 1.5; color: #495057;")
          ),
          
          tags$div(
            style = "margin-bottom: 15px; padding: 12px; background: linear-gradient(135deg, #f8f9fa, #e9ecef); 
                   border-radius: 8px; border-left: 4px solid #198754;",
            tags$h6("Socio-Ecological Vulnerability", style = "font-weight: bold; margin-bottom: 8px; color: #2c3e50;"),
            tags$p("Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels.", 
                   style = "margin: 0; font-size: 12px; line-height: 1.5; color: #495057;")
          )
        )
      )
    ),
    
    # Custom graph controls - only show on Custom Graphs tab
    conditionalPanel(
      condition = "input.tabset == 'Custom Graphs'",
      card(
        card_header(
          tags$h3("Make Your Own CUSTOM GRAPH!", 
                  style = "color: var(--bs-primary, #003087); font-weight: bold; margin: 0;")
        ),
        
        # Better spacing for explanatory text
        tags$div(
          style = "margin-bottom: 15px; padding: 12px; background: #f8f9fa; border-radius: 6px;",
          tags$h6("Global: Each data point represents the average score of a country.", 
                  style = "font-style: italic; margin-bottom: 5px; color: #495057;"),
          tags$h6("Country: Each data point represents the score of a district within a chosen country.", 
                  style = "font-style: italic; margin-bottom: 0; color: #495057;")
        ),
        
        # Analysis level selection
        selectInput("global_or_country", "Select level of investigation:",
                    choices = c("Global" = "global", "Country" = "country"),
                    selected = "Global"),
        
        # Dynamic components
        uiOutput("global_or_country_components")
      )
    ),
    
    # Map controls - only show on Country Comparison Map tab
    conditionalPanel(
      tags$style(HTML("
         #map_1_country_search + .selectize-control .selectize-dropdown,
         #map_2_country_search + .selectize-control .selectize-dropdown {
          bottom: 100% !important;
          top: auto !important;
        }
      ")),
      condition = "input.tabset == 'Country Comparison'",
      
      # Enhanced Country Search for Comparison Maps
      card(
        card_header(
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$i(class = "fas fa-balance-scale", style = "color: #0d6efd;"),
            "Country Comparison Search"
          )
        ),
        tags$div(
          style = "padding: 15px 10px;",
          tags$div(
            style = "position: relative; margin-bottom: 10px;",
            tags$div(
              style = "position: absolute; left: 12px; top: 50%; transform: translateY(-50%); z-index: 10; color: #6c757d;",
              tags$i(class = "fas fa-search", style = "font-size: 14px;")
            ),
            selectizeInput("comparison_country_search", 
                           label = NULL,
                           choices = NULL, 
                           selected = NULL,
                           options = list(
                             placeholder = "Search for a country to compare...",
                             maxItems = 1,
                             create = FALSE
                           )),
            tags$style(HTML("
              #comparison_country_search + .selectize-control .selectize-input {
                padding-left: 35px !important;
                border: 2px solid #e9ecef;
                border-radius: 8px;
                font-size: 14px;
              }
              #comparison_country_search + .selectize-control .selectize-input:focus-within {
                border-color: #0d6efd;
                box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);
              }
            "))
          ),
          tags$p("Select a country above to display it on both comparison maps", 
                 style = "font-size: 12px; color: #6c757d; margin: 0; text-align: center;")
        )
      ),
      
      # Enhanced checkbox for comparison scale
      tags$div(
        style = "margin-bottom: 15px; padding: 12px; background: #f8f9fa; border-radius: 6px;",
        checkboxInput(
          inputId = "use_comparison_country_scale",
          label = tags$span(
            style = "font-weight: 500; color: #495057;",
            "Use country color scale"
          ),
          value = FALSE
        )
      ),
      
      card(
        card_header(
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$span("Map One Controls", style = "font-weight: 600;"),
            tags$span("1", style = "background: #0d6efd; color: white; border-radius: 50%; 
                     width: 20px; height: 20px; display: flex; align-items: center; 
                     justify-content: center; font-size: 12px; font-weight: bold;")
          )
        ),
        selectInput("map_1_indicator_category", "Choose Composite Score:", 
                    choices = composite_choices, selected = "Social Inequality"),
        
        selectInput("map_1_variable_choice", "Choose a Variable:", choices = NULL)
      ),
      
      card(
        card_header(
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$span("Map Two Controls", style = "font-weight: 600;"),
            tags$span("2", style = "background: #198754; color: white; border-radius: 50%; 
                     width: 20px; height: 20px; display: flex; align-items: center; 
                     justify-content: center; font-size: 12px; font-weight: bold;")
          )
        ),
        selectInput("map_2_indicator_category", "Choose Composite Score:", 
                    choices = composite_choices, selected = "Social Inequality"),
        
        selectInput("map_2_variable_choice", "Choose a Variable:", choices = NULL)
      )
    )
  ),
  
  # Main content area with tabs
  navset_card_tab(
    id = "tabset",
    nav_panel("Interactive Map", 
              leafletOutput("map", height = 600)),
    
    nav_panel("Custom Graphs",
              # Global analysis results
              conditionalPanel(
                condition = "input.global_or_country == 'global'",
                tags$div(
                  style = "padding: 20px;",
                  tags$h3("Global Analysis Results", 
                          style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                  
                  card(
                    card_header("Bivariate Scatter Plot"),
                    plotOutput("global_custom_scatter")
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  card(
                    card_header("Correlation Analysis"),
                    verbatimTextOutput("global_correlation")
                  )
                )
              ),
              
              # Country analysis results
              conditionalPanel(
                condition = "input.global_or_country == 'country'",
                tags$div(
                  style = "padding: 20px;",
                  tags$h3("Country Analysis Results", 
                          style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                  
                  # Country info section
                  card(
                    card_header("Selected Country"),
                    tags$div(
                      style = "text-align: center; padding: 15px;",
                      textOutput("countryDisplay"),
                      tags$div(style = "margin: 10px 0;"),
                      imageOutput("country_flag", height = "120px")
                    )
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  # Histogram section
                  card(
                    card_header("Distribution Analysis"),
                    plotOutput("country_histogram")
                  ),
                  
                  tags$div(style = "margin: 20px 0;"),
                  
                  # Bivariate analysis section
                  card(
                    card_header("Bivariate Analysis"),
                    plotOutput("custom_scatter"),
                    tags$div(
                      style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
                      verbatimTextOutput("correlation")
                    )
                  )
                )
              )
    ),
    
    nav_panel("Country Comparison",
              tags$head(
                tags$style(HTML("
                .no-gutters > [class^='col-'] {
                  padding-left: 2px !important;
                  padding-right: 2px !important;
                }
              "))
              ),
              tags$div(
                style = "text-align: center;",
                tags$h3("Country Comparison Tool", 
                        style = "color: var(--bs-primary, #003087); margin-bottom: 20px;"),
                
                fluidRow(
                  class = "no-gutters",
                  style="padding-top: 15px;",
                  column(width=6,
                         div(
                           leafletOutput("compare_map_1", width = "100%", height = 700)
                         )
                  ),
                  column(width = 6,
                         div(
                           leafletOutput("compare_map_2", width = "100%", height = 700)
                         )
                  )
                ),
                
                fluidRow(
                  class = "no-gutters",
                  style="padding-top: 15px",
                  column(width = 6,
                         card(style="height: 100%",
                              card_header("Summary Statistics"),
                         )
                  ),
                  column(width = 6,class = "no-gutters",
                         card(style="height: 100%",
                              card_header("Summary Statistics"),
                         )
                  )
                )
              )
    )
  ),
  
  # Add Font Awesome for icons and custom JavaScript for search functionality
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      /* Additional custom styles for enhanced aesthetics */
      .card-header {
        background: linear-gradient(135deg, #f8f9fa, #e9ecef);
        border-bottom: 2px solid #dee2e6;
        font-weight: 600;
      }
      
      .selectize-input {
        transition: all 0.2s ease !important;
      }
      
      .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      
      /* Smooth animations */
      * {
        transition: all 0.2s ease;
      }
    ")),
    tags$script(HTML("
      // Country search functionality
      var countries = [];
      var selectedIndex = -1;
      
      $(document).ready(function() {
        // Initialize countries list when received from server
        Shiny.addCustomMessageHandler('updateCountriesList', function(data) {
          countries = data;
        });
        
        // Search input handler
        $('#country_search').on('input', function() {
          var query = $(this).val().toLowerCase();
          var suggestions = $('#country_suggestions');
          
          if (query.length === 0) {
            suggestions.hide();
            Shiny.setInputValue('country_search_query', '', {priority: 'event'});
            return;
          }
          
          // Filter countries
          var matches = countries.filter(function(country) {
            return country.toLowerCase().includes(query);
          }).slice(0, 10); // Limit to 10 suggestions
          
          if (matches.length === 0) {
            suggestions.hide();
            return;
          }
          
          // Build suggestions HTML
          var html = '';
          matches.forEach(function(country, index) {
            html += '<div class=\"country-suggestion\" data-country=\"' + country + '\" data-index=\"' + index + '\">' + country + '</div>';
          });
          
          suggestions.html(html).show();
          selectedIndex = -1;
          
          // Send query to server
          Shiny.setInputValue('country_search_query', query, {priority: 'event'});
        });
        
        // Click handler for suggestions
        $(document).on('click', '.country-suggestion', function() {
          var country = $(this).data('country');
          $('#country_search').val(country);
          $('#country_suggestions').hide();
          Shiny.setInputValue('country_search_selected', country, {priority: 'event'});
        });
        
        // Keyboard navigation
        $('#country_search').on('keydown', function(e) {
          var suggestions = $('.country-suggestion');
          
          if (e.key === 'ArrowDown') {
            e.preventDefault();
            selectedIndex = Math.min(selectedIndex + 1, suggestions.length - 1);
            updateSelection();
          } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            selectedIndex = Math.max(selectedIndex - 1, -1);
            updateSelection();
          } else if (e.key === 'Enter') {
            e.preventDefault();
            if (selectedIndex >= 0) {
              var country = suggestions.eq(selectedIndex).data('country');
              $('#country_search').val(country);
              $('#country_suggestions').hide();
              Shiny.setInputValue('country_search_selected', country, {priority: 'event'});
            }
          } else if (e.key === 'Escape') {
            $('#country_suggestions').hide();
            selectedIndex = -1;
          }
        });
        
        // Hide suggestions when clicking outside
        $(document).on('click', function(e) {
          if (!$(e.target).closest('#country_search, #country_suggestions').length) {
            $('#country_suggestions').hide();
          }
        });
        
        function updateSelection() {
          $('.country-suggestion').removeClass('highlighted');
          if (selectedIndex >= 0) {
            $('.country-suggestion').eq(selectedIndex).addClass('highlighted');
          }
        }
      });
    "))
  )
)
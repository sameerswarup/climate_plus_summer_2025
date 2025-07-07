library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

ui <- fluidPage(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        overflow: hidden;
      }
      
      .container-fluid {
        padding: 0;
        height: 100vh;
      }
      
      #map {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1;
      }
      
      .controls-panel {
        position: absolute;
        top: 20px;
        left: 20px;
        width: 340px;
        max-height: calc(100vh - 40px);
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        border-radius: 12px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
        z-index: 1000;
        overflow-y: auto;
        border: 1px solid rgba(255, 255, 255, 0.3);
      }
      
      .hamburger-menu {
        position: absolute;
        top: 15px;
        right: 15px;
        cursor: pointer;
        z-index: 1001;
        background: rgba(255, 255, 255, 0.9);
        border: 1px solid rgba(0, 0, 0, 0.1);
        border-radius: 6px;
        padding: 8px 10px;
        transition: all 0.2s ease;
      }
      
      .hamburger-menu:hover {
        background: rgba(255, 255, 255, 1);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
      }
      
      .hamburger-menu i {
        font-size: 16px;
        color: #495057;
      }
      
      .menu-dropdown {
        position: absolute;
        top: 100%;
        right: 0;
        background: white;
        border: 1px solid rgba(0, 0, 0, 0.1);
        border-radius: 6px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        min-width: 160px;
        display: none;
        z-index: 1002;
        margin-top: 5px;
      }
      
      .menu-dropdown.show {
        display: block;
      }
      
      .menu-item {
        padding: 10px 15px;
        cursor: pointer;
        border-bottom: 1px solid rgba(0, 0, 0, 0.05);
        transition: background-color 0.2s ease;
        font-size: 14px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .menu-item:hover {
        background-color: rgba(13, 110, 253, 0.05);
      }
      
      .menu-item.active {
        background-color: rgba(13, 110, 253, 0.1);
        color: #0d6efd;
        font-weight: 500;
      }
      
      .menu-item:last-child {
        border-bottom: none;
      }
      
      .menu-item i {
        font-size: 14px;
        width: 16px;
      }
      
      .panel-section {
        display: none;
      }
      
      .panel-section.active {
        display: block;
      }
      
      .control-group {
        padding: 15px;
        border-bottom: 1px solid rgba(0, 0, 0, 0.1);
      }
      
      .control-group:last-child {
        border-bottom: none;
      }
      
      .control-title {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 10px;
        font-size: 14px;
      }
      
      .section-header {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 15px;
        font-size: 16px;
        padding: 15px 15px 0 15px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .section-header i {
        font-size: 18px;
        color: #0d6efd;
      }
      
      .search-container {
        position: relative;
        margin-bottom: 15px;
        width: 100%;
      }
      
      .search-icon {
        position: absolute;
        left: 12px;
        top: 50%;
        transform: translateY(-50%);
        color: #6c757d;
        z-index: 10;
      }
      
      #country_search, #country_search_graphs {
        padding-left: 35px !important;
        border: 2px solid #e9ecef;
        border-radius: 8px;
        font-size: 14px;
        background: #fff;
        transition: all 0.2s ease;
        width: 100% !important;
        box-sizing: border-box;
        margin: 0;
      }
      
      #country_search:focus, #country_search_graphs:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);
        outline: none;
      }
      
      #country_suggestions, #country_suggestions_graphs {
        position: absolute;
        top: 100%;
        left: 0;
        right: 0;
        background: white;
        border: 1px solid #dee2e6;
        border-top: none;
        border-radius: 0 0 8px 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        z-index: 1000;
        display: none;
        max-height: 200px;
        overflow-y: auto;
      }
      
      .country-suggestion {
        padding: 10px 12px;
        border-bottom: 1px solid #f0f0f0;
        cursor: pointer;
        font-size: 14px;
        transition: background-color 0.2s ease;
      }
      
      .country-suggestion:hover,
      .country-suggestion.highlighted {
        background-color: #f8f9fa;
      }
      
      .country-suggestion:last-child {
        border-bottom: none;
      }
      
      .form-select, .form-control {
        border: 2px solid #e9ecef;
        border-radius: 6px;
        font-size: 13px;
        transition: all 0.2s ease;
      }
      
      .form-select:focus, .form-control:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #0d6efd, #0b5ed7);
        border: none;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(13, 110, 253, 0.3);
      }
      
      .btn-secondary {
        background: linear-gradient(135deg, #6c757d, #5a6268);
        border: none;
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      
      .btn-secondary:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(108, 117, 125, 0.3);
      }
      
      .form-check {
        margin: 8px 0;
      }
      
      .form-check-label {
        font-size: 13px;
        color: #495057;
        font-weight: 500;
      }
      
      .plot-container {
        margin: 15px 0;
        background: rgba(248, 249, 250, 0.8);
        border-radius: 8px;
        padding: 10px;
        border: 1px solid rgba(0, 0, 0, 0.05);
      }
      
      .controls-panel::-webkit-scrollbar {
        width: 6px;
      }
      
      .controls-panel::-webkit-scrollbar-track {
        background: rgba(0, 0, 0, 0.1);
        border-radius: 3px;
      }
      
      .controls-panel::-webkit-scrollbar-thumb {
        background: rgba(0, 0, 0, 0.3);
        border-radius: 3px;
      }
      
      @media (max-width: 768px) {
        .controls-panel {
          width: calc(100vw - 40px);
          max-width: 400px;
        }
      }
    "))
  ),
  
  leafletOutput("map", width = "100%", height = "100vh"),
  
  tags$div(
    id = "comparison-maps",
    # style = "display: none; position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 1;",
    tags$div(
      style = "display: flex; flex-direction: column; height: 100vh;",
      tags$div(
        style = "height: 50%; position: relative; width: 100%;",
        leafletOutput("map1", width = "100%", height = "100%"),
        tags$div(
          style = "position: absolute; top: 30px; right: 50px; background: rgba(255,255,255,0.9); padding: 8px 12px; border-radius: 6px; font-weight: 600; font-size: 14px; z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
          "Map 1"
        )
      ),
      tags$div(
        style = "height: 50%; position: relative; width: 100%;",
        leafletOutput("map2", width = "100%", height = "100%"),
        tags$div(
          style = "position: absolute; top: 10px; right: 50px; background: rgba(255,255,255,0.9); padding: 8px 12px; border-radius: 6px; font-weight: 600; font-size: 14px; z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
          "Map 2"
        )
      )
    )
  ),
  
  tags$div(
    class = "controls-panel",
    
    tags$div(
      class = "hamburger-menu",
      id = "hamburger-menu",
      tags$i(class = "fas fa-bars"),
      tags$div(
        class = "menu-dropdown",
        id = "menu-dropdown",
        tags$div(class = "menu-item active", `data-section` = "map",
                 tags$i(class = "fas fa-map"), "Interactive Map"),
        tags$div(class = "menu-item", `data-section` = "graphs",
                 tags$i(class = "fas fa-chart-line"), "Country Analysis"),
        tags$div(class = "menu-item", `data-section` = "comparison",
                 tags$i(class = "fas fa-balance-scale"), "Comparison"),
        tags$div(class = "menu-item", `data-section` = "about",
                 tags$i(class = "fas fa-info-circle"), "About")
      )
    ),
    
    tags$div(
      class = "header-section",
      style = "text-align: center; padding: 20px 15px 15px 15px; border-bottom: 1px solid rgba(0, 0, 0, 0.1);",
      tags$img(
        src = "dukelogo.jpg",
        alt = "Duke University",
        style = "max-width: 200px; height: auto; max-height: 60px; margin-bottom: 15px;"
      ),
      tags$div(
        class = "app-title",
        style = "font-size: 22px; font-weight: 700; color: #1e3a8a; letter-spacing: -0.5px; line-height: 1.2; margin-bottom: 5px; text-shadow: 0 1px 2px rgba(0,0,0,0.1);",
        "High-Stakes Coastal Mapper"
      ),
      tags$div(
        style = "font-size: 12px; color: #64748b; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px;",
        "Socio-Economic Analysis Platform"
      )
    ),
    
    tags$div(
      class = "panel-section active",
      id = "map-section",
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Country Search"),
        tags$div(
          class = "search-container",
          tags$i(class = "fas fa-search search-icon"),
          textInput(
            "country_search", 
            label = NULL,
            value = "",
            placeholder = "Search for a country...",
            width = "100%"
          ),
          tags$div(id = "country_suggestions")
        ),
        actionButton(
          "global_view_button", 
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; justify-content: center;",
            tags$i(class = "fas fa-globe-americas", style = "font-size: 12px;"),
            "Global View"
          ),
          style = "width: 100%;",
          class = "btn btn-primary"
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Map Controls"),
        selectInput("indicator_category", "Theme:", 
                    choices = composite_choices, selected = "Weak Governance"),
        conditionalPanel(condition = "input.indicator_category == 'Socio-Ecological Vulnerability'",
                         selectInput("composite_choice", "Composite Score:",
                                     choices = names(composite_data_options), selected = "Climate Risk")
                         ),
        selectInput("variable_choice", "Variable:", choices = NULL),
        tags$div(
          class = "form-check",
          checkboxInput(
            inputId = "use_country_specific_scale",
            label = "Country-specific scale",
            value = FALSE
          )
        ),
        tags$div(
          class = "form-check",
          checkboxInput(
            "satellite_view", 
            label = "Satellite view", 
            value = FALSE
          )
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "About Composite Scores"),
        tags$div(
          style = "font-size: 12px; line-height: 1.4; color: #6c757d;",
          tags$p(tags$strong("Weak Governance:"), "Government effectiveness, regulatory quality, rule of law, corruption control."),
          tags$p(tags$strong("Social Inequality:"), "Gender gaps, income distribution, health outcome disparities."),
          tags$p(tags$strong("Socio-Ecological Vulnerability:"), "Environmental degradation, coastal risks, nutritional dependencies.")
        )
      )
    ),
    
    tags$div(
      class = "panel-section",
      id = "graphs-section",
      
      tags$div(class = "section-header",
               tags$i(class = "fas fa-chart-line"), "Country Analysis"),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Country Search"),
        tags$div(
          class = "search-container",
          tags$i(class = "fas fa-search search-icon"),
          textInput(
            "country_search_graphs", 
            label = NULL,
            value = "",
            placeholder = "Search for a country...",
            width = "100%"
          ),
          tags$div(id = "country_suggestions_graphs")
        ),
        actionButton(
          "global_scale_button", 
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; justify-content: center;",
            tags$i(class = "fas fa-globe", style = "font-size: 12px;"),
            "View Global Scale"
          ),
          style = "width: 100%; margin-bottom: 15px;",
          class = "btn btn-secondary"
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Analysis Variables"),
        selectInput("country_histogram_indicator", "Histogram Variable:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "povmap.grdi.v1.sc"),
        tags$small(textOutput("country_histogram_description"),
                   style = "font-style: italic"),
        tags$br(),
        selectInput("first_indicator", "First Indicator:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "povmap.grdi.v1.sc"),
        tags$small(textOutput("first_indicator_country_description"),
                   style = "font-style: italic"),
        tags$br(),
        selectInput("second_indicator", "Second Indicator:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "perc.pop.world.coastal.merit.10m.log.sc"),
        tags$small(textOutput("second_indicator_country_description"),
                   style = "font-style: italic")
        
      ),
      
      tags$div(
        class = "control-group",
        textOutput("countryDisplay"),
        tags$div(class = "plot-container",
                 style = "overflow-x: auto;",
                 plotOutput("country_histogram", height = "120px",
                            width = "100%")),
        actionButton(
          "histogram_zoom",
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; justify-content: center;",
            tags$i(class = "fas fa-search-plus", style = "font-size: 12px;"),
            "Display Full Histogram"
          ),
          style = "width: 100%;",
          class = "btn btn-secondary"
        ),
        tags$div(class = "plot-container",
                 style = "overflow-x: auto;",
                 plotOutput("custom_scatter", height = "170px",
                            width = "100%")),

        verbatimTextOutput("correlation"),
        actionButton(
          "scatter_zoom",
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; justify-content: center;",
            tags$i(class = "fas fa-search-plus", style = "font-size: 12px;"),
            "Display Full Graph"
          ),
          style = "width: 100%;",
          class = "btn btn-secondary"
        )
        
      )
    ),
    
    #COMPARISON
    tags$div(
      class = "panel-section",
      id = "comparison-section",
      
      tags$div(class = "section-header",
               tags$i(class = "fas fa-balance-scale"), "Country Comparison"),
      
      tags$div(
        class = "control-group",
        selectizeInput("comparison_country_search", "Search Country:",
                       choices = NULL, selected = NULL,
                       options = list(placeholder = "Search for a country...", maxItems = 1, create = FALSE)),
        checkboxInput("use_comparison_country_scale", "Country-specific scale", value = FALSE)
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Map 1 Controls"),
        selectInput("map_1_indicator_category", "Composite Score:", 
                    choices = composite_choices, selected = "Social Inequality"),
        selectInput("map_1_variable_choice", "Variable:", choices = NULL)
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Map 2 Controls"), 
        selectInput("map_2_indicator_category", "Composite Score:",
                    choices = composite_choices, selected = "Social Inequality"),
        selectInput("map_2_variable_choice", "Variable:", choices = NULL)
      ),
      
      tags$div(
        class = "control-group",
        tags$p("Note: Comparison maps will appear as overlays when this mode is active.", 
               style = "font-size: 12px; color: #6c757d; font-style: italic;")
      )
    ),
    
    tags$div(
      class = "panel-section",
      id = "about-section",
      
      tags$div(class = "section-header",
               tags$i(class = "fas fa-info-circle"), "About"),
      
      tags$div(
        class = "control-group",
        tags$p("Content coming soon...", 
               style = "font-size: 14px; color: #6c757d; font-style: italic; text-align: center; padding: 40px 20px;")
      )
    )
  ),
  
  tags$script(HTML("
    $(document).ready(function() {
      $('#hamburger-menu').click(function(e) {
        e.stopPropagation();
        $('#menu-dropdown').toggleClass('show');
      });
      
      $(document).click(function(e) {
        if (!$(e.target).closest('#hamburger-menu').length) {
          $('#menu-dropdown').removeClass('show');
        }
      });
      
      $('.menu-item').click(function(e) {
        e.preventDefault();
        
        $('.menu-item').removeClass('active');
        $('.panel-section').removeClass('active');
        
        $(this).addClass('active');
        
        var section = $(this).data('section');
        $('#' + section + '-section').addClass('active');
        
        if (section === 'comparison') {
          $('#map').hide();
          $('#comparison-maps').show();
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
            if (window.Shiny) {
              Shiny.setInputValue('trigger_map_resize', Math.random(), {priority: 'event'});
            }
          }, 100);
        } else {
          $('#comparison-maps').hide();
          $('#map').show();
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 100);
        }
        
        $('#menu-dropdown').removeClass('show');
        
        // Re-initialize search functionality when switching to graphs section
        if (section === 'graphs') {
          setTimeout(function() {
            setupCountrySearch('country_search_graphs', 'country_suggestions_graphs');
          }, 100);
        }
      });
      
      var countries = [];
      var selectedIndex = -1;
      var currentSuggestionsId = '';
      
      Shiny.addCustomMessageHandler('updateCountriesList', function(data) {
        countries = data;
      });
      
      function setupCountrySearch(inputId, suggestionsId) {
        // Remove any existing event handlers to prevent duplicates
        $('#' + inputId).off('input keydown');
        
        $('#' + inputId).on('input', function() {
          var query = $(this).val().toLowerCase();
          var suggestions = $('#' + suggestionsId);
          currentSuggestionsId = suggestionsId;
          
          if (query.length === 0) {
            suggestions.hide();
            return;
          }
          
          var matches = countries.filter(function(country) {
            return country.toLowerCase().includes(query);
          }).slice(0, 10);
          
          if (matches.length === 0) {
            suggestions.hide();
            return;
          }
          
          var html = '';
          matches.forEach(function(country, index) {
            html += '<div class=\"country-suggestion\" data-country=\"' + country + '\" data-input=\"' + inputId + '\">' + country + '</div>';
          });
          
          suggestions.html(html).show();
          selectedIndex = -1;
        });
        
        $('#' + inputId).on('keydown', function(e) {
          var suggestions = $('#' + suggestionsId + ' .country-suggestion');
          
          if (e.key === 'ArrowDown') {
            e.preventDefault();
            selectedIndex = Math.min(selectedIndex + 1, suggestions.length - 1);
            updateSelection(suggestionsId);
          } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            selectedIndex = Math.max(selectedIndex - 1, -1);
            updateSelection(suggestionsId);
          } else if (e.key === 'Enter') {
            e.preventDefault();
            if (selectedIndex >= 0) {
              var country = suggestions.eq(selectedIndex).data('country');
              $('#' + inputId).val(country);
              suggestions.parent().hide();
              Shiny.setInputValue(inputId + '_selected', country, {priority: 'event'});
            }
          } else if (e.key === 'Escape') {
            suggestions.parent().hide();
            selectedIndex = -1;
          }
        });
      }
      
      // Initialize main search immediately
      setupCountrySearch('country_search', 'country_suggestions');
      
      // Handle clicks on suggestions
      $(document).on('click', '.country-suggestion', function() {
        var country = $(this).data('country');
        var inputId = $(this).data('input');
        
        $('#' + inputId).val(country);
        $(this).parent().hide();
        Shiny.setInputValue(inputId + '_selected', country, {priority: 'event'});
        selectedIndex = -1;
      });
      
      // Close suggestions when clicking outside
      $(document).on('click', function(e) {
        if (!$(e.target).closest('#country_search, #country_suggestions, #country_search_graphs, #country_suggestions_graphs').length) {
          $('#country_suggestions, #country_suggestions_graphs').hide();
          selectedIndex = -1;
        }
      });
      
      function updateSelection(suggestionsId) {
        $('#' + suggestionsId + ' .country-suggestion').removeClass('highlighted');
        if (selectedIndex >= 0) {
          $('#' + suggestionsId + ' .country-suggestion').eq(selectedIndex).addClass('highlighted');
        }
      }
      
      // Watch for UI changes and reinitialize search when graphs UI is rendered
      var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.type === 'childList') {
            // Check if country_search_graphs was added to the DOM
            if ($('#country_search_graphs').length > 0 && !$('#country_search_graphs').data('initialized')) {
              setTimeout(function() {
                setupCountrySearch('country_search_graphs', 'country_suggestions_graphs');
                $('#country_search_graphs').data('initialized', true);
              }, 50);
            }
          }
        });
      });
      
      // Start observing
      observer.observe(document.body, {
        childList: true,
        subtree: true
      });
    });
  "))
)
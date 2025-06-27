library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

ui <- fluidPage(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  
  # Initialize shinyjs
  useShinyjs(),
  
  # Custom CSS for full-screen overlay layout
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      /* Remove default margins and padding */
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
      
      /* Full screen map */
      #map {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1;
      }
      
      /* Comparison maps - side by side */
      .comparison-container {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1;
        display: none;
      }
      
      .comparison-container.active {
        display: flex;
      }
      
      .comparison-map {
        width: 50%;
        height: 100vh;
        position: relative;
      }
      
      .map-label {
        position: absolute;
        top: 15px;
        left: 15px;
        background: rgba(255, 255, 255, 0.9);
        padding: 8px 12px;
        border-radius: 6px;
        font-weight: 600;
        font-size: 14px;
        z-index: 1000;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      }
      
      .map-label.map1 {
        background: rgba(13, 110, 253, 0.9);
        color: white;
      }
      
      .map-label.map2 {
        background: rgba(220, 53, 69, 0.9);
        color: white;
      }
      
      /* Floating controls panel */
      .controls-panel {
        position: absolute;
        top: 20px;
        left: 20px;
        width: 400px;
        max-height: calc(100vh - 40px);
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        border-radius: 12px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
        z-index: 1000;
        overflow-y: auto;
        border: 1px solid rgba(255, 255, 255, 0.3);
      }
      
      /* Hamburger menu styling */
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
      
      /* Dropdown menu */
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
      
      /* Panel sections */
      .panel-section {
        display: none;
      }
      
      .panel-section.active {
        display: block;
      }
      
      /* Control styling */
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
      
      /* Section headers */
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
      
      /* Search bar styling */
      .search-container {
        position: relative;
        margin-bottom: 15px;
      }
      
      .search-icon {
        position: absolute;
        left: 12px;
        top: 50%;
        transform: translateY(-50%);
        color: #6c757d;
        z-index: 10;
      }
      
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
      
      /* Suggestions dropdown */
      #country_suggestions {
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
      
      /* Custom form controls */
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
      
      /* Button styling */
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
      
      /* Checkbox styling */
      .form-check {
        margin: 8px 0;
      }
      
      .form-check-label {
        font-size: 13px;
        color: #495057;
        font-weight: 500;
      }
      
      /* Plot containers */
      .plot-container {
        margin: 15px 0;
        background: rgba(248, 249, 250, 0.8);
        border-radius: 8px;
        padding: 10px;
        border: 1px solid rgba(0, 0, 0, 0.05);
      }
      
      /* Scrollbar styling */
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
      
      /* Mobile responsiveness */
      @media (max-width: 768px) {
        .controls-panel {
          width: calc(100vw - 40px);
          max-width: 400px;
        }
      }
    "))
  ),
  
  # Full screen map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # Comparison maps container
  tags$div(
    class = "comparison-container",
    id = "comparison-container",
    tags$div(
      class = "comparison-map",
      leafletOutput("comp_map1", width = "100%", height = "100vh"),
      tags$div(class = "map-label map1", "Map 1")
    ),
    tags$div(
      class = "comparison-map", 
      leafletOutput("comp_map2", width = "100%", height = "100vh"),
      tags$div(class = "map-label map2", "Map 2")
    )
  ),
  
  # Floating controls panel with hamburger menu
  tags$div(
    class = "controls-panel",
    
    # Hamburger menu
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
                 tags$i(class = "fas fa-chart-line"), "Custom Graphs"),
        tags$div(class = "menu-item", `data-section` = "comparison",
                 tags$i(class = "fas fa-balance-scale"), "Comparison"),
        tags$div(class = "menu-item", `data-section` = "about",
                 tags$i(class = "fas fa-info-circle"), "About")
      )
    ),
    
    # Header section (Duke logo + title) - appears on all tabs
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
    
    # Interactive Map Section (Default)
    tags$div(
      class = "panel-section active",
      id = "map-section",
      
      # Search section
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
      
      # Map controls section
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Map Controls"),
        selectInput("indicator_category", "Composite Score:", 
                    choices = composite_choices, selected = "Weak Governance"),
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
      
      # Descriptions section
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
    
    # Custom Graphs Section
    tags$div(
      class = "panel-section",
      id = "graphs-section",
      
      tags$div(class = "section-header",
               tags$i(class = "fas fa-chart-line"), "Custom Analysis"),
      
      tags$div(
        class = "control-group",
        selectInput("global_or_country", "Analysis Level:",
                    choices = c("Global" = "global", "Country" = "country"),
                    selected = "global"),
        uiOutput("global_or_country_components")
      ),
      
      # Results area
      tags$div(
        class = "control-group",
        conditionalPanel(
          condition = "input.global_or_country == 'global'",
          tags$div(class = "plot-container",
                   plotOutput("global_custom_scatter", height = "300px")),
          verbatimTextOutput("global_correlation")
        ),
        conditionalPanel(
          condition = "input.global_or_country == 'country'",
          textOutput("countryDisplay"),
          tags$div(class = "plot-container",
                   plotOutput("country_histogram", height = "200px")),
          tags$div(class = "plot-container",
                   plotOutput("custom_scatter", height = "300px")),
          verbatimTextOutput("correlation")
        )
      )
    ),
    
    # Comparison Section
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
    
    # About Section
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
  
  # JavaScript for hamburger menu and search functionality
  tags$script(HTML("
    $(document).ready(function() {
      // Hamburger menu functionality
      $('#hamburger-menu').click(function(e) {
        e.stopPropagation();
        $('#menu-dropdown').toggleClass('show');
      });
      
      // Close dropdown when clicking outside
      $(document).click(function(e) {
        if (!$(e.target).closest('#hamburger-menu').length) {
          $('#menu-dropdown').removeClass('show');
        }
      });
      
      // Menu item selection
      $('.menu-item').click(function(e) {
        e.preventDefault();
        
        // Remove active class from all menu items and sections
        $('.menu-item').removeClass('active');
        $('.panel-section').removeClass('active');
        
        // Add active class to clicked menu item
        $(this).addClass('active');
        
        // Show corresponding section
        var section = $(this).data('section');
        $('#' + section + '-section').addClass('active');
        
        // Handle map visibility
        if (section === 'comparison') {
          $('#map').hide();
          $('#comparison-container').addClass('active');
        } else {
          $('#comparison-container').removeClass('active');
          $('#map').show();
        }
        
        // Hide dropdown
        $('#menu-dropdown').removeClass('show');
      });
      
      // Country search functionality
      var countries = [];
      var selectedIndex = -1;
      
      Shiny.addCustomMessageHandler('updateCountriesList', function(data) {
        countries = data;
      });
      
      $('#country_search').on('input', function() {
        var query = $(this).val().toLowerCase();
        var suggestions = $('#country_suggestions');
        
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
          html += '<div class=\"country-suggestion\" data-country=\"' + country + '\">' + country + '</div>';
        });
        
        suggestions.html(html).show();
        selectedIndex = -1;
      });
      
      $(document).on('click', '.country-suggestion', function() {
        var country = $(this).data('country');
        $('#country_search').val(country);
        $('#country_suggestions').hide();
        Shiny.setInputValue('country_search_selected', country, {priority: 'event'});
      });
      
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
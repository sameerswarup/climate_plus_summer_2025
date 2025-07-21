library(shiny)
library(leaflet)
library(bslib)
library(shinyjs)

ui <- fluidPage(
  title = "High Stakes Socio-Economic Mapping of Coastal Zones",
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        height: 100%;
        overflow: hidden;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        font-weight: 400;
        letter-spacing: -0.01em;
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
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        border-radius: 16px;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.12), 0 8px 16px rgba(0, 0, 0, 0.08);
        z-index: 1000;
        overflow-y: auto;
        border: 1px solid rgba(255, 255, 255, 0.4);
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .controls-panel:hover {
        box-shadow: 0 24px 48px rgba(0, 0, 0, 0.15), 0 12px 20px rgba(0, 0, 0, 0.1);
        transform: translateY(-2px);
      }
      
      .hamburger-menu {
        position: absolute;
        top: 18px;
        right: 18px;
        cursor: pointer;
        z-index: 1001;
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(8px);
        -webkit-backdrop-filter: blur(8px);
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-radius: 10px;
        padding: 12px 14px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
      }
      
      .hamburger-menu:hover {
        background: rgba(255, 255, 255, 1);
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
        transform: translateY(-1px) scale(1.02);
        border-color: rgba(13, 110, 253, 0.2);
      }
      
      .hamburger-menu:active {
        transform: translateY(0) scale(0.98);
        transition: all 0.1s ease;
      }
      
      .hamburger-menu i {
        font-size: 16px;
        color: #495057;
        transition: all 0.3s ease;
      }
      
      .hamburger-menu:hover i {
        color: #0d6efd;
        transform: rotate(90deg);
      }
      
      .menu-dropdown {
        position: absolute;
        top: 100%;
        right: 0;
        background: rgba(255, 255, 255, 0.96);
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(0, 0, 0, 0.12), 0 4px 12px rgba(0, 0, 0, 0.08);
        min-width: 200px;
        display: none;
        z-index: 1002;
        margin-top: 8px;
        overflow: hidden;
        animation: dropdownSlide 0.2s cubic-bezier(0.4, 0, 0.2, 1);
        white-space: nowrap;
      }
      
      @keyframes dropdownSlide {
        from {
          opacity: 0;
          transform: translateY(-8px) scale(0.95);
        }
        to {
          opacity: 1;
          transform: translateY(0) scale(1);
        }
      }
      
      .menu-dropdown.show {
        display: block;
      }
      
      .menu-item {
        padding: 14px 18px;
        cursor: pointer;
        border-bottom: 1px solid rgba(0, 0, 0, 0.04);
        transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
        font-size: 14px;
        font-weight: 500;
        display: flex;
        align-items: center;
        gap: 12px;
        position: relative;
        background: transparent;
      }
      
      .menu-item::before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        height: 100%;
        width: 4px;
        background: #0d6efd;
        transform: scaleY(0);
        transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        border-radius: 0 2px 2px 0;
      }
      
      .menu-item:hover {
        background: rgba(13, 110, 253, 0.06);
        padding-left: 22px;
        color: #0d6efd;
        transform: translateX(2px);
      }
      
      .menu-item:hover::before {
        transform: scaleY(1);
      }
      
      .menu-item.active {
        background: rgba(13, 110, 253, 0.1);
        color: #0d6efd;
        font-weight: 600;
        padding-left: 22px;
      }
      
      .menu-item.active::before {
        transform: scaleY(1);
      }
      
      .menu-item:last-child {
        border-bottom: none;
      }
      
      .menu-item i {
        font-size: 12px;
        width: 16px;
        flex-shrink: 0;
        transition: all 0.3s ease;
      }
      
      .menu-item:hover i {
        transform: scale(1.08);
      }
      
      .panel-section {
        display: none;
        animation: panelFadeIn 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      @keyframes panelFadeIn {
        from {
          opacity: 0;
          transform: translateY(10px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .panel-section.active {
        display: block;
      }
      
      .control-group {
        padding: 20px;
        border-bottom: 1px solid rgba(0, 0, 0, 0.06);
        transition: all 0.3s ease;
      }
      
      .control-group:hover {
        background: rgba(248, 249, 250, 0.5);
      }
      
      .control-group:last-child {
        border-bottom: none;
      }
      
      .control-title {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 12px;
        font-size: 14px;
        letter-spacing: -0.02em;
        position: relative;
        padding-left: 12px;
      }
      
      .control-title::before {
        content: '';
        position: absolute;
        left: 0;
        top: 50%;
        transform: translateY(-50%);
        width: 3px;
        height: 16px;
        background: linear-gradient(135deg, #0d6efd, #0b5ed7);
        border-radius: 2px;
      }
      
      .section-header {
        font-weight: 700;
        color: #1a1f36;
        margin-bottom: 18px;
        font-size: 18px;
        padding: 20px 20px 0 20px;
        display: flex;
        align-items: center;
        gap: 12px;
        letter-spacing: -0.03em;
        position: relative;
      }
      
      .section-header.with-line {
        padding-left: 32px;
      }
      
      .section-header.with-line::before {
        content: '';
        position: absolute;
        left: 20px;
        top: 50%;
        transform: translateY(-50%);
        width: 4px;
        height: 24px;
        background: linear-gradient(135deg, #0d6efd, #0b5ed7);
        border-radius: 2px;
      }
      
      .section-header i {
        font-size: 20px;
        color: #0d6efd;
        transition: all 0.3s ease;
      }
      
      .section-header:hover i {
        transform: scale(1.05);
      }
      
      .search-container {
        position: relative;
        margin-bottom: 16px;
        width: 100%;
      }
      
      .search-icon {
        position: absolute;
        left: 14px;
        top: 50%;
        transform: translateY(-50%);
        color: #6c757d;
        z-index: 10;
        transition: all 0.3s ease;
      }
      
      .search-container:hover .search-icon {
        color: #0d6efd;
        transform: translateY(-50%) scale(1.05);
      }
      
      #country_search, #country_search_graphs {
        padding-left: 42px !important;
        border: 2px solid #e9ecef;
        border-radius: 10px;
        font-size: 14px;
        font-weight: 500;
        background: #fff;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        width: 100% !important;
        box-sizing: border-box;
        margin: 0;
        height: 44px;
      }
      
      #country_search:focus, #country_search_graphs:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.15);
        outline: none;
        transform: translateY(-1px);
      }
      
      #country_suggestions, #country_suggestions_graphs {
        position: absolute;
        top: 100%;
        left: 0;
        right: 0;
        background: rgba(255, 255, 255, 0.96);
        backdrop-filter: blur(8px);
        -webkit-backdrop-filter: blur(8px);
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-top: none;
        border-radius: 0 0 10px 10px;
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12);
        z-index: 1000;
        display: none;
        max-height: 200px;
        overflow-y: auto;
      }
      
      .country-suggestion {
        padding: 12px 16px;
        border-bottom: 1px solid rgba(0, 0, 0, 0.04);
        cursor: pointer;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .country-suggestion:hover,
      .country-suggestion.highlighted {
        background: rgba(13, 110, 253, 0.08);
        color: #0d6efd;
        transform: translateX(4px);
      }
      
      .country-suggestion:last-child {
        border-bottom: none;
      }
      
      .form-select, .form-control {
        border: 2px solid #e9ecef;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        font-family: 'Inter', sans-serif;
      }
      
      .form-select:focus, .form-control:focus {
        border-color: #0d6efd;
        box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.15);
        transform: translateY(-1px);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #0d6efd, #0b5ed7);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        font-family: 'Inter', sans-serif;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        box-shadow: 0 4px 12px rgba(13, 110, 253, 0.2);
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(13, 110, 253, 0.3);
        background: linear-gradient(135deg, #0b5ed7, #0a58ca);
      }
      
      .btn-primary:active {
        transform: translateY(0);
        transition: all 0.1s ease;
      }
      
      .btn-secondary {
        background: linear-gradient(135deg, #6c757d, #5a6268);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        font-family: 'Inter', sans-serif;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        box-shadow: 0 4px 12px rgba(108, 117, 125, 0.2);
      }
      
      .btn-secondary:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(108, 117, 125, 0.3);
        background: linear-gradient(135deg, #5a6268, #495057);
      }
      
      .btn-secondary:active {
        transform: translateY(0);
        transition: all 0.1s ease;
      }
      
      .form-check {
        margin: 10px 0;
      }
      
      .form-check-label {
        font-size: 14px;
        color: #495057;
        font-weight: 500;
        font-family: 'Inter', sans-serif;
      }
      
      .plot-container {
        margin: 16px 0;
        background: rgba(248, 249, 250, 0.8);
        border-radius: 10px;
        padding: 12px;
        border: 1px solid rgba(0, 0, 0, 0.04);
        transition: all 0.3s ease;
      }
      
      .plot-container:hover {
        background: rgba(248, 249, 250, 1);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
        transform: translateY(-1px);
      }
      
      .controls-panel::-webkit-scrollbar {
        width: 8px;
      }
      
      .controls-panel::-webkit-scrollbar-track {
        background: rgba(0, 0, 0, 0.04);
        border-radius: 4px;
      }
      
      .controls-panel::-webkit-scrollbar-thumb {
        background: rgba(0, 0, 0, 0.2);
        border-radius: 4px;
        transition: all 0.3s ease;
      }
      
      .controls-panel::-webkit-scrollbar-thumb:hover {
        background: rgba(0, 0, 0, 0.4);
      }
      
      .header-section {
        text-align: center; 
        padding: 24px 20px 20px 20px; 
        border-bottom: 1px solid rgba(0, 0, 0, 0.08);
        background: linear-gradient(135deg, rgba(13, 110, 253, 0.02), rgba(255, 255, 255, 0.8));
      }
      
      .app-title {
        font-size: 24px; 
        font-weight: 800; 
        color: #1e3a8a; 
        letter-spacing: -0.5px; 
        line-height: 1.2; 
        margin-bottom: 6px; 
        text-shadow: 0 2px 4px rgba(0,0,0,0.08);
        font-family: 'Inter', sans-serif;
      }
      
      .team-member {
        display: flex; 
        align-items: center; 
        gap: 12px; 
        padding: 12px; 
        background: rgba(248, 249, 250, 0.8); 
        border-radius: 10px; 
        border: 1px solid rgba(0, 0, 0, 0.04); 
        margin-bottom: 12px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      .team-member:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.08);
        background: rgba(13, 110, 253, 0.04);
      }
      
      /* Custom styles for select dropdowns */
      .selectize-input {
        border: 2px solid #e9ecef !important;
        border-radius: 8px !important;
        font-family: 'Inter', sans-serif !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
      }
      
      .selectize-input.focus {
        border-color: #0d6efd !important;
        box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.15) !important;
      }
      
      /* Code/monospace elements */
      code, pre, .verbatim {
        font-family: 'JetBrains Mono', 'Monaco', 'Menlo', monospace;
        font-weight: 500;
      }
      
      @media (max-width: 768px) {
        .controls-panel {
          width: calc(100vw - 40px);
          max-width: 400px;
        }
      }
      
      .leaflet-tooltip {
  background: rgba(255, 255, 255, 0.96) !important;
  border: 1px solid rgba(148, 163, 184, 0.2) !important;
  border-radius: 8px !important;
  box-shadow: 0 8px 20px rgba(0, 0, 0, 0.12) !important;
  
  font-family: 'Inter', sans-serif !important;
  font-size: 12px !important;
  font-weight: 600 !important;
  color: #1e3a8a !important;  /* Duke Blue */
  
  padding: 8px 12px !important;
  white-space: nowrap !important;
  pointer-events: none !important;
}

.leaflet-tooltip::before {
  display: none !important;
}

/* Simple Legend Styling - Duke Blue Text */
.leaflet-control {
  background: rgba(255, 255, 255, 0.95) !important;
  backdrop-filter: blur(8px) !important;
  border: 1px solid rgba(148, 163, 184, 0.2) !important;
  border-radius: 8px !important;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1) !important;
}

.leaflet-control .leaflet-control-layers-list,
.leaflet-control div,
.leaflet-control span {
  color: #1e3a8a !important; /* Duke Blue */
  font-family: 'Inter', sans-serif !important;
  font-weight: 600 !important;
  font-size: 12px !important;
}
    "))
  ),
  # COMPARISON
  tags$div(
    id = "comparison-maps",
    # style = "display: none; position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 1;",
    
    tags$div(
      style = "display: flex; flex-direction: column; height: 100vh;",
      
      tags$div(
        style = "height: 50%; position: relative; width: 100%;",
        
        # Main map (for Social Inequality, Weak Governance, and Socio-Ecological Vulnerability with Inequity)
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Social Inequality' || input.indicator_category_map_1 == 'Weak Governance' || (input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'Inequity')",
          leafletOutput("map1", width = "100%", height = "50vh")
        ),
        
        # Climate Risk map
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'Climate Risk'",
          leafletOutput("climate_map_1", width = "100%", height = "50vh")
        ),
        
        # ND GAIN map
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'ND GAIN'",
          leafletOutput("nd_gain_map_1", width = "100%", height = "50vh")
        ),
        
        #leafletOutput("map1", width = "100%", height = "100%"),
        tags$div(
          style = "position: absolute; top: 30px; right: 50px; background: rgba(255,255,255,0.9); padding: 8px 12px; border-radius: 6px; font-weight: 600; font-size: 14px; z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
          "Map 1"
        )
      ),
      
      
      tags$div(
        style = "height: 50%; position: relative; width: 100%;",
        #leafletOutput("map2", width = "100%", height = "100%"),
        
        # Main map (for Social Inequality, Weak Governance, and Socio-Ecological Vulnerability with Inequity)
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Social Inequality' || input.indicator_category_map_2 == 'Weak Governance' || (input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'Inequity')",
          leafletOutput("map2", width = "100%", height = "50vh")
        ),
        
        # Climate Risk map
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'Climate Risk'",
          leafletOutput("climate_map_2", width = "100%", height = "50vh")
        ),
        
        # ND GAIN map
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'ND GAIN'",
          leafletOutput("nd_gain_map_2", width = "100%", height = "50vh")
        ),
        
        tags$div(
          style = "position: absolute; top: 10px; right: 50px; background: rgba(255,255,255,0.9); padding: 8px 12px; border-radius: 6px; font-weight: 600; font-size: 14px; z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
          "Map 2"
        )
      )
    )
  ),
  
  
  
  
  # MAIN MAP
  # Main map (for Social Inequality, Weak Governance, and Socio-Ecological Vulnerability with Inequity)
  conditionalPanel(
    condition = "input.indicator_category == 'Social Inequality' || input.indicator_category == 'Weak Governance' || (input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'Inequity')",
    leafletOutput("map", width = "100%", height = "100vh")
  ),
  
  # ND GAIN map
  conditionalPanel(
    condition = "input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'ND GAIN'",
    leafletOutput("nd_gain_map", width = "100%", height = "100vh")
  ),
  
  # Climate Risk map
  conditionalPanel(
    condition = "input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'Climate Risk'",
    leafletOutput("climate_map", width = "100%", height = "100vh")
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
        tags$div(class = "menu-item", `data-section` = "data-overview",  
                 tags$i(class = "fas fa-sitemap"), "Data Overview"),
        tags$div(class = "menu-item", `data-section` = "about",
                 tags$i(class = "fas fa-info-circle"), "About")
      )
    ),
    
    tags$div(
      class = "header-section",
      tags$img(
        src = "dukelogo.jpg",
        alt = "Duke University",
        style = "max-width: 200px; height: auto; max-height: 60px; margin-bottom: 15px;"
      ),
      tags$div(
        class = "app-title",
        "High-Stakes Coastal Mapper"
      ),
      tags$div(
        style = "font-size: 12px; color: #64748b; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; font-family: 'Inter', sans-serif;",
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
            value = "Global (Default)",
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
                    choices = composite_choices, selected = "Socio-Ecological Vulnerability"),
        
        # Optional: hide composite_choice unless relevant
        conditionalPanel(
          condition = "input.indicator_category == 'Socio-Ecological Vulnerability'",
          selectInput("composite_choice", "Composite Score:",
                      choices = names(composite_data_options), selected = "Inequity")
        ),
        conditionalPanel(
          condition = "input.indicator_category == 'Social Inequality' || input.indicator_category == 'Weak Governance' || (input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'Inequity')",
          selectInput("variable_choice", "Variable:", choices = NULL)
        ),
        tags$div(
          class = "form-check",
          checkboxInput(
            inputId = "use_country_specific_scale",
            label = "Country-specific scale",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.indicator_category != 'Socio-Ecological Vulnerability' || input.composite_choice != 'Climate Risk'",
          tags$div(
            class = "form-check",
            checkboxInput(
              "satellite_view", 
              label = "Street map view", 
              value = FALSE
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'ND GAIN'",
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "What is ND GAIN?"),
          tags$small(
            style = "font-style: italic;",
            "The Notre Dame Global Adaptation Initiative's (ND-GAIN) Country Index is a free, open
          source index that shows a country's current vulnerability to climate disruptions. It also
          assesses a country's readiness to leverage private and public sector investment for
          adaptive actions. The ND-GAIN Country Index brings together more than 40 core
          indicators to measure vulnerability and readiness of 182 UN countries from 1995 to the
          present (10 countries only have readiness scores)."
          )
        ),
        
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Map Controls"),
          selectInput(inputId = "variable_nd",
                      label = "Choose a variable/indicator:",
                      choices = gainVars,
                      selected = "Value..gain"),
          tags$small(
            style = "font-style: italic;",
            textOutput("indDescOutput")
          ),
          sliderInput(inputId = "nd_year",
                      label = "Choose a year:",
                      min = 1995,
                      max = 2022,
                      value = 1995,
                      sep = "",
                      animate = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.country_search !=  'Global (Default)'",
          tags$div(
            class = "control-group",
            tags$div(class = "control-title", "Data Summary"),
            wellPanel(
              h4(textOutput("summary_title")),
              p(strong("Score:"), textOutput("summary_score"))
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.country_search !=  'Global (Default)'",
          plotOutput("nd_graph", width = "95%", height = "300px"),
        ),
        

        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "About Indicators"),
          tags$div(
            style = "font-size: 12px; line-height: 1.4; color: #6c757d;",
            tags$p(tags$strong("Readiness Indicators:"), "0 - 1, Higher is Better"),
            tags$p(tags$strong("Vulnerability Indicators:"), "0 - 1, Lower is Better"),
            tags$p(tags$strong("ND GAIN Index:"), "0 - 100, Higher is Better"),
          )
        ),
        
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Data Source"),
          tags$a(
            href = "https://gain.nd.edu/our-work/country-index/download-data/",
            "Notre Dame Global Adaptation Initiative",
            target = "_blank"
          ),
          tags$small(
            "Download up to half a million data points for more than 180 UN countries. Data is updated annually, but includes all ND-GAIN indicators across 20+ years. Data is provided as separate CSV files in a single compressed file.",
            style = "font-style: italic"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.indicator_category == 'Socio-Ecological Vulnerability' && input.composite_choice == 'Climate Risk'",
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Climate Variable Controls"),
          
          selectInput(
            "climate_variable", 
            "Select Climate Variable:", 
            choices = names(climate_data_options)
          ),
          
          uiOutput("data_type_selector"),
          uiOutput("time_period_selector"),
          uiOutput("variable_info"),
          
          checkboxInput(
            "use_masked_raster",
            label = "Show only Exclusive Economic Zones (EEZs)",
            value = FALSE
          )
        ),
        
        # Filter controls card
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Data Filters"),
          
          uiOutput("value_range_slider"),
          uiOutput("manual_min_input"),
          uiOutput("manual_max_input"),
          
          radioButtons(
            "filter_mode", 
            "Filter Mode:",
            choices = list(
              "Show All Data" = "none",
              "Show Values in Range" = "range",
              "Show Above Threshold" = "above", 
              "Show Below Threshold" = "below"
            ),
            selected = "none"
          ),
          
          actionButton(
            "reset_filters", 
            "Reset Filters", 
            class = "btn-outline-secondary btn-sm",
            style = "margin-top: 10px;"
          )
        ),
        
        # Info display
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Data Summary"),
          
          div(
            style = "font-family: Arial, sans-serif;",
            verbatimTextOutput("data_info")
          )
        ),
        
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Data Distribution Plots"),
          
          tags$small(
            style = "font-style: italic;",
            "Explore the distribution of climate variable values."
          ),
          
          plotlyOutput("histogram_plot")
        ), 
        
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Data Source"),
          
          tags$a(
            href = "https://interactive-atlas.ipcc.ch/",
            "Climate Risk Data Source",
            target = "_blank"
          ),
          
          tags$small(
            style = "font-style: italic;",
            "Description to be added soon..."
          )
        ),
        
        # ✅ Moved Click Info card INSIDE this conditional
        tags$div(
          class = "control-group",
          tags$div(class = "control-title", "Click Info"),
          
          verbatimTextOutput("click_info")
        ),
        conditionalPanel(
          condition = "input.climate_variable == 'Ocean pH' || input.climate_variable == 'Sea Level Rise' || input.climate_variable == 'Heating Degree Days'",
          tags$div(
            class = "control-group",
            tags$div(class = "control-title", "CMIP6 Model Assumptions"),
            tags$small(
              style = "font-size: 12px; line-height: 1.4;",
              "This CMIP6 model, used for this visualization, is based on the following assumptions:",
              tags$ul(
                tags$li(tags$strong("Dynamics:"), " Model assumes large-scale atmospheric and oceanic flows are solved on a grid, with smaller-scale processes like convection represented with simplified parameterizations."),
                tags$li(tags$strong("Radiation:"), " Model assumes radiative transfer is calculated using spectral bands with defined optical properties for gases, clouds, and aerosols."),
                tags$li(tags$strong("Clouds:"), " Model assumes cloud properties are represented as functions of temperature, humidity, and atmospheric dynamics."),
                tags$li(tags$strong("Cloud Feedbacks:"), " Model assumes changes in cloud properties with warming are represented by parameterizations that generally produce positive feedback."),
                tags$li(tags$strong("Aerosols:"), " Model assumes aerosols affect climate through scattering, absorption, and cloud interactions, captured with simplified parameterizations."),
                tags$li(tags$strong("Climate Sensitivity:"), " Models assumes equilibrium climate sensitivity reflects the combined effect of feedbacks."),
                tags$li(tags$strong("Land and Carbon Cycle:"), " Model assumes photosynthesis, respiration, and soil carbon processes can be described with equations based on temperature, CO₂ concentration, and moisture."),
                tags$li(tags$strong("Ocean and Sea Ice:"), " Models assumes ocean circulation is resolved on grids, with subgrid-scale mixing and sea ice dynamics represented by parameterizations."),
                tags$li(tags$strong("Chemistry:"), " Model assumes simplified chemical reactions and nutrient cycling capture the main behavior of trace gases and carbon flows."),
                tags$li(tags$strong("Spatial Resolution:"), " Model assumes grid and subgrid parameterizations together represent key climate processes at typical resolutions of 50–200 km."),
                tags$li(tags$strong("Forcing Data Consistency:"), " Model assumes historical and scenario-based forcings, such as greenhouse gas concentrations and land-use.")
              )
            )
          )
        )
      ),
      
      uiOutput("dynamic_about_section"),
    ),
    
    tags$div(
      class = "panel-section",
      id = "graphs-section",
      
      tags$div(class = "section-header",
               "Country Analysis"),
      
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
        ),
        tags$small("View global scale scatter plot: each point represents a country's average score for both chosen indicators.",
                   style = "font-style: italic")
      ),
      
      tags$div(
        class = "control-group",
        textOutput("countryDisplay"),
        
        tags$div(class = "control-title", "Analysis Variables"),
        selectInput("country_histogram_indicator", "Histogram Variable:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "povmap.grdi.v1.sc"),
        tags$small(textOutput("country_histogram_description"),
                   style = "font-style: italic"),
        tags$br(),
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
        )
        
      ),
      
      tags$div(
        class = "control-group",
        
        selectInput("first_indicator", "First Scatter Plot Indicator:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "povmap.grdi.v1.sc"),
        tags$small(textOutput("first_indicator_country_description"),
                   style = "font-style: italic"),
        tags$br(),
        selectInput("second_indicator", "Second Scatter Plot Indicator:", 
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "perc.pop.world.coastal.merit.10m.log.sc"),
        tags$small(textOutput("second_indicator_country_description"),
                   style = "font-style: italic"),
        tags$br(),
        
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
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Regional Analysis"),
        tags$small(textOutput("number_of_regions_text")),
        tags$br(),
        selectInput("ca_region_chooser", textOutput("region_country_text"), 
                    choices = NULL),
        
        selectInput("ra_bar_graph_selector", "Select a Score:",
                    choices = c("Degraded Ecosystems" = "mean.count.grav.V2.log.sc",
                                "Relative Deprivation Index" = "povmap.grdi.v1.sc",
                                "Coastal Vulnerability" = "perc.pop.world.coastal.merit.10m.log.sc"), 
                    selected = "povmap.grdi.v1.sc"),
        tags$div(class = "plot-container",
                 style = "overflow-x: auto;",
                 plotOutput("ra_bar_graph", height = "300px",
                            width = "100%")),
        tags$small("World: Averaged score of selected indicator across 114 countries worldwide."),
        tags$div(style = "font-size: smaller;", textOutput("country_average_description")),
        tags$div(style = "font-size: smaller;", textOutput("region_average_description"))
        
      )
    ),
    
    
    
    
    #COMPARISON
    tags$div(
      class = "panel-section",
      id = "comparison-section",
      
      tags$div(class = "section-header",
               "Country Comparison"),
      
      # MAP 1 CONTROLS
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Map 1 Controls", style="color: darkblue"),
        selectizeInput("comparison_country_search_map_1", "Search Country:",
                       choices = NULL, selected = NULL,
                       options = list(placeholder = "Search for a country...", maxItems = 1, create = FALSE)),
        #selectInput("indicator_category", "Theme:", 
        #            choices = composite_choices, selected = "Weak Governance"),
        selectInput("indicator_category_map_1", "Theme:", #Composite Score:
                    choices = composite_choices, selected = "Social Inequality"),
        
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Socio-Ecological Vulnerability'",
          selectInput("composite_choice_map_1", "Composite Score:",
                      choices = names(composite_data_options), selected = "Inequity")
        ),        
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Social Inequality' || input.indicator_category_map_1 == 'Weak Governance' || (input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'Inequity')",
          selectInput("variable_choice_map_1", "Variable:", choices = NULL),
        ),   
        
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'ND GAIN'",
          
          # tags$div(
          #   class = "control-group",
          selectInput(inputId = "variable_nd_map_1",
                      label = "Choose a variable/indicator:",
                      choices = gainVars,
                      selected = "Value..gain"),
          
          sliderInput(inputId = "nd_year_map_1",
                      label = "Choose a year:",
                      min = 1995,
                      max = 2022,
                      value = 1995,
                      sep = "",
                      animate = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.indicator_category_map_1 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_1 == 'Climate Risk'",
          
          selectInput(
            "climate_variable_map_1", 
            "Select Climate Variable:", 
            choices = names(climate_data_options)
          ),
          
          uiOutput("data_type_selector_map_1"),
          uiOutput("time_period_selector_map_1"),
          
          checkboxInput(
            "use_masked_raster_map_1",
            label = "Show only Exclusive Economic Zones (EEZs)",
            value = FALSE
          ),
          
          tags$div(class = "control-title", "Data Filters"),
          
          uiOutput("value_range_slider_map_1"),
          uiOutput("manual_min_input_map_1"),
          uiOutput("manual_max_input_map_1"),
          
          radioButtons(
            "filter_mode_map_1", 
            "Filter Mode:",
            choices = list(
              "Show All Data" = "none",
              "Show Values in Range" = "range",
              "Show Above Threshold" = "above", 
              "Show Below Threshold" = "below"
            ),
            selected = "none"
          ),
          
          actionButton(
            "reset_filters_map_1", 
            "Reset Filters", 
            class = "btn-outline-secondary btn-sm",
            style = "margin-top: 10px;"
          ),
          
          tags$div(class = "control-title", "Data Summary"),
          
          div(
            style = "font-family: Arial, sans-serif;",
            verbatimTextOutput("data_info_map_1")
          ),
          
          
          # ✅ Moved Click Info card INSIDE this conditional
          tags$div(
            class = "control-group",
            tags$div(class = "control-title", "Click Info"),
            
            verbatimTextOutput("click_info_map_1")
          )
          
        ),      
      ),
      
      tags$div(
        tags$style(HTML("
         #variable_choice_map_2 + .selectize-control .selectize-dropdown {
          bottom: 100% !important;
          top: auto !important;
        }
      ")),
        class = "control-group",
        tags$div(class = "control-title", "Map 2 Controls", style="color: darkblue"), 
        
        
        selectizeInput("comparison_country_search_map_2", "Search Country:",
                       choices = NULL, selected = NULL,
                       options = list(placeholder = "Search for a country...", maxItems = 1, create = FALSE)),
        #selectInput("indicator_category", "Theme:", 
        #            choices = composite_choices, selected = "Weak Governance"),
        selectInput("indicator_category_map_2", "Theme:", #Composite Score:
                    choices = composite_choices, selected = "Social Inequality"),
        
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Socio-Ecological Vulnerability'",
          selectInput("composite_choice_map_2", "Composite Score:",
                      choices = names(composite_data_options), selected = "Inequity")
        ),        
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Social Inequality' || input.indicator_category_map_2 == 'Weak Governance' || (input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'Inequity')",
          selectInput("variable_choice_map_2", "Variable:", choices = NULL),
        ),   
        
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'ND GAIN'",
          
          # tags$div(
          #   class = "control-group",
          selectInput(inputId = "variable_nd_map_2",
                      label = "Choose a variable/indicator:",
                      choices = gainVars,
                      selected = "Value..gain"),
          
          sliderInput(inputId = "nd_year_map_2",
                      label = "Choose a year:",
                      min = 1995,
                      max = 2022,
                      value = 1995,
                      sep = "",
                      animate = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.indicator_category_map_2 == 'Socio-Ecological Vulnerability' && input.composite_choice_map_2 == 'Climate Risk'",
          
          selectInput(
            "climate_variable_map_2", 
            "Select Climate Variable:", 
            choices = names(climate_data_options)
          ),
          
          uiOutput("data_type_selector_map_2"),
          uiOutput("time_period_selector_map_2"),
          
          checkboxInput(
            "use_masked_raster_map_2",
            label = "Show only Exclusive Economic Zones (EEZs)",
            value = FALSE
          ),
          
          tags$div(class = "control-title", "Data Filters"),
          
          uiOutput("value_range_slider_map_2"),
          uiOutput("manual_min_input_map_2"),
          uiOutput("manual_max_input_map_2"),
          
          radioButtons(
            "filter_mode_map_2", 
            "Filter Mode:",
            choices = list(
              "Show All Data" = "none",
              "Show Values in Range" = "range",
              "Show Above Threshold" = "above", 
              "Show Below Threshold" = "below"
            ),
            selected = "none"
          ),
          
          actionButton(
            "reset_filters_map_2", 
            "Reset Filters", 
            class = "btn-outline-secondary btn-sm",
            style = "margin-top: 10px;"
          ),
          
          tags$div(class = "control-title", "Data Summary"),
          
          div(
            style = "font-family: Arial, sans-serif;",
            verbatimTextOutput("data_info_map_2")
          ),
          
          verbatimTextOutput("click_info_map_2")
        )
        
        
        
        
        
      ),
      # 
      # tags$div(
      #   class = "control-group",
      #   tags$p("Note: Comparison maps will appear as overlays when this mode is active.", 
      #          style = "font-size: 12px; color: #6c757d; font-style: italic;")
      # )
    ),
    
    tags$style(HTML("
 .data-tree {
    font-family: 'Inter', sans-serif;
    line-height: 1.4;
    margin: 10px 0;
  }
  
  .data-overview-content {
    max-height: none !important;
    overflow: visible !important;
  }
  
  .tree-node {
    margin: 4px 0;
    padding-left: 0;
  }
  
  .category-level-1 {
    margin-bottom: 16px;
  }
  
  .category-level-1 > .node-label {
    background: linear-gradient(135deg, #1e3a8a, #1e40af);
    color: white;
    padding: 10px 14px;
    border-radius: 8px;
    font-weight: 700;
    font-size: 14px;
    margin-bottom: 8px;
    box-shadow: 0 2px 8px rgba(30, 58, 138, 0.3);
  }
  
  .category-description {
    color: #6b7280;
    font-size: 11px;
    font-style: italic;
    margin: 4px 0 12px 0;
    padding: 8px 12px;
    background: rgba(248, 249, 250, 0.8);
    border-radius: 4px;
    border-left: 3px solid rgba(30, 58, 138, 0.3);
  }
  
  .category-level-2 > .node-label {
    background: rgba(30, 58, 138, 0.1);
    color: #1e3a8a;
    padding: 8px 12px;
    border-radius: 6px;
    font-weight: 600;
    font-size: 13px;
    margin: 6px 0 6px 20px;
    border-left: 3px solid #1e3a8a;
  }
  
  .variable-node {
    margin-left: 40px;
    padding: 8px 12px;
    background: rgba(248, 249, 250, 0.5);
    border-radius: 4px;
    margin-bottom: 6px;
    border-bottom: 1px solid rgba(0, 0, 0, 0.04);
  }
  
  .variable-label {
    color: #1e3a8a;
    font-weight: 600;
    font-size: 12px;
    margin-bottom: 4px;
    display: block;
  }
  
  .variable-description {
    color: #4b5563;
    font-size: 11px;
    line-height: 1.3;
    margin-bottom: 6px;
    font-style: italic;
  }
  
  .variable-code {
    color: #6b7280;
    font-family: 'JetBrains Mono', monospace;
    font-size: 10px;
    background: rgba(0, 0, 0, 0.04);
    padding: 2px 6px;
    border-radius: 3px;
    display: inline-block;
    margin-top: 2px;
    margin-right: 8px;
  }
  
  .data-source-link {
    display: inline-block;
    color: #1e3a8a;
    font-size: 10px;
    font-weight: 500;
    text-decoration: none;
    background: rgba(30, 58, 138, 0.1);
    padding: 2px 6px;
    border-radius: 3px;
    margin-top: 2px;
    transition: all 0.2s ease;
  }
  
  .data-source-link:hover {
    background: rgba(30, 58, 138, 0.2);
    text-decoration: none;
    color: #1e3a8a;
    transform: translateY(-1px);
  }
  
  .node-children {
    margin-top: 8px;
  }
  
  .category-level-1 .node-children {
    border-left: 2px solid rgba(30, 58, 138, 0.2);
    margin-left: 10px;
    padding-left: 10px;
  }
")),
    
    # Then add this section right before your about section:
    tags$div(
      class = "panel-section",
      id = "data-overview-section",
      
      tags$div(class = "section-header",
               tags$i(class = "fas fa-sitemap"),
               "Data Overview"),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Project Data Structure"),
        tags$p("This visualization shows how all variables and indicators in the High-Stakes Coastal Mapper are organized and categorized, with descriptions and data source links.",
               style = "font-size: 12px; color: #64748b; line-height: 1.4; margin-bottom: 15px;")
      ),
      
      tags$div(
        class = "control-group data-overview-content",
        
        tags$div(class = "data-tree",
                 # Socio-Ecological Vulnerability
                 tags$div(class = "tree-node category-level-1",
                          tags$div(class = "node-label", "Socio-Ecological Vulnerability"),
                          tags$div(class = "category-description", "Measures coastal communities' exposure to damaged marine environments, including threats to sea life, reliance on ocean-based food and jobs, and vulnerability to rising sea levels."),
                          tags$div(class = "node-children",
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Composite Score"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Socio-Ecological Vulnerability (Composite)"),
                                                              tags$div(class = "variable-description", "Overall composite ranking combining all vulnerability components into a single score for coastal regions."),
                                                              tags$div(class = "variable-code", "vulnerab.score.rank"),
                                                              tags$a(class = "data-source-link", href = "https://example.com/data-methodology", target = "_blank", "Methodology")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Components"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Degraded Ecosystems"),
                                                              tags$div(class = "variable-description", "Measures the extent of environmental degradation in coastal marine ecosystems, including coral bleaching, pollution, and biodiversity loss."),
                                                              tags$div(class = "variable-code", "mean.count.grav.V2.log.sc"),
                                                              tags$a(class = "data-source-link", href = "https://www.worldbank.org/en/topic/environment", target = "_blank", "World Bank Environmental Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Relative Deprivation Index"),
                                                              tags$div(class = "variable-description", "Composite measure of poverty and social deprivation including access to healthcare, education, and basic services in coastal communities."),
                                                              tags$div(class = "variable-code", "povmap.grdi.v1.sc"),
                                                              tags$a(class = "data-source-link", href = "https://www.worldbank.org/en/topic/poverty", target = "_blank", "World Bank Poverty Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Coastal Climate Vulnerability"),
                                                              tags$div(class = "variable-description", "Percentage of population living in low-elevation coastal zones vulnerable to sea level rise and extreme weather events."),
                                                              tags$div(class = "variable-code", "perc.pop.world.coastal.merit.10m.log.sc"),
                                                              tags$a(class = "data-source-link", href = "https://www.climatecentral.org/", target = "_blank", "Climate Central")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Nutritional Dependence"),
                                                              tags$div(class = "variable-description", "Reliance on marine resources for protein and nutritional security, particularly fish and seafood consumption per capita."),
                                                              tags$div(class = "variable-code", "Nutritional.dependence.sc"),
                                                              tags$a(class = "data-source-link", href = "https://www.fao.org/fishery/en", target = "_blank", "FAO Fisheries Data")
                                                     )
                                            )
                                   )
                          )
                 ),
                 
                 # Social Inequality
                 tags$div(class = "tree-node category-level-1",
                          tags$div(class = "node-label", "Social Inequality"),
                          tags$div(class = "category-description", "Measures economic and social disparities through gender wage gaps, income distribution differences, and unequal health outcomes across different population groups."),
                          tags$div(class = "node-children",
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Composite Score"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Social Inequality (Composite)"),
                                                              tags$div(class = "variable-description", "Overall ranking combining gender, income, and health inequality metrics into a single composite measure."),
                                                              tags$div(class = "variable-code", "ineq.score.rank"),
                                                              tags$a(class = "data-source-link", href = "https://example.com/inequality-methodology", target = "_blank", "Methodology")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Components"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Gender Inequality"),
                                                              tags$div(class = "variable-description", "Measures disparities between men and women in areas of reproductive health, empowerment, and labor market participation."),
                                                              tags$div(class = "variable-code", "gender.ineq.sc"),
                                                              tags$a(class = "data-source-link", href = "https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index", target = "_blank", "UNDP Gender Index")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Income Inequality"),
                                                              tags$div(class = "variable-description", "Gini coefficient and other measures of income distribution disparities within countries and regions."),
                                                              tags$div(class = "variable-code", "income.ineq.sc"),
                                                              tags$a(class = "data-source-link", href = "https://data.worldbank.org/indicator/SI.POV.GINI", target = "_blank", "World Bank Gini Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Inequality Adjusted Life Expectancy"),
                                                              tags$div(class = "variable-description", "Life expectancy adjusted for inequality in distribution of expected years of life across the population."),
                                                              tags$div(class = "variable-code", "le.ineq.log.sc"),
                                                              tags$a(class = "data-source-link", href = "https://hdr.undp.org/", target = "_blank", "UNDP Human Development")
                                                     )
                                            )
                                   )
                          )
                 ),
                 
                 # Weak Governance
                 tags$div(class = "tree-node category-level-1",
                          tags$div(class = "node-label", "Weak Governance"),
                          tags$div(class = "category-description", "Measures how well governments function through public service quality, business regulation effectiveness, law enforcement, corruption prevention, political stability, and citizen participation."),
                          tags$div(class = "node-children",
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Composite Score"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Weak Governance (Composite)"),
                                                              tags$div(class = "variable-description", "Overall ranking combining all governance indicators from the World Bank Worldwide Governance Indicators project."),
                                                              tags$div(class = "variable-code", "gov.score.rank"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", "World Bank WGI")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Components"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Government Ineffectiveness"),
                                                              tags$div(class = "variable-description", "Quality of public services, civil service, policy formulation and implementation, and government commitment to policies."),
                                                              tags$div(class = "variable-code", "Gov_effect.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", "WGI Government Effectiveness")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Poor Regulatory Quality"),
                                                              tags$div(class = "variable-description", "Government's ability to formulate and implement sound policies and regulations that promote private sector development."),
                                                              tags$div(class = "variable-code", "Reg_quality.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", "WGI Regulatory Quality")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Weak Rule of Law"),
                                                              tags$div(class = "variable-description", "Confidence in and abiding by rules of society, including contract enforcement, property rights, police, and courts."),
                                                              tags$div(class = "variable-code", "Rule_law.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", "WGI Rule of Law")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Weak Control of Corruption"),
                                                              tags$div(class = "variable-description", "Extent to which public power is exercised for private gain, including petty and grand forms of corruption."),
                                                              tags$div(class = "variable-code", "control_corr.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", "WGI Control of Corruption")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Low Voice and Accountability"),
                                                              tags$div(class = "variable-description", "Citizens' ability to participate in selecting their government, freedom of expression, association, and free media."),
                                                              tags$div(class = "variable-code", "Voice_account.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", " WGI Voice & Accountability")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Political Instability"),
                                                              tags$div(class = "variable-description", "Likelihood of political instability and/or politically motivated violence, including terrorism."),
                                                              tags$div(class = "variable-code", "Political_stab.sc"),
                                                              tags$a(class = "data-source-link", href = "https://info.worldbank.org/governance/wgi/", target = "_blank", " WGI Political Stability")
                                                     )
                                            )
                                   )
                          )
                 ),
                 
                 # Climate Risk Data
                 tags$div(class = "tree-node category-level-1",
                          tags$div(class = "node-label", "Climate Risk Data"),
                          tags$div(class = "category-description", "Comprehensive climate projections and current conditions from IPCC models and NOAA monitoring systems, covering ocean chemistry, sea level, temperature, and coral bleaching."),
                          tags$div(class = "node-children",
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Ocean pH Data"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Ocean Acidification Projections"),
                                                              tags$div(class = "variable-description", "pH change and absolute values for near-term (2021-2040), medium-term (2041-2060), and long-term (2081-2100) periods."),
                                                              tags$div(class = "variable-code", "IPCC Interactive Atlas"),
                                                              tags$a(class = "data-source-link", href = "https://interactive-atlas.ipcc.ch/", target = "_blank", "IPCC Interactive Atlas")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Sea Level Rise"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Sea Level Change Projections"),
                                                              tags$div(class = "variable-description", "Projected change in sea level relative to 1995-2014 baseline for three time periods under various emission scenarios."),
                                                              tags$div(class = "variable-code", "IPCC Interactive Atlas"),
                                                              tags$a(class = "data-source-link", href = "https://interactive-atlas.ipcc.ch/", target = "_blank", "IPCC Sea Level Data")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Temperature Data"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Heating Degree Days"),
                                                              tags$div(class = "variable-description", "Temperature-based metrics for energy demand, showing both absolute values and changes from historical baseline."),
                                                              tags$div(class = "variable-code", "IPCC Interactive Atlas"),
                                                              tags$a(class = "data-source-link", href = "https://interactive-atlas.ipcc.ch/", target = "_blank", " IPCC Temperature Data")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Coral Bleaching"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Degree Heating Weeks"),
                                                              tags$div(class = "variable-description", "Real-time coral bleaching thermal stress monitoring with daily updates showing accumulated heat stress over 12-week periods."),
                                                              tags$div(class = "variable-code", "NOAA Coral Reef Watch"),
                                                              tags$a(class = "data-source-link", href = "https://coralreefwatch.noaa.gov/", target = "_blank", "NOAA Coral Watch")
                                                     )
                                            )
                                   )
                          )
                 ),
                 
                 # ND-GAIN Indicators
                 tags$div(class = "tree-node category-level-1",
                          tags$div(class = "node-label", "ND-GAIN Indicators"),
                          tags$div(class = "category-description", "Notre Dame Global Adaptation Initiative indicators measuring countries' vulnerability to climate disruptions and readiness to leverage investment for adaptive actions (1995-2022)."),
                          tags$div(class = "node-children",
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Overall Score"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "GAIN Index"),
                                                              tags$div(class = "variable-description", "Overall country ranking combining vulnerability and readiness scores to measure climate adaptation capacity."),
                                                              tags$div(class = "variable-code", "Value..gain"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/", target = "_blank", "ND-GAIN Country Index")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Vulnerability Indicators"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Ecosystem Vulnerability"),
                                                              tags$div(class = "variable-description", "Vulnerability of ecosystem services to climate change impacts including biodiversity loss and habitat degradation."),
                                                              tags$div(class = "variable-code", "Value..ecosystems"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", "ND-GAIN Ecosystem Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Food Vulnerability"),
                                                              tags$div(class = "variable-description", "Vulnerability of food system to climate change including agricultural productivity and food security risks."),
                                                              tags$div(class = "variable-code", "Value..food"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", "ND-GAIN Food Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Health Vulnerability"),
                                                              tags$div(class = "variable-description", "Vulnerability of human health to climate change impacts including heat stress, disease vectors, and healthcare capacity."),
                                                              tags$div(class = "variable-code", "Value..health"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", "ND-GAIN Health Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Infrastructure Vulnerability"),
                                                              tags$div(class = "variable-description", "Vulnerability of critical infrastructure to climate change including transportation, energy, and communication systems."),
                                                              tags$div(class = "variable-code", "Value..infrastructure"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", " ND-GAIN Infrastructure Data")
                                                     )
                                            )
                                   ),
                                   tags$div(class = "tree-node category-level-2",
                                            tags$div(class = "node-label", "Readiness Indicators"),
                                            tags$div(class = "node-children",
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Economic Readiness"),
                                                              tags$div(class = "variable-description", "Economic capacity to implement adaptation measures including investment capability and economic stability."),
                                                              tags$div(class = "variable-code", "Value..economic"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", "ND-GAIN Economic Data")
                                                     ),
                                                     tags$div(class = "tree-node variable-node",
                                                              tags$div(class = "variable-label", "Governance Readiness"),
                                                              tags$div(class = "variable-description", "Institutional and governance capacity to implement adaptation strategies including policy effectiveness and rule of law."),
                                                              tags$div(class = "variable-code", "Value..governance"),
                                                              tags$a(class = "data-source-link", href = "https://gain.nd.edu/our-work/country-index/download-data/", target = "_blank", " ND-GAIN Governance Data")
                                                     )
                                            )
                                   )
                          )
                 )
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Data Sources Summary"),
        tags$div(
          style = "font-size: 11px; line-height: 1.4; color: #64748b;",
          tags$p(tags$strong("Inequity Indicators:"), " Custom composite scores based on ", 
                 tags$a(href = "https://data.worldbank.org/", target = "_blank", style = "color: #1e3a8a;", "World Bank"), ", ",
                 tags$a(href = "https://www.un.org/en/", target = "_blank", style = "color: #1e3a8a;", "UN agencies"), ", and academic research data"),
          tags$p(tags$strong("Climate Risk Data:"), " ", 
                 tags$a(href = "https://interactive-atlas.ipcc.ch/", target = "_blank", style = "color: #1e3a8a;", "IPCC Interactive Atlas"), " and ",
                 tags$a(href = "https://coralreefwatch.noaa.gov/", target = "_blank", style = "color: #1e3a8a;", "NOAA Coral Reef Watch")),
          tags$p(tags$strong("ND-GAIN:"), " ", 
                 tags$a(href = "https://gain.nd.edu/", target = "_blank", style = "color: #1e3a8a;", "Notre Dame Global Adaptation Initiative"), " Country Index (1995-2022)"),
          tags$p(tags$strong("Geographic Coverage:"), " 114+ coastal countries worldwide with sub-national data for selected regions")
        )
      )
    ),
    
    tags$div(
      class = "panel-section",
      id = "about-section",
      
      tags$div(class = "section-header",
               "About"),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Project Overview"),
        tags$p("The High-Stakes Coastal Mapper is a global coastal social-environmental atlas: a high-resolution, interactive data platform that serves as a comprehensive hub of social, economic, demographic, and environmental data relevant to climate and ocean science and policy.", 
               style = "font-size: 14px; line-height: 1.6; margin-bottom: 15px;"),
        tags$p("This platform addresses critical knowledge gaps in understanding coastal vulnerabilities by integrating multiple data sources to visualize socio-ecological vulnerability, social inequality, and governance effectiveness across coastal regions worldwide.",
               style = "font-size: 14px; line-height: 1.6; margin-bottom: 15px;")
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Data+ Program 2025"),
        tags$p("This project is part of Duke University's Data+ program for 2025, an intensive 10-week collaborative research experience that brings together faculty, graduate students, and undergraduates to tackle real-world data science challenges.",
               style = "font-size: 14px; line-height: 1.6; margin-bottom: 10px;"),
        tags$p("The Data+ program (including Climate+ projects) provides students with hands-on experience in data analysis, visualization, and scientific computing while addressing pressing societal issues through interdisciplinary collaboration.",
               style = "font-size: 14px; line-height: 1.6; margin-bottom: 15px;")
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Research Team"),
        
        tags$div(
          style = "margin-bottom: 20px;",
          tags$div(
            style = "margin-bottom: 15px;",
            tags$h6("Faculty & Mentors", style = "font-weight: 600; margin-bottom: 10px; color: #2c3e50;"),
            tags$div(
              class = "team-member",
              tags$img(
                src = "davidgill.png",
                alt = "David Gill",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0;"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("David Gill"), " - Faculty Lead", style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("Ocean Synthesis Lab", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("david.gill@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            ),
            tags$div(
              class = "team-member",
              tags$img(
                src = "sameerswarup.png",
                alt = "Sameer Swarup",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0;"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("Sameer Swarup"), " - Mentor", style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("Ocean Synthesis Lab", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("sameer.swarup@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            )
          )
        ),
        
        tags$div(
          style = "margin-bottom: 15px;",
          tags$h6("Student Researchers", style = "font-weight: 600; margin-bottom: 10px; color: #2c3e50;"),
          tags$div(
            style = "display: flex; flex-direction: column; gap: 12px;",
            
            tags$a(
              style = "display: flex; align-items: center; gap: 12px; padding: 10px; background: rgba(248, 249, 250, 0.8); border-radius: 8px; border: 1px solid rgba(0, 0, 0, 0.05); transition: all 0.3s ease; cursor: pointer; text-decoration: none; color: inherit;",
              onmouseover = "this.style.transform = 'translateY(-2px)'; this.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.1)'; this.style.background = 'rgba(13, 110, 253, 0.05)';",
              onmouseout = "this.style.transform = 'translateY(0)'; this.style.boxShadow = 'none'; this.style.background = 'rgba(248, 249, 250, 0.8)';",
              href = "https://github.com/ethanchou",
              target = "_blank",
              tags$img(
                src = "ethancho.png",
                alt = "Ethan Cho",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0; transition: all 0.3s ease;",
                onmouseover = "this.style.transform = 'scale(1.05)'; this.style.borderColor = '#0b5ed7';",
                onmouseout = "this.style.transform = 'scale(1)'; this.style.borderColor = '#0d6efd';"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("Ethan Cho"), style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("CS & Environmental Science", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("ethan.cho@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            ),
            
            tags$a(
              style = "display: flex; align-items: center; gap: 12px; padding: 10px; background: rgba(248, 249, 250, 0.8); border-radius: 8px; border: 1px solid rgba(0, 0, 0, 0.05); transition: all 0.3s ease; cursor: pointer; text-decoration: none; color: inherit;",
              onmouseover = "this.style.transform = 'translateY(-2px)'; this.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.1)'; this.style.background = 'rgba(13, 110, 253, 0.05)';",
              onmouseout = "this.style.transform = 'translateY(0)'; this.style.boxShadow = 'none'; this.style.background = 'rgba(248, 249, 250, 0.8)';",
              href = "https://github.com/megankolenski",
              target = "_blank",
              tags$img(
                src = "megankolenski.png",
                alt = "Megan Kolenski",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0; transition: all 0.3s ease;",
                onmouseover = "this.style.transform = 'scale(1.05)'; this.style.borderColor = '#0b5ed7';",
                onmouseout = "this.style.transform = 'scale(1)'; this.style.borderColor = '#0d6efd';"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("Megan Kolenski"), style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("Statistics", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("megan.kolenski@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            ),
            
            tags$a(
              style = "display: flex; align-items: center; gap: 12px; padding: 10px; background: rgba(248, 249, 250, 0.8); border-radius: 8px; border: 1px solid rgba(0, 0, 0, 0.05); transition: all 0.3s ease; cursor: pointer; text-decoration: none; color: inherit;",
              onmouseover = "this.style.transform = 'translateY(-2px)'; this.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.1)'; this.style.background = 'rgba(13, 110, 253, 0.05)';",
              onmouseout = "this.style.transform = 'translateY(0)'; this.style.boxShadow = 'none'; this.style.background = 'rgba(248, 249, 250, 0.8)';",
              href = "https://github.com/abhinavmeduri",
              target = "_blank",
              tags$img(
                src = "abhinavmeduri.png",
                alt = "Abhinav Meduri",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0; transition: all 0.3s ease;",
                onmouseover = "this.style.transform = 'scale(1.05)'; this.style.borderColor = '#0b5ed7';",
                onmouseout = "this.style.transform = 'scale(1)'; this.style.borderColor = '#0d6efd';"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("Abhinav Meduri"), style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("CS & Statistics", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("abhinav.meduri@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            ),
            
            tags$a(
              style = "display: flex; align-items: center; gap: 12px; padding: 10px; background: rgba(248, 249, 250, 0.8); border-radius: 8px; border: 1px solid rgba(0, 0, 0, 0.05); transition: all 0.3s ease; cursor: pointer; text-decoration: none; color: inherit;",
              onmouseover = "this.style.transform = 'translateY(-2px)'; this.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.1)'; this.style.background = 'rgba(13, 110, 253, 0.05)';",
              onmouseout = "this.style.transform = 'translateY(0)'; this.style.boxShadow = 'none'; this.style.background = 'rgba(248, 249, 250, 0.8)';",
              href = "https://github.com/leonardeshun",
              target = "_blank",
              tags$img(
                src = "leonardeshun.jpg",
                alt = "Leonard Eshun",
                style = "width: 45px; height: 45px; border-radius: 50%; object-fit: cover; border: 2px solid #0d6efd; flex-shrink: 0; transition: all 0.3s ease;",
                onmouseover = "this.style.transform = 'scale(1.05)'; this.style.borderColor = '#0b5ed7';",
                onmouseout = "this.style.transform = 'scale(1)'; this.style.borderColor = '#0d6efd';"
              ),
              tags$div(
                style = "flex: 1; min-width: 0;",
                tags$div(tags$strong("Leonard Eshun"), style = "font-size: 13px; font-weight: 600; color: #2c3e50; margin-bottom: 2px; line-height: 1.2;"),
                tags$div("Data Science Graduate", style = "font-size: 11px; line-height: 1.3; color: #6c757d;"),
                tags$div("leonard.eshun@duke.edu", style = "font-size: 10px; color: #0d6efd; margin-top: 2px; font-style: italic;")
              )
            )
          )
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Data Sources"),
        tags$div(
          style = "font-size: 14px; line-height: 1.6;",
          tags$p(tags$strong("Climate Data:"), " IPCC Interactive Atlas for ocean pH, sea level rise, and temperature projections"),
          tags$p(tags$strong("Coral Bleaching:"), " NOAA Coral Reef Watch for real-time thermal stress monitoring"),
          tags$p(tags$strong("Adaptation Metrics:"), " Notre Dame Global Adaptation Initiative (ND-GAIN) Country Index"),
          tags$p(tags$strong("Socio-economic Indicators:"), " World Bank, UN agencies, and other authoritative sources for governance, inequality, and development metrics")
        )
      ),
      
      tags$div(
        class = "control-group",
        tags$div(class = "control-title", "Contact & Support"),
        tags$div(
          style = "font-size: 12px; line-height: 1.4; color: #6c757d;",
          tags$p("For questions about this platform or research collaboration opportunities, contact the Ocean Synthesis Lab at Duke University or reach out to any team member directly.")
        )
      )
    )
  ),
  
  tags$script(HTML("
    $(document).ready(function() {
    
      // Hamburger menu click handler
      $('#hamburger-menu').click(function(e) {
        e.stopPropagation();
        $('#menu-dropdown').toggleClass('show');
      });

      // Close menu when clicking outside or when mouse leaves the entire menu area
      $(document).on('click', function(e) {
        if (!$(e.target).closest('#hamburger-menu').length) {
          $('#menu-dropdown').removeClass('show');
        }
      });
      
      // Close menu when mouse leaves the hamburger menu container
      $('#hamburger-menu').mouseleave(function() {
        setTimeout(function() {
          if (!$('#menu-dropdown:hover').length && !$('#hamburger-menu:hover').length) {
            $('#menu-dropdown').removeClass('show');
          }
        }, 200); // Small delay to allow mouse to move to dropdown
      });
      
      // Close menu when mouse leaves the dropdown
      $('#menu-dropdown').mouseleave(function() {
        $('#menu-dropdown').removeClass('show');
      });

      // Menu item click handler
      $('.menu-item').click(function(e) {
        e.preventDefault();

        $('.menu-item').removeClass('active');
        $('.panel-section').removeClass('active');

        $(this).addClass('active');

        var section = $(this).data('section');
        $('#' + section + '-section').addClass('active');

        // Automatically close the menu after selection
        $('#menu-dropdown').removeClass('show');

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

        // Re-initialize search functionality when switching to graphs section
        if (section === 'graphs') {
          setTimeout(function() {
            setupCountrySearch('country_search_graphs', 'country_suggestions_graphs');
          }, 100);
        }
        
        Shiny.setInputValue('active_panel', section, {priority: 'event'});

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
# **Mapping High-Stakes Coastal Zones Technical Document**

This repository contains the R code for the processing of several coastal datasets, as well as the back-end R Shiny code for the high-stakes coastal mapper interactive dashboard. The following primary datasets comprise the main data displayed in the dashboard:

-   **Contextual Inequity**: Comprises 14 variables within 3 main components: Social-Ecological Vulnerability, Weak Governance, and Social Inequality.

-   **ND GAIN**: Notre Dame Global Adaptation Initiative Country Index (ND GAIN) that assesses 182 UN countries’ climate vulnerability and readiness, with 40 scores between 1995 to 2022.

-   **IPCC**: Intergovernmental Panel on Climate Change’s (IPCC) WGI Atlas CMIP6 SSP5-8.5 model for climate risk projections (pH, sea level rise, heating degree days) across near-term, medium- term, and long-term time horizons.

-   **NOAA**: National Oceanic and Atmospheric Administration’s (NOAA) daily Degree Heating Weeks data for coral reefs during the month of April.

## **Citation**

The datasets were collected from the following sources:

-   Chen, C. et al. (2015), University of Notre Dame Global Adaptation Initiative Country Index Technical Report . University of Notre Dame. <https://gain.nd.edu/assets/581554/nd_gain_countryindex_technicalreport_2024.pdf> 

-   Gill, D.A., D’Agata, S., Blythe, J.L., Claudet, J., Ban, N.C.,  Annasawmy, P., Bennett, N., Di Franco, A., Epstein, G., Evans, L., et al. 2025. Investing smarter and deeper to advance equity in high-stakes coastal locations, in review

-   Iturbide, M., et al. (2021). IPCC‑WGI AR6 Interactive Atlas Monthly Dataset: Aggregated Regionally [Data set]. Instituto de Física de Cantabria. <https://doi.org/10.5281/zenodo.5171760>

-   National Oceanic and Atmospheric Administration (NOAA). (n.d.). NOAA Coral Reef Watch Homepage and Near Real-Time Products Portal. <https://coralreefwatch.noaa.gov/>

## **R Packages Required**

-   shiny, shinyjs, leaflet, and bslib to develop the UI of the dashboard. shiny was used for building the dashboard, bslib for customizing features, and leaflet for displaying the four different datasets on a global map.

-   ggplot2, ggthemes, and plotly were used for the various data visualizations displayed throughout the dashboard.

-   tidyverse, pryr, and qs were used for handling the various datasets (in CSV and RDS formats), including accessing, cleaning, filtering, and mutating data frames.

-   terra, sf, and rnaturalearth were used for processing and displaying spatial data, including shape files and raster files. rnaturalearth was specifically used for its built-in world coastlines and country shape polygon datasets.

-   viridis provided different color palettes used in legends, maps, and plots.

## **Data Pre-Processing Code**

**Notre Dame Global Adaptation Initiative Country Index**

-   The **NDGAINpreprocessing.RMD** code was used to compile CSV files containing data for different variables that comprise the Notre Dame Global Adaptation Initiative Country Index. The vulnerability and readiness indicators were originally each stored in different files, with data for 182 United Nations countries for all years between 1995 and 2022.

-   The **mergeNDandInequity.RMD** code was used to map the time-series ND GAIN data to each of the 800,000+ points filtered in the contextual inequity dataset. While doing this would still cause every point in a country to have the same ND GAIN score, this allows for a point-level resolution for this dataset as well. This markdown file was also used to create a dataset with regional averages for all scores.

**Intergovernmental Panel on Climate Change**

-   The **IPCC-EEZ-filter.RMD** script filters GeoTIFF raster data from the IPCC WGI Interactive Atlas to retain only the pixel values that fall within global Exclusive Economic Zones (EEZs), as defined by the International Hydrographic Organization’s[ World EEZ v12](https://www.google.com/search?q=iho+world+eez&sca_esv=397bf5706f45ee43&sxsrf=AE3TifPPtJIQ7_bTM4mDIMB2h6SVg4WaRA%3A1753196356770&ei=RKd_aM7dLoqf5NoPweTR8Q4&ved=0ahUKEwjOj8Sw3dCOAxWKD1kFHUFyNO4Q4dUDCBA&uact=5&oq=iho+world+eez&gs_lp=Egxnd3Mtd2l6LXNlcnAiDWlobyB3b3JsZCBlZXoyBRAhGKABMgUQIRigATIFECEYoAEyBRAhGKABMgUQIRigATIFECEYnwVIvBtQrANY8hpwAXgEkAEBmAGOAqAB2AuqAQU2LjYuMbgBA8gBAPgBAZgCEKACwAvCAgQQABhHwgIEECMYJ8ICChAjGIAEGCcYigXCAg0QIxjwBRiABBgnGIoFwgILEAAYgAQYkQIYigXCAgoQABiABBhDGIoFwgIQEC4YgAQYQxjHARiKBRivAcICDRAuGIAEGEMY1AIYigXCAgsQABiABBixAxiDAcICCBAAGIAEGLEDwgIREC4YgAQYsQMY0QMYgwEYxwHCAhEQLhiABBiSAxjHARiOBRivAcICCxAAGIAEGLEDGMkDwgILEAAYgAQYkgMYigXCAgoQABiABBixAxgKwgIQEC4YgAQY0QMYFBiHAhjHAcICBRAAGIAEwgIKEAAYgAQYFBiHAsICChAAGIAEGMkDGArCAggQABgWGAoYHsICBhAAGBYYHsICCxAAGIAEGIYDGIoFwgIIEAAYgAQYogTCAgUQIRirApgDAIgGAZAGCJIHBTkuNi4xoAfTa7IHBTUuNi4xuAe1C8IHBjAuMTEuNcgHJw&sclient=gws-wiz-serp) shapefile (released 2023-10-25, 122 MB). To perform the masking, the EEZ shapefile is first reprojected to match the coordinate reference system (CRS) of each raster. The raster is then cropped and masked using the reprojected EEZ boundaries. If desired, the final output raster can be extended to a global bounding box of -180 to 180 longitude and -90 to 90 latitude to ensure consistent spatial extent across all outputs.

**National Oceanic and Atmospheric Administration**

-   We used the Coral Reef Watch Satellite Monitoring data from NOAA and focused on visualizing the Coral Bleaching Data. The code for this is located in the **IPCC-EEZ-filter.RMD.** The files are originally in NetCDF file formats and we convert them to GeoTiff using the processing code located at “Preprocessing/Convert_NC_to_TIFF.rmd”.

## **Dashboard Workflow**

1.  First, it’s important to understand the structure of the data, specifically whether it's organized by country or some other format, as well as whether the data is just a data frame, is a shape file, or is a raster file. The contextual inequity data and ND GAIN data both have columns that classify which country a data point is part of, so if datasets have country data then it will be easier to display on a point-level resolution on the interactive dashboard.

2.  Next, it is important to process the data. If the data is national-level, filter the countries to only include coastal countries. If the data has a sub-national resolution, filter the points to only include those that are coastal. This dashboard classified coastal regions as those containing a point within 5 km of the coast. 5 km works, but the threshold can also be increased to 10 km.

3.  The contextual inequity data is the only dataset in this dashboard that is inherently point-level. While the ND GAIN data is national-level and only has one score for the entire country, the data was joined with the 800,000+ points in the inequity dataset so that each geometric point within a country also has assigned ND GAIN scores. This was simply done by joining the ND GAIN scores with the 800,000+ points according to which country that point is in. While this means that displaying all this point-level data would show the same color/score across the entire country for national-level indicators like ND GAIN, this ensures that the resolution is consistent across all scores.

4.  It is also important to understand which overarching theme each dataset falls under. As the contextual inequity dataset was used as the base dataset of the dashboard, the three main themes were chosen based on the three components of contextual inequity: Weak Governance, Socio-Ecological Vulnerability, and Social Inequality. Currently, ND GAIN and Climate Risk are classified as composite scores under Socio-Ecological Vulnerability, but if any new datasets are added, then it is important to understand which of the three themes the dataset is most related to. As of now, Weak Governance and Social Inequality only have data from the contextual inequity dataset, and thus, the indicators/variables are those from contextual inequity. A “Composite Score” dropdown can be added if more datasets outside of contextual inequity are added.

5.  The most important part of implementing this data into the dashboard is being able to display it on Leaflet. The addPolygons() function inside Leaflet can be used to display country polygons, while the addCircleMarkers() function can be used for displaying point-level data. As long as data points have geometries (whether that be a POINT for circle markers or a MULTIPOLYGON for country polygons), displaying on Leaflet should be very simple. The “Purples” color palette is currently being used for all country polygons and point-level data.

6.  The Country Analysis page contains all data that can be visualized in some type of graph. Country-level data can be displayed on the global scale scatterplot, and point-level data can be used for country-level and regional analysis. It is important to restructure the lists and vectors used for the various dropdowns throughout the Country Analysis page to implement new variables. As of now, there is also a slider for ND GAIN data so that users can choose which year of ND GAIN they would like to visualize in the global scale scatter plot. Regional averages for contextual inequity were calculated by averaging all data points with the same NAME_2 column value of the contextual inequity dataset. 

## **Dashboard Code & Modules**

The code for the dashboard created in R Shiny is split into three main sections:

-   **global.R:** Primarily used for loading all relevant R packages, processing datasets, and initializing variables used across all files in the dashboard. The main contextual inequity, ND GAIN, IPCC, and NOAA datasets are all loaded and assigned to variables in this file, and are further processed or filtered if necessary. All lists and vectors used in dropdowns, text boxes, and other components of the UI were also initialized in this file.

-   **ui.R:** Contains all the code for the front-end of the dashboard, including Shiny features like sliders, dropdown boxes, plot outputs, and text boxes. HTML, Javascript, and CSS script were also used for more advanced display features and to make a generally cleaner dashboard. The code for outputting the Leaflet maps is also included in this file, as well as conditional panels that change what is being displayed based on the chosen page or chosen dropdown options.

-   **server.R:** The server is essentially the hub for the back-end of the dashboard. This file contains the server function that has all of the instructions for building the dashboard. The server takes inputs from the UI (such as the selected dropdown options or an inputted text) and processes them to render some other component of the UI. Reactive values are objects used for reading and writing values, like a variable, but with special capabilities for reactive programming. When inputs change on the UI side, these reactive values also dynamically change within the server, and accordingly change the outputs. 

Due to the large amount of code, the server file was split into several modules in the **modules** folder. Each of these modules corresponds to a certain feature or page in the dashboard.

-   **countryAnalysisModule.R:** This file contains the code for the Country Analysis page, which allows users to create customizable graphs on a global, national, and regional level. Users can create global scatter plots and change the x and y axes to their global-level indicators of choice, and also see where a certain target country lies in respect to all other countries in terms of indicator scores. This file also contains the code for creating univariate and bivariate plots within a country (histograms and scatterplots), where each data point is one point in the country. Each renderPlot function dynamically filters the global dataset based on chosen countries, regions, values and years, and visualizes the filtered data frame using ggplot.  

-   **countryComparison.R:** This is the base backend code file for the comparison feature of the dashboard. It has the code for all the themes except for ND Gain and Climate Risk.  The comparison codes for ND Gain and Climate Risk have files with names ending in “\_map_1.R” or “\_map_2.R” as extended files. These files start out as copies of the main files with names climate_risk.R and nd_gain.R. The comparison backend file and the extended files are imported into the main backend file, server.R.

-   **climate_risk_map_1.R and climate_risk_map_2.R:** These server files support the “Country Comparison” section of the dashboard by managing two independent raster-based climate visualizations (“panels”) intended to be viewed side by side. Each file handles user inputs for selecting a climate variable, data type, and time period, and loads the corresponding GeoTIFF raster from the IPCC WGI Interactive Atlas or NOAA Coral Reef Watch (including the 5 km Degree Heating Week product). The rasters are processed to match a standard projection, optionally masked using EEZ boundaries, and smoothed for display. Users can apply value-based filters, view summary statistics, and click on the map to inspect specific pixel values. The two maps operate independently, allowing users to compare different variables or scenarios using a consistent interface and structure.

-   **climateRisk.R:** This file contains the server logic for displaying climate data from the IPCC WGI Interactive Atlas and NOAA Coral Reef Watch in the “Interactive Map” section of the dashboard. It handles the loading, filtering, and display of raster datasets such as ocean pH, sea level rise, heating degree days, and NOAA’s 5km daily Degree Heating Week (DHW) product for coral bleaching. Users can select variables, data types, and time periods, apply filters to raster values, and view summary statistics. The dashboard supports interactivity through map clicks, which return pixel-level values, and includes visualizations like histograms and boxplots. It also incorporates EEZ-filtered versions of the raster data to focus on coastal areas. 

-   **ndGAIN_map_1.R and ndGAIN_map_2.R:** These files contain the code for displaying the ND GAIN dataset on the Comparison Map page. It takes the ND GAIN data loaded in global.R and visualizes it on a Leaflet map, with the same zoom features and country-level/point-level resolution as the contextual inequity interactive maps. These files take in selected inputs from the Comparison Maps 1 & 2 sidebar variables and dynamically filters the data based on the chosen year and country. If no country is chosen for a specific map, it shows the Global data for that map.

-   **ndGAIN.R:** This file contains the code for displaying the ND GAIN dataset on the main Interactive Map page. It takes the ND GAIN data loaded in global.R and visualizes it on a Leaflet map, with the same zoom features and country-level/point-level resolution as the contextual inequity interactive maps. This server file takes in inputs from the ND GAIN sidebar and dynamically filters the data based on the chosen year and country. A graph for variation across time for an ND GAIN indicator within a country is also created using ggplot.

Miscellaneous files were placed in the **www** folder, including Duke University logos and profile pictures of members of the Mapping High Stakes Coastal Zone Data+/Climate+ team.

## **Acknowledgements**

Thank you to Dr. David Gill and Sameer Swarup from the Duke Ocean Synthesis Lab for

sponsoring this project and providing us with important insight, guidance, and feedback

throughout the entire program. This project was also supported by the Data+ and Climate+ team, most notably Gregory Herschlag, Ariel Dawn, Kyle Bradbury, and Katie Cloud. 

The conceptual framework and majority of underlying data for this project came from the following reference:	

Gill, D.A., D'Agata, S., Blythe, J.L., Claudet, J., Ban, N.C.,  Annasawmy, P., Bennett, N., Di Franco, A., Epstein, G., Evans, L., et al. 2025. Investing smarter and deeper to advance equity in high-stakes coastal locations, in review

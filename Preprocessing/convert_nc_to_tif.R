# Load required package
library(terra)

# Set directory containing .nc files
input_dir <- "LeonardEDA/nc_shiny_app/Coral_Bleaching_data/NC_Files"
output_dir <- "LeonardEDA/nc_shiny_app/Coral_Bleaching_data"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List all NetCDF files
nc_files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)

# Loop through each NetCDF file
for (nc_file in nc_files) {
  message("Checking: ", nc_file)
  
  # Load raster stack from NetCDF
  r <- tryCatch(rast(nc_file), error = function(e) NULL)
  
  if (!is.null(r)) {
    # Get available layer names
    layer_names <- names(r)
    print(paste("Layer names: ", layer_names))
    
    if ("degree_heating_week" %in% layer_names) {
      message("  -> Found 'degree_heating_week'")
      
      # Extract the layer
      dhw_layer <- r[["degree_heating_week"]]
      
      # Aggregate to LOWER resolution (e.g., 2x larger cells)
      r_coarse <- aggregate(dhw_layer, fact = 6)
      
      # Create output file name
      base_name <- tools::file_path_sans_ext(basename(nc_file))
      out_file <- file.path(output_dir, paste0(base_name, ".tif"))
      
      # Save as GeoTIFF
      writeRaster(r_coarse, out_file, overwrite = TRUE)
      message("  -> Saved: ", out_file)
    } else {
      message("  -> 'degree_heating_week' not found in this file.")
    }
  } else {
    message("  -> Failed to read file.")
  }
}

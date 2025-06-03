# Load required packages
library(tidyverse)
library(sf)
library(ggplot2)

# Read the data (update with your actual file path)
df <- read_csv("your_data.csv")  # Replace with your actual file name

# Filter and clean relevant columns
df_clean <- df %>%
  select(NAME1, NAME2, CENTROID_X, CENTROID_Y, TOTAL_A_KM, WATER_A_KM, LAND_A_KM) %>%
  mutate(
    CENTROID_X = as.numeric(CENTROID_X),
    CENTROID_Y = as.numeric(CENTROID_Y)
  ) %>%
  drop_na(CENTROID_X, CENTROID_Y)

# Convert to spatial points
points_sf <- st_as_sf(df_clean, coords = c("CENTROID_X", "CENTROID_Y"), crs = 4326)

# Plot with ggplot2
ggplot() +
  borders("world", colour = "gray80", fill = "gray90") +
  geom_sf(data = points_sf, aes(color = NAME1), size = 2) +
  theme_minimal() +
  labs(title = "Geographic Distribution of Dataset Regions",
       subtitle = "Plotted using centroid coordinates",
       x = "Longitude", y = "Latitude")

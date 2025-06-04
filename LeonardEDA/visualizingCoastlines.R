library(ggplot2)
library(maps)
library(sf)
library(dplyr)
library(leaflet)
library(raster)
library(data.table)

csv_folder <- "/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files"

csv_files <- list.files(path = csv_folder, pattern = "\\.csv$", full.names = TRUE)



worldData <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

worldData <- st_as_sf(worldData,
                      coords = c("CENTROID_X", "CENTROID_Y"),
                      crs = 4326)


df <- readRDS("/Users/student/Desktop/Data+/climate_plus_summer_2025/EthanDashboardCopy/shiny_app copy/data copy/df.cont.inequity.compo.coastal.scores_sr.rds") %>%
  st_transform(4326)


df <- dplyr::select(df, "name_en", "iso_a3", "mean.count.grav.V2.log.sc")

df_viet <- filter(df, iso_a3 == "VNM")
df_viet <- filter(df_viet, mean.count.grav.V2.log.sc > 0.4)

ggplot(df_viet) +
  geom_sf(data = world, fill = "#ADD8E6", color = "white") +
  geom_sf(aes(color = mean.count.grav.V2.log.sc)) +
  scale_color_viridis_c() +
  coord_sf(xlim = c(102, 110), ylim = c(8, 24), expand = FALSE) +
  theme_minimal() +
  ggtitle("Inequity Vietnam")

worldData <- rbindlist(lapply(csv_files, fread), use.names = TRUE, fill = TRUE)

worldData <- st_as_sf(worldData,
                coords = c("CENTROID_X", "CENTROID_Y"),
                crs = 4326)

data_pop_2020 <- dplyr::select(data, "ISOALPHA", "COUNTRYNM", 
                              "NAME1", "NAME2", "NAME3", "NAME4",
                              "NAME5", "NAME6", 
                              "UN_2020_E")

data_pop_viet_2020 <- filter(data_pop_2020, ISOALPHA == "VNM")



ggplot(data_pop_viet_2020) +
  geom_sf(data = world, fill = "#ADD8E6", color = "white") +
  geom_sf(aes(color = UN_2020_E)) +
  scale_color_viridis_c() +
  coord_sf(xlim = c(102, 110), ylim = c(8, 24), expand = FALSE) +
  theme_minimal() +
  ggtitle("Global Admin Units by 2020 Population")

View(data_pop_2020)

data_pop_2020_country <- data_pop_2020 |>
  st_drop_geometry() |>
  group_by(ISOALPHA) |>
  summarise(
    name_en = first(ISOALPHA),
    population = sum(UN_2020_E)
  ) 

world_joined_pop <- world |>
  left_join(data_pop_2020_country, by = c("iso_a3" = "ISOALPHA"))
View(world_joined_pop)

world_joined_pop_filtered <- select(world_joined_pop, sov_a3, population)
View(world_joined_pop_filtered)

usa_pop <- filter(data_pop_2020_country, "ISOALPHA" == "USA")
View(usa_pop)

# VIETNAM CHECKING

viet <- read.csv("/Users/student/Downloads/gpw-v4-admin-unit-center-points-population-estimates-rev11_vnm_csv (1)/gpw_v4_admin_unit_center_points_population_estimates_rev11_vnm.csv")
viet <- st_as_sf(viet,
                coords = c("CENTROID_X", "CENTROID_Y"),
                crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(viet) +
  geom_sf(data = world, fill = "#ADD8E6", color = "white") +
  geom_sf(aes(color = UN_2020_E)) +
  coord_sf(xlim = c(102, 110), ylim = c(8, 24), expand = FALSE) +
  scale_color_viridis_c() +
  theme_minimal() + 
  ggtitle("Vietnam Admin Units by 2020 Population")

library(sf)

# Example: your data frame with longitude and latitude
coords <- read.csv("/Users/student/Downloads/gpw-v4-admin-unit-center-points-population-estimates-rev11_vnm_csv (1)/gpw_v4_admin_unit_center_points_population_estimates_rev11_vnm.csv")

points_sf <- st_as_sf(coords, coords = c("CENTROID_X", "CENTROID_Y"), crs = 4326)


library(rnaturalearth)

coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")


# Transform both to a projected CRS for accurate distance (e.g., meters)
points_proj <- st_transform(points_sf, 3857)
coast_proj <- st_transform(coastline, 3857)

# Calculate distance in meters
distances <- st_distance(points_proj, coast_proj)

# Get the minimum distance from each point to the entire coastline
min_distances <- apply(distances, 1, min)

# Check which are within 5 km
within_5km <- min_distances <= 5000

# Add to data frame
coords$within_5km_of_coast <- within_5km
coords$distance_to_coast_km <- as.numeric(min_distances) / 1000

vnm_filtered <- filter(coords, within_5km_of_coast == TRUE)
print(vnm_filtered)

vnm_filtered_sf <-st_as_sf(vnm_filtered,
                  coords = c("CENTROID_X", "CENTROID_Y"),
                  crs = 4326)

ggplot(vnm_filtered_sf) +
  geom_sf(data = world, fill = "#ADD8E6", color = "white") +
  geom_sf(aes(color = UN_2020_E)) +
  coord_sf(xlim = c(102, 110), ylim = c(8, 24), expand = FALSE) +
  scale_color_viridis_c() +
  theme_minimal() + 
  ggtitle("Vietnam Coast Admin Units by 2020 Population")

# KOREA CHECKING

kor <- read.csv("/Users/student/Downloads/gpw-v4-admin-unit-center-points-population-estimates-rev11_kor_csv (2)/gpw_v4_admin_unit_center_points_population_estimates_rev11_kor.csv")
kor <- st_as_sf(kor,
                coords = c("CENTROID_X", "CENTROID_Y"),
                crs = 4326)

ggplot(kor) +
  geom_sf(data = world, fill = "#ADD8E6", color = "white") +
  geom_sf(aes(color = UN_2020_E)) +
  coord_sf(xlim = c(126, 130), ylim = c(33, 39), expand = FALSE) +
  scale_color_viridis_c() +
  theme_minimal() + 
  ggtitle("Republic of Korea Admin Units by 2020 Population")

bbox_df <- do.call(rbind, lapply(1:nrow(world), function(i) {
  bb <- st_bbox(world[i, ])
  data.frame(
    name = world$admin[i],
    xmin = bb["xmin"],
    ymin = bb["ymin"],
    xmax = bb["xmax"],
    ymax = bb["ymax"]
  )
}))





data_pop_2020


leaflet(data_pop_2020) |>
  addTiles() |>
  addCircleMarkers(
    radius = 6,
    color = "blue",
    fillOpacity = 0.7,
    label = ~COUNTRYNM
  )
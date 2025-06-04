library(ggplot2)
library(maps)
library(sf)
library(dplyr)
library(leaflet)
library(raster)
library(data.table)

# FILTERED FILES INSPECTION

filter1 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1.csv")
filter2 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_100001.csv")
filter3 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_200001.csv")
filter4 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_300001.csv")
filter5 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_400001.csv")
filter6 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_500001.csv")
filter7 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_600001.csv")
filter8 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_700001.csv")
filter9 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_800001.csv")
filter10 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_900001.csv")
filter11 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1000001.csv")
filter12 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1100001.csv")
filter13 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1200001.csv")
filter14 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1300001.csv")
filter15 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1400001.csv")
filter16 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1500001.csv")
filter17 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1600001.csv")
filter18 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1700001.csv")
filter19 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1800001.csv")
filter20 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_1900001.csv")
filter21 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_2000001.csv")
filter22 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_2100001.csv")
filter23 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_2200001.csv")
filter24 <- read.csv("/Users/student/Desktop/Data+/climate_plus_summer_2025/LeonardEDA/Filtered_Files/filtered_2300001.csv")

filter_list<- list(filter1, filter2, filter3, filter4, filter5, filter6,
                   filter7, filter8, filter9, filter10, filter11, filter12,
                   filter13, filter14, filter15, filter16, filter17, filter18,
                   filter19, filter20, filter21, filter22, filter23, filter24)

total_obs <- 0

for (filter in filter_list) {
  total_obs <- total_obs + nrow(filter)
}
total_obs


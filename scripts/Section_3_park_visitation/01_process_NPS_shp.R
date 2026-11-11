#-------------------------------------------------------------------------------
# 
# '01_process_NPS_shp.R': This script reads in NPS' official shapefile of park
# locations and isolates NPS National Parks in CONUS
#
#-------------------------------------------------------------------------------

#load settings and packages
source('./scripts/setup.R')

#load nps boundary data from "https://www.arcgis.com/home/group.html?id=00f2977287f74c79aad558708e3b6649#overview"
nps_data <- st_read("./data/spatial_bound_data/NPS_boundaries/nps_boundary.shp", quiet = T)

#reproject
nps_data <- st_transform(nps_data, "EPSG:5070")

#load CONUS boundary data to crop
CONUS_bound <- st_read("./data/spatial_bound_data/CONUS_ARD_grid/conus_ard_grid.shp", quiet = T)
CONUS_bound <- st_transform(CONUS_bound, "EPSG:5070")
CONUS_bound <- st_union(CONUS_bound)
nps_data <- st_crop(nps_data, CONUS_bound)

#filter to just include national parks, not other NPS designations (e.g., monuments, historic sites)
nps_data <- nps_data %>%
  filter(str_detect(tolower(UNIT_NAME), "national park")) %>%
  filter(!str_detect(tolower(UNIT_NAME), "wolf trap")) %>%
  dplyr::select(UNIT_CODE, UNIT_NAME, STATE, REGION)

#save into processed_data folder
saveRDS(nps_data, "./data/processed_data/nps_data.RDS")

#save 200 km buffered version
nps_data_buff <- st_buffer(nps_data, dist = 200000)
nps_data_buff <- st_crop(nps_data_buff, CONUS_bound)

saveRDS(nps_data, "./data/processed_data/nps_data_buff.RDS")

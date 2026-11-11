
#-------------------------------------------------------------------------------
# "08_filtered_fire_process.R": This script filters HMS fire points by 
# intersecting with MTBS fire perimeters
#
# Important:
# Times given in UTC/GMT times
#
#
#-------------------------------------------------------------------------------

#load script settings
source('./scripts/setup.R')

#load MTBS perimeter data
perim_data <- st_read("./data/mtbs_perimeter_data/mtbs_perims_DD.shp")
perim_data <- st_transform(perim_data, "EPSG:4326")

#load processedHMS fire points
hms_data <- readRDS("./data/processed_data/fire_data.RDS")
hms_data <- st_transform(hms_data, "EPSG:4326")

hms_data$ymd <- as.Date(hms_data$fire_detect_UTC, "%Y-%m-%d", tz="UTC")

## intersect

#filter fire perimeters to more recent events
perim_data <- perim_data %>%
  filter(Ig_Date >= "2018-06-01") %>%
  dplyr::select(Incid_Name, Ig_Date)

#buffer perimeters 1 km
perim_data <- st_make_valid(perim_data)
perim_data$area <- st_area(perim_data)
perim_data_simp <- st_simplify(perim_data, dTolerance = 100)
fires_buf <- st_buffer(perim_data, 1000)

fire_points_list <- vector("list", length = nrow(fires_buf))
for (j in 1:nrow(fires_buf)){
  fire_j <- fires_buf[j,] 
  area_j <- perim_data$area[j]
  relevant_pts_j <- hms_data %>%
    filter(ymd >= fire_j$Ig_Date,
           ymd <= fire_j$Ig_Date %m+% months(6))
  
  pts_w_fire_j <- st_join(
    relevant_pts_j,
    fire_j,
    st_intersects,
    left = F
  )
  
  pts_w_fire_j <- pts_w_fire_j %>%
    mutate(fire_total_area = area_j)
  
  fire_points_list[[j]] <- pts_w_fire_j
  print(j)
}

#filter out fires that don't end up matching (non CONUS or small in scale)
filtered_fire_points_list <- Filter(function(x) nrow(x) > 0, fire_points_list)
fire_points_df <- rbindlist(filtered_fire_points_list)
saveRDS(fire_points_df, "./data/processed_data/filtered_fire_points.RDS")












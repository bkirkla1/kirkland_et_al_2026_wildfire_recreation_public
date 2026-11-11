#-------------------------------------------------------------------------------
#
# "09_polygon_data_process.R": this script takes polygons and attributes all 
# pm2.5, fire points, and weather to the given polygons (in our case, NPS parks)
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#load polygons
poly_data <- readRDS("./data/processed_data/nps_data.RDS")

#define date range of study 
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2022-12-31")
dates_main <- seq(start_date, end_date, by = "day")

## now add in timezone data to original poly sf
#load time zone data and simplify
time_zones <- st_read('./data/spatial_bound_data/time_zones/timeznp020.shp', quiet = T)
time_zones <- time_zones %>%
  group_by(TIMEZONE) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(TIMEZONE %in% c("Central", "Eastern", "Mountain", "Pacific"))
time_zones <- st_transform(time_zones, "EPSG:5070")

#create poly centroids and buffer
poly_centroids <- st_centroid(poly_data)
poly_centroids <- st_buffer(poly_centroids, dist = 3000)

#intersect nps centroids and time zones to get time zone for each park
poly_time_int <- st_intersects(poly_centroids, time_zones)
poly_data$time_zone <- time_zones$TIMEZONE[unlist(psoly_time_int)]

#change time zone character to match lubridate conventions
convert_convention <- function(time_zone) {
  tz_map <- c("Eastern" = "America/New_York",
              "Central" = "America/Chicago",
              "Mountain" = "America/Denver",
              "Pacific" = "America/Los_Angeles")
  local_tz <- tz_map[time_zone]}

poly_data$time_zone <- convert_convention(poly_data$time_zone)

##---- process pm2.5 baseline measure

## from global GH data
{
#identify files to loop over and extract nps data
pm25_files <- list.files("E:/GH_PM25_data/GH_PM25_data_cropped", full.names = T)
pm25_files <- pm25_files[grep("\\.nc", pm25_files)]
pm25_files <- pm25_files[!grepl("\\.aux", pm25_files)]

#convert poly_data to same crs as pm25 rasters
test_rast <- rast(pm25_files[1])
poly_data_trans <- st_transform(poly_data, crs(test_rast))

# Set up parallel backend
plan(multisession, workers = parallel::detectCores() - 2)

# loop in parallel and extract mean pm25 at parks, store results
pm25_data_list <- future_lapply(seq_along(pm25_files), function(j) {
  r <- rast(pm25_files[j])
  
  # Extract date from file name 
  file_date <- as.Date(gsub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", pm25_files[j]))
  
  vals <- extract(r, poly_data_trans, fun = mean, na.rm = TRUE)
  return(data.frame(
    date = file_date,
    UNIT_NAME = poly_data$UNIT_NAME,
    GH_pm25 = vals[, 2]
  ))
})
  
#combine into one dataframe
pm25_data_df <- rbindlist(pm25_data_list)

#merge in time zones
pm25_data_df <- merge(pm25_data_df,
                      poly_data %>% dplyr::select(UNIT_NAME, time_zone) %>% st_drop_geometry(),
                      by = c("UNIT_NAME"))

##convert from UTC averages to tz-specific averages

#each row reflects the full UTC day (00:00–23:59 UTC)
pm25_data_df[, utc_start := as.POSIXct(date, tz = "UTC")]

# Compute local start time row-wise using mapply
pm25_data_df[, local_start := as.POSIXct(mapply(function(dt, tz) {
  format(with_tz(dt, tzone = tz), "%Y-%m-%d %H:%M:%S")
}, dt = utc_start, tz = time_zone), tz = "UTC")]

# Get the local calendar date this corresponds to
pm25_data_df[, local_date := as.Date(local_start)]

# Create a lagged version (Day t−1)
pm25_data_df_lag <- copy(pm25_data_df)[, date := date + 1]  # shift forward to join with lead
setnames(pm25_data_df_lag, "GH_pm25", "GH_pm25_prev")

# Join lag and lead
pm25_merge <- merge(
  pm25_data_df,
  pm25_data_df_lag[, .(UNIT_NAME, date, GH_pm25_prev)],
  by = c("UNIT_NAME", "date"),
  all.x = TRUE
)

# Recreate utc_start and local_start (defensive)
pm25_merge[, utc_start := as.POSIXct(date, tz = "UTC")]
pm25_merge[, local_start := mapply(function(dt, tz) {
  with_tz(dt, tzone = tz)
}, dt = utc_start, tz = time_zone)]

# Compute UTC offset in hours
pm25_merge[, utc_offset_hr := as.numeric(difftime(local_start, utc_start, units = "hours"))]

# Calculate weights from UTC offset
pm25_merge[, `:=`(
  w_prev = pmax(0, -utc_offset_hr) / 24,
  w_curr = pmin(24, 24 + utc_offset_hr) / 24
)]

# Compute weighted local average
pm25_merge[, GH_pm25_local := w_prev * GH_pm25_prev + w_curr * GH_pm25]

#clean up
pm25_merge <- pm25_merge %>%
  dplyr::select(UNIT_NAME, local_date, GH_pm25_local) %>%
  filter(!is.na(GH_pm25_local)) %>%
  rename(park = UNIT_NAME)

#no results for 12-31-2022 because no GH data 2023. Fill in with NA's
data_12_31_2022 <- data.frame(park = unique(pm25_merge$park), 
                              local_date = as.Date("2022-12-31"),
                              GH_pm25_local = NA)
pm25_merge <- rbind(pm25_merge, data_12_31_2022)

#save results
saveRDS(pm25_merge,"./data/processed_data/GH_pm25_data_processed.RDS")
}

## from epa air quality station data
{
#load location of all monitors (from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Meta)
aqs_sites <- read.csv("./data/EPA_AQS_data/aqs_sites.csv")
aqs_sites <- aqs_sites %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
aqs_sites <- st_as_sf(aqs_sites, coords = c("Longitude", "Latitude"), crs = 4326)

#filter to get monitors that have active data during out time frame
aqs_sites <- aqs_sites %>%
  filter(Site.Closed.Date > "2019-01-01" | Site.Closed.Date == "")

#read in official pm2.5 data
unq_years <- sort(unique(year(dates_main)))
epa_data_list <- vector("list", length = length(unq_years))
for (j in 1:length(unq_years)){
  year_j <- unq_years[j]
  dir_j <- paste0("./data/EPA_AQS_data/daily_88101_", year_j, ".csv")
  epa_data_list[[j]] <- read.csv(dir_j)
}
data_88101 <- rbindlist(epa_data_list)

#repeat with non FRM pm2.5 data from EPA to expand spatial coverage
epa_nonFRM_list <- vector("list", length=  length(unq_years))
for (j in 1:length(unq_years)){
  year_j <- unq_years[j]
  dir_j <- paste0("./data/EPA_AQS_data/daily_88502_", year_j, ".csv")
  epa_nonFRM_list[[j]] <- read.csv(dir_j)
}
data_88502 <- rbindlist(epa_nonFRM_list)

#combine
epa_aqs_data <- rbind(data_88101, data_88502)

#can register negative pm25 at low concentrations, replace with 0
epa_aqs_data <- epa_aqs_data %>%
  mutate(Arithmetic.Mean = ifelse(Arithmetic.Mean < 0, 0, Arithmetic.Mean))

#create unique station id
epa_aqs_data <- epa_aqs_data %>%
  mutate(station_id = paste0(State.Code, "_", County.Code, "_", Site.Num))

#isolate unique lat-lon combos
epa_aqs_data_lat_lon <- epa_aqs_data %>%
  dplyr::select(station_id, Latitude, Longitude) %>%
  distinct()

#convert to sf object
epa_aqs_data_lat_lon <- st_as_sf(epa_aqs_data_lat_lon,
                                 coords = c("Longitude", "Latitude"),
                                 crs = 4326)

#id those within 100 km of a park
poly_data_trans <- st_transform(poly_data, crs = crs(epa_aqs_data_lat_lon))

#identify those within 100km
dist = 100000
park_buff <- st_buffer(poly_data_trans, dist = dist)
park_station_int <- st_intersects(park_buff, epa_aqs_data_lat_lon)

# For each park compute distance to identified stations
park_station_df <- purrr::map_dfr(seq_along(park_station_int), function(i) {
  park_name <- poly_data_trans$UNIT_NAME[i]
  park_geom <- poly_data_trans[i, ]
  stations_idx <- park_station_int[[i]]
  
  if (length(stations_idx) == 0) return(NULL)
  
  nearby_stations <- epa_aqs_data_lat_lon[stations_idx, ]
  nearby_stations$park <- park_name
  nearby_stations$distance_m <- st_distance(nearby_stations, park_geom) |> as.numeric()
  
  st_drop_geometry(nearby_stations)
})

#clean up epa aqs data
epa_aqs_data <- epa_aqs_data %>%
  filter(station_id %fin% park_station_df$station_id) %>%
  mutate(date = as.Date(Date.Local),
         pm25 = Arithmetic.Mean) %>%
  dplyr::select(date, pm25, station_id)

#join (Dry Tortugas NP gets dropped because no nearby station, fine because missing data for lots of weather/pm25)
epa_with_parks <- epa_aqs_data %>%
  inner_join(park_station_df, by = "station_id")

#create inverse distance weighting
epa_with_parks <- epa_with_parks %>%
  mutate(inv_dist = 1 / (distance_m + 1),
         within50  = distance_m <= 50000)

# Compute daily IDW PM2.5 for 0–100 km (existing) and 0–50 km (new)
epa_pm25_daily_weighted <- epa_with_parks %>%
  group_by(park, date) %>%
  summarise(
    # 0–100 km IDW 
    epa_base_pm25       = sum(pm25 * inv_dist, na.rm = TRUE) / sum(inv_dist, na.rm = TRUE),
    min_station_dist    = min(distance_m, na.rm = TRUE),
    n_stations_100km    = sum(!is.na(pm25)),
    
    # 0–50 km IDW (only stations within 50 km)
    epa_pm25_50km = {
      w <- inv_dist[within50]
      x <- pm25[within50]
      if (length(w) > 0 && sum(w, na.rm = TRUE) > 0) {
        sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
      } else NA_real_
    },
    min_station_dist_50km = if (any(within50)) min(distance_m[within50], na.rm = TRUE) else NA_real_,
    n_stations_50km       = sum(within50 & !is.na(pm25)),
    .groups = "drop"
  )


#create master grid to track park-date combos with missing data
park_date_grid <- expand.grid(
  park = unique(poly_data_trans$UNIT_NAME),
  date = dates_main,
  stringsAsFactors = FALSE
)

#rejoin
epa_pm25_daily_weighted <- park_date_grid %>%
  left_join(
    epa_pm25_daily_weighted,
    by = c("park", "date")
  )

#save
saveRDS(epa_pm25_daily_weighted, "./data/processed_data/epa_park_pm25_data.RDS")
}

##----weather data processing
{
#load GRIDMET data
years <- c(2018:2023)
vars <- c("tmmx", "tmmn", "pr", "vs", "th") #max_temp (K), min_temp (K), precip (mm), wind speed (m/s), wind direction (degrees from north)

#set gridmet crs manually. struggles matching despite both being 4326
GRIDMET_crs <- "GEOGCRS[\"WGS 84\",\n
DATUM[\"World Geodetic System 1984\",\n
ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n
LENGTHUNIT[\"metre\",1]]],\n
PRIMEM[\"Greenwich\",0,\n
ANGLEUNIT[\"degree\",0.0174532925199433]],\n
CS[ellipsoidal,2],\n
AXIS[\"geodetic latitude (Lat)\",north,\n
ORDER[1],\n
ANGLEUNIT[\"degree\",0.0174532925199433]],\n
AXIS[\"geodetic longitude (Lon)\",east,\n
ORDER[2],\n
ANGLEUNIT[\"degree\",0.0174532925199433]],\n
ID[\"EPSG\",4326]]"
poly_data <- st_transform(poly_data, crs = GRIDMET_crs)

# Function to calculate area-weighted mean wind direction
# Adjusted function to calculate area-weighted mean wind direction and return in long format
area_weighted_mean_wind_direction <- function(raster, polygons, year) {
  num_days <- nlyr(raster)  # Number of layers (days) in the raster
  
  # Initialize a list to store daily results
  daily_results <- vector("list", num_days)
  
  for (day in 1:num_days) {
    # Extract the wind direction layer for the specific day
    wind_direction <- raster[[day]]
    
    # Convert wind direction from degrees to radians
    rast_sin <- sin(wind_direction * pi / 180)
    rast_cos <- cos(wind_direction * pi / 180)
    
    # Extract area-weighted means of sine and cosine components
    mean_sin <- extract(rast_sin, polygons, fun = mean, weights = TRUE, na.rm = TRUE)
    mean_cos <- extract(rast_cos, polygons, fun = mean, weights = TRUE, na.rm = TRUE)
    
    # Calculate mean wind direction in radians and convert back to degrees
    mean_wind_direction <- atan2(mean_sin[,2], mean_cos[,2]) * 180 / pi
    
    # Handle NA values and adjust for negative angles
    mean_wind_direction <- ifelse(is.na(mean_wind_direction), NA, 
                                  ifelse(mean_wind_direction < 0, mean_wind_direction + 360, mean_wind_direction))
    
    # Store the result with the date
    daily_results[[day]] <- data.frame(
      ID = mean_sin[,1],
      date = as.Date(paste0(year, "-01-01")) + (day - 1),
      th = mean_wind_direction
    )
  }
  
  # Combine daily results into a single data frame
  result <- do.call(rbind, daily_results)
  
  return(result)
}


#create storage list for results
weather_data_list <- vector("list", length = length(years)*length(vars))
counter <- 1

for (i in 1:length(vars)){
  for (j in 1:length(years)){
    
    #load in gridmet rasters
    dir_ji <- paste0("./data/GRIDMET_data/", vars[i], "_", years[j], ".nc")
    rast_ji <- rast(dir_ji)
    
    #calculate area-weighted mean of the variable
    if (vars[i] == "th"){
      mean_var <- area_weighted_mean_wind_direction(rast_ji, poly_data, years[j])}
    else{mean_var <- extract(rast_ji, poly_data, fun = mean, weights = T, na.rm = T)}
    
    #reshape data 
    mean_var[,1] <- poly_data$UNIT_NAME
    mean_var <- mean_var %>% rename(UNIT_NAME = ID)
    
    if (vars[i] != "th"){
    long_data <- mean_var %>%
      pivot_longer(
        cols = -UNIT_NAME,
        names_to = "var_descript",
        values_to = "value")
    
    #add date 
    start_date_j <- as.Date(paste0(years[j], "-01-", "01"))
    end_date_j <- as.Date(paste0(years[j], "-12-", "31"))
    dates_j <- seq(start_date_j, end_date_j, by = "days")
    long_data$date <- rep(dates_j, times = nrow(poly_data))
    
    #clean up columns and names
    new_var_name <- vars[i]
    mean_var <- long_data %>%
      dplyr::select(UNIT_NAME, date, value) %>%
      rename(!!new_var_name := value)
    }
    
    #store results
    weather_data_list[[counter]] <- mean_var
    counter <- counter+1
    
    print(paste("i is", i))
    print(paste("j is", j))
  }
}

#combine all into one long df 

#create unique dates-park combinations
parks <- unique(poly_data$UNIT_NAME)
all_dates <- seq(as.Date(paste0(min(years), "-01-01")),
                 as.Date(paste0(max(years), "-12-31")),
                 by = "day")

master_weather_df <- expand.grid(
  UNIT_NAME = parks,
  date      = all_dates,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

#iteratively merge columns by name (each column.x represents a different year)
weather_df <- master_weather_df

for (i in 1:length(vars)){
   #combine all years for a given weather_variable
  var_i <- vars[i]
  indexes <- which(sapply(weather_data_list, var_column_ind, col_name = var_i))
  df_var_i <- weather_data_list[indexes]
  df_var_i <- rbindlist(df_var_i)
    
  weather_df <- weather_df %>%
    left_join(df_var_i, by = c("UNIT_NAME", "date"))
}

saveRDS(weather_df, './data/processed_data/nps_weather.RDS')
}

##----calculate distance to fires

##with all HMS data
{
#load fire data  
fire_data <- readRDS("./data/processed_data/fire_buff.RDS")
fire_data <- fire_data %>%
  filter(as.Date(fire_detect_UTC) %in% dates_main)
fire_data <- st_as_sf(fire_data)

#recreate centroids
poly_cent <- st_centroid(poly_data)

#simplify geometry of park polygons (except Gateway Arch)
poly_data_simp <- st_simplify(poly_data[-28,], dTolerance = 1000)
poly_data_simp <- rbind(poly_data_simp[1:27,], poly_data[28,], poly_data_simp[28:nrow(poly_data_simp),])

#calculate distances each day for centroids and park borders

#define date range
dates_fire_point <- unique(fire_data$fire_detect_UTC)

#pre-split fire data by date
fire_data_list <- fire_data %>%
  group_split(fire_detect_UTC)
fire_dist_list <- vector("list", length(dates_fire_point))

for (j in 1:length(dates_fire_point)){
  
  #filter fires to date j
  fire_j <- fire_data_list[[j]]
  fire_j <- st_as_sf(fire_j)
  
  #confirm crs match
  if (crs(fire_j) != crs(poly_data_simp)){break}
  
  #calculate closest fires in km
  dist_j_bound <- st_distance(fire_j, poly_data_simp)
  fire_dist_km_bound <- apply(dist_j_bound, 2, min, na.rm = T)
  fire_dist_km_bound <- round(fire_dist_km_bound/1000, 3)
  
  dist_j_cent <- st_distance(fire_j, poly_cent)
  fire_dist_km_cent <- apply(dist_j_cent, 2, min, na.rm = T)
  fire_dist_km_cent <- round(fire_dist_km_cent/1000, 3)
  
  #clean results and store
  results <- data.frame(cbind(poly_data$UNIT_NAME, fire_dist_km_bound, fire_dist_km_cent)) %>%
    mutate(date_LT = fire_j$fire_detect_UTC[1]) %>%
    rename(park = V1)
  fire_dist_list[[j]] <- results 
  
  #track progress
  print(fire_j$fire_detect_UTC[1])
}

#combine data
fire_dist_centroid_df <- rbindlist(fire_dist_list)

#load nps road data
nps_roads_dir <- "./data/spatial_bound_data/NPS_roads/NPS_-_Roads_-_Geographic_Coordinate_System.shp"
nps_roads <- st_read(nps_roads_dir)
nps_roads <- st_zm(nps_roads) #drop weird z and m coordinates
nps_roads <- st_transform(nps_roads, crs(poly_data))

#keep roads that intersect parks
roads_poly_int <- st_intersects(nps_roads, poly_data)
nps_roads$park <- as.numeric(roads_poly_int)
nps_roads <- nps_roads %>%
  filter(!is.na(park))

#group by park and union
nps_roads_union <- nps_roads %>%
  group_by(park) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

nps_roads_cropped_list <- vector("list", length(nps_roads_union))

#crop to keep roads within park boundaries
for (j in 1:length(unique(nps_roads_union$park))){
  
  park_j <- unique(nps_roads_union$park)[j]
  nps_roads_j <- nps_roads_union %>%
    filter(park == park_j) 
  nps_roads_j <- st_crop(nps_roads_j, poly_data[park_j,])
  nps_roads_cropped_list[[j]] <- nps_roads_j
  
}
nps_roads_cropped <- st_as_sf(rbindlist(nps_roads_cropped_list))

#now loop and calculate distances
fire_dist_list_roads <- vector("list", length(dates_fire_point))

for (j in 1:length(dates_fire_point)){
  
  #filter fires to date j
  fire_j <- fire_data_list[[j]]
  fire_j <- st_as_sf(fire_j)
  
  #confirm crs match
  if (crs(fire_j) != crs(poly_cent)){break}
  
  #calculate closest fires in km
  dist_j_road <- st_distance(fire_j, nps_roads_cropped)
  fire_dist_km_road <- apply(dist_j_road, 2, min, na.rm = T)
  fire_dist_km_road <- round(fire_dist_km_road/1000, 3)
  
  #clean results and store
  results <- data.frame(cbind(poly_data$UNIT_NAME[nps_roads_cropped$park], fire_dist_km_road)) %>%
    mutate(date_LT = fire_j$fire_detect_UTC[1]) %>%
    rename(park = V1)
  fire_dist_list_roads[[j]] <- results 
  
  #track progress
  print(fire_j$fire_detect_UTC[1])
}
fire_dist_roads_df <- rbindlist(fire_dist_list_roads)

#calculate number of fires within 100 km of the park on a given day

#create storage list for results
fire_number_list <- vector("list", length = length(dates_fire_point))

#buffer parks by 100km and 250km
poly_data_simp_buff_100km <- st_buffer(poly_data_simp, dist = 100000)
poly_data_simp_buff_250km <- st_buffer(poly_data_simp, dist = 250000)

for (j in 1:length(dates_fire_point)){
  
  #filter fires to date j
  fire_j <- fire_data_list[[j]]
  fire_j <- st_as_sf(fire_j)
  
  #confirm crs match
  if (crs(fire_j) != crs(poly_cent)){break}
  
  #count number of fire events overlapping the buffered park for 100 km and 250 km
  overlap_counts_100km <- st_intersects(poly_data_simp_buff_100km, fire_j, sparse = F)
  fire_num_100km <- rowSums(overlap_counts_100km)
  
  overlap_counts_250km <- st_intersects(poly_data_simp_buff_250km, fire_j, sparse = F)
  fire_num_250km <- rowSums(overlap_counts_250km)
  
  #clean results and store
  results <- data.frame(cbind(poly_data$UNIT_NAME, fire_num_100km, fire_num_250km)) %>%
    mutate(date_LT = fire_j$fire_detect_UTC[1]) %>%
    rename(park = V1)
  fire_number_list[[j]] <- results 
  
  #track progress
  print(fire_j$fire_detect_UTC[1])
}

#save results
fire_number_df <- rbindlist(fire_number_list)

#combine all fire variables into one main df and save 
main_fire_data <- merge(fire_dist_centroid_df, fire_dist_roads_df, 
                        by = c("date_LT", "park"),
                        all.x = T)
main_fire_data <- merge(main_fire_data, fire_number_df, 
                        by = c("date_LT", "park"),
                        all.x = T)

#create grid of all park-date combinations to hold onto park-dates with missing data (HMS)
base_fire_df <- expand_grid(date_LT = as.character(dates_main), park = unique(main_fire_data$park))
base_fire_df <- base_fire_df %>%
  left_join(main_fire_data,
            by = c("date_LT", "park"))

#save
saveRDS(base_fire_df, "./data/processed_data/fire_variables.RDS")

}

##with filtered HMS data
{
  #load fire data  
  fire_data_filt <- readRDS("./data/processed_data/filtered_fire_points.RDS")
  fire_data_filt <- fire_data_filt %>%
    dplyr::select(-c(fire_detect_UTC, Ig_Date, Incid_Name)) %>%
    rename(fire_detect_UTC = ymd) %>%
    filter(fire_detect_UTC %in% dates_main)
  
  #convert crs
  fire_data_filt <- st_as_sf(fire_data_filt, crs = "EPSG:4326")
  fire_data_filt <- st_transform(fire_data_filt, crs = crs(poly_data))
  
  #calculate distances each day for centroids and park borders using filtered hms points
  
  #define dates
  dates_fire_point_filt <- unique(fire_data_filt$fire_detect_UTC)
  
  #pre-split fire data by date
  fire_data_list_filt <- fire_data_filt %>%
    group_by(fire_detect_UTC) %>%
    group_split()
  fire_dist_list_filt <- vector("list", length(dates_fire_point_filt))
  
  for (j in 1:length(dates_fire_point_filt)){
    
    #filter fires to date j
    fire_j <- fire_data_list_filt[[j]]
    fire_j <- st_as_sf(fire_j)
    
    #confirm crs match
    if (crs(fire_j) != crs(poly_cent)){break}
    
    #calculate closest fires in km
    dist_j_bound <- st_distance(fire_j, poly_data_simp)
    fire_dist_km_bound <- apply(dist_j_bound, 2, min, na.rm = T)
    fire_dist_km_bound <- round(fire_dist_km_bound/1000, 2)
    
    dist_j_cent <- st_distance(fire_j, poly_cent)
    fire_dist_km_cent <- apply(dist_j_cent, 2, min, na.rm = T)
    fire_dist_km_cent <- round(fire_dist_km_cent/1000, 2)
    
    #get area of the closest fire
    # find which fire is closest for each park
    idx_bound <- apply(dist_j_bound, 2, which.min)
    idx_cent  <- apply(dist_j_cent,  2, which.min)
    # extract the matching area
    area_bound <- round(as.numeric(fire_j$area[idx_bound])/1000000,2) 
    area_cent  <- round(as.numeric(fire_j$area[idx_cent])/1000000,2)
    
    #clean results and store
    results <- data.frame(
      park                 = poly_data$UNIT_NAME,
      fire_dist_km_bound   = fire_dist_km_bound,
      nearest_area_bound_km2   = area_bound,
      fire_dist_km_cent    = fire_dist_km_cent,
      nearest_area_cent_km2    = area_cent,
      date_LT              = fire_j$fire_detect_UTC[1]
    )
    fire_dist_list_filt[[j]] <- results 
    
    #track progress
    print(fire_j$fire_detect_UTC[1])
  }
  
  #combine data
  fire_dist_centroid_df_filt <- rbindlist(fire_dist_list_filt)
  
  #repeat for distance from filtered HMS points to nps roads loop and calculate distances
  fire_dist_list_roads_filt <- vector("list", length(dates_fire_point))
  
  for (j in 1:length(dates_fire_point_filt)){
    
    #filter fires to date j
    fire_j <- fire_data_list_filt[[j]]
    fire_j <- st_as_sf(fire_j)
    
    #confirm crs match
    if (crs(fire_j) != crs(poly_cent)){break}
    
    #calculate closest fires in km
    dist_j_road <- st_distance(fire_j, nps_roads_cropped)
    fire_dist_km_road <- apply(dist_j_road, 2, min, na.rm = T)
    fire_dist_km_road <- round(fire_dist_km_road/1000, 2)
    
    #get area of the closest fire
    # find which fire is closest for each park
    idx_road <- apply(dist_j_road, 2, which.min)
    # extract the matching area
    area_road <- round(as.numeric(fire_j$area[idx_road])/1000000,2) 
    
    # assemble the road‐based results
    results <- data.frame(
      park                   = poly_data$UNIT_NAME[nps_roads_cropped$park],
      fire_dist_km_road      = fire_dist_km_road,
      nearest_area_road_km2  = area_road,
      date_LT                = fire_j$fire_detect_UTC[1]
    )
    
    fire_dist_list_roads_filt[[j]] <- results
    
    #track progress
    print(fire_j$fire_detect_UTC[1])
  }
  fire_dist_roads_df_filt <- rbindlist(fire_dist_list_roads_filt)
  
  #calculate number of fires within 100 km of the park on a given day
  
  #create storage list for results
  fire_number_list_filt <- vector("list", length = length(dates_fire_point))
  
  for (j in 1:length(dates_fire_point_filt)){
    
    #filter fires to date j
    fire_j <- fire_data_list_filt[[j]]
    fire_j <- st_as_sf(fire_j)
    
    #confirm crs match
    if (crs(fire_j) != crs(poly_cent)){break}
    
    #count number of fire events overlapping the buffered park for 100 km and 250 km
    overlap_counts_100km <- st_intersects(poly_data_simp_buff_100km, fire_j, sparse = F)
    fire_num_100km <- rowSums(overlap_counts_100km)
    
    overlap_counts_250km <- st_intersects(poly_data_simp_buff_250km, fire_j, sparse = F)
    fire_num_250km <- rowSums(overlap_counts_250km)
    
    #clean results and store
    results <- data.frame(
      park            = poly_data$UNIT_NAME,
      fire_num_100km  = fire_num_100km,
      fire_num_250km  = fire_num_250km,
      date_LT         = fire_j$fire_detect_UTC[1]
    )
    fire_number_list_filt[[j]] <- results 
    
    #track progress
    print(fire_j$fire_detect_UTC[1])
  }
  
  #combine results
  fire_number_df_filt <- rbindlist(fire_number_list_filt)
  
  #combine all fire variables into one main df  
  main_fire_data_filt <- merge(fire_dist_centroid_df_filt, fire_dist_roads_df_filt, 
                          by = c("date_LT", "park"),
                          all.x = T)
  main_fire_data_filt <- merge(main_fire_data_filt, fire_number_df_filt, 
                          by = c("date_LT", "park"),
                          all.x = T)
  main_fire_data_filt <- main_fire_data_filt %>%
    mutate(date_LT = as.Date(date_LT))
  
  #some days are not represented (when no fire points detected in CONUS (no major wildfires, or data issues)
  #create outline of all park-date combinations
  main_full_filt <- main_fire_data_filt %>%
    distinct(park) %>%
    crossing(date_LT = dates_main)
  
  main_fire_data_filt <- main_full_filt %>%
    left_join(main_fire_data_filt, by = c("park", "date_LT"))
  
  #id dates without any fire data
  missing_dates <- main_fire_data_filt %>%
    filter(is.na(fire_dist_km_bound)) %>%
    dplyr::select(date_LT) %>%
    distinct()
  
  #identify which are due to missing data
  fire_file_names <- list.files("./data/fire_data")
  fire_file_dates <- fire_file_names %>%
    str_extract("\\d{8}") %>%
    as.Date(format = "%Y%m%d")
  
  #these ones stay NA
  missing_data_dates <- as.Date(setdiff(missing_dates$date_LT, fire_file_dates), format = "%Y-%m-%d")
  
  #the rest get corrected
  missing_fire_dates <- as.Date(setdiff(missing_dates$date_LT, missing_data_dates), format = "%Y-%m-%d")
  
  #create flag for correction
  date_flags <- tibble(
    date_LT = seq(min(dates_main), max(dates_main), by="day")
  ) %>% 
    mutate(
      no_file      = date_LT %in% missing_data_dates,
      no_fire      = date_LT %in% missing_fire_dates,
      any_fire     = !(no_file | no_fire)  # these are the days your loop actually found fires
    )
  
  main_fire_data_filt <- main_fire_data_filt %>%
    left_join(date_flags, by = "date_LT")
  
  #if no fire, choose maximum distance plus one km
  max_bound <- max(main_fire_data_filt$fire_dist_km_bound, na.rm = TRUE) + 1
  max_cent  <- max(main_fire_data_filt$fire_dist_km_cent,  na.rm = TRUE) + 1
  max_road  <- max(main_fire_data_filt$fire_dist_km_road,  na.rm = TRUE) + 1
  
  main_fire_data_filt <- main_fire_data_filt %>%
    mutate(
      # 1) Whenever there was a file but no fire, counts → 0
      across(
        starts_with("fire_num"),
        ~ if_else(no_fire, 0L, .),
        .names = "{.col}"
      ),
      
      # 2) Whenever there was a file but no fire, distances → a big “far‐away” number
      across(
        starts_with("fire_dist_km"),
        ~ if_else(no_fire, max_bound, .),
        .names = "{.col}"
      ),
      
      # 3) Whenever there was a file but no fire, areas → 0
      across(
        starts_with("nearest_area"),
        ~ if_else(no_fire, 0, .),
        .names = "{.col}"
      )
    )
  
  #rename variables to factor in filtered
  main_fire_data_filt <- main_fire_data_filt %>%
    rename(fire_dist_km_bound_filt = fire_dist_km_bound,
           fire_dist_km_cent_filt = fire_dist_km_cent,
           fire_dist_km_road_filt = fire_dist_km_road,
           fire_num_100km_filt = fire_num_100km,
           fire_num_250km_filt = fire_num_250km,
           nearest_area_bound_km2_filt = nearest_area_bound_km2) %>%
    dplyr::select(-c(any_fire, nearest_area_cent_km2, nearest_area_road_km2,
                     no_file, no_fire))
  
  #save output
  saveRDS(main_fire_data_filt, "./data/processed_data/fire_variables_filtered.RDS")
  
}

##----process Burke's smoke-induced pm2.5 anomaly data
{

#load smoke predictions
smoke_predicts = readRDS("./data/pm2.5_data/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")

#load 10km grid
grid_10km = read_sf("./data/pm2.5_data/10km_grid_wgs84/10km_grid_wgs84.shp")

#choose dates 2018 onward and filter data
dates_smoke <- seq.Date(ymd("20180101"), ymd("20231231"), by = "day")

#loop over one year at a time for memories sake
smoke_data_burke <- expand.grid(grid_id_10km = grid_10km$ID, date = dates_smoke)
smoke_data_burke <- left_join(smoke_data_burke, smoke_predicts, by = c("grid_id_10km", "date"))

#predict 0 for remaining grid cells on non-smoke days
smoke_data_burke = mutate(smoke_data_burke, smokePM_pred = replace_na(smokePM_pred, 0))

#calculate area-weighted mean for a given polygon

#convert to same crs
grid_10km <- st_transform(grid_10km, st_crs(poly_data))

#precompute grid-polygon intersections
poly_grid_int <- st_intersects(poly_data, grid_10km)

#compute area of polygons
poly_areas <- st_area(poly_data)

#initialize storage
poly_smoke_pm25_list <- vector("list", length = nrow(poly_data))

for (j in 1:nrow(poly_data)){
  
  #i.d. grid cells that overlap park j
  int_grid <- grid_10km[poly_grid_int[[j]],]
  
  #calculate relevant areas and pm2.5 weights
  grid_park_j_intersection <- st_intersection(int_grid, poly_data[j,])
  grid_park_j_int_area <- st_area(grid_park_j_intersection)
  int_grid$weights <- grid_park_j_int_area/poly_areas[j]
  
  #now calculate area-weighted pm2.5 for each day for park j
  smoke_data_burke_filt <- smoke_data_burke %>%
    filter(grid_id_10km %in% int_grid$ID)
  
  smoke_data_burke_filt <- smoke_data_burke_filt %>%
    inner_join(int_grid, by = c("grid_id_10km" = "ID"))
  
  smoke_data_burke_filt <- smoke_data_burke_filt %>%
    group_by(date) %>%
    summarise(mean_smoke_pm2.5 = sum(smokePM_pred*weights, na.rm = T), #calculate area weighted mean
              max_smoke_pm2.5 = max(smokePM_pred, na.rm = T)) %>% # calculate max
    mutate(park = poly_data$UNIT_NAME[j], 
           mean_smoke_pm2.5 = as.numeric(mean_smoke_pm2.5))
  
  #store results
  poly_smoke_pm25_list[[j]] <- smoke_data_burke_filt
  
  #print progress
  print(j)
}

#save results
burke_pm25_df <- rbindlist(poly_smoke_pm25_list)
burke_pm25_df <- burke_pm25_df %>%
  rename(date_LT = date)
saveRDS(burke_pm25_df, "./data/processed_data/park_burke_pm25.RDS")

}

##---- merge together 

#load individual datasets
weather_data <- readRDS("./data/processed_data/nps_weather.RDS")
fire_data <- readRDS("./data/processed_data/fire_variables.RDS")
fire_data_filtered <- readRDS("./data/processed_data/fire_variables_filtered.RDS")
burke_pm25_data <- readRDS("./data/processed_data/park_burke_pm25.RDS")
visit_data <- readRDS("./data/processed_data/visit_data.RDS")
GH_pm25_data <- readRDS("./data/processed_data/GH_pm25_data_processed.RDS")
epa_pm25_data <- readRDS("./data/processed_data/epa_park_pm25_data.RDS")

#change variable names for merge
fire_data <- fire_data %>%
  mutate(date_LT = as.Date(date_LT))

weather_data <- weather_data %>%
  rename(park = UNIT_NAME, date_LT = date)

visit_data <- visit_data %>%
  rename(park = location_name,
         date_LT = date_long) %>%
  mutate(date_LT = as.Date(date_LT))

GH_pm25_data <- GH_pm25_data %>%
  rename(date_LT = local_date)

epa_pm25_data <- epa_pm25_data %>%
  rename(date_LT = date)

#create master dataset
main_data <- expand.grid(park = unique(poly_data$UNIT_NAME),
                         date_LT = dates_main)

#merge one by one
main_data <- merge(main_data, weather_data, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, fire_data, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, fire_data_filtered, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, burke_pm25_data, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, visit_data, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, GH_pm25_data, by = c("park", "date_LT"), all.x = T)
main_data <- merge(main_data, epa_pm25_data, by = c("park", "date_LT"), all.x = T)

##check out sources of NA's from merges
na_check <- colSums(is.na(main_data))

#visit data missing CBG home data for some parks, especially North Cascades
#visit data missing city and postal codes for many parks
#visit data missing median dwell time for early sample Yosemite and lots of North Cascades

#all weather data missing for Dry Tortugas national park because Gridmet does not cover KEYs!

#filtered fires missing data is from days without major fire points in HMS data
#filtered and nonfiltered fire to road distances are missing for 
#cuyahoga, dry tortugas, gateway, hot springs, and isle royale because no NPS road data

#epa base pm2.5 measure missing for dry tortugas because no nearby station, missing 
# days of data for other parks because of missing epa data for stations within 100km

#few days of fire_data missing because missing HMS data (4 days)

## save results
saveRDS(main_data, "./data/processed_data/main_data.RDS")






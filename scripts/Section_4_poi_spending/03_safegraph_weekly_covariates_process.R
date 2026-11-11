#-------------------------------------------------------------------------------
#
# '03_safegraph_weekly_covariates_process.R': this script reads in the processed
# Safegraph data and generates all weather, smoke, pm2.5, etc. data at the weekly
# level
#
#-------------------------------------------------------------------------------

#load relevant packages
source('scripts/setup.R')

#load safegraph data
safegraph_data <- readRDS("./data/processed_data/safegraph_unfiltered_weekly_data.RDS")

#filter out missing and null geometries
safegraph_data <- safegraph_data %>%
  st_as_sf() %>%
  mutate(geometry = {
    g <- st_geometry(.)
    null_idx <- vapply(g, is.null, logical(1))
    if (any(null_idx)) {
      g[null_idx] <- st_sfc(rep(list(st_point()), sum(null_idx)), crs = st_crs(.))
    }
    g
  }) %>%
  filter(!is.na(st_geometry(.)), !st_is_empty(st_geometry(.)))

#create dataframe of date-week id combinations to use to aggregate data

#isolate week_id's, start and stop dates
unq_week_ids <- safegraph_data %>%
  group_by(week_id) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(week_id, week_start_date, week_end_date)  

#create df of potential dates
unq_dates <- data.frame(seq(min(safegraph_data$week_start_date), max(safegraph_data$week_end_date),
                            by = "day"))
colnames(unq_dates) <- "date"
unq_dates$week_id <- NA

for (j in 1:nrow(unq_dates)){
  
  date_j <- unq_dates$date[j]
  
  week_id_j <- unq_week_ids$week_id[date_j >= unq_week_ids$week_start_date & date_j <= unq_week_ids$week_end_date]
  
  if (length(week_id_j) != 0){
    unq_dates$week_id[j] <- week_id_j}
  else{unq_dates$week_id[j] <- NA}
}

#filter to just include through end of 2022
unq_dates <- unq_dates %>%
  filter(week_id < 210)

#filter out those dates from safegraph data
safegraph_data <- safegraph_data %>%
  filter(week_id %fin% unq_dates$week_id)

#create simplified df of poi-lat-lon combos to use in all data aggregation
unq_placekeys <- safegraph_data %>%
  dplyr::select(PLACEKEY, geometry) %>%
  group_by(PLACEKEY) %>%
  slice_head(n = 1) %>%
  ungroup()

#### gather data from nearby parks from original visit dataset,
# aggregate up to weekly level to correspond with safegraph data

#load daily visitation dataset
park_visit_data <- readRDS("./data/processed_data/main_data_all_final.RDS")

#remove week_id already associated with the daily park data
park_visit_data <- park_visit_data %>% dplyr::select(-c(week_id))

#create week id in daily visitation data
park_visit_data <- merge(park_visit_data, unq_dates, 
                    by.x = "date_LT",
                    by.y = "date")

#filter to only keep park-week combinations with complete 7 days of observations
park_visit_data <- park_visit_data %>%
  group_by(park, week_id) %>%
  filter(n() == 7) %>%
  ungroup()

#recategorize aqi cat
pm25_to_aqi_category <- function(pm25) {
  dplyr::case_when(
    pm25 <= 12.0 ~ "Good",
    pm25 > 12 & pm25 <= 35.4 ~ "Moderate",
    pm25 > 35.4 & pm25 <= 55.4 ~ "Unhealthy for Sensitive Groups",
    pm25 > 55.4 & pm25 <= 150.4 ~ "Unhealthy",
    pm25 > 150.4 ~ "Very Unhealthy or Hazardous",
    is.na(pm25) ~ NA_character_
  )
}
park_visit_data <- park_visit_data %>%
  mutate(aqi_cat = pm25_to_aqi_category(mean_smoke_pm2.5))

#calculate weekly park variables
#define modal aqi function
mode_cat <- function(x) {
  if (all(is.na(x))) return(NA_character_)
  
  # Predefined AQI order from best to worst
  aqi_order <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy or Hazardous")
  
  # Factorize with levels to ensure consistent order
  x <- factor(x, levels = aqi_order)
  counts <- tabulate(x, nbins = length(aqi_order))
  
  # Choose the worst among tied modes
  mode_index <- max(which(counts == max(counts)))
  return(aqi_order[mode_index])
}

#define function to get max aqi cat in a week
worst_cat <- function(x) {
  if (all(is.na(x))) return(NA_character_)
  
  # Ordered from best to worst
  aqi_order <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy or Hazardous")
  
  # Convert to ordered factor to preserve severity ranking
  x <- factor(x, levels = aqi_order)
  
  # Find index of worst observed level
  worst_index <- max(as.numeric(x), na.rm = TRUE)
  
  if (is.finite(worst_index)) aqi_order[worst_index] else NA_character_
}

#calculate weekly park variables
park_visit_data_weekly_vars <- park_visit_data %>%
  mutate(mean_smoke_pm2.5 = as.numeric(mean_smoke_pm2.5)) %>%
  group_by(park, week_id) %>%
  summarise(
    mean_weekly_pm25_park = mean(mean_smoke_pm2.5, na.rm = T),
    mean_weekly_temp_park = mean(tmmx, na.rm = T),
    modal_aqi_cat_park = mode_cat(aqi_cat),
    total_weekly_prcp_park = sum(pr, na.rm = T),
    fire_within_100km_park = as.integer(any(fire_dist_km_bound <= 100, na.rm = TRUE)),
    total_weekly_visits_park = sum(daily_visits_norm_state, na.rm = T),
    
    #create # of days in each aqi category
    days_aqi_good                 = sum(aqi_cat == "Good", na.rm = TRUE),
    days_aqi_moderate             = sum(aqi_cat == "Moderate", na.rm = TRUE),
    days_aqi_usg                  = sum(aqi_cat == "Unhealthy for Sensitive Groups", na.rm = TRUE),
    days_aqi_unhealthy            = sum(aqi_cat == "Unhealthy", na.rm = TRUE),
    days_aqi_very_unhealthy_or_hazardous      = sum(aqi_cat == "Very Unhealthy or Hazardous", na.rm = TRUE),

    #dummy for any unhealthy or worse days
    dummy_unhealthy_or_worse_park = as.integer(
      any(aqi_cat %in% c("Unhealthy", "Very Unhealthy or Hazardous"), na.rm = TRUE)),
    
    #factor for worst aqi cat in the week
    worst_aqi_cat_week_park = worst_cat(aqi_cat),
    
    #calculate total raw and normalized park visits
    raw_weekly_visits_park = sum(raw_daily_visits, na.rm = T),
    normalized_weekly_visits_park = sum(daily_visits_norm_state, na.rm = T)
    
  )

#### calculate weather weekly covariates at the POI's/businesses (already run)
{
## weather data
years <- c(min(year(unq_dates$date)):max(year(unq_dates$date)))
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
unq_placekeys <- st_transform(st_as_sf(unq_placekeys), crs = GRIDMET_crs)

#create a wind direction binning and mode functions

wind_dir_cat_func <- function(degrees) {
  # Shift by 22.5° so that bins align correctly
  shifted <- (degrees + 22.5) %% 360
  
  cut(
    shifted,
    breaks = seq(0, 360, by = 45),  
    labels = FALSE,  
    right = FALSE,
    include.lowest = TRUE
  )
}

#create storage list for results
weather_data_list <- vector("list", length = length(vars))

for (i in 1:length(vars)){
 
  #read in all years of data for one variable
  rast_stack_i <- NULL
  for (j in 1:length(years)){
    #load in gridmet rasters
    dir_ji <- paste0("./data/GRIDMET_data/", vars[i], "_", years[j], ".nc")
    rast_ji <- rast(dir_ji)
    rast_stack_i <- c(rast_stack_i, rast_ji)
  }
  #combine into one stacked raster
  rast_stack_i <- do.call(c,rast_stack_i)
  
  #calculate weekly means
  
  #isolate week index lengths consistent with 3rd dimension of stacked raster (same amount of total days of data)
  #double check that 1st date in unq_dates is same as first date in gridmet raster
  if (unq_dates$date[1] != ymd(paste0(years[1], "0101"))){break}
  
  #crop week ids by relevant gridmet date domain
  week_id_i <- unq_dates$week_id[1:dim(rast_stack_i)[3]]
  week_id_i <- week_id_i[!is.na(week_id_i)]
  
  #calculate weekly means/sum/mode (variable dependent) consistent with week_ids
  if (vars[i] %in% c("tmmx", "tmmn", "vs")){
    weekly_stack <- tapp(rast_stack_i, index = week_id_i, fun = mean, na.rm = T)
  }
  else if (vars[i] %in% c("pr")){
    weekly_stack <- tapp(rast_stack_i, index = week_id_i, fun = sum, na.rm = T)
  }
  else { #use for wind direction
    rast_stack_i <- app(rast_stack_i, wind_dir_cat_func)
    weekly_stack <- tapp(rast_stack_i, index = week_id_i, fun = fmode, na.rm = T)
  }
  
  #extract weekly means at POI's
  mean_var_by_plk <- extract(weekly_stack, unq_placekeys, na.rm = T)
  
  #convert data to long format
  mean_var_by_plk[,1] <- unq_placekeys$PLACEKEY
  long_data <- mean_var_by_plk %>%
    pivot_longer(
      cols = -ID,
      names_to = "week_id",
      values_to = vars[i])
  
  #add week_id
  long_data <- long_data %>%
    mutate(week_id = as.numeric(str_remove(week_id, "^X")))
  
  #store
  weather_data_list[[i]] <- long_data
  
  #progress
  print(i)
}

#now combine results into one long dataframe with all dates and all weather vars
weather_df <- reduce(weather_data_list, left_join, 
               by = c("ID", "week_id"))

#identify week id's without 7 days worth of means, and drop those observations
weather_df <- weather_df %>%
  filter(week_id %in% unq_dates$week_id)

#clean up and rename variables
weather_df <- weather_df %>%
  rename(PLACEKEY = ID)

#save intermediate 
saveRDS(weather_df, "./data/processed_data/weather_data_weekly_POIs.RDS")
}

#### Aggregate burke pm2.5 data to weekly level matching safegraph aggregations
{
#load smoke predictions
smoke_predicts = readRDS("./data/pm2.5_data/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")

#load 10km grid
grid_10km = read_sf("./data/pm2.5_data/10km_grid_wgs84/10km_grid_wgs84.shp")

#choose dates 2018 onward and filter data
dates_smoke <- seq.Date(min(unq_dates$date), max(unq_dates$date), by = "day")

#loop over one year at a time for memories sake
smoke_data_burke <- expand.grid(grid_id_10km = grid_10km$ID, date = dates_smoke)
smoke_data_burke <- left_join(smoke_data_burke, smoke_predicts, by = c("grid_id_10km", "date"))

#predict 0 for remaining grid cells on non-smoke days
smoke_data_burke = mutate(smoke_data_burke, smokePM_pred = replace_na(smokePM_pred, 0))

#create week id
smoke_data_burke <- collapse::join(smoke_data_burke, unq_dates,
                                   how = "left", on = "date")

#drop data for dates not included in main dataset
smoke_data_burke <- smoke_data_burke %>% filter(!is.na(week_id))

#clear up some memory
rm(smoke_predicts, dates_smoke)
gc()


##calculate weekly mean pm2.5 and modal aqi ca

#first create aqi categories
smoke_data_burke <- smoke_data_burke %>%
  mutate(aqi_cat = pm25_to_aqi_category(smokePM_pred))

#group by week and grid cell and calculate mean pm2.5 and modal aqi cat, etc.
smoke_data_burke_weekly <- smoke_data_burke %>%
  group_by(grid_id_10km, week_id) %>%
  summarise(mean_smoke_pm2.5_weekly = mean(smokePM_pred, na.rm = T),
            modal_aqi_cat = mode_cat(aqi_cat),
            dummy_unhealthy_or_worse = as.integer(
              any(aqi_cat %in% c("Unhealthy", "Very Unhealthy or Hazardous"), na.rm = TRUE)),
            worst_aqi_cat_week = worst_cat(aqi_cat))

#extract weekly mean pm2.5 values at the safegraph poi's
#convert to same crs
unq_placekeys <- st_transform(st_as_sf(unq_placekeys), crs = 4326)
grid_10km <- st_transform(grid_10km, st_crs(unq_placekeys))

#match safegraph placekeys to grid_10km id's based upon intersections
placekey_grid_df <- st_join(unq_placekeys, grid_10km, join = st_within)
placekey_grid_df <- placekey_grid_df %>%
  dplyr::select(PLACEKEY, ID) %>%
  st_drop_geometry() %>%
  rename(grid_id_10km = ID)

#expand the df to accomodate week_ids
placekey_grid_df <- placekey_grid_df %>%
  crossing(week_id = min(unq_dates$week_id):max(unq_dates$week_id))

#merge with the pm2.5 data 
pm25_data_weekly <- collapse::join(placekey_grid_df, smoke_data_burke_weekly,
                          how = "left", on = c("week_id", "grid_id_10km"))

#drop grid id column
pm25_data_weekly <- pm25_data_weekly %>%
  dplyr::select(-c(grid_id_10km))

#save 
saveRDS(pm25_data_weekly, "./data/processed_data/poi_burke_pm25_weekly.RDS")

#remove some stuff to save RAM
rm(placekey_grid_df, smoke_data_burke_weekly, smoke_data_burke)
gc()

}

#### calculate fire distances to the POIs 
{
#load fire points and filter dates
fire_data <- readRDS("./data/processed_data/fire_buff.RDS")
fire_data <- fire_data %>%
  filter(as.Date(fire_detect_UTC) %in% unq_dates$date)
fire_data <- st_as_sf(fire_data)

#make placekey data spatial and coincide crs
unq_placekeys <- st_as_sf(unq_placekeys, crs = 4326)


#transform fire_data to correct crs and make valid
fire_data <- st_transform(fire_data, crs = crs(unq_placekeys))

validity_test <- st_is_valid(fire_data)
valid_fires <- fire_data[validity_test,]
corrected_fires <- st_make_valid(fire_data[!validity_test,])
fire_data <- rbind(valid_fires, corrected_fires)

#take centroids of the fire points for faster processing at scale.
fire_data <- st_centroid(fire_data)

#create week_id's for fires
fire_data <- fire_data %>%
  mutate(fire_detect_UTC = as.Date(fire_detect_UTC)) %>%
  left_join(unq_dates, by = c("fire_detect_UTC" = "date"))

#bring in POI-level data to get zip codes of the POIs for faster distance calculations
Total_POIs <- readRDS("./data/processed_data/safegraph_POIs.RDS")

Total_POIs <- Total_POIs %>%
  dplyr::select(PLACEKEY, POSTAL_CODE, REGION) %>%
  distinct()

#merge with safegraph spending unique polygons
unq_placekeys_w_zcta <- unq_placekeys %>%
  left_join(
    Total_POIs %>% dplyr::select(PLACEKEY, POSTAL_CODE),
    by = "PLACEKEY"
  )

#load zcta boundaries
options(tigris_use_cache = T)
zctas_shp <- tigris::zctas(
  year    = 2020,
  class   = "sf",
  cb      = FALSE    
)
zctas_shp <- st_transform(zctas_shp, crs = crs(fire_data))

#identify matching placekeys
matching_placekeys <- unq_placekeys_w_zcta %>%
  filter(POSTAL_CODE %fin% zctas_shp$GEOID20)

#identify placekeys that are missing a zip code match
nonmatching_placekeys <- unq_placekeys_w_zcta %>%
  filter(!POSTAL_CODE %fin% zctas_shp$GEOID20) 

#isolate points to intersect with the zctas
nonmatching_zctas <- nonmatching_placekeys %>%
  group_by(POSTAL_CODE) %>%
  slice_head(n = 1) %>%
  ungroup()

#intersect safegraph points with zctas to assign safegraph pois a new zip code
nonmatching_zctas_int <- st_within(nonmatching_zctas, zctas_shp)
first_idx <- sapply(nonmatching_zctas_int, '[', 1) #
nonmatching_zctas <- nonmatching_zctas %>%
  mutate(new_zip_code = zctas_shp$GEOID20[first_idx]) %>%
  st_drop_geometry() %>%
  dplyr::select(-c(PLACEKEY)) %>%
  st_drop_geometry()

#merge back in with the original missing placekeys
nonmatching_placekeys <- merge(nonmatching_placekeys, nonmatching_zctas,
                               by = "POSTAL_CODE")
nonmatching_placekeys <- nonmatching_placekeys %>%
  dplyr::select(-c(POSTAL_CODE)) %>%
  st_drop_geometry()

#merge back into data that is not missing zipcodes
matching_placekeys <- matching_placekeys %>%
  st_drop_geometry() %>%
  rename(new_zip_code = POSTAL_CODE)

update_placekeys <- rbind(nonmatching_placekeys, matching_placekeys)
unq_placekeys_w_zcta <- merge(unq_placekeys_w_zcta, update_placekeys,
                              by = "PLACEKEY")
unq_placekeys_w_zcta <- unq_placekeys_w_zcta %>%
  dplyr::select(-c(POSTAL_CODE))

#filter to only include zip code polygons that match unq_placekey zip codes
#(some NA's because of non overlapping points or empty geometry)
zctas_shp_filt <- zctas_shp %>%
  filter(GEOID20 %fin% unq_placekeys_w_zcta$new_zip_code)

#take centroids to speed up computation
zctas_centroids <- st_centroid(zctas_shp_filt)
zctas_centroids <- zctas_centroids %>%
  dplyr::select(GEOID20, geometry)


#repeat for counties
counties_shp <- tigris::counties(
  year = 2020,
  class = "sf",
  cb = T
)
counties_shp <- st_transform(counties_shp, crs = crs(fire_data))

##calculate if fire intersects county that zcta is located in

#tag zcta centroid with its county
zctas_centroids <- zctas_centroids %>%
  st_join(counties_shp %>% dplyr::select(county_fips = GEOID), join = st_within)

#Build a county‐week fire dummy
county_fires <- fire_data %>%
  st_join(counties_shp %>% dplyr::select(county_fips = GEOID), join = st_within) %>%
  st_drop_geometry() %>%
  distinct(county_fips, week_id) %>%
  mutate(fire_in_county = 1L) %>%
  filter(!is.na(county_fips)) #any fires outside of US counties in counties_shp will get dropped

# build a ZCTA‐week 100 km fire dummy (takes awhile)
zcta_dist100k <- st_join(
  zctas_centroids %>% dplyr::select(GEOID20),
  fire_data %>% dplyr::select(week_id),
  join = function(z, f) st_is_within_distance(z, f, dist = 100000),
  left = FALSE) %>%
  st_drop_geometry() %>%
  distinct(GEOID20, week_id) %>%
  mutate(fire_within_100km = 1L)

#assemble into one df

#create dataframe with all zcta-week combos
zctas_weeks <- expand.grid(
  GEOID20 = zctas_centroids$GEOID20,
  week_id = unique(fire_data$week_id),
  stringsAsFactors = FALSE
) %>% as_tibble()

#merge in county dummy
zctas_weeks <- zctas_weeks %>%
  left_join(zctas_centroids %>% st_drop_geometry() %>% dplyr::select(GEOID20, county_fips),
            by = "GEOID20") %>%
  left_join(county_fires, by = c("county_fips", "week_id")) %>%
  mutate(fire_in_county = if_else(is.na(fire_in_county), 0L, fire_in_county))

#merge in 100km dummy
zctas_weeks <- zctas_weeks %>%
  left_join(zcta_dist100k, by = c("GEOID20", "week_id")) %>%
  mutate(fire_within_100km = if_else(is.na(fire_within_100km), 0L, fire_within_100km))

#clean up for merge
zctas_weeks <- zctas_weeks %>%
  dplyr::select(-c(county_fips)) %>%
  rename(new_zip_code = GEOID20)

unq_placekeys_w_zcta <- unq_placekeys_w_zcta %>% 
  st_drop_geometry()

#merge

#build placekey-week-grid
placekey_week_grid <- unq_placekeys_w_zcta %>%
  distinct(PLACEKEY) %>%
  crossing(week_id = unique(zctas_weeks$week_id))

#bring in zip codes
placekey_week_grid <- placekey_week_grid %>%
  left_join(unq_placekeys_w_zcta, by = "PLACEKEY")

#bring in fire data
placekey_week_grid <- placekey_week_grid %>%
  left_join(zctas_weeks, by = c("new_zip_code", "week_id"))

#rename
fire_data_main <- placekey_week_grid
rm(placekey_week_grid)
gc()

#save results
saveRDS(fire_data_main, "./data/processed_data/safegraph_weekly_fire_data.RDS")

}

#### create county identifier for the POIs
{
  #load county shapefile
  counties_sf <- tigris::counties(cb = TRUE, year = 2023)  
  counties_sf <- st_transform(counties_sf, crs = crs(unq_placekeys))
  
  # spatial join: assigns county attributes to each point
  points_with_county <- st_join(unq_placekeys, counties_sf, join = st_within)
  
  #a few (n = 157) POIs appear right outside of county shp (usually coastal/lakes/islands), fix
  counties_sf_buff <- st_buffer(counties_sf, dist = 1500)
  missing_counties <- points_with_county %>% filter(is.na(GEOID)) %>% dplyr::select(PLACEKEY)
  missing_counties <- st_join(missing_counties, counties_sf_buff, join = st_within) %>%
    group_by(PLACEKEY) %>%
    slice_head(n = 1) %>% #one POI has two overlapping counties, choose first
    ungroup()
  
  #rejoin the two
  points_with_county <- points_with_county %>%
    filter(!is.na(GEOID))
  points_with_county <- rbind(points_with_county, missing_counties)
  
  # keep only relevant cols
  points_with_county <- points_with_county %>%
    dplyr::select(PLACEKEY, GEOID) %>%
    rename(county_fips = GEOID) %>%
    st_drop_geometry

}

#### calculate travel time to nearest park
{
  #load nps polygons
  poly_data <- readRDS("./data/processed_data/nps_data.RDS")
  poly_data <- st_transform(poly_data, crs = crs(unq_placekeys))
  
  #load nps POI's to get entrance and visitor stations
  nps_pois <-  st_read("./data/spatial_bound_data/NPS_poi/NPS_-_Points_of_Interest_(POIs)_-_Geographic_Coordinate_System.shp")
  nps_pois <- nps_pois %>%
    filter(!st_is_empty(geometry))
  nps_pois <- st_transform(nps_pois, crs = crs(unq_placekeys))
  
  #filter to include entrances or visitor station-related buildings
  nps_pois <- nps_pois %>%
    filter(POITYPE == "Entrance Station" | POITYPE == "Visitor Center" ) %>%
    mutate(geometry = st_zm(geometry, "Z"))
  
  #crop by park
  nps_pois <- st_transform(nps_pois, crs(poly_data))
  nps_pois <- st_intersection(nps_pois, poly_data)
  
  #north cascades has no listed POIS that match, use centroid
  nc_centroid <- st_centroid(poly_data %>% filter(UNIT_NAME == "North Cascades National Park"))
  nps_pois <- nps_pois %>%
    dplyr::select(UNITNAME)
  nc_centroid <- nc_centroid %>%
    dplyr::select(UNIT_NAME) %>%
    rename(UNITNAME = UNIT_NAME)
  nps_pois <- rbind(nps_pois, nc_centroid)
  nps_pois <- st_join(nps_pois, st_buffer(poly_data["UNIT_NAME"],1000))
  nps_pois <- nps_pois %>%
    dplyr::select(-c(UNITNAME))
  
  #convert to lat-lon for osrm package
  nps_coords <- nps_pois %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  #add id column to nps coords 
  nps_coords <- nps_coords %>%
    mutate(id = 1:nrow(nps_coords))
  
  #convert POI geometryto lat-lon coordinates for osrm package
  unq_POI_coords <- unq_placekeys %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  #merge in nearest park info
  unq_placekeys_w_park <- safegraph_data %>%
    st_drop_geometry() %>%
    dplyr::select(PLACEKEY, nearest_park) %>%
    group_by(PLACEKEY) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  unq_POI_coords <- merge(unq_POI_coords, unq_placekeys_w_park, by = "PLACEKEY")
  
  #loop through and calculate distances (parallizing runs into errors with OSRM package)
  unq_parks <- sort(unique(unq_POI_coords$nearest_park))
  
  storage_list_i <- vector("list", length = length(unq_parks))
  for (i in 1:length(unq_parks)){
    
    #identify entrance stations to park i
    park_i_coords <- nps_coords %>% filter(UNIT_NAME == unq_parks[i])
    
    #index all POIs with park_i as nearest park
    unq_POIS_i_coords <- unq_POI_coords %>% filter(nearest_park == unq_parks[i])
    
    #divide POIs into chunks
    POI_i_chunks <- split(unq_POIS_i_coords, ceiling(seq_len(nrow(unq_POIS_i_coords))/50))
    
    storage_list_j <- vector("list", length = length(POI_i_chunks))
    for(j in 1:length(POI_i_chunks)){
      chunk_j <- POI_i_chunks[[j]]
      
      #compute distances and durations
      result <- osrm::osrmTable(src = chunk_j, dst = park_i_coords,
                                 measure = c("duration", "distance"))
      if (!is.null(result)) {
        
        # Convert duration matrix to long format
        duration_df <- as.data.frame(result$duration)
        duration_df <- duration_df %>%
          mutate(travel_time = apply(., 1, min)) %>%
          dplyr::select(travel_time)
        duration_df <- cbind(PLACEKEY = chunk_j$PLACEKEY, duration_df)
        
        # Convert distance matrix to long format
        distance_df <- as.data.frame(result$distance)
        distance_df <- distance_df %>%
          mutate(travel_distance = apply(., 1, min)) %>%
          dplyr::select(travel_distance)
        distance_df <- cbind(PLACEKEY = chunk_j$PLACEKEY, distance_df)
        
        # Merge duration and distance
        merged_df <- cbind(duration_df, travel_distance = distance_df[,"travel_distance"])
        
        #store in list
        storage_list_j[[j]] <- merged_df
      }
    }
    #combine list from park i and re-add park name
    results_i <- rbindlist(storage_list_j)
    results_i <- results_i %>% mutate(nearest_park = park_i_coords$UNIT_NAME[1])
    
    #store
    storage_list_i[[i]] <- results_i
    
    #print progress
    print(i)
  }
  
  #combine into one main df
  POI_park_travel_data <- rbindlist(storage_list_i)
  
  #replace distances/times for POIs within parks with 0
  POIs_inside <- st_join(unq_POI_coords, poly_data["UNIT_NAME"], st_within)
  POIs_inside <- POIs_inside %>%
    filter(!is.na(UNIT_NAME)) %>%
    mutate(
      travel_time = 0,
      travel_distance = 0,
      park_POI = paste0(UNIT_NAME, "_", PLACEKEY)
    ) %>%
    st_drop_geometry() %>%
    dplyr::select(c(PLACEKEY, nearest_park, travel_distance, travel_time, park_POI))
  
  POI_park_travel_data <- POI_park_travel_data %>%
    mutate(park_POI = paste0(nearest_park, "_", PLACEKEY)) %>%
    filter(!park_POI %fin% POIs_inside$park_POI)
  
  POI_park_travel_data <- rbind(POI_park_travel_data, POIs_inside)
  
  #save output for merging later (missing Channel Island park because on island)
  saveRDS(POI_park_travel_data, "./data/processed_data/POI_travel_times.RDS")
}

#### create gateway community indicator
{
#add in gateway community indicator following stoker et al (2020)
# Define parameters
year <- 2020

# Get population for all places in U.S.
us_pop <- get_decennial(
  geography = "place",
  variables = "P1_001N",  # Total population
  year = year,
  geometry = FALSE
)

#Get list of states (to iterate over TIGER place geometries)
state_list <- unique(fips_codes$state)

#  Download TIGER place boundaries by state and combine
safe_places_download <- function(st, attempts = 3) {
  cache_file <- file.path(tempdir(), paste0("tl_2023_", st, "_place.gpkg"))
  
  # Return cached version if exists
  if (file.exists(cache_file)) {
    message("Using cached ", st)
    return(st_read(cache_file, quiet = TRUE))
  }
  
  for (i in 1:attempts) {
    message("Downloading ", st, " (try ", i, ")")
    try({
      pl <- tigris::places(state = st, year = 2023, cb = FALSE)
      st_write(pl, cache_file, quiet = TRUE, delete_dsn = TRUE)
      message("Success: ", st)
      return(pl)
    }, silent = TRUE)
    Sys.sleep(3)
  }
  message("Skipping ", st, " — failed after ", attempts, " attempts.")
  Sys.sleep(2)
  return(NULL)
}

us_places_list <- lapply(state_list, safe_places_download)

# Drop NULLs
us_places_list <- us_places_list[!sapply(us_places_list, is.null)]

# Merge 
us_places <- bind_rows(
  lapply(us_places_list, st_drop_geometry)
)

# Merge geometries separately and reattach
geom_list <- lapply(us_places_list, st_geometry)
geom_all <- do.call(c, geom_list)
us_places <- st_as_sf(us_places, geometry = geom_all, crs = 4269) 

#Join population data
us_places_pop <- us_places %>%
  left_join(us_pop %>% dplyr::select(GEOID, population = value), by = "GEOID")

#Filter by population threshold
us_places_over150 <- us_places_pop %>%
  filter(!is.na(population), population >= 150 & population < 25000)

#identify those that are within 10 miles of a national park

#load park data and buffer to 10 miles
nps_data <- readRDS("./data/processed_data/nps_data.RDS")
nps_data <- nps_data %>% filter(UNIT_NAME %fin% safegraph_data$nearest_park)
nps_data_buff <- st_buffer(nps_data, dist = (16093.4*1))

#intersect with parks to see if close
us_places_over150 <- st_transform(us_places_over150, crs = crs(nps_data_buff))
us_places_over150_int <- st_intersects(us_places_over150, nps_data_buff)
us_places_over150_nps <- us_places_over150[lengths(us_places_over150_int) != 0,]

# Filter to >15 miles from any urban area ---

#define urban places as more than 50000
high_pop_places <- us_places_pop %>%
  filter(population > 50000)
high_pop_places <- st_transform(high_pop_places, crs = crs(us_places_over150_nps))

us_places_over150_nps_buffer <- st_buffer(us_places_over150_nps, dist = 16093.4)
places_urban_int <- st_intersects(us_places_over150_nps_buffer, high_pop_places)
us_places_over150_nps <- us_places_over150_nps[lengths(places_urban_int) == 0,]

#identify pois within these communities

#intersect pois with the places
us_places_over150_nps <- st_transform(us_places_over150_nps, crs = crs(unq_placekeys))
us_places_over150_nps <- st_buffer(us_places_over150_nps, 1000)
poi_places_int <- st_intersects(unq_placekeys, us_places_over150_nps)

#create indicator
placekey_gateway_grid <- data.frame(
  PLACEKEY = unq_placekeys$PLACEKEY,
  gateway_dummy = as.integer(lengths(poi_places_int) > 0)
)

}

#### identify gateway communities through Google search trends
{

#get search keywords
parks <- unique(safegraph_data$nearest_park) #trends automatically lower case
geo_scope <- "US"
time_window <- "2019-01-01 2023-01-01"

#create function to call gtrends and retry if timeout hit
safe_gtrends <- function(keyword, tries = 5, pause = 8) {
  for (i in seq_len(tries)) {
    res <- try(
      gtrends(keyword = keyword,
              geo = "US",
              time = time_window,
              low_search_volume = TRUE),
      silent = TRUE
    )
    if (!inherits(res, "try-error")) return(res)
    cat("Retry", i, "for", keyword, "after timeout...\n")
    Sys.sleep(pause * i)  # exponential backoff
  }
  cat("Failed to fetch data for", keyword, "\n")
  return(NULL)
}

#loop through park names and get the top 25 places
gtrends_data <- vector("list", length(parks))

for (j in seq_along(parks)) {
  cat("Processing:", parks[j], "\n")
  res <- safe_gtrends(parks[j])
  if (!is.null(res) && !is.null(res$interest_by_city)) {
    gtrends_data[[j]] <- res$interest_by_city[1:25, ]
  } else {
    gtrends_data[[j]] <- data.frame()  # empty placeholder
  }
  Sys.sleep(3)  # avoid rate-limit
}

#need second loop for periodic empty values
is_empty <- function(x) is.null(x) || (is.data.frame(x) && nrow(x) == 0)

empty_idx <- which(vapply(gtrends_data, is_empty, logical(1)))

# beefier retry for empties
for (idx in empty_idx) {
  cat("Retrying:", parks[idx], "\n")
  res <- safe_gtrends(parks[idx], tries = 6, pause = 10)  # more tries, longer pause
  if (!is.null(res) && !is.null(res$interest_by_city) && nrow(res$interest_by_city) > 0) {
    gtrends_data[[idx]] <- res$interest_by_city[1:min(25, nrow(res$interest_by_city)), ]
  } else if (!is.null(res) && !is.null(res$interest_by_dma) && nrow(res$interest_by_dma) > 0) {
    # optional: DMA fallback so you still capture something numeric
    gtrends_data[[idx]] <- res$interest_by_dma[1:min(25, nrow(res$interest_by_dma)), ]
  } else {
    cat("Still no data for:", parks[idx], "\n")
  }
  Sys.sleep()
}

#combine
gtrends_cities <- rbindlist(gtrends_data)

#filter us places data to match cities
us_places_filter <- us_places %>%
  mutate(city_name = str_to_lower(NAME)) %>%
  filter(city_name %fin% str_to_lower(gtrends_cities$location))

#keep those within 100 miles distance of park
nps_data_buff_60miles <- st_buffer(nps_data, 96560.4)
nps_data_buff_60miles <- st_transform(nps_data_buff_60miles, crs = crs(us_places_filter))
nps_gtrends_int <- st_intersects(us_places_filter, nps_data_buff_100miles)
us_places_filter_nps <- us_places_filter[lengths(nps_gtrends_int) != 0,]

#now filter for 90 minutes travel time

#load nps POI's to get entrance and visitor stations
nps_pois <-  st_read("./data/spatial_bound_data/NPS_poi/NPS_-_Points_of_Interest_(POIs)_-_Geographic_Coordinate_System.shp")

#filter to include entrances or visitor station-related buildings
nps_pois <- nps_pois %>%
  filter(POITYPE == "Entrance Station" | POITYPE == "Visitor Center" ) %>%
  mutate(geometry = st_zm(geometry, "Z"))

#crop by park
nps_pois <- st_transform(nps_pois, crs(nps_data))
nps_pois <- st_intersection(nps_pois, nps_data)

#north cascades has no listed POIS that match, use centroid
nc_centroid <- st_centroid(nps_data %>% filter(UNIT_NAME == "North Cascades National Park"))
nps_pois <- nps_pois %>%
  dplyr::select(UNITNAME)
nc_centroid <- nc_centroid %>%
  dplyr::select(UNIT_NAME) %>%
  rename(UNITNAME = UNIT_NAME)
nps_pois <- rbind(nps_pois, nc_centroid)
nps_pois <- st_join(nps_pois, st_buffer(nps_data["UNIT_NAME"],1000))
nps_pois <- nps_pois %>%
  dplyr::select(-c(UNITNAME))

#convert to lat-lon for osrm package
nps_coords <- nps_pois %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

#create centroids for the places
us_places_filter_nps_centroids <- st_centroid(us_places_filter_nps)
us_places_filter_nps_centroids <- st_transform(us_places_filter_nps_centroids, crs = crs(nps_data))

#convert to lat-lon coordinates for osrm package
us_places_filter_nps_centroids <- us_places_filter_nps_centroids %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

#add id column to nps coords 
nps_coords <- nps_coords %>%
  mutate(id = 1:nrow(nps_coords))

#flag CBG inside of parks
centroids_inside <- st_join(us_places_filter_nps_centroids, nps_data["UNIT_NAME"], st_within)
centroids_inside <- centroids_inside %>%
  filter(!is.na(UNIT_NAME)) %>%
  mutate(
    travel_time = 0,
    park_id_GEOID = paste0(UNIT_NAME, "_", GEOID)
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID, park = UNIT_NAME, travel_time, park_id_GEOID)

# Function to compute travel times and distances in chunks
options(osrm.server = "https://routing.openstreetmap.de/")
options(osrm.profile = "car")

process_osrm_chunk <- function(chunk) {
  tryCatch({
    result <- osrm::osrmTable(src = chunk, dst = nps_coords, measure = c("duration", "distance"))
    
    if (!is.null(result)) {
      # Convert duration matrix to long format
      duration_df <- as.data.frame(result$duration)
      colnames(duration_df) <- paste0(nps_coords$UNIT_NAME,"_",nps_coords$id)
      duration_df <- duration_df %>%  
        mutate(GEOID = chunk$GEOID) %>%
        pivot_longer(cols = -GEOID, names_to = "park", values_to = "travel_time") %>%
        separate(park, into = c("park", "id"), sep = "_") %>%
        group_by(GEOID, park) %>%
        slice_min(travel_time, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
        ungroup() %>%
        mutate(park_id_cbg = paste0(park,"_", id, "_", GEOID))

      return(duration_df)
    }
    return(NULL)
  }, error = function(e) {
    message("Error processing chunk: ", conditionMessage(e))
    return(NULL)
  })
}

#split into chunks for processing
centroid_chunks <- split(us_places_filter_nps_centroids, ceiling(seq_len(nrow(us_places_filter_nps_centroids))/50))

#run travel distance function and combine into one df
results_list <- map(centroid_chunks, process_osrm_chunk)
travel_data <- bind_rows(results_list)

#combine data for points inside and outside of parks
travel_data <- travel_data %>%
  dplyr::select(GEOID, park, travel_time) %>%
  mutate(park_id_GEOID = paste0(park, "_", GEOID)) %>%
  filter(!park_id_GEOID %fin% centroids_inside$park_id_GEOID)
travel_data <- rbind(travel_data, centroids_inside)

#keep only places centroids within 90 minutes travel time of park
travel_data_90_min <- travel_data %>%
  filter(travel_time <= 90)
us_places_filter_nps <- us_places_filter_nps %>%
  filter(GEOID %fin% travel_data_90_min$GEOID)

#now control for population of the nearby town
us_places_filter_nps <- us_places_filter_nps %>%
  left_join(us_pop %>% dplyr::select(GEOID, population = value), by = "GEOID") %>%
  filter(population <= 25000)

# Filter to >15 miles from any urban area ---

#define urban places as more than 50000
high_pop_places <- us_places_pop %>%
  filter(population > 50000)
high_pop_places <- st_transform(high_pop_places, crs = crs(us_places_filter_nps))

us_places_filter_nps_buff <- st_buffer(us_places_filter_nps, dist = 16093.4)
gtrends_places_urban_int <- st_intersects(us_places_filter_nps_buff, high_pop_places)
us_places_filter_nps <- us_places_filter_nps[lengths(gtrends_places_urban_int) == 0,]

#identify poi's that fall within these places

#intersect pois with the places
us_places_filter_nps <- st_transform(us_places_filter_nps, crs = crs(unq_placekeys))
us_places_filter_nps <- st_buffer(us_places_filter_nps, 1000)
poi_places_int_gtrends <- st_intersects(unq_placekeys, us_places_filter_nps)

#create indicator
placekey_gateway_grid_gtrends <- data.frame(
  PLACEKEY = unq_placekeys$PLACEKEY,
  gateway_dummy_gtrends = as.integer(lengths(poi_places_int_gtrends) > 0)
)
#save
saveRDS(placekey_gateway_grid_gtrends, "./data/processed_data/poi_gtrends_data.RDS")
}

#### merge all datasets together

#load previously executed data
weather_data <- readRDS("./data/processed_data/weather_data_weekly_POIs.RDS")
pm25_data_weekly <- readRDS("./data/processed_data/poi_burke_pm25_weekly.RDS")
fire_data_main <- readRDS("./data/processed_data/safegraph_fire_data.RDS")
POI_park_travel_data <- readRDS("./data/processed_data/POI_travel_times.RDS")
placekey_gateway_grid_gtrends <- readRDS("./data/processed_data/poi_gtrends_data.RDS")
gc()

#create grid of all placekey-week combinations
placekey_week_grid <- expand.grid(PLACEKEY = unique(safegraph_data$PLACEKEY),
                                  week_id = unique(safegraph_data$week_id))

#merge in spending data
safegraph_data <- safegraph_data %>%
  st_drop_geometry()
placekey_week_grid <- collapse::join(placekey_week_grid, safegraph_data,
                                     how = "left",
                                     on = c("PLACEKEY", "week_id"))

#merge in gateway dummy generic
placekey_week_grid <- collapse::join(placekey_week_grid, placekey_gateway_grid,
                                     how = "left",
                                     on = c("PLACEKEY"))

#merge in gateway dummy from gtrends
placekey_week_grid <- collapse::join(placekey_week_grid, placekey_gateway_grid_gtrends,
                                     how = "left",
                                     on = c("PLACEKEY"))

#merge in park-specific weekly variables
placekey_week_grid <- placekey_week_grid %>% 
  rename(park = nearest_park)

placekey_week_grid <- collapse::join(placekey_week_grid, park_visit_data_weekly_vars,
                                        how = "left", on = c("park", "week_id"))

#bring in pm2.5 at the POI
placekey_week_grid <- collapse::join(placekey_week_grid, pm25_data_weekly,
                                       how = "left", on = c("PLACEKEY", "week_id"))

#bring in weather at the POI
#filter for quicker merges
placekey_week_grid <- placekey_week_grid %>%
  mutate(placekey_weekid = paste0(PLACEKEY, week_id))
weather_df <- weather_df %>%
  mutate(placekey_weekid = paste0(PLACEKEY, week_id)) %>%
  filter(placekey_weekid %in% placekey_week_grid$placekey_weekid)
placekey_week_grid <- collapse::join(placekey_week_grid, weather_df,
                                        how = "left", on = c("PLACEKEY", "week_id"))

#bring in fire data
placekey_week_grid <- collapse::join(placekey_week_grid, fire_data_main,
                                        how = "left", on   = c("PLACEKEY", "week_id"))

#bring in county fips id's
placekey_week_grid <- collapse::join(placekey_week_grid, points_with_county,
                                     how = "left", on = c("PLACEKEY"))

#bring in travel times and distances
POI_park_travel_data <- POI_park_travel_data %>% dplyr::select(-nearest_park)

setDT(placekey_week_grid)
setDT(POI_park_travel_data)

placekey_week_grid[
  POI_park_travel_data,
  on = .(PLACEKEY),
  `:=`(
    travel_time    = i.travel_time,
    travel_distance = i.travel_distance
  )
]

#drop unneeded columns
placekey_week_grid <- placekey_week_grid %>%
  dplyr::select(-c(OPEN_HOURS, OPENED_ON, CLOSED_ON, TRACKING_CLOSED_SINCE))

#check data for missing vars
na_vars <- colSums(is.na(placekey_week_grid))

#safegraph data missing (36131029 rows = nrow(placekey_week_grid)-nrow(safegraph_data)) because
# many placekeys do not have data for all week_ids
# park data also missing for these same values because nearest park calculated before merge

#weather variables have some na's for POIs in Keys (FL) and sliver of Maine coast because GRIDMET
# data does not cover

#fire data has some missing variables because some POI's do not link to relevant zip codes

#park variables missing (beyond the missing safegraph variables, n = 1,156,721) because
#many parks dropped if they dont have full 7 days worth of data within a given week. If only one week of
# advan week is missing, this results in two weeks of safegraph-park data missing (could relax)

#save final dataset
saveRDS(placekey_week_grid, "./data/processed_data/safegraph_weekly_data_with_covariates.RDS")


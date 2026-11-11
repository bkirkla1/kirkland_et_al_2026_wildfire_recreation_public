#-------------------------------------------------------------------------------
#
# "06_advan_daily_covariates_process.R": this script takes POI points and
# attributes all rasterized pollution, weather, smoke_plume, 
# and fire distance data to the given POIs. It also adds in daily park-level 
# covariates
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#load business data
bus_data <- readRDS("./data/processed_data/advan_daily_business_visits.RDS")

#filter to just safegraph gateway communities for now
safegraph_data <- readRDS("./data/processed_data/main_weekly_safegraph_data_final.RDS")
safegraph_data <- safegraph_data %>% filter(gateway_dummy_gtrends == 1)
bus_data <- bus_data %>%
  filter(placekey %fin% unique(safegraph_data$PLACEKEY))
rm(safegraph_data)
gc()

#isolate unique placekey-lat-lon pairs
unq_placekeys <- bus_data %>%
  distinct(placekey, latitude, longitude)

#convert to sf object
unq_placekeys <- st_as_sf(
  unq_placekeys,
  coords = c("longitude", "latitude"),
  crs = 4326  # WGS84
)

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
time_zones <- st_transform(time_zones, crs(unq_placekeys))

#intersect poi points and time zones to get time zone for poi
time_zones <- st_make_valid(time_zones)
poi_time_int <- st_intersects(unq_placekeys, time_zones)
unq_placekeys <- st_join(unq_placekeys, time_zones["TIMEZONE"])

#someo points dont match, buffer and then re-add
unmatched_pois <- unq_placekeys[is.na(unq_placekeys$TIMEZONE), ]
unmatched_pois_buff <- st_buffer(unmatched_pois, dist = 1000)
unmatched_filled <- st_join(unmatched_pois_buff, time_zones["TIMEZONE"])
unq_placekeys$TIMEZONE[is.na(unq_placekeys$TIMEZONE)] <- unmatched_filled$TIMEZONE.y

#change time zone character to match lubridate conventions
convert_convention <- function(time_zone) {
  tz_map <- c("Eastern" = "America/New_York",
              "Central" = "America/Chicago",
              "Mountain" = "America/Denver",
              "Pacific" = "America/Los_Angeles")
  local_tz <- tz_map[time_zone]}

unq_placekeys$time_zone <- convert_convention(unq_placekeys$TIMEZONE)

##----weather data processing
{
  #load GRIDMET data
  years <- c(2019:2022)
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
  unq_placekeys <- st_transform(unq_placekeys, crs = GRIDMET_crs)
  
  #loop to calculate weather at the placekeys
  weather_data_list <- vector("list", length = length(years) * length(vars))
  counter <- 1
  
  for (i in seq_along(vars)) {
    for (j in seq_along(years)) {
      
      # load in gridmet rasters
      dir_ji <- paste0("./data/GRIDMET_data/", vars[i], "_", years[j], ".nc")
      rast_ji <- rast(dir_ji)
      
      # calculate daily mean at poi point
      mean_var <- extract(rast_ji, unq_placekeys, na.rm = TRUE)
      
      # attach placekeys
      mean_var[, 1] <- unq_placekeys$placekey
      mean_var <- mean_var %>% rename(placekey = ID)
      
      # reshape data
      long_data <- mean_var %>%
        pivot_longer(
          cols = -placekey,
          names_to = "var_descript",
          values_to = "value"
        )
      
      # add dates
      dates_j <- seq(as.Date(paste0(years[j], "-01-01")),
                     as.Date(paste0(years[j], "-12-31")),
                     by = "day")
      long_data$date <- rep(dates_j, times = nrow(unq_placekeys))
      
      # add variable name (instead of renaming column)
      long_data <- long_data %>%
        mutate(variable = vars[i]) %>%
        dplyr::select(placekey, date, variable, value)
      
      # store results
      weather_data_list[[counter]] <- long_data
      counter <- counter + 1
    }
  }
  
  #combine all into one long df 
  weather_data <- bind_rows(weather_data_list)
  
  #pivot to wide format
  weather_data_wide <- weather_data %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    )
  
  #create unique dates-poi combinations to account for na's appropriately
  all_dates <- seq(as.Date(paste0(min(years), "-01-01")),
                   as.Date(paste0(max(years), "-12-31")),
                   by = "day")
  
  master_weather_df <- expand.grid(
    placekey = unique(unq_placekeys$placekey),
    date      = all_dates,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble()
  
  #merge
  setDT(weather_data_wide)
  setDT(master_weather_df)
  
  # set keys for fast join
  setkey(master_weather_df, placekey, date)
  setkey(weather_data_wide, placekey, date)
  
  # left join (keep all master rows, fill NAs where missing)
  weather_df <- weather_data_wide[master_weather_df]
  
  #save
  saveRDS(weather_df, './data/processed_data/advan_business_weather.RDS')
}

##----calculate distance to fires

##with all HMS data
{
  #load fire data  
  fire_data <- readRDS("./data/processed_data/fire_buff.RDS")
  fire_data <- fire_data %>%
    filter(as.Date(fire_detect_UTC) %in% dates_main)
  fire_data <- st_as_sf(fire_data)

  #calculate distances each day for pois
  dates_fire_point <- unique(fire_data$fire_detect_UTC)
  
  #pre-split fire data by date
  fire_data_list <- fire_data %>%
    group_split(fire_detect_UTC)
  fire_dist_list <- vector("list", length(dates_fire_point))
  
  #reproject unq placekeys
  unq_placekeys <- st_transform(unq_placekeys, crs = crs(fire_data))
  
  for (j in 1:length(dates_fire_point)){
    
    #filter fires to date j
    fire_j <- fire_data_list[[j]]
    fire_j <- st_as_sf(fire_j)
    
    #confirm crs match
    if (crs(fire_j) != crs(unq_placekeys)){break}
    
    #calculate closest fires in km
    dist_j <- st_distance(fire_j, unq_placekeys)
    fire_dist_km <- apply(dist_j, 2, min, na.rm = T)
    fire_dist_km <- round(fire_dist_km/1000, 3)

    #clean results and store
    results <- data.frame(cbind(unq_placekeys$placekey, fire_dist_km)) %>%
      mutate(date_LT = fire_j$fire_detect_UTC[1]) %>%
      rename(placekey = V1)
    fire_dist_list[[j]] <- results 
    
    #track progress
    print(fire_j$fire_detect_UTC[1])
  }
  
  #combine data
  fire_dist_centroid_df <- rbindlist(fire_dist_list)
  
  #create grid of all park-date combinations to hold onto park-dates with missing data (HMS)
  base_fire_df <- expand_grid(date_LT = as.character(dates_main), placekey = unique(fire_dist_centroid_df$placekey))
  base_fire_df <- base_fire_df %>%
    left_join(fire_dist_centroid_df,
              by = c("date_LT", "placekey"))
  
  #save
  saveRDS(base_fire_df, "./data/processed_data/business_fire_variables.RDS")
  
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
  fire_data_filt <- st_transform(fire_data_filt, crs = crs(unq_placekeys))

  #calculate distances each day for centroids and park borders using filtered hms points
  
  #define date range
  dates_fire_point_filt <- unique(fire_data_filt$fire_detect_UTC)
  
  #pre-split fire data by date
  fire_data_list_filt <- fire_data_filt %>%
    group_split(fire_detect_UTC)
  fire_dist_list_filt <- vector("list", length(dates_fire_point_filt))
  
  for (j in 1:length(dates_fire_point_filt)){
    
    #filter fires to date j
    fire_j <- fire_data_list_filt[[j]]
    fire_j <- st_as_sf(fire_j)
    
    #confirm crs match
    if (crs(fire_j) != crs(unq_placekeys)){break}
    
    #calculate closest fires in km
    dist_j <- st_distance(fire_j, unq_placekeys)
    fire_dist_km <- apply(dist_j, 2, min, na.rm = T)
    fire_dist_km <- round(fire_dist_km/1000, 3)
    
    #get area of the closest fire
    # find which fire is closest for each park
    idx_cent <- apply(dist_j, 2, which.min)
    # extract the matching area
    area_cent <- round(as.numeric(fire_j$area[idx_cent])/1000000,2) 
    
    #clean results and store
    results <- data.frame(
      placekey                 = unq_placekeys$placekey,
      fire_dist_km   = fire_dist_km,
      nearest_area_km2   = area_cent,
      date_LT              = fire_j$fire_detect_UTC[1]
    )
    fire_dist_list_filt[[j]] <- results 
    
    #track progress
    print(fire_j$fire_detect_UTC[1])
  }
  
  #combine data
  fire_dist_centroid_df_filt <- rbindlist(fire_dist_list_filt)

  #some days are not represented (when no fire points detected in CONUS (no major wildfires, or data issues)
  #create outline of all park-date combinations
  main_full_filt <- fire_dist_centroid_df_filt %>%
    distinct(placekey) %>%
    crossing(date_LT = dates_main)
  
  main_fire_data_filt <- main_full_filt %>%
    left_join(fire_dist_centroid_df_filt, by = c("placekey", "date_LT"))
  
  #id dates without any fire data
  missing_dates <- main_fire_data_filt %>%
    filter(is.na(fire_dist_km)) %>%
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
  max_dist <- max(main_fire_data_filt$fire_dist_km, na.rm = TRUE) + 1
  
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
        ~ if_else(no_fire, max_dist, .),
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
    rename(fire_dist_km_filt = fire_dist_km,
           nearest_area_km2_filt = nearest_area_km2) %>%
    dplyr::select(-c(any_fire, no_file, no_fire))
  
  #save output
  saveRDS(main_fire_data_filt, "./data/processed_data/business_fire_variables_filtered.RDS")
  
}

##----process Burke's smoke-induced pm2.5 anomaly data
{
  
  #load smoke predictions
  smoke_predicts = readRDS("./data/pm2.5_data/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")
  
  #load 10km grid
  grid_10km = read_sf("./data/pm2.5_data/10km_grid_wgs84/10km_grid_wgs84.shp")
  
  #choose dates 2019 onward and filter data
  dates_smoke <- seq.Date(ymd("20190101"), ymd("20231231"), by = "day")
  
  #loop over one year at a time for memories sake
  smoke_data_burke <- expand.grid(grid_id_10km = grid_10km$ID, date = dates_smoke)
  smoke_data_burke <- left_join(smoke_data_burke, smoke_predicts, by = c("grid_id_10km", "date"))
  
  #predict 0 for remaining grid cells on non-smoke days
  smoke_data_burke = mutate(smoke_data_burke, smokePM_pred = replace_na(smokePM_pred, 0))
  
  #convert to same crs
  grid_10km <- st_transform(grid_10km, st_crs(unq_placekeys))

  #attach each POI to a 10km grid cell (point-in-polygon)
  poi_to_grid <- st_join(
    unq_placekeys[, c("placekey")],
    grid_10km[, c("ID")],
    left = TRUE
  ) %>% sf::st_drop_geometry()
  
  #create df of poi-date-pm2.5 combinations
  final_poi_smoke <- tidyr::crossing(
    poi_to_grid,                          
    date = dates_smoke                    
  ) %>%
    dplyr::mutate(date_lead1y = date %m+% years(1)) %>%   
    dplyr::left_join(
      smoke_data_burke %>%                
        dplyr::select(grid_id_10km, date, smokePM_pred),
      by = c("ID" = "grid_id_10km", "date" = "date")
    ) %>%
    dplyr::left_join(                                      
      smoke_data_burke %>%
        dplyr::select(grid_id_10km, date, smokePM_pred) %>%
        dplyr::rename(date_lead1y = date, smoke_pm25_lead1y = smokePM_pred),
      by = c("ID" = "grid_id_10km", "date_lead1y" = "date_lead1y")
    ) %>%
    dplyr::rename(mean_smoke_pm25 = smokePM_pred) %>%
    dplyr::arrange(placekey, date)
  
  

  #save results
  final_poi_smoke <- final_poi_smoke %>%
    dplyr::filter(date <= ymd("20221231")) %>%             
    dplyr::select(-ID, -date_lead1y) %>%                   
    rename(date_LT = date)
  
  saveRDS(final_poi_smoke, "./data/processed_data/business_burke_pm25.RDS")
  
}

##---- merge together 

#load individual datasets
weather_data <- readRDS("./data/processed_data/advan_business_weather.RDS")
fire_data <- readRDS("./data/processed_data/business_fire_variables.RDS")
fire_data_filtered <- readRDS("./data/processed_data/business_fire_variables_filtered.RDS")
burke_pm25_data <- readRDS( "./data/processed_data/business_burke_pm25.RDS")
epa_pm25_data <- readRDS("./data/processed_data/epa_business_pm25_data.RDS")

#load weekly final safegraph dataset to get information on nearest parks to a given placekey (alongside other time invariant variables)
safegraph_data <- readRDS("./data/processed_data/main_weekly_safegraph_data_final.RDS")
safegraph_data <- safegraph_data %>%
  dplyr::select(PLACEKEY, park, gateway_dummy, gateway_dummy_gtrends,
                nearest_park_dist_km, travel_distance, travel_time, county_fips,
                NAICS_CODE, LOCATION_NAME) %>%
  distinct()

## bring in daily park-level covariates from main visits dataset
daily_park_data <- readRDS("./data/processed_data/main_data_subset_final.RDS")
daily_park_data <- daily_park_data %>%
  dplyr::select(park, date_LT, tmmn, tmmx, pr, vs, wind_cat, day_of_week, month, year,
                mean_smoke_pm2.5, aqi_cat, fire_dist_km_bound, raw_daily_visits, Visits)

#convert all to datatables
setDT(weather_data)
setDT(fire_data)
setDT(fire_data_filtered)
setDT(burke_pm25_data)
setDT(epa_pm25_data)
setDT(safegraph_data)
setDT(daily_park_data)
setDT(bus_data)

#change variable names for merge
fire_data <- fire_data %>%
  mutate(date_LT = as.Date(date_LT))

weather_data <- weather_data %>%
  rename(date_LT = date)

epa_pm25_data <- epa_pm25_data %>%
  rename(date_LT = date)

safegraph_data <- safegraph_data %>%
  rename(placekey = PLACEKEY)

daily_park_data <- daily_park_data %>%
  rename(aqi_cat_park = aqi_cat,
         fire_dist_km_bound_park = fire_dist_km_bound,
         mean_smoke_pm2.5_park = mean_smoke_pm2.5,
         pr_park = pr,
         tmmn_park = tmmn,
         tmmx_park = tmmx,
         vs_park = vs,
         wind_cat_park = wind_cat)

bus_data <- bus_data %>%
  rename(date_LT = date_long) %>%
  dplyr::select(-c(city, visitor_home_aggregation))

#create grid to fll in
main_df <- setDT(expand.grid(placekey = unique(unq_placekeys$placekey), date_LT = dates_main))

#merge in data on by one
main_df <- main_df[weather_data, on = c("placekey", "date_LT")]
main_df <- main_df[fire_data,  on = c("placekey", "date_LT")]
main_df <- main_df[fire_data_filtered,  on = c("placekey", "date_LT")]
main_df <- main_df[burke_pm25_data, on = c("placekey", "date_LT")]
main_df <- main_df[safegraph_data, on = c("placekey")]
main_df <- main_df[daily_park_data, on = c("park", "date_LT")]
main_df <- main_df[bus_data, on = c("placekey", "date_LT")]

#rename visit variables for clarity
main_df <- main_df %>%
  rename(
    raw_daily_visits_park = raw_daily_visits,
    normalized_daily_visits_park = Visits, 
    raw_daily_visits_poi = daily_visits,
    normalized_daily_visits_poi = normalized_daily_visits
  ) %>%
  dplyr::select(-c(normalized_visits_by_state_scaling, raw_visitor_counts))

##check out sources of NA's from merges
na_check <- colSums(is.na(main_df))

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



#### manually remove placekeys that don't match with the safegraph placekeys 
#arches -> greenboro, nc; isle royale -> richmond,va; jtree -> tampa, fl; crater lake -> little rock, ak

#load nps data
nps_data <- readRDS("./data/processed_data/nps_data.RDS")
nps_data <- nps_data %>% rename(park = UNIT_NAME)

#get unique pl-park pairs
unique_pairs <- main_df %>%
  distinct(placekey, park, latitude, longitude) %>%
  left_join(nps_data %>% dplyr::select(park), by = "park")  # brings geometry along

# Separate into POI sf and park sf
poi_sf <- unique_pairs %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

park_sf_matched <- unique_pairs %>%
  st_as_sf() %>%               
  st_transform(st_crs(poi_sf))

#calculate element-wise distance
unique_pairs$dist_km <- as.numeric(
  st_distance(poi_sf, park_sf_matched, by_element = TRUE)
) / 1000

unique_pairs$within_250km <- unique_pairs$dist_km <= 250

#drop the na parks
unique_pairs <- unique_pairs %>%
  filter(!is.na(park)) %>%
  st_drop_geometry() %>%
  dplyr::select(-c(geometry, dist_km, latitude, longitude))

#Join back
main_df <- main_df %>%
  left_join(unique_pairs,
            by = c("placekey", "park"))

#drop the odd mismatched placekeys
main_df <- main_df %>% 
  filter(within_250km == TRUE) %>%
  dplyr::select(-within_250km)

## save results
saveRDS(main_df, "./data/processed_data/advan_daily_business_data_w_covariates.RDS")



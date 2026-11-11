#-------------------------------------------------------------------------------
#
# '02_safegraph_weekly_placekey_filter.R': this script reads in Safegraph data
# and processes the dataset at a weekly resolution
#
#-------------------------------------------------------------------------------

#load relevant packages
source('scripts/setup.R')

#### FILTER POI LOCATION DATA FROM SAFEGRAPH TO CREATE BASELINE SET OF POIs

#load park shapefiles
nps_data <- readRDS('./data/processed_data/nps_data.RDS')
nps_data <- st_transform(nps_data, 4326)

#simplify data for processing speed
nps_data <- st_simplify(nps_data, dTolerance = 2000)

#buffer park data 
buff_dist = 100000
nps_data_buff_100km <- st_buffer(nps_data, dist = buff_dist)

#read in POI data into one dataframe
POI_file_list <- list.files("./data/safegraph_POI_data", full.names = T, recursive = T)

#read and filter data in parallel
plan(multisession, workers = 4)  

# Parallelized function to process each file
process_file <- function(file_j, buffered_data) {
  
  # Read the file
  data_j <- arrow::read_csv_arrow(file_j, read_options = list(block_size = 204800576L))
  
  # Convert to sf object
  data_j <- st_as_sf(data_j, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  # Intersect 
  park_int <- st_intersects(data_j, buffered_data)
  park_poi_data <- data_j[lengths(park_int) > 0, ]
  
  return(park_poi_data)
}

# Use future_lapply for parallel processing
POI_data_list <- future_lapply(POI_file_list, process_file,
                               buffered_data = nps_data_buff_100km,
                               future.seed = TRUE)

#combine into one df 
Total_POIs <- rbindlist(POI_data_list)
rm(POI_data_list)

#save the POIs in question
saveRDS(Total_POIs, "./data/processed_data/safegraph_POIs.RDS")

#### REPEAT WITH SPENDING DATA, FILTERING BY IDENTIFIED POIS

##repeat for spend data, but filter by POI placekeys
spend_file_list <- list.files("./data/safegraph_data", recursive = T, full.names = T)
unq_placekeys <- unique(Total_POIs$PLACEKEY)

# Define the function to process each file
process_spend_file <- function(file_i, unq_placekeys) {
  
  # Read the file
  data_i <- arrow::read_csv_arrow(file_i, read_options = list(block_size = 204800576L))
  
  # Filter rows based on PLACEKEY
  data_i <- data_i %>% filter(PLACEKEY %fin% unq_placekeys)
  
  return(data_i)
}

# Parallelized future_lapply to process each file
spend_data_list <- future_lapply(spend_file_list,
                                 FUN = process_spend_file,
                                 unq_placekeys = unq_placekeys)

# close worker
plan(sequential)

#combine and save
spend_data_df <- bind_rows(spend_data_list)
rm(spend_data_list)

#save raw data to be used in daily aggregation in other script
saveRDS(spend_data_df, "./data/processed_data/raw_safegraph_data.RDS")


#### CONVERT DATA TO LONG FORMAT BY WEEK

#first scale to daily
spend_data_long <- spend_data_df %>%
  mutate(
    spend_list = str_extract_all(SPEND_BY_DAY, "\\d+(?:\\.\\d+)?") %>% map(as.numeric),
    spend_per_txn_list = str_extract_all(SPEND_PER_TRANSACTION_BY_DAY, "\\d+(?:\\.\\d+)?") %>% map(as.numeric),
    date_seq = map2(SPEND_DATE_RANGE_START, SPEND_DATE_RANGE_END, 
                    ~ seq.Date(as.Date(.x), as.Date(.y) - 1, by = "day"))
  ) %>%
  dplyr::select(PLACEKEY, date_seq, spend_list, spend_per_txn_list, online_) %>%
  unnest(c(date_seq, spend_list, spend_per_txn_list)) %>%
  rename(
    date_long              = date_seq,
    daily_spend            = spend_list,
    daily_spend_per_txn    = spend_per_txn_list
  )

## aggregate up to the weekly level. 

#create vector of dates that span whole range of dataset
spend_dates <- seq(min(spend_data_long$date_long),
                   max(spend_data_long$date_long),
                   by = "day")

#index tuesdays
spend_dates_dow <- wday(spend_dates)
ind_tues <- which(spend_dates_dow == 3)

#create storage dataframe for week id's
spend_dates <- data.frame(spend_dates)
colnames(spend_dates) <- c("date_long")

#assign day of week column
spend_dates$dow <- spend_dates_dow

#initialize week_id column
spend_dates$week_id <- NA
spend_dates$week_id[ind_tues] <- 1:length(ind_tues)

#loop over to fill in values between Tuesdays and create start-stop weeks
for (i in 1:nrow(spend_dates)){
  spend_dates$week_id[i] <- ifelse(is.na(spend_dates$week_id[i]),
                                   spend_dates$week_id[i-1],
                                   spend_dates$week_id[i]) 
  
}

#drop weeks in time sample (2019-2023) that don't have complete weeks
complete_weeks_base <- spend_dates %>%
  group_by(week_id) %>%
  summarise(num_days = n()) %>%
  ungroup() %>%
  filter(num_days == 7)

spend_dates <- spend_dates %>%
  filter(week_id %fin% complete_weeks_base$week_id)

#create week start date column
tues_dates <- spend_dates$date_long[spend_dates$dow == 3]
rep_dates <- rep(tues_dates, each = 7)

spend_dates$week_start_date <- rep_dates

#create week end dates column
end_week_dates <- tues_dates+6
rep_end_dates <- rep(end_week_dates, each = 7)
spend_dates$week_end_date <- rep_end_dates

#simplify
spend_dates <- spend_dates %>% dplyr::select(-c(dow))

#merge with main dataset
setDT(spend_data_long)
setDT(spend_dates)
spend_data_long <- spend_data_long[spend_dates, on = "date_long"]

#identify placekey-weeks without 7 full days of observation and drop
complete_weeks <- spend_data_long %>% 
  count(PLACEKEY, week_id, name = "num_days_in_week") %>%
  filter(num_days_in_week == 7)

#filter main data by complete weeks
spend_data_long <- spend_data_long %>%
  semi_join(complete_weeks, by = c("PLACEKEY", "week_id"))

#sum daily spend to placekey-week level
weekly_data <- spend_data_long %>%
  group_by(PLACEKEY, week_id, week_start_date, week_end_date) %>%
  summarise(weekly_spend = sum(daily_spend))

## identify which park(s) is within given distance and record

#filter to identify POI's with actual spend data
Total_POIs <- Total_POIs %>%
  filter(PLACEKEY %fin% weekly_data$PLACEKEY) %>%
  dplyr::select(PLACEKEY, LOCATION_NAME, NAICS_CODE, POSTAL_CODE, geometry) 

#convert to sf object
Total_POIs <- st_as_sf(Total_POIs, crs = 4326)

#identify parks within 250 km
park_dist <- 250000
POI_park_int <- st_intersects(Total_POIs, st_buffer(nps_data, park_dist))

#create json-formatted column of nearby park names
unit_names <- as.character(nps_data$UNIT_NAME)

Total_POIs$nearby_parks_250km <- unlist(
  lapply(POI_park_int, function(indexes) {
    toJSON(unit_names[indexes], auto_unbox = TRUE)
  }),
  use.names = FALSE
)

#identify closest park
Total_POIs$nearest_park <- unit_names[st_nearest_feature(Total_POIs, nps_data)]

#get distance of closest park
POI_park_dist = data.frame(st_distance(Total_POIs, nps_data))
min_dists <- apply(POI_park_dist, MARGIN = 1, FUN = min, na.rm = TRUE)/1000
Total_POIs$nearest_park_dist_km <- round(min_dists, 2)

#remerge with weekly data
setDT(weekly_data)
setDT(Total_POIs)
setkey(weekly_data, PLACEKEY)
setkey(Total_POIs, PLACEKEY)

# Merge in-place 
weekly_data <- Total_POIs[weekly_data]

#filter out irrelevant variables
weekly_data <- weekly_data %>%
  dplyr::select(-c(BRANDS, CATEGORY_TAGS, CITY, DOMAINS, GEOMETRY_TYPE, ISO_COUNTRY_CODE,
                   PHONE_NUMBER, POSTAL_CODE, REGION, SAFEGRAPH_BRAND_IDS, STORE_ID, 
                   STREET_ADDRESS, WEBSITE))

#save baseline weekly data
saveRDS(weekly_data, "./data/processed_data/safegraph_unfiltered_weekly_data.RDS")


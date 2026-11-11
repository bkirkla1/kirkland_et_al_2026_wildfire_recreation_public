#-------------------------------------------------------------------------------
#
# '02_tc_data_cbg_process.R': This script separates local vs. nonlocal 
# visitation and creates scaled weekly variables at the park level
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#### calculate distance between cbgs and parks (already run)
poly_data <- readRDS("./data/processed_data/nps_data.RDS")

{
#load nps polygons
poly_data <- readRDS("./data/processed_data/nps_data.RDS")

#load nps POI's to get entrance and visitor stations
nps_pois <-  st_read("./data/spatial_bound_data/NPS_poi/NPS_-_Points_of_Interest_(POIs)_-_Geographic_Coordinate_System.shp")

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

#load cbg shapefiles from tigris and create centroids
Sys.setenv(TIGRIS_CACHE_DIR = "./data/cbg_shapefiles")
cbgs_sf <- tigris::block_groups(year = 2019, cb = TRUE)

#filter by states in CONUS
conus_statefp <- c(
  "01", "04", "05", "06", "08", "09", "10", "11", "12", "13",
  "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
  "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
  "37", "38", "39", "40", "41", "42", "44", "45", "46", "47",
  "48", "49", "50", "51", "53", "54", "55", "56"
)

cbgs_sf <- cbgs_sf %>% 
  filter(STATEFP %in% conus_statefp)

cbgs_centroids <- st_centroid(cbgs_sf)
cbgs_centroids <- st_transform(cbgs_centroids, crs = crs(poly_data))

cbgs_centroids <- cbgs_centroids %>%
  dplyr::select(GEOID)

#convert to lat-lon coordinates for osrm package
cbgs_coords <- cbgs_centroids %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

#add id column to nps coords 
nps_coords <- nps_coords %>%
  mutate(id = 1:nrow(nps_coords))

#flag CBG inside of parks
cbgs_inside <- st_join(cbgs_centroids, poly_data["UNIT_NAME"], st_within)
cbgs_inside <- cbgs_inside %>%
  filter(!is.na(UNIT_NAME)) %>%
  mutate(
    travel_time = 0,
    travel_distance = 0,
    park_id_cbg = paste0(UNIT_NAME, "_", GEOID)
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(census_cbg = GEOID, park = UNIT_NAME, travel_time, travel_distance, park_id_cbg)

# Function to compute travel times and distances in chunks
process_osrm_chunk <- function(chunk) {
  tryCatch({
    result <- osrm::osrmTable(src = chunk, dst = nps_coords, measure = c("duration", "distance"))
    
    if (!is.null(result)) {
      # Convert duration matrix to long format
      duration_df <- as.data.frame(result$duration)
      colnames(duration_df) <- paste0(nps_coords$UNIT_NAME,"_",nps_coords$id)
      duration_df <- duration_df %>%  
        mutate(census_cbg = chunk$GEOID) %>%
        pivot_longer(cols = -census_cbg, names_to = "park", values_to = "travel_time") %>%
        separate(park, into = c("park", "id"), sep = "_") %>%
        group_by(census_cbg, park) %>%
        slice_min(travel_time, n = 1, with_ties = FALSE, na_rm = T) %>%
        ungroup() %>%
        mutate(park_id_cbg = paste0(park,"_",id,"_",census_cbg))
      
      # Convert distance matrix to long format
      distance_df <- as.data.frame(result$distance)
      colnames(distance_df) <- paste0(nps_coords$UNIT_NAME,"_",nps_coords$id)
      distance_df <- distance_df %>%
        mutate(census_cbg = chunk$GEOID) %>%
        pivot_longer(cols = -census_cbg, names_to = "park", values_to = "travel_distance") %>%
        mutate(park_id_cbg = paste0(park,"_",census_cbg)) %>%
        filter(park_id_cbg %in% duration_df$park_id_cbg) %>%
        dplyr::select(-c(census_cbg, park))

      # Merge duration and distance
      merged_df <- left_join(duration_df, distance_df, by = c("park_id_cbg"))
      return(merged_df)
    }
    return(NULL)
  }, error = function(e) {
    message("Error processing chunk: ", conditionMessage(e))
    return(NULL)
  })
}

#split into chunks for processing
cbg_chunks <- split(cbgs_coords, ceiling(seq_len(nrow(cbgs_coords))/50))

#run travel distance function and combine into one df
results_list <- map(cbg_chunks, process_osrm_chunk)
travel_data <- bind_rows(results_list)

#combine data for points inside and outside of parks
travel_data <- travel_data %>%
  dplyr::select(census_cbg, park, travel_time, travel_distance) %>%
  mutate(park_id_cbg = paste0(park, "_", census_cbg)) %>%
  filter(!park_id_cbg %fin% cbgs_inside$park_id_cbg)
travel_data <- rbind(travel_data, cbgs_inside)

#clean up memory
rm(results_list, cbg_chunks)
gc()

#save data
saveRDS(travel_data, "./data/processed_data/travel_dist_time_data.RDS")

}


#### create long formatted df with all park-cbg-week combinations for cbg's
{
#load weekly advan data from 'advan_placekey_filter.R"
visit_data <- readRDS("./data/processed_data/weekly_advan_data.RDS")

#expand dataset to long format, where each row is a park-week-ct visit count
visit_data_long <- visit_data %>%
  # Make sure visitor_home_cbg is not missing or empty
  filter(!is.na(visitor_home_cbgs) & visitor_home_cbgs != "") %>%
  # Parse JSON in each row into a list-column
  mutate(visitor_home_cbgs = map(visitor_home_cbgs, fromJSON)) %>%
  # Unnest that list-column so each CBG and count becomes its own row
  unnest_longer(visitor_home_cbgs, indices_include = TRUE) %>%
  dplyr::select(date_range_start, location_name,
                visitor_home_cbgs, visitor_home_cbgs_id) %>%
  rename(raw_visitor_cbg = visitor_home_cbgs, census_cbg = visitor_home_cbgs_id)

}

#### process census data (already run)
{
#define census api key
#census_api_key("fd9013c2c5cdfe8681d7b8a9127a5ab229fbea2e", install = TRUE, overwrite = TRUE)

#define relevant census variables
cbg_vars <- c(
  # Income & Population
  median_income = "B19013_001",   # Median Household Income
  total_pop = "B01003_001",       # Total Population
  median_age = "B01002_001",      # Median Age
  
  # Education
  total_edu = "B15003_001",       # Total Population 25+ (for denominator)
  bach = "B15003_022",            # Bachelor's Degree
  masters = "B15003_023",         # Master's Degree
  professional = "B15003_024",    # Professional Degree
  doctorate = "B15003_025",       # Doctorate Degree
  
  # Poverty & Health Insurance
  poverty_rate = "B17001_002",    # Below Poverty Line (numerator)
  poverty_total = "B17001_001",   # Total Population (denominator)
  uninsured = "B27010_017",       # No Health Insurance (numerator)
  uninsured_total = "B27010_001", # Total Population (denominator)
  
  # Age Groups
  age_65plus = c("B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", 
                 "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"), # Seniors (65+)
  age_kids = c("B01001_003", "B01001_027"), # Children (<5)
  
  # Race & Ethnicity
  white_pop = "B03002_003",
  black_pop = "B03002_004",
  hispanic_pop = "B03002_012",
  asian_pop = "B03002_006",
  race_total = "B03002_001"
)


#get census data, looping through all states
us_states <- unique(fips_codes$state)[!is.na(unique(fips_codes$state))]
territories <- c("AS", "GU", "MP", "VI", "UM")
us_states <- setdiff(us_states, territories)

acs_cbg_data <- map_dfr(us_states, function(state) {
  message(paste("Fetching data for", state, "..."))  # Print progress
  get_acs(
    geography = "block group",
    variables = cbg_vars,
    year = 2019,
    survey = "acs5",
    state = state,
    output = "wide"
  )
})

#treat cbgs with population = 0 as =1
acs_cbg_data <- acs_cbg_data %>%
  mutate(total_popE = ifelse(total_popE == 0, 1, total_popE))

#save data
saveRDS(acs_cbg_data, "./data/processed_data/acs_cbg_data.RDS")

}

### scale all other variables to the park-week level 
{
#drop time subscripts from date range, which converts to days in local time. 
visit_data_park <- visit_data %>%
  mutate(date_range_start = as.Date(date_range_start),
         date_range_end = as.Date(date_range_end)-1)

#create week_id_df for separate weeks
unq_weeks <- sort(unique(visit_data_park$date_range_start))
week_id_w <- 1:length(unq_weeks)
week_id_df_w <- data.frame(date_range_start = unq_weeks, week_id = week_id_w)

#assign week_ids in weekly data
visit_data_park <- merge(visit_data_park, week_id_df_w, by = "date_range_start")

#create week id df for individual days
dates <- seq(min(visit_data_park$date_range_start), max(visit_data_park$date_range_end), "days")
week_id_d <- rep(1:(length(dates)/7), each = 7)
week_id_df_d <- data.frame(date_LT = dates, week_id = week_id_d)

#add week_id to daily data

#load daily visitation dataset
visit_data_daily <- readRDS("./data/processed_data/main_data_all_final.RDS")
visit_data_daily <- visit_data_daily %>%
  filter(!is.na(mean_smoke_pm2.5))

#create week id in daily visitation data
visit_data_daily <- merge(visit_data_daily, week_id_df_d, 
                          by = "date_LT")

#filter to only keep park-week combinations with complete 7 days of observations (drops 174 park-days)
visit_data_daily <- visit_data_daily %>%
  group_by(park, week_id) %>%
  filter(n() == 7) %>%
  ungroup()

# Function to categorize PM2.5 concentrations into 5 AQI categories
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

# Define modal AQI function 
mode_cat <- function(x) {
  if (all(is.na(x))) return(NA_character_)
  
  # Predefined AQI order from best to worst (5 levels)
  aqi_order <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy or Hazardous")
  
  # Factorize to ensure consistent order
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

# Calculate weekly park-level variables
visit_data_weekly_vars <- visit_data_daily %>%
  mutate(
    mean_smoke_pm2.5 = as.numeric(mean_smoke_pm2.5),
    aqi_cat = pm25_to_aqi_category(mean_smoke_pm2.5)
  ) %>%
  group_by(park, week_id) %>%
  summarise(
    mean_weekly_pm25 = mean(mean_smoke_pm2.5, na.rm = TRUE),
    mean_weekly_temp = mean(tmmx, na.rm = TRUE),
    modal_aqi_cat = mode_cat(aqi_cat),
    total_weekly_prcp = sum(pr, na.rm = TRUE),
    total_days_unh_sens_worse_park = sum(mean_smoke_pm2.5 > 35.4, na.rm = TRUE),
    total_days_unh_worse_park = sum(mean_smoke_pm2.5 > 55.4, na.rm = TRUE),
    fire_within_100km_park = as.integer(any(fire_dist_km_bound <= 100, na.rm = TRUE)),
    holiday_week = as.integer(any(holiday_dum == 1, na.rm = TRUE)),
    
    # --- Number of days per AQI category (5-level system) ---
    days_good = sum(aqi_cat == "Good", na.rm = TRUE),
    days_moderate = sum(aqi_cat == "Moderate", na.rm = TRUE),
    days_unhealthy_sens = sum(aqi_cat == "Unhealthy for Sensitive Groups", na.rm = TRUE),
    days_unhealthy = sum(aqi_cat == "Unhealthy", na.rm = TRUE),
    days_very_unhealthy_or_hazardous = sum(aqi_cat == "Very Unhealthy or Hazardous", na.rm = TRUE),
    
    # --- dummy for any unhealthy or worse days
      dummy_unhealthy_or_worse = as.integer(
        any(aqi_cat %in% c("Unhealthy", "Very Unhealthy or Hazardous"), na.rm = TRUE)),
    
    # --- factor for worst aqi cat in the week
    worst_aqi_cat_week = worst_cat(aqi_cat)
  ) %>%
  ungroup()

#merge the two datasets
visit_data_park <- visit_data_park %>%
  rename(park = location_name)

visit_data_park <- merge(visit_data_park, visit_data_weekly_vars,
                         by = c("park", "week_id"))

#simplify weekly park data
visit_data_park <- visit_data_park %>%
  dplyr::select(fire_within_100km_park, mean_weekly_pm25, mean_weekly_temp, modal_aqi_cat,
                park, total_days_unh_sens_worse_park, total_days_unh_worse_park,
                days_good, days_moderate, days_unhealthy_sens, days_unhealthy, days_very_unhealthy_or_hazardous,
                total_weekly_prcp, week_id, date_range_start, dummy_unhealthy_or_worse, worst_aqi_cat_week,
                holiday_week)

#convert long data's starting date range
visit_data_long <- visit_data_long %>%
  mutate(date_range_start = as.Date(date_range_start))

visit_data_long <- visit_data_long %>% 
  rename(park = location_name) %>%
  filter(park %in% visit_data_park$park,
         date_range_start %in% visit_data_park$date_range_start)

#isolate missing park-week data 
all_park_weeks <- expand.grid(park = unique(visit_data_long$park), date_range_start = unique(visit_data_long$date_range_start))
all_park_weeks <- all_park_weeks %>% mutate(park_week = paste0(park, "_", date_range_start))
miss_park_week <- all_park_weeks %>%
  filter(!park_week %fin% paste0(visit_data_long$park, "_", visit_data_long$date_range_start))

#clear some memory
rm(visit_data, visit_data_daily,visit_data_weekly_vars)
gc()

}

#### create park substitute pm2.5 data

## substitutes by distance to the park itself
{
# Load park shapefile and compute inter-park distances
nps_shp <- readRDS("./data/processed_data/nps_data.RDS")

parks <- nps_shp %>%
  st_transform(4326) %>%        
  dplyr::select(UNIT_NAME, geometry)

# Get park boundary distances
park_boundaries <- st_boundary(parks)
dist_mat <- st_distance(park_boundaries, park_boundaries)

# Convert km
dist_km <- dist_mat / 1000

# Convert to long format with park names
dist_df <- as.data.frame(dist_km)
rownames(dist_df) <- parks$UNIT_NAME
colnames(dist_df) <- parks$UNIT_NAME

# Create neighbor sets for 100 miles, 250, 400+ miles
neighbors_100 <- dist_df %>%
  mutate(park = rownames(.)) %>%
  pivot_longer(-park, names_to = "other_park", values_to = "dist_miles") %>%
  filter(park != other_park,
         as.numeric(dist_km) <= 160.934)

neighbors_250 <- dist_df %>%
  mutate(park = rownames(.)) %>%
  pivot_longer(-park, names_to = "other_park", values_to = "dist_miles") %>%
  filter(park != other_park,
         as.numeric(dist_km) <= 402.336)

neighbors_400 <- dist_df %>%
  mutate(park = rownames(.)) %>%
  pivot_longer(-park, names_to = "other_park", values_to = "dist_miles") %>%
  filter(park != other_park,
         as.numeric(dist_km) <= 643.738)

# Count number of substitutes for each park
neighbor_count_100 <- neighbors_100 %>%
  group_by(park) %>%
  summarise(num_neighbors_100 = n_distinct(other_park), .groups = "drop")

neighbor_count_250 <- neighbors_250 %>%
  group_by(park) %>%
  summarise(num_neighbors_250 = n_distinct(other_park), .groups = "drop")

neighbor_count_400 <- neighbors_400 %>%
  group_by(park) %>%
  summarise(num_neighbors_400 = n_distinct(other_park), .groups = "drop")

# Extract weekly PM2.5 values for each park
park_week_pm25 <- visit_data_park %>%
  dplyr::select(park, date_range_start, mean_weekly_pm25, modal_aqi_cat, dummy_unhealthy_or_worse, worst_aqi_cat_week) %>%
  distinct()

# Join weekly PM2.5 to neighbor tables
neighbor_pm25_100 <- park_week_pm25 %>%
  left_join(neighbors_100, by = "park") %>%
  left_join(park_week_pm25,
            by = c("other_park" = "park", "date_range_start"),
            suffix = c("", "_neighbor"))

neighbor_pm25_250 <- park_week_pm25 %>%
  left_join(neighbors_250, by = "park") %>%
  left_join(park_week_pm25,
            by = c("other_park" = "park", "date_range_start"),
            suffix = c("", "_neighbor"))

neighbor_pm25_400 <- park_week_pm25 %>%
  left_join(neighbors_400, by = "park") %>%
  left_join(park_week_pm25,
            by = c("other_park" = "park", "date_range_start"),
            suffix = c("", "_neighbor"))

# Mean PM2.5 among substitutes
mean_neighbor_pm25_100 <- neighbor_pm25_100 %>%
  group_by(park, date_range_start) %>%
  summarise(
    mean_pm25_sub_100 = mean(mean_weekly_pm25_neighbor, na.rm = TRUE),
    .groups = "drop"
  )

mean_neighbor_pm25_250 <- neighbor_pm25_250 %>%
  group_by(park, date_range_start) %>%
  summarise(
    mean_pm25_sub_250 = mean(mean_weekly_pm25_neighbor, na.rm = TRUE),
    .groups = "drop"
  )

mean_neighbor_pm25_400 <- neighbor_pm25_400 %>%
  group_by(park, date_range_start) %>%
  summarise(
    mean_pm25_sub_400 = mean(mean_weekly_pm25_neighbor, na.rm = TRUE),
    .groups = "drop"
  )

# PM2.5 to AQI category function
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

# Apply AQI conversion to neighbors
neighbor_pm25_100 <- neighbor_pm25_100 %>%
  mutate(aqi_neighbor = pm25_to_aqi_category(mean_weekly_pm25_neighbor))

neighbor_pm25_250 <- neighbor_pm25_250 %>%
  mutate(aqi_neighbor = pm25_to_aqi_category(mean_weekly_pm25_neighbor))

neighbor_pm25_400 <- neighbor_pm25_400 %>%
  mutate(aqi_neighbor = pm25_to_aqi_category(mean_weekly_pm25_neighbor))

# AQI ranking
aqi_order <- c("Good",
               "Moderate",
               "Unhealthy for Sensitive Groups",
               "Unhealthy",
               "Very Unhealthy or Hazardous")

# Best AQI alternative (cleanest weekly average) 
best_aqi_alt_100 <- neighbor_pm25_100 %>%
  group_by(park, date_range_start) %>%
  summarise(
    best_aqi_subst_100 = {
      ordered_vals <- factor(aqi_neighbor, levels = aqi_order)
      aqi_order[min(as.numeric(ordered_vals), na.rm = TRUE)]
    },
    .groups = "drop"
  )

best_aqi_alt_250 <- neighbor_pm25_250 %>%
  group_by(park, date_range_start) %>%
  summarise(
    best_aqi_subst_250 = {
      ordered_vals <- factor(aqi_neighbor, levels = aqi_order)
      aqi_order[min(as.numeric(ordered_vals), na.rm = TRUE)]
    },
    .groups = "drop"
  )

best_aqi_alt_400 <- neighbor_pm25_400 %>%
  group_by(park, date_range_start) %>%
  summarise(
    best_aqi_subst_400 = {
      ordered_vals <- factor(aqi_neighbor, levels = aqi_order)
      aqi_order[min(as.numeric(ordered_vals), na.rm = TRUE)]
    },
    .groups = "drop"
  )


# Join substitute metrics back to data
substitute_data <- mean_neighbor_pm25_100 %>%
  left_join(mean_neighbor_pm25_250,  by = c("park", "date_range_start")) %>%
  left_join(mean_neighbor_pm25_400,  by = c("park", "date_range_start")) %>%
  left_join(best_aqi_alt_100,       by = c("park", "date_range_start")) %>%
  left_join(best_aqi_alt_250,       by = c("park", "date_range_start")) %>%
  left_join(best_aqi_alt_400,       by = c("park", "date_range_start")) %>%
  left_join(neighbor_count_100,     by = "park") %>%
  left_join(neighbor_count_250,     by = "park") %>%
  left_join(neighbor_count_400,     by = "park")
}

## substitutes by 400 miles driving distance from home cbg
{
  # convert 400 miles to meters
  dist_m <- 400 * 1609.344
  
  #isolate cbg park substitution choice set
  cbg_feasible_parks <- travel_data %>%
    filter(travel_distance <= dist_m) %>%
    transmute(
      census_cbg,
      park
    ) %>%
    distinct()
  
  
  # Self-join to get all focal–substitute pairs per CBG
  cbg_focal_sub_pairs <- cbg_feasible_parks %>%
    rename(focal_park = park) %>%
    inner_join(
      cbg_feasible_parks %>% rename(sub_park = park),
      by = "census_cbg"
    ) %>%
    filter(focal_park != sub_park)
  
  #isolate park-week pm2.5 attributes
  park_week_attrs <- visit_data_park %>%
    dplyr::select(
      park,
      date_range_start,
      mean_weekly_pm25,
      worst_aqi_cat_week,
      modal_aqi_cat
    ) %>%
    distinct()
  
  cbg_focal_sub_pairs <- cbg_focal_sub_pairs %>%
    left_join(
      park_week_attrs,
      by = c("sub_park" = "park")
    )
  
  #define aqi order
  aqi_order <- c(
    "Good",
    "Moderate",
    "Unhealthy for Sensitive Groups",
    "Unhealthy",
    "Very Unhealthy or Hazardous"
  )
  
  #calculate best aqi and mean aqi among the substitutes (using data.table because faster)
  setDT(cbg_focal_sub_pairs)
  
  cbg_focal_week_sub_pairs <- cbg_focal_sub_pairs[
    ,
    .(
      best_mean_pm25_subst_parks_cbg =
        min(mean_weekly_pm25, na.rm = TRUE),
      
      best_worst_aqi_subst_parks_cbg = {
        aqi_num <- match(worst_aqi_cat_week, aqi_order)
        aqi_order[min(aqi_num, na.rm = TRUE)]
      },
      
      best_modal_aqi_subst_parks_cbg = {
        aqi_num <- match(modal_aqi_cat, aqi_order)
        aqi_order[min(aqi_num, na.rm = TRUE)]
      },
      
      n_substitute_parks = .N
    ),
    by = .(census_cbg, focal_park, date_range_start)
  ]
  
  #save results
  saveRDS(cbg_focal_week_sub_pairs, "./data/processed_data/cbg_substitute_park_pm25_data.RDS")
}

#### bring in home pm2.5 data and scale to weekly (already run)
{
#load daily pm2.5 data for every cbg
  cbg_smoke_pm25_daily <- readRDS("./data/processed_data/daily_pm25_by_cbg.RDS")
  
  # Convert to data.table for efficient join
  setDT(cbg_smoke_pm25_daily)
  setDT(week_id_df_d)
  
  # Merge week IDs
  cbg_smoke_pm25_daily <- cbg_smoke_pm25_daily[week_id_df_d,
                                               on = c(date = "date_LT")]
  
  # AQI conversion function reused
  cbg_smoke_pm25_daily[, aqi_cat := pm25_to_aqi_category(mean_smoke_pm2.5)]
  
  # Calculate the SAME WEEKLY METRICS as for PARKS
  cbg_smoke_pm25_weekly <- cbg_smoke_pm25_daily[, .(
    mean_weekly_pm25_cbg = mean(mean_smoke_pm2.5, na.rm = TRUE),
    
    modal_aqi_cat_home = mode_cat(aqi_cat),
    worst_aqi_cat_home = worst_cat(aqi_cat),
    
    dummy_unhealthy_or_worse_home =
      as.integer(any(aqi_cat %in% c("Unhealthy",
                                    "Very Unhealthy or Hazardous"),
                     na.rm = TRUE))
  ), by = .(census_cbg = cbg, week_id)]
  
  gc()
  
  #save result
  saveRDS(cbg_smoke_pm25_weekly, "./data/processed_data/cbg_pm25_weekly_data.RDS")
}

####create long dataset of all park-cbg combos (within 800 miles one way trip), with 0 counts,

##load previously executed data
#cbg substitute park data
cbg_focal_week_sub_pairs <- readRDS("./data/processed_data/cbg_substitute_park_pm25_data.RDS")

#cbg home data
cbg_smoke_pm25_weekly <- readRDS("./data/processed_data/cbg_pm25_weekly_data.RDS")

#travel data
travel_data <- readRDS("./data/processed_data/travel_dist_time_data.RDS")
# filter cbg-park combos within a certain travel distance of a given park (800 miles for now)
travel_data_filt <- travel_data %>%
  filter(travel_distance < (643738*2)) %>%
  mutate(cbg_park_combos = paste0(census_cbg,"_",park)) %>%
  dplyr::select(-c(park_id_cbg))

#census data
acs_cbg_data <- readRDS("./data/processed_data/acs_cbg_data.RDS")
#clean up and simplify for now. 
acs_cbg_data_filt <- acs_cbg_data %>%
  mutate(perc_higher_ed = (doctorateE + mastersE + bachE + professionalE)/total_eduE,
         perc_white = white_popE/total_popE) %>%
  dplyr::select(GEOID, total_popE, median_ageE, median_incomeE, perc_higher_ed,
                perc_white)

## trim down park data to match the subsetted 38 parks
subset_daily_data <- readRDS("./data/processed_data/main_data_subset_final.RDS")
unq_parks <- unique(subset_daily_data$park)

visit_data_long <-  visit_data_long %>%
  filter(park %fin% unq_parks)
visit_data_park <- visit_data_park %>%
  filter(park %fin% unq_parks)
substitute_data <- substitute_data %>%
  filter(park %fin% unq_parks)
substitute_cbg_data <- cbg_focal_week_sub_pairs %>%
  rename(park = focal_park) %>%
  filter(park %fin% unq_parks)

## create expanded base dataset

#get all (park, census_cbg) pairs that are within distance threshold
possible_combos <- travel_data_filt %>%
  filter(park %in% unique(visit_data_long$park)) %>%
  distinct(park, census_cbg)

#get all possible start dates
all_dates <- visit_data_long %>%
  distinct(date_range_start)

#get all combos of cbg, park, weeks
all_combos <- crossing(possible_combos, all_dates)

##join original visit data to fill in missing combos with zero counts.
visit_data_long <- all_combos %>%
  left_join(visit_data_long, by = c("park", "census_cbg", "date_range_start")) %>%
  mutate(raw_visitor_cbg = ifelse(is.na(raw_visitor_cbg), 0, raw_visitor_cbg)) %>%
  mutate(cbg_park_combos = paste0(census_cbg, "_", park))

# Convert to data.table for faster merge
setDT(visit_data_long)
setDT(miss_park_week)
setDT(acs_cbg_data_filt)
setDT(visit_data_park)
setDT(travel_data_filt)
setDT(cbg_smoke_pm25_weekly)
setDT(substitute_data)
setDT(substitute_cbg_data)

#drop park-weeks that had missing data (that were previously filled in with 0 counts)
visit_data_long <- visit_data_long[!.(miss_park_week$park, miss_park_week$date_range_start),
                                   on = .(park, date_range_start)]

#free up memory
rm(cbgs_centroids, cbgs_coords, cbgs_sf, poly_centroids, poly_coords,
   all_combos, possible_combos, all_dates)
gc()

# merge
visit_data_long <- acs_cbg_data_filt[visit_data_long, on = .(GEOID = census_cbg)]
Sys.sleep(10)
gc()

visit_data_long <- visit_data_park[visit_data_long, on = .(date_range_start, park)]
Sys.sleep(10)
gc()

visit_data_long <- travel_data_filt[visit_data_long, on = .(cbg_park_combos)]
Sys.sleep(10)
gc()

visit_data_long <- cbg_smoke_pm25_weekly[visit_data_long, on = .(week_id, census_cbg)]
Sys.sleep(10)
gc()

visit_data_long <- substitute_data[visit_data_long, on = .(park, date_range_start)]
Sys.sleep(10)
gc()

visit_data_long <- substitute_cbg_data[visit_data_long, on = .(census_cbg, park, date_range_start)]
Sys.sleep(10)
gc()

rm(travel_data, travel_data_filt, acs_cbg_data, acs_cbg_data_filt, cbg_smoke_pm25_weekly,
   substitute_data, substitute_cbg_data)
gc()

#clean up
visit_data_long <- visit_data_long %>% 
  dplyr::select(-c(cbg_park_combos, i.park, week_id))

#### additional variable creation

## time fixed effects
#create month fixed effect variable
unq_start_dates <- as.Date(unique(visit_data_long$date_range_start))
days = map(unq_start_dates, ~ seq(from = .x, to = .x+6, by = "days"))
months = map(.x = days, .f = ~ lubridate::month(.x))
week_month =  map_int(.x = months, ~ as.integer(names(which.max(table(.x)))))
date_month_map_df <- data.frame(date_range_start = unq_start_dates, 
                                month = week_month)
setDT(date_month_map_df)
visit_data_long <- date_month_map_df[visit_data_long, on = "date_range_start"]

#create year and season fixed effect variable
visit_data_long <- visit_data_long %>%
  mutate(
    year = year(date_range_start),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_  
    )
  )

##load data for travel cost function

#create park entry fee cost by park
park_entry_fee_df <- data.frame(park = sort(unique(visit_data_long$park)),
                                park_entry_fee = c(35, 30, 30, 30, 30, 35,30, 20, 0, 30, 30, 35, 35, 35,
                                                   0, 25 ,25, 7, 30, 35, 30, 0, 30, 30, 0, 0, 30, 0, 30, 35, 30,
                                                   30, 0, 25, 0, 35, 35, 35))
setDT(park_entry_fee_df)
visit_data_long <- park_entry_fee_df[visit_data_long, on = "park"]
gc()

#people per car data
ppl_per_car_nps <- read.csv("./data/nps_official_data/np_rec_use_data.csv")
setDT(ppl_per_car_nps)
visit_data_long <- ppl_per_car_nps[visit_data_long, on = "park"]
gc()

#load gasoline data
gas_files <- list.files("./data/eia_gas_price_data", full.names = T)
gas_price_list <- vector("list", length = length(gas_files))

for (i in 1:length(gas_files)){
  
  #load gas data 
  gas_data_i <- readxl::read_xls(gas_files[i], sheet = 2)
  
  #capture region 
  region_i <- str_extract(colnames(gas_data_i)[2], "(?<=Data 1: ).*?(?= Regular)")
  
  #rename columns, reformat date, and filter by relevant dates
  colnames(gas_data_i) <- c("Date", "gas_price_nom")
  gas_data_i <- gas_data_i[3:nrow(gas_data_i),]
  gas_data_i <- gas_data_i %>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
           year = year(Date),
           month = month(Date)) %>%
    filter(year > 2018 & year < 2023) %>%
    dplyr::select(-c(Date))
  
  #readd region
  gas_data_i <- gas_data_i %>%
    mutate(region = region_i)
  
  #store
  gas_price_list[[i]] <- gas_data_i
}
gas_price_df <- rbindlist(gas_price_list)

#convert gas prices to common year

#define manual average cpi from FRED
cpi_data <- data.frame(year = c(2019, 2020, 2021, 2022, 2023),
                       cpi = c(255.653, 258.856, 270.968, 292.265, 304.704))
gas_price_df <- merge(gas_price_df, cpi_data, by = "year", all.x = T)
gas_price_df <- gas_price_df %>%
  mutate(gas_price_real_per_gal = as.numeric(gas_price_nom) *(304.704/cpi)) %>%
  dplyr::select(-c(gas_price_nom, cpi))

#assign each park a eia region
poly_data_simp <- poly_data %>%
  dplyr::select(UNIT_NAME, STATE) %>%
  rename(park = UNIT_NAME,
         state = STATE) %>%
  st_drop_geometry()

#manually define EIA regions
eia_regions <- tribble(
  ~state, ~region,
  
  # East Coast (PADD 1)
  "CT", "East Coast", "ME", "East Coast", "MA", "East Coast",
  "NH", "East Coast", "RI", "East Coast", "VT", "East Coast",
  "DE", "East Coast", "DC", "East Coast", "MD", "East Coast",
  "NJ", "East Coast", "NY", "East Coast", "PA", "East Coast",
  "FL", "East Coast", "GA", "East Coast", "NC", "East Coast",
  "SC", "East Coast", "VA", "East Coast", "WV", "East Coast",
  
  # Midwest (PADD 2)
  "IL", "Midwest", "IN", "Midwest", "IA", "Midwest", "KS", "Midwest",
  "KY", "Midwest", "MI", "Midwest", "MN", "Midwest", "MO", "Midwest",
  "NE", "Midwest", "ND", "Midwest", "OH", "Midwest", "OK", "Midwest",
  "SD", "Midwest", "TN", "Midwest", "WI", "Midwest",
  
  # Gulf Coast (PADD 3)
  "AL", "Gulf Coast", "AR", "Gulf Coast", "LA", "Gulf Coast",
  "MS", "Gulf Coast", "NM", "Gulf Coast", "TX", "Gulf Coast",
  
  # Rocky Mountain (PADD 4)
  "CO", "Rocky Mountain", "ID", "Rocky Mountain", "MT", "Rocky Mountain",
  "UT", "Rocky Mountain", "WY", "Rocky Mountain",
  
  # West Coast (PADD 5)
  "AK", "West Coast", "AZ", "West Coast", "CA", "West Coast",
  "HI", "West Coast", "NV", "West Coast", "OR", "West Coast", "WA", "West Coast"
)

#merge and simplify
poly_data_simp <- merge(poly_data_simp, eia_regions, by = "state")
poly_data_simp <- poly_data_simp %>%
  dplyr::select(-c(state))

#add eia region indicator to main data
setDT(poly_data_simp)
visit_data_long <- poly_data_simp[visit_data_long, on = "park"]
Sys.sleep(5)
gc()

#merge in gas prices
setDT(gas_price_df)
visit_data_long <- gas_price_df[visit_data_long, on = .(year, month, region)]
Sys.sleep(5)
gc()

#convert acs income to 2023
visit_data_long <- visit_data_long %>%
  mutate(median_incomeE = median_incomeE*(304.704/255.653))

#define other relevant tc parameters

# all values from (https://newsroom.aaa.com/wp-content/uploads/2023/08/YDC-Brochure_2023-FINAL-8.30.23-.pdf)
p_m <- .0983/1.60934 # dollars per kilometer maintenance
p_d <- 4538/24140.1 # dollars per kilometer depreciation

kmpg <- 22.8 * 1.60934 #sales weighted kilometer per gallon of passenger vehicles and light duty trucks (https://www.bts.gov/content/average-fuel-efficiency-us-light-duty-vehicles)
hours <- 2080 # hours worked a year

#define travel cost function
travel_cost_func <- function(dist, gas_price, time, median_wage, park_entry_fee, ppl_per_car){
  
  total_non_gas_d_costs <- 2*(p_m+p_d)*dist*(1/ppl_per_car)
  total_gas_d_costs <- 2*(gas_price/kmpg)*dist*(1/ppl_per_car)
  
  total_time_cost <- 2*(time/60)*median_wage*(1/3)*(1/hours)
  
  park_entry_cost <- park_entry_fee*(1/ppl_per_car)
  
  tc = total_non_gas_d_costs + total_gas_d_costs + total_time_cost + park_entry_cost
  
  return(tc)
}

#calculate travel cost variable:
visit_data_long <- visit_data_long %>%
  mutate(
    travel_cost = travel_cost_func((travel_distance/1000), gas_price_real_per_gal, travel_time, median_incomeE, park_entry_fee, persons_per_car)
  )

##modify variable names and scaling, clean up

#adjust names of variables
visit_data_long <- visit_data_long %>%
  rename(travel_distance_m = travel_distance,
         prop_higher_ed = perc_higher_ed,
         prop_white = perc_white)

#create relative pm2.5 measure
visit_data_long <- visit_data_long %>%
  mutate(rel_pm25_park_cbg = mean_weekly_pm25-mean_weekly_pm25_cbg)

#create faster park-month FEs 
visit_data_long <- visit_data_long %>%
  mutate(park_month = paste0(park, "_", month))

#create population-weighted visits and scale travel costs
visit_data_long <- visit_data_long %>%
  mutate(raw_visitor_cbg_per_1000 = raw_visitor_cbg/total_popE,
         travel_cost_100s = travel_cost/100
  )

#scale temp 
visit_data_long <- visit_data_long %>%
  mutate(mean_weekly_temp = mean_weekly_temp - 273.15)

#create park-month-year effects
visit_data_long <- visit_data_long %>%
  mutate(park_month_year = paste0(park_month,"_",year))

#rename visits variable
visit_data_long <- visit_data_long %>%
  rename(Visits = raw_visitor_cbg)

#reorder worst aqi variable for better table outputs
visit_data_long <- visit_data_long %>%
  mutate(worst_aqi_cat_week = factor(
    worst_aqi_cat_week,
    levels = c(
      "Good",
      "Moderate",
      "Unhealthy for Sensitive Groups",
      "Unhealthy",
      "Very Unhealthy or Hazardous"
    )
  ))

#filter to only include park-cbg combos that have atleast one visit across the whole dataset
cbg_park_sums <- visit_data_long %>%
  group_by(census_cbg, park) %>%
  summarise(total_visits = sum(Visits)) %>%
  filter(total_visits > 0)

visit_data_long <- visit_data_long %>% 
  semi_join(cbg_park_sums, by = c("census_cbg", "park"))

## diagnostics on data set

# check na count
na_count <- colSums(is.na(visit_data_long))
na_count

#some mean_weekly_pm25_cbg NA's for census blocks that fall outside of burke pm2.5 grid or don't match
#some census block missing variables because of cbg id matches
#day_trip_duration missing for 5 parks
#travel cost missing because of missing income data
#missing substitution data for parks that don't have nearby parks
#lots of missing cbg substitutions because trimmed to cbg-park sets within 400 miles

##save dataset
saveRDS(visit_data_long, "./data/processed_data/travel_cost_dataset.RDS")



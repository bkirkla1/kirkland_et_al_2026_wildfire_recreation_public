#---------------------------------------------------------------------------------------------
#
# 03_advan_placekey_filter: this script filters the raw weekly advan data, and filters to isolate
# national park visitation. It saves a weekly and daily version of the nps data.
#
#----------------------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

# Load manually filtered Advan placekeys for national parks 
nps_placekey_data <- read.csv("./data/spatial_bound_data/nps_advan_placekeys.csv")
nps_placekeys <- unique(nps_placekey_data$placekey)

# Define years to loop over
years <- c(2019:2022)

# Define advan columns to keep
imp_cols <- c("date_range_start", "distance_from_home", "latitude", "longitude",
              "naics_code", "placekey", "postal_code", "raw_visitor_counts",
              "visits_by_day", "city", "date_range_end", "iso_country_code",
              "location_name", "median_dwell", "normalized_visits_by_state_scaling",
              "poi_cbg", "polygon_wkt", "raw_visit_counts", "region",
              "visitor_home_cbgs", "visitor_home_aggregation")

# Define the function to read and filter in chunks using arrow
process_file <- function(file_j, imp_cols) {
  
  # filter by placekeys
  placekey_filter <- nps_placekeys
  
  # read CSV and filter
  data_j <- arrow::read_csv_arrow(file_j, col_select = imp_cols,
                                  read_options = list(block_size = 204800576L)) %>%
    filter(placekey %in% placekey_filter)
  
  return(data_j)
}

#setup parallel back-end
plan(multisession, workers = availableCores()-2)

#run loop to read in raw weekly Advan data, filter by placekey
for (i in 1:length(years)) {
  
  # Define directory where files are located
  file_dir <- paste0("E:/advan_data_", years[i])
  
  # Read in the list of files
  file_list <- list.files(file_dir, full.names = T)
  
  # Determine the chunk size (1/10th of the length of file_list)
  chunk_size <- ceiling(length(file_list) / 20)
  
  # Loop over chunks
  for (chunk_idx in 1:20) {
    
    # Calculate the range of file indices for the current chunk
    start_idx <- (chunk_idx - 1) * chunk_size + 1
    end_idx <- min(chunk_idx * chunk_size, length(file_list))

    #define files to lapply over for a given chunk
    file_list_chunk_idx <- file_list[start_idx:end_idx]
    
    #apply load and filtering function
    data_chunk_idx <- future.apply::future_lapply(file_list_chunk_idx, function(j) {
      tryCatch({
        process_file(j, imp_cols)
      }, error = function(e) {
        message(paste("Error in file:", j, ":", e$message))
        return(NULL)
      })
    })
    
    #handle data type errors
    data_list <- keep(data_chunk_idx, ~ !is.null(.x) && nrow(.x) > 0)
    
    data_list <- lapply(data_list, function(df) {
      if (!is.null(df)) {
        df$distance_from_home <- as.character(df$distance_from_home)  # Replace column_2 with the correct type
      }
      return(df)
    })
    
    #combine results
    data_df <- rbindlist(data_list)
    
    #save into new directory
    file_name <- paste0("E:/processed_advan_data/advan_data_filtered_", years[i], 
                        "_chunk_", chunk_idx)
    saveRDS(data_df, paste0(file_name, "_np.rds"))
    
    #clear up data
    rm(data_df, data_list)
    gc()
    
    #print progress
    print(paste0("Year: ", years[i], " - Chunk: ", chunk_idx))
  }
}

####load data into one df

#define folder of advan chunks
raw_data_dir <- "E:/processed_advan_data"

#define storage list for results
data_list <- vector("list", length = length(years))

#loop over years and read in data
for (j in 1:length(years)) {
  
  # Find all files that correspond to nps data in year j
  year_j <- years[j]
  files <- list.files(raw_data_dir, full.names = TRUE)
  filtered_files <- grep(year_j, files, value = TRUE)
  
  # Use lapply to read all files for the current year
  data_j_list <- lapply(filtered_files, readRDS)
  
  # Merge all results for year j into one data frame and store
  data_year_j_df <- rbindlist(data_j_list)
  data_list[[j]] <- data_year_j_df
  
  #print progress
  print(year_j)
}

#combine into one df
main_data <- rbindlist(data_list)

##explore data

#count na values per week
na_per_week <- main_data%>%
  mutate(date = as.Date(date_range_start)) %>%  # Extract date from date-time column
  group_by(date) %>%
  summarise(na_count = sum(is.na(visits_by_day)))  

#count visits by month and graph 
 pk_loc_visit_counts <- main_data %>%
  mutate(week = format(as.Date(date_range_start), "%Y-%m-%d")) %>%  # Extract month as "YYYY-MM"
  group_by(placekey, location_name, week) %>%
  summarise(total_visits = sum(normalized_visits_by_state_scaling, na.rm = TRUE)) %>%
   ungroup()# Sum visits per group

 pk_loc_visit_counts <- pk_loc_visit_counts %>%
   filter(!is.na(week) & !is.na(total_visits))
 
 unique_combos <- pk_loc_visit_counts %>% dplyr::select(placekey, location_name) %>% distinct()

#save weekly data
main_data <- main_data %>%
   filter(date_range_start >= "2018-12-31" & date_range_start <= "2022-12-31") %>%
  filter(!is.na(visits_by_day))

saveRDS(main_data, "./data/processed_data/weekly_advan_data.RDS")

#convert to long daily format format
data_list_long <- vector("list", length = nrow(main_data))

for (i in 1:nrow(main_data)){
  
  data_i <- main_data[i,]
  #convert visits
  visits_i <- data_i$visits_by_day
  visits_i_long <- as.numeric(unlist(str_extract_all(visits_i, "\\d+")))
  
  #convert dates
  start_date = as.Date(format(as.Date(data_i$date_range_start),"%Y-%m-%d"))
  end_date = as.Date(format(as.Date(data_i$date_range_end), "%Y-%m-%d"))
  date_i_long = seq(start_date, end_date-1, by = "days")
  
  #combine
  data_i_long <- data.frame(visits_i_long, date_i_long)
  data_i_long$location_name <- data_i$location_name
  data_i_long <- data_i_long %>%
    rename(daily_visits = visits_i_long, date_long = date_i_long)
  data_i_long <- merge(data_i_long, data_i, by = c("location_name"))

  #store
  data_list_long[[i]] <- data_i_long
  
  #print progress
  print(i)
}

main_data_long <- rbindlist(data_list_long)

#save daily data
saveRDS(main_data_long, "./data/processed_data/raw_advan_daily_data.RDS")

## create normalized visitation metrics

#method 1: normalize by total visitors
norm_stat_data <- read.csv("./data/advan_normalization_data/Monthly_Patterns_Normalization_Stats.csv")
norm_stat_data <- norm_stat_data %>%
  mutate(date_long = as.Date(paste0(YEAR, "-", MONTH, "-", DAY), format = "%Y-%m-%d")) %>%
  dplyr::select(date_long, REGION, ISO_COUNTRY_CODE, TOTAL_DEVICES_SEEN) %>%
  rename(region = REGION, iso_country_code = ISO_COUNTRY_CODE)

main_data_long <- merge(main_data_long, norm_stat_data, 
                             by = c("date_long", "region", "iso_country_code"),
                             all.x = T, all.y = F)

main_data_long <- main_data_long %>% 
  mutate(daily_visits_norm_tv = daily_visits/TOTAL_DEVICES_SEEN)

#method 2, weight by cbg or state

#read in "short" data
main_data <- readRDS("./data/processed_data/weekly_advan_data.RDS")

#load panel normalization data
panel_stat_data <- readRDS("./data/processed_data/panel_summary_normalization.RDS")

#load census block population data
#establish api key
census_api <- "e55ccfb702180e11cf22d42a42db5086804f0b4a"
census_api_key(census_api, install = TRUE, overwrite = T)

# List of all state abbreviations (including DC)
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

# Define a function to get data for a given year and state
get_acs_data <- function(year, state) {
  get_acs(
    geography = "block group",
    variables = "B01003_001",  # Total population variable in ACS
    year = year,
    survey = "acs5",
    state = state,
    output = "wide"
  )
}

#define acs years to pull (code can weight by year, but advan data uses 2019 acs block definitions)
acs_years <- 2019

# Initialize an empty list to store data
acs_data_list <- vector("list", length(acs_years)*length(states))

# Loop over each state and year, collecting data
counter <- 1
for (state in states) {
  for (year in acs_years) {
    try({
      data <- get_acs_data(year, state)
      data <- data %>% mutate(year = year, state = state) 
      acs_data_list[[counter]] <- data  
    }, silent = TRUE)  # Continue on error (e.g., missing data for some states/years)
  counter <- counter+1
    }
}

acs_data <- bind_rows(acs_data_list)
#clean up acs data
acs_data <- acs_data %>% 
  dplyr::select(GEOID,B01003_001E, year, state) %>%
  rename(CBG_population = B01003_001E)

main_data_long_list <- vector("list", length = nrow(main_data))

#loop over rows to predict weekly normalized raw visits
for (z in 1:nrow(main_data)){
  data_z <- main_data[z,]
  
  #isolate which year is appropriate, when some weeks contain dates from multiple years
  date_seq_z <- seq(as.Date(data_z$date_range_start), as.Date(data_z$date_range_end), by = "days")
  if (length(unique(year(date_seq_z))) == 1){
    year_z <- year(as.Date(data_z$date_range_start))}
  else{
    year_table <- table(year(date_seq_z)) 
    year_z <- as.numeric(names(year_table)[which.max(year_table)])
  }
  
  #isolate which month is appropriate, when some weeks contain dates from multiple months
  if (length(unique(month(date_seq_z))) == 1){
    month_z <- month(as.Date(data_z$date_range_start))}
  else{
    month_table <- table(month(date_seq_z)) 
    month_z <- as.numeric(names(month_table)[which.max(month_table)])
  }
  
  #extract cbg home locations from JSON format to df
  if (!is.na(data_z$visitor_home_cbgs)){
  cbg_visits <- as.data.frame(jsonlite::fromJSON(data_z$visitor_home_cbgs), stringsAsFactors = FALSE, check.names = F)
  cbg_visits <- data.frame(CBG = names(cbg_visits), visitor_count = unlist(cbg_visits), row.names = NULL)

  #bring in cbg population data, depending on whether we want heterogeneity in acs years
  if (year_z %in% unique(acs_data$year)){
    acs_data_z <- acs_data %>%
      filter(GEOID %in% cbg_visits$CBG, year == year_z) %>%
      dplyr::select(-c(year))}
  else{
    acs_data_z <- acs_data %>%
      filter(GEOID %in% cbg_visits$CBG) %>%
      dplyr::select(-c(year))
  }
  
  #merge
  merged_data_z <- merge(cbg_visits, acs_data_z, by.x = "CBG", by.y = "GEOID")
  
  #bring in normalization data
  norm_data_z <- panel_stat_data %>% 
    filter(YEAR == year_z, MON == month_z) %>%
    filter(CENSUS_BLOCK_GROUP %in% merged_data_z$CBG) %>%
    dplyr::select(CENSUS_BLOCK_GROUP, NUMBER_DEVICES_RESIDING)
  
  #merge again
  merged_data_z <- merge(merged_data_z, norm_data_z, by.x = "CBG", by.y = "CENSUS_BLOCK_GROUP")
  
  #calculate "true" visits per census block
  merged_data_z <- merged_data_z %>%
    mutate(normalized_visitors = visitor_count * (CBG_population/NUMBER_DEVICES_RESIDING)*(data_z$raw_visit_counts/data_z$raw_visitor_counts))
  
  #sum for poi
  sum_true_visits <- sum(merged_data_z$normalized_visitors, na.rm = T)
  }
  else{sum_true_visits <- NA}
  
  #now partition weekly visits to daily visits
  data_z_long <- main_data_long %>%
    filter(location_name == data_z$location_name) %>%
    filter(date_range_start == data_z$date_range_start)
  
  data_z_long$normalized_visits_by_CBG_scaling <- sum_true_visits
  
  daily_proportions <- data_z_long$daily_visits/data_z_long$raw_visit_counts
 
  data_z_long <- data_z_long %>%
    mutate(daily_visits_norm_state = daily_proportions*normalized_visits_by_state_scaling, 
           daily_visits_norm_CBG = daily_proportions*normalized_visits_by_CBG_scaling)
  
  #store results
  main_data_long_list[[z]] <- data_z_long
  print(z)
}

main_data_long_comp <- rbindlist(main_data_long_list)

#save results
saveRDS(main_data_long_comp, "./data/processed_data/visit_data.RDS")


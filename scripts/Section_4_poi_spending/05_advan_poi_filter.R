#-------------------------------------------------------------------------------
#
# "05_advan_poi_filter.R": this script takes the raw advan data and filters it to just
# include points of interest around parks
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#load safegraph spend data to isolate placekeys of interest
safegraph_data <- readRDS("./data/processed_data/main_weekly_safegraph_data_final.RDS")
unq_placekeys <- unique(safegraph_data$PLACEKEY)
rm(safegraph_data)

# Define years to loop over
years <- c(2019:2022)

# Define columns of interest
imp_cols <- c("date_range_start", "distance_from_home", "latitude", "longitude",
              "naics_code", "placekey", "postal_code", "raw_visitor_counts",
              "visits_by_day", "city", "date_range_end", "iso_country_code",
              "location_name", "median_dwell", "normalized_visits_by_state_scaling",
              "poi_cbg", "polygon_wkt", "raw_visit_counts", "region",
              "visitor_home_cbgs", "visitor_home_aggregation")

# Define the function to read and filter in chunks using arrow
process_file <- function(file_j, imp_cols) {
  
  # filter by placekeys
  placekey_filter <- unq_placekeys
  
  # read CSV and filter
  data_j <- arrow::read_csv_arrow(file_j, col_select = imp_cols,
                                  read_options = list(block_size = 204800576L)) %>%
    filter(placekey %in% placekey_filter)
  
  return(data_j)
}

#setup parallel backend
plan(multisession, workers = availableCores()-2)

for (i in 1:length(years)) {
  
  # Define directory where files are located
  file_dir <- paste0("E:/advan_data_", years[i])
  
  # Read in the list of files
  file_list <- list.files(file_dir, full.names = T)
  
  # Determine the chunk size (1/10th of the length of file_list)
  chunk_size <- ceiling(length(file_list) / 50)
  
  # Loop over chunks
  for (chunk_idx in 1:50) {
    
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
    
    
    #combine results and save
    #handle weird data type issue for 2023
    data_list <- keep(data_chunk_idx, ~ !is.null(.x) && nrow(.x) > 0)
    
    data_list <- lapply(data_list, function(df) {
      if (!is.null(df)) {
        df$distance_from_home <- as.character(df$distance_from_home)  # Replace column_2 with the correct type
        # Do this for other columns as needed
      }
      return(df)
    })
    
    data_df <- rbindlist(data_list)
    
    file_name <- paste0("E:/processed_advan_business_data/advan_business_data_filtered_", years[i], 
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
raw_data_dir <- "E:/processed_advan_business_data"

#define storage list for results
data_list <- vector("list", length = length(years))

#loop over years and read in data
for (j in 1:length(years)) {
  
  # Find all files that correspond to business data data in year j
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

#clear up some memory
rm(data_year_j_df, data_j_list, spend_data_sf)
gc()

#drop polygon_wkt (polygon geometry) to save local memory
data_list <- lapply(
  data_list,
  function(dt) dt[, !"polygon_wkt", with = FALSE]
)
gc()

#combine into one df
main_data <- rbindlist(data_list)


#filter weekly data to specific dates and keep only those with visits data
main_data <- main_data %>%
  filter(date_range_start >= "2018-12-31" & date_range_start <= "2022-12-31") %>%
  filter(!is.na(visits_by_day))

#save intermediate data
saveRDS(main_data, "./data/processed_data/weekly_advan_business_data.RDS")

#turn into long format by day
main_data_subset <- main_data %>%
  dplyr::select(placekey, date_range_start, date_range_end, visits_by_day)

main_data_subset_long <- main_data_subset %>%
  # 1) Convert "visits_by_day" into list-column of numeric
  mutate(visits_list = str_extract_all(visits_by_day, "\\d+") %>% map(as.numeric)) %>%
  # 2) Build a list-column of dates
  mutate(date_list = map2(as.Date(date_range_start), as.Date(date_range_end),
                          ~ seq(.x, .y - 1, by = "day"))) %>%
  # 3) Unnest the two list-columns side by side
  unnest(cols = c(visits_list, date_list)) %>%
  rename(daily_visits = visits_list, date_long = date_list)

#bring back in weekly attributes from main data
main_data_imp_cols <- main_data %>%
  dplyr::select(city, median_dwell, normalized_visits_by_state_scaling, poi_cbg,
                raw_visitor_counts, visitor_home_aggregation, visitor_home_cbgs,
                latitude, longitude, placekey, raw_visit_counts, date_range_start) 
rm(main_data_subset, main_data)
gc()

main_data_subset_long <- main_data_subset_long %>%
  dplyr::select(-c(visits_by_day, date_range_end))
main_data_main <- main_data_subset_long %>%
  inner_join(
    main_data_imp_cols, 
    by = c("date_range_start", "placekey")
  )

## create normalized visitation metrics
main_data_main <- main_data_main %>%
  mutate(normalized_daily_visits = round((daily_visits/raw_visit_counts)*(normalized_visits_by_state_scaling)))

#save 
saveRDS(main_data_main, "./data/processed_data/advan_daily_business_visits.RDS")


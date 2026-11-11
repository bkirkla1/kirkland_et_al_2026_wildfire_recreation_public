
#-------------------------------------------------------------------------------
# 
# '02_dewey_advan_api_call.R': This script interacts with Dewey's API using the
# 'deweydatar' package to download all Advan visitation data for years
# 2019-2022
#
# DEWEY SUBSCRIPTION AND API KEY NEEDED TO RUN THIS SCRIPT AND DOWNLOAD DATA
#-------------------------------------------------------------------------------

#load settings
source('scripts/setup.R')

#load NPS polygon data, transform, and buffer by 100 km
poly_data <- readRDS("./data/processed_data/nps_data.RDS")
poly_data <- st_transform(poly_data, 4326)
poly_data_buff <- st_buffer(poly_data, 100000)

#establish api call parameters
apikey_ <- readLines("secrets/dewey_api_key.txt") 
pp_advan_wp <- "https://app.deweydata.io/external-api/v3/products/50f21464-9d1b-4f44-89b5-f633ac31e64f/files"
meta_endpoint <- "https://app.deweydata.io/external-api/v3/products/50f21464-9d1b-4f44-89b5-f633ac31e64f/files/metadata"

#check out file contents for all years
files_df <- deweydatar::get_file_list(apikey = apikey_, pp_advan_wp,
                                       start_date = "2019-01-01", end_date = "2022-12-31",
                                       print_info = T)

#get metadata
meta_data <- deweydatar::get_meta(apikey_, pp_advan_wp)

#create function that downloads files, checks if they already exists, and gives updates
download_files_mod <- function(files_df, dest_folder, filename_prefix = NULL, skip_exists = FALSE) {
  dest_folder = gsub("\\", "/", dest_folder, fixed = TRUE)
  if (!endsWith(dest_folder, "/")) {
    dest_folder = paste0(dest_folder, "/")
  }
  total_pages = attr(files_df, "selection_meta")$total_pages[1]
  num_files = nrow(files_df)
  
  for (i in 1:num_files) {
    message(paste0("Downloading file ", i, "/", num_files, 
                   " (file index = ", files_df$index[i], ")", 
                   ", page ", files_df$page[i], "/", total_pages))
    file_name = paste0(filename_prefix, files_df$file_name[i])
    dest_path = paste0(dest_folder, file_name)
    
    if (file.exists(dest_path) && skip_exists) {
      message(paste0("File already exists: ", dest_path))
      message("Skipping...")
      next
    }
    
    message(paste0("Writing ", dest_path))
    message("Please be patient. It may take a while...")
    
    req = request(files_df$link[i]) %>% req_retry(max_tries=15, backoff = ~15)
    response = req_perform(req)
    
    file_con = file(dest_path, "wb")
    writeBin(response$body, file_con)
    close(file_con)
    
    message("   ")
    
    # Introduce a delay to stay under 2 API hits per second
    Sys.sleep(0.2)  # 0.6 seconds delay
  }
}

# Define years to download
years <- 2019:2022

for (yr in years) {
  
  message(paste0("=== Processing year: ", yr, " ==="))
  
  # Set date range for this year
  start <- paste0(yr, "-01-01")
  end   <- paste0(yr, "-12-31")
  
  # Get file list for this year
  files_df <- deweydatar::get_file_list(
    apikey = apikey_,
    pp_advan_wp,
    start_date = start,
    end_date   = end,
    print_info = TRUE
  )
  
  # Create year-specific destination folder
  dest_folder <- paste0("E:/advan_data_", yr)
  if (!dir.exists(dest_folder)) dir.create(dest_folder, recursive = TRUE)
  
  # Download
  download_files_mod(files_df, dest_folder, skip_exists = TRUE)
  
}


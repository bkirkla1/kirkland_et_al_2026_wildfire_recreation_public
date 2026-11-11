#-------------------------------------------------------------------------------
#
# "04_filter_GH_PM25_data.R: this script reads in GH PM2.5 data from Wei et al. 
# 2023, scrops to CONUS for use in dataset creation
#
#-------------------------------------------------------------------------------

#load functions and packages
source("./scripts/Setup.R")

#define main year folders to loop over
main_folders <- list.files("E:/GH_PM25_data/", full.names = T)

#begin parallel session
plan(multisession, workers = 4) 

#for each folder, unzip every folder (monthly, contain daily rasters)
#ONLY NEED TO RUN ONCE, WILL NOT RUN IF ALREADY COMPLETED
for (j in 1:length(main_folders)){
  sub_folders <- list.files(main_folders[j], full.names = T)
  zipped_sub_folders <- sub_folders[grep("\\.zip", sub_folders)]
  
  #unzip them all
  future_lapply(zipped_sub_folders, function(z) {
    out_dir <- tools::file_path_sans_ext(z)
    if (!dir.exists(out_dir)) {
      unzip(z, exdir = out_dir)
    }
  })
  
  #.zip vs noncompressed file sizes are similar, so delete .zips
  file.remove(zipped_sub_folders)
}

##identify all non-zipped subfolders, loop through, crop to CONUS, and save in new folder

#id files, ignoring .zip and regular folders
all_files <- list.files("E:/GH_PM25_data/", full.names = T, recursive = T)
nc_files <- all_files[grep("\\.nc", all_files)]

#create conus bounding box to crop
conus_bbox <- ext(-125, -66.5, 24, 50)

#initialize directory to store conus cropped results
output_dir <- "E:/GH_PM25_data/GH_PM25_data_cropped/"
if(!file.exists(output_dir)){
  dir.create(output_dir, showWarnings = FALSE)}

#extract dates from the netcdf files to stack as one year raster
file_names <- basename(nc_files)
date_strings <- sub(".*_(\\d{8})_V1\\.nc", "\\1", file_names)
file_dates <- as.Date(date_strings, format = "%Y%m%d")
years <- format(file_dates, "%Y")

# 4. Loop by year: crop and stack
for (yr in unique(years)[2:4]) {
  cat("Processing year", yr, "\n")
  
  # Filter and sort files for this year
  year_idxs <- which(years == yr)
  files_year <- nc_files[year_idxs]
  dates_year <- file_dates[year_idxs]
  sorted_idx <- order(dates_year)
  files_year <- files_year[sorted_idx]
  
  cropped_list <- future_lapply(seq_along(files_year), function(i) {
    f <- files_year[i]
    d <- dates_year[i]
    r <- rast(f)
    r_crop <- crop(r, conus_bbox)
    
    out_day_file <- file.path(output_dir, paste0("GH_PM25_", d, "_CONUS.nc"))
    writeRaster(r_crop, out_day_file, overwrite = TRUE, filetype = "netCDF")
    
    return(out_day_file)  # Return path, not object
  })
}









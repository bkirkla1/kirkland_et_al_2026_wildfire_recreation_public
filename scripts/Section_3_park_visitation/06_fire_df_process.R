#-------------------------------------------------------------------------------
# "06_fire_df_process.R": This script merges all HMS fire points into one 
# data.table
#
# Important:
# Times given in UTC/GMT times
#
# Source: https://www.ospo.noaa.gov/Products/land/hms.html#about
#-------------------------------------------------------------------------------

#load settings
source("./scripts/setup.R")

#unzip all hms fire folders, and just keep shapefiles
zipF <- list.files(path = './data/fire_data', pattern = "*.zip", full.names = TRUE)
plyr::ldply(.data = zipF, .fun = unzip, exdir = './data/fire_data')
shp_files <- list.files(path = './data/fire_data', pattern = "*.shp", full.names = T)

#isolate shapefiles for 2019-2022
file_dates <- str_extract(shp_files, "(?<=hms_fire)\\d{8}(?=\\.shp)")
file_dates <- as.Date(file_dates, format = "%Y%m%d")
keep_ix <- file_dates >= as.Date("2018-12-31") &
  file_dates <= as.Date("2023-01-01")
shp_files <- shp_files[keep_ix]

# read the shp files into one spatial dataframe
fire_data <- st_as_sf(plyr::ldply(.data = shp_files, .fun = st_read, quiet = TRUE))

#project to albers conic
fire_data <- st_transform(fire_data, "EPSG:5070")

#Reformat date variable
fire_data <- subset(fire_data, select = c(YearDay, Time, geometry))
fire_data$Year <- as.numeric(substr(fire_data$YearDay, 1, 4))
fire_data$doy <- as.numeric(substr(fire_data$YearDay, 5,7))
#trim first time to relevant years
fire_data$Date <- as.Date(fire_data$doy, origin = paste0(as.character(fire_data$Year-1),"-12-31"))
fire_data$Time_form <- paste0(substr(fire_data$Time, 1, 2), ":", substr(fire_data$Time, 3, 4))
fire_data$fire_detect_UTC <- ymd_hm(paste(fire_data$Date,fire_data$Time_form))

#read in CONUS raster from 'process_grid' to crop fire boundary to free up some memory
CONUS_bound <- st_read(dd_name("/spatial_bound_data/CONUS_ARD_grid/conus_ard_grid.shp"), quiet = T)
CONUS_bound <- st_transform(CONUS_bound, "EPSG:5070")
CONUS_bound <- st_union(CONUS_bound)
fire_data <- st_crop(fire_data, CONUS_bound)

#Drop unnecessary data
fire_data <- fire_data[!is.na(fire_data$fire_detect_UTC),]
fire_data <- subset(fire_data, select = c(fire_detect_UTC, geometry))

#save
saveRDS(fire_data, "./data/processed_data/fire_data.RDS")

#remove everything but .zip files from folder to save memory
all_file <- list.files(path = dd_name('/fire_data'), full.names = T)
non_zip <- all_file[which(!(all_file %in% zipF))]
file.remove(non_zip)




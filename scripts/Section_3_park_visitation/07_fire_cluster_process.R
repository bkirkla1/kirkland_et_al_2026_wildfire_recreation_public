#-------------------------------------------------------------------------------
# "07_fire_cluster_process.R": This script creates fire clusters from the hms
# fire points following Burke et al. 2021
#
# Important:
# Times given in UTC/GMT times
#
#
#-------------------------------------------------------------------------------

#load in settings and data
source('scripts/setup.R')
fire_data <- readRDS('./data/processed_data/fire_data.RDS')

#buffer fire points (see "Burke et al. 2022)

#reformat dates
fire_data$ymd <- as.Date(fire_data$fire_detect_UTC, "%Y-%m-%d", tz="UTC")

#divide fire data up into chunks to make more manageable and parallelize
unique_days <- unique(fire_data$ymd)
sections <- split(unique_days, cut(1:length(unique_days), breaks = 300, labels = F))
num_sects <- length(sections)
i_days_ind <- 1:length(unique_days)
i_sections <- split(i_days_ind, cut(1:length(i_days_ind), breaks = 300, labels = F))

#function to buffer fires, and union them, keeping only the intersected polygons
buffer_and_union <- function(i, fire_data,unique_days) {
  if (i>3){
    
    #choose day i and last 3 days before it  
    days <- c(unique_days[(i-3):i])
    fire_on_dates <- fire_data %>%
      filter(ymd %in% days)
    
    #buffer by 3 km, identify clusters, and union
    fod_buff <- st_buffer(fire_on_dates, dist = 3000)
    intersects <- st_intersects(fod_buff)
    fod_buff_list <- fod_buff[lengths(intersects) > 1,]
    
    #union results
    fire_union <- st_union(fod_buff_list)
    fire_union <- st_make_valid(fire_union)
    st_crs(fire_union) <- crs(fod_buff)
    
    #separate multipolygon into component polygons
    fire_sep <- st_cast(fire_union, "POLYGON")
    fire_sep <- st_sf(geometry = fire_sep)
    fire_sep <- st_make_valid(fire_sep)

    date <- as.character(unique_days[i])
    fire_sep <- fire_sep %>%
      mutate(fire_detect_UTC = date)

    return(fire_sep)
  }
  else{return(NULL)}
}

# Use pblapply to loop
fire_days <- pblapply(1:num_sects, function(j) {
  days <- i_sections[[j]]
  fire_days_j <- pblapply(days, buffer_and_union, fire_data = fire_data, unique_days = unique_days)
  print(j)
  return(rbindlist(fire_days_j))
})

fire_days_df <- rbindlist(fire_days)

saveRDS(fire_days, './data/processed_data/fire_buff.RDS')



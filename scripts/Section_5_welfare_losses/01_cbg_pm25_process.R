#-------------------------------------------------------------------------------
#
# '01_cbg_pm25_process.R': This script calculates smoke-attributable pm2.5 at 
# a given CBG
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

# load cbg shp
poly_data <- st_read("C:/Users/bkirk/OneDrive - University of Wyoming/EPSCoR_wildfire/Code/wildfire_recreation/data/spatial_bound_data/cb_2019_us_bg_500k")
poly_data <- st_transform(poly_data, "EPSG:5070")

#load smoke predictions
smoke_predicts = readRDS("./data/pm2.5_data/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")

#load 10km grid
grid_10km = read_sf("./data/pm2.5_data/10km_grid_wgs84/10km_grid_wgs84.shp")

#choose dates 2019 onward and filter data
dates_smoke <- seq.Date(ymd("20190101"), ymd("20221231"), by = "day")

#loop over one year at a time for memories sake
smoke_data_burke <- expand.grid(grid_id_10km = grid_10km$ID, date = dates_smoke)
smoke_data_burke <- left_join(smoke_data_burke, smoke_predicts, by = c("grid_id_10km", "date"))

#predict 0 for remaining grid cells on non-smoke days
smoke_data_burke = mutate(smoke_data_burke, smokePM_pred = replace_na(smokePM_pred, 0))

#calculate area-weighted mean for a given cbg

#convert to same crs
grid_10km <- st_transform(grid_10km, st_crs(poly_data))

# Add unique ID to each CBG polygon
poly_data$cbg_id <- seq_len(nrow(poly_data))

#precompute grid-polygon intersections
intersections <- st_intersection(grid_10km, poly_data)

#calculate areas and create weights
cbg_areas <- st_area(poly_data)
intersections$cbg_area <- cbg_areas[intersections$cbg_id]
intersections$int_area <- st_area(intersections)
intersections$weights <- intersections$int_area/intersections$cbg_area

# Create a lookup table of grid cells to polygons with weights
lookup_df <- data.frame(
  grid_id_10km = intersections$ID,
  cbg = intersections$GEOID,
  weight = as.numeric(intersections$weights)
)

# Perform the join (much faster than dplyr for large data)
smoke_data_with_weights <- smoke_data_burke %>%
  inner_join(lookup_df, by = "grid_id_10km")

#clear some memory
Sys.sleep(10)
rm(grid_10km, smoke_data_burke, poly_data, lookup_df, smoke_predicts, intersections)
gc()

# Convert to data.table if not already
setDT(smoke_data_with_weights)

#calculated area-weighted mean, max, etc.
poly_smoke_pm25 <- smoke_data_with_weights[, .(
    mean_smoke_pm2.5 = sum(smokePM_pred * weight, na.rm = TRUE),
    max_smoke_pm2.5 = max(smokePM_pred, na.rm = TRUE)
  ), by = .(cbg, date)]

#save result
saveRDS(poly_smoke_pm25, "./data/processed_data/daily_pm25_by_cbg.RDS")

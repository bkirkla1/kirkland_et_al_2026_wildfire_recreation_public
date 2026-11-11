#-------------------------------------------------------------------------------
#
# '04_safegraph_final_weekly_data_process.R': this script reads in the processed 
# Safegraph data and creates all final variables
#
#-------------------------------------------------------------------------------

#load relevant packages
source('scripts/setup.R')

#load data
main_data_long_og <- readRDS("./data/processed_data/safegraph_weekly_data_with_covariates.RDS")

#filter data to just include 38 parks consistent with daily regressions
main_data_visits <- readRDS("main_data_subset_final.RDS")
main_parks <- unique(main_data_visits$park)
main_data_long <- main_data_long_og %>%
  filter(park %fin% main_parks)

#create number of weeks of data indicator
main_data_long <- main_data_long %>%
  group_by(PLACEKEY) %>%
  mutate(n_weeks = n())

#####create variables needed for regression 

#adjust pm2.5 from units to numeric
main_data_long <- main_data_long %>%
  mutate(mean_weekly_pm25_park = as.numeric(mean_weekly_pm25_park),
         mean_smoke_pm2.5_weekly = as.numeric(mean_smoke_pm2.5_weekly)) %>%
  rename(mean_weekly_pm25 = mean_smoke_pm2.5_weekly)

#create time fixed effects

#create lookup table for week start date and end date pairings
unq_weeks <- data.frame(week_start_date = unique(main_data_long$week_start_date))

week_lookup <-  unq_weeks %>%
  # compute modal month and year
  mutate(
    start_m       = month(week_start_date),
    end_m         = month(week_start_date + days(6)),
    eos           = floor_date(week_start_date, "month") + days(days_in_month(week_start_date) - 1),
    days_in_start = as.integer(eos - week_start_date) + 1,
    month         = if_else(days_in_start >= 4L, start_m, end_m),
    year          = year(week_start_date)
  ) %>%
  dplyr::select(week_start_date, month, year)

#join with full data

# Convert to data.table 
setDT(main_data_long)
setDT(week_lookup)

# join
main_data_long[
  week_lookup,
  on = .(week_start_date),
  `:=`(
    month = i.month,
    year  = i.year
  )
]

#create holiday dummy
#Pull unique years 
years_needed <- unique(c(
  year(main_data_long$week_start),
  year(main_data_long$week_end)
))

#Get and sort all the holiday dates
holiday_dates <- sort(as.Date(holidayNYSE(years_needed)))

# 3) For each row, find the first holiday strictly after (or on) week_start
#    findInterval(x, vec) gives the index of the last vec[i] <= x.
#    So +1 gives the first holiday > week_start; then we check if that date ≤ week_end.
idx_first_after <- findInterval(main_data_long$week_start_date, holiday_dates) + 1L

# pull out that holiday (out‐of‐bounds → NA)
first_holiday <- holiday_dates[ idx_first_after ]
first_holiday[ idx_first_after > length(holiday_dates) ] <- NA

# Build dummy in one vectorized step
main_data_long <- main_data_long %>%
  mutate(
    holiday_dum = as.integer(!is.na(first_holiday) & first_holiday <= week_end_date)
  )

#convert temp from kelvins to centigrade
main_data_long <- main_data_long %>%
  mutate(tmmx = tmmx - 273.15,
         tmmn = tmmn - 273.15)

#bin weather vars
bin_col <- function(column, n_quantiles = 5, prefix = "Q") {
  
  # Create quantile bins
  binned_column <- cut(
    column,
    breaks = quantile(column, probs = seq(0, 1, length.out = n_quantiles + 1), na.rm = TRUE),
    include.lowest = TRUE,
    labels = paste0(prefix, 1:n_quantiles)
  )
  
  return(binned_column)
}

main_data_long$bin_tmmx <- bin_col(main_data_long$tmmx)
main_data_long$bin_vs <- bin_col(main_data_long$vs)
main_data_long$bin_park_temp <- bin_col(main_data_long$mean_weekly_temp_park)

#handle pr separately because of lots of 0s
pr_quantile <- quantile(main_data_long$pr, seq(0,1,0.01), na.rm = T)
main_data_long$bin_pr <- cut(
  main_data_long$pr,
  breaks = c(0, 0.0001, 1, 5, 10, 25, Inf), 
  labels = c("Zero", "0-1", "1-5", "5-10", "10-25", "25+"), 
  include.lowest = TRUE # Include the lowest value in the first bin
)

pr_quantile_park <- quantile(main_data_long$total_weekly_prcp_park, seq(0,1,0.01), na.rm = T)
main_data_long$bin_park_prcp <- cut(
  main_data_long$total_weekly_prcp_park,
  breaks = c(0, 0.0001, 1, 5, 10, 25, Inf), 
  labels = c("Zero", "0-1", "1-5", "5-10", "10-25", "25+"), 
  include.lowest = TRUE # Include the lowest value in the first bin
)

#recategorize pm2.5 weekly counts as factors
main_data_long <- main_data_long %>%
  mutate(across(dplyr::starts_with("days_aqi_"), ~ factor(., levels = 0:7, ordered = TRUE)))

#drop unneeded columns to shrink dataset
main_data_long <- main_data_long %>%
  dplyr::select(-c(placekey_weekid_weather_df, placekey_weekid, n_weeks))

#save results
saveRDS(main_data_long, "./data/processed_data/main_weekly_safegraph_data_final.RDS")




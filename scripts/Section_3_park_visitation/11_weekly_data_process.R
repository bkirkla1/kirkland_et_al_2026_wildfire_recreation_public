#-------------------------------------------------------------------------------
#
# '11_weekly_data_process.R': this script processes daily data up to the weekly level
# 
#
#-------------------------------------------------------------------------------

#load setings
source("./scripts/setup.R")

#load advan weekly data
visit_data <- readRDS("./data/processed_data/weekly_advan_data.RDS")

#drop time subscripts from date range, which converts to days in local time. 
visit_data <- visit_data %>%
  mutate(date_range_start = as.Date(date_range_start),
         date_range_end = as.Date(date_range_end)-1)

#create week_id_df for separate weeks
unq_weeks <- sort(unique(visit_data$date_range_start))
week_id_w <- 1:length(unq_weeks)
week_id_df_w <- data.frame(date_range_start = unq_weeks, week_id = week_id_w)

#assign week_ids in weekly data
visit_data <- merge(visit_data, week_id_df_w, by = "date_range_start")

#create week id df for individual days
dates <- seq(min(visit_data$date_range_start), max(visit_data$date_range_end), "days")
week_id_d <- rep(1:(length(dates)/7), each = 7)
week_id_df_d <- data.frame(date_LT = dates, week_id = week_id_d)

#load daily visitation dataset
visit_data_daily <- readRDS('./data/processed_data/main_data_subset_final.RDS')
visit_data_daily <- visit_data_daily %>%
  filter(!is.na(mean_smoke_pm2.5))

#create week id in daily visitation data
visit_data_daily <- merge(visit_data_daily, week_id_df_d, 
                          by = "date_LT")

#filter to only keep park-week combinations with complete 7 days of observations (COULD ADJUST)
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
    holiday_dum = as.integer(any(holiday_dum == 1, na.rm = TRUE)),
    median_dwell_weekly = median(median_dwell),
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
visit_data <- visit_data %>%
  rename(park = location_name)

visit_data <- merge(visit_data, visit_data_weekly_vars,
                         by = c("park", "week_id"))

#simplify
visit_data <- visit_data %>%
  dplyr::select(c(park, date_range_start, fire_within_100km_park, mean_weekly_pm25,
                  mean_weekly_temp, modal_aqi_cat, normalized_visits_by_state_scaling,
                  total_weekly_prcp, week_id, holiday_dum, median_dwell_weekly,
                  days_good, days_moderate, days_unhealthy_sens, days_unhealthy, days_very_unhealthy_or_hazardous,
                  dummy_unhealthy_or_worse, worst_aqi_cat_week)) %>%
  rename(Visits = normalized_visits_by_state_scaling)

#create lagged variables
bad_aqi_cats <- c("Unhealthy", "Very Unhealthy", "Hazardous")

visit_data <- visit_data %>%
  arrange(park, date_range_start) %>%
  group_by(park) %>%
  mutate(across(
    modal_aqi_cat,
    list(
      lag1 = ~lag(., 1),
      lag2 = ~lag(., 2),
      lag3 = ~lag(., 3),
      lag4 = ~lag(., 4)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup() %>%
  mutate(across(
    starts_with("modal_aqi_cat_lag"),
    ~ifelse(.x %in% bad_aqi_cats, 1, ifelse(is.na(.x), NA, 0)),
    .names = "{.col}_dum"
  ))

#create fixed effects
visit_data <- visit_data %>%
  mutate(
    # generate the 7 days for each week
    days = map(date_range_start, ~ seq(from = .x, to = .x+6, by = "days")),
    # extract months
    months = map(.x = days, .f = ~ lubridate::month(.x)),
    # choose modal month
    week_month = map_int(.x = months, ~ as.integer(names(which.max(table(.x)))))) %>%
  dplyr::select(-c(days, months)) %>%
  rename(month = week_month) %>%
  mutate(year = year(date_range_start))

#save data
saveRDS(visit_data, "./data/processed_data/main_data_subset_weekly.RDS")



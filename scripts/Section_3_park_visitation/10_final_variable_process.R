  #-------------------------------------------------------------------------------
  #
  # '10_final_variable_process.R': this script makes final, quick variables and filters 
  # the main dataset to include parks filtered from 'data_summary_stats.R'
  # 
  #-------------------------------------------------------------------------------
  
  #load packages and settings
  source('./scripts/setup.R')
  
  #load main dataset
  main_data_long <- readRDS('./data/processed_data/main_data.RDS')
  
  #####create variables needed for regression 
  
  #adjust pm2.5 from units to numeric
  main_data_long$mean_smoke_pm2.5 <- as.numeric(main_data_long$mean_smoke_pm2.5)
  
  #aqi categories
  # Function to categorize PM2.5 concentrations into AQI categories
  pm25_to_aqi_category <- function(pm25) {
    dplyr::case_when(
      pm25 <= 12.0 ~ "Good",
      pm25 > 12 & pm25 <= 35.4 ~ "Moderate",
      pm25 > 35.4 & pm25 <= 55.4 ~ "Unhealthy for Sensitive Groups",
      pm25 > 55.4 & pm25 <= 150.4 ~ "Unhealthy",
      pm25 > 150.4 & pm25 <= 250.4 ~ "Very Unhealthy",
      pm25 > 250.4 ~ "Hazardous",
      is.na(pm25) ~ NA, 
    )
  }
  
  main_data_long$aqi_cat <- pm25_to_aqi_category(main_data_long$mean_smoke_pm2.5)
  main_data_long$max_aqi_cat <- pm25_to_aqi_category(main_data_long$max_smoke_pm2.5)
  
  #combine very unhealthy and hazardous
  main_data_long <- main_data_long %>%
    mutate(aqi_cat = case_when(
      aqi_cat %in% c("Very Unhealthy", "Hazardous") ~ "Very Unhealthy or Hazardous",
      TRUE ~ as.character(aqi_cat)
    )) %>%
    mutate(aqi_cat = factor(
      aqi_cat,
      levels = c(
        "Good",
        "Moderate",
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy or Hazardous"
      )
    ))
  
  #order the aqi_cat factors
  main_data_long$aqi_cat <- factor(
    main_data_long$aqi_cat,
    levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy or Hazardous")
  )
  
  
  #adjust fire number days
  main_data_long <- main_data_long %>%
    mutate(fire_num_100km = as.numeric(fire_num_100km),
           fire_num_250km = as.numeric(fire_num_250km))
  
  #create wind categories
  wind_direction_category <- function(degrees) {
    dplyr::case_when(
      degrees >= 337.5 | degrees < 22.5  ~ "N",   
      degrees >= 22.5 & degrees < 67.5   ~ "NE",  
      degrees >= 67.5 & degrees < 112.5  ~ "E",   
      degrees >= 112.5 & degrees < 157.5 ~ "SE",  
      degrees >= 157.5 & degrees < 202.5 ~ "S",   
      degrees >= 202.5 & degrees < 247.5 ~ "SW",  
      degrees >= 247.5 & degrees < 292.5 ~ "W",   
      degrees >= 292.5 & degrees < 337.5 ~ "NW",
      is.na(degrees) ~ NA, # If pm25 is NA, set wind_cat to NA
    )
  }
  
  main_data_long$wind_cat <- wind_direction_category(main_data_long$th)
  
  #create time fixed effects
  main_data_long <- main_data_long %>%
    mutate(month = month(date_LT),
           year = year(date_LT), 
           weekend_dum = ifelse(weekdays(date_LT) %in% c("Saturday", "Sunday"), 1, 0),
           day_of_week = wday(date_LT)) 
  
  #create holiday dummy
  us_holidays <- data.frame(
    date = as.Date(c(
      "2019-01-01",
      "2019-01-21",
      "2019-02-18",
      "2019-05-27",
      "2019-07-04",
      "2019-09-02",
      "2019-10-14",
      "2019-11-11",
      "2019-11-28",
      "2019-12-25",
      "2020-01-01",
      "2020-01-20",
      "2020-02-17",
      "2020-05-25",
      "2020-07-04",
      "2020-09-07",
      "2020-10-12",
      "2020-11-11",
      "2020-11-26",
      "2020-12-25",
      "2021-01-01",
      "2021-01-18",
      "2021-02-15",
      "2021-05-31",
      "2021-06-19",
      "2021-07-04",
      "2021-09-06",
      "2021-10-11",
      "2021-11-11",
      "2021-11-25",
      "2021-12-25",
      "2022-01-01",
      "2022-01-17",
      "2022-02-21",
      "2022-05-30",
      "2022-06-19",
      "2022-07-04",
      "2022-09-05",
      "2022-10-10",
      "2022-11-11",
      "2022-11-24",
      "2022-12-25"
    )))
  
  # Create dummy variable for holiday
  main_data_long <- main_data_long %>%
    mutate(holiday_dum = ifelse(date_LT %fin% us_holidays$date, 1, 0))

  #modify class of fire distance
  main_data_long$fire_dist_km_bound <- as.numeric(main_data_long$fire_dist_km_bound)
  
  #create basic fire dummy
  main_data_long$fire_dum_250 <- ifelse(main_data_long$fire_dist_km_bound < 250, 1, 0)
  main_data_long$fire_dum_100 <- ifelse(main_data_long$fire_dist_km_bound < 100, 1, 0)
  main_data_long$fire_dum_0 <- ifelse(main_data_long$fire_dist_km_bound == 0, 1, 0)
  
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
  
  #handle pr separately because of lots of 0s
  pr_quantile <- quantile(main_data_long$pr, seq(0,1,0.01), na.rm = T)
  main_data_long$bin_pr <- cut(
    main_data_long$pr,
    breaks = c(0, 0.0001, 1, 5, 10, 25, Inf), 
    labels = c("Zero", "0-1", "1-5", "5-10", "10-25", "25+"), 
    include.lowest = TRUE # Include the lowest value in the first bin
  )
  
  #lag smoke 8 weeks
  #load burke data
  smoke_data <- readRDS("./data/processed_data/park_burke_pm25.RDS")

  #lag burke smoke_data
  smoke_data <- smoke_data %>%
    arrange(park, date_LT) %>%  
    group_by(park) %>%
    mutate(across(
      c(mean_smoke_pm2.5),
      list(
        lag1  = ~lag(.,  1),
        lag2  = ~lag(.,  2),
        lag3  = ~lag(.,  3),
        lag4  = ~lag(.,  4),
        lag5  = ~lag(.,  5),
        lag6  = ~lag(.,  6),
        lag7  = ~lag(.,  7),
        lag8  = ~lag(.,  8),
        lag9  = ~lag(.,  9),
        lag10 = ~lag(., 10),
        lag11 = ~lag(., 11),
        lag12 = ~lag(., 12),
        lag13 = ~lag(., 13),
        lag14 = ~lag(., 14),
        lag15 = ~lag(., 15),
        lag16 = ~lag(., 16),
        lag17 = ~lag(., 17),
        lag18 = ~lag(., 18),
        lag19 = ~lag(., 19),
        lag20 = ~lag(., 20),
        lag21 = ~lag(., 21),
        lag22 = ~lag(., 22),
        lag23 = ~lag(., 23),
        lag24 = ~lag(., 24),
        lag25 = ~lag(., 25),
        lag26 = ~lag(., 26),
        lag27 = ~lag(., 27),
        lag28 = ~lag(., 28),
        lag29 = ~lag(., 29),
        lag30 = ~lag(., 30),
        lag31 = ~lag(., 31),
        lag32 = ~lag(., 32),
        lag33 = ~lag(., 33),
        lag34 = ~lag(., 34),
        lag35 = ~lag(., 35),
        lag36 = ~lag(., 36),
        lag37 = ~lag(., 37),
        lag38 = ~lag(., 38),
        lag39 = ~lag(., 39),
        lag40 = ~lag(., 40),
        lag41 = ~lag(., 41),
        lag42 = ~lag(., 42),
        lag43 = ~lag(., 43),
        lag44 = ~lag(., 44),
        lag45 = ~lag(., 45),
        lag46 = ~lag(., 46),
        lag47 = ~lag(., 47),
        lag48 = ~lag(., 48),
        lag49 = ~lag(., 49),
        lag50 = ~lag(., 50),
        lag51 = ~lag(., 51),
        lag52 = ~lag(., 52),
        lag53 = ~lag(., 53),
        lag54 = ~lag(., 54),
        lag55 = ~lag(., 55),
        lag56 = ~lag(., 56)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    ungroup() %>%
    dplyr::select(-c(max_smoke_pm2.5)) 
  
  #create aqi category and lags
  smoke_data <- smoke_data %>%
    mutate(aqi_cat = pm25_to_aqi_category(mean_smoke_pm2.5)) %>%
    arrange(park, date_LT) %>%  
    group_by(park) %>%
    mutate(across(
      c(aqi_cat),
      list(
        lag1  = ~lag(.,  1),
        lag2  = ~lag(.,  2),
        lag3  = ~lag(.,  3),
        lag4  = ~lag(.,  4),
        lag5  = ~lag(.,  5),
        lag6  = ~lag(.,  6),
        lag7  = ~lag(.,  7),
        lag8  = ~lag(.,  8),
        lag9  = ~lag(.,  9),
        lag10 = ~lag(., 10),
        lag11 = ~lag(., 11),
        lag12 = ~lag(., 12),
        lag13 = ~lag(., 13),
        lag14 = ~lag(., 14),
        lag15 = ~lag(., 15),
        lag16 = ~lag(., 16),
        lag17 = ~lag(., 17),
        lag18 = ~lag(., 18),
        lag19 = ~lag(., 19),
        lag20 = ~lag(., 20),
        lag21 = ~lag(., 21),
        lag22 = ~lag(., 22),
        lag23 = ~lag(., 23),
        lag24 = ~lag(., 24),
        lag25 = ~lag(., 25),
        lag26 = ~lag(., 26),
        lag27 = ~lag(., 27),
        lag28 = ~lag(., 28),
        lag29 = ~lag(., 29),
        lag30 = ~lag(., 30),
        lag31 = ~lag(., 31),
        lag32 = ~lag(., 32),
        lag33 = ~lag(., 33),
        lag34 = ~lag(., 34),
        lag35 = ~lag(., 35),
        lag36 = ~lag(., 36),
        lag37 = ~lag(., 37),
        lag38 = ~lag(., 38),
        lag39 = ~lag(., 39),
        lag40 = ~lag(., 40),
        lag41 = ~lag(., 41),
        lag42 = ~lag(., 42),
        lag43 = ~lag(., 43),
        lag44 = ~lag(., 44),
        lag45 = ~lag(., 45),
        lag46 = ~lag(., 46),
        lag47 = ~lag(., 47),
        lag48 = ~lag(., 48),
        lag49 = ~lag(., 49),
        lag50 = ~lag(., 50),
        lag51 = ~lag(., 51),
        lag52 = ~lag(., 52),
        lag53 = ~lag(., 53),
        lag54 = ~lag(., 54),
        lag55 = ~lag(., 55),
        lag56 = ~lag(., 56)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    ungroup()
  
  #create lag dummies
  bad_aqi_cats <- c("Unhealthy", "Very Unhealthy", "Hazardous")
  smoke_data <- smoke_data %>%
    mutate(
      # Creating dummy variables for aqi_cat and all lags
      aqi_cat_dum = ifelse(aqi_cat %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag1_dum = ifelse(aqi_cat_lag1 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag2_dum = ifelse(aqi_cat_lag2 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag3_dum = ifelse(aqi_cat_lag3 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag4_dum = ifelse(aqi_cat_lag4 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag5_dum = ifelse(aqi_cat_lag5 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag6_dum = ifelse(aqi_cat_lag6 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag7_dum = ifelse(aqi_cat_lag7 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag8_dum = ifelse(aqi_cat_lag8 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag9_dum = ifelse(aqi_cat_lag9 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag10_dum = ifelse(aqi_cat_lag10 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag11_dum = ifelse(aqi_cat_lag11 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag12_dum = ifelse(aqi_cat_lag12 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag13_dum = ifelse(aqi_cat_lag13 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag14_dum = ifelse(aqi_cat_lag14 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag15_dum = ifelse(aqi_cat_lag15 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag16_dum = ifelse(aqi_cat_lag16 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag17_dum = ifelse(aqi_cat_lag17 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag18_dum = ifelse(aqi_cat_lag18 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag19_dum = ifelse(aqi_cat_lag19 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag20_dum = ifelse(aqi_cat_lag20 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag21_dum = ifelse(aqi_cat_lag21 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag22_dum = ifelse(aqi_cat_lag22 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag23_dum = ifelse(aqi_cat_lag23 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag24_dum = ifelse(aqi_cat_lag24 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag25_dum = ifelse(aqi_cat_lag25 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag26_dum = ifelse(aqi_cat_lag26 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag27_dum = ifelse(aqi_cat_lag27 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag28_dum = ifelse(aqi_cat_lag28 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag29_dum = ifelse(aqi_cat_lag29 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag30_dum = ifelse(aqi_cat_lag30 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag31_dum = ifelse(aqi_cat_lag31 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag32_dum = ifelse(aqi_cat_lag32 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag33_dum = ifelse(aqi_cat_lag33 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag34_dum = ifelse(aqi_cat_lag34 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag35_dum = ifelse(aqi_cat_lag35 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag36_dum = ifelse(aqi_cat_lag36 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag37_dum = ifelse(aqi_cat_lag37 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag38_dum = ifelse(aqi_cat_lag38 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag39_dum = ifelse(aqi_cat_lag39 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag40_dum = ifelse(aqi_cat_lag40 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag41_dum = ifelse(aqi_cat_lag41 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag42_dum = ifelse(aqi_cat_lag42 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag43_dum = ifelse(aqi_cat_lag43 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag44_dum = ifelse(aqi_cat_lag44 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag45_dum = ifelse(aqi_cat_lag45 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag46_dum = ifelse(aqi_cat_lag46 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag47_dum = ifelse(aqi_cat_lag47 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag48_dum = ifelse(aqi_cat_lag48 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag49_dum = ifelse(aqi_cat_lag49 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag50_dum = ifelse(aqi_cat_lag50 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag51_dum = ifelse(aqi_cat_lag51 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag52_dum = ifelse(aqi_cat_lag52 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag53_dum = ifelse(aqi_cat_lag53 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag54_dum = ifelse(aqi_cat_lag54 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag55_dum = ifelse(aqi_cat_lag55 %in% bad_aqi_cats, 1, 0),
      aqi_cat_lag56_dum = ifelse(aqi_cat_lag56 %in% bad_aqi_cats, 1, 0)
    )
  

##create smoke leads
# add forward-looking (lead) variables for 2 weeks
smoke_data <- smoke_data %>%
  arrange(park, date_LT) %>%
  group_by(park) %>%
  mutate(across(
    c(mean_smoke_pm2.5),
    list(
      lead1 = ~lead(., 1),
      lead2 = ~lead(., 2),
      lead3 = ~lead(., 3),
      lead4 = ~lead(., 4),
      lead5 = ~lead(., 5),
      lead6 = ~lead(., 6),
      lead7 = ~lead(., 7),
      lead8 = ~lead(., 8),
      lead9 = ~lead(., 9),
      lead10 = ~lead(., 10),
      lead11 = ~lead(., 11),
      lead12 = ~lead(., 12),
      lead13 = ~lead(., 13),
      lead14 = ~lead(., 14)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup()

#create AQI cat leads 
smoke_data <- smoke_data %>%
  mutate(aqi_cat = pm25_to_aqi_category(mean_smoke_pm2.5)) %>%
  arrange(park, date_LT) %>%
  group_by(park) %>%
  mutate(across(
    c(aqi_cat),
    list(
      lead1 = ~lead(., 1),
      lead2 = ~lead(., 2),
      lead3 = ~lead(., 3),
      lead4 = ~lead(., 4),
      lead5 = ~lead(., 5),
      lead6 = ~lead(., 6),
      lead7 = ~lead(., 7),
      lead8 = ~lead(., 8),
      lead9 = ~lead(., 9),
      lead10 = ~lead(., 10),
      lead11 = ~lead(., 11),
      lead12 = ~lead(., 12),
      lead13 = ~lead(., 13),
      lead14 = ~lead(., 14)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup()

#create dummy variables for bad AQI leads
smoke_data <- smoke_data %>%
  mutate(
    aqi_cat_lead1_dum = ifelse(aqi_cat_lead1 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead2_dum = ifelse(aqi_cat_lead2 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead3_dum = ifelse(aqi_cat_lead3 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead4_dum = ifelse(aqi_cat_lead4 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead5_dum = ifelse(aqi_cat_lead5 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead6_dum = ifelse(aqi_cat_lead6 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead7_dum = ifelse(aqi_cat_lead7 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead8_dum = ifelse(aqi_cat_lead8 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead9_dum = ifelse(aqi_cat_lead9 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead10_dum = ifelse(aqi_cat_lead10 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead11_dum = ifelse(aqi_cat_lead11 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead12_dum = ifelse(aqi_cat_lead12 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead13_dum = ifelse(aqi_cat_lead13 %in% bad_aqi_cats, 1, 0),
    aqi_cat_lead14_dum = ifelse(aqi_cat_lead14 %in% bad_aqi_cats, 1, 0)
  )

#simplify smoke_data
smoke_data <- smoke_data %>%
  dplyr::select(-c(aqi_cat, mean_smoke_pm2.5, aqi_cat_dum)) %>%
  filter(date_LT %in% main_data_long$date_LT)

#join with main data
main_data_long <- main_data_long %>%
  left_join(smoke_data,
            by = c("park", "date_LT"))

#round daily visits
main_data_long <- main_data_long %>%
  mutate(daily_visits_norm_state = round(daily_visits_norm_state, 0))

#drop unneeded columns to shrink dataset
main_data_long <- main_data_long %>%
  dplyr::select(-c(city, daily_visits_norm_CBG, daily_visits_norm_tv, date_range_end,
                   date_range_start, distance_from_home, iso_country_code, latitude, longitude,
                   naics_code, normalized_visits_by_CBG_scaling, normalized_visits_by_state_scaling,
                   placekey, poi_cbg, polygon_wkt, postal_code, raw_visit_counts, raw_visitor_counts,
                   region, TOTAL_DEVICES_SEEN, visits_by_day)) %>%
  rename(raw_daily_visits = daily_visits)

#most parks feature incomplete data for december 2022, because parks were dropped from Advan dataset mid recording week starting december 1st, 
# drop december 2022 from sample
main_data_long <- main_data_long %>%
  filter(date_LT < "2022-12-01")

#change visits name for later table creation
main_data_long <- main_data_long %>%
  rename(Visits = daily_visits_norm_state)

#filter main dataset to include filtered parks
filtered_parks <- readRDS('./data/processed_data/filtered_park_df.RDS')
filtered_parks <- filtered_parks %>% dplyr::select(park)
main_data_subset <- main_data_long %>%
  filter(park %in% filtered_parks$park)

#save results
saveRDS(main_data_subset, "./data/processed_data/main_data_subset_final.RDS")
saveRDS(main_data_long, "./data/processed_data/main_data_all_final.RDS")



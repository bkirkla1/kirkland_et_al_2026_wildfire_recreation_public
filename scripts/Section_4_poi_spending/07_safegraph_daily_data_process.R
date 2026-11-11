#-------------------------------------------------------------------------------
#
# "07_safegraph_daily_data_process.R": this script takes monthly-level
# Safegraph spending data, converts to daily, and combines with daily visitation
# data and other relevant covariates
#
#-------------------------------------------------------------------------------


#load relevant packages
source('scripts/setup.R')

#### convert weekly data to daily
{
#load safegraph weekly data
raw_safegraph_data <- readRDS("./data/processed_data/raw_safegraph_data.RDS")


spend_long <- raw_safegraph_data %>%
  transmute(
    placekey = PLACEKEY,
    start_dt = ymd_hms(SPEND_DATE_RANGE_START, tz = "America/Denver"),
    end_dt   = ymd_hms(SPEND_DATE_RANGE_END,   tz = "America/Denver"),
    raw_num_transactions = RAW_NUM_TRANSACTIONS,
    online_spend         = ONLINE_SPEND,
    # parse the JSON-like daily array into numeric vector
    spend_vec = map(SPEND_BY_DAY, ~ as.numeric(fromJSON(.x)))
  ) %>%
  mutate(
    n_days  = map_int(spend_vec, length),
    # build a date sequence matching the length of the spend vector
    date    = map2(as.Date(start_dt), n_days, ~ .x + 0:(.y - 1))
  ) %>%
  dplyr::select(-start_dt, -end_dt, -n_days) %>%
  unnest(c(date, spend_vec), names_repair = "minimal") %>%
  rename(daily_spend = spend_vec)

#rename a few things
spend_long <- spend_long %>%
  rename(raw_num_transactions_monthly = raw_num_transactions,
         online_spend_monthly = online_spend)

#save results for now
saveRDS(spend_long, "./data/processed_data/safegraph_daily_spend.RDS")
}

#read back in saved data
spend_long <- readRDS("./data/processed_data/safegraph_daily_spend.RDS")

#bring in all covariates from advan daily visit data
advan_daily_visit_data <- readRDS("./data/processed_data/advan_daily_business_data_w_covariates.RDS")

#merge with safegraph spend data
spend_long <- spend_long %>% rename(date_LT = date)
setDT(spend_long)
setDT(advan_daily_visit_data)
spend_data_daily <- spend_long[advan_daily_visit_data, on = c("date_LT", "placekey")]

## create a few needed variables
spend_data_daily <- spend_data_daily %>%
  mutate(ln_spending = log(daily_spend+1))

# Define US holidays for given years
#modify holiday variable
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


# Create dummy variable for holiday and day after holiday
spend_data_daily <- spend_data_daily %>%
  mutate(
    holiday_dum = ifelse(date_LT %fin% us_holidays$date, 1, 0),
    day_after_hol = as.integer(
      (date_LT - 1) %fin% us_holidays$date |                            # day after any holiday
        (lubridate::wday(date_LT, week_start = 1) == 1 &                  # Monday ...
           (date_LT - 3) %fin% us_holidays$date)                          # ... after a Friday holiday
    )
  )


##calculate one year placebo park aqi category

#bring in pm2.5 data for 2023
burke_data <- readRDS("./data/processed_data/park_burke_pm25.RDS")

# Add a lead variable that aligns with PM2.5 from one year later
spend_data_daily <- spend_data_daily %>%
  mutate(date_LT_lead1y = date_LT %m+% years(1)) %>%  # precompute lead date
  left_join(
    burke_data %>%
      dplyr::select(park, date_LT, mean_smoke_pm2.5) %>%
      rename(mean_smoke_pm2.5_park_lead1y = mean_smoke_pm2.5),
    by = c("park", "date_LT_lead1y" = "date_LT")
  ) %>%
  dplyr::select(-date_LT_lead1y)

spend_data_daily <- spend_data_daily %>%
  mutate(
    aqi_cat_park_lead1y = case_when(
      mean_smoke_pm2.5_park_lead1y <= 12.0 ~ "Good",
      mean_smoke_pm2.5_park_lead1y <= 35.4 ~ "Moderate",
      mean_smoke_pm2.5_park_lead1y <= 55.4 ~ "Unhealthy for Sensitive Groups",
      mean_smoke_pm2.5_park_lead1y <= 150.4 ~ "Unhealthy",
      mean_smoke_pm2.5_park_lead1y <= 250.4 ~ "Very Unhealthy",
      mean_smoke_pm2.5_park_lead1y > 250.4 ~ "Hazardous",
      TRUE ~ NA_character_
    ),
    # merge "Very Unhealthy" and "Hazardous"
    aqi_cat_park_lead1y = case_when(
      aqi_cat_park_lead1y %in% c("Very Unhealthy", "Hazardous") ~ "Very Unhealthy or Hazardous",
      TRUE ~ aqi_cat_park_lead1y
    ),
    aqi_cat_park_lead1y = factor(
      aqi_cat_park_lead1y,
      levels = c(
        "Good",
        "Moderate",
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy or Hazardous"
      )
    )
  )

##create individual day lag park aqi dummies for 4 weeks in past, 1 week in advance.

#define lags/lead window
n_lags  <- 56   # 4 weeks back
n_leads <- 14    # 2 week ahead

# build lag/lead dummies on the full burke daily panel
lag_exprs  <- set_names(map(1:n_lags,  ~expr(lag(any_smoke,  !!.x))), sprintf("smoke_lag%02d",  1:n_lags))
lead_exprs <- set_names(map(1:n_leads, ~expr(lead(any_smoke, !!.x))), sprintf("smoke_lead%02d", 1:n_leads))

burke_lags <- burke_data %>%
  mutate(date_LT = as.Date(date_LT)) %>%
  distinct(park, date_LT, .keep_all = TRUE) %>%
  group_by(park) %>%
  complete(date_LT = seq(min(date_LT), max(date_LT), by = "day")) %>%
  arrange(park, date_LT) %>%
  mutate(
    any_smoke = as.integer(mean_smoke_pm2.5 > 55.4),
    !!!lag_exprs,
    !!!lead_exprs
  ) %>%
  ungroup() %>%
  mutate(
    smoke_lag_wk1  = as.integer(if_any(sprintf("smoke_lag%02d", 1:7),   ~ .x == 1)),
    smoke_lag_wk2  = as.integer(if_any(sprintf("smoke_lag%02d", 8:14),  ~ .x == 1)),
    smoke_lag_wk3  = as.integer(if_any(sprintf("smoke_lag%02d", 15:21), ~ .x == 1)),
    smoke_lag_wk4  = as.integer(if_any(sprintf("smoke_lag%02d", 22:28), ~ .x == 1)),
    smoke_lag_wk5  = as.integer(if_any(sprintf("smoke_lag%02d", 29:35), ~ .x == 1)),
    smoke_lag_wk6  = as.integer(if_any(sprintf("smoke_lag%02d", 36:42), ~ .x == 1)),
    smoke_lag_wk7  = as.integer(if_any(sprintf("smoke_lag%02d", 43:49), ~ .x == 1)),
    smoke_lag_wk8  = as.integer(if_any(sprintf("smoke_lag%02d", 50:56), ~ .x == 1)),
    smoke_lead_wk1 = as.integer(if_any(sprintf("smoke_lead%02d", 1:7),  ~ .x == 1)),
    smoke_lead_wk2 = as.integer(if_any(sprintf("smoke_lead%02d", 8:14),  ~ .x == 1))
  ) %>%
  dplyr::select(park, date_LT, starts_with("smoke_lag"), starts_with("smoke_lead"))

#join back into data
spend_data_daily <- spend_data_daily %>%
  left_join(burke_lags, by = c("park", "date_LT"))

#rename visits dependent variable
spend_data_daily <- spend_data_daily %>%
  rename(Visits = normalized_daily_visits_poi)

#clean up county fips variable for table outputs
spend_data_daily <- spend_data_daily %>%
  mutate(county = as.factor(county_fips))

#bring naics categories
naics_data <- readxl::read_excel("./data/NAICS_data/unique_naics_codes_workbook_20250904.xlsx")
naics_data <- naics_data %>%
  dplyr::select(unq_naics_codes, Category, `2-6DigitCodes vlookup`, Indoors) %>%
  rename(NAICS_CODE = unq_naics_codes, 
         NAICS_cat = Category,
         description = `2-6DigitCodes vlookup`) %>%
  mutate(NAICS_cat = ifelse(NAICS_cat == "Food & Beverage Services/Establishments",
                            "Restaurants & Bars", NAICS_cat),
         NAICS_cat = ifelse(NAICS_cat == "Food Stores",
                            "Food and Beverage Stores", NAICS_cat))

#merge into main data
spend_data_daily <- spend_data_daily %>%
  left_join(naics_data, by = "NAICS_CODE")

#set "Other" as the reference level for regressions
spend_data_daily <- spend_data_daily %>%
  mutate(
    NAICS_cat = factor(NAICS_cat),                # convert to factor
    NAICS_cat = relevel(NAICS_cat, ref = "Other") # set "Other" as baseline
  )

#create tourism dummy
spend_data_daily <- spend_data_daily %>%
  mutate(tourism_dum = ifelse(NAICS_cat != "Other", 1, 0))

#reduce to just western parks in our sample
eastern_parks <- c("Theodore Roosevelt National Park",
                   "Mammoth Cave National Park",
                   "New River Gorge National Park and Preserve",
                   "Shenandoah National Park",
                   "Acadia National Park",
                   "Indiana Dunes National Park",
                   "Big Bend National Park",
                   "Badlands National Park",
                   "Isle Royale National Park",
                   "Voyageurs National Park",
                   "Wind Cave National Park")

spend_data_daily <- spend_data_daily %>%
  filter(!park %fin% eastern_parks)

#drop na spending
spend_data_daily <- spend_data_daily %>%
  filter(!is.na(ln_spending))

#keep only poi-days with non-NA park visits data
spend_data_daily <- spend_data_daily %>% 
  filter(!is.na(normalized_daily_visits_park))

#scale park visits
spend_data_daily <- spend_data_daily %>%
  mutate(normalized_daily_visits_park_1000s = normalized_daily_visits_park/1000)

#save data for use in regressions
saveRDS(spend_data_daily, "./data/processed_data/main_spend_data_daily.RDS")


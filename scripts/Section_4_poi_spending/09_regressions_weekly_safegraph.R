#-------------------------------------------------------------------------------
#
# '09_regressions_weekly_safegraph.R': this script creates a weekly-level version
# of the daily spending dataset and estimates weekly-level regressions as a 
# robustness check on credit/debit report timing inconsistencies
#
#-------------------------------------------------------------------------------

#load relevant packages
source('scripts/setup.R')

#load data
main_data_long <- readRDS("./data/processed_data/main_weekly_safegraph_data_final.RDS")

#load daily data 
spend_data_daily_reduc <- readRDS("./data/processed_data/main_spend_data_daily.RDS")

#trim weekly data to same placekey-date range as daily data (since weekly sg data and advan park was used to create daily data sample)
main_data_long <- main_data_long %>%
  filter(week_start_date <= max(spend_data_daily_reduc$date_LT)) %>%
  filter(PLACEKEY %in% unique(spend_data_daily_reduc$placekey))

#create improved county variable output for regression table
main_data_long <- main_data_long %>%
  mutate(county = as.factor(county_fips))

#order factors
main_data_long <- main_data_long %>%
  mutate(modal_aqi_cat_park = factor(
    modal_aqi_cat_park,
    levels = c(
      "Good",
      "Moderate",
      "Unhealthy for Sensitive Groups",
      "Unhealthy",
      "Very Unhealthy or Hazardous"
    )),
    worst_aqi_cat_week_park = factor(
      worst_aqi_cat_week_park,
      levels = c(
        "Good",
        "Moderate",
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy or Hazardous"
      ))
    )

#create binary variable =1 if the week contains a day right after a weekday holiday

#list holidays
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
hol_dates <- us_holidays$date

# Day-after-holiday dates, mirroring the daily definition=
day_after_hol_dates <- sort(unique(c(
  hol_dates + 1,
  hol_dates[lubridate::wday(hol_dates, week_start = 1) == 5] + 3
)))

# TRUE if any date in `d` falls within [start, end] 
any_in_week <- function(d, start, end) {
  d <- sort(unique(d))
  findInterval(end, d) - findInterval(start - 1, d) > 0
}

main_data_long <- main_data_long %>%
  mutate(
    holiday_dum       = as.integer(any_in_week(hol_dates,           week_start_date, week_end_date)),
    day_after_hol_week = as.integer(any_in_week(day_after_hol_dates, week_start_date, week_end_date))
  )


#define minimal function for extra lines in weekly output
make_extralines_weekly <- function(models,
                                   weather_vars = c("tmmx", "vs", "pr",
                                                    "mean_weekly_temp", "total_weekly_prcp"),
                                   fire_vars    = c("fire_within_100km_park"),
                                   check_symbol = "\\checkmark") {
  
  fe_terms <- function(m) names(fixef(m))
  
  has_vars <- function(m, vars) {
    fml <- paste(deparse(formula(m)), collapse = " ")
    any(sapply(vars, function(v) grepl(v, fml)))
  }
  
  has_pm <- function(m) {
    any(grepl("\\^", fe_terms(m)) &
          grepl("park", fe_terms(m)) &
          grepl("month", fe_terms(m)) &
          !grepl("year", fe_terms(m)))
  }
  
  has_pmy <- function(m) {
    any(grepl("\\^", fe_terms(m)) &
          grepl("park", fe_terms(m)) &
          grepl("month", fe_terms(m)) &
          grepl("year", fe_terms(m)))
  }
  
  list(
    "Weather controls" =
      sapply(models, function(m)
        if (has_vars(m, weather_vars)) check_symbol else ""),
    
    "Fire controls" =
      sapply(models, function(m)
        if (has_vars(m, fire_vars)) check_symbol else ""),
    
    "Holiday" =
      sapply(models, function(m)
        if (any(grepl("holiday", fe_terms(m)))) check_symbol else ""),
    
    "Day after holiday" =
      sapply(models, function(m)
        if (any(grepl("day_after_hol", fe_terms(m)))) check_symbol else ""),
    
    "POI" =
      sapply(models, function(m)
        if (any(grepl("PLACEKEY", names(fixef(m))))) check_symbol else ""),
    
    "Park × Month × Year" =
      sapply(models, function(m)
        if (has_pmy(m)) check_symbol else "")
  )
}

make_simple_etable_weekly <- function(models,
                                      outfile,
                                      title,
                                      keep,
                                      dict          = dict_master,
                                      digits        = "r4",
                                      stats_digits  = "r4",
                                      insert_midrule = TRUE) {
  
  # Remove existing file
  if (file.exists(outfile)) file.remove(outfile)
  
  # Create table
  fixest::etable(
    models,
    tex   = TRUE,
    file  = outfile,
    title = title,
    
    keep  = keep,
    dict  = dict,
    
    fitstat = "n",
    drop.section = "fixef",
    extralines   = make_extralines_weekly(models),
    
    digits       = digits,
    digits.stats = stats_digits,
    fixef_sizes  = FALSE
  )
  
  # Optional horizontal line before FE / controls panel
  if (insert_midrule) {
    tex <- readLines(outfile)
    idx <- grep("^\\s*(Weather controls|Fire controls|Holiday)\\s*&", tex)
    
    if (length(idx) > 0) {
      tex <- append(tex, "\\midrule", after = idx[1] - 1)
      writeLines(tex, outfile)
    } else {
      warning("Could not find FE/control row; midrule not inserted.")
    }
  }
  
  invisible(outfile)
}

#define variable display 

dict_output_main <- c(
  "modal_aqi_cat_parkModerate"   = "Modal AQI: Moderate",
  "modal_aqi_cat_parkUnhealthyforSensitiveGroups" = "Modal AQI: Unhealthy for Sensitive Groups",
  "modal_aqi_cat_parkUnhealthy"  = "Modal AQI: Unhealthy",
  "modal_aqi_cat_parkVeryUnhealthyorHazardous" = "Modal AQI: Very Unhealthy or Hazardous",
  
  "worst_aqi_cat_week_parkModerate"   = "Worst AQI: Moderate",
  "worst_aqi_cat_week_parkUnhealthyforSensitiveGroups" = "Worst AQI: Unhealthy for Sensitive Groups",
  "worst_aqi_cat_week_parkUnhealthy"  = "Worst AQI: Unhealthy",
  "worst_aqi_cat_week_parkVeryUnhealthyorHazardous" = "Worst AQI: Very Unhealthy or Hazardous",

  "mean_weekly_temp_park" = "Mean temperature at park",
  "total_weekly_prcp_park" = "Total prcp. at park",
  "fire_within_100km_park" = "Fire within 100km of park",
  "as.factor(year)" = "Year",
  "park^as.factor(month)" = "Park:month",
  "park^month^year" = "Park:month:year",
  "as.factor(county_fips)" = "County",
  "as.factor(day_of_week)" = "Day of week",
  "weekly_spend" = "Spending"
)
  
#run regressions 
reg_aqi_modal_aqi <- fixest::fepois(weekly_spend ~ 
                                           modal_aqi_cat_park + 
                                            mean_weekly_temp_park + total_weekly_prcp_park + fire_within_100km_park | 
                                         holiday_dum + day_after_hol_week + as.factor(PLACEKEY) + park^as.factor(month)^as.factor(year),
                                          cluster = ~ park + county,
                                          data = main_data_long)
summary(reg_aqi_modal_aqi)

reg_aqi_worst_aqi <- fixest::fepois(weekly_spend ~ 
                                           worst_aqi_cat_week_park + 
                                           mean_weekly_temp_park + total_weekly_prcp_park + fire_within_100km_park | 
                                           holiday_dum + day_after_hol_week + as.factor(PLACEKEY) + park^as.factor(month)^as.factor(year),
                                         cluster = ~ park + county,
                                         data = main_data_long)
summary(reg_aqi_worst_aqi)

#collect models
models_main_weekly <- list(reg_aqi_modal_aqi, reg_aqi_worst_aqi)

#make table
make_simple_etable_weekly(
  models  = models_main_weekly,
  outfile = "./output/tables/main_safegraph_results_weekly.tex",
  title   = "PPLM regression of weekly spending on weekly smoke conditions at nearby national parks",
  keep    = "%aqi",   
  dict    = dict_output_main
)




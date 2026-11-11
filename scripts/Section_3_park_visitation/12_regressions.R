#-------------------------------------------------------------------------------
#
# '12_regressions.R': This script runs regressions for the effect of pm2.5 
# on net visitation alongside several robustness checks and additional analysis
#
#-------------------------------------------------------------------------------

#### load main packages and data

#load packages and settings
source('./scripts/setup.R')

#load data
main_data_subset <- readRDS("./data/processed_data/main_data_subset_final.RDS")
main_data_all <- readRDS("./data/processed_data/main_data_all_final.RDS")

#### setup to create consistently formatted tables

# create master dict file for table outputs
dict_master <- c(
  # ---- AQI ----
  "as.factor(aqi_cat)Moderate" = "AQI: Moderate",
  "as.factor(aqi_cat)UnhealthyforSensitiveGroups" = "AQI: Unhealthy for Sensitive Groups",
  "as.factor(aqi_cat)Unhealthy" = "AQI: Unhealthy",
  "as.factor(aqi_cat)VeryUnhealthyorHazardous" = "AQI: Very Unhealthy or Hazardous",
  
  "as.factor(max_aqi_cat)Moderate"   = "AQI: Moderate",
  "as.factor(max_aqi_cat)UnhealthyforSensitiveGroups" = "AQI: Unhealthy for Sensitive Groups",
  "as.factor(max_aqi_cat)Unhealthy"  = "AQI: Unhealthy",
  "as.factor(max_aqi_cat)Very Unhealthy or Hazardous" = "AQI: Very Unhealthy or Hazardous",
  
  "as.factor(modal_aqi_cat)Moderate"                    = "Modal AQI: Moderate",
  "as.factor(modal_aqi_cat)Unhealthy for Sensitive Groups" = "Modal AQI: Unhealthy for Sensitive Groups",
  "as.factor(modal_aqi_cat)Unhealthy"                   = "Modal AQI: Unhealthy",
  "as.factor(modal_aqi_cat)VeryUnhealthyorHazardous"               = "Modal AQI: Very Unhealthy or Hazardous",
  
  "as.factor(worst_aqi_cat_week)Moderate" = "Worst AQI: Moderate",
  "as.factor(worst_aqi_cat_week)Unhealthy for Sensitive Groups" = "Worst AQI: Unhealthy for Sensitive Groups",
  "as.factor(worst_aqi_cat_week)Unhealthy"                   = "Worst AQI: Unhealthy",
  "as.factor(worst_aqi_cat_week)VeryUnhealthyorHazardous"               = "Worst AQI: Very Unhealthy or Hazardous",
  
  "dummy_unhealthy_or_worse" = "Unhealthy or worse AQI",
  
  # ---- Linear pm2.5 ----
  "mean_smoke_pm2.5" = "Mean smoke PM2.5", 
  "I(mean_smoke_pm2.5^2)" = "Mean smoke PM2.5 squared",
  
  # ---- Placebo vars ---- 
  "pm25_lead1y_aqiModerate"   = "AQI +1 year: Moderate",
  "pm25_lead1y_aqiUnhealthy for Sensitive Groups" = "AQI +1 year: Unhealthy for Sensitive Groups",
  "pm25_lead1y_aqiUnhealthy"  = "AQI +1 year: Unhealthy",
  "pm25_lead1y_aqiVery Unhealthy or Hazardous" = "AQI +1 year: Very Unhealthy or Hazardous",
  
  "aqi_cat_lag7_dum" = "Smoke: Lag t-7",
  "aqi_cat_lag6_dum" = "Smoke: Lag t-6",
  "aqi_cat_lag5_dum" = "Smoke: Lag t-5",
  "aqi_cat_lag4_dum" = "Smoke: Lag t-4",
  "aqi_cat_lag3_dum" = "Smoke: Lag t-3",
  "aqi_cat_lag2_dum" = "Smoke: Lag t-2",
  "aqi_cat_lag1_dum" = "Smoke: Lag t-1",
  
  "aqi_cat_lead7_dum" = "Smoke: Lead t+7",
  "aqi_cat_lead6_dum" = "Smoke: Lead t+6",
  "aqi_cat_lead5_dum" = "Smoke: Lead t+5",
  "aqi_cat_lead4_dum" = "Smoke: Lead t+4",
  "aqi_cat_lead3_dum" = "Smoke: Lead t+3",
  "aqi_cat_lead2_dum" = "Smoke: Lead t+2",
  "aqi_cat_lead1_dum" = "Smoke: Lead t+1",
  
  # ---- Binned weather vars ----
  "bin_tmmxQ2" = "Max temp. Q2",
  "bin_tmmxQ3" = "Max temp. Q3",
  "bin_tmmxQ4" = "Max temp. Q4",
  "bin_tmmxQ5" = "Max temp. Q5",
  "bin_vsQ2" = "Mean wind speed Q2",
  "bin_vsQ3" = "Mean wind speed Q3",
  "bin_vsQ4" = "Mean wind speed Q4",
  "bin_vsQ5" = "Mean wind speed Q5",
  "bin_pr0-1" = "Cumulative precipitation 0-1mm",
  "bin_pr1-5" = "Cumulative precipitation 1-5mm",
  "bin_pr5-10" = "Cumulative precipitation 5-10mm",
  "bin_pr10-25" = "Cumulative precipitation 10-25mm",
  "bin_pr25+" = "Cumulative precipitation 25+mm",
  
  # ---- FIRE ----
  "fire_dist_km_bound" = "Fire distance",
  "I(fire_dist_km_bound^2)" = "Fire distance squared",
  "fire_dist_km_road" = "Fire distance from NPS roads",
  "I(fire_dist_km_road^2)" = "Fire distance from NPS roads squared",
  "as.factor(fire_distance_bin)[1,25)" = "Fire within 1 to 25 km of park",
  "as.factor(fire_distance_bin)[25,50)" = "Fire within 25 to 50 km of park",
  "as.factor(fire_distance_bin)[50,100)" = "Fire within 50 to 100 km of park",
  "as.factor(fire_distance_bin)[100,250)" = "Fire within 100 to 250 km of park",
  "as.factor(fire_distance_bin)250+" = "Fire greater than 250 km from park",
  "fire_num_100km" = "# of HMS fire points within 100km of park",
  "fire_dist_km_bound_filt" = "Fire distance from park boundary",
  "I(fire_dist_km_bound_filt^2" = "Fire distance from park boundary squared",
  "fire_dist_km_road_filt" = "Fire distance from NPS roads",
  "I(fire_dist_km_road_filt^2)" = "Fire distance from NPS roads squared",
  "as.factor(fire_distance_bin_filt)[1,25)" = "Fire within 1 to 25 km of park",
  "as.factor(fire_distance_bin_filt)[25,50)" = "Fire within 25 to 50 km of park",
  "as.factor(fire_distance_bin_filt)[50,100)" = "Fire within 50 to 100 km of park",
  "as.factor(fire_distance_bin_filt)[100,250)" = "Fire within 100 to 250 km of park",
  "as.factor(fire_distance_bin_filt)250+" = "Fire greater than 250 km from park",
  "fire_num_100km_filt" = "# of HMS fire points within 100km of park",
  "nearest_area_bound_km2_filt" = "Final area of nearest wildfire to park",
  
  # ---- WEATHER ----
  "tmmx" = "Max temp.",
  "vs"   = "Mean wind speed",
  "pr"   = "Cumulative precipitation",
  
  # ---- FIXED EFFECTS  ----
  "holiday_dum" = "Holiday",
  "as.factor(wind_cat)" = "Wind direction",
  "as.factor(day_of_week)" = "Day of week",
  "as.factor(park)" = "Park",
  "as.factor(month)" = "Month",
  "as.factor(year)" = "Year",
  "as.factor(park)-as.factor(month)" = "Park × Month",
  
  #---- weekly controls -----
  "fire_within_100km_park"    = "Fire within 100km of park",
  "mean_weekly_temp"                  = "Mean weekly temp.",
  "total_weekly_prcp"                    = "Total weekly precip.",
  
  # ---- Alt. dependent vars ----
  "ln_Visits" = "$Log(Visits)$",
  "raw_daily_visits" = "Raw Visits",
  "Weekly_visits" = "Weekly visits",
  
  #specification test vars and control vars
  "epa_base_pm25_aqiModerate" = "EPA aggregate PM2.5 100km: AQI Moderate",
  "epa_base_pm25_aqiUnhealthy for Sensitive Groups" = "EPA aggregate PM2.5 100km: AQI Unhealthy for Sensitive Groups",
  "epa_base_pm25_aqiUnhealthy" = "EPA aggregate PM2.5 100km: AQI Unhealthy",
  "epa_base_pm25_aqiVery Unhealthy or Hazardous" = "EPA aggregate PM2.5 100km: AQI Very Unhealthy or Hazardous",
  "epa_pm25_50km_aqiModerate" = "EPA aggregate PM2.5 50km: AQI Moderate",
  "epa_pm25_50km_aqiUnhealthy for Sensitive Groups" = "EPA aggregate PM2.5 50km: AQI Unhealthy for Sensitive Groups",
  "epa_pm25_50km_aqiUnhealthy" = "EPA aggregate PM2.5 50km: AQI Unhealthy",
  "epa_pm25_50km_aqiVery Unhealthy or Hazardous" = "EPA aggregate PM2.5 50km: AQI Very Unhealthy or Hazardous",
  "gh_pm25_aqiModerate" = "GH PM2.5: AQI Moderate",
  "gh_pm25_aqiUnhealthy for Sensitive Groups" = "GH PM2.5: AQI Unhealthy for Sensitive Groups",
  "gh_pm25_aqiUnhealthy" = "GH PM2.5: AQI Unhealthy",
  "gh_pm25_aqiVery Unhealthy or Hazardous" = "GH PM2.5: AQI Very Unhealthy or Hazardous",
  "main_pm25_gh_pm25_diff" = "Non-smoke PM2.5 (GH)",
  "main_pm25_epa_pm25_diff" = "Non-smoke PM2.5 (EPA)"
  
)

#define function to help with fixed effect line printouts (for abbreviated table)
make_extralines <- function(models,
                            weather_vars = c("tmmx", "vs", "pr"),
                            fire_vars    = c("fire_dist_km_bound"),
                            check_symbol = "\\checkmark") {
  
  fe_terms <- function(m) names(fixef(m))
  
  has_vars <- function(m, vars) {
    fml <- paste(deparse(formula(m)), collapse = " ")
    all(sapply(vars, function(v) grepl(v, fml)))
  }
  
  # true interaction detectors (must contain "^")
  has_pmy <- function(m) {
    ft <- fe_terms(m)
    any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & grepl("year", ft))
  }
  
  has_pm <- function(m) {
    ft <- fe_terms(m)
    any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & !grepl("year", ft))
  }
  
  # simple FE detectors (separate terms)
  has_park <- function(m) any(grepl("park",  fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  has_month <- function(m) any(grepl("month", fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  has_year <- function(m) any(grepl("year",  fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  
  list(
    # ---------------- CONTROLS ----------------
    "Weather controls" =
      sapply(models, function(m) {
        ft <- fe_terms(m)
        if (has_vars(m, weather_vars) && any(grepl("wind_cat", ft)))
          check_symbol else ""
      }),
    
    "Fire controls" =
      sapply(models, function(m) if (has_vars(m, fire_vars)) check_symbol else ""),
    
    "Holiday" =
      sapply(models, function(m) {
        if (any(grepl("holiday_dum", fe_terms(m)))) check_symbol else ""
      }),
    
    # ---------------- FIXED EFFECTS (HIERARCHICAL) ----------------
    "Day of week" =
      sapply(models, function(m) {
        if (any(grepl("day_of_week", fe_terms(m)))) check_symbol else ""
      }),
    
    "Park" =
      sapply(models, function(m) {
        if (!has_pm(m) && !has_pmy(m) && has_park(m)) check_symbol else ""
      }),
    
    "Month" =
      sapply(models, function(m) {
        if (!has_pm(m) && !has_pmy(m) && has_month(m)) check_symbol else ""
      }),
    
    "Year" =
      sapply(models, function(m) {
        if (!has_pmy(m) && has_year(m)) check_symbol else ""
      }),
    
    "Park × Month" =
      sapply(models, function(m) if (has_pm(m)) check_symbol else ""),
    
    "Park × Month × Year" =
      sapply(models, function(m) if (has_pmy(m)) check_symbol else "")
  )
}
  
#define function to help with fixed effect line printouts (for full table)
make_extralines_full <- function(models,
                                 check_symbol = "\\checkmark") {
  
  fe_terms <- function(m) names(fixef(m))
  
  # ---- interaction detectors ----
  has_pmy <- function(m) {
    ft <- fe_terms(m)
    any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & grepl("year", ft))
  }
  
  has_pm <- function(m) {
    ft <- fe_terms(m)
    any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & !grepl("year", ft))
  }
  
  # ---- simple FE detectors ----
  has_park  <- function(m) any(grepl("park",  fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  has_month <- function(m) any(grepl("month", fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  has_year  <- function(m) any(grepl("year",  fe_terms(m)) & !grepl("\\^", fe_terms(m)))
  
  list(
    # ---------------- FIXED EFFECTS ----------------
    "Holiday" =
      sapply(models, function(m) {
        if (any(grepl("holiday_dum", fe_terms(m)))) check_symbol else ""
      }),
    
    "Wind direction" =
      sapply(models, function(m) {
        if (any(grepl("wind_cat", fe_terms(m)))) check_symbol else ""
      }),
    
    "Park" =
      sapply(models, function(m) {
        if (!has_pm(m) && !has_pmy(m) && has_park(m)) check_symbol else ""
      }),
    
    "Month" =
      sapply(models, function(m) {
        if (!has_pm(m) && !has_pmy(m) && has_month(m)) check_symbol else ""
      }),
    
    "Year" =
      sapply(models, function(m) {
        if (!has_pmy(m) && has_year(m)) check_symbol else ""
      }),
    
    "Park × Month" =
      sapply(models, function(m) {
        if (has_pm(m)) check_symbol else ""
      }),
    
    "Park × Month × Year" =
      sapply(models, function(m) {
        if (has_pmy(m)) check_symbol else ""
      })
  )
}

#define function to create a simple, consistent etable output
make_simple_etable <- function(models,
                               outfile,
                               title,
                               keep,
                               dict           = dict_master,
                               digits         = "r4",
                               stats_digits   = "r4",
                               insert_midrule = TRUE,
                               col_spanners   = NULL,
                               col_sublabels  = NULL) {   # <-- new argument
  
  if (file.exists(outfile)) file.remove(outfile)
  
  fixest::etable(
    models,
    tex          = TRUE,
    file         = outfile,
    title        = title,
    keep         = keep,
    dict         = dict,
    fitstat      = "n",
    drop.section = "fixef",
    extralines   = make_extralines(models),
    digits       = digits,
    digits.stats = stats_digits,
    fixef_sizes  = FALSE
  )
  
  tex <- readLines(outfile)
  
  # ---- insert midrule before controls panel ----
  if (insert_midrule) {
    idx <- grep("^\\s*Weather controls\\s*&", tex)
    if (length(idx) > 0) {
      tex <- append(tex, "\\midrule", after = idx[1] - 1)
    } else {
      warning("Could not find 'Weather controls' row; midrule not inserted.")
    }
  }
  
  # ---- insert column spanners + sublabels ----
  # col_spanners:  named list, e.g. list("Conley SE" = 1:3, "Two-way Cluster" = 4:5)
  # col_sublabels: character vector, one entry per model column, e.g.
  #                c("100mi", "250mi", "500mi", "Park $\\times$ State", "Park $\\times$ Region")
  if (!is.null(col_spanners)) {
    
    # -- spanner row --
    cmidrules <- paste(
      sapply(col_spanners, function(cols) {
        sprintf("\\cmidrule(lr){%d-%d}", min(cols) + 1, max(cols) + 1)
      }),
      collapse = " "
    )
    
    spanner_cells <- sapply(seq_along(col_spanners), function(i) {
      cols   <- col_spanners[[i]]
      label  <- names(col_spanners)[i]
      sprintf("\\multicolumn{%d}{c}{%s}", length(cols), label)
    })
    
    spanner_row <- paste0(" & ", paste(spanner_cells, collapse = " & "), " \\\\")
    
    # -- sublabel row (one cell per model column) --
    sublabel_row <- NULL
    if (!is.null(col_sublabels)) {
      n_models <- length(models)
      if (length(col_sublabels) != n_models) {
        warning("col_sublabels length does not match number of models; sublabels skipped.")
      } else {
        sublabel_cells <- sapply(col_sublabels, function(x) {
          sprintf("\\multicolumn{1}{c}{\\small %s}", x)
        })
        sublabel_row <- paste0(" & ", paste(sublabel_cells, collapse = " & "), " \\\\")
      }
    }
    
    # -- inject before "Model:" row --
    model_row_idx <- grep("Model:", tex)
    if (length(model_row_idx) > 0) {
      insert_lines <- c(spanner_row, cmidrules)
      if (!is.null(sublabel_row)) {
        insert_lines <- c(insert_lines, sublabel_row, "\\midrule")
      }
      tex <- append(tex, insert_lines, after = model_row_idx[1] - 1)
    } else {
      warning("Could not find 'Model:' row; spanners not inserted.")
    }
  }
  
  writeLines(tex, outfile)
  invisible(outfile)
}

#### MAIN REGRESSIONS
{
#negbin regression with binned mean aqi effects
reg_aqi_no_fe <- fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                                 fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                 tmmx + vs + pr  | holiday_dum + as.factor(wind_cat),
                               data = main_data_subset,
                               cluster = ~ park)
summary(reg_aqi_no_fe)
  
  
reg_aqi_no_pm_fe <-  fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                           fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                           tmmx + vs + pr  |holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                          as.factor(park) + as.factor(month) + as.factor(year),
                        data = main_data_subset,
                        cluster = ~ park)
summary(reg_aqi_no_pm_fe)

reg_aqi_pm_fe <-  fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                                        fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                        tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                        as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                      data = main_data_subset,
                                      cluster = ~ park)
summary(reg_aqi_pm_fe)

reg_aqi_main <- fenegbin(Visits ~  as.factor(aqi_cat) +
                fire_dist_km_bound + I(fire_dist_km_bound^2) +
                tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                  as.factor(day_of_week) +
                 as.factor(park)^as.factor(month)^as.factor(year),
              data = main_data_subset,
              cluster = ~ park)

summary(reg_aqi_main)

##print results for abbreviated table main text

#define model list
models <- list(
  reg_aqi_no_fe,
  reg_aqi_no_pm_fe,
  reg_aqi_pm_fe,
  reg_aqi_main
)

make_simple_etable(
  models  = models,
  outfile = "./output/tables/main_regression_results.tex",
  title   = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks",
  keep = "%aqi"
)


##print results for full output table appendix

#create and clear file
outfile_aqi_full <- "./output/tables/main_regression_results_full.tex"
if (file.exists(outfile_aqi_full)) file.remove(outfile_aqi_full)

#make table
fixest::etable(
  models,
  
  tex = TRUE,
  file = outfile_aqi_full,
  title = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks",
  
  # AQI coefficients only (IMPORTANT % prefix)
  dict = dict_master,
  
  # Only show N
  fitstat = "n",
  
  # Remove default FE section
  drop.section = "fixef",
  
  # Custom FE / controls panel
  extralines = make_extralines_full(models),
  
  digits = "r4",
  digits.stats = "r4",
  fixef_sizes = FALSE
)
#add horizontal line to table
tex <- readLines(outfile_aqi_full)
# Find the first extraline row label (we use "Weather controls" as the anchor)
idx <- grep("^\\s*Holiday\\s*&", tex)

if (length(idx) > 0) {
  # Insert \midrule on the line *before* the first extraline row
  tex <- append(tex, "\\midrule", after = idx[1] - 1)
} else {
  warning("Could not find 'Weather controls' row in LaTeX output; midrule not inserted.")
}
writeLines(tex, outfile_aqi_full)

}

#### ----- Appendix figures ----- ####

#### Weekly scaling
{
  #load data
  data_weekly <- readRDS("./data/processed_data/main_data_subset_weekly.RDS")
  
  #rename visits var
  data_weekly <- data_weekly %>%
    rename(Weekly_visits = Visits)
  
  #order modal and worst aqi cats
  data_weekly <- data_weekly %>%
    mutate(modal_aqi_cat = factor(
      modal_aqi_cat,
      levels = c(
        "Good",
        "Moderate",
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy or Hazardous"
      )),
      worst_aqi_cat_week = factor(
        worst_aqi_cat_week,
        levels = c(
          "Good",
          "Moderate",
          "Unhealthy for Sensitive Groups",
          "Unhealthy",
          "Very Unhealthy or Hazardous"
        ))
    )
  
  ##run regressions
  
  #with modal aqi cat
  weekly_reg_modal_aqi_cat <- fenegbin(Weekly_visits ~ as.factor(modal_aqi_cat) +
                                         mean_weekly_temp + total_weekly_prcp + fire_within_100km_park |
                                         holiday_dum + as.factor(year) +
                                         as.factor(month)^as.factor(park),
                                       cluster = ~ park,
                                       data = data_weekly)
  summary(weekly_reg_modal_aqi_cat)
  
  weekly_reg_modal_aqi_cat_pmy <- fenegbin(Weekly_visits ~ as.factor(modal_aqi_cat) +
                                             mean_weekly_temp + total_weekly_prcp + fire_within_100km_park |
                                             holiday_dum +
                                             as.factor(month)^as.factor(park)^as.factor(year),
                                           cluster = ~ park,
                                           data = data_weekly)
  summary(weekly_reg_modal_aqi_cat_pmy)
  
  #with worst day aqi
  weekly_reg_worst_aqi <- fenegbin(Weekly_visits ~ as.factor(worst_aqi_cat_week) +
                                     mean_weekly_temp + total_weekly_prcp + fire_within_100km_park |
                                     as.factor(year) + holiday_dum +
                                     as.factor(month)^as.factor(park),
                                   cluster = ~ park,
                                   data = data_weekly)
  summary(weekly_reg_worst_aqi)
  
  weekly_reg_worst_aqi_pmy <- fenegbin(Weekly_visits ~ as.factor(worst_aqi_cat_week) +
                                         mean_weekly_temp + total_weekly_prcp + fire_within_100km_park | holiday_dum +
                                         as.factor(month)^as.factor(park)^as.factor(year),
                                       cluster = ~ park,
                                       data = data_weekly)
  summary(weekly_reg_worst_aqi_pmy)
  
  #store models
  models_weekly <- list(
    weekly_reg_worst_aqi,
    weekly_reg_worst_aqi_pmy,
    weekly_reg_modal_aqi_cat,
    weekly_reg_modal_aqi_cat_pmy
  )
  
  #define minimal function for extra lines in weekly output
  make_extralines_weekly <- function(models,
                                     weather_vars = c("tmmx", "vs", "pr",
                                                      "mean_weekly_temp", "total_weekly_prcp"),
                                     fire_vars    = c("fire_dist", "fire_within_100km"),
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
      
      "Park × Month" =
        sapply(models, function(m)
          if (has_pm(m)) check_symbol else ""),
      
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
  
  #run to create table 
  make_simple_etable_weekly(
    models  = models_weekly,
    outfile = "./output/tables/main_regression_results_weekly.tex",
    title   = "Negative binomial regressions of the effect of smoke AQI on weekly visitation at national parks",
    keep    = "%aqi"
  )
}

#### PLACEBO TEST
{
  #bring in pm2.5 data for 2023
  burke_data <- readRDS("./data/processed_data/park_burke_pm25.RDS")
  
  # Add a lead variable that aligns with PM2.5 from one year later
  main_data_subset <- main_data_subset %>%
    mutate(date_LT_lead1y = date_LT %m+% years(1)) %>%  # precompute lead date
    left_join(
      burke_data %>%
        dplyr::select(park, date_LT, mean_smoke_pm2.5) %>%
        rename(pm25_lead1y = mean_smoke_pm2.5),
      by = c("park", "date_LT_lead1y" = "date_LT")
    ) %>%
    dplyr::select(-date_LT_lead1y)
  
  
  main_data_subset <- main_data_subset %>%
    mutate(
      pm25_lead1y_aqi = case_when(
        pm25_lead1y <= 12.0 ~ "Good",
        pm25_lead1y <= 35.4 ~ "Moderate",
        pm25_lead1y <= 55.4 ~ "Unhealthy for Sensitive Groups",
        pm25_lead1y <= 150.4 ~ "Unhealthy",
        pm25_lead1y <= 250.4 ~ "Very Unhealthy",
        pm25_lead1y > 250.4 ~ "Hazardous",
        TRUE ~ NA_character_
      ),
      # merge "Very Unhealthy" and "Hazardous"
      pm25_lead1y_aqi = case_when(
        pm25_lead1y_aqi %in% c("Very Unhealthy", "Hazardous") ~ "Very Unhealthy or Hazardous",
        TRUE ~ pm25_lead1y_aqi
      ),
      pm25_lead1y_aqi = factor(
        pm25_lead1y_aqi,
        levels = c(
          "Good",
          "Moderate",
          "Unhealthy for Sensitive Groups",
          "Unhealthy",
          "Very Unhealthy or Hazardous"
        )
      )
    )
  
  ##placebo test with same day, one year in future
  
  #regressions
  reg_aqi_no_fe_placebo <- fenegbin(Visits ~  pm25_lead1y_aqi + 
                                      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                      tmmx + vs + pr  | as.factor(wind_cat) + holiday_dum,
                                    data = main_data_subset,
                                    cluster = ~ park)
  summary(reg_aqi_no_fe_placebo)
  
  
  reg_aqi_no_pm_fe_placebo <-  fenegbin(Visits ~  pm25_lead1y_aqi + 
                                          fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                          tmmx + vs + pr | as.factor(wind_cat)+ as.factor(day_of_week) + holiday_dum +
                                          as.factor(park) + as.factor(month) + as.factor(year),
                                        data = main_data_subset,
                                        cluster = ~ park)
  summary(reg_aqi_no_pm_fe_placebo)
  
  reg_aqi_pm_fe_placebo <-  fixest::fenegbin(Visits ~ pm25_lead1y_aqi + 
                                               fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                               tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                               as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                             data = main_data_subset,
                                             cluster = ~ park)
  summary(reg_aqi_pm_fe_placebo)
  
  reg_aqi_main_placebo <- fenegbin(Visits ~   pm25_lead1y_aqi +
                                     fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                     tmmx + vs + pr  | holiday_dum + as.factor(wind_cat) + 
                                     as.factor(day_of_week) +
                                     as.factor(park)^as.factor(month)^as.factor(year),
                                   data = main_data_subset,
                                   cluster = ~ park)
  
  summary(reg_aqi_main_placebo)
  
  #placebo test 7 days in future
  
  reg_placebo_daily_aqi_no_fe <- fenegbin(Visits ~ aqi_cat_lag7_dum +  aqi_cat_lag6_dum +  aqi_cat_lag5_dum +  aqi_cat_lag4_dum +  aqi_cat_lag3_dum +  aqi_cat_lag2_dum +  aqi_cat_lag1_dum +
                                            as.factor(aqi_cat) +
                                            aqi_cat_lead1_dum + aqi_cat_lead2_dum + aqi_cat_lead3_dum + aqi_cat_lead4_dum + aqi_cat_lead5_dum + aqi_cat_lead6_dum + aqi_cat_lead7_dum +
                                            fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                            tmmx + vs + pr | as.factor(wind_cat) + holiday_dum,
                                          data = main_data_subset, 
                                          cluster = ~ park)
  
  summary(reg_placebo_daily_aqi_no_fe)
  
  reg_placebo_daily_aqi_no_pm_fe <- fenegbin(Visits ~ aqi_cat_lag7_dum +  aqi_cat_lag6_dum +  aqi_cat_lag5_dum +  aqi_cat_lag4_dum +  aqi_cat_lag3_dum +  aqi_cat_lag2_dum +  aqi_cat_lag1_dum +
                                               as.factor(aqi_cat) +
                                               aqi_cat_lead1_dum + aqi_cat_lead2_dum + aqi_cat_lead3_dum + aqi_cat_lead4_dum + aqi_cat_lead5_dum + aqi_cat_lead6_dum + aqi_cat_lead7_dum +
                                               fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                               tmmx + vs + pr | holiday_dum +
                                               as.factor(wind_cat) + 
                                               as.factor(day_of_week) +
                                               as.factor(park) + as.factor(month) + as.factor(year)
                                             ,
                                             data = main_data_subset, 
                                             cluster = ~ park)
  summary(reg_placebo_daily_aqi_no_pm_fe)
  
  reg_placebo_daily_aqi_pm_fe <-  fixest::fenegbin(Visits ~ aqi_cat_lag7_dum +  aqi_cat_lag6_dum +  aqi_cat_lag5_dum +  aqi_cat_lag4_dum +  aqi_cat_lag3_dum +  aqi_cat_lag2_dum +  aqi_cat_lag1_dum +
                                                     as.factor(aqi_cat) +
                                                     aqi_cat_lead1_dum + aqi_cat_lead2_dum + aqi_cat_lead3_dum + aqi_cat_lead4_dum + aqi_cat_lead5_dum + aqi_cat_lead6_dum + aqi_cat_lead7_dum + 
                                                     fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                                     tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                                     as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                                   data = main_data_subset,
                                                   cluster = ~ park)
  summary(reg_placebo_daily_aqi_pm_fe)
  
  
  reg_placebo_daily_aqi <- fenegbin(Visits ~  aqi_cat_lag7_dum +  aqi_cat_lag6_dum +  aqi_cat_lag5_dum +  aqi_cat_lag4_dum +  aqi_cat_lag3_dum +  aqi_cat_lag2_dum +  aqi_cat_lag1_dum +
                                      as.factor(aqi_cat) +
                                      aqi_cat_lead1_dum + aqi_cat_lead2_dum + aqi_cat_lead3_dum + aqi_cat_lead4_dum + aqi_cat_lead5_dum + aqi_cat_lead6_dum + aqi_cat_lead7_dum + 
                                      fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                      tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                      as.factor(day_of_week) +
                                      as.factor(park)^as.factor(month)^as.factor(year),
                                    data = main_data_subset, 
                                    cluster = ~ park)
  
  summary(reg_placebo_daily_aqi)
  
  ###Combine placebo test figures
  
  models_placebo_total <- list(  reg_aqi_no_fe_placebo,
                                 reg_aqi_no_pm_fe_placebo,
                                 reg_aqi_pm_fe_placebo,
                                 reg_aqi_main_placebo,
                                 reg_placebo_daily_aqi_no_fe,
                                 reg_placebo_daily_aqi_no_pm_fe,
                                 reg_placebo_daily_aqi_pm_fe,
                                 reg_placebo_daily_aqi)
  make_simple_etable(
    models = models_placebo_total,
    outfile = "./output/tables/main_regression_results_placebo_combined.tex",
    keep = "%aqi",
    title = "Negative binomial placebo tests estimating the effect of future smoke on visitation
    at national parks"
  )
}

#### SPECIFICATION TEST
{
  # Get residuals from main results
  resids <- residuals(reg_aqi_main, type = "pearson")
  
  # Initialize NA column in full dataset
  main_data_subset$Residuals <- NA_real_
  
  # Get kept row indices (positive ones, not removed)
  keep_rows <- setdiff(seq_len(nrow(main_data_subset)), abs(reg_aqi_main$obs_selection$obsRemoved))
  
  # Assign residuals only to rows kept in the model
  main_data_subset$Residuals[keep_rows] <- resids
  
  #create binned aqi binned baseline pm2.5
  # Define helper function for PM2.5 → AQI categories
  pm25_to_aqi <- function(x) {
    cut(
      x,
      breaks = c(-Inf, 12.0, 35.4, 55.4, 150.4, 250.4, Inf),
      labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy", "Hazardous"),
      right = TRUE
    )
  }
  
  main_data_subset <- main_data_subset %>%
    mutate(
      epa_base_pm25_aqi = pm25_to_aqi(epa_base_pm25),
      epa_pm25_50km_aqi = pm25_to_aqi(epa_pm25_50km),
      gh_pm25_aqi       = pm25_to_aqi(GH_pm25_local)
      ) %>%
    # Merge "Very Unhealthy" and "Hazardous" and set final factor order
    mutate(across(ends_with("_aqi"), ~case_when(
      .x %in% c("Very Unhealthy", "Hazardous") ~ "Very Unhealthy or Hazardous",
      TRUE ~ as.character(.x)
    ))) %>%
    mutate(across(ends_with("_aqi"), ~factor(.x, levels = c(
      "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy or Hazardous"
    ))))
  
  
  #run regressions with 100km data
  resid_reg_epa_bin <- feols(Residuals ~ epa_base_pm25_aqi, 
                             data = main_data_subset, 
                             cluster = ~ park)
  summary(resid_reg_epa_bin)
  
  #run regressions with 50km data
  resid_reg_epa_bin_50km <- feols(Residuals ~ epa_pm25_50km_aqi, 
                                  data = main_data_subset, 
                                  cluster = ~ park)
  summary(resid_reg_epa_bin_50km)
  
  #run with gh data
  resid_reg_gh_aqi <- feols(Residuals ~ gh_pm25_aqi, 
                            data = main_data_subset, 
                            cluster = ~ park)
  summary(resid_reg_gh_aqi)
  
  
  #create regression table
  dict_output_resid <- c(
    "epa_base_pm25_aqiModerate" = "EPA aggregate PM2.5 100km: AQI Moderate",
    "epa_base_pm25_aqiUnhealthy for Sensitive Groups" = "EPA aggregate PM2.5 100km: AQI Unhealthy for Sensitive Groups",
    "epa_base_pm25_aqiUnhealthy" = "EPA aggregate PM2.5 100km: AQI Unhealthy",
    "epa_base_pm25_aqiVery Unhealthy or Hazardous" = "EPA aggregate PM2.5 100km: AQI Very Unhealthy or Hazardous",
    "epa_pm25_50km_aqiModerate" = "EPA aggregate PM2.5 50km: AQI Moderate",
    "epa_pm25_50km_aqiUnhealthy for Sensitive Groups" = "EPA aggregate PM2.5 50km: AQI Unhealthy for Sensitive Groups",
    "epa_pm25_50km_aqiUnhealthy" = "EPA aggregate PM2.5 50km: AQI Unhealthy",
    "epa_pm25_50km_aqiVery Unhealthy or Hazardous" = "EPA aggregate PM2.5 50km: AQI Very Unhealthy or Hazardous",
    "gh_pm25_aqiModerate" = "GH PM2.5: AQI Moderate",
    "gh_pm25_aqiUnhealthy for Sensitive Groups" = "GH PM2.5: AQI Unhealthy for Sensitive Groups",
    "gh_pm25_aqiUnhealthy" = "GH PM2.5: AQI Unhealthy",
    "gh_pm25_aqiVery Unhealthy or Hazardous" = "GH PM2.5: AQI Very Unhealthy or Hazardous"
  )
  
  outfile_resid <- "./output/tables/main_regression_resid_robust.tex"
  if (file.exists(outfile_resid)) file.remove(outfile_resid)
  
  fixest::etable(
    resid_reg_epa_bin,
    resid_reg_epa_bin_50km,
    resid_reg_gh_aqi,
    digits = "r4",
    digits.stats = "r4",
    tex = TRUE,         
    fitstat = "n",
    dict = dict_output_resid,
    order = names(dict_output_resid),
    title = "OLS regression of main negative binomial model residuals on aggregate (combined smoke and non-smoke) PM2.5",
    file = outfile_resid
  )
  
  
}

#### controlling for baseline pm2.5
{
  #epa control 
  reg_aqi_main_control_epa <- fenegbin(Visits ~  as.factor(aqi_cat) + epa_base_pm25_aqi +
                                         fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                         tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset,
                                       cluster = ~ park)
  summary(reg_aqi_main_control_epa)
  
  #gh control
  reg_aqi_main_control_gh <- fenegbin(Visits ~  as.factor(aqi_cat) + gh_pm25_aqi +
                                        fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                        tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                        as.factor(day_of_week) +
                                        as.factor(park)^as.factor(month)^as.factor(year),
                                      data = main_data_subset,
                                      cluster = ~ park)
  summary(reg_aqi_main_control_gh)
  
  #calculate differences
  main_data_subset <- main_data_subset %>%
    mutate(main_pm25_epa_pm25_diff = epa_base_pm25-mean_smoke_pm2.5,
           main_pm25_gh_pm25_diff = GH_pm25_local-mean_smoke_pm2.5)
  
  #epa control diff
  reg_aqi_main_control_epa_diff <- fenegbin(Visits ~  as.factor(aqi_cat) + main_pm25_epa_pm25_diff +
                                              fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                              tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                              as.factor(day_of_week) +
                                              as.factor(park)^as.factor(month)^as.factor(year),
                                            data = main_data_subset,
                                            cluster = ~ park)
  
  summary(reg_aqi_main_control_epa_diff)
  
  #gh control diff
  reg_aqi_main_control_gh_diff <- fenegbin(Visits ~  as.factor(aqi_cat) + main_pm25_gh_pm25_diff +
                                             fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                             tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                             as.factor(day_of_week) +
                                             as.factor(park)^as.factor(month)^as.factor(year),
                                           data = main_data_subset,
                                           cluster = ~ park)
  
  summary(reg_aqi_main_control_gh_diff)
  
  #make table
  models_pm25_control <- list(reg_aqi_main_control_epa, reg_aqi_main_control_gh, reg_aqi_main_control_epa_diff, reg_aqi_main_control_gh_diff)
  make_simple_etable(
    models  = models_pm25_control,
    outfile = "./output/tables/main_regression_results_control_base_pm25.tex",
    title   = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks, controlling for baseline $PM2.5$ and non-smoke attributable $PM2.5$",
    keep = "%aqi|diff"
  )
  
  
}

#### iteratively drop parks with unhealthy or hazardous days
{
  #id parks with greatest number of very unhealthy or hazardous days
  parks_w_bad_aqi <- main_data_subset %>% filter(aqi_cat %in% c("Unhealthy", "Very Unhealthy or Hazardous"))
  parks_w_bad_aqi <- unique(parks_w_bad_aqi$park)
  parks_w_bad_aqi <- sort(parks_w_bad_aqi)
  
  #loop and run main regression dropping these parks
  store_drop_models <- vector("list", length = length(parks_w_bad_aqi))
  names(store_drop_models) <- paste0("Drop: ", parks_w_bad_aqi)
  
  #loop and drop a single park per iteration
  for (i in 1:length(parks_w_bad_aqi)){
    park_to_drop = parks_w_bad_aqi[i]
    main_data_subset_drop_i <- main_data_subset %>%
      filter(!park %fin% park_to_drop)
    
    store_drop_models[[i]] <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                         fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                         tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset_drop_i,
                                       cluster = ~ park)
    
    #edit park name and store as attribute
    park_to_drop <- gsub(" National Park", "", park_to_drop)
    park_to_drop <- trimws(park_to_drop)
    attr(store_drop_models[[i]], "dropped_park") <- park_to_drop
    
  }
  
  #create function that makes extra park drop lines just for this table
  make_extralines_drop <- function(models,
                                   weather_vars = c("tmmx", "vs", "pr"),
                                   fire_vars    = c("fire_dist_km_bound"),
                                   check_symbol = "\\checkmark") {
    
    fe_terms <- function(m) names(fixef(m))
    
    has_vars <- function(m, vars) {
      fml <- paste(deparse(formula(m)), collapse = " ")
      all(sapply(vars, function(v) grepl(v, fml)))
    }
    
    has_pmy <- function(m) {
      ft <- fe_terms(m)
      any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & grepl("year", ft))
    }
    
    has_pm <- function(m) {
      ft <- fe_terms(m)
      any(grepl("\\^", ft) & grepl("park", ft) & grepl("month", ft) & !grepl("year", ft))
    }
    
    has_park <- function(m) any(grepl("park", fe_terms(m)) & !grepl("\\^", fe_terms(m)))
    has_month <- function(m) any(grepl("month", fe_terms(m)) & !grepl("\\^", fe_terms(m)))
    has_year  <- function(m) any(grepl("year",  fe_terms(m)) & !grepl("\\^", fe_terms(m)))
    
    list(
      
      # ---------------- CONTROLS ----------------
      "Weather controls" =
        sapply(models, function(m) {
          ft <- fe_terms(m)
          if (has_vars(m, weather_vars) && any(grepl("wind_cat", ft)))
            check_symbol else ""
        }),
      
      "Fire controls" =
        sapply(models, function(m) if (has_vars(m, fire_vars)) check_symbol else ""),
      
      "Holiday" =
        sapply(models, function(m) {
          if (any(grepl("holiday_dum", fe_terms(m)))) check_symbol else ""
        }),
      
      # ---------------- FIXED EFFECTS ----------------
      "Day of week" =
        sapply(models, function(m) {
          if (any(grepl("day_of_week", fe_terms(m)))) check_symbol else ""
        }),
      
      "Park" =
        sapply(models, function(m) {
          if (!has_pm(m) && !has_pmy(m) && has_park(m)) check_symbol else ""
        }),
      
      "Month" =
        sapply(models, function(m) {
          if (!has_pm(m) && !has_pmy(m) && has_month(m)) check_symbol else ""
        }),
      
      "Year" =
        sapply(models, function(m) {
          if (!has_pmy(m) && has_year(m)) check_symbol else ""
        }),
      
      "Park × Month" =
        sapply(models, function(m) if (has_pm(m)) check_symbol else ""),
      
      "Park × Month × Year" =
        sapply(models, function(m) if (has_pmy(m)) check_symbol else ""),
      
      # ---------------- NEW: DROPPED PARK ROW ----------------
      "Dropped park" =
        sapply(models, function(m) {
          dp <- attr(m, "dropped_park")
          if (is.null(dp)) "" else dp
        })
    )
  }
  
  #define function to create a simple, consistent etable output for the drop park scenario
  make_simple_etable_drop <- function(models,
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
      extralines   = make_extralines_drop(models),
      
      digits       = digits,
      digits.stats = stats_digits,
      fixef_sizes  = FALSE
    )
    
    # Optional horizontal line before FE / controls panel
    if (insert_midrule) {
      tex <- readLines(outfile)
      idx <- grep("^\\s*Weather controls\\s*&", tex)
      
      if (length(idx) > 0) {
        tex <- append(tex, "\\midrule", after = idx[1] - 1)
        writeLines(tex, outfile)
      } else {
        warning("Could not find 'Weather controls' row; midrule not inserted.")
      }
    }
    
    invisible(outfile)
  }
  
  make_simple_etable_drop(
    models  = store_drop_models,
    outfile = "./output/tables/main_regression_results_drop_parks_iteration.tex",
    title   = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks, dropping one park per column to test whether estimated results are driven by singular parks",
    keep    = "%aqi"
  )
  
}

#### QUASI POISSON MODEL
{
  
  # quasi-poisson regression with binned mean aqi effects
  reg_aqi_no_fe_qp <- fixest::feglm(
    Visits ~ as.factor(aqi_cat) + 
      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
      tmmx + vs + pr | as.factor(wind_cat) + holiday_dum,
    data = main_data_subset,
    family = quasipoisson(),
    cluster = ~ park
  )
  summary(reg_aqi_no_fe_qp)
  
  
  # quasi-poisson with additional fixed effects (no park-month-year)
  reg_aqi_no_pm_fe_qp <- fixest::feglm(
    Visits ~ as.factor(aqi_cat) + 
      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
      tmmx + vs + pr | 
      as.factor(wind_cat) +
      as.factor(day_of_week) +
      as.factor(park) + 
      as.factor(month) + 
      as.factor(year) + holiday_dum,
    data = main_data_subset,
    family = quasipoisson(),
    cluster = ~ park
  )
  summary(reg_aqi_no_pm_fe_qp)
  
  reg_aqi_pm_fe_qp <-  fixest::feglm(Visits ~ as.factor(aqi_cat) + 
                                       fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                       tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                       as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                     data = main_data_subset,
                                     family = quasipoisson(),
                                     cluster = ~ park)
  summary(reg_aqi_pm_fe_qp)
  
  
  # quasi-poisson main specification (with park × month × year FE)
  reg_aqi_main_qp <- fixest::feglm(
    Visits ~ as.factor(aqi_cat) +
      fire_dist_km_bound + I(fire_dist_km_bound^2) +
      tmmx + vs + pr | 
      as.factor(wind_cat) + holiday_dum +
      as.factor(day_of_week) +
      as.factor(park)^as.factor(month)^as.factor(year),
    data = main_data_subset,
    family = quasipoisson(),
    cluster = ~ park
  )
  summary(reg_aqi_main_qp)
  
  
  #print results

  #define model list
  models_qp <- list(
    reg_aqi_no_fe_qp,
    reg_aqi_no_pm_fe_qp,
    reg_aqi_pm_fe_qp,
    reg_aqi_main_qp
  )
  
  make_simple_etable(
    models = models_qp,
    outfile = "./output/tables/main_regression_results_qp.tex",
    title = "Quasi-poisson regression of the effect of daily smoke AQI on visitation at national parks",
    keep = "%aqi"
  )
  
}

#### OLS LOG MODEL
{
  #calculate natural log of visits
  main_data_subset <- main_data_subset %>%
    mutate(ln_Visits = ifelse(Visits == 0, log(Visits+0.1), log(Visits)))
  
  # ols regression with binned mean aqi effects
  reg_aqi_no_fe_ols <- feols(
    ln_Visits ~ as.factor(aqi_cat) + 
      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
      tmmx + vs + pr  | as.factor(wind_cat) + holiday_dum,
    data = main_data_subset,
    cluster = ~ park
  )
  summary(reg_aqi_no_fe_ols)
  
  
  reg_aqi_no_pm_fe_ols <- feols(
    ln_Visits ~ as.factor(aqi_cat) + 
      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
      tmmx + vs + pr | 
      as.factor(wind_cat) + holiday_dum +
      as.factor(day_of_week) +
      as.factor(park) + 
      as.factor(month) + 
      as.factor(year),
    data = main_data_subset,
    cluster = ~ park
  )
  summary(reg_aqi_no_pm_fe_ols)
  
  reg_aqi_pm_fe_ols <-  feols(ln_Visits ~ as.factor(aqi_cat) + 
                                       fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                       tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                       as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                     data = main_data_subset,
                                     family = quasipoisson(),
                                     cluster = ~ park)
  summary(reg_aqi_pm_fe_ols)
  
  
  reg_aqi_main_ols <- feols(
    ln_Visits ~ as.factor(aqi_cat) +
      fire_dist_km_bound + I(fire_dist_km_bound^2) +
      tmmx + vs + pr |  holiday_dum +
      as.factor(wind_cat) + 
      as.factor(day_of_week) +
      as.factor(park)^as.factor(month)^as.factor(year),
    data = main_data_subset,
    cluster = ~ park
  )
  summary(reg_aqi_main_ols)
  
  #print results
  
  #define model list
  models_ols <- list(
    reg_aqi_no_fe_ols,
    reg_aqi_no_pm_fe_ols,
    reg_aqi_pm_fe_ols,
    reg_aqi_main_ols
  )
  
  make_simple_etable(
    models = models_ols,
    outfile = "./output/tables/main_regression_results_ols.tex",
    title = "OLS regression of the effect of daily smoke AQI on the natural log of visitation at national parks",
    keep = "%aqi"
  )
  
}

#### create combined table for all specification-related robustness checks
{
models_spec_robust <- list(reg_aqi_main_qp, reg_aqi_main_ols)
make_simple_etable(
  models = models_spec_robust,
  outfile = "./output/tables/main_regression_results_specification_robust_combined.tex",
  title = "OLS and quasi-poisson regressions of the effect of daily smoke AQI on visitation at national parks",
  keep = "%aqi"
)
}

#### Alternative standard errors
{
  
  #load data and get coordinates of park centroids
  nps_data <- readRDS("./data/processed_data/nps_data.RDS")
  nps_data <- st_transform(nps_data, 4326)
  parks_centroids <- st_centroid(nps_data)
  parks_centroids$lon <- st_coordinates(parks_centroids)[,1]
  parks_centroids$lat <- st_coordinates(parks_centroids)[,2]
  
  #simplify for join
  unq_parks <- unique(main_data_subset$park)
  parks_centroids <- parks_centroids %>%
    dplyr::select(UNIT_NAME, lon, lat) %>%
    filter(UNIT_NAME %fin% unq_parks) %>%
    rename(park = UNIT_NAME) %>%
    st_drop_geometry()
  
  main_data_subset <- main_data_subset %>%
    left_join(., parks_centroids, by = "park")
  
  #run simple conley model
  reg_aqi_conley_100 <- fenegbin(
    Visits ~ as.factor(aqi_cat) +
      fire_dist_km_bound + I(fire_dist_km_bound^2) +
      tmmx + vs + pr |
      holiday_dum + as.factor(wind_cat) +
      as.factor(day_of_week) +
      as.factor(park)^as.factor(month)^as.factor(year),
    
    data = main_data_subset,
    
    vcov = conley(cutoff = "100mi")
  )
  
  summary(reg_aqi_conley_100)
  
  reg_aqi_conley_250 <- fenegbin(
    Visits ~ as.factor(aqi_cat) +
      fire_dist_km_bound + I(fire_dist_km_bound^2) +
      tmmx + vs + pr |
      holiday_dum + as.factor(wind_cat) +
      as.factor(day_of_week) +
      as.factor(park)^as.factor(month)^as.factor(year),
    
    data = main_data_subset,
    
    vcov = conley(cutoff = "250mi")
  )
  
  summary(reg_aqi_conley_250)
  
  reg_aqi_conley_500 <- fenegbin(
    Visits ~ as.factor(aqi_cat) +
      fire_dist_km_bound + I(fire_dist_km_bound^2) +
      tmmx + vs + pr |
      holiday_dum + as.factor(wind_cat) +
      as.factor(day_of_week) +
      as.factor(park)^as.factor(month)^as.factor(year),
    
    data = main_data_subset,
    
    vcov = conley(cutoff = "500mi")
  )
  
  summary(reg_aqi_conley_500)
  
  #run with park and time clustering
  
  
  #park month
  reg_aqi_main_park_month_cluster <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                                fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                                tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                                as.factor(day_of_week) +
                                                as.factor(park)^as.factor(month)^as.factor(year),
                                              data = main_data_subset,
                                              cluster = ~ park + month)
  summary(reg_aqi_main_park_month_cluster)
  
  #run with park and nps region clustering
  reg_aqi_main_park_year_cluster <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                               fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                               tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                               as.factor(day_of_week) +
                                               as.factor(park)^as.factor(month)^as.factor(year),
                                             data = main_data_subset,
                                             cluster = ~ park + year)
  summary(reg_aqi_main_park_year_cluster)
  
  #run with newey west standard errors to allow for temporal correlation within parks
  reg_aqi_main_park_nw_6 <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                               fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                               tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                               as.factor(day_of_week) +
                                               as.factor(park)^as.factor(month)^as.factor(year),
                                             data = main_data_subset,
                                             NW(6) ~ park + date_LT)
  summary(reg_aqi_main_park_nw_6)
  
  reg_aqi_main_park_nw_30 <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                       fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                       tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                       as.factor(day_of_week) +
                                       as.factor(park)^as.factor(month)^as.factor(year),
                                     data = main_data_subset,
                                     NW(30) ~ park + date_LT)
  summary(reg_aqi_main_park_nw_30)
  
  #create table
  models_cluster_checks <- list(reg_aqi_conley_100, reg_aqi_conley_250, reg_aqi_conley_500,
                                reg_aqi_main_park_month_cluster, reg_aqi_main_park_year_cluster,
                                reg_aqi_main_park_nw_6, reg_aqi_main_park_nw_30)
  
  
  make_simple_etable(
    models  = models_cluster_checks,
    outfile = "./output/tables/main_regression_results_cluster_robustness.tex",
    title   = "Negative binomial regressions of the effect of daily smoke AQI on visits at national parks using Conley standard errors and alternative clustering of standard errors",
    keep    = "%aqi",
    col_spanners = list(
      "Conley SE"        = 1:3,
      "Two-way Cluster"  = 4:5,
      "Newey-West SE" = 6:7
    ),
    col_sublabels = c(
      "100 mi",
      "250 mi",
      "500 mi",
      "Park $\\times$ Month",
      "Park $\\times$ Year",
      "L = 6",
      "L = 30"
    )
  )
  
}

#### BINNED WEATHER COVARIATES
{
  
  #run three regressions
  reg_aqi_no_fe_weather_bin <- fenegbin(Visits ~ as.factor(aqi_cat) + 
                                          fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                          bin_tmmx + bin_vs + bin_pr | holiday_dum + as.factor(wind_cat),
                                        data = main_data_subset,
                                        cluster = ~ park)
  summary(reg_aqi_no_fe_weather_bin)
  
  
  reg_aqi_no_pm_fe_weather_bin <-  fenegbin(Visits ~ as.factor(aqi_cat) + 
                                              fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                              bin_tmmx + bin_vs + bin_pr  | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                              as.factor(park) + as.factor(month) + as.factor(year),
                                            data = main_data_subset,
                                            cluster = ~ park)
  summary(reg_aqi_no_pm_fe_weather_bin)
  
  reg_aqi_pm_fe_weather_bin <- fenegbin(Visits ~ as.factor(aqi_cat) + 
                                          fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                          bin_tmmx + bin_vs + bin_pr  | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                          as.factor(month)^as.factor(park) + as.factor(year),
                                        data = main_data_subset,
                                        cluster = ~ park)
  summary(reg_aqi_pm_fe_weather_bin)
  
  reg_aqi_main_weather_bin <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                         fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                         bin_tmmx + bin_vs + bin_pr
                                        | holiday_dum + as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset,
                                       cluster = ~ park)
  summary(reg_aqi_main_weather_bin)
  
  
  #define model list
  models_weather_bin <- list(
    reg_aqi_no_fe_weather_bin,
    reg_aqi_no_pm_fe_weather_bin,
    reg_aqi_pm_fe_weather_bin,
    reg_aqi_main_weather_bin
  )
  
  make_simple_etable(
    models  = models_weather_bin,
    outfile = "./output/tables/main_regression_results_weather_bin.tex",
    title   = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks with binned weather covariates",
    keep = "%aqi"
  )
  
}

#### WITH LINEAR MEAN PM2.5 and quadratic term
{
  #negbin regression with binned mean aqi effects
  reg_linear_no_fe <- fenegbin(Visits ~ mean_smoke_pm2.5 + 
                                 fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                 tmmx + vs + pr  | holiday_dum + as.factor(wind_cat),
                               data = main_data_subset,
                               cluster = ~ park)
  summary(reg_linear_no_fe)
  
  
  reg_linear_no_pm_fe <-  fenegbin(Visits ~ mean_smoke_pm2.5 + 
                                     fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                     tmmx + vs + pr  | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                     as.factor(park) + as.factor(month) + as.factor(year),
                                   data = main_data_subset,
                                   cluster = ~ park)
  summary(reg_linear_no_pm_fe)
  
  reg_linear_pm_fe <- fenegbin(Visits ~ mean_smoke_pm2.5 + 
                                          fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                 tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                          as.factor(month)^as.factor(park) + as.factor(year),
                                        data = main_data_subset,
                                        cluster = ~ park)
  summary(reg_linear_pm_fe)
  
  reg_linear_main <- fenegbin(Visits ~  mean_smoke_pm2.5 +
                                fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                tmmx + vs + pr  |holiday_dum + as.factor(wind_cat) + 
                                as.factor(day_of_week) +
                                as.factor(park)^as.factor(month)^as.factor(year),
                              data = main_data_subset,
                              cluster = ~ park)
  summary(reg_linear_main)
  
  reg_quadratic_main <- fenegbin(Visits ~  mean_smoke_pm2.5 + I(mean_smoke_pm2.5^2)+
                                   fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                   tmmx + vs + pr  |holiday_dum + as.factor(wind_cat) + 
                                   as.factor(day_of_week) +
                                   as.factor(park)^as.factor(month)^as.factor(year),
                                 data = main_data_subset,
                                 cluster = ~ park)
  summary(reg_quadratic_main)
  
  
  #create table
  models_linear <- list(
    reg_linear_no_fe, 
    reg_linear_no_pm_fe,
    reg_linear_pm_fe,
    reg_linear_main
  )
    
  make_simple_etable(
    models = models_linear,
    title = "Negative binomial regression of the effect of linear smoke $PM2.5$ on visitation at national parks",
    keep = "%pm2.5",
    outfile = "./output/tables/main_regression_results_linear.tex" 
  )

  
}

#### maximum daily aqi within park
{
  
  #order the max_aqi_cat factors
  main_data_subset$max_aqi_cat <- factor(
    main_data_subset$max_aqi_cat,
    levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
  )
  
  main_data_subset <- main_data_subset %>%
    mutate(max_aqi_cat = case_when(
      max_aqi_cat %in% c("Very Unhealthy", "Hazardous") ~ "Very Unhealthy or Hazardous",
      TRUE ~ as.character(aqi_cat)
    )) %>%
    mutate(max_aqi_cat = factor(
      max_aqi_cat,
      levels = c(
        "Good",
        "Moderate",
        "Unhealthy for Sensitive Groups",
        "Unhealthy",
        "Very Unhealthy or Hazardous"
      )
    ))
  
  #Poisson regression with binned mean aqi effects
  reg_aqi_no_fe_max_aqi <- fenegbin(Visits ~ as.factor(max_aqi_cat) + 
                                      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                      tmmx + vs + pr  |  holiday_dum + as.factor(wind_cat),
                                    data = main_data_subset,
                                    cluster = ~ park)
  summary(reg_aqi_no_fe_max_aqi)
  
  
  reg_aqi_no_pm_fe_max_aqi <-  fenegbin(Visits ~ as.factor(max_aqi_cat) + 
                                          fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                          tmmx + vs + pr  |  holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                          as.factor(park) + as.factor(month) + as.factor(year),
                                        data = main_data_subset,
                                        cluster = ~ park)
  summary(reg_aqi_no_pm_fe_max_aqi)
  
  reg_aqi_pm_fe_max_aqi <- fenegbin(Visits ~ as.factor(max_aqi_cat) + 
                                      fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                      tmmx + vs + pr |  holiday_dum +  as.factor(wind_cat)+ as.factor(day_of_week)+
                                      as.factor(month)^as.factor(park) + as.factor(year),
                                    data = main_data_subset,
                                    cluster = ~ park)
  summary(reg_aqi_pm_fe_max_aqi)
  
  reg_aqi_main_max_aqi <- fenegbin(Visits ~  as.factor(max_aqi_cat) +
                                     fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                     tmmx + vs + pr |  holiday_dum + as.factor(wind_cat) + 
                                     as.factor(day_of_week) +
                                     as.factor(park)^as.factor(month)^as.factor(year),
                                   data = main_data_subset,
                                   cluster = ~ park)
  summary(reg_aqi_main_max_aqi)
  
  #store models
  models_max_aqi <- list(
    reg_aqi_no_fe_max_aqi,
    reg_aqi_no_pm_fe_max_aqi,
    reg_aqi_pm_fe_max_aqi,
    reg_aqi_main_max_aqi
  )
  
  #make table
  make_simple_etable(
    models = models_max_aqi,
    title = "Negative binomial regression of the effect of daily maximum smoke AQI on visitation at national parks",
    outfile = "./output/tables/main_regression_results_max_aqi.tex",
    keep = "%aqi"
  )
  
}

#### combined table of the four specifications above
{
  models_var_definitions <- list(reg_aqi_main_weather_bin, reg_aqi_main_max_aqi, reg_linear_main, reg_quadratic_main)
  make_simple_etable(
    models = models_var_definitions, 
    outfile = "./output/tables/main_regression_results_bin_weather_linear_max_pm25_combined.tex",
    title = "Negative binomial regressions of the effect of daily smoke AQI on visitation at national parks
with binned weather covariates, with maximum daily AQI, with linear mean PM2.5, and with quadratic mean PM2.5",
    keep = "%aqi|pm2.5",
  )
  
}

#### ALT. FIRE MEASURES
{
  
  #fill in missing fire days (currently NAs) with the max distance value
  main_data_subset <- main_data_subset %>%
    mutate(fire_dist_km_bound_filt = ifelse(is.na(fire_dist_km_bound_filt),
                                            max(fire_dist_km_bound_filt, na.rm = T),
                                            fire_dist_km_bound_filt),
           fire_dist_km_road_filt = ifelse(is.na(fire_dist_km_road_filt),
                                           max(fire_dist_km_road_filt, na.rm = T),
                                           fire_dist_km_road_filt),
           fire_num_100km_filt = ifelse(is.na(fire_num_100km_filt), 0, 1),
           nearest_area_bound_km2_filt = ifelse(is.na(nearest_area_bound_km2_filt), 0, 1)
    )
  
  #create fire distance bins
  main_data_subset <- main_data_subset %>%
    mutate(
      fire_distance_bin = cut(
        fire_dist_km_bound,
        breaks = c(-0.1, 0, 25, 50, 100, 250, Inf),
        labels = c("[0,0]", "[1,25)", "[25,50)", "[50,100)", "[100, 250)", "250+"),
        right = T  # makes intervals like [1,25)
      ), 
      fire_num_100km = as.numeric(fire_num_100km),
      fire_distance_bin_filt = cut(
        fire_dist_km_bound_filt,
        breaks = c(-0.1, 0, 25, 50, 100, 250, Inf),
        labels = c("[0,0]", "[1,25)", "[25,50)", "[50,100)", "[100, 250)", "250+"),
        right = T  # makes intervals like [1,25)
      ),
      fire_dist_km_road = as.numeric(fire_dist_km_road),
      fire_dist_km_road_filt = as.numeric(fire_dist_km_road_filt)
    )
  
  ## with all HMS data
  
  #fire bins
  reg_aqi_main_binned_fire <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                         as.factor(fire_distance_bin) +
                                         tmmx + vs + pr |holiday_dum +as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset,
                                       cluster = ~ park)
  summary(reg_aqi_main_binned_fire)
  
  #fire dist from roads
  reg_aqi_main_fire_road <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                       fire_dist_km_road + I(fire_dist_km_road^2) +
                                       tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                       as.factor(day_of_week) +
                                       as.factor(park)^as.factor(month)^as.factor(year),
                                     data = main_data_subset,
                                     cluster = ~ park)
  summary(reg_aqi_main_fire_road)
  
  #number of fires within 100km count
  reg_aqi_main_fire_intensity <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                            fire_num_100km +
                                            tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                            as.factor(day_of_week) +
                                            as.factor(park)^as.factor(month)^as.factor(year),
                                          data = main_data_subset,
                                          cluster = ~ park)
  summary(reg_aqi_main_fire_intensity)
  
  models_fire_sens <- list(
    reg_aqi_main_binned_fire,
    reg_aqi_main_fire_road,
    reg_aqi_main_fire_intensity
  )
  
  #define new function to remove fire control line from table to show fire coefficients
  make_simple_etable_fire <- function(models,
                                      outfile,
                                      title,
                                      keep,
                                      dict          = dict_master,
                                      digits        = "r4",
                                      stats_digits  = "r4",
                                      insert_midrule = TRUE) {
    
    # build extralines, then drop "Fire controls"
    extralines_fire <- make_extralines(models)
    extralines_fire[["Fire controls"]] <- NULL
    
    # remove existing file
    if (file.exists(outfile)) file.remove(outfile)
    
    # create table
    fixest::etable(
      models,
      tex   = TRUE,
      file  = outfile,
      title = title,
      
      keep  = keep,
      dict  = dict,
      
      fitstat = "n",
      drop.section = "fixef",
      extralines   = extralines_fire,
      
      digits       = digits,
      digits.stats = stats_digits,
      fixef_sizes  = FALSE
    )
    
    # optional horizontal rule
    if (insert_midrule) {
      tex <- readLines(outfile)
      idx <- grep("^\\s*(Weather controls|Holiday)\\s*&", tex)
      
      if (length(idx) > 0) {
        tex <- append(tex, "\\midrule", after = idx[1] - 1)
        writeLines(tex, outfile)
      }
    }
    
    invisible(outfile)
  }
  
  
  make_simple_etable_fire(
    models  = models_fire_sens,
    outfile = "./output/tables/main_regression_results_fire_sens.tex",
    title   = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks using alternative fire intensity and distance measures",
    keep    = "%(aqi|fire)"
  )
  
  ## repeat with filtered hms points
  
  #filter filtered fire data to match the nonfiltered na values
  main_data_fire_filtered <- main_data_subset %>%
    filter(!is.na(fire_dist_km_bound))
  
  #main specification
  reg_aqi_main_fire_filter <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                         fire_dist_km_bound_filt +
                                         tmmx + vs + pr | as.factor(wind_cat) + holiday_dum +
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_fire_filtered,
                                       cluster = ~ park)
  summary(reg_aqi_main_fire_filter)
  
  #fire bins
  reg_aqi_main_binned_fire_filter <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                                as.factor(fire_distance_bin_filt) +
                                                tmmx + vs + pr | as.factor(wind_cat) +holiday_dum +
                                                as.factor(day_of_week) +
                                                as.factor(park)^as.factor(month)^as.factor(year),
                                              data = main_data_fire_filtered,
                                              cluster = ~ park)
  summary(reg_aqi_main_binned_fire_filter)
  
  #fire dist from roads
  reg_aqi_main_fire_road_filt <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                            fire_dist_km_road_filt +
                                            tmmx + vs + pr | as.factor(wind_cat) + holiday_dum + 
                                            as.factor(day_of_week) +
                                            as.factor(park)^as.factor(month)^as.factor(year),
                                          data = main_data_fire_filtered,
                                          cluster = ~ park)
  summary(reg_aqi_main_fire_road_filt)
  
  #number of fires within 100km count
  reg_aqi_main_fire_intensity_filt <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                                 fire_num_100km_filt + 
                                                 tmmx + vs + pr | as.factor(wind_cat) +  holiday_dum +
                                                 as.factor(day_of_week) +
                                                 as.factor(park)^as.factor(month)^as.factor(year),
                                               data = main_data_fire_filtered,
                                               cluster = ~ park)
  summary(reg_aqi_main_fire_intensity_filt)
  
  #main specification alongside nearest fire area
  reg_aqi_main_fire_w_area_filt <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                              fire_dist_km_bound_filt + nearest_area_bound_km2_filt +
                                              tmmx + vs + pr  | as.factor(wind_cat) + holiday_dum +
                                              as.factor(day_of_week) +
                                              as.factor(park)^as.factor(month)^as.factor(year),
                                            data = main_data_fire_filtered,
                                            cluster = ~ park)
  summary(reg_aqi_main_fire_w_area_filt)
  
  #define models list for table
  models_fire_filt <- list(
    reg_aqi_main_fire_filter,
    reg_aqi_main_binned_fire_filter,
    reg_aqi_main_fire_road_filt,
    reg_aqi_main_fire_intensity_filt,
    reg_aqi_main_fire_w_area_filt)
  
  make_simple_etable_fire(
    models = models_fire_filt,
    title = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks using alternative fire intensity and distance measures and filtered HMS fire points",
    keep    = "%(aqi|fire|area)",
    outfile = "./output/tables/main_regression_results_fire_sens_filt.tex"
  )
  
  
}

#### RAW VISITS INSTEAD OF NORMALIZED,
{
  reg_aqi_main_raw <- fixest::fenegbin(raw_daily_visits ~  as.factor(aqi_cat) +
                                         fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                         tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset,
                                       cluster = ~ park)
  
  summary(reg_aqi_main_raw)
  
}

#### FULL 51 PARK SAMPLE
{
  reg_aqi_main_51_parks <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                      fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                      tmmx + vs + pr | as.factor(wind_cat) + holiday_dum +
                                      as.factor(day_of_week) +
                                      as.factor(park)^as.factor(month)^as.factor(year),
                                    data = main_data_all,
                                    cluster = ~ park)
  summary(reg_aqi_main_51_parks)
  
}

#### combined table of the mobile phone data assumption robustness checks
{
  models_mobile_phone_robust <- list(reg_aqi_main_raw, reg_aqi_main_51_parks)
  make_simple_etable(
    models =  models_mobile_phone_robust, 
    outfile = "./output/tables/main_regression_results_mobile_phone_robust_combined.tex",
    title = "Negative binomial regressions of the effect of daily smoke AQI on raw visits at national parks and the effect of daily smoke AQI on visitation using the entire sample of 51 national parks",
    keep = "%aqi",
  )
  
}

#### addressing potential park closures
{
  #read in nps monthly visits data and clean
  nps_monthly_data <- read.csv("./data/nps_official_data/nps_monthly_visit_data.csv")
  nps_monthly_data <- nps_monthly_data %>%
    dplyr::select(ParkName, Year, Month, RecreationVisits, NonRecreationVisits) %>%
    rename(park = ParkName, year = Year, month = Month,
           recreation_visits_nps = RecreationVisits, nonrecreation_visits_nps = NonRecreationVisits,) %>%
    mutate(park = gsub("\\bNP\\b", "National Park", park),
           park = ifelse(park == "Great Sand Dunes National Park & PRES", "Great Sand Dunes National Park", park),
           park = ifelse(park == "New River Gorge National Park & PRES", "New River Gorge National Park and Preserve", park)) 
  
  #merge into main data
  main_data_subset <- main_data_subset %>%
    left_join(., nps_monthly_data, by = c("park", "month", "year"))

  #subset data
  main_data_subset_no_nps_visits <- main_data_subset %>%
    filter(recreation_visits_nps > 0 | nonrecreation_visits_nps > 0)
  
  #re-estimate main regressions
  reg_aqi_main_no_closures <- fenegbin(Visits ~  as.factor(aqi_cat) +
                             fire_dist_km_bound + I(fire_dist_km_bound^2) +
                             tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                             as.factor(day_of_week) +
                             as.factor(park)^as.factor(month)^as.factor(year),
                           data = main_data_subset_no_nps_visits,
                           cluster = ~ park)
  
  summary(reg_aqi_main_no_closures)
  
  #now subset again to stricter definition to capture some more potential closures
  main_data_subset_no_nps_visits_strict <- main_data_subset_no_nps_visits %>%
    group_by(park, month, year) %>%
    mutate(total_monthly_observed_visits_year_y = sum(Visits, na.rm = T)) %>%
    ungroup() %>%
    group_by(park, month) %>%
    mutate(median_monthly_observed_visits = median(total_monthly_observed_visits_year_y, na.rm = T)) %>%
    ungroup() %>%
    filter(total_monthly_observed_visits_year_y/median_monthly_observed_visits > 0.25)
  
  reg_aqi_main_no_closures_strict <- fenegbin(Visits ~  as.factor(aqi_cat) +
                                         fire_dist_km_bound + I(fire_dist_km_bound^2) +
                                         tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                                         as.factor(day_of_week) +
                                         as.factor(park)^as.factor(month)^as.factor(year),
                                       data = main_data_subset_no_nps_visits_strict,
                                       cluster = ~ park)
  
  summary(reg_aqi_main_no_closures_strict)
  
}


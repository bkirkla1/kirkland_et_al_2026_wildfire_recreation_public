
#-------------------------------------------------------------------------------
#
# '03_tc_regressions.R': This script runs TC regressions at the CBG level
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#load data
data <- readRDS("./data/processed_data/travel_cost_dataset.RDS")
Sys.sleep(15)

#filter to within 400 miles - MAIN SPECIFICATION
data <- data %>% 
  filter(travel_distance_m <= 643738)
gc()

## create functions for table creation

# ---------------------------
# TC: extra lines (controls + FE panel)
# ---------------------------
make_extralines_tc <- function(models,
                               weather_vars = c("mean_weekly_temp", "total_weekly_prcp"),
                               fire_vars    = c("fire_within_100km_park", "fire_dist", "fire_num"),
                               demo_vars    = c("median_ageE", "median_incomeE", "prop_higher_ed", "prop_white"),
                               check_symbol = "\\checkmark") {
  
  # safer formula string (works for fixest models)
  fml_txt <- function(m) paste(deparse(formula(m)), collapse = " ")
  
  has_any <- function(m, vars) {
    txt <- fml_txt(m)
    any(sapply(vars, function(v) grepl(paste0("\\b", v, "\\b"), txt)))
  }
  
  fe_terms <- function(m) names(fixef(m))
  
  has_holiday <- function(m) {
    txt <- fml_txt(m)
    grepl("\\bholiday_", txt) || grepl("\\bholiday\\b", txt) || any(grepl("holiday", fe_terms(m)))
  }
  
  has_offset_pop <- function(m) {
    txt <- fml_txt(m)
    grepl("offset\\s*\\(", txt) && grepl("total_pop", txt)
  }
  
  has_pmy <- function(m) {
    ft <- fe_terms(m)
    any(grepl("park_month_year", ft)) 
  }
  
  list(
    # ---- CONTROLS ----
    "Weather controls" =
      sapply(models, \(m) if (has_any(m, weather_vars)) check_symbol else ""),
    
    "Fire controls" =
      sapply(models, \(m) if (has_any(m, fire_vars)) check_symbol else ""),
    
    "Demographics" =
      sapply(models, \(m) if (has_any(m, demo_vars)) check_symbol else ""),
    
    "Holiday" =
      sapply(models, \(m) if (has_holiday(m)) check_symbol else ""),
    
    "Population offset" =
      sapply(models, \(m) if (has_offset_pop(m)) check_symbol else ""),
    
    # ---- FIXED EFFECTS ----
    "Park × Month x Year" =
      sapply(models, \(m) if (has_pmy(m)) check_symbol else "")
  )
}

# ---------------------------
# TC: make a simple etable that only SHOWS PM2.5/AQI + travel cost terms
# and optionally wraps tabular in a LaTeX scalebox
# ---------------------------
make_simple_etable_tc <- function(models,
                                  outfile,
                                  title,
                                  keep,
                                  dict,
                                  order = NULL,
                                  digits         = "r4",
                                  stats_digits   = "r4",
                                  insert_midrule = TRUE,
                                  scale          = "\\tcscale",
                                  extralines_fun = make_extralines_tc  # <-- NEW
) {
  
  if (file.exists(outfile)) file.remove(outfile)
  
  # allow either a function(models)->list OR a precomputed list
  extralines_arg <- if (is.function(extralines_fun)) extralines_fun(models) else extralines_fun
  
  fixest::etable(
    models,
    tex   = TRUE,
    file  = outfile,
    title = title,
    
    keep  = keep,
    dict  = dict,
    order = order,
    
    fitstat      = "n",
    drop.section = "fixef",
    extralines   = extralines_arg,   # <-- UPDATED
    
    digits       = digits,
    digits.stats = stats_digits,
    fixef_sizes  = FALSE
  )
  
  # midrule above control panel
  # midrule above control panel
  if (insert_midrule) {
    tex <- readLines(outfile)
    
    # Prefer to anchor on first row of your extralines list
    ex_names <- names(extralines_arg)
    anchor   <- ex_names[1]  # e.g., "Travel cost" if you put it first
    
    # escape special regex chars in anchor (×, $, \, etc.)
    anchor_rx <- gsub("([][{}()^$.|*+?\\\\-])", "\\\\\\1", anchor)
    
    idx <- grep(paste0("^\\s*", anchor_rx, "\\s*&"), tex)
    
    # fallback to old behavior if not found
    if (length(idx) == 0) idx <- grep("^\\s*Weather controls\\s*&", tex)
    
    if (length(idx) > 0) {
      tex <- append(tex, "\\midrule", after = idx[1] - 1)
      writeLines(tex, outfile)
    } else {
      warning("Could not find extralines anchor row; midrule not inserted.")
    }
  }
  
  # wrap tabular in scalebox
  tex <- readLines(outfile)
  
  envs <- c("tabularx", "tabular\\*", "tabular", "longtable")
  start_idx <- NA_integer_
  env_used  <- NA_character_
  
  for (ev in envs) {
    idx <- grep(paste0("^\\s*\\\\begin\\{", ev, "\\}"), tex)
    if (length(idx) > 0) {
      start_idx <- idx[1]
      env_used  <- ev
      break
    }
  }
  
  if (!is.na(start_idx)) {
    tex <- append(tex, sprintf("\\scalebox{%s}{", scale), after = start_idx - 1)
    end_idx <- grep(paste0("^\\s*\\\\end\\{", env_used, "\\}"), tex)
    if (length(end_idx) > 0) {
      tex <- append(tex, "}", after = end_idx[1])
      writeLines(tex, outfile)
    } else {
      warning("Found table start but not the matching end; scalebox not inserted.")
    }
  } else {
    warning("Could not find tabular/tabularx/tabular*/longtable environment; scalebox not inserted.")
  }
}

# ---------------------------
# Variable labels (dict)
# (cleaned: removed duplicates)
# ---------------------------
dict_tc <- c(
  # ---- PM2.5 (linear) ----
  "mean_weekly_pm25"               = "Mean weekly PM2.5",
  "best_mean_pm25_subst_parks_cbg" = "Lowest mean weekly PM2.5 of substitute parks",
  
  # ---- Worst AQI ----
  "worst_aqi_cat_weekModerate"                   = "Worst AQI: Moderate",
  "worst_aqi_cat_weekUnhealthyforSensitiveGroups"= "Worst AQI: Unhealthy for Sensitive Groups",
  "worst_aqi_cat_weekUnhealthy"                  = "Worst AQI: Unhealthy",
  "worst_aqi_cat_weekVeryUnhealthyorHazardous"   = "Worst AQI: Very Unhealthy or Hazardous",
  
  # Interactions: worst AQI × travel cost
  "%^worst_aqi_cat_weekModerate:travel_cost_100s$" = "Worst AQI: Moderate x Travel costs ($100's)",
  "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:travel_cost_100s$" = "Worst AQI: Unhealthy for Sensitive Groups x Travel costs ($100's)",
  "%^worst_aqi_cat_weekUnhealthy:travel_cost_100s$" = "Worst AQI: Unhealthy x Travel costs ($100's)",
  "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:travel_cost_100s$" = "Worst AQI: Very Unhealthy or Hazardous x Travel costs ($100's)",
  
  # Interactions: worst AQI × age
  "median_ageE"    = "Median age",
  "%^worst_aqi_cat_weekModerate:median_ageE$" = "Worst AQI: Moderate x Median age",
  "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:median_ageE$" = "Worst AQI: Unhealthy for Sensitive Groups x Median age",
  "%^worst_aqi_cat_weekUnhealthy:median_ageE$" = "Worst AQI: Unhealthy x Median age",
  "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:median_ageE$" = "Worst AQI: Very Unhealthy or Hazardous x Median age",

  # Interactions: worst AQI × income
  "median_incomeE1000s" = "Median income ($1000s)",
  "%^worst_aqi_cat_weekModerate:median_incomeE$" = "Worst AQI: Moderate x Median income ($1000s)",
  "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:median_incomeE$" = "Worst AQI: Unhealthy for Sensitive Groups x Median income ($1000s)",
  "%^worst_aqi_cat_weekUnhealthy:median_incomeE$" = "Worst AQI: Unhealthy x Median income ($1000s)",
  "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:median_incomeE$" = "Worst AQI: Very Unhealthy or Hazardous x Median income ($1000s)",
  
  #lags
  "aqi_unh_or_worse" = "Smoke",
  "aqi_unh_or_worse_lag1" = "Smoke Lag w-1",
  "aqi_unh_or_worse_lag2" = "Smoke Lag w-2",
  "aqi_unh_or_worse_lag3" = "Smoke Lag w-3",
  "aqi_unh_or_worse_lag4" = "Smoke Lag w-4",
  
  #lag interacted with travel costs
  "travel_cost_100s:aqi_unh_or_worse_lag1" = "Smoke Lag w-1 x Travel costs ($100's)",
  "travel_cost_100s:aqi_unh_or_worse_lag2" = "Smoke Lag w-2 x Travel costs ($100's)",
  "travel_cost_100s:aqi_unh_or_worse_lag3" = "Smoke Lag w-3 x Travel costs ($100's)",
  "travel_cost_100s:aqi_unh_or_worse_lag4" = "Smoke Lag w-4 x Travel costs ($100's)",

  # ---- Substitute parks: best AQI ----
  "best_worst_aqi_subst_parks_cbgModerate"                    = "Best weekly AQI of substitute parks: Moderate",
  "best_worst_aqi_subst_parks_cbgUnhealthyforSensitiveGroups" = "Best weekly AQI of substitute parks: Unhealthy for Sensitive Groups",
  "best_worst_aqi_subst_parks_cbgUnhealthy"                   = "Best weekly AQI of substitute parks: Unhealthy",
  "best_worst_aqi_subst_parks_cbgVeryUnhealthyorHazardous"    = "Best weekly AQI of substitute parks: Very Unhealthy or Hazardous",
  
  #home cbgs
  "worst_aqi_cat_homeModerate" = "Worst AQI home CBG: Moderate",
  "worst_aqi_cat_homeUnhealthyforSensitiveGroups " = "Worst AQI home CBG: Unhealthy for Sensitive Groups",
  "worst_aqi_cat_homeUnhealthy" = "Worst AQI home CBG: Unhealthy",
  "worst_aqi_cat_homeVeryUnhealthyorHazardous" = "Worst AQI home CBG: Very Unhealthy or Hazardous",
  
  "mean_weekly_pm25_cbg" = "Mean weekly PM2.5 home CBG",
  
  # ---- Travel cost ----
  "travel_cost_100s"               = "Travel cost ($100's)"
)

# (interaction labels pulled from model coef names so you don't fight factor naming)
add_tc_interaction_dict <- function(model, prefix_label = "Travel cost ($100's) × ") {
  cn <- names(coef(model))
  inter <- grep("^travel_cost_100s:as\\.factor\\(park\\)", cn, value = TRUE)
  
  labs <- sub("^travel_cost_100s:as\\.factor\\(park\\)", prefix_label, inter)
  setNames(labs, inter)
}

# ---------------------------
# keep: show only PM2.5/AQI + travel cost + travel cost×park
# ---------------------------
keep_tc <- c(
  # PM2.5 + AQI
  "%^mean_weekly_pm25$",
  "%^best_mean_pm25_subst_parks_cbg$",
  
  "%^worst_aqi_cat_weekModerate$",
  "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups$",
  "%^worst_aqi_cat_weekUnhealthy$",
  "%^worst_aqi_cat_weekVeryUnhealthyorHazardous$",
  
  "%^best_worst_aqi_subst_parks_cbgModerate$",
  "%^best_worst_aqi_subst_parks_cbgUnhealthyforSensitiveGroups$",
  "%^best_worst_aqi_subst_parks_cbgUnhealthy$",
  "%^best_worst_aqi_subst_parks_cbgVeryUnhealthyorHazardous$",
  
  "%^worst_aqi_cat_homeModerate$",
  "%^worst_aqi_cat_homeUnhealthyforSensitiveGroups$",
  "%^worst_aqi_cat_homeUnhealthy$",
  "%^worst_aqi_cat_homeVeryUnhealthyorHazardous$",
  
  "%^mean_weekly_pm25_cbg",
  
  # Travel cost + park-specific slopes
  "%^travel_cost_100s$",
  "%^travel_cost_100s:as\\.factor\\(park\\)"
)

# ---------------------------
# order: force PM2.5/AQI rows BEFORE travel cost rows
# ---------------------------
order_tc <- c(
  # Smoke / AQI first
  "%^worst_aqi_cat_week",
  "%^worst_aqi_cat_week:travel_cost_100s",
  "%^worst_aqi_cat_week:median_ageE",
  "%^worst_aqi_cat_week:median_incomeE1000s",
  "%^best_worst_aqi_subst_parks_cbg",
  "%^worst_aqi_cat_home",
  "%^mean_weekly_pm25$",
  "%^best_mean_pm25_subst_parks_cbg$",
  "%^mean_weekly_pm25_cbg",
  
  # Then travel cost
  "%^travel_cost_100s$",
  "%^travel_cost_100s:as\\.factor\\(park\\)"
)

#### run 400 mile regressions (with substitution)
{
  #linear pm2.5
  reg_1_sub = fenegbin(
    Visits ~ mean_weekly_pm25 + fire_within_100km_park + best_mean_pm25_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  reg_1_sub_summary <- summary(reg_1_sub)
  reg_1_sub_summary
  saveRDS(reg_1_sub_summary, "./output/tc_reg_results/reg_linear_tc_400miles_sub.RDS")
  rm(reg_1_sub, reg_1_sub_summary)
  gc()
  

  #simple with worst aqi cat in week
  reg_2_sub = fenegbin(
    Visits ~ worst_aqi_cat_week + fire_within_100km_park + best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  reg_2_sub_summary <- summary(reg_2_sub)
  reg_2_sub_summary
  saveRDS(reg_2_sub_summary, "./output/tc_reg_results/reg_worst_tc_400miles_sub.RDS")
  rm(reg_2_sub, reg_2_sub_summary)
  gc()
  
  ## produce regression table (have to reload because of ram limitations)
  reg_1_sub_summary <- readRDS("./output/tc_reg_results/reg_linear_tc_400miles_sub.RDS")
  reg_2_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub.RDS")

  #collect models
  models_tc <- list(reg_2_sub_summary, reg_1_sub_summary)  
    
  # add interaction dict using the first model
  dict_tc_full <- c(dict_tc, add_tc_interaction_dict(models_tc[[1]]))
  
  #make table
  make_simple_etable_tc(
    models   = models_tc,
    outfile  = "./output/tables/main_tc_regression_results_sub.tex",
    title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks, using a 400 mile cutoff, a CBG population offset, and inclusive of alternative park substitution effects",
    keep     = keep_tc,
    dict     = dict_tc_full,
    order    = order_tc,
    extralines_fun = make_extralines_tc,
    scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
  )
  
  #clear data to save RAM
  rm(reg_1_sub_summary, reg_2_sub_summary)
  gc()
  Sys.sleep(30)
}

#### run 400 mile regressions (no substitution)
{
    #linear pm2.5
    reg_1_sub = fenegbin(
      Visits ~ mean_weekly_pm25 + fire_within_100km_park  +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_1_sub_summary <- summary(reg_1_sub)
    reg_1_sub_summary
    saveRDS(reg_1_sub_summary, "./output/tc_reg_results/reg_linear_tc_400miles_no_sub.RDS")
    rm(reg_1_sub, reg_1_sub_summary)
    gc()
    
    
    #simple with worst aqi cat in wee
    reg_2_sub = fenegbin(
      Visits ~ worst_aqi_cat_week + fire_within_100km_park +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_2_sub_summary <- summary(reg_2_sub)
    reg_2_sub_summary
    saveRDS(reg_2_sub_summary, "./output/tc_reg_results/reg_worst_tc_400miles_no_sub.RDS")
    rm(reg_2_sub, reg_2_sub_summary)
    gc()
    
    ## produce regression table (have to reload because of ram limitations)
    reg_1_sub_summary <- readRDS("./output/tc_reg_results/reg_linear_tc_400miles_no_sub.RDS")
    reg_2_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_no_sub.RDS")
    
    #collect models
    models_tc <- list(reg_2_sub_summary, reg_1_sub_summary)  
    
    # add interaction dict using the first model
    dict_tc_full <- c(dict_tc, add_tc_interaction_dict(models_tc[[1]]))
    
    #make table
    make_simple_etable_tc(
      models   = models_tc,
      outfile  = "./output/tables/main_tc_regression_results_no_sub.tex",
      title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks, using a 400 mile cutoff, a CBG population offset, and without inclusion of alternative park substitution effects",
      keep     = keep_tc,
      dict     = dict_tc_full,
      order    = order_tc,
      extralines_fun = make_extralines_tc,
      scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
    )
    
    
    #clear data to save RAM
    rm(reg_1_sub_summary, reg_2_sub_summary)
    gc()
    Sys.sleep(30)
  }

#### rerun main specifications with home cbg smoke
{
    #linear pm2.5
    reg_1_sub = fenegbin(
      Visits ~ mean_weekly_pm25 + mean_weekly_pm25_cbg +  fire_within_100km_park + best_mean_pm25_subst_parks_cbg +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_1_sub_summary <- summary(reg_1_sub)
    reg_1_sub_summary
    saveRDS(reg_1_sub_summary, "./output/tc_reg_results/reg_linear_tc_400miles_sub_home.RDS")
    rm(reg_1_sub, reg_1_sub_summary)
    gc()
    
    
    #simple with worst aqi cat in wee
    reg_2_sub = fenegbin(
      Visits ~ worst_aqi_cat_week + worst_aqi_cat_home + fire_within_100km_park + best_worst_aqi_subst_parks_cbg +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_2_sub_summary <- summary(reg_2_sub)
    reg_2_sub_summary
    saveRDS(reg_2_sub_summary, "./output/tc_reg_results/reg_worst_tc_400miles_sub_home.RDS")
    rm(reg_2_sub, reg_2_sub_summary)
    gc()
    
    ## produce regression table (have to reload because of ram limitations)
    reg_1_sub_summary <- readRDS("./output/tc_reg_results/reg_linear_tc_400miles_sub_home.RDS")
    reg_2_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub_home.RDS")
    
    #collect models
    models_tc <- list(reg_2_sub_summary, reg_1_sub_summary)  
    
    # add interaction dict using the first model
    dict_tc_full <- c(dict_tc, add_tc_interaction_dict(models_tc[[1]]))
    
    #make table
    make_simple_etable_tc(
      models   = models_tc,
      outfile  = "./output/tables/main_tc_regression_results_sub_home.tex",
      title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks, using a 400 mile cutoff, a CBG population offset, controlling for home CBG $PM2.5$, and inclusive of alternative park substitution effects",
      keep     = keep_tc,
      dict     = dict_tc_full,
      order    = order_tc,
      extralines_fun = make_extralines_tc,
      scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
    )
    
    #clear data to save RAM
    rm(reg_1_sub_summary, reg_2_sub_summary)
    gc()
    Sys.sleep(30)
}

#### run heterogeneity regressions
{
   ## run with tc-based heterogeneity for main text
  
  #without home location pm2.5
  reg_lag_worst_het = fenegbin(
    Visits ~ worst_aqi_cat_week +
      worst_aqi_cat_week:travel_cost_100s + best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) + 
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het)
  saveRDS(reg_lag_worst_het, "./output/tc_reg_results/tc_heterogeneity_reg.RDS")
  rm(reg_lag_worst_het)
  
  #with home location pm2.5
  reg_lag_worst_het_w_home = fenegbin(
    Visits ~ worst_aqi_cat_week +
      worst_aqi_cat_week:travel_cost_100s + worst_aqi_cat_home +
      best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) + 
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het_w_home)
  saveRDS(reg_lag_worst_het_w_home, "./output/tc_reg_results/tc_heterogeneity_reg_w_home.RDS")
  rm(reg_lag_worst_het_w_home)
  gc()
  
 #create table 
  #load results
  reg_lag_worst_het <- readRDS("./output/tc_reg_results/tc_heterogeneity_reg.RDS")
  reg_lag_worst_het_w_home <- readRDS("./output/tc_reg_results/tc_heterogeneity_reg_w_home.RDS")
  
  #simpler abbreviated table creation function
  make_extralines_tc_min <- function(models,
                                 weather_vars = c("mean_weekly_temp", "total_weekly_prcp"),
                                 fire_vars    = c("fire_within_100km_park", "fire_dist", "fire_num"),
                                 demo_vars    = c("median_ageE", "median_incomeE", "prop_higher_ed", "prop_white"),
                                 check_symbol = "\\checkmark") {
    
    # safer formula string (works for fixest models)
    fml_txt <- function(m) paste(deparse(formula(m)), collapse = " ")
    
    has_any <- function(m, vars) {
      txt <- fml_txt(m)
      any(vapply(vars, function(v) grepl(paste0("\\b", v, "\\b"), txt), logical(1)))
    }
    
    fe_terms <- function(m) names(fixef(m))
    
    has_holiday <- function(m) {
      txt <- fml_txt(m)
      grepl("\\bholiday_", txt) || grepl("\\bholiday\\b", txt) || any(grepl("holiday", fe_terms(m)))
    }
    
    has_offset_pop <- function(m) {
      txt <- fml_txt(m)
      grepl("offset\\s*\\(", txt) && grepl("total_pop", txt)
    }
    
    has_pmy <- function(m) {
      ft <- fe_terms(m)
      any(grepl("park_month_year", ft))
    }
    
    has_home_AQI <- function(m){
      txt <- fml_txt(m)
      grepl("\\bworst_aqi_cat_home", txt)
    }
    
    has_travel_cost <- function(m) {
      txt <- fml_txt(m)
      grepl("\\btravel_cost_100s\\b", txt)
    }
    
    has_travel_cost_x_park <- function(m) {
      txt <- fml_txt(m)
      grepl("travel_cost_100s\\s*:\\s*as\\.factor\\(park\\)", txt) ||
        grepl("as\\.factor\\(park\\)\\s*:\\s*travel_cost_100s", txt)
    }
    
    has_substitute_parks <- function(m) {
      txt <- fml_txt(m)
      grepl("\\bbest_mean_pm25_subst_parks_cbg\\b", txt) ||
        grepl("\\bbest_worst_aqi_subst_parks_cbg\\b", txt)
    }
    
    list(
      # ---- NEW rows (added) ----
      "Travel cost" =
        sapply(models, \(m) if (has_travel_cost(m)) check_symbol else ""),
      
      "Travel cost $\\times$ Park" =
        sapply(models, \(m) if (has_travel_cost_x_park(m)) check_symbol else ""),
      
      "AQI substitute park(s)" =
        sapply(models, \(m) if (has_substitute_parks(m)) check_symbol else ""),
      
      "Home CBG AQI" =
        sapply(models, \(m) if (has_home_AQI(m)) check_symbol else ""),
      
      # ---- ORIGINAL rows (unchanged) ----
      "Weather controls" =
        sapply(models, \(m) if (has_any(m, weather_vars)) check_symbol else ""),
      
      "Fire controls" =
        sapply(models, \(m) if (has_any(m, fire_vars)) check_symbol else ""),
      
      "Demographics" =
        sapply(models, \(m) if (has_any(m, demo_vars)) check_symbol else ""),
      
      "Holiday" =
        sapply(models, \(m) if (has_holiday(m)) check_symbol else ""),
      
      "Population offset" =
        sapply(models, \(m) if (has_offset_pop(m)) check_symbol else ""),
      
      "Park $\\times$ Month $\\times$ Year" =
        sapply(models, \(m) if (has_pmy(m)) check_symbol else "")
    )
  }
  
  #make alternative keep vector
  keep_tc_alt <- c(
    # PM2.5 + AQI
    "%^worst_aqi_cat_weekModerate$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups$",
    "%^worst_aqi_cat_weekUnhealthy$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous$",
    
    # Interactions: worst AQI × travel cost
    "%^worst_aqi_cat_weekModerate:travel_cost_100s$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:travel_cost_100s$",
    "%^worst_aqi_cat_weekUnhealthy:travel_cost_100s$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:travel_cost_100s$",
    
    # Interactions: worst AQI × age
    "%^worst_aqi_cat_weekModerate:median_ageE$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:median_ageE$",
    "%^worst_aqi_cat_weekUnhealthy:median_ageE$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:median_ageE$",
    
    # Interactions: worst AQI × income
    "%^worst_aqi_cat_weekModerate:median_incomeE1000s$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:median_incomeE1000s$",
    "%^worst_aqi_cat_weekUnhealthy:median_incomeE1000s$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:median_incomeE1000s$"
      )
  
  #create table
  models_tc_het <- list(reg_lag_worst_het, reg_lag_worst_het_w_home)
  
  make_simple_etable_tc(
    models   = models_tc_het,
    outfile  = "./output/tables/tc_heterogeneity_results.tex",
    title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks testing for heterogeneous responses to smoke by travel cost",
    keep     = keep_tc_alt,
    dict     = dict_tc,
    order    = order_tc,  
    extralines_fun = make_extralines_tc_min,
    scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
  )
  
  rm(models_tc_het, reg_lag_worst_het, reg_lag_worst_het_w_home)
  gc()
  

  ## run with age-based for appendix
  reg_lag_worst_het_age = fenegbin(
    Visits ~ worst_aqi_cat_week +
      worst_aqi_cat_week:median_ageE + best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) + 
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het_age)
  
  ##run with income-based for appendix
  
  #scale income
  data <- data %>%
    mutate(median_incomeE1000s = median_incomeE/1000)
  
  reg_lag_worst_het_inc = fenegbin(
    Visits ~ worst_aqi_cat_week +
      worst_aqi_cat_week:median_incomeE1000s + best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) + 
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE1000s + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het_inc)
  
 #create table
  make_simple_etable_tc(
    models = list(reg_lag_worst_het_age, reg_lag_worst_het_inc),
    outfile  = "./output/tables/age_inc_heterogeneity_results.tex",
    title    = "Negative binomial travel cost model with heteregeneous responses to smoke based on age and income",
    keep     = keep_tc_alt,
    dict     = dict_tc,
    order    = order_tc,  
    extralines_fun = make_extralines_tc_min,
    scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
  )
}  

#### run decision timing heterogeneity regressions
{
  #create lag dummies
  data <- data %>%
    mutate(
      # define "unhealthy or worse" at the current week
      aqi_unh_or_worse = as.integer(worst_aqi_cat_week %in% c("Unhealthy", "Very Unhealthy or Hazardous"))
    ) %>%
    group_by(park, census_cbg) %>%
    arrange(date_range_start, .by_group = TRUE) %>% 
    # create lagged AQI category (as the factor itself)
    mutate(
      worst_aqi_cat_week_lag1 = lag(worst_aqi_cat_week, 1),
      worst_aqi_cat_week_lag2 = lag(worst_aqi_cat_week, 2),
      worst_aqi_cat_week_lag3 = lag(worst_aqi_cat_week, 3),
      worst_aqi_cat_week_lag4 = lag(worst_aqi_cat_week, 4),
      worst_aqi_cat_week_lag5 = lag(worst_aqi_cat_week, 5),
      worst_aqi_cat_week_lag6 = lag(worst_aqi_cat_week, 6),
      worst_aqi_cat_week_lag7 = lag(worst_aqi_cat_week, 7),
      worst_aqi_cat_week_lag8 = lag(worst_aqi_cat_week, 8)
    ) %>%
    # create lagged unhealthy-or-worse dummy
    mutate(
      aqi_unh_or_worse_lag1 = lag(aqi_unh_or_worse, 1),
      aqi_unh_or_worse_lag2 = lag(aqi_unh_or_worse, 2),
      aqi_unh_or_worse_lag3 = lag(aqi_unh_or_worse, 3),
      aqi_unh_or_worse_lag4 = lag(aqi_unh_or_worse, 4),
      aqi_unh_or_worse_lag5 = lag(aqi_unh_or_worse, 5),
      aqi_unh_or_worse_lag6 = lag(aqi_unh_or_worse, 6),
      aqi_unh_or_worse_lag7 = lag(aqi_unh_or_worse, 7),
      aqi_unh_or_worse_lag8 = lag(aqi_unh_or_worse, 8),
    )
  
  #run regression
  reg_lag_worst_het_dt_4_simp = fenegbin(
    Visits ~ aqi_unh_or_worse + aqi_unh_or_worse:travel_cost_100s + 
      aqi_unh_or_worse_lag1 + aqi_unh_or_worse_lag1:travel_cost_100s +
      aqi_unh_or_worse_lag2 +  aqi_unh_or_worse_lag2:travel_cost_100s +
      aqi_unh_or_worse_lag3 + aqi_unh_or_worse_lag3:travel_cost_100s +
      aqi_unh_or_worse_lag4 + aqi_unh_or_worse_lag4:travel_cost_100s +
      best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het_dt_4_simp)
  
  reg_lag_worst_het_dt_4 = fenegbin(
    Visits ~ worst_aqi_cat_week + worst_aqi_cat_week:travel_cost_100s + 
      aqi_unh_or_worse_lag1 + aqi_unh_or_worse_lag1:travel_cost_100s +
      aqi_unh_or_worse_lag2 +  aqi_unh_or_worse_lag2:travel_cost_100s +
      aqi_unh_or_worse_lag3 + aqi_unh_or_worse_lag3:travel_cost_100s +
      aqi_unh_or_worse_lag4 + aqi_unh_or_worse_lag4:travel_cost_100s +
      best_worst_aqi_subst_parks_cbg +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week + fire_within_100km_park +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data
  )
  summary(reg_lag_worst_het_dt_4)
 
#create table for result
  #make alternative keep vector
  keep_tc_alt_lag <- c(
    # PM2.5 + AQI
    "%^worst_aqi_cat_weekModerate$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups$",
    "%^worst_aqi_cat_weekUnhealthy$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous$",
    
    "%^aqi_unh_or_worse$",
    
    # Interactions: worst AQI × travel cost
    "%^worst_aqi_cat_weekModerate:travel_cost_100s$",
    "%^worst_aqi_cat_weekUnhealthyforSensitiveGroups:travel_cost_100s$",
    "%^worst_aqi_cat_weekUnhealthy:travel_cost_100s$",
    "%^worst_aqi_cat_weekVeryUnhealthyorHazardous:travel_cost_100s$",
    
  # lag coefficients
  "%aqi_unh_or_worse_lag1$",
  "%aqi_unh_or_worse_lag2$",
  "%aqi_unh_or_worse_lag3$",
  "%aqi_unh_or_worse_lag4$"
  )
  
  #create table
  models_tc_het_lag <- list(reg_lag_worst_het_dt_4_simp, reg_lag_worst_het_dt_4)
  
  make_simple_etable_tc(
    models   = models_tc_het_lag,
    outfile  = "./output/tables/tc_heterogeneity_results_tc_lagged.tex",
    title    = "Negative binomial travel cost model with AQI × travel cost heterogeneity in responses to smoke preceding a trip",
    keep     = keep_tc_alt_lag,
    dict     = dict_tc,
    order    = order_tc,  
    extralines_fun = make_extralines_tc_min,
    scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
  )
  
}

#### regression with log(pop) as covariate instead of offset
{
    #linear pm2.5
    reg_1_sub = fenegbin(
      Visits ~ mean_weekly_pm25 + fire_within_100km_park + best_mean_pm25_subst_parks_cbg +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + log(total_popE)|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_1_sub_summary <- summary(reg_1_sub)
    reg_1_sub_summary
    saveRDS(reg_1_sub_summary, "./output/tc_reg_results/reg_linear_tc_400miles_sub_no_offset.RDS")
    rm(reg_1_sub, reg_1_sub_summary)
    gc()
    
    
    #simple with worst aqi cat in wee
    reg_2_sub = fenegbin(
      Visits ~ worst_aqi_cat_week + fire_within_100km_park + best_worst_aqi_subst_parks_cbg +
        travel_cost_100s + travel_cost_100s:as.factor(park) +
        mean_weekly_temp + total_weekly_prcp + holiday_week +
        median_ageE + median_incomeE + prop_higher_ed + prop_white + log(total_popE)|
        as.factor(park_month_year),
      cluster = ~ park,
      data = data
    )
    reg_2_sub_summary <- summary(reg_2_sub)
    reg_2_sub_summary
    saveRDS(reg_2_sub_summary, "./output/tc_reg_results/reg_worst_tc_400miles_sub_no_offset.RDS")
    rm(reg_2_sub, reg_2_sub_summary)
    gc()
    
    ## produce regression table (have to reload because of ram limitations)
    reg_1_sub_summary <- readRDS("./output/tc_reg_results/reg_linear_tc_400miles_sub_no_offset.RDS")
    reg_2_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub_no_offset.RDS")
    
    #collect models
    models_tc <- list(reg_2_sub_summary, reg_1_sub_summary)  
    
    # add interaction dict using the first model
    dict_tc_full <- c(dict_tc, add_tc_interaction_dict(models_tc[[1]]))
    
    #make table
    make_simple_etable_tc(
      models   = models_tc,
      outfile  = "./output/tables/main_tc_regression_results_sub_no_offset.tex",
      title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks, using a 400 mile cutoff, the natural log of CBG population as a covariate, and inclusive of alternative park substitution effects",
      keep     = keep_tc,
      dict     = dict_tc_full,
      order    = order_tc,
      extralines_fun = make_extralines_tc,
      scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
    )
    
    
    #clear data to save RAM
    rm(reg_1_sub_summary, reg_2_sub_summary)
    gc()
    Sys.sleep(30)
  }  
  
#### regressions for alternative 250 mile cuttoff
{
  
  #filter data to 250 miles
  data_filtered_subset <- data %>% 
    filter(travel_distance_m <= 402336)
  gc()
  
  #linear pm2.5 (without substitution otherwise wont estimate)
  reg_1_sub = fenegbin(
    Visits ~ mean_weekly_pm25 + fire_within_100km_park +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data_filtered_subset
    )
  reg_1_sub_summary <- summary(reg_1_sub)
  reg_1_sub_summary
  saveRDS(reg_1_sub_summary, "./output/tc_reg_results/reg_linear_tc_250miles_no_sub.RDS")
  rm(reg_1_sub, reg_1_sub_summary)
  gc()
  
  
  #simple with worst aqi cat in wee
  reg_2_sub = fenegbin(
    Visits ~ worst_aqi_cat_week + fire_within_100km_park +
      travel_cost_100s + travel_cost_100s:as.factor(park) +
      mean_weekly_temp + total_weekly_prcp + holiday_week +
      median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
      as.factor(park_month_year),
    cluster = ~ park,
    data = data_filtered_subset
  )
  reg_2_sub_summary <- summary(reg_2_sub)
  reg_2_sub_summary
  saveRDS(reg_2_sub_summary, "./output/tc_reg_results/reg_worst_tc_250miles_no_sub.RDS")
  rm(reg_2_sub, reg_2_sub_summary)
  gc()
  
  ## produce regression table (have to reload because of ram limitations)
  reg_1_sub_summary <- readRDS("./output/tc_reg_results/reg_linear_tc_250miles_no_sub.RDS")
  reg_2_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_250miles_no_sub.RDS")
  
  #collect models
  models_tc <- list(reg_2_sub_summary, reg_1_sub_summary)  
  
  # add interaction dict using the first model
  dict_tc_full <- c(dict_tc, add_tc_interaction_dict(models_tc[[1]]))
  
  #make table
  make_simple_etable_tc(
    models   = models_tc,
    outfile  = "./output/tables/main_tc_regression_results_sub_250miles.tex",
    title    = "Negative binomial travel cost model of the effect of smoke on visitation at national parks, using a 250 mile cutoff, a CBG population offset, and without inclusion of alternative park substitution effects",
    keep     = keep_tc,
    dict     = dict_tc_full,
    order    = order_tc,
    extralines_fun = make_extralines_tc,
    scale    = "\\tcscale"      # <- LaTeX: \\newcommand{\\tcscale}{0.90}
  )
  
  
  #clear data to save RAM
  rm(reg_1_sub_summary, reg_2_sub_summary)
  gc()
  Sys.sleep(30)
}

#### regressions for alternative 250 mile cuttoff (with substitution, dropping Acadia NP)
{
#pre-clean all observations to drop any that are all NA's (mainly substitute park visits for CBG's visiting Acadia)
  
vars_needed <- c(
  "Visits","mean_weekly_pm25","fire_within_100km_park","best_mean_pm25_subst_parks_cbg",
  "travel_cost_100s","park","mean_weekly_temp","total_weekly_prcp","holiday_week",
  "median_ageE","median_incomeE","prop_higher_ed","prop_white",
  "total_popE","park_month_year", "best_worst_aqi_subst_parks_cbg", "worst_aqi_cat_week"
)

data_filtered_subset_clean <- data_filtered_subset %>%
  ungroup() %>%
  dplyr::select(all_of(vars_needed)) %>%
  filter(complete.cases(.)) %>%                # drop RHS NAs upfront
  group_by(park_month_year) %>%
  filter(any(Visits > 0)) %>%                  # drop “all-zero outcome” FE groups upfront
  ungroup()

#linear pm2.5 (without substitution otherwise wont estimate)
reg_1_sub = fenegbin(
  Visits ~ mean_weekly_pm25 + fire_within_100km_park + best_mean_pm25_subst_parks_cbg +
    travel_cost_100s + travel_cost_100s:as.factor(park) +
    mean_weekly_temp + total_weekly_prcp + holiday_week +
    median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
    as.factor(park_month_year),
  cluster = ~ park,
  data = data_filtered_subset_clean
)
reg_1_sub_summary <- summary(reg_1_sub)
rm(reg_1_sub,reg_1_sub_summary)

#simple with worst aqi cat in week
reg_2_sub = fenegbin(
  Visits ~ worst_aqi_cat_week + fire_within_100km_park + best_worst_aqi_subst_parks_cbg +
    mean_weekly_temp + total_weekly_prcp + holiday_week +
    median_ageE + median_incomeE + prop_higher_ed + prop_white + offset(log(total_popE))|
    as.factor(park_month_year),
  cluster = ~ park,
  data = data_filtered_subset_clean
)
reg_2_sub_summary <- summary(reg_2_sub)
rm(reg_2_sub, reg_2_sub_summary)
gc()
}

#### calculate aggregate cs parameters to use in welfare losses script - MAIN SPECIFICATION
{
  
  extract_travel_cost <- function(model, data, label_suffix = "") {
    
    # significance threshold
    alpha <- 0.05
    
    # coefficients and VCV
    b <- coef(model)
    V <- vcov(model) 
    
    # drop theta / any non-coef params
    V <- V[names(b), names(b), drop = FALSE]
    
    # main effect
    beta_main <- b["travel_cost_100s"]
    Var_main  <- V["travel_cost_100s", "travel_cost_100s"]
    
    # find interaction terms
    interaction_terms <- grep("^travel_cost_100s:as.factor\\(park\\)", 
                              names(b), value = TRUE)
    
    # compute p-values
    z_values <- b / sqrt(diag(V))
    p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
    
    # detect parks in data and baseline
    all_parks <- unique(data$park)
    interact_parks <- trimws(sub("travel_cost_100s:as.factor\\(park\\)", "", interaction_terms))
    baseline_park <- setdiff(all_parks, interact_parks)
    baseline_park <- as.character(baseline_park)
    
    # storage
    results <- list()
    
    # baseline park row
    results[[baseline_park]] <- data.frame(
      park  = baseline_park,
      trend = beta_main,
      se    = sqrt(Var_main),
      included_interaction = FALSE
    )
    
    # loop through interaction parks
    for(term in interaction_terms) {
      
      park_name <- trimws(sub("travel_cost_100s:as.factor\\(park\\)", "", term))
      
      # significance
      p_val <- p_values[term]
      sig   <- p_val < alpha
      
      if(sig) {
        beta_int <- b[term]
        Var_int  <- V[term, term]
        Covar    <- V["travel_cost_100s", term]
        
        trend <- beta_main + beta_int
        se    <- sqrt(Var_main + Var_int + 2 * Covar)
        use_int <- TRUE
        
      } else {
        # insignificant → revert to baseline slope
        trend <- beta_main
        se    <- sqrt(Var_main)
        use_int <- FALSE
      }
      
      results[[park_name]] <- data.frame(
        park  = park_name,
        trend = trend,
        se    = se,
        included_interaction = use_int
      )
    }
    
    # bind all results
    out <- bind_rows(results) %>%
      mutate(
        CS = 100 / -trend
      )
    
    # add suffix label 
    if(label_suffix != "") {
      names(out)[names(out) == "trend"] <- paste0("trend", label_suffix)
      names(out)[names(out) == "se"]    <- paste0("se", label_suffix)
      names(out)[names(out) == "CS"]    <- paste0("CS", label_suffix)
      names(out)[names(out) == "included_interaction"] <- 
        paste0("included_interaction", label_suffix)
    }
    
    return(out)
  }
  
  # run for main model 400 miles
  main_model <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub.RDS")
  model_1_results <- extract_travel_cost(
    model = main_model,
    data  = data,
    label_suffix = ""     
  )
  
  # run for robust 250 mile model
  model_250 <- readRDS("./output/tc_reg_results/reg_worst_tc_250miles_no_sub.RDS")
  model_250_results <- extract_travel_cost(
    model = model_250,
    data  = data_filtered_subset,
    label_suffix = "_250"
  )
  
  #run for model without substitution effects
  model_no_sub <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_no_sub.RDS")
  model_no_sub_results <- extract_travel_cost(
    model = model_no_sub,
    data  = data,
    label_suffix = "_no_sub"
  )
  
  #combine
  travel_cost_trends_combined <- model_1_results %>%
    left_join(model_250_results, by = "park") %>%
    left_join(model_no_sub_results, by = "park")
  
  #save
  saveRDS(travel_cost_trends_combined, "./data/processed_data/cs_estimates.RDS")
}

#### create joint table for robustness checks
{
  #main model without substitution effects
  reg_2_no_sub_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_no_sub.RDS")
  
  #main model with home AQI
  reg_2_home_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub_home.RDS")
  
  #main model without offset m
  reg_2_no_offset_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_400miles_sub_no_offset.RDS")
  
  #main with 250 mile cuttoff
  reg_2_250_summary <- readRDS("./output/tc_reg_results/reg_worst_tc_250miles_no_sub.RDS")
  
  #collect all models
  models_robust_comb <- list(reg_2_250_summary, reg_2_no_sub_summary, reg_2_home_summary, reg_2_no_offset_summary)
  
  # add interaction dict using the first model
  dict_tc_robust <- c(dict_tc, add_tc_interaction_dict(models_robust_comb[[1]]))
  
  #make table
  make_simple_etable_tc(
    models   = models_robust_comb,
    outfile  = "./output/tables/main_tc_regression_results_robust_combined.tex",
    title    = "Robustness checks on travel cost modeling of the effect of smoke on visitation at national parks, using an alternative 250 mile cutoff,
    omitting park substitution effects, controlling for home AQI, and omitting the CBG population offset",
    keep     = keep_tc,
    dict     = dict_tc_robust,
    order    = order_tc,
    extralines_fun = make_extralines_tc,
    scale    = "\\tcscale"      
  )
  
}


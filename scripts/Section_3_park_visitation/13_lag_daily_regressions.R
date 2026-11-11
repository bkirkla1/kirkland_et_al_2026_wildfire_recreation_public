#-------------------------------------------------------------------------------
#
# '13_lag_daily_regressions.R': This script runs regressions for lagged daily
# regressions of the effect of smoke-attributable pm2.5 on park visitation
#
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#load data
main_data_subset <- readRDS('./data/processed_data/main_data_subset_final.RDS')

#define function to help with fixed effect line printouts
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

#### REGRESSIONS
#negbin regression with binned mean aqi effects
reg_aqi_no_fe <- fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                                    aqi_cat_lag1_dum + aqi_cat_lag2_dum + aqi_cat_lag3_dum + 
                                    aqi_cat_lag4_dum + aqi_cat_lag5_dum + aqi_cat_lag6_dum + 
                                    aqi_cat_lag7_dum + aqi_cat_lag8_dum + aqi_cat_lag9_dum + 
                                    aqi_cat_lag10_dum + aqi_cat_lag11_dum + aqi_cat_lag12_dum + 
                                    aqi_cat_lag13_dum + aqi_cat_lag14_dum + aqi_cat_lag15_dum + 
                                    aqi_cat_lag16_dum + aqi_cat_lag17_dum + aqi_cat_lag18_dum + 
                                    aqi_cat_lag19_dum + aqi_cat_lag20_dum + aqi_cat_lag21_dum + 
                                    fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                    tmmx + vs + pr  | holiday_dum + as.factor(wind_cat),
                                  data = main_data_subset,
                                  cluster = ~ park)
summary(reg_aqi_no_fe)


reg_aqi_no_pm_fe <-  fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                                        aqi_cat_lag1_dum + aqi_cat_lag2_dum + aqi_cat_lag3_dum + 
                                        aqi_cat_lag4_dum + aqi_cat_lag5_dum + aqi_cat_lag6_dum + 
                                        aqi_cat_lag7_dum + aqi_cat_lag8_dum + aqi_cat_lag9_dum + 
                                        aqi_cat_lag10_dum + aqi_cat_lag11_dum + aqi_cat_lag12_dum + 
                                        aqi_cat_lag13_dum + aqi_cat_lag14_dum + aqi_cat_lag15_dum + 
                                        aqi_cat_lag16_dum + aqi_cat_lag17_dum + aqi_cat_lag18_dum + 
                                        aqi_cat_lag19_dum + aqi_cat_lag20_dum + aqi_cat_lag21_dum + 
                                        fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                        tmmx + vs + pr  |holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                        as.factor(park) + as.factor(month) + as.factor(year),
                                      data = main_data_subset,
                                      cluster = ~ park)
summary(reg_aqi_no_pm_fe)

reg_aqi_pm_fe <-  fixest::fenegbin(Visits ~ as.factor(aqi_cat) + 
                                     aqi_cat_lag1_dum + aqi_cat_lag2_dum + aqi_cat_lag3_dum + 
                                     aqi_cat_lag4_dum + aqi_cat_lag5_dum + aqi_cat_lag6_dum + 
                                     aqi_cat_lag7_dum + aqi_cat_lag8_dum + aqi_cat_lag9_dum + 
                                     aqi_cat_lag10_dum + aqi_cat_lag11_dum + aqi_cat_lag12_dum + 
                                     aqi_cat_lag13_dum + aqi_cat_lag14_dum + aqi_cat_lag15_dum + 
                                     aqi_cat_lag16_dum + aqi_cat_lag17_dum + aqi_cat_lag18_dum + 
                                     aqi_cat_lag19_dum + aqi_cat_lag20_dum + aqi_cat_lag21_dum + 
                                     fire_dist_km_bound + I(fire_dist_km_bound^2) + 
                                     tmmx + vs + pr | holiday_dum + as.factor(wind_cat)+ as.factor(day_of_week)+
                                     as.factor(park) + as.factor(year) + as.factor(month)^as.factor(park),
                                   data = main_data_subset,
                                   cluster = ~ park)
summary(reg_aqi_pm_fe)

reg_aqi_main <- fenegbin(Visits ~  as.factor(aqi_cat) + 
                           aqi_cat_lag1_dum + aqi_cat_lag2_dum + aqi_cat_lag3_dum + 
                           aqi_cat_lag4_dum + aqi_cat_lag5_dum + aqi_cat_lag6_dum + 
                           aqi_cat_lag7_dum + aqi_cat_lag8_dum + aqi_cat_lag9_dum + 
                           aqi_cat_lag10_dum + aqi_cat_lag11_dum + aqi_cat_lag12_dum + 
                           aqi_cat_lag13_dum + aqi_cat_lag14_dum + aqi_cat_lag15_dum + 
                           aqi_cat_lag16_dum + aqi_cat_lag17_dum + aqi_cat_lag18_dum + 
                           aqi_cat_lag19_dum + aqi_cat_lag20_dum + aqi_cat_lag21_dum + 
                           fire_dist_km_bound + I(fire_dist_km_bound^2) +
                           tmmx + vs + pr | holiday_dum + as.factor(wind_cat) + 
                           as.factor(day_of_week) +
                           as.factor(park)^as.factor(month)^as.factor(year),
                         data = main_data_subset,
                         cluster = ~ park)

summary(reg_aqi_main)

#print results

#define variable names
dict_output_aqi <- c(
  "as.factor(aqi_cat)Moderate"   = "AQI: Moderate",
  "as.factor(aqi_cat)UnhealthyforSensitiveGroups" = "AQI: Unhealthy for Sensitive Groups",
  "as.factor(aqi_cat)Unhealthy"  = "AQI: Unhealthy",
  "as.factor(aqi_cat)VeryUnhealthyorHazardous" = "AQI:Very Unhealthy or Hazardous",

  # AQI lag dummies
  "aqi_cat_lag1_dum"  = "Smoke Lag t-1",
  "aqi_cat_lag2_dum"  = "Smoke Lag t-2",
  "aqi_cat_lag3_dum"  = "Smoke Lag t-3",
  "aqi_cat_lag4_dum"  = "Smoke Lag t-4",
  "aqi_cat_lag5_dum"  = "Smoke Lag t-5",
  "aqi_cat_lag6_dum"  = "Smoke Lag t-6",
  "aqi_cat_lag7_dum"  = "Smoke Lag t-7",
  "aqi_cat_lag8_dum"  = "Smoke Lag t-8",
  "aqi_cat_lag9_dum"  = "Smoke Lag t-9",
  "aqi_cat_lag10_dum" = "Smoke Lag t-10",
  "aqi_cat_lag11_dum" = "Smoke Lag t-11",
  "aqi_cat_lag12_dum" = "Smoke Lag t-12",
  "aqi_cat_lag13_dum" = "Smoke Lag t-13",
  "aqi_cat_lag14_dum" = "Smoke Lag t-14",
  "aqi_cat_lag15_dum" = "Smoke Lag t-15",
  "aqi_cat_lag16_dum" = "Smoke Lag t-16",
  "aqi_cat_lag17_dum" = "Smoke Lag t-17",
  "aqi_cat_lag18_dum" = "Smoke Lag t-18",
  "aqi_cat_lag19_dum" = "Smoke Lag t-19",
  "aqi_cat_lag20_dum" = "Smoke Lag t-20",
  "aqi_cat_lag21_dum" = "Smoke Lag t-21",
  
  "fire_dist_km_bound" = "Fire distance",
  "I(fire_dist_km_bound^2)" = "Fire distance squared",
  "tmmx"                         = "Max temp.",
  "vs"                           = "Mean wind speed",
  "pr"                           = "Cumulative precipitation",
  "holiday_dum" = "Holiday",
  "as.factor(wind_cat)" = "Wind direction", 
  "as.factor(park)" = "Park",
  "as.factor(month)" = "Month",
  "as.factor(year)" = "Year",
  "as.factor(park)-as.factor(month)" = "Park:month",
  "as.factor(day_of_week)" = "Day of week"
)

#define output path
outfile_aqi <- "./output/tables/lagged_daily_regression_table.tex"
if (file.exists(outfile_aqi)) file.remove(outfile_aqi)

#define model list
models <- list(
  reg_aqi_no_fe,
  reg_aqi_no_pm_fe,
  reg_aqi_pm_fe,
  reg_aqi_main
)

#make table
fixest::etable(
  models,
  
  tex = TRUE,
  file = outfile_aqi,
  title = "Negative binomial regression of the effect of daily smoke AQI on visitation at national parks",
  
  # AQI coefficients only (IMPORTANT % prefix)
  keep = "%aqi",
  dict = dict_output_aqi,
  
  # Only show N
  fitstat = "n",
  
  # Remove default FE section
  drop.section = "fixef",
  
  # Custom FE / controls panel
  extralines = make_extralines(models),
  
  digits = "r4",
  digits.stats = "r4",
  fixef_sizes = FALSE
)

#add horizontal line to table
tex <- readLines(outfile_aqi)

# Find the first extraline row label (we use "Weather controls" as the anchor)
idx <- grep("^\\s*Weather controls\\s*&", tex)

if (length(idx) > 0) {
  # Insert \midrule on the line *before* the first extraline row
  tex <- append(tex, "\\midrule", after = idx[1] - 1)
} else {
  warning("Could not find 'Weather controls' row in LaTeX output; midrule not inserted.")
}

writeLines(tex, outfile_aqi)

#### WEEKLY LAG REGRESSIONS


## construct dummies
main_data_subset <- main_data_subset %>%
  mutate(
    smoke_lag_wk1 = as.integer(rowSums(
      dplyr::select(., aqi_cat_lag1_dum:aqi_cat_lag7_dum)) > 0),
    smoke_lag_wk2 = as.integer(rowSums(
      dplyr::select(., aqi_cat_lag8_dum:aqi_cat_lag14_dum)) > 0),
    smoke_lag_wk3 = as.integer(rowSums(
      dplyr::select(., aqi_cat_lag15_dum:aqi_cat_lag21_dum)) > 0),
    smoke_lag_wk4 = as.integer(rowSums(
      dplyr::select(., aqi_cat_lag22_dum:aqi_cat_lag28_dum)) > 0)
  )


## regressions
reg_wk_no_fe <- fixest::fenegbin(
  Visits ~ as.factor(aqi_cat) +
    smoke_lag_wk1 + smoke_lag_wk2 + smoke_lag_wk3 + smoke_lag_wk4 +
    fire_dist_km_bound + I(fire_dist_km_bound^2) +
    tmmx + vs + pr | holiday_dum + as.factor(wind_cat),
  data    = main_data_subset,
  cluster = ~ park
)
summary(reg_wk_no_fe)

reg_wk_no_pm_fe <- fixest::fenegbin(
  Visits ~ as.factor(aqi_cat) +
    smoke_lag_wk1 + smoke_lag_wk2 + smoke_lag_wk3 + smoke_lag_wk4 +
    fire_dist_km_bound + I(fire_dist_km_bound^2) +
    tmmx + vs + pr | holiday_dum + as.factor(wind_cat) +
    as.factor(day_of_week) +
    as.factor(park) + as.factor(month) + as.factor(year),
  data    = main_data_subset,
  cluster = ~ park
)
summary(reg_wk_no_pm_fe)

reg_wk_pm_fe <- fixest::fenegbin(
  Visits ~ as.factor(aqi_cat) +
    smoke_lag_wk1 + smoke_lag_wk2 + smoke_lag_wk3 + smoke_lag_wk4 +
    fire_dist_km_bound + I(fire_dist_km_bound^2) +
    tmmx + vs + pr | holiday_dum + as.factor(wind_cat) +
    as.factor(day_of_week) +
    as.factor(park) + as.factor(year) +
    as.factor(month)^as.factor(park),
  data    = main_data_subset,
  cluster = ~ park
)
summary(reg_wk_pm_fe)

reg_wk_main <- fixest::fenegbin(
  Visits ~ as.factor(aqi_cat) +
    smoke_lag_wk1 + smoke_lag_wk2 + smoke_lag_wk3 + smoke_lag_wk4 +
    fire_dist_km_bound + I(fire_dist_km_bound^2) +
    tmmx + vs + pr | holiday_dum + as.factor(wind_cat) +
    as.factor(day_of_week) +
    as.factor(park)^as.factor(month)^as.factor(year),
  data    = main_data_subset,
  cluster = ~ park
)
summary(reg_wk_main)

## create table

dict_output_wk <- c(
  "as.factor(aqi_cat)Moderate"                     = "AQI: Moderate",
  "as.factor(aqi_cat)UnhealthyforSensitiveGroups"  = "AQI: Unhealthy for Sensitive Groups",
  "as.factor(aqi_cat)Unhealthy"                    = "AQI: Unhealthy",
  "as.factor(aqi_cat)VeryUnhealthyorHazardous"     = "AQI: Very Unhealthy or Hazardous",
  "smoke_lag_wk1"                                  = "Smoke Lag: Days 1--7",
  "smoke_lag_wk2"                                  = "Smoke Lag: Days 8--14",
  "smoke_lag_wk3"                                  = "Smoke Lag: Days 15--21",
  "smoke_lag_wk4"                                  = "Smoke Lag: Days 22--28",
  "fire_dist_km_bound"                             = "Fire distance",
  "I(fire_dist_km_bound^2)"                        = "Fire distance squared",
  "tmmx"                                           = "Max temp.",
  "vs"                                             = "Mean wind speed",
  "pr"                                             = "Cumulative precipitation",
  "holiday_dum"                                    = "Holiday",
  "as.factor(wind_cat)"                            = "Wind direction",
  "as.factor(park)"                                = "Park",
  "as.factor(month)"                               = "Month",
  "as.factor(year)"                                = "Year",
  "as.factor(park)-as.factor(month)"               = "Park × Month",
  "as.factor(day_of_week)"                         = "Day of week"
)

models_wk <- list(
  reg_wk_no_fe,
  reg_wk_no_pm_fe,
  reg_wk_pm_fe,
  reg_wk_main
)

outfile_wk <- "./output/tables/lagged_weekly_regression_table.tex"
if (file.exists(outfile_wk)) file.remove(outfile_wk)

fixest::etable(
  models_wk,
  tex           = TRUE,
  file          = outfile_wk,
  title         = "Negative binomial regression of the effect of weekly smoke AQI on visitation at national parks",
  keep          = "%aqi|smoke_lag_wk",   # keep today's AQI cats + weekly lags
  dict          = dict_output_wk,
  fitstat       = "n",
  drop.section  = "fixef",
  extralines    = make_extralines(models_wk),  # reuses existing function
  digits        = "r4",
  digits.stats  = "r4",
  fixef_sizes   = FALSE
)

# Insert midrule above the controls panel
tex_wk <- readLines(outfile_wk)
idx_wk  <- grep("^\\s*Weather controls\\s*&", tex_wk)
if (length(idx_wk) > 0) {
  tex_wk <- append(tex_wk, "\\midrule", after = idx_wk[1] - 1)
} else {
  warning("Could not find 'Weather controls' row; midrule not inserted.")
}
writeLines(tex_wk, outfile_wk)


## whisker plots

coef_df_wk <- broom::tidy(reg_wk_main, conf.int = TRUE)

## coefficients plot

today_coefs_wk_log <- coef_df_wk %>%
  filter(grepl("^as.factor\\(aqi_cat\\)", term)) %>%
  mutate(
    aqi_cat = gsub("as.factor\\(aqi_cat\\)", "", term),
    aqi_cat = factor(aqi_cat, levels = c(
      "Good",
      "Moderate",
      "Unhealthy for Sensitive Groups",
      "Unhealthy",
      "Very Unhealthy or Hazardous"
    ))
  )

wk_coefs_log <- coef_df_wk %>%
  filter(grepl("^smoke_lag_wk\\d$", term)) %>%
  mutate(
    week_label = recode(term,
                        smoke_lag_wk1 = "Days 1-7",
                        smoke_lag_wk2 = "Days 8-14",
                        smoke_lag_wk3 = "Days 15-21",
                        smoke_lag_wk4 = "Days 22-28"
    ),
    week_label = factor(week_label,
                        levels = c("Days 1-7", "Days 8-14", "Days 15-21", "Days 22-28"))
  )

#load new fots

#create whisker plots
plot_df_wk <- bind_rows(
  today_coefs_wk_log %>%
    transmute(label = as.character(aqi_cat), estimate, conf.low, conf.high,
              panel = "A. Effect of AQI day t relative to Good AQI"),
  wk_coefs_log %>%
    transmute(label = as.character(week_label), estimate, conf.low, conf.high,
              panel = "B. Effect of smoke preceding a trip")
) %>%
  mutate(
    # reversed so the first category appears at the top of each panel
    label = factor(label, levels = rev(c(levels(today_coefs_wk_log$aqi_cat),
                                         levels(wk_coefs_log$week_label)))),
    panel = factor(panel, levels = unique(panel))
  )

p_wk <- ggplot(plot_df_wk, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y",
                width = 0.2, linewidth = 0.6, color = "black") +
  geom_point(size = 2.5, color = "black") +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_y_discrete(expand = expansion(add = 0.4)) +   # less padding above/below rows
  labs(x = "Estimate", y = NULL) +
  theme_minimal(base_size = 11, base_family = "Times New Roman") +
  theme(
    text       = element_text(color = "black"),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black", hjust = 0, face = "bold")
  )
p_wk

ggsave(
  filename = "./output/figures/lagged_weekly_effects_plot_logcoeff.pdf",
  plot     = p_wk,
  height   = 4,
  width    = 6.5,
  units = "in",
  device   = cairo_pdf  
)

#### create a combined plot of daily and weekly lagged regressions for the appendix

#define models
models_comb <- list(reg_wk_main, reg_aqi_main)

#define outfile
outfile_comb <-  "./output/tables/lagged_regression_combined_table.tex"
# remove existing file if present
if (file.exists(outfile_comb)) file.remove(outfile_comb)

#call table
fixest::etable(
  models_comb,
  tex           = TRUE,
  file          = outfile_comb,
  title         = "Negative binomial regression of the effect of lagged weekly and daily smoke AQI on visitation at national parks",
  keep          = "%aqi|smoke_lag_wk|aqi_cat_lag",   # keep today's AQI cats + weekly lags
  dict          = c(dict_output_wk, dict_output_aqi),
  fitstat       = "n",
  drop.section  = "fixef",
  extralines    = make_extralines(models_comb),  # reuses existing function
  digits        = "r4",
  digits.stats  = "r4",
  fixef_sizes   = FALSE
)

# Insert midrule above the controls panel
tex_comb <- readLines(outfile_comb)
idx_comb  <- grep("^\\s*Weather controls\\s*&", tex_comb)
if (length(idx_comb) > 0) {
  tex_comb <- append(tex_comb, "\\midrule", after = idx_comb[1] - 1)
} else {
  warning("Could not find 'Weather controls' row; midrule not inserted.")
}
writeLines(tex_comb, outfile_comb)












#-------------------------------------------------------------------------------
#
# "08_regressions_safegraph.R": this script runs spending-related regressions 
# using daily data
#
#-------------------------------------------------------------------------------


#load relevant packages
source('scripts/setup.R')

#### setup

#load data 
spend_data_daily_reduc <- readRDS("./data/processed_data/main_spend_data_daily.RDS")

#create table creation functions
{
  # precompute FE metadata to make faster
  build_fe_info <- function(models) {
    lapply(models, function(m) {
      fe_terms <- tryCatch(names(fixef(m)), error = function(e) character(0))
      list(
        has_fe   = length(fe_terms) > 0,
        fe_terms = fe_terms,
        terms    = attr(m$terms, "term.labels")
      )
    })
  }
  
  # ---- Fast extralines builder ----
  make_extralines <- function(models, fe_info,
                              weather_vars = c("tmmx_park", "vs_park", "pr_park"),
                              fire_vars    = c("fire_dist_km_bound_park"),
                              check_symbol = "\\checkmark") {
    
    has_vars <- function(info, vars) all(vars %in% info$terms)
    
    has_pmy <- function(info) {
      any(grepl("\\^", info$fe_terms) &
            grepl("park",  info$fe_terms) &
            grepl("month", info$fe_terms) &
            grepl("year",  info$fe_terms))
    }
    
    has_pm <- function(info) {
      any(grepl("\\^", info$fe_terms) &
            grepl("park",  info$fe_terms) &
            grepl("month", info$fe_terms) &
            !grepl("year", info$fe_terms))
    }
    
    has_simple <- function(info, term) {
      any(grepl(term, info$fe_terms) & !grepl("\\^", info$fe_terms))
    }
    
    list(
      "Weather controls" =
        sapply(models, function(m) {
          coefs <- names(coef(m))
          if (all(weather_vars %in% coefs))
            check_symbol else ""
        }),
      
      "Fire controls" =
        sapply(models, function(m) {
          coefs <- names(coef(m))
          if (fire_vars %in% coefs)
            check_symbol else ""
        }),
      
      "Holiday" =
        sapply(seq_along(models), function(i) {
          if (any(grepl("holiday_dum", fe_info[[i]]$fe_terms))) check_symbol else ""
        }),
      
      "Day of week" =
        sapply(seq_along(models), function(i) {
          if (any(grepl("day_of_week", fe_info[[i]]$fe_terms))) check_symbol else ""
        }),
      
      "Day after holiday" = 
        sapply(seq_along(models), function(i) {
          if (any(grepl("day_after_hol", fe_info[[i]]$fe_terms)))
            check_symbol else ""
        }),
      
      "POI" = 
        sapply(seq_along(models), function(i) {
          if (any(grepl("placekey", fe_info[[i]]$fe_terms))) check_symbol else ""
        }),
      
      "Park" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          
          # standalone park FE only
          has_standalone_park <- any(
            grepl("^park$", ft) |
              grepl("^as\\.factor\\(park\\)$", ft)
          )
          
          # interacted park FEs (park^month or park^month^year)
          has_interacted_park <- any(
            grepl("\\^", ft) & grepl("park", ft)
          )
          
          if (has_standalone_park && !has_interacted_park)
            check_symbol else ""
        }),
      
      "Month" =
        sapply(seq_along(models), function(i) {
          info <- fe_info[[i]]
          if (!has_pm(info) && !has_pmy(info) && has_simple(info, "month"))
            check_symbol else ""
        }),
      
      "Year" =
        sapply(seq_along(models), function(i) {
          info <- fe_info[[i]]
          if (!has_pmy(info) && has_simple(info, "year"))
            check_symbol else ""
        }),
      
      "Park × Month" =
        sapply(seq_along(models), function(i) {
          if (has_pm(fe_info[[i]])) check_symbol else ""
        }),
      
      "Park × Month × Year" =
        sapply(seq_along(models), function(i) {
          if (has_pmy(fe_info[[i]])) check_symbol else ""
        })
    )
  }
  
  #function to make a full table for appendix
  make_extralines_full <- function(models, fe_info,
                                   check_symbol = "\\checkmark") {
    
    list(
      
      # ---------------- FIXED EFFECTS ----------------
      "Holiday" =
        sapply(seq_along(models), function(i) {
          if (any(grepl("holiday_dum", fe_info[[i]]$fe_terms)))
            check_symbol else ""
        }),
      
      "Wind direction" =
        sapply(seq_along(models), function(i) {
          if (any(grepl("wind_cat", fe_info[[i]]$fe_terms)))
            check_symbol else ""
        }),
      
      "Day of week" =
        sapply(seq_along(models), function(i) {
          if (any(grepl("day_of_week", fe_info[[i]]$fe_terms)))
            check_symbol else ""
        }),
      
      "Day after holiday" = 
        sapply(seq_along(models), function(i) {
          if (any(grepl("day_after_hol", fe_info[[i]]$fe_terms)))
            check_symbol else ""
        }),
      
      "POI" = 
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          if (any(grepl("placekey", ft)))
            check_symbol else ""
        }),
  
      "Park" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          
          # standalone park FE only
          has_standalone_park <- any(
            grepl("^park$", ft) |
              grepl("^as\\.factor\\(park\\)$", ft)
          )
          
          # interacted park FEs (park^month or park^month^year)
          has_interacted_park <- any(
            grepl("\\^", ft) & grepl("park", ft)
          )
          
          if (has_standalone_park && !has_interacted_park)
            check_symbol else ""
        }),
      
      "Month" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          if (!any(grepl("\\^", ft)) &&
              any(grepl("month", ft)))
            check_symbol else ""
        }),
      
      "Year" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          if (!any(grepl("park", ft) & grepl("month", ft) & grepl("year", ft)) &&
              any(grepl("year", ft)))
            check_symbol else ""
        }),
      
      "Park × Month" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          if (any(grepl("\\^", ft) &
                  grepl("park", ft) &
                  grepl("month", ft) &
                  !grepl("year", ft)))
            check_symbol else ""
        }),
      
      "Park × Month × Year" =
        sapply(seq_along(models), function(i) {
          ft <- fe_info[[i]]$fe_terms
          if (any(grepl("\\^", ft) &
                  grepl("park", ft) &
                  grepl("month", ft) &
                  grepl("year", ft)))
            check_symbol else ""
        })
    )
  }
  
  # ---- Fast etable wrapper ----
  make_simple_etable <- function(models,
                                 outfile,
                                 title,
                                 keep,
                                 drop          = NULL,      
                                 dict          = dict_master,
                                 digits        = "r4",
                                 stats_digits  = "r4",
                                 placement      = "H",
                                 insert_midrule = TRUE,
                                 headers        = NULL) {
    
    if (file.exists(outfile)) file.remove(outfile)
    
    fe_info <- build_fe_info(models)
    single  <- length(models) == 1
    el      <- make_extralines(models, fe_info)
    
    fixest::etable(
      models,
      tex   = TRUE,
      file  = outfile,
      title = title,
      keep  = keep,
      drop  = drop,
      dict  = dict,
      fitstat = "n",
      drop.section = "fixef",
      extralines   = if (single) NULL else el,   # only this line differs
      digits       = digits,
      digits.stats = stats_digits,
      fixef_sizes  = FALSE,
      placement    = placement,
      headers      = headers                                
    )
    
    tex <- readLines(outfile)
    
    if (single) {
      rows <- c("      \\midrule",
                paste0("      ", names(el), " & ", unlist(el), "\\\\"))
      tex <- append(tex, rows, after = grep("Fit statistics", tex)[1] - 2)
      writeLines(tex, outfile)
    } else if (insert_midrule) {
      idx <- grep("^\\s*Weather controls\\s*&", tex)
      if (length(idx) > 0) {
        tex <- append(tex, "\\midrule", after = idx[1] - 1)
        writeLines(tex, outfile)
      }
    }
    
    invisible(outfile)
  }
  
  }

#create master dictionary
{
dict_master <- c(
  
  #--- aqi variables ---
  "aqi_cat_parkModerate"   = "Park AQI: Moderate",
  "aqi_cat_parkUnhealthy for Sensitive Groups" = "Park AQI: Unhealthy for Sensitive Groups",
  "aqi_cat_parkUnhealthy"  = "Park AQI: Unhealthy",
  "aqi_cat_parkVery Unhealthy or Hazardous" = "Park AQI: Very Unhealthy or Hazardous",
  
  "aqi_cat_parkModerate:tourism_dum" = "Park AQI: Moderate x Tourism-dependent",
  "aqi_cat_parkUnhealthy for Sensitive Groups:tourism_dum" = "Park AQI: Unhealthy for Sensitive Groups x Tourism-dependent",
  "aqi_cat_parkUnhealthy:tourism_dum" = "Park AQI: Unhealthy x Tourism-dependent",
  "aqi_cat_parkVery Unhealthy or Hazardous:tourism_dum" = "Park AQI: Very Unhealthy or Hazardous x Tourism-dependent",
  
  "aqi_cat_park_lead1yModerate"   = "Park AQI +1 year : Moderate",
  "aqi_cat_park_lead1yUnhealthy for Sensitive Groups" = "Park AQI +1 year: Unhealthy for Sensitive Groups",
  "aqi_cat_park_lead1yUnhealthy"  = "Park AQI +1 year: Unhealthy",
  "aqi_cat_park_lead1yVery Unhealthy or Hazardous" = "Park AQI +1 year: Very Unhealthy or Hazardous",
  
  
  
  #--- aqi x industry (i() naming) ---
  "aqi_cat_park::Moderate:NAICS_cat::Accommodation" = "Accommodation: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Accommodation" = "Accommodation: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Accommodation" = "Accommodation: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Accommodation" = "Accommodation: Park AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_park::Moderate:NAICS_cat::Restaurants & Bars" = "Restaurant/bar: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Restaurants & Bars" = "Restaurant/bar: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Restaurants & Bars" = "Restaurant/bar: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Restaurants & Bars" = "Restaurant/bar: Park AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_park::Moderate:NAICS_cat::Food and Beverage Stores" = "Food and Beverage Stores: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Food and Beverage Stores" = "Food and Beverage Stores: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Food and Beverage Stores" = "Food and Beverage Stores: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Food and Beverage Stores" = "Food and Beverage Stores: Park AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_park::Moderate:NAICS_cat::Recreation & Entertainment" = "Recreation/entertainment: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Recreation & Entertainment" = "Recreation/entertainment: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Recreation & Entertainment" = "Recreation/entertainment: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Recreation & Entertainment" = "Recreation/entertainment: Park AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_park::Moderate:NAICS_cat::Retail & Shopping" = "Retail: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Retail & Shopping" = "Retail: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Retail & Shopping" = "Retail: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Retail & Shopping" = "Retail: Park AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_park::Moderate:NAICS_cat::Transportation" = "Transportation: Park AQI Moderate",
  "aqi_cat_park::Unhealthy for Sensitive Groups:NAICS_cat::Transportation" = "Transportation: Park AQI Unhealthy for Sensitive Groups",
  "aqi_cat_park::Unhealthy:NAICS_cat::Transportation" = "Transportation: Park AQI Unhealthy",
  "aqi_cat_park::Very Unhealthy or Hazardous:NAICS_cat::Transportation" = "Transportation: Park AQI Very Unhealthy or Hazardous",

  #visit variables
  "raw_daily_visits_park" = "Park Visits",
  "raw_daily_visits_poi" = "POI Visits",

  #poi controls
  "aqi_cat_poiModerate"   = "POI AQI Moderate",
  "aqi_cat_poiUnhealthy for Sensitive Groups" = "POI AQI Unhealthy for Sensitive Groups",
  "aqi_cat_poiUnhealthy"  = "POI AQI Unhealthy",
  "aqi_cat_poiVery Unhealthy or Hazardous" = "POI AQI Very Unhealthy or Hazardous",
  
  "aqi_cat_poiModerate:tourism_dum" = "POI AQI Moderate x Tourism-dependent",
  "aqi_cat_poiUnhealthy for Sensitive Groups:tourism_dum" = "POI AQI Unhealthy for Sensitive Groups x Tourism-dependent",
  "aqi_cat_poiUnhealthy:tourism_dum" = "POI AQI Unhealthy x Tourism-dependent",
  "aqi_cat_poiVery Unhealthy or Hazardous:tourism_dum" = "POI AQI Very Unhealthy or Hazardous x Tourism-dependent",
  
  #--- fire variables ---
  "fire_dist_km_bound_park" = "Fire distance",
  "I(fire_dist_km_bound_park^2)" = "Fire distance squared",
  
  #--- weather ---
  "tmmx_park"                         = "Max temp.",
  "vs_park"                           = "Mean wind speed",
  "pr_park"                           = "Cumulative precipitation",
  
  #--- dependent variables ---
  "ln_spending" = "$Log(Spending)$",
  "daily_spend" = "Spending",
  "daily_visits" = "Visits to POI",
  
  #--- other ----
  "tourism_dum" = "Tourism-dependent",
  "normalized_daily_visits_park_1000s" = "Park visits (1000s)",
  "Visits" = "POI visits",

  #--- fixed effects ---
  "holiday_dum" = "Holiday",
  "wind_cat_park" = "Wind direction", 
  "park" = "Park",
  "as.factor(month)" = "Month",
  "as.factor(year)" = "Year",
  "park-as.factor(month)-as.factor(year)" = "Park:month:year",
  "day_of_week" = "Day of week",
  
  #--- lagged variables ---
  "smoke_lag_wk1" = "Park smoke lag: 1-7 days",
  "smoke_lag_wk2" = "Park smoke lag: 8-14 days",
  "smoke_lag_wk3" = "Park smoke lag: 15-21 days",
  "smoke_lag_wk4" = "Park smoke lag: 22-28 days",
  
  "smoke_lag07" = "Park smoke: Lag t-7",
  "smoke_lag06" = "Park smoke: Lag t-6",
  "smoke_lag05" = "Park smoke: Lag t-5",
  "smoke_lag04" = "Park smoke: Lag t-4",
  "smoke_lag03" = "Park smoke: Lag t-3",
  "smoke_lag02" = "Park smoke: Lag t-2",
  "smoke_lag01" = "Park smoke: Lag t-1",

  "smoke_lead01" = "Park smoke: Lead t+1",
  "smoke_lead02" = "Park smoke: Lead t+2",
  "smoke_lead03" = "Park smoke: Lead t+3",
  "smoke_lead04" = "Park smoke: Lead t+4",
  "smoke_lead05" = "Park smoke: Lead t+5",
  "smoke_lead06" = "Park smoke: Lead t+6",
  "smoke_lead07" = "Park smoke: Lead t+7",
  "smoke_lead08" = "Park smoke: Lead t+8",
  "smoke_lead09" = "Park smoke: Lead t+9",
  "smoke_lead10" = "Park smoke: Lead t+10",
  "smoke_lead11" = "Park smoke: Lead t+11",
  "smoke_lead12" = "Park smoke: Lead t+12",
  "smoke_lead13" = "Park smoke: Lead t+13",
  "smoke_lead14" = "Park smoke: Lead t+14"
)
}
  
#### main regressions
{
  
  #models without visitation control
  reg_aqi_no_fe <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                   pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                   wind_cat_park + holiday_dum + day_of_week + day_after_hol +  park + as.factor(placekey),
                                 cluster = ~ park + county,
                                 data = spend_data_daily_reduc)
  summary(reg_aqi_no_fe)
  
  reg_aqi_no_pm_fe <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                      pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2)|
                                      wind_cat_park + holiday_dum + day_of_week + day_after_hol + park + as.factor(placekey) + as.factor(month) + as.factor(year),
                                    cluster = ~ park + county,
                                    data = spend_data_daily_reduc)
  summary(reg_aqi_no_pm_fe)
  
  reg_aqi_pm_fe <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                   pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                   wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month) + as.factor(year),
                                 cluster = ~ park + county,
                                 data = spend_data_daily_reduc)
  summary(reg_aqi_pm_fe)
  
  reg_aqi <- fixest::fepois(daily_spend ~ aqi_cat_park  +
                             pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                             wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                           cluster = ~ park + county,
                           data = spend_data_daily_reduc)
  summary(reg_aqi)
  
  #models with visitation control
  reg_aqi_visits <- fixest::fepois(daily_spend ~ aqi_cat_park  + normalized_daily_visits_park_1000s +
                             pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                             wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                           cluster = ~ park + county,
                           data = spend_data_daily_reduc)
  summary(reg_aqi_visits)
  
  #make simple table
  models_main <- list(
    reg_aqi_no_fe, 
    reg_aqi_no_pm_fe,
    reg_aqi_pm_fe,
    reg_aqi,
    reg_aqi_visits
  )
  
  make_simple_etable(
    models = models_main,
    outfile = "./output/tables/safegraph_main_regression_results.tex",
    title = "PPML regression of the effect of smoke AQI at nearby national parks on daily spending at businesses in gateway communities, with and without controlling for park visits",
    keep = "%aqi|visits"
  )
  
  #print results for full output table appendix
  
  #create and clear file
  outfile_aqi_full <- "./output/tables/safegraph_main_regression_results_full.tex"
  if (file.exists(outfile_aqi_full)) file.remove(outfile_aqi_full)
  
  #precompute fixed effects
  fe_info_main <- build_fe_info(models_main)
  
  #make table
  fixest::etable(
    models_main,
    
    tex = TRUE,
    file = outfile_aqi_full,
    title = "Full PPML regression results of the effect of smoke AQI at nearby national parks on daily spending at businesses in gateway communities, with and without controlling for park visits",
    
    # AQI coefficients only 
    dict = dict_master,
    
    # Only show N
    fitstat = "n",
    
    # Remove default FE section
    drop.section = "fixef",
    
    # Custom FE / controls panel
    extralines = make_extralines_full(models_main, fe_info_main),
    
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

#### heterogeneous tourism effects
{
  #drops tourism_dum because of POI fixed effects, but needs to be included in fixest call
  reg_aqi_tour <- fixest::fepois(daily_spend ~ aqi_cat_park  + tourism_dum + tourism_dum:aqi_cat_park +
                             pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                             wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                           cluster = ~ park + county,
                           data = spend_data_daily_reduc)
  summary(reg_aqi_tour)
  
  
  #make whisker plot of results for main text
  b     <- coef(reg_aqi_tour)
  V     <- vcov(reg_aqi_tour)
  cats  <- c("Moderate", "Unhealthy for Sensitive Groups",
             "Unhealthy", "Very Unhealthy or Hazardous")
  main  <- paste0("aqi_cat_park", cats)
  inter <- paste0("aqi_cat_park", cats, ":tourism_dum")
  stopifnot(all(c(main, inter) %in% names(b)))
  dft    <- degrees_freedom(reg_aqi_tour, type = "t")
  crit   <- qt(0.975, dft)
  se_all <- sqrt(diag(V))
  
  coef_df <- function(terms, panel) {
    data.frame(aqi = cats,
               est = unname(b[terms]),
               se  = unname(se_all[terms]),
               panel = panel)
  }
  
  plot_df2 <- bind_rows(
    coef_df(main,  "A. Effect relative to Good AQI (non-tourism POIs)"),
    coef_df(inter, "B. Difference for tourism POIs (interaction)")
  ) |>
    mutate(
      lo    = est - crit * se,
      hi    = est + crit * se,
      aqi   = factor(aqi, levels = rev(cats)),
      panel = factor(panel, levels = unique(panel))
    )
  
  windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
  tour_plot <- ggplot(plot_df2, aes(x = est, y = aqi)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                  width = 0.2, linewidth = 0.6, color = "black") +
    geom_point(size = 2.5, color = "black") +
    facet_wrap(~ panel, ncol = 1) +
    labs(x = "Estimate", y = NULL) +
    theme_minimal(base_size = 11, base_family = "Times New Roman") +
    theme(
      text       = element_text(color = "black"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      strip.text = element_text(color = "black", hjust = 0, face = "bold")
    )
  tour_plot
  
  ggsave("./output/figures/aqi_tourism_heterogeneity.pdf",
         plot = tour_plot, width = 6.5, height = 5, units = "in",
         device = cairo_pdf)
}

#### lagged smoke effects/intertemporal substitution of spending
{
  reg_aqi_lagged <- fixest::fepois(daily_spend ~ aqi_cat_park  + 
                                     smoke_lag_wk1 +
                                     smoke_lag_wk2 +
                                     smoke_lag_wk3 +
                                     smoke_lag_wk4 +
                                     pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                     wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                   cluster = ~ park + county,
                                   data = spend_data_daily_reduc)
  summary(reg_aqi_lagged)
  
  #create whisker plot
  dft  <- degrees_freedom(reg_aqi_lagged, type = "t")
  crit <- qt(0.975, dft)
  b_l  <- coef(reg_aqi_lagged)
  se_l <- sqrt(diag(vcov(reg_aqi_lagged)))
  
  cats    <- c("Moderate", "Unhealthy for Sensitive Groups",
               "Unhealthy", "Very Unhealthy or Hazardous")
  cur_trm <- paste0("aqi_cat_park", cats)
  
  lag_trm    <- paste0("smoke_lag_wk", 1:4)
  lag_labels <- c("Days 1-7", "Days 8-14", "Days 15-21", "Days 22-28")
  
  stopifnot(all(c(cur_trm, lag_trm) %in% names(b_l)))
  
  plot_df_lag <- bind_rows(
    data.frame(label = cats, est = unname(b_l[cur_trm]), se = unname(se_l[cur_trm]),
               panel = "A. Effect of AQI on day t relative to Good AQI"),
    data.frame(label = lag_labels, est = unname(b_l[lag_trm]), se = unname(se_l[lag_trm]),
               panel = "B. Effect of smoke in preceding weeks")
  ) |>
    mutate(
      lo    = est - crit * se,
      hi    = est + crit * se,
      label = factor(label, levels = rev(c(cats, lag_labels))),
      panel = factor(panel, levels = unique(panel))
    )
  
  lag_plot <- ggplot(plot_df_lag, aes(x = est, y = label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                  width = 0.2, linewidth = 0.6, color = "black") +
    geom_point(size = 2.5, color = "black") +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    scale_y_discrete(expand = expansion(add = 0.4)) +
    labs(x = "Estimate", y = NULL) +
    theme_minimal(base_size = 11, base_family = "Times New Roman") +
    theme(
      text       = element_text(color = "black"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      strip.text = element_text(color = "black", hjust = 0, face = "bold")
    )
  lag_plot
  
  ggsave("./output/figures/aqi_lagged_effects_spending.pdf",
         plot = lag_plot, width = 6.5, height = 4, units = "in",
         device = cairo_pdf)
  
}

#### create combined full estimate table for tourism and lagged effects for appendix
{
  models_tour_lagged <- list(
    reg_aqi_tour, 
    reg_aqi_lagged
  )
  
  #create and clear file
  outfile_aqi_full_tour_lagged <- "./output/tables/safegraph_main_regression_results_full_tour_lagged.tex"
  if (file.exists(outfile_aqi_full_tour_lagged)) file.remove(outfile_aqi_full_tour_lagged)
  
  #precompute fixed effects
  build_fe_info <- function(models) {
    lapply(models, function(m) {
      fe_terms <- m$fixef_vars
      if (is.null(fe_terms)) fe_terms <- character(0)
      list(
        has_fe   = length(fe_terms) > 0,
        fe_terms = fe_terms,
        terms    = attr(m$terms, "term.labels")
      )
    })
  }
  
  #make table
  fixest::etable(
    models_tour_lagged,
    
    tex = TRUE,
    file = outfile_aqi_full_tour_lagged,
    title = "Full PPML regression results of the effect of smoke AQI at nearby national parks on daily spending at businesses in gateway communities, separately allowing for heterogeneous smoke effects by tourism and non-tourism-based businesses and lagged park-level smoke effects",
    
    # AQI coefficients only 
    dict = dict_master,
    
    # Only show N
    fitstat = "n",
    
    # Remove default FE section
    drop.section = "fixef",
    
    # Custom FE / controls panel
    extralines = make_extralines_full(models_tour_lagged, build_fe_info(models_tour_lagged)),
    
    digits = "r4",
    digits.stats = "r4",
    fixef_sizes = FALSE,
    placement = "H"
  )
  
  #add horizontal line to table
  tex <- readLines(outfile_aqi_full_tour_lagged)
  # Find the first extraline row label (we use "Weather controls" as the anchor)
  idx <- grep("^\\s*Holiday\\s*&", tex)
  
  if (length(idx) > 0) {
    # Insert \midrule on the line *before* the first extraline row
    tex <- append(tex, "\\midrule", after = idx[1] - 1)
  } else {
    warning("Could not find 'Weather controls' row in LaTeX output; midrule not inserted.")
  }
  writeLines(tex, outfile_aqi_full_tour_lagged)
}

#### heterogeneous effects by NAICS category 
{
  reg_aqi_NAICS_ref <- fixest::fepois(
    daily_spend ~ aqi_cat_park +
      i(aqi_cat_park, NAICS_cat, ref = "Good", ref2 = "Other") +
      pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
      wind_cat_park + holiday_dum + day_of_week + day_after_hol +
      as.factor(placekey) + park^as.factor(month)^as.factor(year),
    cluster = ~ park + county,
    data = spend_data_daily_reduc
  )
  summary(reg_aqi_NAICS_ref)
  
  #make simple table
  models_main_NAICS <- list(
    reg_aqi_NAICS_ref
  )
  
  make_simple_etable(
    models = models_main_NAICS,
    outfile = "./output/tables/safegraph_main_regression_results_naics.tex",
    title = "PPML regression of the effect of smoke AQI at nearby national parks on daily spending in gateway communities, differentiated between by general tourism-dependent industry category",
    keep = "%aqi")
  
}

#### effect on POI visits instead of spending
{
  
  #trim to just include POI-days used in main spending estimations
  spend_data_daily_reduc_trimmed <- spend_data_daily_reduc  %>%
    slice(fixest::obs(reg_aqi))
  
  reg_aqi_poi_visits <- fixest::fenegbin(Visits ~ aqi_cat_park  +
                             pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                             wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                           cluster = ~ park + county,
                           data = spend_data_daily_reduc_trimmed)
  summary(reg_aqi_poi_visits)
  
  #models with visitation control
  reg_aqi_poi_visits_park_visits <- fixest::fenegbin(Visits ~ aqi_cat_park  + normalized_daily_visits_park_1000s +
                                    pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                    wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                  cluster = ~ park + county,
                                  data = spend_data_daily_reduc_trimmed)
  summary(reg_aqi_poi_visits_park_visits)
  
  #put models into object
  model_poi_visits <- list(
    reg_aqi_poi_visits,
    reg_aqi_poi_visits_park_visits
  )
  
  #make simple table
  make_simple_etable(
    models = model_poi_visits,
    outfile = "./output/tables/safegraph_poi_visits_results.tex",
    title = "Negative binomial regression of the effect of smoke AQI at nearby national parks on daily visits to businesses in gateway communities, with and without controlling for park visits",
    keep = "%aqi|visits"
  )
  
}

#### main regressions exploring what is driving spending losses
{
  #drop 0's 
  spend_data_daily_reduc_nonzero <- spend_data_daily_reduc %>%
    filter(daily_spend > 0)
  
  reg_aqi_nonzero <- fixest::fepois(daily_spend ~ aqi_cat_park + 
                                     pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                     wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                   cluster = ~ park + county,
                                   data = spend_data_daily_reduc_nonzero)
  summary(reg_aqi_nonzero)
  
  #reduce data to just POIs with all 4 years of data
  spend_data_daily_reduc_all4 <- spend_data_daily_reduc %>%
    group_by(placekey) %>%
    mutate(n_unq_days = n()) %>%
    ungroup() %>%
    filter(n_unq_days == max(n_unq_days))
  
  reg_aqi_all4 <- fixest::fepois(daily_spend ~ aqi_cat_park + 
                                  pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                  wind_cat_park + holiday_dum + day_of_week + day_after_hol +  as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                cluster = ~ park + county,
                                data = spend_data_daily_reduc_all4)
  summary(reg_aqi_all4)
  
  #identify poi's with any online spending in the sample
  online_pks <- spend_data_daily_reduc %>%
    group_by(placekey) %>%
    summarize(any_online = any(online_spend_monthly > 0, na.rm = TRUE)) %>%
    filter(any_online) %>%
    pull(placekey)
  
  spend_data_daily_reduc_no_online <- spend_data_daily_reduc %>%
    filter(!placekey %in% online_pks)
  
  reg_aqi_no_online <- fixest::fepois(daily_spend ~ aqi_cat_park + 
                                       pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                       wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                     cluster = ~ park + county,
                                     data = spend_data_daily_reduc_no_online)
  summary(reg_aqi_no_online)
  
  #print results to table
  models_robust_spending_drive <- list(reg_aqi_nonzero,  
                                       reg_aqi_all4,
                                       reg_aqi_no_online)
  
  make_simple_etable(
    models = models_robust_spending_drive,
    outfile = "./output/tables/safegraph_main_regression_results_robust_driving_spending.tex",
    title = "PPML regression of the effect of smoke AQI at nearby national parks on daily spending at
businesses in gateway communities, conditional on non-zero observed daily spending, a full 4 years of observed spending data, or with zero reported online spending",
    keep = "%aqi"
  )
} 

#### create NAICS bridge table
{
  NAICS_data_for_table <- naics_data %>%
    dplyr::select(-c(Indoors)) %>%
    dplyr::select(NAICS_CODE, description, NAICS_cat) %>%
    filter(NAICS_cat != "Other") %>%
    rename(
      `NAICS Code` = NAICS_CODE,
      Description = description,
      `NAICS Category` = NAICS_cat
    ) 
  
  NAICS_latex <- NAICS_data_for_table %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("NAICS Code", "Description", "NAICS Category"),
      caption = "NAICS codes and tourism-related categories",
      escape = TRUE,
      linesep = ""
    ) %>%
    kable_styling(
      latex_options = c("repeat_header"),
      font_size = 8
    ) %>%
    column_spec(1, width = "1.75cm") %>%     
    column_spec(2, width = "9.5cm") %>%     
    column_spec(3, width = "3.75cm")
  
  
  writeLines(NAICS_latex, "output/tables/naics_category_table.tex")
  
  
}

#### placebo test
{
  #using smoke one year in advance
  reg_aqi_no_fe_placebo <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y + 
                                           pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                           wind_cat_park + holiday_dum + day_of_week + day_after_hol + park + as.factor(placekey),
                                         cluster = ~ park + county,
                                         data = spend_data_daily_reduc)
  summary(reg_aqi_no_fe_placebo)
  
  reg_aqi_no_pm_fe_placebo  <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                               pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2)|
                                               wind_cat_park + holiday_dum + day_of_week + day_after_hol + park + as.factor(placekey) + as.factor(month) + as.factor(year),
                                             cluster = ~ park + county,
                                             data = spend_data_daily_reduc)
  summary(reg_aqi_no_pm_fe_placebo)
  
  reg_aqi_pm_fe_placebo  <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                            pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                            wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month) + as.factor(year),
                                          cluster = ~ park + county,
                                          data = spend_data_daily_reduc)
  summary(reg_aqi_pm_fe_placebo)
  
  
  reg_aqi_placebo <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                       pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                        holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                     cluster = ~ park + county,
                                     data = spend_data_daily_reduc)
  summary(reg_aqi_placebo)
  
  #using daily lags and leads +- 7 days
  reg_aqi_no_fe_placebo_days <- fixest::fepois(daily_spend ~ 
                                                smoke_lag01 + smoke_lag02 + smoke_lag03 + smoke_lag04 + smoke_lag05 + smoke_lag06 + smoke_lag07 + 
                                                aqi_cat_park + 
                                                smoke_lead01 + smoke_lead02 + smoke_lead03 + smoke_lead04 + smoke_lead05 + smoke_lead06 + smoke_lead07 +
                                                pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                                wind_cat_park + holiday_dum + day_of_week + day_after_hol + park + as.factor(placekey),
                                              cluster = ~ park + county,
                                              data = spend_data_daily_reduc)
  summary(reg_aqi_no_fe_placebo_days)
  
  reg_aqi_no_pm_fe_placebo_days  <- fixest::fepois(daily_spend ~
                                                    smoke_lag01 + smoke_lag02 + smoke_lag03 + smoke_lag04 + smoke_lag05 + smoke_lag06 + smoke_lag07 + 
                                                    aqi_cat_park + 
                                                    smoke_lead01 + smoke_lead02 + smoke_lead03 + smoke_lead04 + smoke_lead05 + smoke_lead06 + smoke_lead07 +
                                                    pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2)|
                                                    wind_cat_park + holiday_dum + day_of_week + day_after_hol + park + as.factor(placekey) + as.factor(month) + as.factor(year),
                                                  cluster = ~ park + county,
                                                  data = spend_data_daily_reduc)
  summary(reg_aqi_no_pm_fe_placebo_days)
  
  reg_aqi_pm_fe_placebo_days  <- fixest::fepois(daily_spend ~ 
                                                 smoke_lag01 + smoke_lag02 + smoke_lag03 + smoke_lag04 + smoke_lag05 + smoke_lag06 + smoke_lag07 + 
                                                 aqi_cat_park + 
                                                 smoke_lead01 + smoke_lead02 + smoke_lead03 + smoke_lead04 + smoke_lead05 + smoke_lead06 + smoke_lead07 +
                                                 pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                                 wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month) + as.factor(year),
                                               cluster = ~ park + county,
                                               data = spend_data_daily_reduc)
  summary(reg_aqi_pm_fe_placebo_days)
  
  reg_aqi_placebo_days  <- fixest::fepois(daily_spend ~ 
                                           smoke_lag01 + smoke_lag02 + smoke_lag03 + smoke_lag04 + smoke_lag05 + smoke_lag06 + smoke_lag07 + 
                                           aqi_cat_park + 
                                           smoke_lead01 + smoke_lead02 + smoke_lead03 + smoke_lead04 + smoke_lead05 + smoke_lead06 + smoke_lead07 +
                                           pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                           wind_cat_park + holiday_dum + day_of_week + day_after_hol + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                         cluster = ~ park + county,
                                         data = spend_data_daily_reduc)
  summary(reg_aqi_placebo_days)

  #create table
  models_placebo <- list(
    reg_aqi_no_fe_placebo, reg_aqi_no_pm_fe_placebo, reg_aqi_pm_fe_placebo, reg_aqi_placebo,
    reg_aqi_no_fe_placebo_days, reg_aqi_no_pm_fe_placebo_days, reg_aqi_pm_fe_placebo_days,  reg_aqi_placebo_days
  )
  
  make_simple_etable(
    models = models_placebo,
    outfile = "./output/tables/safegraph_placebo_regression_results.tex",
    title = "PPML regression placebo tests estimating the effect of future smoke POI spending nearby national parks",
    keep = "%aqi|lag|lead"
  )
  
  ##explore one year placebo results
  
  #rerun dropping two dates with unusually high spending
  spend_data_subsample_one_day <- spend_data_daily_reduc %>% 
    filter(!date_LT %in% c("2019-09-03"))
  
  reg_aqi_omit_labor <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                         pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                         holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                       cluster = ~ park + county,
                                       data = spend_data_subsample_one_day)
  summary(reg_aqi_omit_labor)
  
  
  reg_aqi_placebo_subsample_labor <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                                pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                                holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                              cluster = ~ park + county,
                                              data = spend_data_subsample_one_day)
  summary(reg_aqi_placebo_subsample_labor)
  
  spend_data_subsample_one_day_col <- spend_data_daily_reduc %>% 
    filter(!date_LT %in% c("2019-10-15"))
  
  reg_aqi_omit_col <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                         pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                         holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                       cluster = ~ park + county,
                                       data = spend_data_subsample_one_day_col)
  summary(reg_aqi_omit_col)
  
  
  reg_aqi_placebo_subsample_col <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                                      pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                                      holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                                    cluster = ~ park + county,
                                                    data = spend_data_subsample_one_day_col)
  summary(reg_aqi_placebo_subsample_col)
  
  spend_data_subsample_two_day <- spend_data_daily_reduc %>% 
    filter(!date_LT %in% c("2019-09-03", "2019-10-15"))
  
  reg_aqi_omit_both <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                       pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                       holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                     cluster = ~ park + county,
                                     data = spend_data_subsample_two_day)
  summary(reg_aqi_omit_both)
  
  
  reg_aqi_placebo_subsample_both <- fixest::fepois(daily_spend ~ aqi_cat_park_lead1y +
                                                    pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                                    holiday_dum + day_after_hol + wind_cat_park + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                                  cluster = ~ park + county,
                                                  data = spend_data_subsample_two_day)
  summary(reg_aqi_placebo_subsample_both)
  
  #create table
  models_placebo_diag <- list(
    reg_aqi_omit_labor,  reg_aqi_placebo_subsample_labor,
    reg_aqi_omit_col, reg_aqi_placebo_subsample_col,
    reg_aqi_omit_both, reg_aqi_placebo_subsample_both
  )
  
  make_simple_etable(
    models = models_placebo_diag,
    outfile = "./output/tables/safegraph_placebo_regression_results_diagnostic.tex",
    title = "PPML regressions diagnosing placebo tests by dropping influential post-holiday dates with potential uncontrolled report timing inconsistencies",
    keep = "%aqi|lag|lead",
    headers = list("^" = list("Drop 09/03/2019" = 2,
                              "Drop 10/15/2019" = 2,
                              "Drop both" = 2))
  )
  
  ##explore seven day placebo results
  reg_aqi_placebo_days_diag_lead  <- fixest::fepois(daily_spend ~ 
                                            smoke_lag01 + smoke_lag02 + smoke_lag03 + smoke_lag04 + smoke_lag05 + smoke_lag06 + smoke_lag07 + 
                                            aqi_cat_park + 
                                            smoke_lead01 + smoke_lead02 + smoke_lead03 + smoke_lead04 + smoke_lead05 + smoke_lead06 + smoke_lead07 +
                                            smoke_lead08 + smoke_lead09 + smoke_lead10 + smoke_lead11 + smoke_lead12 + smoke_lead13 + smoke_lead14 +
                                            pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                                            wind_cat_park + holiday_dum + day_of_week + day_after_hol + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                          cluster = ~ park + county,
                                          data = spend_data_daily_reduc)
  summary(reg_aqi_placebo_days_diag_lead)
  
  #make table
  models_placebo_diag_day <- list(reg_aqi_placebo_days_diag_lead)
  
  make_simple_etable(
    models = models_placebo_diag_day,
    outfile = "./output/tables/safegraph_placebo_regression_results_diagnostic_days.tex",
    title   = "PPML placebo test with 14 daily leads of smoke, testing for unobserved weekly repeating patterns in spending",
    keep = "%aqi|lag|lead",
  )
}

#### spatial correlation in smoke
{
  #create poi-AQI variable
  # Function to categorize PM2.5 concentrations into AQI categories
  pm25_to_aqi_category <- function(pm25) {
    dplyr::case_when(
      pm25 <= 12.0 ~ "Good",
      pm25 > 12 & pm25 <= 35.4 ~ "Moderate",
      pm25 > 35.4 & pm25 <= 55.4 ~ "Unhealthy for Sensitive Groups",
      pm25 > 55.4 & pm25 <= 150.4 ~ "Unhealthy",
      pm25 > 150.4 ~ "Very Unhealthy or Hazardous",
      is.na(pm25) ~ NA, 
    )
  }
  spend_data_daily_reduc <- spend_data_daily_reduc %>%
    mutate(aqi_cat_poi = pm25_to_aqi_category(mean_smoke_pm25)) %>%
    mutate(aqi_cat_poi = factor(aqi_cat_poi,
                                levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy or Hazardous")
    ))
  
  #run regression controlling for poi AQI
  reg_aqi_poi_aqi_control <- fixest::fepois(daily_spend ~ aqi_cat_park  + aqi_cat_poi + 
                                             pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                             wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                           cluster = ~ park + county,
                                           data = spend_data_daily_reduc)
  summary(reg_aqi_poi_aqi_control) 
  
  #create table
  models_poi_controls <- list(reg_aqi_poi_aqi_control)
  
  make_simple_etable(
    models = models_poi_controls,
    outfile = "./output/tables/safegraph_main_control_poi_AQI.tex",
    title = "PPML regressions of the effect of park-level AQI on daily spending at businesses in gateway communities, controlling for POI-level AQI",
    keep = "%aqi"
  )
  
  ## calculate strength of correlation in smoke, controlling for other variables
  
  #using our model, gives more weight to parks with more POIs and POIs with more days
  reg_poi_smoke <- fixest::feols(mean_smoke_pm25 ~ 
                                              pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                              wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                            cluster = ~ park + county,
                                            data = spend_data_daily_reduc)
  summary(reg_poi_smoke) 
  
  reg_park_smoke <-  fixest::feols(mean_smoke_pm2.5_park ~ 
                                     pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                     wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                   cluster = ~ park + county,
                                   data = spend_data_daily_reduc)
  summary(reg_park_smoke) 
  
  cor(residuals(reg_poi_smoke), residuals(reg_park_smoke))
  
  #simple correlation between parks and their surrounding POIs
  cors_by_poi_my <- spend_data_daily_reduc %>%
    group_by(placekey, month, year) %>%
    summarize(cor_poi_park = cor(mean_smoke_pm25, mean_smoke_pm2.5_park))
  
  cors_by_poi <- spend_data_daily_reduc %>%
    group_by(placekey) %>%
    summarize(cor_poi_park = cor(mean_smoke_pm25, mean_smoke_pm2.5_park))
  
}

#### alternative OLS model
{
  #trim to just include POI-days used in main spending estimations
  spend_data_daily_reduc_trimmed <- spend_data_daily_reduc  %>%
    slice(fixest::obs(reg_aqi))
  
  reg_aqi_OLS <- fixest::feols(ln_spending ~ aqi_cat_park  +
                              pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) |
                              wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                            cluster = ~ park + county,
                            data =  spend_data_daily_reduc_trimmed)
  summary(reg_aqi_OLS)
  
  #create table
  models_OLS <- list(reg_aqi_OLS)
  
  make_simple_etable(
    models = models_OLS,
    outfile = "./output/tables/safegraph_main_OLS.tex",
    title = "OLS regressions of the effect of park-level AQI on the natural log of daily spending at businesses in gateway communities",
    keep = "%aqi"
  )

}

#### main regression dropping weekends + Monday 
{
  spend_data_daily_reduc_drop_we <- spend_data_daily_reduc %>%
    filter(day_of_week %in% c(3, 4, 5, 6))
  
  #main regressions 
  reg_aqi_drop_we <- fixest::fepois(daily_spend ~ aqi_cat_park + 
                                     pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                     wind_cat_park + holiday_dum + day_of_week + day_after_hol + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                   cluster = ~ park + county,
                                   data = spend_data_daily_reduc_drop_we)
  summary(reg_aqi_drop_we)
  
  #make simple table
  models_main_drop_we <- list(
    reg_aqi_drop_we)
  
  make_simple_etable(
    models = models_main_drop_we,
    outfile = "./output/tables/safegraph_main_regression_results_drop_we.tex",
    title = "PPML regression of the effect of smoke AQI at nearby national parks on daily spending at businesses in gateway communities, isolating Tuesdays, Wednesday, Thursdays, and Fridays to check for the robustness of results to potential debit reporting inconsistencies",
    keep = "%aqi")
  
}

#### Alt. gateway community measures
{

  #with all pois within 15 minutes travel time
  spend_data_close_tt_15 <- spend_data_daily_reduc %>%
    filter(travel_time <= 15)
  
  reg_aqi_alt_tt_15 <- fixest::fepois(daily_spend ~ aqi_cat_park +
                                       pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                       wind_cat_park + holiday_dum + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                     cluster = ~ park + county,
                                     data = spend_data_close_tt_15)
  summary(reg_aqi_alt_tt_15)

  
  #with all pois within 60 minutes travel time
  spend_data_close_tt_60 <- spend_data_daily_reduc %>%
    filter(travel_time <= 60)
  
  reg_aqi_alt_tt_60 <- fixest::fepois(daily_spend  ~ aqi_cat_park +
                                       pr_park + tmmx_park + vs_park + fire_dist_km_bound_park + I(fire_dist_km_bound_park^2) | 
                                       wind_cat_park + holiday_dum + day_of_week + as.factor(placekey) + park^as.factor(month)^as.factor(year),
                                     cluster = ~ park + county,
                                     data = spend_data_close_tt_60)
  summary(reg_aqi_alt_tt_60)
  
  #gather models
  models_gwc_robust <- list(reg_aqi_alt_tt_15, reg_aqi_alt_tt_60)
  make_simple_etable(
    models = models_gwc_robust,
    outfile = "./output/tables/safegraph_main_regression_results_gwc_robust.tex",
    title = "PPML regressions of the effect of smoke AQI at nearby national parks on daily spending at businesses in gateway communities using more restrictive gateway community definitions",
    keep = "%aqi"
  )
  
}


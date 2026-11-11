#-------------------------------------------------------------------------------
#
# '04_welfare_losses.R': This script calculates rec. losses and welfare losses
# following consumer surplus results from '03_tc_regressions.R' and daily
# visitation losses from the main regression
#
# Welfare losses have two components (following Gellman et al. 2025, JEEM):
#   (1) forgone visitation: lost visits x value of a lost visit, where the
#       value of a lost visit is capped at the WTP to avoid smoke (fn. 47)
#   (2) smoke-affected visits: visits still taken x WTP to avoid smoke
#
# Change markers:  # >>> CHANGED / # >>> NEW / # >>> REMOVED ... # <<< END
#-------------------------------------------------------------------------------

#load packages and settings
source('./scripts/setup.R')

#### calculate recreation losses by park-month

## get coefficient estimates and percent changes
#load data
main_data_subset <- readRDS("./data/processed_data/main_data_subset_final.RDS")

#run main visitation regression
reg_aqi_main <- fenegbin(Visits ~  as.factor(aqi_cat) +
                           fire_dist_km_bound + I(fire_dist_km_bound^2) +
                           tmmx + vs + pr | as.factor(wind_cat) + 
                           as.factor(day_of_week) + holiday_dum +
                           as.factor(year) + as.factor(park)^as.factor(month)^as.factor(year) + as.factor(park) + as.factor(month),
                         data = main_data_subset,
                         cluster = ~ park)
summary(reg_aqi_main)

#bring in smoke data from sample
visit_data_subset <- main_data_subset %>%
  dplyr::select(park, month, aqi_cat, mean_smoke_pm2.5)

#print out coefficient plot and order 
coefs <- broom::tidy(reg_aqi_main, conf.int = T)
coefs_filtered <- coefs %>%
  filter(grepl("aqi_cat", term)) %>%
  mutate(term = str_remove(term, "as\\.factor\\(aqi_cat\\)"))
aqi_order <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
               "Unhealthy", "Very Unhealthy or Hazardous")
coefs_filtered <- coefs_filtered %>%
  mutate(term = factor(term, levels = aqi_order))

#calculate percent changes 
coefs_filtered <- coefs_filtered %>%
  mutate(
    effect_pct = (exp(estimate) - 1) * 100,
    conf.low_pct = (exp(conf.low) - 1) * 100,
    conf.high_pct = (exp(conf.high) - 1) * 100
  )


## calculate visitation losses by park

#bring in visitation data and simplify, calculate mean hours per visit
nps_visit_data <- read.csv("./data/nps_official_data/rec_hours_nps_data.csv")
park_month_grid <- nps_visit_data %>%
  dplyr::select(ParkName, Month, Year, RecreationVisits, RecreationHours) %>%
  rename(park = ParkName,
         year = Year,
         month = Month) %>%
  mutate(hours_per_visit = RecreationHours/RecreationVisits,
         park = gsub(" NP$", " National Park", park),
         park = gsub("NP & PRES$", "National Park", park),
         park = gsub("New River Gorge National Park", "New River Gorge National Park and Preserve", park)) %>%
  filter(park %fin% unique(main_data_subset$park))

#calculate mean monthly visits and rec hours per visit
park_month_grid <- park_month_grid %>%
  group_by(park, month) %>%
  summarise(mean_monthly_visits = mean(RecreationVisits,na.rm = T),
            mean_monthly_hrs_per_visit = mean(hours_per_visit, na.rm = T))

#count number of days in each aqi category by month by park
smoke_days_by_park <- main_data_subset %>% 
  group_by(park, year, month, aqi_cat) %>%
  summarise(days = n(), .groups = "drop") %>%
  pivot_wider(names_from = aqi_cat,
              values_from = days,
              values_fill = 0)

#merge in visitation data
smoke_days_by_park <- merge(smoke_days_by_park, park_month_grid, 
                            by = c("park", "month"))

#create number of days in the month variable
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(days_in_month = days_in_month(ymd(paste(year, month, "01", sep = "-"))))

#for park-months with missing days, assume good; calculate avg_visit per day in month to park
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(Good = ifelse((Good + Moderate + `Unhealthy for Sensitive Groups` + Unhealthy + `Very Unhealthy or Hazardous`) < days_in_month,
                       Good + (days_in_month-(Good + Moderate + `Unhealthy for Sensitive Groups` + Unhealthy + `Very Unhealthy or Hazardous`)),
                       Good),
         avg_daily_visit = mean_monthly_visits/days_in_month)

#calibrate effect sizes
usg_effect <- coefs_filtered$effect_pct[coefs_filtered$term == "Unhealthy for Sensitive Groups"]/100
unh_effect <- coefs_filtered$effect_pct[coefs_filtered$term == "Unhealthy"]/100
v_unh_effect <- coefs_filtered$effect_pct[coefs_filtered$term == "Very Unhealthy or Hazardous"]/100

# calculate lost days
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(lost_usg   = -usg_effect   * avg_daily_visit * `Unhealthy for Sensitive Groups`,
         lost_unh   = -unh_effect   * avg_daily_visit * Unhealthy,
         lost_v_unh = -v_unh_effect * avg_daily_visit * `Very Unhealthy or Hazardous`,
         monthly_rec_losses = lost_usg + lost_unh + lost_v_unh)

annual_mean_losses_by_park <- smoke_days_by_park %>%
  group_by(park) %>%
  summarise(mean_annual_rec_losses = sum(monthly_rec_losses)/length(unique(year)))

#calculate smoke affected visit 
#smoke_days_by_park <- smoke_days_by_park %>%
#  mutate(monthly_unh_affected_visits = Unhealthy*avg_daily_visit,
#         monthly_v_unh_haz_affected_visits = `Very Unhealthy or Hazardous`*avg_daily_visit)

#annual_mean_smoke_affected_visits_by_park <- smoke_days_by_park %>%
#  group_by(park) %>%
#  summarise(mean_annual_smoke_affected_visits_unh = sum(monthly_unh_affected_visits)/length(unique(year)),
#            mean_annual_smoke_affected_visits_v_unh_haz = sum(monthly_v_unh_haz_affected_visits)/length(unique(year)))


# load tc model coefficients
get_aqi_coefs <- function(path) {
  co <- coef(readRDS(path))
  co <- co[grepl("^worst_aqi_cat_week", names(co))]
  names(co) <- gsub("\\s", "", sub("^worst_aqi_cat_week", "", names(co)))
  co
}
b_400    <- get_aqi_coefs("./output/tc_reg_results/reg_worst_tc_400miles_sub.RDS")     # main
b_no_sub <- get_aqi_coefs("./output/tc_reg_results/reg_worst_tc_400miles_no_sub.RDS")  # robustness
b_250    <- get_aqi_coefs("./output/tc_reg_results/reg_worst_tc_250miles_no_sub.RDS")  # robustness

# value of lost visits, capped at WTP
forgone_CS <- function(lost_usg, lost_unh, lost_v_unh, cs_day, b) {
  cap <- function(coef) unname(pmax(0, pmin(1, -coef)))
  cs_day * (lost_usg   * cap(b["UnhealthyforSensitiveGroups"]) +
              lost_unh   * cap(b["Unhealthy"]) +
              lost_v_unh * cap(b["VeryUnhealthyorHazardous"]))
}

#### calculate welfare losses from forgone visitation
{
#load cs estimate data 
cs_data <- readRDS("./data/processed_data/cs_estimates.RDS")
cs_data <- cs_data %>%
  dplyr::select(park, CS, CS_no_sub, CS_250) %>%
  rename(CS_per_trip        = CS,
         CS_per_trip_no_sub = CS_no_sub,
         CS_per_trip_250    = CS_250)

#merge into rec loss data
smoke_days_by_park <- merge(smoke_days_by_park, cs_data, by = "park")

#calculate CS per day, assume that 12 hours is a day
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(mean_monthly_days_per_visit = mean_monthly_hrs_per_visit/12,
         mean_monthly_days_per_visit = ifelse(mean_monthly_days_per_visit <= 1, 1, mean_monthly_days_per_visit),
         CS_per_trip_day = CS_per_trip/mean_monthly_days_per_visit)

# calculate cs losses from smoke-affected visits
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(CS_losses = forgone_CS(lost_usg, lost_unh, lost_v_unh, CS_per_trip_day, b_400),
         CS_losses_uncapped = monthly_rec_losses*CS_per_trip_day)
print(paste0("Forgone visitation CS losses total $", round(sum(smoke_days_by_park$CS_losses)), " over 4 years or $", round(sum(smoke_days_by_park$CS_losses))/4, " per year",
             " (uncapped: $", round(sum(smoke_days_by_park$CS_losses_uncapped))/4, " per year)"))
}

#### calculate welfare losses from smoke-affected visits
{
#load data
data <- readRDS("./data/processed_data/travel_cost_dataset.RDS")
Sys.sleep(15)

#filter to within 400 miles - MAIN SPECIFICATION
data <- data %>% 
  filter(travel_distance_m <= 643738)
gc()

## calculate wtp measures
#load cs data
cs_data <- readRDS("./data/processed_data/cs_estimates.RDS")

# calculate wtp measures
cs_data <- cs_data %>%
  mutate(unh_wtp          = -b_400["Unhealthy"]                   * CS,
         v_unh_wtp        = -b_400["VeryUnhealthyorHazardous"]    * CS,
         unh_wtp_no_sub   = -b_no_sub["Unhealthy"]                * CS_no_sub,
         v_unh_wtp_no_sub = -b_no_sub["VeryUnhealthyorHazardous"] * CS_no_sub,
         unh_wtp_250      = -b_250["Unhealthy"]                   * CS_250,
         v_unh_wtp_250    = -b_250["VeryUnhealthyorHazardous"]    * CS_250)

## calculate smoke affected visits

# Build a week-level "worst AQI" classification
week_pm25_data <- data %>%
  dplyr::select(park, date_range_start, worst_aqi_cat_week) %>%
  distinct()

#load visitation data and allocate across weeks consistent with the above smoke data
nps_visit_data <- read.csv("./data/nps_official_data/rec_hours_nps_data.csv")
park_month_grid <- nps_visit_data %>%
  dplyr::select(ParkName, Month, Year, RecreationVisits, RecreationHours) %>%
  rename(park = ParkName,
         year = Year,
         month = Month) %>%
  mutate(hours_per_visit = RecreationHours/RecreationVisits,
         park = gsub(" NP$", " National Park", park),
         park = gsub("NP & PRES$", "National Park", park),
         park = gsub("New River Gorge National Park", "New River Gorge National Park and Preserve", park)) %>%
  filter(park %fin% unique(data$park))

#calculate mean monthly visits 
park_month_grid <- park_month_grid %>%
  group_by(park, month) %>%
  summarise(mean_monthly_visits = mean(RecreationVisits,na.rm = T))

# a 7-day week can straddle at most one month boundary -- split its days
# across the month(s) it touches
week_pm25_data <- week_pm25_data %>%
  mutate(
    date_range_end     = date_range_start + 6,
    last_day_of_month1 = ceiling_date(date_range_start, "month") - days(1),
    days_month1        = as.integer(pmin(date_range_end, last_day_of_month1) - date_range_start) + 1L,
    days_month2        = 7L - days_month1,
    year1  = year(date_range_start),  month1 = month(date_range_start),
    year2  = year(date_range_end),    month2 = month(date_range_end)
  )

# attach each side's mean_monthly_visits, convert to a daily rate using that
# specific year's month length, and sum to "visits this week" 
week_visits <- week_pm25_data %>%
  left_join(park_month_grid, by = c("park", "month1" = "month")) %>%
  rename(mean_monthly_visits_1 = mean_monthly_visits) %>%
  left_join(park_month_grid, by = c("park", "month2" = "month")) %>%
  rename(mean_monthly_visits_2 = mean_monthly_visits) %>%
  mutate(
    avg_daily_visit_1 = mean_monthly_visits_1 / days_in_month(ymd(paste(year1, month1, "01", sep = "-"))),
    avg_daily_visit_2 = mean_monthly_visits_2 / days_in_month(ymd(paste(year2, month2, "01", sep = "-"))),
    week_visits       = days_month1 * avg_daily_visit_1 + days_month2 * avg_daily_visit_2
  )

#calculate smoke-affected visits
week_visits <- week_visits %>%
  mutate(
    unh_affected_visits       = ifelse(worst_aqi_cat_week == "Unhealthy", week_visits, 0),
    v_unh_haz_affected_visits = ifelse(worst_aqi_cat_week == "Very Unhealthy or Hazardous", week_visits, 0)
  )

# annual totals by park
annual_smoke_affected_visits_by_park <- week_visits %>%
  group_by(park, year1) %>%
  summarise(
    annual_unh_affected_visits       = sum(unh_affected_visits),
    annual_v_unh_haz_affected_visits = sum(v_unh_haz_affected_visits),
    .groups = "drop"
  ) %>%
  group_by(park) %>%
  summarise(
    mean_annual_unh_affected_visits       = mean(annual_unh_affected_visits),
    mean_annual_v_unh_haz_affected_visits = mean(annual_v_unh_haz_affected_visits)
  )

# merge in cs data and calculate welfare losses
annual_smoke_affected_visits_by_park <- annual_smoke_affected_visits_by_park %>%
  left_join(cs_data, by = "park") %>%
  mutate(
    annual_unh_CS_losses       = mean_annual_unh_affected_visits       * unh_wtp,
    annual_v_unh_haz_CS_losses = mean_annual_v_unh_haz_affected_visits * v_unh_wtp,
    annual_CS_losses           = annual_unh_CS_losses + annual_v_unh_haz_CS_losses,
    annual_CS_losses_no_sub    = mean_annual_unh_affected_visits * unh_wtp_no_sub +
      mean_annual_v_unh_haz_affected_visits * v_unh_wtp_no_sub,
    annual_CS_losses_250       = mean_annual_unh_affected_visits * unh_wtp_250 +
      mean_annual_v_unh_haz_affected_visits * v_unh_wtp_250
  ) %>%
  dplyr::select(park, mean_annual_unh_affected_visits, mean_annual_v_unh_haz_affected_visits,
                unh_wtp, v_unh_wtp, CS, annual_unh_CS_losses, annual_v_unh_haz_CS_losses,
                annual_CS_losses,
                annual_CS_losses_no_sub, annual_CS_losses_250)   # >>> CHANGED: added robustness columns

print(paste0("Mean annual welfare losses across all 38 parks are $", round(sum(annual_smoke_affected_visits_by_park$annual_CS_losses))))
}

#### create a table for the article
{
# Simplify for table (include AQI category days)
smoke_days_by_park_simp <- smoke_days_by_park %>%
  group_by(park, year) %>%
  summarise(
    annual_CS_losses   = sum(CS_losses, na.rm = TRUE),
    annual_rec_losses  = sum(monthly_rec_losses, na.rm = TRUE),
    annual_visits      = sum(mean_monthly_visits, na.rm = TRUE),
    good_days          = sum(Good, na.rm = TRUE),
    moderate_days      = sum(Moderate, na.rm = TRUE),
    usg_days           = sum(`Unhealthy for Sensitive Groups`, na.rm = TRUE),
    unhealthy_days     = sum(Unhealthy, na.rm = TRUE),
    vunhealthy_days    = sum(`Very Unhealthy or Hazardous`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(park) %>%
  summarise(
    mean_annual_CS_losses   = mean(annual_CS_losses, na.rm = TRUE),
    mean_annual_rec_losses  = mean(annual_rec_losses, na.rm = TRUE) / 1000,
    mean_annual_visits      = mean(annual_visits, na.rm = TRUE) / 1e6,
    mean_good_days          = mean(good_days, na.rm = TRUE),
    mean_moderate_days      = mean(moderate_days, na.rm = TRUE),
    mean_usg_days           = mean(usg_days, na.rm = TRUE),
    mean_unhealthy_days     = mean(unhealthy_days, na.rm = TRUE),
    mean_vunhealthy_days    = mean(vunhealthy_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # >>> CHANGED: bring in smoke-affected visits and their losses, keep only the total
  left_join(annual_smoke_affected_visits_by_park %>%
              dplyr::select(park, mean_annual_unh_affected_visits,
                            mean_annual_v_unh_haz_affected_visits, annual_CS_losses),
            by = "park") %>%
  mutate(smoke_affected_visits = (mean_annual_unh_affected_visits +
                                    mean_annual_v_unh_haz_affected_visits) / 1000,
         total_CS_losses       = (mean_annual_CS_losses + annual_CS_losses) / 1000) %>%   
  # <<< END
  dplyr::select(
    Park = park,
    `Annual visits (millions)`       = mean_annual_visits,
    `Good AQI`                       = mean_good_days,
    `Mod. AQI`                       = mean_moderate_days,
    `USG AQI`                        = mean_usg_days,
    `Unh. AQI`                       = mean_unhealthy_days,
    `V. Unh. or Haz. AQI`            = mean_vunhealthy_days,
    `Recreation losses (thousands)`  = mean_annual_rec_losses,
    # >>> CHANGED
    `Smoke-affected visits (thousands)`     = smoke_affected_visits,
    `Welfare losses ($ thousands per year)` = total_CS_losses
    # <<< END
  ) %>%
  arrange(Park)

# Add total row 
total_row <- smoke_days_by_park_simp %>%
  summarise(
    Park = "Total",
    `Annual visits (millions)`              = sum(`Annual visits (millions)`, na.rm = TRUE),
    `Recreation losses (thousands)`         = sum(`Recreation losses (thousands)`, na.rm = TRUE),
    `Smoke-affected visits (thousands)`     = sum(`Smoke-affected visits (thousands)`, na.rm = TRUE),      
    `Welfare losses ($ thousands per year)` = sum(`Welfare losses ($ thousands per year)`, na.rm = TRUE),  
    `Good AQI`                       = sum(`Good AQI`, na.rm = TRUE),
    `Mod. AQI`                       = sum(`Mod. AQI`, na.rm = TRUE),
    `USG AQI`                        = sum(`USG AQI`, na.rm = TRUE),
    `Unh. AQI`                       = sum(`Unh. AQI`, na.rm = TRUE),
    `V. Unh. or Haz. AQI`            = sum(`V. Unh. or Haz. AQI`, na.rm = TRUE))

smoke_days_by_park_simp <- bind_rows(smoke_days_by_park_simp, total_row)

# Format for display (apply rounding/number/dollar last)

#create helper function that rounds up 
round2 <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  z / 10^digits * posneg
}

smoke_days_by_park_simp <- smoke_days_by_park_simp %>%
  mutate(
    `Annual visits (millions)`              = number(`Annual visits (millions)`, accuracy = 0.1),
    `Good AQI`                              = as.character(round2(`Good AQI`)),
    `Mod. AQI`                              = as.character(round2(`Mod. AQI`)),
    `USG AQI`                               = as.character(round2(`USG AQI`)),
    `Unh. AQI`                              = as.character(round2(`Unh. AQI`)),
    `V. Unh. or Haz. AQI`                   = as.character(round2(`V. Unh. or Haz. AQI`)),
    `Recreation losses (thousands)`         = number(`Recreation losses (thousands)`, accuracy = 0.1),
    `Smoke-affected visits (thousands)`     = number(`Smoke-affected visits (thousands)`, accuracy = 0.1),  
    `Welfare losses ($ thousands per year)` = dollar(`Welfare losses ($ thousands per year)`, accuracy = 1)  
  )

# Build LaTeX table
loss_table <- kbl(
  smoke_days_by_park_simp,
  format = "latex",
  booktabs = TRUE,
  align = c("l","r","r","r","r","r","r","r","r","r"),
  caption = "Mean annual smoke-induced recreation and welfare losses with mean annual AQI days for years 2019-2022",
  linesep = "",
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  ) %>%
  row_spec(
    which(smoke_days_by_park_simp$Park == "Total"),
    bold = TRUE
  )

# Save table to file
writeLines(as.character(loss_table), "./output/tables/CS_loss_table.tex")
}

#### robustness table
{
# forgone visitation losses under each robustness specification
smoke_days_by_park <- smoke_days_by_park %>%
  mutate(CS_per_trip_day_no_sub = CS_per_trip_no_sub / mean_monthly_days_per_visit,
         CS_per_trip_day_250    = CS_per_trip_250    / mean_monthly_days_per_visit,
         CS_losses_no_sub = forgone_CS(lost_usg, lost_unh, lost_v_unh, CS_per_trip_day_no_sub, b_no_sub),
         CS_losses_250    = forgone_CS(lost_usg, lost_unh, lost_v_unh, CS_per_trip_day_250,    b_250))

# park-level: AQI days, mean CS per day, and mean annual forgone losses
robust_table <- smoke_days_by_park %>%
  group_by(park, year) %>%
  summarise(
    usg_days   = sum(`Unhealthy for Sensitive Groups`, na.rm = TRUE),   
    unh_days   = sum(Unhealthy, na.rm = TRUE),                          
    v_unh_days = sum(`Very Unhealthy or Hazardous`, na.rm = TRUE),      
    cs_day_400    = mean(CS_per_trip_day, na.rm = TRUE),
    cs_day_no_sub = mean(CS_per_trip_day_no_sub, na.rm = TRUE),
    cs_day_250    = mean(CS_per_trip_day_250, na.rm = TRUE),
    forgone_400    = sum(CS_losses, na.rm = TRUE),
    forgone_no_sub = sum(CS_losses_no_sub, na.rm = TRUE),
    forgone_250    = sum(CS_losses_250, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(park) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::select(-year) %>%
  # add per-trip WTP (from cs_data) and smoke-affected losses
  left_join(cs_data %>% dplyr::select(park, unh_wtp, unh_wtp_no_sub, unh_wtp_250,
                                      v_unh_wtp, v_unh_wtp_no_sub, v_unh_wtp_250),
            by = "park") %>%
  left_join(annual_smoke_affected_visits_by_park %>%
              dplyr::select(park, annual_CS_losses, annual_CS_losses_no_sub, annual_CS_losses_250),
            by = "park") %>%
  # total welfare losses (forgone + smoke-affected), in $ thousands
  mutate(total_400    = (forgone_400    + annual_CS_losses)        / 1000,
         total_no_sub = (forgone_no_sub + annual_CS_losses_no_sub) / 1000,
         total_250    = (forgone_250    + annual_CS_losses_250)    / 1000) %>%
  dplyr::select(park,
                usg_days, unh_days, v_unh_days,                       
                cs_day_400, cs_day_no_sub, cs_day_250,
                unh_wtp, unh_wtp_no_sub, unh_wtp_250,
                v_unh_wtp, v_unh_wtp_no_sub, v_unh_wtp_250,
                total_400, total_no_sub, total_250) %>%
  arrange(park)

# total row (AQI days and welfare losses)
total_row <- robust_table %>%
  summarise(park = "Total",
            across(c(usg_days, unh_days, v_unh_days,                  
                     total_400, total_no_sub, total_250), ~ sum(.x, na.rm = TRUE)))

robust_table <- bind_rows(robust_table, total_row) %>%
  mutate(across(c(usg_days, unh_days, v_unh_days), ~ as.character(round2(.x))),   
         across(c(starts_with("cs_day"), contains("wtp"), starts_with("total")),
                ~ dollar(.x, accuracy = 1)),
         across(-park, ~ ifelse(is.na(.x), "-", .x)))

# build LaTeX table with grouped headers
spec_names <- c("400 mi", "400 mi, no subst.", "250 mi, no subst.")

robust_loss_table <- kbl(
  robust_table,
  format    = "latex",
  booktabs  = TRUE,
  col.names = c("Park", "USG", "Unh.", "V. Unh. or Haz.", rep(spec_names, 4)),   
  align     = c("l", rep("r", 15)),                                             
  caption   = "Mean annual AQI days, consumer surplus, willingness to pay to avoid smoke, and mean annual welfare losses by park under the main specification (400 mile cutoff, inclusive of substitute park AQI) and robustness specifications omitting substitute park AQI and using a 250 mile cutoff",
  linesep   = ""
) %>%
  add_header_above(c(" " = 1,
                     "AQI days per year" = 3,                                    
                     "Mean CS per day ($)" = 3,
                     "WTP, Unhealthy ($ per trip)" = 3,
                     "WTP, V. Unh. or Haz. ($ per trip)" = 3,
                     "Welfare losses ($ thousands per year)" = 3)) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8) %>%  
  row_spec(nrow(robust_table), bold = TRUE)

# wrap in landscape (needs \usepackage{pdflscape}) and save
writeLines(paste0("\\begin{landscape}\n", as.character(robust_loss_table), "\n\\end{landscape}\n"),
           "./output/tables/CS_loss_table_robust.tex")
}





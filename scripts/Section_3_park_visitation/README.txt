This folder contains all scripts used to create main park visits dataset and conduct analysis for Section 2 of the manuscript. The scripts are numbered in the order they must be run.

File contents:

-'01_process_NPS_shp.R': This script reads in NPS' official shapefile of park locations and isolates NPS National Parks in CONUS.
-'02_dewey_advan_api_call.R': This script interacts with Dewey's API using the 'deweydatar' package to download all Advan visitation data for years 2019-2022
-'03_advan_placekey_filter.R': this script filters the raw weekly advan data, and filters to isolate national park visitation. It saves a weekly and daily version of the nps data.
-'04_filter_GH_PM25_data.R': this script reads in GH PM2.5 data from Wei et al. 2023, scrops to CONUS for use in dataset creation
-'05_fire_data_scrape.R': This script scrapes HMS fire data (series of shapefiles) and copies them to main data folder
-'06_fire_df_process.R': This script merges all HMS fire points into one data.table
-'07_fire_cluster_process.R': This script creates fire clusters from the hms fire points following Burke et al. 2021
-'08_filtered_fire_process.R': This script filters HMS fire points by intersecting with MTBS fire perimeters
-'09_polygon_data_process.R': this script takes polygons and attributes all pm2.5, fire points, and weather to the given polygons (in our case, NPS parks)
-'10_final_variable_process.R': this script makes final, quick variables and filters the main dataset to include parks filtered from 'data_summary_stats.R'
-'11_weekly_data_process.R': this script processes daily data up to the weekly level for weekly-level regressions
-'12_regressions.R': This script runs regressions for the effect of pm2.5 on net visitation alongside several robustness checks and additional analysis
-'13_lag_daily_regressions.R': This script runs regressions for lagged daily regressions of the effect of smoke-attributable pm2.5 on park visitation
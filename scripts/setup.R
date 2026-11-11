#-------------------------------------------------------------------------------
# Purpose: This script loads the packages used in the other main scripts and
# creates a few default settings
# 
#
#
#-------------------------------------------------------------------------------

# Clear console:
cat('\014')   

# Clear environment:
rm(list = setdiff(ls(), lsf.str())) 

# Clear all plots:
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

#set max print for output
options(max.print=10000)

#library packages
#install.packages('pacman')
pacman::p_load('readr', 'lubridate', 'httr', 'dplyr', 'sf', 'mapview',
               'rvest', 'RSelenium', 'tidyverse', 'data.table', 'pacman',
               'furrr', 'future', 'pbapply', 'progress', 'RANN',
               'sandwich', 'ivreg', 'raster', 'slider', 
               'parallel', 'purrr', 'data.table','gtools', 'margins',
               'lmtest', 'tableone', 'rayshader', 'ggplot2', 'lwgeom',
               'gstat', 'ncdf4', 'circular', 'stringr', 'wdman', 'devtools',
               'R.utils', 'prism', 'terra', 'abind', 'gridExtra', 'tidycensus',
               'leaflet', 'future.apply', 'jsonlite', 'rnaturalearth',
                'collapse', 'osrm', 'timeDate', 'broom', 'fixest', 'gt','scales',
               'kableExtra', 'gtrendsR', 'curl', 'cowplot', 'progressr', 'patchwork',
               install=F)

#remotes::install_github("Dewey-Data/deweydatar")
library(deweydatar)

#use S2 in sf package
sf::sf_use_s2(TRUE)

#avoid scientific notation in outputs
options(scipen = 999)

#create fast %in% function to speed things up
library(fastmatch)
`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}

#define mode function
get_mode <- function(x) {
  x <- na.omit(x)                 # drop NAs
  if (length(x) == 0) return(NA)  # handle all-NA cases
  tab <- table(x)
  names(tab)[which.max(tab)]
}

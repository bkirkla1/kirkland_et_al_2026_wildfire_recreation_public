
#load relevant packages
source('scripts/setup.R')

#load main daily data
spend_data_daily <- readRDS("./data/processed_data/main_spend_data_daily.RDS")
spend_data_daily_reduc <- spend_data_daily %>% filter(gateway_dummy_gtrends == 1,
                                                      !is.na(ln_spending))

#filter to new sample of western parks
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
spend_data_daily_reduc <- spend_data_daily_reduc %>%
  filter(!park %in% eastern_parks)

#load places spatial data

#Get list of states (to iterate over TIGER place geometries)
state_list <- unique(fips_codes$state)

#  Download TIGER place boundaries by state and combine
safe_places_download <- function(st, attempts = 3) {
  cache_file <- file.path(tempdir(), paste0("tl_2023_", st, "_place.gpkg"))
  
  # Return cached version if exists
  if (file.exists(cache_file)) {
    message("Using cached ", st)
    return(st_read(cache_file, quiet = TRUE))
  }
  
  for (i in 1:attempts) {
    message("Downloading ", st, " (try ", i, ")")
    try({
      pl <- tigris::places(state = st, year = 2023, cb = FALSE)
      st_write(pl, cache_file, quiet = TRUE, delete_dsn = TRUE)
      message("Success: ", st)
      return(pl)
    }, silent = TRUE)
    Sys.sleep(3)
  }
  message("Skipping ", st, " — failed after ", attempts, " attempts.")
  Sys.sleep(2)
  return(NULL)
}

us_places_list <- lapply(state_list, safe_places_download)

#filter main data to isolate unique placekey, lat-lon pairs
unq_placekeys <- spend_data_daily_reduc %>%
  dplyr::select(placekey, longitude, latitude, park) %>%
  distinct() 

#convert to sf object
unq_placekeys <- st_as_sf(unq_placekeys,
                          coords = c("longitude", "latitude"))
st_crs(unq_placekeys) <- st_crs(4326)

#combine all US places
us_places <- do.call(rbind, us_places_list)
us_places <- st_transform(us_places, crs = st_crs(unq_placekeys))

#spatial join placekeys and places
placekey_with_place <- st_join(
  unq_placekeys,
  us_places,
  join = st_within
)

#buffer the subset of places found and then rejoin
us_places_buff <- us_places %>%
  filter(GEOIDFQ %in% placekey_with_place$GEOIDFQ) %>%
  st_buffer(., dist = 1000)

placekey_with_place <- st_join(
  unq_placekeys,
  us_places_buff,
  join = st_within
)
#some placekey-park combinations have NA park (caused by missing timeline data back in other processing)
#each placekey matches with at least on park


#simplify
placekey_with_place <- placekey_with_place %>%
  dplyr::select(placekey, park, GEOIDFQ, NAME) %>%
  filter(!is.na(park))

#find unique park-gw community combos
unq_parks_gwc = placekey_with_place %>%
  st_drop_geometry() %>%
  dplyr::select(park, GEOIDFQ, NAME) %>%
  distinct() %>%
  filter(if_all(everything(), ~ !is.na(.)))

#load nps park shapefile
nps_data <- readRDS("./data/processed_data/nps_data.RDS")
nps_data <- nps_data %>%
  filter(UNIT_NAME %in% unique(spend_data_daily_reduc$park)) %>%
  arrange(UNIT_NAME)

#load state shapefile
states <- tigris::states(cb = TRUE, year = 2023) %>%
  st_transform(st_crs(us_places))  # match your points/parks CRS

#loop and create maps
plots_list <- vector("list", length = nrow(nps_data))

for (i in 1:nrow(nps_data)) {
  
  # get park data
  park_i <- nps_data[i, ]
  
  # get gateway communities for this park
  places_i <- unq_parks_gwc %>%
    filter(park == park_i$UNIT_NAME) %>%
    left_join(., us_places, by = c("GEOIDFQ", "NAME")) %>%
    dplyr::select(NAME, geometry) %>%
    st_as_sf()
  
  # ensure CRS match
  park_i <- st_transform(park_i, st_crs(states))
  if(nrow(places_i) > 0) places_i <- st_transform(places_i, st_crs(states))
  
  # subset states intersecting park + communities
  combined_geom <- if(nrow(places_i) > 0) st_union(park_i, places_i) else park_i
  states_sub <- st_filter(states, combined_geom, .predicate = st_intersects)
  
  # compute bounding box for zoom with enforced aspect ratio
  bbox <- st_bbox(combined_geom)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  
  target_ratio <- 4/3  # width:height
  
  if ((x_range / y_range) < target_ratio) {
    x_pad <- (y_range * target_ratio - x_range) / 2
    y_pad <- y_range * 0.1
  } else {
    y_pad <- (x_range / target_ratio - y_range) / 2
    x_pad <- x_range * 0.1
  }
  
  xlim <- c(bbox$xmin - x_pad, bbox$xmax + x_pad)
  ylim <- c(bbox$ymin - y_pad, bbox$ymax + y_pad)
  # bounding box polygon 
  bbox_poly <- st_polygon(list(
    matrix(c(
      xlim[1], ylim[1],  # bottom-left
      xlim[2], ylim[1],  # bottom-right
      xlim[2], ylim[2],  # top-right
      xlim[1], ylim[2],  # top-left
      xlim[1], ylim[1]   # close polygon
    ), ncol = 2, byrow = TRUE)
  )) %>%
    st_sfc(crs = st_crs(states))
  
  # get counties intersecting bounding box
  counties_sub <- tigris::counties(state = unique(states_sub$STUSPS)) %>%
    st_transform(st_crs(states)) %>%
    st_filter(bbox_poly, .predicate = st_intersects)
  
  # get roads for each county and filter by bounding box
  roads <- lapply(1:nrow(counties_sub), function(j) {
    tigris::roads(state = counties_sub$STATEFP[j],
                  county = counties_sub$COUNTYFP[j]) %>%
      filter(MTFCC %in% c("S1100", "S1200"))  # primary + secondary roads
  }) %>% do.call(rbind, .) %>%
    st_transform(st_crs(states)) %>%
    st_filter(bbox_poly, .predicate = st_intersects) %>%
    filter(FULLNAME != "")
  
  # assign layer types for colors
  park_i$layer_type <- "National Park"
  if(nrow(places_i) > 0) places_i$layer_type <- "Gateway Community"
  roads$layer_type <- "Major Road"
  
  # create ggplot
  p <- ggplot() +
    # state outlines
    geom_sf(data = states_sub, fill = NA, color = "gray70", size = 0.5) +
    
    # roads
    geom_sf(data = roads, aes(color = layer_type), size = 0.3, linetype = "solid") +
    
    # park and communities
    geom_sf(data = park_i, aes(fill = layer_type), color = "black", alpha = 0.6) +
    {if(nrow(places_i) > 0) geom_sf(data = places_i, aes(fill = layer_type), color = "#FFC857", alpha = 0.6)} +
    
    # labels
    geom_sf_text(data = park_i, aes(label = UNIT_NAME), size = 2, color = "black") +
    {if(nrow(places_i) > 0) geom_sf_text(data = places_i, aes(label = NAME), size = 2, color = "black", check_overlap = TRUE)} +
    
    # bounding box (black rectangle)
    geom_sf(data = bbox_poly, fill = NA, color = "black", size = 0.8) +
    
    # zoom
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    # manual scales with no legend titles
    scale_fill_manual(values = c("National Park" = "#4CAF80",
                                 "Gateway Community" = "#FFC857"),
                      guide = guide_legend(title = NULL)) +
    scale_color_manual(values = c("Major Road" = "gray50"),
                       guide = guide_legend(title = NULL)) +
    
    # theme
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
      legend.position = "right"
    ) +
    
    # plot title
    ggtitle(paste0("Gateway communities near ", park_i$UNIT_NAME))
  
  # store plot
  plots_list[[i]] <- p
}

#create pdf


# number of plots per page
plots_per_page <- 2

# total pages
n_pages <- ceiling(length(plots_list) / plots_per_page)

# output PDF
pdf("./output/gwc_by_park_maps.pdf", width = 8.5, height = 11)

for (page in 1:n_pages) {
  start_idx <- (page - 1) * plots_per_page + 1
  end_idx   <- min(page * plots_per_page, length(plots_list))
  page_plots <- plots_list[start_idx:end_idx]
  
  while (length(page_plots) < plots_per_page) {
    page_plots <- c(page_plots, list(NULL))
  }
  
  grid_page <- cowplot::plot_grid(
    page_plots[[1]],
    page_plots[[2]],
    ncol = 1,
    nrow = 2,
    align = "hv"
  )
  
  # Wrap in ggdraw to enforce 1-inch margins on an 8.5x11 page
  # 1 inch / 8.5 inches ≈ 0.118 (left/right), 1 inch / 11 inches ≈ 0.091 (top/bottom)
  margined_page <- cowplot::ggdraw() +
    cowplot::draw_plot(
      grid_page,
      x      = 1/8.5,   # left margin: 1 inch
      y      = 1/11,     # bottom margin: 1 inch
      width  = 6.5/8.5,  # 8.5 - 2 inches of margins
      height = 9/11      # 11 - 2 inches of margins
    )
  
  print(margined_page)
}

dev.off()

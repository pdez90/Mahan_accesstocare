## =============================================================================
## 20_access_measures_map.R — Supplementary map of the village-level access-to-
## care measures across the 93 study villages. Each panel shows one measure at
## the village centroids (median household GPS), standardised so that higher
## values denote worse access, on a common colour scale. Needs the GISproject
## GPS files and the outputs of scripts 08 (composite index) and 19 (Weiss
## travel time). Degrades gracefully if the GPS files are absent.
## Writes: output/20_access_measures_map.png
## =============================================================================
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")
suppressPackageStartupMessages({library(ggplot2); library(sf)})

GIS    <- Sys.getenv("RSV_GIS_DIR", "/Users/priyanka/Downloads/RSV_India/GISproject/GISProject/")
HH_GPS <- file.path(GIS, "capstone project Mattie/critical_data_gpsdata.csv")

if (!file.exists(HH_GPS)) {
  message("20: GPS data not found — skipping access-measures map (set RSV_GIS_DIR).")
} else {
  num  <- function(x) suppressWarnings(as.numeric(
    ifelse(trimws(x) %in% c("", "50 mtr"), ifelse(trimws(x) == "50 mtr", "0.05", NA), x)))
  bin1 <- function(x) as.integer(startsWith(trimws(toupper(as.character(x))), "1"))
  in_box <- function(d) d[!is.na(d$latitude) & !is.na(d$longitude) &
                          d$latitude > 20.5 & d$latitude < 22.5 &
                          d$longitude > 76 & d$longitude < 78, ]
  hav <- function(lo, la, lo2, la2) { R <- 6371
    dlo <- (lo2 - lo) * pi/180; dla <- (la2 - la) * pi/180
    a <- sin(dla/2)^2 + cos(la*pi/180) * cos(la2*pi/180) * sin(dlo/2)^2
    2 * R * asin(pmin(1, sqrt(a))) }
  mh <- data.frame(lon = 76.959, lat = 21.543)   # MAHAN referral hospital

  ## village centroids
  hh <- in_box(read_csv(HH_GPS, show_col_types = FALSE))
  hh$vcode <- as.numeric(sub("^([0-9]+).*", "\\1", hh$code_village))
  cent <- hh %>% group_by(vcode) %>%
    summarise(lon = median(longitude), lat = median(latitude), .groups = "drop") %>%
    filter(vcode %in% unique(ari$vcode))
  cent$dist_mahan <- hav(cent$lon, cent$lat, mh$lon, mh$lat)

  ## measures
  vm <- vil %>% transmute(vcode, dist_phc = num(distance_primary_center_km),
                          no_bus = 1 - bin1(bus_village))
  va <- tryCatch(read_csv(file.path(OUT_DIR, "08_village_access_index.csv"),
                          show_col_types = FALSE) %>% select(vcode, access_barrier_z),
                 error = function(e) NULL)
  tt <- tryCatch(read_csv(file.path(OUT_DIR, "19_village_travel_time.csv"),
                          show_col_types = FALSE) %>% select(vcode, motorized_min, walking_min),
                 error = function(e) NULL)

  d <- cent %>% left_join(vm, by = "vcode")
  if (!is.null(va)) d <- d %>% left_join(va, by = "vcode")
  if (!is.null(tt)) d <- d %>% left_join(tt, by = "vcode")

  meas <- c("Distance to primary health centre"      = "dist_phc",
            "GIS distance to MAHAN hospital"          = "dist_mahan",
            "No scheduled bus service"                = "no_bus",
            "Composite access-barrier index"          = "access_barrier_z",
            "Motorized travel time to healthcare"     = "motorized_min",
            "Walking travel time to healthcare"       = "walking_min")
  meas <- meas[meas %in% names(d)]

  long <- do.call(rbind, lapply(names(meas), function(lab) {
    z <- as.numeric(scale(d[[meas[[lab]]]]))
    data.frame(lon = d$lon, lat = d$lat, measure = lab, z = z)
  }))
  long$measure <- factor(long$measure, levels = names(meas))

  ## optional basemap (taluka boundaries)
  BBOX <- c(xmin = 76.30, ymin = 21.05, xmax = 77.25, ymax = 21.66)
  read_crop <- function(f) { if (!file.exists(f)) return(NULL)
    tryCatch({ x <- sf::st_make_valid(sf::st_read(f, quiet = TRUE))
      if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
      suppressWarnings(sf::st_crop(x, BBOX)) }, error = function(e) NULL) }
  adm  <- read_crop(file.path(GIS, "gpsdata/DataER/IND_adm3.shp"))
  base <- if (!is.null(adm)) geom_sf(data = adm, fill = "grey96", colour = "grey80", linewidth = 0.25) else NULL

  p <- ggplot() + base +
    geom_point(data = long, aes(lon, lat, colour = z), size = 1.7, alpha = 0.9) +
    geom_point(data = mh, aes(lon, lat), shape = 8, size = 2.4, stroke = 1, colour = "black") +
    scale_colour_viridis_c(option = "C", direction = -1, name = "Worse access\n(standardised)") +
    coord_sf(xlim = c(BBOX["xmin"], BBOX["xmax"]), ylim = c(BBOX["ymin"], BBOX["ymax"]), expand = FALSE) +
    facet_wrap(~measure, ncol = 3) +
    labs(title = "Village-level access-to-care measures across the 93 study villages",
         subtitle = "Each point is a village at its median household GPS; colour is the standardised measure (higher = worse access). Star = MAHAN referral hospital.") +
    theme_void(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 8.5),
          legend.position = "right", plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 8), plot.margin = margin(6,6,6,6),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(OUT_DIR, "20_access_measures_map.png"), p, width = 9.5, height = 6.5, dpi = 200, bg = "white")

  hdr("20 — ACCESS-TO-CARE MEASURES MAP")
  cat("Mapped measures:", paste(names(meas), collapse = " | "), "\n")
  cat("Wrote 20_access_measures_map.png\n")
}

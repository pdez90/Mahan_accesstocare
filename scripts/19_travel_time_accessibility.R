## =============================================================================
## 19_travel_time_accessibility.R — external accessibility validation using the
## Malaria Atlas Project / Weiss et al. 2020 global travel-time-to-healthcare
## surfaces (motorized and walking, ~1 km; https://malariaatlas.org). Extracts
## travel time at each village centroid, describes the study's spatial scale, and
## compares this independent metric with our survey-based composite access-barrier
## index and with the two outcomes.
##
## Requires the `terra` package (install.packages("terra")) and the zipped MAP
## rasters in ACCESS_DIR. Reads the GeoTIFFs directly inside the zips via GDAL's
## /vsizip/, so they need not be unzipped. Degrades gracefully if anything is
## missing. Writes: output/19_village_travel_time.csv, output/19_accessibility_compare.csv
## =============================================================================
if (!exists("hdr")) source("00_setup.R")
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")

if (!requireNamespace("terra", quietly = TRUE)) {
  message("19: package 'terra' not installed — skipping. Run install.packages('terra').")
} else {
  suppressPackageStartupMessages(library(terra))

  ACCESS_DIR <- Sys.getenv("RSV_ACCESS_DIR",
    "/Users/priyanka/Downloads/RSV_India/Access_to_care_paper/accessibility")
  GIS <- Sys.getenv("RSV_GIS_DIR",
    "/Users/priyanka/Downloads/RSV_India/GISproject/GISProject/")
  hh_path <- file.path(GIS, "capstone project Mattie/critical_data_gpsdata.csv")

  ## ---- village centroids (median household GPS), identical to the maps -------
  inbox <- function(d) d[!is.na(d$latitude) & !is.na(d$longitude) &
                         d$latitude > 20.5 & d$latitude < 22.5 &
                         d$longitude > 76 & d$longitude < 78, ]
  hh <- inbox(readr::read_csv(hh_path, show_col_types = FALSE))
  hh$vcode <- as.numeric(sub("^([0-9]+).*", "\\1", hh$code_village))
  cent <- hh %>% group_by(vcode) %>%
    summarise(lon = median(longitude), lat = median(latitude), .groups = "drop") %>%
    filter(vcode %in% unique(ari$vcode))
  pts <- terra::vect(as.data.frame(cent), geom = c("lon", "lat"), crs = "EPSG:4326")

  ## ---- open a GeoTIFF from inside a MAP zip via /vsizip/ ---------------------
  open_zip_tif <- function(zip) {
    if (!file.exists(zip)) return(NULL)
    inner <- tryCatch(utils::unzip(zip, list = TRUE)$Name, error = function(e) character(0))
    tif <- inner[grepl("\\.tif?f?$|\\.geotif?f?$", inner, ignore.case = TRUE)]
    if (!length(tif)) return(NULL)
    tryCatch(terra::rast(file.path("/vsizip", zip, tif[1])), error = function(e) NULL)
  }
  rasters <- list(
    motorized_min = "2020_motorized_travel_time_to_healthcare.zip",
    walking_min   = "2020_walking_only_travel_time_to_healthcare.zip",
    city_min      = "2015_accessibility_to_cities_v1.0.zip")

  tt <- as.data.frame(cent)
  for (nm in names(rasters)) {
    r <- open_zip_tif(file.path(ACCESS_DIR, rasters[[nm]]))
    if (is.null(r)) { message("19: could not open ", rasters[[nm]]); next }
    tt[[nm]] <- terra::extract(r, pts)[, 2]
  }
  readr::write_csv(tt, file.path(OUT_DIR, "19_village_travel_time.csv"))

  hdr("19 — EXTERNAL ACCESSIBILITY (Weiss et al. 2020, travel time to healthcare)")
  ## ---- spatial scale of the study -------------------------------------------
  span_ns <- (max(cent$lat) - min(cent$lat)) * 111
  span_ew <- (max(cent$lon) - min(cent$lon)) * 111 * cos(mean(cent$lat) * pi / 180)
  cat(sprintf("Spatial scale: %d villages; extent lon %.3f–%.3f, lat %.3f–%.3f.\n",
              nrow(cent), min(cent$lon), max(cent$lon), min(cent$lat), max(cent$lat)))
  cat(sprintf("Bounding box ~%.0f km (N–S) x %.0f km (E–W); accessibility raster resolution ~1 km.\n\n",
              span_ns, span_ew))
  for (nm in names(rasters)) {
    if (is.null(tt[[nm]]) || all(is.na(tt[[nm]]))) next
    v <- tt[[nm]]
    cat(sprintf("%-14s: median %.0f min [IQR %.0f–%.0f], range %.0f–%.0f\n", nm,
                median(v, na.rm = TRUE), quantile(v, .25, na.rm = TRUE),
                quantile(v, .75, na.rm = TRUE), min(v, na.rm = TRUE), max(v, na.rm = TRUE)))
  }

  ## ---- compare with our composite index and the two outcomes ----------------
  va <- readr::read_csv(file.path(OUT_DIR, "08_village_access_index.csv"), show_col_types = FALSE)
  vr <- ari %>% filter(sev_vs) %>% group_by(vcode) %>%
    summarise(pct_reached = 100 * mean(reached_facility), .groups = "drop")
  vd <- deaths %>% group_by(vcode) %>%
    summarise(pct_comm = 100 * mean(community_type == 1), .groups = "drop")
  cmp <- tt %>% left_join(va %>% select(vcode, access_barrier_z), by = "vcode") %>%
    left_join(vr, by = "vcode") %>% left_join(vd, by = "vcode")

  sp <- function(a, b) {
    ok <- complete.cases(cmp[[a]], cmp[[b]])
    if (sum(ok) < 4) return(sprintf("n=%d (too few)", sum(ok)))
    ct <- suppressWarnings(cor.test(cmp[[a]][ok], cmp[[b]][ok], method = "spearman"))
    sprintf("rho=%+.2f, p=%.3f, n=%d", unname(ct$estimate), ct$p.value, sum(ok))
  }
  for (nm in names(rasters)) {
    if (is.null(cmp[[nm]]) || all(is.na(cmp[[nm]]))) next
    cat(sprintf("\n%s vs:\n", nm))
    cat("  composite access-barrier index :", sp(nm, "access_barrier_z"), "\n")
    cat("  % reaching facility            :", sp(nm, "pct_reached"), "\n")
    cat("  % community death              :", sp(nm, "pct_comm"), "\n")
  }
  readr::write_csv(cmp, file.path(OUT_DIR, "19_accessibility_compare.csv"))
  cat("\nWrote 19_village_travel_time.csv and 19_accessibility_compare.csv\n")
}

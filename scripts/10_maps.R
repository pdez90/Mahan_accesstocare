## =============================================================================
## 10_maps.R — spatial maps of access to care
## Figure 3 — the 93 study villages, coloured by % of severe/very-severe
##            episodes reaching a facility (MAHAN referral hospital marked)
## Figure 4 — the 205 LRTI deaths at household GPS, coloured by where the child
##            died (village / in transit / hospital)
##
## Uses the GPS data in the GISproject folder. Paths default to Priyanka's Mac;
## override with the environment variables below.
## Writes: output/fig3_village_access_map.png, output/fig4_death_location_map.png
## =============================================================================
if (!exists("ari")) source("03_derive_and_merge.R")
suppressPackageStartupMessages({library(ggplot2); library(sf)})

GIS <- Sys.getenv("RSV_GIS_DIR",
  "/Users/priyanka/Downloads/RSV_India/GISproject/GISProject/")
HH_GPS    <- file.path(GIS, "capstone project Mattie/critical_data_gpsdata.csv")
DEATH_GPS <- file.path(GIS, "capstone project Mattie/critical_data_deaths_gpsdata.csv")
MAHAN_SHP <- file.path(GIS, "gpsdata/DataER/MAHAN.shp")

## skip gracefully if the GIS/GPS files are not available on this machine
if (!file.exists(HH_GPS)) {
  message("10_maps.R: GPS data not found at ", HH_GPS,
          " — skipping maps. Set RSV_GIS_DIR to the GISproject folder to enable.")
} else {

in_box <- function(d) d[!is.na(d$latitude) & !is.na(d$longitude) &
                        d$latitude  > 20.5 & d$latitude  < 22.5 &
                        d$longitude > 76.0 & d$longitude < 78.0, ]

## MAHAN referral hospital
mahan <- st_read(MAHAN_SHP, quiet = TRUE)
mh <- data.frame(lon = st_coordinates(mahan)[1,1], lat = st_coordinates(mahan)[1,2])

## ---- basemap (taluka boundaries, roads, rivers) cropped to the study area --
BBOX <- c(xmin = 76.30, ymin = 21.05, xmax = 77.25, ymax = 21.66)
read_crop <- function(f) {
  if (!file.exists(f)) return(NULL)
  tryCatch({
    x <- sf::st_make_valid(sf::st_read(f, quiet = TRUE))
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
    suppressWarnings(sf::st_crop(x, BBOX))
  }, error = function(e) NULL)
}
adm   <- read_crop(file.path(GIS, "gpsdata/DataER/IND_adm3.shp"))        # talukas
roads <- read_crop(file.path(GIS, "gpsdata/DataER/IND_roads.shp"))
water <- read_crop(file.path(GIS, "gpsdata/DataER/IND_water_lines_dcw.shp"))
basemap <- list(
  if (!is.null(adm))   geom_sf(data = adm,   fill = "grey96", colour = "grey78", linewidth = 0.3),
  if (!is.null(water)) geom_sf(data = water, colour = "#9ecae1", linewidth = 0.35),
  if (!is.null(roads)) geom_sf(data = roads, colour = "grey65", linewidth = 0.28)
)
map_coord <- coord_sf(xlim = c(BBOX["xmin"], BBOX["xmax"]),
                      ylim = c(BBOX["ymin"], BBOX["ymax"]), expand = FALSE)
cat("basemap layers loaded — taluka:", !is.null(adm),
    "roads:", !is.null(roads), "rivers:", !is.null(water), "\n")

## ---- village centroids from household GPS ----------------------------------
hh <- in_box(read_csv(HH_GPS, show_col_types = FALSE))
hh$vcode <- as.numeric(sub("^([0-9]+).*", "\\1", hh$code_village))
cent <- hh %>% group_by(vcode) %>%
  summarise(lon = median(longitude), lat = median(latitude),
            n_hh = n(), .groups = "drop")

## village outcomes (from the analytic data)
vr <- ari %>% filter(sev_vs) %>% group_by(vcode) %>%
  summarise(n_sv = n(), pct_reached = 100*mean(reached_facility), .groups="drop")
vmap <- cent %>% inner_join(vr, by = "vcode")

hdr("10 — SPATIAL MAPS")
cat("Villages mapped:", nrow(vmap), "| MAHAN hospital at",
    round(mh$lon,3), round(mh$lat,3), "\n")

theme_map <- theme_void(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13, hjust=0),
        legend.position = "right", plot.margin = margin(8,8,8,8),
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA))

## ---- Figure 3: village access map ------------------------------------------
p3 <- ggplot() + basemap +
  geom_point(data = vmap, aes(lon, lat, colour = pct_reached, size = n_sv), alpha = 0.9) +
  geom_point(data = mh, aes(lon, lat), shape = 8, size = 4, stroke = 1.2, colour = "black") +
  geom_text(data = mh, aes(lon, lat, label = "  MAHAN hospital"),
            hjust = 0, size = 3.4, fontface = "bold") +
  scale_colour_viridis_c(option = "C", direction = -1, name = "% reaching\na facility") +
  scale_size_area(max_size = 8, name = "Severe\nepisodes") +
  map_coord +
  labs(title = "Facility contact for severe LRTI, by village",
       caption = "Point = village centroid (median household GPS). Star = MAHAN referral hospital.") +
  theme_map
ggsave(file.path(OUT_DIR, "fig3_village_access_map.png"), p3, width = 7.4, height = 6, dpi = 300, bg = "white")

## ---- Figure 4: death-location map ------------------------------------------
dg <- in_box(read_csv(DEATH_GPS, show_col_types = FALSE))
## the GPS file has its own death_location column; use the clean one from the
## analytic deaths file, under a distinct name to avoid a join clash.
dloc <- deaths %>% select(studyid, dl = death_location)
dmap <- dg %>% select(studyid, lat = latitude, lon = longitude) %>%
  inner_join(dloc, by = "studyid") %>%
  mutate(where = factor(dl,
           levels = c("village","on way","hospital"),
           labels = c("Village (community)","In transit","Hospital")))

cat("LRTI deaths mapped:", sum(!is.na(dmap$where)), "of 205\n")

DCOL <- c("Village (community)"="#D55E00","In transit"="#E69F00","Hospital"="#0072B2")
p4 <- ggplot() + basemap +
  geom_point(data = vmap, aes(lon, lat), colour = "grey65", size = 0.7) +
  geom_point(data = dmap, aes(lon, lat, colour = where), size = 2.2, alpha = 0.85) +
  geom_point(data = mh, aes(lon, lat), shape = 8, size = 4, stroke = 1.2, colour = "black") +
  geom_text(data = mh, aes(lon, lat, label = "  MAHAN hospital"),
            hjust = 0, size = 3.4, fontface = "bold") +
  scale_colour_manual(values = DCOL, name = "Place of death") +
  map_coord +
  labs(title = "Where children with fatal LRTI died",
       caption = "Each point = one LRTI death at the household location. Grey = village centroids. Star = MAHAN hospital.") +
  theme_map
ggsave(file.path(OUT_DIR, "fig4_death_location_map.png"), p4, width = 7.4, height = 6, dpi = 300, bg = "white")

## village map table for reference
write_csv(vmap, file.path(OUT_DIR, "10_village_coords_outcomes.csv"))
cat("\nWrote Figures 3-4 to", OUT_DIR, "\n")

## ---- spatial autocorrelation (Moran's I, inverse-distance weights) ---------
## Tests whether village outcomes cluster geographically.
moran_test <- function(x, lon, lat, seed_perm = 999) {
  ok <- !is.na(x); x <- x[ok]; lon <- lon[ok]; lat <- lat[ok]; n <- length(x)
  R <- 6371; rad <- pi/180; d <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) if (i != j) {
    dl <- (lat[j]-lat[i])*rad; dn <- (lon[j]-lon[i])*rad*cos(lat[i]*rad)
    d[i, j] <- R*sqrt(dl^2 + dn^2)
  }
  W <- 1/d; diag(W) <- 0; W[!is.finite(W)] <- 0
  z <- x - mean(x); S0 <- sum(W)
  I <- (n/S0)*(sum(W*outer(z, z))/sum(z^2)); EI <- -1/(n-1)
  set.seed(1)
  perm <- replicate(seed_perm, { zp <- sample(z)
    (n/S0)*(sum(W*outer(zp, zp))/sum(zp^2)) })
  p <- (1 + sum(abs(perm - EI) >= abs(I - EI))) / (seed_perm + 1)
  data.frame(outcome = NA, moran_I = round(I,3), expected = round(EI,3),
             p_perm = round(p,3), n_villages = n)
}
vcomm <- cent %>% inner_join(
  deaths %>% group_by(vcode) %>% summarise(pct_comm = 100*mean(community_type==1), .groups="drop"),
  by = "vcode")
mi <- rbind(
  cbind(outcome = "pct_reached_facility", moran_test(vmap$pct_reached, vmap$lon, vmap$lat)[-1]),
  cbind(outcome = "pct_community_death",  moran_test(vcomm$pct_comm,  vcomm$lon, vcomm$lat)[-1]))
hdr("10b — SPATIAL AUTOCORRELATION (Moran's I)")
print(mi)
write_csv(mi, file.path(OUT_DIR, "10_moran_I.csv"))

## ---- 10c: GIS-derived straight-line distance to MAHAN referral hospital -----
## Objective distance (vs. the self-reported PHC distance in the village survey).
hav <- function(lon, lat, lon2 = mh$lon, lat2 = mh$lat) {
  R <- 6371; r <- pi/180; dlon <- (lon2-lon)*r; dlat <- (lat2-lat)*r
  a <- sin(dlat/2)^2 + cos(lat*r)*cos(lat2*r)*sin(dlon/2)^2; 2*R*asin(sqrt(a))
}
gis <- vmap %>% mutate(dist_mahan_km = hav(lon, lat))
rho <- cor(gis$dist_mahan_km, gis$pct_reached, method = "spearman")
pv  <- suppressWarnings(cor.test(gis$dist_mahan_km, gis$pct_reached, method = "spearman"))$p.value
hdr("10c — GIS distance to MAHAN hospital vs facility contact")
cat("Straight-line distance village->MAHAN (km):\n"); print(round(summary(gis$dist_mahan_km),1))
cat(sprintf("vs %% reaching a facility: Spearman rho = %+.2f, p = %.3f\n", rho, pv))
write_csv(gis %>% select(vcode, lon, lat, dist_mahan_km, pct_reached),
          file.path(OUT_DIR, "10_gis_distance.csv"))

## community health-worker coverage (from the village survey, loaded in script 03)
if (exists("vil") && "bus_village" %in% names(vil)) {
  asha <- as.integer(startsWith(trimws(toupper(as.character(vil$pres_asha_anm))), "1"))
  cat(sprintf("\nVillages with a resident ASHA/ANM (community health worker): %d of %d\n",
              sum(asha, na.rm = TRUE), sum(!is.na(asha))))
}

} ## end file.exists(HH_GPS) guard

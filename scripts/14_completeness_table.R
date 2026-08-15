## =============================================================================
## 14_completeness_table.R — build the data-completeness table (Supplementary
## Table S1) entirely from the data, so nothing in the manuscript is hand-typed.
## Every count and percentage below is computed from the actual files.
## Writes: output/14_completeness_table.csv
## =============================================================================
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")

## non-missing counter (treats blank strings as missing)
nm <- function(x) {
  if (is.character(x)) sum(!is.na(x) & trimws(x) != "") else sum(!is.na(x))
}
num <- function(x) suppressWarnings(as.numeric(ifelse(trimws(x) %in% c("","50 mtr"),
        ifelse(trimws(x)=="50 mtr","0.05",NA), x)))
row <- function(variable, used_for, n, N, note = "", unit = "") {
  data.frame(variable = variable, used_for = used_for,
             complete = sprintf("%s / %s%s", format(n, big.mark=","),
                                format(N, big.mark=","), unit),
             pct = round(100 * n / N, 1), note = note, stringsAsFactors = FALSE)
}

Nep <- nrow(ari); Nde <- nrow(deaths); Nch <- nrow(dem); Nvi <- nrow(vil)

tab <- rbind(
  row("LRTI severity (non-severe/severe/very severe)", "Case classification",
      nm(ari$lri_cat), Nep),
  row("Care location (community vs facility)", "Outcome 1", nm(ari$community), Nep),
  row("Age at episode", "All analyses", nm(ari$age_grp), Nep),
  row("Sex", "Outcome 1, models", nm(ari$sexr), Nep,
      "Missing in the 2021-22 extension subset"),
  row("Death location and timing", "Outcomes 2-3",
      min(nm(deaths$death_location), nm(deaths$died_lt_1d)), Nde),
  row("Household wealth quintile", "Individual model", nm(dem$quintile_rural), Nch,
      "DHS-referenced asset index"),
  row("Maternal education", "Individual model", nm(dem$mother_edu_categorized), Nch),
  row("Village distance to primary health centre", "Village analysis",
      nm(num(vil$distance_primary_center_km)), Nvi),
  row("Village bus service", "Village analysis", nm(vil$bus_village), Nvi),
  row("Viral subtyping (HMPV, PIV, influenza)", "Not used here",
      nm(ari$hmpv_episode), Nep, "Not used in this access analysis")
)

## GPS rows (computed from the GISproject files if available) --------------------
GIS <- Sys.getenv("RSV_GIS_DIR",
  "/Users/priyanka/Downloads/RSV_India/GISproject/GISProject/")
hh_path <- file.path(GIS, "capstone project Mattie/critical_data_gpsdata.csv")
dg_path <- file.path(GIS, "capstone project Mattie/critical_data_deaths_gpsdata.csv")
if (file.exists(hh_path) && file.exists(dg_path)) {
  inbox <- function(d) d[!is.na(d$latitude)&!is.na(d$longitude)&
                         d$latitude>20.5&d$latitude<22.5&d$longitude>76&d$longitude<78, ]
  hh <- inbox(read_csv(hh_path, show_col_types = FALSE))
  hh$vcode <- as.numeric(sub("^([0-9]+).*", "\\1", hh$code_village))
  nvil_gps <- length(intersect(unique(ari$vcode), unique(hh$vcode)))
  dg <- inbox(read_csv(dg_path, show_col_types = FALSE))   # same in-box filter as the map
  nde_gps <- length(intersect(deaths$studyid, dg$studyid))
  tab <- rbind(tab,
    row("Household GPS (village centroids)", "Maps, GIS distance",
        nvil_gps, n_distinct(ari$vcode), unit = " villages",
        note = sprintf("%s households geolocated", format(nrow(hh), big.mark=","))),
    row("Death GPS", "Death map", nde_gps, Nde))
} else {
  message("14: GPS files not found — GPS completeness rows omitted (set RSV_GIS_DIR).")
}

hdr("14 — DATA COMPLETENESS TABLE (Supplementary Table S1, computed from data)")
print(tab, row.names = FALSE)
write_csv(tab, file.path(OUT_DIR, "14_completeness_table.csv"))

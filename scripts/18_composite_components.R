## =============================================================================
## 18_composite_components.R — completeness of each component of the composite
## village access-barrier index across the 93 villages, and how many of the five
## components each village actually contributes. Documents that the index is a
## mean over AVAILABLE components (some villages have fewer than five).
## Writes: output/18_composite_components.csv, output/18_components_per_village.csv
## =============================================================================
if (!exists("hdr")) source("00_setup.R")
if (!exists("vil")) source("03_derive_and_merge.R")

## same cleaner as script 08 (blank and "50 mtr" handled)
num <- function(x) suppressWarnings(as.numeric(
  ifelse(trimws(x) %in% c("", "50 mtr"),
         ifelse(trimws(x) == "50 mtr", "0.05", NA), x)))
present <- function(x) !is.na(x) & trimws(as.character(x)) != ""

comps <- list(
  "Distance to primary health centre" = !is.na(num(vil$distance_primary_center_km)),
  "Distance to sub-centre"            = !is.na(num(vil$distance_sub_center_km)),
  "Distance to bus stand"             = !is.na(num(vil$bus_stand_dist_km)),
  "Bus service in village"            = present(vil$bus_village),
  "Rainy-season road cutoff"          = present(vil$river_floods_rainyseason_cuts_o)
)
Nv <- nrow(vil)

tab <- data.frame(
  component         = names(comps),
  villages_complete = sapply(comps, sum),
  N_villages        = Nv,
  pct               = round(100 * sapply(comps, sum) / Nv, 1),
  row.names = NULL, stringsAsFactors = FALSE)

mat   <- do.call(cbind, comps)                 # 93 x 5 logical
ncomp <- rowSums(mat)
dist_n <- as.data.frame(table(factor(ncomp, levels = 0:5)))
names(dist_n) <- c("n_components_available", "villages")

hdr("18 — COMPOSITE INDEX COMPONENT COMPLETENESS (93 villages)")
print(tab, row.names = FALSE)
cat("\nNumber of the 5 components available per village:\n")
print(dist_n, row.names = FALSE)
cat(sprintf("\nVillages with all 5 components: %d of %d; median components/village: %g.\n",
            sum(ncomp == 5), Nv, median(ncomp)))

write_csv(tab,    file.path(OUT_DIR, "18_composite_components.csv"))
write_csv(dist_n, file.path(OUT_DIR, "18_components_per_village.csv"))

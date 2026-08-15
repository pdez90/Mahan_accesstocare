## =============================================================================
## 07_village_access.R
## Village-level (ecological) associations between access-to-care outcomes and
## geographic access: distance to the nearest primary health centre, and the
## presence of a scheduled bus service.
## =============================================================================
if (!exists("vil")) source("03_derive_and_merge.R")

## village-level aggregates -----------------------------------------------------
vill_reached <- ari %>% filter(sev_vs) %>%
  group_by(vcode) %>%
  summarise(n_sv = n(), reached_sv = sum(reached_facility),
            pct_reached_sv = 100 * reached_sv / n_sv, .groups = "drop")

vill_deaths <- deaths %>%
  group_by(vcode) %>%
  summarise(n_death = n(), comm_death = sum(community_type == 1),
            pct_comm_death = 100 * comm_death / n_death, .groups = "drop")

vill <- vill_reached %>%
  left_join(vil %>% select(vcode, distance_primary_center_km,
                           bus_stand_dist_km, bus_yes), by = "vcode") %>%
  left_join(vill_deaths, by = "vcode")

hdr("07 — VILLAGE-LEVEL GEOGRAPHIC ACCESS")
cat("Villages in facility-contact regression:", sum(!is.na(vill$pct_reached_sv)),
    "| in community-death regression:", sum(!is.na(vill$pct_comm_death)),
    "(only villages with >=1 LRTI death).\n\n")
cat("Distance to nearest primary health centre (km):\n")
print(summary(vil$distance_primary_center_km))
cat("\nVillages with a scheduled bus service:",
    sum(vil$bus_yes, na.rm = TRUE), "of", sum(!is.na(vil$bus_yes)), "\n")

## regressions -----------------------------------------------------------------
f1 <- lm(pct_reached_sv ~ distance_primary_center_km, data = vill)
f2 <- lm(pct_reached_sv ~ factor(bus_yes),            data = vill)
f3 <- lm(pct_comm_death ~ distance_primary_center_km, data = vill)

cat("\n% severe/vsev reaching a facility ~ distance to primary centre:\n"); print(tidy(f1))
cat("\n% severe/vsev reaching a facility ~ bus service in village:\n");     print(tidy(f2))
bus_means <- vill %>% filter(!is.na(bus_yes)) %>% group_by(bus_yes) %>%
  summarise(mean_pct_reached = round(mean(pct_reached_sv, na.rm = TRUE), 1),
            n_villages = n(), .groups = "drop")
cat("\nMean village % reaching a facility, by bus service (1=yes,0=no):\n"); print(bus_means)
cat("\n% community deaths ~ distance to primary centre:\n"); print(tidy(f3))

write_csv(vill,             file.path(OUT_DIR, "07_village_table.csv"))
write_csv(tidy(f1),         file.path(OUT_DIR, "07_fit_dist_reached.csv"))
write_csv(tidy(f2),         file.path(OUT_DIR, "07_fit_bus_reached.csv"))
write_csv(tidy(f3),         file.path(OUT_DIR, "07_fit_dist_commdeath.csv"))
write_csv(bus_means,        file.path(OUT_DIR, "07_bus_means.csv"))

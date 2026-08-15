## =============================================================================
## 06_outcome3_effective_care.R
## OUTCOME 3 — proportion of LRTI deaths occurring in the community, in transit,
## or within 24 hours of reaching a facility (i.e. without effective facility care).
## =============================================================================
if (!exists("deaths") || !"vcode" %in% names(deaths)) source("03_derive_and_merge.R")

deaths <- deaths %>%
  mutate(
    on_way            = death_location == "on way",
    hosp_death        = community_type == 0,
    hosp_death_lt1d   = hosp_death & died_lt_1d == 1,
    no_effective_care = (community_type == 1) | on_way | hosp_death_lt1d
  )

hdr("06 — OUTCOME 3: deaths beyond the reach of effective facility care")
cat("Analytic N:", nrow(deaths),
    "deaths; place and same-day timing (died_lt_1d) recorded for all",
    sum(!is.na(deaths$community_type) & !is.na(deaths$died_lt_1d)), "used here.\n\n")

o3 <- deaths %>%
  summarise(
    n_deaths                 = n(),
    community                = sum(community_type == 1),
    in_transit               = sum(on_way),
    transit_not_already_comm = sum(on_way & community_type == 0),
    facility_within_24h      = sum(hosp_death_lt1d),
    union_no_care            = sum(no_effective_care),
    pct_union                = round(100 * union_no_care / n_deaths, 1),
    died_within_1d_all       = sum(died_lt_1d == 1),
    pct_died_within_1d       = round(100 * mean(died_lt_1d == 1), 1)
  )
print(o3)

o3_timing <- deaths %>%
  summarise(across(c(died_lt_1d, died_1to7d, died_8to14d, died_14to30d),
                   ~ sum(.x == 1, na.rm = TRUE)))
cat("\nTiming of death from first assessment (counts):\n"); print(o3_timing)

write_csv(o3,        file.path(OUT_DIR, "06_o3_summary.csv"))
write_csv(o3_timing, file.path(OUT_DIR, "06_o3_timing.csv"))

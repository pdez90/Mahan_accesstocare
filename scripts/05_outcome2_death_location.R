## =============================================================================
## 05_outcome2_death_location.R
## OUTCOME 2 — proportion of LRTI deaths occurring in the community vs a facility.
## =============================================================================
if (!exists("deaths") || !"vcode" %in% names(deaths)) source("03_derive_and_merge.R")

hdr("05 — OUTCOME 2: location of LRTI deaths")
cat("Analytic N:", nrow(deaths), "deaths; place of death recorded for",
    sum(!is.na(deaths$death_location)), "(",
    round(100*mean(!is.na(deaths$death_location)),1), "% complete).\n\n")

## Percentages are computed among deaths with KNOWN location (n_known), so a
## missing community_type can neither propagate NA through sum() nor put
## unknown-location deaths in the denominator. n_deaths is reported alongside.
o2 <- deaths %>%
  summarise(n_deaths  = n(),
            n_known   = sum(!is.na(community_type)),
            community = sum(community_type == 1, na.rm = TRUE),
            facility  = sum(community_type == 0, na.rm = TRUE),
            pct_community = round(100 * community / n_known, 1))
cat("All LRTI deaths, community vs facility:\n"); print(o2)

o2_place <- deaths %>% count(death_location) %>%
  mutate(pct = round(100 * n / sum(n), 1))
cat("\nRecorded place of death:\n"); print(o2_place)

o2_sv <- deaths %>% filter(lri_cat %in% c(1, 2)) %>%
  summarise(n = n(), n_known = sum(!is.na(community_type)),
            community = sum(community_type == 1, na.rm = TRUE),
            pct_community = round(100 * community / n_known, 1))
cat("\nRestricted to severe/very-severe deaths:\n"); print(o2_sv)

write_csv(o2,       file.path(OUT_DIR, "05_o2_overall.csv"))
write_csv(o2_place, file.path(OUT_DIR, "05_o2_place.csv"))
write_csv(o2_sv,    file.path(OUT_DIR, "05_o2_severe.csv"))

## =============================================================================
## 21_death_by_tier_location.R — location of LRTI death by severity tier
## (a reviewer-requested breakdown: village / in transit / hospital, and the
## community percentage, within each WHO severity category).
## NB: summary columns use DISTINCT names (n_comm/n_fac) so they do not shadow
## the source `community` column during summarise (dplyr >=1.1 hazard).
## Writes: output/21_death_by_tier_location.csv
## =============================================================================
if (!exists("hdr"))    source("00_setup.R")
if (!exists("deaths")) source("01_load_data.R")

by_tier <- function(d) {
  d %>% summarise(deaths   = n(),
                  village  = sum(death_location == "village"),
                  transit  = sum(death_location == "on way"),
                  hospital = sum(death_location == "hospital"),
                  n_comm   = sum(community == 1),
                  n_fac    = sum(community == 0), .groups = "drop")
}

tl <- deaths %>%
  mutate(severity = as.character(factor(lri_cat, levels = c(3, 2, 1),
             labels = c("Non-severe", "Severe", "Very severe")))) %>%
  group_by(severity) %>% by_tier() %>%
  arrange(match(severity, c("Non-severe", "Severe", "Very severe")))

all_row <- by_tier(deaths) %>% mutate(severity = "All LRTI deaths")

tl <- bind_rows(tl, all_row) %>%
  mutate(community = n_comm, facility = n_fac,
         pct_community = round(100 * n_comm / deaths, 1)) %>%
  select(severity, deaths, village, transit, hospital, community, facility, pct_community)

write_csv(tl, file.path(OUT_DIR, "21_death_by_tier_location.csv"))
hdr("21 — DEATH LOCATION BY SEVERITY TIER")
print(as.data.frame(tl), row.names = FALSE)

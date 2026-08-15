## =============================================================================
## 16_rsv_underascertainment.R — is the community-death under-ascertainment also
## present among RSV-positive fatal LRTI episodes? National RSV-mortality models
## (e.g. Joseph et al. 2025) build on facility-based case-fatality; this asks how
## many RSV-associated child deaths would be invisible to such systems.
##
## IMPORTANT CAVEAT: RSV-positive status requires a swab, and swabbing is more
## likely for children who reached a facility. RSV-positive fatal episodes are
## therefore enriched for facility deaths, biasing the community fraction DOWNWARD.
## Treat the RSV-specific fraction as descriptive and confounded, not a clean
## estimate. Un-swabbed episodes are coded 0, so RSV-positive deaths are also a
## lower bound in number.
## Writes: output/16_rsv_underascertainment.csv
## =============================================================================
if (!exists("hdr"))    source("00_setup.R")
if (!exists("deaths")) source("01_load_data.R")

rsv_fatal <- ari %>% filter(lrti_death == 1) %>%
  group_by(studyid) %>%
  summarise(rsv = as.integer(any(rsva_episode == 1 | rsvb_episode == 1)),
            .groups = "drop")

d <- deaths %>% left_join(rsv_fatal, by = "studyid")
matched <- sum(!is.na(d$rsv))

hdr("16 — RSV-SPECIFIC UNDER-ASCERTAINMENT OF FATAL LRTI (descriptive; confounded)")
cat(sprintf("Fatal episodes matched to episode-file RSV status: %d of %d deaths.\n",
            matched, nrow(deaths)))
cat("Caveat: RSV+ requires a swab (more likely at facilities), so the RSV+ community\n",
    "fraction is biased downward; interpret descriptively only.\n\n")

## NB: summary columns are given DISTINCT names (n_community/n_facility) so they do
## not shadow the source `community` column during summarise (dplyr >=1.1 hazard).
by_rsv <- d %>% filter(!is.na(rsv)) %>%
  group_by(rsv) %>%
  summarise(n = n(),
            n_community = sum(community == 1),
            n_facility  = sum(community == 0),
            .groups = "drop") %>%
  mutate(pct_community    = round(100 * n_community / n, 1),
         correction_factor = ifelse(n_facility > 0, round(n / n_facility, 2), NA_real_),
         group = ifelse(rsv == 1, "RSV-positive", "RSV-negative/untested")) %>%
  select(group, rsv, n, n_community, n_facility, pct_community, correction_factor)

print(as.data.frame(by_rsv), row.names = FALSE)
write_csv(by_rsv, file.path(OUT_DIR, "16_rsv_underascertainment.csv"))

rp <- by_rsv[by_rsv$rsv == 1, ]
cat(sprintf("\nAmong %d RSV-positive fatal LRTI episodes, %d (%.0f%%) died in the community\n",
            rp$n, rp$n_community, rp$pct_community))
cat(sprintf("or in transit and %d at a facility.\n", rp$n_facility))
if (rp$n < 20) {
  cat("N is small AND swab-ascertainment biases this toward facility deaths —\n",
      "report descriptively; do not present a precise multiplier.\n")
}

## =============================================================================
## 08_access_index_and_individual_model.R
##
## (a) Builds a composite VILLAGE ACCESS-BARRIER INDEX from the transportation /
##     distance variables in the village survey.
## (b) Fits the PRIMARY individual-episode model for facility contact, adding
##     household wealth, maternal education, and village access to age and sex,
##     with nested random intercepts for village and child. This is far better
##     powered than the village-level (ecological) regressions in script 07.
##
## Requires lme4 (+ broom.mixed). Degrades gracefully if not installed.
## =============================================================================
if (!exists("vil")) source("03_derive_and_merge.R")

num <- function(x) suppressWarnings(as.numeric(
  ifelse(trimws(x) %in% c("", "50 mtr"),
         ifelse(trimws(x) == "50 mtr", "0.05", NA), x)))
bin1 <- function(x) as.integer(startsWith(trimws(toupper(as.character(x))), "1"))

## ---- (a) composite village access-barrier index ---------------------------
## Structural / transport access. Each component oriented so HIGHER = WORSE
## access, standardised to a z-score, then averaged over available components.
va <- vil %>% transmute(
  vcode,
  z_dist_phc     = scale(num(distance_primary_center_km))[, 1],
  z_dist_subctr  = scale(num(distance_sub_center_km))[, 1],
  z_bus_stand    = scale(num(bus_stand_dist_km))[, 1],
  z_no_bus       = scale(1 - bin1(bus_village))[, 1],          # no bus = worse
  z_flood_cutoff = scale(bin1(river_floods_rainyseason_cuts_o))[, 1]  # cutoff = worse
)
va$access_barrier <- rowMeans(
  va[, c("z_dist_phc","z_dist_subctr","z_bus_stand","z_no_bus","z_flood_cutoff")],
  na.rm = TRUE)
## re-standardise the index to mean 0, SD 1 (so model ORs are per-SD)
va$access_barrier_z <- as.numeric(scale(va$access_barrier))

hdr("08(a) — COMPOSITE VILLAGE ACCESS-BARRIER INDEX")
cat("Higher = worse geographic/transport access. Components: distance to PHC,\n",
    "distance to sub-centre, distance to bus stand, no bus in village, rainy-season cutoff.\n\n")
print(summary(va$access_barrier_z))
write_csv(va, file.path(OUT_DIR, "08_village_access_index.csv"))

## village-level association of the composite index with the two outcomes
vr <- ari %>% filter(sev_vs) %>% group_by(vcode) %>%
  summarise(pct_reached = 100 * mean(reached_facility), .groups = "drop")
vd <- deaths %>% group_by(vcode) %>%
  summarise(pct_comm = 100 * mean(community_type == 1), .groups = "drop")
vv <- vr %>% left_join(vd, by = "vcode") %>% left_join(va, by = "vcode")
cat("\nComposite index vs outcomes (village level, Spearman):\n")
cat(sprintf("  vs %% reaching facility : rho = %+.2f, p = %.3f\n",
            cor(vv$access_barrier_z, vv$pct_reached, method = "spearman", use = "complete.obs"),
            cor.test(vv$access_barrier_z, vv$pct_reached, method = "spearman")$p.value))
cat(sprintf("  vs %% community death   : rho = %+.2f, p = %.3f\n",
            cor(vv$access_barrier_z, vv$pct_comm, method = "spearman", use = "complete.obs"),
            suppressWarnings(cor.test(vv$access_barrier_z, vv$pct_comm, method = "spearman"))$p.value))

## ---- (b) individual-episode model -----------------------------------------
## Merge household SES (wealth quintile, maternal education) and the village
## access index onto severe/very-severe episodes.
ses <- dem %>% select(uid, quintile_rural, mother_edu_categorized)
sv <- ari %>% filter(sev_vs) %>%
  left_join(ses, by = "uid") %>%
  left_join(va %>% select(vcode, access_barrier_z), by = "vcode") %>%
  mutate(
    wealth_q   = as.numeric(quintile_rural),                 # 1 (poorest) .. 5
    mother_edu = factor(mother_edu_categorized,
                        levels = c("complete primary", "more than primary")),
    monsoon    = as.integer(ai_month %in% 6:9)               # SW monsoon (Jun–Sep)
  )

## seasonal descriptive: facility contact by monsoon and by month
o1_season <- sv %>% group_by(monsoon) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct_reached = round(100*mean(reached_facility), 1), .groups = "drop")
o1_month <- sv %>% group_by(ai_month) %>%
  summarise(n = n(), pct_reached = round(100*mean(reached_facility), 1), .groups = "drop")
hdr("08(b0) — SEASONALITY: facility contact by monsoon (Jun–Sep) and by month")
cat("By monsoon (0=Oct–May, 1=Jun–Sep):\n"); print(o1_season)
cat("\nBy calendar month:\n"); print(o1_month)
write_csv(o1_season, file.path(OUT_DIR, "08_facility_by_monsoon.csv"))
write_csv(o1_month,  file.path(OUT_DIR, "08_facility_by_month.csv"))

hdr("08(b) — INDIVIDUAL-EPISODE MODEL: facility contact ~ age + sex + wealth + maternal edu + village access + monsoon")
cat("Complete-case episodes:", sum(complete.cases(
  sv[, c("reached_facility","age_grp","sexr","wealth_q","mother_edu","access_barrier_z")])),
  "of", nrow(sv), "severe/very-severe episodes.\n\n")

if (requireNamespace("lme4", quietly = TRUE)) {
  m2 <- lme4::glmer(
    reached_facility ~ factor(age_grp) + factor(sexr) + scale(wealth_q) +
      mother_edu + access_barrier_z + monsoon + (1 | vcode) + (1 | uid),
    data = sv, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa"))
  cat("Model fitted on", nobs(m2), "episodes from",
      lme4::ngrps(m2)["uid"], "children in", lme4::ngrps(m2)["vcode"], "villages.\n\n")
  if (requireNamespace("broom.mixed", quietly = TRUE)) {
    out <- broom.mixed::tidy(m2, effects = "fixed", exponentiate = TRUE, conf.int = TRUE)
    print(out)
    cat("\nRandom-effect variances (village, child):\n")
    print(as.data.frame(lme4::VarCorr(m2)))
    write_csv(out, file.path(OUT_DIR, "08_individual_model.csv"))
  } else {
    print(summary(m2)$coefficients)
  }
} else {
  message("lme4 not installed: skipping individual model. ",
          "Install with install.packages(c('lme4','broom.mixed')).")
}

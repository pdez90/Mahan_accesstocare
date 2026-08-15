## =============================================================================
## 22_primary_access_model.R
##
## Because the composite village access-barrier index has missing components in
## many villages (only 51 of 93 have all five; see script 18), the PRIMARY
## individual-episode model uses the objective, complete Weiss/Malaria-Atlas
## motorized travel-time-to-healthcare surface as the geographic-access term
## (available for all 93 villages). The composite index is retained only as a
## SENSITIVITY analysis, restricted to the 51 fully-observed villages.
##
## Needs the outputs of scripts 08 (composite index + z-components) and 19 (Weiss
## travel time). Requires lme4 + broom.mixed.
## Writes: output/22_primary_model_weiss.csv, output/22_sensitivity_composite_51.csv
## =============================================================================
if (!exists("hdr")) source("00_setup.R")
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")

tt_path <- file.path(OUT_DIR, "19_village_travel_time.csv")
va_path <- file.path(OUT_DIR, "08_village_access_index.csv")
if (!requireNamespace("lme4", quietly = TRUE) || !file.exists(tt_path) || !file.exists(va_path)) {
  message("22: needs lme4 and the outputs of scripts 08 and 19 — skipping.")
} else {
  suppressPackageStartupMessages({library(lme4); library(broom.mixed)})

  tt <- read_csv(tt_path, show_col_types = FALSE) %>% select(vcode, motorized_min, walking_min)
  va <- read_csv(va_path, show_col_types = FALSE)

  ## fully-observed villages = all FIVE composite components genuinely recorded,
  ## defined identically to Supplementary Table S3 / script 18 (blank != imputed).
  present <- function(x) !is.na(x) & trimws(as.character(x)) != ""
  numc <- function(x) suppressWarnings(as.numeric(
    ifelse(trimws(x) %in% c("", "50 mtr"), ifelse(trimws(x) == "50 mtr", "0.05", NA), x)))
  comp5 <- cbind(!is.na(numc(vil$distance_primary_center_km)),
                 !is.na(numc(vil$distance_sub_center_km)),
                 !is.na(numc(vil$bus_stand_dist_km)),
                 present(vil$bus_village),
                 present(vil$river_floods_rainyseason_cuts_o))
  complete_villages <- vil$vcode[rowSums(comp5) == 5]

  ses <- dem %>% select(uid, quintile_rural, mother_edu_categorized)
  sv <- ari %>% filter(sev_vs) %>%
    left_join(ses, by = "uid") %>%
    left_join(va %>% select(vcode, access_barrier_z), by = "vcode") %>%
    left_join(tt, by = "vcode") %>%
    mutate(wealth_q   = as.numeric(quintile_rural),
           mother_edu = factor(mother_edu_categorized,
                               levels = c("complete primary", "more than primary")),
           monsoon    = as.integer(ai_month %in% 6:9))

  ctrl <- glmerControl(optimizer = "bobyqa")
  show <- function(m, keep) broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE,
                                              conf.int = TRUE) %>%
    filter(grepl(keep, term)) %>% select(term, estimate, conf.low, conf.high, p.value)

  ## ---- PRIMARY: Weiss motorized travel time (complete for all villages) ------
  hdr("22 — PRIMARY MODEL: geographic access = Weiss motorized travel time")
  mP <- glmer(reached_facility ~ factor(age_grp) + factor(sexr) + scale(wealth_q) +
                mother_edu + scale(motorized_min) + monsoon + (1 | vcode) + (1 | uid),
              data = sv, family = binomial, control = ctrl)
  cat("N =", nobs(mP), "episodes from", ngrps(mP)["uid"], "children in",
      ngrps(mP)["vcode"], "villages.\n")
  print(as.data.frame(show(mP, "motorized|monsoon|sexr|wealth|mother")), row.names = FALSE)
  cat("\nRandom-effect variances (village, child):\n")
  print(as.data.frame(VarCorr(mP))[, c("grp", "vcov")])
  write_csv(broom.mixed::tidy(mP, effects = "fixed", exponentiate = TRUE, conf.int = TRUE),
            file.path(OUT_DIR, "22_primary_model_weiss.csv"))

  ## ---- SENSITIVITY: composite index on the 51 fully-observed villages --------
  sv51 <- sv %>% filter(vcode %in% complete_villages)
  hdr("22b — SENSITIVITY: composite index, 51 fully-observed villages only")
  cat("Fully-observed villages:", length(complete_villages),
      "| severe/vsev episodes:", nrow(sv51), "\n")
  mS <- glmer(reached_facility ~ factor(age_grp) + factor(sexr) + scale(wealth_q) +
                mother_edu + access_barrier_z + monsoon + (1 | vcode) + (1 | uid),
              data = sv51, family = binomial, control = ctrl)
  cat("N =", nobs(mS), "episodes in", ngrps(mS)["vcode"], "villages.\n")
  print(as.data.frame(show(mS, "access_barrier|monsoon|sexr|wealth")), row.names = FALSE)
  write_csv(broom.mixed::tidy(mS, effects = "fixed", exponentiate = TRUE, conf.int = TRUE),
            file.path(OUT_DIR, "22_sensitivity_composite_51.csv"))
}

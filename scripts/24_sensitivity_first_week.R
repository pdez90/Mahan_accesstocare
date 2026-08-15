## =============================================================================
## 24_sensitivity_first_week.R
##
## Rowena's observation (email): facility contact is very high in the first week
## of life (50.4%, 69/137) and drops to ~20% thereafter. Likely explanation:
## ~60-70% of births occur in a health facility and mother+baby stay ~a week, so
## a severe LRTI episode recorded in week 1 is "at a facility" because the child
## was still there for the birth — not because of care-seeking for the illness.
##
## This script (1) TESTS that hypothesis using the recorded place of birth, and
## (2) runs the SENSITIVITY analysis Priyanka proposed: exclude the birth-driven
## week-1 episodes and re-examine whether any access-to-care measure predicts
## facility contact among the remaining episodes.
##
## Reproducible in the cloud from the composite index, the individual survey
## distances, and the GIS distance; the Malaria-Atlas (Weiss) motorized travel
## time is included automatically when script 19's output is present.
##
## Writes: output/24_agegrp_contact.csv, output/24_week1_by_birthplace.csv,
##         output/24_primary_model_by_cohort.csv, output/24_access_screen_drop_wk1.csv
## =============================================================================
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")
suppressPackageStartupMessages({library(dplyr); library(readr)})
has_lme4 <- requireNamespace("lme4", quietly = TRUE) &&
            requireNamespace("broom.mixed", quietly = TRUE)

numc <- function(x) suppressWarnings(as.numeric(
  ifelse(trimws(x) %in% c("", "50 mtr"), ifelse(trimws(x) == "50 mtr", "0.05", NA), x)))
bin1 <- function(x) as.integer(startsWith(trimws(toupper(as.character(x))), "1"))

## ---- village-level access measures (reproducible) --------------------------
va  <- read_csv(file.path(OUT_DIR, "08_village_access_index.csv"), show_col_types = FALSE) %>%
         select(vcode, access_barrier_z)
gis <- tryCatch(read_csv(file.path(OUT_DIR, "10_gis_distance.csv"), show_col_types = FALSE) %>%
         select(vcode, dist_mahan_km), error = function(e) NULL)
tt_path <- file.path(OUT_DIR, "19_village_travel_time.csv")
tt <- if (file.exists(tt_path))
  read_csv(tt_path, show_col_types = FALSE) %>% select(vcode, motorized_min) else NULL
if (is.null(tt)) message("24: 19_village_travel_time.csv not found — Weiss term skipped ",
                         "in this run (it runs automatically where script 19 has produced output).")

vgeo <- vil %>% transmute(vcode,
  dist_phc    = numc(distance_primary_center_km),
  dist_subctr = numc(distance_sub_center_km),
  bus_stand   = numc(bus_stand_dist_km),
  bus_service = bin1(bus_village),
  flood_cut   = bin1(river_floods_rainyseason_cuts_o))

## ---- place of birth --------------------------------------------------------
## institutional birth = subcentre/PHC/qualified-private/rural/sub-district/
## district hospital (codes 2,3,4,6,7,8); home/on-road/other = 1,5,9,10.
bp <- dem %>% mutate(
        bp_code = suppressWarnings(as.integer(sub("-.*", "", trimws(birth_place)))),
        facility_birth = as.integer(bp_code %in% c(2, 3, 4, 6, 7, 8))) %>%
      select(uid, facility_birth, bp_code)

cat(sprintf("Institutional (facility) birth rate in cohort: %.1f%% (%d/%d children with birth place)\n",
            100 * mean(bp$facility_birth[!is.na(bp$bp_code)]),
            sum(bp$facility_birth, na.rm = TRUE), sum(!is.na(bp$bp_code))))

## ---- analytic episode set (severe/very-severe) -----------------------------
ses <- dem %>% select(uid, quintile_rural, mother_edu_categorized)
sv <- ari %>% filter(sev_vs) %>%
  left_join(ses,  by = "uid") %>%
  left_join(bp,   by = "uid") %>%
  left_join(va,   by = "vcode") %>%
  left_join(vgeo, by = "vcode") %>%
  { if (!is.null(gis)) left_join(., gis, by = "vcode") else . } %>%
  { if (!is.null(tt))  left_join(., tt,  by = "vcode") else . } %>%
  mutate(wealth_q   = as.numeric(quintile_rural),
         mother_edu = factor(mother_edu_categorized,
                             levels = c("complete primary", "more than primary")),
         monsoon    = as.integer(ai_month %in% 6:9),
         first_week = as.integer(age_grp == 1))

## =============================================================================
## PART 1 — TEST THE HOSPITAL-BIRTH HYPOTHESIS
## =============================================================================
AGE_LAB <- c("0-6 d", "7-27 d", "28-89 d", "90-179 d", "180-364 d", "365-730 d")
agetab <- sv %>% group_by(age_grp) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct = round(100 * mean(reached_facility), 1), .groups = "drop") %>%
  mutate(age = AGE_LAB[age_grp])
hdr("24(1a) — Facility contact by age group (reproduces Figure S1 numbers)")
print(as.data.frame(agetab[, c("age_grp","age","n","reached","pct")]), row.names = FALSE)
write_csv(agetab, file.path(OUT_DIR, "24_agegrp_contact.csv"))

## the decisive test: within week 1, is high facility contact confined to
## facility-born children?  Compare with later infancy as a reference.
w1 <- sv %>% filter(first_week == 1, !is.na(facility_birth)) %>%
  group_by(facility_birth) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct = round(100 * mean(reached_facility), 1), .groups = "drop") %>%
  mutate(birth = ifelse(facility_birth == 1, "facility-born", "home/road-born"))
later <- sv %>% filter(first_week == 0, !is.na(facility_birth)) %>%
  group_by(facility_birth) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct = round(100 * mean(reached_facility), 1), .groups = "drop") %>%
  mutate(birth = ifelse(facility_birth == 1, "facility-born", "home/road-born"))
hdr("24(1b) — Week-1 facility contact by place of birth (vs later infancy)")
cat("First week of life (0-6 d):\n")
print(as.data.frame(w1[, c("birth","n","reached","pct")]), row.names = FALSE)
cat("\nRest of infancy/2nd year (>=7 d), for reference:\n")
print(as.data.frame(later[, c("birth","n","reached","pct")]), row.names = FALSE)
write_csv(bind_rows(mutate(w1, period = "week1"), mutate(later, period = "later")),
          file.path(OUT_DIR, "24_week1_by_birthplace.csv"))

## =============================================================================
## PART 2 — SENSITIVITY: refit access models excluding the birth-driven episodes
## =============================================================================
cohorts <- list(
  "full cohort"                 = sv,
  "drop all week-1 (0-6 d)"     = filter(sv, first_week == 0),
  "drop facility-born week-1"   = filter(sv, !(first_week == 1 & facility_birth == 1)))

## geographic-access terms to test (village-level, merged to episodes)
geo_terms <- c(
  if (!is.null(tt))  "motorized_min" = "motorized_min",
  "access_barrier_z" = "access_barrier_z",
  if (!is.null(gis)) "dist_mahan_km" = "dist_mahan_km",
  "dist_phc" = "dist_phc")

fit_geo <- function(d, geo) {
  d2 <- d %>% mutate(GEO = scale(as.numeric(.data[[geo]]))[, 1])
  ok <- sum(!is.na(d2$GEO) & !is.na(d2$reached_facility))
  f  <- reached_facility ~ factor(age_grp) + factor(sexr) + scale(wealth_q) +
          mother_edu + GEO + monsoon + (1 | vcode) + (1 | uid)
  m  <- tryCatch(lme4::glmer(f, data = d2, family = binomial,
          control = lme4::glmerControl(optimizer = "bobyqa")), silent = TRUE,
          error = function(e) NULL)
  if (is.null(m) || inherits(m, "try-error")) return(NULL)
  broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "GEO") %>%
    transmute(term = paste0(geo, " (per SD)"), OR = round(estimate, 3),
              conf.low = round(conf.low, 3), conf.high = round(conf.high, 3),
              p.value = signif(p.value, 3), n = nobs(m))
}

hdr("24(2) — PRIMARY MODEL: geographic-access ORs by cohort (adjusted)")
rows <- list()
for (cn in names(cohorts)) {
  d <- cohorts[[cn]]
  cat(sprintf("\n-- %s: %d episodes, facility contact %.1f%% --\n",
              cn, nrow(d), 100 * mean(d$reached_facility)))
  if (!has_lme4) next
  for (g in geo_terms) {
    r <- fit_geo(d, g)
    if (!is.null(r)) { r$cohort <- cn; print(as.data.frame(r), row.names = FALSE); rows[[length(rows)+1]] <- r }
  }
}
if (length(rows)) write_csv(bind_rows(rows) %>% select(cohort, everything()),
                            file.path(OUT_DIR, "24_primary_model_by_cohort.csv"))

## =============================================================================
## PART 3 — full geographic-access screen on the drop-week-1 cohort
## (does ANY access measure predict facility contact once births are removed?)
## =============================================================================
dwk1 <- cohorts[["drop all week-1 (0-6 d)"]]
screen_vars <- c(
  if (!is.null(tt)) "motorized_min",
  "access_barrier_z",
  if (!is.null(gis)) "dist_mahan_km",
  "dist_phc", "dist_subctr", "bus_stand", "bus_service", "flood_cut")

screen_one <- function(v) {
  cont <- !(v %in% c("bus_service", "flood_cut"))
  d2 <- dwk1 %>% mutate(X = if (cont) scale(as.numeric(.data[[v]]))[, 1] else as.numeric(.data[[v]]))
  f  <- reached_facility ~ X + factor(age_grp) + factor(sexr) + (1 | vcode) + (1 | uid)
  m  <- tryCatch(lme4::glmer(f, data = d2, family = binomial,
          control = lme4::glmerControl(optimizer = "bobyqa")), silent = TRUE,
          error = function(e) NULL)
  if (is.null(m)) return(NULL)
  broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "X") %>%
    transmute(measure = paste0(v, if (cont) " (per SD)" else " (yes vs no)"),
              OR = round(estimate, 3), conf.low = round(conf.low, 3),
              conf.high = round(conf.high, 3), p.value = signif(p.value, 3), n = nobs(m))
}
hdr("24(3) — ACCESS SCREEN on drop-week-1 cohort (EXPLORATORY; adjusted for age + sex)")
if (has_lme4) {
  scr <- bind_rows(lapply(screen_vars, screen_one))
  ## multiple-testing control, as in script 12: BH false-discovery-rate
  scr$q.value <- signif(p.adjust(scr$p.value, method = "BH"), 3)
  print(as.data.frame(scr), row.names = FALSE)
  write_csv(scr, file.path(OUT_DIR, "24_access_screen_drop_wk1.csv"))
  cat("\nExploratory screen,", nrow(scr), "tests, Benjamini-Hochberg corrected.",
      "\nMeasures with q < 0.05:",
      paste(scr$measure[!is.na(scr$q.value) & scr$q.value < 0.05], collapse = ", "),
      "(none listed = no access measure predicts contact after removing births)\n")
}

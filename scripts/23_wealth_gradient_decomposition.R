## =============================================================================
## 23_wealth_gradient_decomposition.R
##
## WHY: the "effective care" model (script 11: reached_survived = reached a
## facility AND survived) showed an INVERSE wealth gradient (each higher quintile
## -> LOWER odds of effective care, OR ~0.90), even though CRUDE facility contact
## (script 08) shows no wealth gradient. Because effective care = contact x
## survival, and contact is flat in wealth, the inverse gradient must live in the
## SURVIVAL half. This script decomposes it to see (a) whether it is real,
## (b) which half it lives in, and (c) whether it is a severity-mix artefact.
##
## Depends on script 11 (for `svr`, which carries reached_facility, died,
## reached_survived, wealth_q, lri_cat and all covariates on severe/v-severe
## episodes). Uses lme4 + broom.mixed; falls back to glm if a mixed model is
## singular on a subset.
##
## Writes: output/23_wealth_by_quintile.csv, output/23_wealth_decomposition_models.csv
## =============================================================================
if (!exists("svr")) source("11_refined_outcomes.R")
suppressPackageStartupMessages({library(dplyr)})
has_lme4 <- requireNamespace("lme4", quietly = TRUE) &&
            requireNamespace("broom.mixed", quietly = TRUE)

## make sure survival + severity flags are present and clean
d <- svr %>%
  mutate(survived    = as.integer(died == 0),
         very_severe = as.integer(lri_cat == 1),
         wealth_q    = as.numeric(wealth_q)) %>%
  filter(!is.na(wealth_q))

cat("SD of wealth quintile (for per-SD vs per-quintile interpretation):",
    round(sd(d$wealth_q), 2), "\n")

## ---- (1) DESCRIPTIVE: everything by wealth quintile ------------------------
by_q <- d %>%
  group_by(wealth_q) %>%
  summarise(
    n_episodes       = n(),
    pct_very_severe  = round(100 * mean(very_severe), 1),
    pct_reached      = round(100 * mean(reached_facility), 1),
    pct_survived_all = round(100 * mean(survived), 1),
    n_reached        = sum(reached_facility == 1),
    pct_surv_reached = round(100 * sum(reached_facility == 1 & survived == 1) /
                             pmax(sum(reached_facility == 1), 1), 1),
    pct_effective    = round(100 * mean(reached_survived), 1),
    .groups = "drop")

hdr("23(1) — DESCRIPTIVE by wealth quintile (severe/very-severe episodes)")
cat("pct_reached      = crude facility contact (script-08 outcome; expected flat)\n")
cat("pct_survived_all = survival over ALL severe episodes\n")
cat("pct_surv_reached = survival AMONG facility reachers (collider-prone)\n")
cat("pct_effective    = reached AND survived (the composite showing OR~0.90)\n\n")
print(as.data.frame(by_q), row.names = FALSE)
write_csv(by_q, file.path(OUT_DIR, "23_wealth_by_quintile.csv"))

## ---- (2) does wealth predict SEVERITY? (the obvious confounder) ------------
hdr("23(2) — Does wealth track severity mix? (very-severe ~ wealth)")
if (has_lme4) {
  ms <- try(lme4::glmer(very_severe ~ scale(wealth_q) + factor(age_grp) +
              factor(sexr) + (1 | vcode) + (1 | uid),
              data = d, family = binomial,
              control = lme4::glmerControl(optimizer = "bobyqa")), silent = TRUE)
  if (!inherits(ms, "try-error")) {
    print(broom.mixed::tidy(ms, effects = "fixed", exponentiate = TRUE,
                            conf.int = TRUE) %>%
          filter(grepl("wealth", term)))
  }
}
cat("If wealthier children are MORE often very-severe, that alone can drag down\n",
    "their survival and hence 'effective care'.\n")

## ---- (3) MODEL DECOMPOSITION -----------------------------------------------
## Fit the wealth term four ways. We report wealth as PER-QUINTILE (raw wealth_q)
## so the OR matches the manuscript wording, with covariates matching script 11.
## Random effects: (1|vcode)+(1|uid) where estimable; on the reacher subset the
## child effect is dropped and we fall back to glm if singular.
covars_full <- "factor(age_grp) + factor(sexr) + mother_edu + access_barrier_z + monsoon"

## force_glm: for the survival outcomes ~95% of children survive, so a mixed
## model is near-separated and returns a singular/degenerate fit (unstable ORs);
## we fit those with plain fixed-effects logistic regression, which is stable.
fit_wealth <- function(yvar, data, label, add_severity = FALSE, simple_re = FALSE,
                       force_glm = FALSE) {
  sev  <- if (add_severity) " + factor(lri_cat)" else ""
  re   <- if (simple_re) "(1 | vcode)" else "(1 | vcode) + (1 | uid)"
  fml  <- paste0(yvar, " ~ wealth_q + ", covars_full, sev, " + ", re)
  res  <- NULL
  if (has_lme4 && !force_glm) {
    m <- try(lme4::glmer(as.formula(fml), data = data, family = binomial,
             control = lme4::glmerControl(optimizer = "bobyqa")), silent = TRUE)
    if (!inherits(m, "try-error")) {
      tt <- broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE, conf.int = TRUE)
      res <- tt %>% filter(term == "wealth_q") %>%
        transmute(model = label, n = nobs(m), fit = "glmer",
                  OR_per_quintile = round(estimate, 3),
                  conf.low = round(conf.low, 3), conf.high = round(conf.high, 3),
                  p.value = signif(p.value, 3))
    }
  }
  if (is.null(res)) {                      # fall back to fixed-effects glm
    fml2 <- paste0(yvar, " ~ wealth_q + ", covars_full, sev)
    g <- glm(as.formula(fml2), data = data, family = binomial)
    ct <- summary(g)$coefficients["wealth_q", ]
    res <- data.frame(model = label, n = nobs(g),
                      fit = if (force_glm) "glm (fixed-effects)" else "glm (RE singular)",
                      OR_per_quintile = round(exp(ct[1]), 3),
                      conf.low = round(exp(ct[1] - 1.96 * ct[2]), 3),
                      conf.high = round(exp(ct[1] + 1.96 * ct[2]), 3),
                      p.value = signif(ct[4], 3))
  }
  res
}

hdr("23(3) — WEALTH TERM (per quintile) across the decomposition")
dec <- bind_rows(
  fit_wealth("reached_facility", d, "(a) crude facility contact  [all severe]"),
  fit_wealth("survived",         d, "(b) survival, severity-UNadj [all severe]", force_glm = TRUE),
  fit_wealth("survived",         d, "(c) survival, severity-adj  [all severe]", add_severity = TRUE, force_glm = TRUE),
  fit_wealth("survived", filter(d, reached_facility == 1),
             "(d) survival AMONG reachers [sev-adj]", add_severity = TRUE, force_glm = TRUE),
  fit_wealth("reached_survived", d, "(e) effective care = reached&survived [all severe]")
)
print(as.data.frame(dec), row.names = FALSE)
write_csv(dec, file.path(OUT_DIR, "23_wealth_decomposition_models.csv"))

cat("\nHow to read this:\n",
    "- If (a) ~1 and (e) <1, the gradient is NOT in contact.\n",
    "- Compare (b) vs (c): if adjusting for severity moves the OR back toward 1,\n",
    "    the 'gradient' is largely a wealth->severity-mix artefact.\n",
    "- (d) isolates survival conditional on reaching a facility (collider-prone):\n",
    "    a strong OR<1 here with few events = fragile, interpret with caution.\n",
    "- Watch the event counts (n_reached, deaths): small numbers = wide CIs.\n")

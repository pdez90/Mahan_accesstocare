## =============================================================================
## 11_refined_outcomes.R — refined access-to-care outcomes (NOT yet in manuscript)
##
## Reproduces:
##   (1) the care-pathway typology of severe/very-severe episodes + a figure
##   (2) two refined outcomes modelled like the primary one:
##         A. timely/effective facility contact (reached & NOT died <=24h)
##         B. effective care ending in survival (reached & survived)
##   (3) the recovery-at-follow-up confounding-by-severity check
##   (4) an exploration of the referral variable (semantics UNCONFIRMED — see note)
##
## Writes to output/: 11_pathway_typology.csv, 11_pathway_typology.png,
##   11_refined_models.csv, 11_recovery_by_severity.csv, 11_referral_crosstab.csv
##
## Depends on script 08 (for `sv` with SES/access/monsoon merged, and `va`).
## =============================================================================
if (!exists("va") || !exists("sv")) source("08_access_index_and_individual_model.R")
suppressPackageStartupMessages(library(ggplot2))

## ---- attach death timing/location (reliable copy from the deaths file) ------
dtiming <- deaths %>%
  filter(lri_cat %in% c(1, 2)) %>%
  transmute(studyid, death_loc = death_location, died_within24 = died_lt_1d)

svr <- sv %>%
  left_join(dtiming, by = "studyid") %>%
  mutate(
    died           = as.integer(lrti_death == 1),
    futile_contact = as.integer(reached_facility == 1 & died == 1 &
                                  !is.na(died_within24) & died_within24 == 1),
    ## refined outcomes -------------------------------------------------------
    reached_survived = as.integer(reached_facility == 1 & died == 0),
    timely_effective = as.integer(reached_facility == 1 & futile_contact == 0)
  )

## ---- (1) care-pathway typology ---------------------------------------------
svr <- svr %>% mutate(pathway = case_when(
  reached_facility == 1 & died == 0                         ~ "Reached facility & survived",
  reached_facility == 1 & died == 1 & futile_contact == 1   ~ "Reached facility, died <=24h (too late)",
  reached_facility == 1 & died == 1                         ~ "Reached facility, died >24h",
  reached_facility == 0 & died == 0                         ~ "Community-managed & survived",
  reached_facility == 0 & died == 1                         ~ "Community-managed & died",
  TRUE ~ "unclassified"))

typ <- svr %>% count(pathway) %>% mutate(pct = round(100 * n / sum(n), 1))
hdr("11(1) — CARE-PATHWAY TYPOLOGY (severe/very-severe episodes)")
print(typ); cat("Total:", nrow(svr), "\n")
write_csv(typ, file.path(OUT_DIR, "11_pathway_typology.csv"))

lev  <- c("Community-managed & survived", "Reached facility & survived",
          "Reached facility, died >24h", "Reached facility, died <=24h (too late)",
          "Community-managed & died")
cols <- c("Community-managed & survived"="#9ecae1", "Reached facility & survived"="#0072B2",
          "Reached facility, died >24h"="#E69F00", "Reached facility, died <=24h (too late)"="#D55E00",
          "Community-managed & died"="#7f2704")
typ_f <- typ %>% mutate(pathway = factor(pathway, levels = rev(lev)))
p <- ggplot(typ_f, aes(1, n, fill = pathway)) +
  geom_col(width = 0.6) + coord_flip() +
  scale_fill_manual(values = cols, name = NULL, breaks = lev) +
  geom_text(aes(label = ifelse(n > 150, paste0(n, "\n(", pct, "%)"), "")),
            position = position_stack(vjust = 0.5), size = 3, colour = "white", fontface = "bold") +
  labs(title = "Care pathways of severe / very-severe LRTI episodes",
       subtitle = "Blue = survived; orange/brown = died. 'Reached a facility' splits into survivors and futile late arrivals.",
       x = NULL, y = "Episodes") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 8),
        plot.subtitle = element_text(size = 9)) +
  guides(fill = guide_legend(nrow = 3))
ggsave(file.path(OUT_DIR, "11_pathway_typology.png"), p, width = 8, height = 3.6, dpi = 200, bg = "white")

## ---- (2) refined outcomes: descriptive + modelled --------------------------
hdr("11(2) — REFINED OUTCOMES vs the crude 'reached a facility'")
summ <- tibble(
  outcome = c("Reached a facility (crude)", "Timely/effective (reached & not <=24h death)",
              "Effective & survived (reached & survived)"),
  n = c(sum(svr$reached_facility), sum(svr$timely_effective), sum(svr$reached_survived)),
  pct = round(100 * c(mean(svr$reached_facility), mean(svr$timely_effective),
                      mean(svr$reached_survived)), 1))
print(summ)
write_csv(summ, file.path(OUT_DIR, "11_refined_summary.csv"))

## model each refined outcome exactly like the primary facility-contact model
fit_refined <- function(yvar) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  f <- as.formula(paste0(yvar,
    " ~ factor(age_grp) + factor(sexr) + scale(wealth_q) + mother_edu + ",
    "access_barrier_z + monsoon + (1 | vcode) + (1 | uid)"))
  m <- lme4::glmer(f, data = svr, family = binomial,
                   control = lme4::glmerControl(optimizer = "bobyqa"))
  cat("\n---", yvar, "— N =", nobs(m), "episodes from",
      lme4::ngrps(m)["uid"], "children ---\n")
  out <- broom.mixed::tidy(m, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(outcome = yvar) %>%
    select(outcome, term, estimate, conf.low, conf.high, p.value)
  print(out, n = 20); out
}
mods <- bind_rows(lapply(c("reached_survived", "timely_effective"), fit_refined))
if (!is.null(mods)) write_csv(mods, file.path(OUT_DIR, "11_refined_models.csv"))

## ---- (3) recovery at follow-up: confounding by severity --------------------
hdr("11(3) — RECOVERY AT 1-WEEK FOLLOW-UP by severity x facility contact (CONFOUNDED)")
rec <- svr %>%
  group_by(lri_cat, reached_facility) %>%
  summarise(n = n(), recovered_fu1 = round(100 * mean(recover_fu1_episode == 1, na.rm = TRUE), 1),
            .groups = "drop") %>%
  mutate(severity = ifelse(lri_cat == 1, "very severe", "severe"),
         facility = ifelse(reached_facility == 1, "reached", "community"))
print(rec)
cat("\nNote: within each severity, reached-facility episodes recover LESS often — sicker\n",
    "children are the ones taken to hospital. Do NOT use raw recovery as an access metric.\n")
write_csv(rec, file.path(OUT_DIR, "11_recovery_by_severity.csv"))

## ---- (4) referral variable — EXPLORATION ONLY (semantics unconfirmed) ------
hdr("11(4) — REFERRAL variable exploration (CONFIRM MEANING WITH ROWENA before use)")
ref_ct <- svr %>% count(referred_hosp_episode, reached_facility) %>%
  tidyr::pivot_wider(names_from = reached_facility, values_from = n,
                     names_prefix = "reached_", values_fill = 0)
print(ref_ct)
cat("\nCAUTION: referred_hosp_episode==1 almost never coincides with reached_facility==1,\n",
    "so the two flags likely describe DIFFERENT care steps, not universal referral failure.\n",
    "Referral-completion is only interpretable once this is confirmed.\n")
write_csv(ref_ct, file.path(OUT_DIR, "11_referral_crosstab.csv"))

cat("\nDone. Refined-outcome outputs written to", OUT_DIR, "\n")

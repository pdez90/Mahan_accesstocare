## =============================================================================
## 04_outcome1_facility_contact.R
## OUTCOME 1 — % of severe & very-severe episodes that reached a health facility,
## overall and by severity, age group, sex, and preterm status.
## =============================================================================
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")

sv <- ari %>% filter(sev_vs)   # denominator: severe or very severe episodes

hdr("04 — OUTCOME 1: facility contact for severe disease")
cat("Denominator (severe or very severe episodes):", nrow(sv), "\n\n")

o1_overall <- reached_by(sv)
o1_sev     <- reached_by(sv, "lri_cat")   # 1 = very severe, 2 = severe
o1_age     <- reached_by(sv, "age_grp")   # 1:0-6d 2:7-27d 3:28-89d 4:90-179d 5:180-364d 6:365-730d
o1_sex     <- reached_by(sv, "sexr")      # 1 = male, 2 = female
o1_preterm <- reached_by(sv %>% filter(!is.na(preterm)), "preterm")  # EXPLORATORY

cat("Overall:\n");            print(o1_overall)
cat("\nBy severity (1=very severe, 2=severe):\n"); print(o1_sev)
cat("\nBy age group:\n");     print(o1_age)
cat("\nBy sex (1=male, 2=female):\n"); print(o1_sex)
cat("\nBy preterm (EXPLORATORY; 1=preterm, 0=term):\n"); print(o1_preterm)

cat("\nSevere/very-severe episodes:", nrow(sv),
    "contributed by", n_distinct(sv$uid), "distinct children.\n")

## adjusted model 1: simple logistic (episodes treated as independent)
m1 <- glm(reached_facility ~ factor(age_grp) + factor(sexr),
          data = sv, family = binomial)
cat("\nModel N:", nobs(m1), "of", nrow(sv),
    "severe/vsev episodes (", nrow(sv) - nobs(m1), "dropped for missing sex).\n")
cat("Logistic model (odds ratios), reaching a facility ~ age + sex:\n")
print(tidy(m1, exponentiate = TRUE, conf.int = TRUE))

## adjusted model 2 (PRIMARY): mixed-effects logistic with a random intercept
## for the child, because many children contribute more than one episode.
## Requires lme4 (+ broom.mixed for tidy output).
m1_me <- NULL
if (requireNamespace("lme4", quietly = TRUE)) {
  m1_me <- lme4::glmer(
    reached_facility ~ factor(age_grp) + factor(sexr) + (1 | uid),
    data = sv, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa"))
  cat("\nMixed-effects logistic (random intercept for child) — N =", nobs(m1_me),
      "episodes from", lme4::ngrps(m1_me)["uid"], "children:\n")
  if (requireNamespace("broom.mixed", quietly = TRUE)) {
    me_tidy <- broom.mixed::tidy(m1_me, effects = "fixed",
                                 exponentiate = TRUE, conf.int = TRUE)
    print(me_tidy)
    cat("Between-child random-intercept variance:\n")
    print(as.data.frame(lme4::VarCorr(m1_me)))
    write_csv(me_tidy, file.path(OUT_DIR, "04_o1_model_mixed.csv"))
  } else {
    print(summary(m1_me)$coefficients)
  }
} else {
  message("lme4 not installed: skipping mixed model. ",
          "Install with install.packages(c('lme4','broom.mixed')).")
}

## save
for (nm in c("o1_overall","o1_sev","o1_age","o1_sex","o1_preterm"))
  write_csv(get(nm), file.path(OUT_DIR, paste0("04_", nm, ".csv")))
write_csv(tidy(m1, exponentiate = TRUE, conf.int = TRUE),
          file.path(OUT_DIR, "04_o1_model_glm.csv"))

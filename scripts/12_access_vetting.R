## =============================================================================
## 12_access_vetting.R — VET candidate access-to-care variables (exploratory)
##
## Tests every candidate predictor against a refined access outcome so you can
## review them in one table and decide which to keep. Nothing here is in the
## manuscript yet.
##
## Outcome (swap freely): "reached_survived" (reached a facility AND survived).
##   Alternatives: "reached_facility" (crude), "timely_effective".
##
## For each predictor we fit a mixed-effects logistic model with random
## intercepts for village and child:  outcome ~ predictor + (1|vcode) + (1|uid)
##   - binary / continuous predictors: odds ratio (per SD if continuous) + Wald p
##   - multi-level factors: likelihood-ratio p (see the by-level model separately)
##
## Writes: output/12_access_vetting.csv, output/12_multivariable.csv,
##         output/12_vetting_forest.png, output/12_referral_provisional.csv
## =============================================================================
if (!exists("va") || !exists("sv")) source("08_access_index_and_individual_model.R")
suppressPackageStartupMessages({library(ggplot2); library(lme4); library(broom.mixed)})

OUTCOME <- "reached_survived"

num  <- function(x) suppressWarnings(as.numeric(ifelse(trimws(x) %in% c("","50 mtr"),
          ifelse(trimws(x)=="50 mtr","0.05",NA), x)))
bin1 <- function(x) as.integer(startsWith(trimws(toupper(as.character(x))), "1"))

## ---- village-level predictors (survey + GIS) -------------------------------
vil2 <- vil %>% transmute(
  vcode,
  dist_phc        = num(distance_primary_center_km),
  dist_subctr     = num(distance_sub_center_km),
  bus_stand_km    = num(bus_stand_dist_km),
  busfare_rs      = num(cost_busfare_rs),
  motorbike_fare  = num(Motorbike_fare_rs),
  bus_in_village  = bin1(bus_village),
  flood_cutoff    = bin1(river_floods_rainyseason_cuts_o),
  health_ngo      = bin1(presence_health_ngo),
  subctr_in_vil   = bin1(sub_center_in_village),
  panchayat_health= bin1(pancht_supp_health)
) %>% left_join(va %>% select(vcode, access_barrier_z), by = "vcode")

## GIS straight-line distance to MAHAN (median household GPS), if available -----
GIS <- Sys.getenv("RSV_GIS_DIR",
  "/Users/priyanka/Downloads/RSV_India/GISproject/GISProject/")
hh_path <- file.path(GIS, "capstone project Mattie/critical_data_gpsdata.csv")
if (file.exists(hh_path)) {
  hh <- read_csv(hh_path, show_col_types = FALSE)
  hh <- hh[!is.na(hh$latitude) & hh$latitude>20.5 & hh$latitude<22.5 &
             hh$longitude>76 & hh$longitude<78, ]
  hh$vcode <- as.numeric(sub("^([0-9]+).*", "\\1", hh$code_village))
  cent <- hh %>% group_by(vcode) %>%
    summarise(lon=median(longitude), lat=median(latitude), .groups="drop") %>%
    mutate(dist_mahan_km = 6371*2*asin(sqrt(
      sin((21.543-lat)*pi/360)^2 + cos(lat*pi/180)*cos(21.543*pi/180)*
      sin((76.959-lon)*pi/360)^2)))
  vil2 <- vil2 %>% left_join(cent %>% select(vcode, dist_mahan_km), by="vcode")
} else { vil2$dist_mahan_km <- NA_real_ }

## ---- household / child predictors + build analytic table -------------------
hh_ses <- dem %>% transmute(
  uid,
  wealth_q       = as.numeric(quintile_rural),
  mother_edu_gt  = as.integer(mother_edu_categorized == "more than primary"),
  father_edu_gt  = as.integer(father_edu_categorized %in%
                    c("incomplete secondary","complete secondary","higher")),
  mother_employed= as.integer(mother_employed_categorized == "Employed"),
  electricity    = as.integer(!is.na(electricityr) & electricityr != "no electricity"),
  toilet_yes     = as.integer(toiletfacilityr == 1)
)

dat <- ari %>% filter(sev_vs) %>%
  mutate(reached_facility = as.integer(community == 0),
         died             = as.integer(lrti_death == 1),
         reached_survived = as.integer(reached_facility == 1 & died == 0),
         timely_effective = reached_facility,   # refined variant computed in 11
         monsoon          = as.integer(ai_month %in% 6:9),
         severity_vs      = as.integer(lri_cat == 1),        # 1 = very severe
         referred         = as.integer(referred_hosp_episode == 1)) %>%
  left_join(hh_ses, by = "uid") %>%
  left_join(vil2,   by = "vcode")

cat("Vetting outcome:", OUTCOME, "| severe/vsev episodes:", nrow(dat),
    "| events:", sum(dat[[OUTCOME]]), "\n\n")

## ---- predictor catalogue ---------------------------------------------------
## type: "cont" (per-SD OR), "bin" (OR vs reference), "factor" (LRT only)
P <- list(
  list(n="severity_vs",     lab="Very severe (vs severe)",            grp="Child",     type="bin"),
  list(n="age_grp",         lab="Age group (6 cat)",                  grp="Child",     type="factor"),
  list(n="sexr",            lab="Female (vs male)",                   grp="Child",     type="bin"),
  list(n="monsoon",         lab="Monsoon episode (Jun–Sep)",          grp="Temporal",  type="bin"),
  list(n="wealth_q",        lab="Household wealth quintile (per SD)", grp="Household",  type="cont"),
  list(n="mother_edu_gt",   lab="Maternal educ > primary",            grp="Household",  type="bin"),
  list(n="father_edu_gt",   lab="Paternal educ > primary",            grp="Household",  type="bin"),
  list(n="mother_employed", lab="Mother employed",                    grp="Household",  type="bin"),
  list(n="electricity",     lab="Household electricity",              grp="Household",  type="bin"),
  list(n="toilet_yes",      lab="Household toilet",                   grp="Household",  type="bin"),
  list(n="dist_phc",        lab="Distance to PHC, km (per SD)",       grp="Village geo",type="cont"),
  list(n="dist_mahan_km",   lab="GIS distance to MAHAN, km (per SD)", grp="Village geo",type="cont"),
  list(n="dist_subctr",     lab="Distance to sub-centre, km (per SD)",grp="Village geo",type="cont"),
  list(n="bus_stand_km",    lab="Distance to bus stand, km (per SD)", grp="Village geo",type="cont"),
  list(n="bus_in_village",  lab="Bus service in village",             grp="Village geo",type="bin"),
  list(n="busfare_rs",      lab="Bus fare, ₹ (per SD)",               grp="Village cost",type="cont"),
  list(n="motorbike_fare",  lab="Motorbike fare, ₹ (per SD)",         grp="Village cost",type="cont"),
  list(n="flood_cutoff",    lab="Road cut off in rainy season",       grp="Village geo",type="bin"),
  list(n="health_ngo",      lab="Health NGO present",                 grp="Village health",type="bin"),
  list(n="subctr_in_vil",   lab="Sub-centre in village",              grp="Village health",type="bin"),
  list(n="panchayat_health",lab="Panchayat supports health",          grp="Village health",type="bin"),
  list(n="access_barrier_z",lab="Composite access-barrier index (SD)",grp="Village geo",type="cont"),
  list(n="referred",        lab="Referred to hospital (PROVISIONAL)", grp="Referral",  type="bin")
)

## ---- fit one univariable mixed model per predictor -------------------------
ctrl <- glmerControl(optimizer = "bobyqa")
vet_one <- function(s) {
  d <- dat[!is.na(dat[[s$n]]), ]
  navail <- nrow(d)
  rhs <- if (s$type == "cont") paste0("scale(", s$n, ")") else paste0("factor(", s$n, ")")
  f1 <- as.formula(paste(OUTCOME, "~", rhs, "+ (1|vcode) + (1|uid)"))
  m1 <- tryCatch(glmer(f1, d, binomial, control = ctrl), error = function(e) NULL)
  if (is.null(m1)) return(data.frame(group=s$grp, variable=s$lab, n=navail,
                                     OR="—", CI="—", p="model failed", p_raw=NA_real_))
  if (s$type == "factor") {
    m0 <- glmer(as.formula(paste(OUTCOME, "~ 1 + (1|vcode) + (1|uid)")), d, binomial, control = ctrl)
    p  <- tryCatch(anova(m0, m1)[["Pr(>Chisq)"]][2], error = function(e) NA)
    data.frame(group=s$grp, variable=s$lab, n=navail, OR="(multi-level)", CI="LRT",
               p=ifelse(is.na(p),"—",ifelse(p<0.001,"<0.001",sprintf("%.3f",p))), p_raw=p)
  } else {
    tt <- broom.mixed::tidy(m1, effects="fixed", exponentiate=TRUE, conf.int=TRUE)
    r  <- tt[2, ]  # the predictor term
    data.frame(group=s$grp, variable=s$lab, n=navail,
               OR=sprintf("%.2f", r$estimate),
               CI=sprintf("%.2f–%.2f", r$conf.low, r$conf.high),
               p=ifelse(r$p.value<0.001,"<0.001",sprintf("%.3f", r$p.value)), p_raw=r$p.value)
  }
}

hdr("12 — ACCESS-TO-CARE VARIABLE VETTING (univariable, outcome = reached & survived)")
vet <- do.call(rbind, lapply(P, vet_one))

## Benjamini-Hochberg false-discovery-rate control across the exploratory screen
## (the provisional referral flag is excluded, as it is not interpreted).
fam <- !is.na(vet$p_raw) & vet$variable != "Referred to hospital (PROVISIONAL)"
vet$q_raw <- NA_real_
vet$q_raw[fam] <- p.adjust(vet$p_raw[fam], method = "BH")
vet$q <- ifelse(is.na(vet$q_raw), "—",
                ifelse(vet$q_raw < 0.001, "<0.001", sprintf("%.3f", vet$q_raw)))

print(vet[, setdiff(names(vet), c("p_raw","q_raw"))], row.names = FALSE)
write_csv(vet[, setdiff(names(vet), c("p_raw","q_raw"))], file.path(OUT_DIR, "12_access_vetting.csv"))
cat(sprintf("\nFDR family: %d tests. Survive q < 0.05: %s\n", sum(fam),
            paste(vet$variable[which(vet$q_raw < 0.05)], collapse = " | ")))

## ---- forest plot of the binary/continuous predictors -----------------------
fp <- vet %>% filter(OR != "(multi-level)", OR != "—", p != "model failed",
                     variable != "Referred to hospital (PROVISIONAL)") %>%
  mutate(est = as.numeric(OR),
         lo  = as.numeric(sub("–.*","",CI)), hi = as.numeric(sub(".*–","",CI)),
         sig = ifelse(!is.na(q_raw) & q_raw < 0.05, "q < 0.05 (FDR)", "n.s."),
         variable = factor(variable, levels = rev(variable)))
pf <- ggplot(fp, aes(est, variable, colour = sig)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey60") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_colour_manual(values = c("q < 0.05 (FDR)"="#0072B2","n.s."="grey55"), name = NULL) +
  labs(title = "Access-to-care predictors of reaching a facility and surviving",
       subtitle = "Univariable mixed models (village + child random intercepts); significance after Benjamini–Hochberg FDR. OR on log scale.",
       x = "Odds ratio (95% CI)", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.subtitle = element_text(size = 8))
ggsave(file.path(OUT_DIR, "12_vetting_forest.png"), pf, width = 8, height = 6, dpi = 200, bg = "white")

## ---- multivariable model (household + key village + temporal) --------------
hdr("12b — MULTIVARIABLE MODEL (candidate keepers, adjusted)")
mv <- tryCatch(glmer(
  reformulate(c("factor(age_grp)","factor(sexr)","scale(wealth_q)","mother_edu_gt",
                "father_edu_gt","monsoon","scale(dist_mahan_km)","bus_in_village",
                "flood_cutoff","access_barrier_z","(1|vcode)","(1|uid)"), OUTCOME),
  dat, binomial, control = ctrl), error = function(e) NULL)
if (!is.null(mv)) {
  mvt <- broom.mixed::tidy(mv, effects="fixed", exponentiate=TRUE, conf.int=TRUE) %>%
    select(term, estimate, conf.low, conf.high, p.value)
  print(mvt, n = 30)
  cat("N =", nobs(mv), "episodes.\n")
  write_csv(mvt, file.path(OUT_DIR, "12_multivariable.csv"))
}

## ---- referral variable — PROVISIONAL descriptive (confirm with Rowena) ------
hdr("12c — REFERRAL COMPLETION (PROVISIONAL — confirm variable meaning with Rowena)")
reftab <- dat %>% filter(!is.na(referred)) %>%
  group_by(referred) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct_reached = round(100*mean(reached_facility), 1),
            n_died = sum(died), pct_died = round(100*mean(died), 1), .groups="drop")
print(reftab)
cat("\nNOTE: referred==1 rarely coincides with reaching a facility, so these flags may\n",
    "describe different care steps. Interpret referral completion only after confirmation.\n")
write_csv(reftab, file.path(OUT_DIR, "12_referral_provisional.csv"))

cat("\nDone. Vetting outputs written to", OUT_DIR, "\n")

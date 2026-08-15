## =============================================================================
## 02_diagnostics.R — data availability diagnostics
## For each file: which variables exist and for how many records they are
## populated. Also subject / household-proxy / village counts and the value
## distributions of the key classification variables.
## Writes CSVs to output/ and prints summaries to the console.
## =============================================================================
if (!exists("ari")) source("01_load_data.R")

## ---- 2.1 variable availability (completeness) ------------------------------
hdr("02.1 — VARIABLE AVAILABILITY: ari.csv (episodes)")
avail_ari <- availability(ari)
print(avail_ari, row.names = FALSE)
write_csv(avail_ari, file.path(OUT_DIR, "02_availability_ari.csv"))

hdr("02.2 — VARIABLE AVAILABILITY: lrtideaths.csv (deaths)")
avail_deaths <- availability(deaths)
print(avail_deaths, row.names = FALSE)
write_csv(avail_deaths, file.path(OUT_DIR, "02_availability_deaths.csv"))

## demographics has ~369 columns: print a curated key set, save the full table.
key_dem <- c("uid","studytid","sexr","code_village_93","village","tahsil",
             "birth_date","gest_age_wk","birth_weight_kg","cohort","study_year",
             "subset_33_villages","village_93_flag","final_disp","date_of_death",
             "wealthscore_rural","quintile_rural","mother_edu_categorized",
             "father_edu_categorized","electricityr","fuel","toiletfacilityr",
             "water_source","children_0to5","adults_in_house")
key_dem <- key_dem[key_dem %in% names(dem)]
hdr("02.3 — VARIABLE AVAILABILITY: demographics.csv (key variables)")
print(availability(dem, key_dem), row.names = FALSE)
write_csv(availability(dem),        file.path(OUT_DIR, "02_availability_demographics_FULL.csv"))
write_csv(availability(dem, key_dem), file.path(OUT_DIR, "02_availability_demographics_key.csv"))

## ---- 2.2 subject / household / village counts ------------------------------
hdr("02.4 — SUBJECT, HOUSEHOLD-PROXY, AND VILLAGE COUNTS")
counts <- tibble(
  metric = c(
    "Distinct children (uid) in demographics",
    "Distinct children (uid) with an ARI/LRTI episode",
    "Distinct children (uid) among LRTI deaths",
    "Distinct villages in demographics (code_village_93)",
    "Distinct villages among episodes (code_village)",
    "Children with non-missing gestational age (gest_age_wk)",
    "Children with non-missing wealth quintile (quintile_rural)"),
  n = c(
    n_distinct(dem$uid),
    n_distinct(ari$uid),
    n_distinct(deaths$uid),
    n_distinct(dem$code_village_93),
    n_distinct(ari$code_village),
    sum(!is.na(dem$gest_age_wk)),
    sum(!is.na(dem$quintile_rural)))
)
print(counts)
write_csv(counts, file.path(OUT_DIR, "02_counts.csv"))

## ---- 2.3 value distributions of key classification variables ---------------
hdr("02.5 — VALUE DISTRIBUTIONS: episode classification variables")
episode_vars <- c("lri_cat","community","hosp_lri","age_grp","sexr","timeline",
                  "referred_hosp_episode","first_source","lrti_death")
dist_ari <- bind_rows(lapply(episode_vars, freq, df = ari))
print(dist_ari, n = 100)
write_csv(dist_ari, file.path(OUT_DIR, "02_distributions_ari.csv"))

hdr("02.6 — VALUE DISTRIBUTIONS: death location / timing variables")
death_vars <- c("death_location","community_type","hosp_type","lri_cat",
                "died_lt_1d","died_1to7d","died_8to14d","died_14to30d")
dist_deaths <- bind_rows(lapply(death_vars, freq, df = deaths))
print(dist_deaths, n = 100)
write_csv(dist_deaths, file.path(OUT_DIR, "02_distributions_deaths.csv"))

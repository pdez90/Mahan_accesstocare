## =============================================================================
## 03_derive_and_merge.R — derived variables and all merges (with diagnostics)
## Adds severity / care-location / preterm flags, merges child gestational age
## onto episodes, builds the numeric village-code key, and loads + cleans the
## village survey. Reports how many records match at each merge.
## =============================================================================
if (!exists("ari")) source("01_load_data.R")

## ---- 3.1 episode-level derived variables -----------------------------------
## Severity from lri_cat: 1 = very severe, 2 = severe, 3 = non-severe
## Care location from `community`: 1 = community-managed, 0 = facility
ari <- ari %>%
  mutate(
    sev_vs           = lri_cat %in% c(1, 2),        # severe OR very severe
    reached_facility = as.integer(community == 0),  # reached a health facility
    death            = as.integer(lrti_death == 1)
  )

## ---- 3.2 preterm from parental-survey gestational age ----------------------
## NOTE: gest_age_wk comes from parental recall; values are heaped at 36 weeks
## and give an implausibly high preterm fraction. Treat preterm as EXPLORATORY.
dem <- dem %>%
  mutate(
    gest_age_wk_clean = ifelse(gest_age_wk >= 20 & gest_age_wk <= 45,
                               gest_age_wk, NA_real_),
    preterm = ifelse(!is.na(gest_age_wk_clean),
                     as.integer(gest_age_wk_clean < 37), NA_integer_)
  )

hdr("03.1 — GESTATIONAL AGE (parental survey) — reliability check")
cat("Distribution of gest_age_wk (cleaned to 20-45 wk):\n")
print(summary(dem$gest_age_wk_clean))
cat("\nApparent preterm fraction among children with GA:\n")
print(round(prop.table(table(dem$preterm)), 3))

## ---- 3.3 merge child gestational age onto episodes -------------------------
ari <- ari %>% left_join(dem %>% select(uid, preterm, gest_age_wk_clean),
                         by = "uid")
hdr("03.2 — MERGE: episodes <- child gestational age (by uid)")
merge1 <- tibble(
  step            = "ari.csv left-join demographics (by uid)",
  episodes        = nrow(ari),
  matched_a_child = sum(ari$uid %in% dem$uid),
  with_GA         = sum(!is.na(ari$gest_age_wk_clean))
)
print(merge1)

## ---- 3.4 numeric village-code key and village survey -----------------------
ari    <- ari    %>% mutate(vcode = as.numeric(sub("^([0-9]+).*", "\\1", code_village)))
deaths <- deaths %>% mutate(vcode = as.numeric(sub("^([0-9]+).*", "\\1", code_village_93)))

vil <- read_dta(VIL_PATH) %>%
  mutate(
    vcode = as.numeric(village_code),
    distance_primary_center_km = suppressWarnings(as.numeric(
      ifelse(distance_primary_center_km == "50 mtr", "0.05",
             distance_primary_center_km))),
    bus_stand_dist_km = suppressWarnings(as.numeric(bus_stand_dist_km)),
    bus_yes = as.integer(startsWith(as.character(bus_village), "1"))
  )

hdr("03.3 — MERGE: village-level survey (numeric village-code prefix)")
merge2 <- tibble(
  step = c("episode villages matched to survey",
           "death villages matched to survey"),
  n_villages_in_data = c(n_distinct(ari$vcode), n_distinct(deaths$vcode)),
  n_matched          = c(n_distinct(ari$vcode[ari$vcode %in% vil$vcode]),
                         n_distinct(deaths$vcode[deaths$vcode %in% vil$vcode]))
)
print(merge2)

write_csv(bind_rows(merge1 %>% mutate(across(everything(), as.character)) %>%
                      pivot_longer(everything(), names_to = "field", values_to = "value")),
          file.path(OUT_DIR, "03_merge_episode_child.csv"))
write_csv(merge2, file.path(OUT_DIR, "03_merge_village.csv"))

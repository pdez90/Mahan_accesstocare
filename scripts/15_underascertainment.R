## =============================================================================
## 15_underascertainment.R — quantify how many child pneumonia deaths would be
## invisible to facility-based / vital-registration systems that lack the active
## community verbal-autopsy surveillance used in this cohort.
##
## Logic: MAHAN community surveillance ascertained ALL 205 fatal LRTI episodes.
## A routine facility/vital-registration system captures a death only if it
## occurs at a facility. The share dying in the community is therefore the share
## such systems would miss. Every count below is computed from the deaths file.
## Writes: output/15_underascertainment.csv
## =============================================================================
if (!exists("hdr"))    source("00_setup.R")
if (!exists("deaths")) source("01_load_data.R")

N <- nrow(deaths)                                     # ascertained LRTI deaths (205)

## --- Definition A: pre-specified community-vs-facility outcome (Outcome 2) ---
fac_A <- sum(deaths$community == 0, na.rm = TRUE)     # died in facility care
com_A <- sum(deaths$community == 1, na.rm = TRUE)     # died in the community

## --- Definition B: physical place of death (unambiguous, from death_location)-
hosp_B    <- sum(deaths$death_location == "hospital", na.rm = TRUE)
outside_B <- sum(deaths$death_location %in% c("village", "on way"), na.rm = TRUE)

mk <- function(label, visible, N) data.frame(
  definition          = label,
  visible_at_facility = visible,
  invisible           = N - visible,
  N                   = N,
  ascertained_pct     = round(100 * visible / N, 1),
  undercount_pct      = round(100 * (N - visible) / N, 1),
  correction_factor   = round(N / visible, 2),
  stringsAsFactors    = FALSE)

tab <- rbind(
  mk("A: community vs facility (pre-specified)", fac_A,  N),
  mk("B: place of death = hospital",             hosp_B, N))

hdr("15 — UNDER-ASCERTAINMENT: child LRTI deaths invisible to facility systems")
print(tab, row.names = FALSE)
write_csv(tab, file.path(OUT_DIR, "15_underascertainment.csv"))

lo <- min(fac_A, hosp_B); hi <- max(fac_A, hosp_B)
cat(sprintf("\nCommunity surveillance ascertained %d LRTI deaths.\n", N))
cat(sprintf("Facility-based reporting would have captured %d-%d of them (%.0f-%.0f%%),\n",
            lo, hi, 100 * lo / N, 100 * hi / N))
cat(sprintf("missing the %d-%d (%.0f-%.0f%%) that died in the community or in transit.\n",
            N - hi, N - lo, 100 * (N - hi) / N, 100 * (N - lo) / N))
cat(sprintf("=> facility-only counting undercounts by %.0f-%.0f%%; the true toll is\n",
            100 * (N - hi) / N, 100 * (N - lo) / N))
cat(sprintf("   ~%.1f-%.1f times the facility-visible count.\n", N / hi, N / lo))

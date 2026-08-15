## =============================================================================
## 01_load_data.R — read the raw files and report their dimensions
## Produces: ari, deaths, dem (data frames) + file_summary
## =============================================================================
if (!exists("DATA_DIR")) source("00_setup.R")

ari    <- read_csv(ARI_PATH,   show_col_types = FALSE)   # LRTI/ARI episodes
deaths <- read_csv(DEATH_PATH, show_col_types = FALSE)   # LRTI deaths
dem    <- read_csv(DEM_PATH,   show_col_types = FALSE)   # enrolled children

file_summary <- tibble(
  file       = c("ari.csv", "lrtideaths.csv", "demographics.csv"),
  role       = c("LRTI/ARI episodes", "LRTI deaths",
                 "Enrolled children (demographics/risk factors)"),
  n_rows     = c(nrow(ari), nrow(deaths), nrow(dem)),
  n_cols     = c(ncol(ari), ncol(deaths), ncol(dem)),
  n_subjects = c(n_distinct(ari$uid), n_distinct(deaths$uid), n_distinct(dem$uid))
)

hdr("01 — RAW DATA FILES")
print(file_summary)
write_csv(file_summary, file.path(OUT_DIR, "01_file_summary.csv"))

## =============================================================================
## run_all.R — run the whole Access-to-Care analysis in order.
##
## Usage:
##   1. Open this project in the `scripts/` folder (setwd here), then:
##        source("run_all.R")
##   2. Or from a shell:
##        Rscript run_all.R
##
## Paths default to Priyanka's machine (see 00_setup.R). To run elsewhere,
## set the environment variables before launching, e.g.:
##   RSV_DATA_DIR=/path/to/Data/ RSV_VIL_PATH=/path/to/village_characteristics.dta Rscript run_all.R
##
## All console output is what the manuscript numbers are drawn from; each script
## also writes its tables to the `output/` subfolder as CSVs.
## =============================================================================

## make sure we run from the scripts folder so relative source() paths resolve
this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
if (!is.null(this_dir) && dir.exists(this_dir)) setwd(this_dir)

source("00_setup.R")                    # paths, packages, helpers
source("01_load_data.R")                # read files + dimensions
source("02_diagnostics.R")              # variable availability + counts
source("03_derive_and_merge.R")         # derived vars + merges
source("04_outcome1_facility_contact.R")# Outcome 1
source("05_outcome2_death_location.R")  # Outcome 2
source("06_outcome3_effective_care.R")  # Outcome 3
source("07_village_access.R")           # village-level analysis
source("08_access_index_and_individual_model.R")  # composite index + individual model
source("09_tables_figures.R")           # Table 1 + Figures 1-2
source("10_maps.R")                     # Figures 3-4 (maps) + Moran's I  (needs GISproject GPS)
source("11_refined_outcomes.R")         # refined access outcomes + pathway typology (exploratory)
source("12_access_vetting.R")           # vet all candidate access-to-care variables (exploratory)
source("13_seasonality.R")               # seasonality / RSV-season check
source("14_completeness_table.R")       # Supplementary Table S1 (computed from data)
source("15_underascertainment.R")       # under-ascertainment of community deaths vs facility systems
source("16_rsv_underascertainment.R")   # RSV-specific under-ascertainment (exploratory; small N)
source("17_concentration_index.R")      # wealth-related concentration index of facility contact
source("18_composite_components.R")     # completeness of each composite-index component
source("19_travel_time_accessibility.R")# external Weiss/MAP travel-time accessibility (needs terra + rasters)
source("20_access_measures_map.R")      # SI map of village-level access measures
source("21_death_by_tier_location.R")   # death location by severity tier (Supp. Table S4)
source("22_primary_access_model.R")     # PRIMARY model: Weiss travel time; composite as sensitivity
source("23_wealth_gradient_decomposition.R")  # decompose the inverse wealth gradient in effective care
source("24_sensitivity_first_week.R")   # birth-driven week-1 contact + access sensitivity excluding it

hdr("DONE — all tables written to:")
cat(OUT_DIR, "\n")
cat("\nSession info:\n")
print(sessionInfo())

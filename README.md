# Mahan_accesstocare

Analysis code for the study **"Geographic barriers and the location of childhood
pneumonia deaths in rural tribal India: an access-to-care analysis of a
prospective cohort in Melghat, Maharashtra."**

This repository contains **only the R analysis code**. The underlying
individual- and death-level data are **not** included and are not publicly
shareable, because they contain sensitive information on children and households
in the MAHAN / Melghat cohort, including household geolocation. See
**Data availability** below.

## What the code does

The pipeline quantifies access to facility-based care for severe childhood lower
respiratory tract infection (LRTI): the proportion of severe and very severe
episodes reaching a health facility (by age and sex), the location of death,
and the proportion of deaths occurring without effective facility-based care,
and it tests whether these outcomes track measurable geographic access using
village-level analyses and individual-episode mixed-effects models.

## Requirements

R (>= 4.1) with:

```r
install.packages(c("readr","dplyr","tidyr","haven","broom",
                   "lme4","broom.mixed","ggplot2","sf","terra"))
```

## Running

The scripts read the restricted data files from local paths, which you supply
via environment variables (defaults are set in `scripts/00_setup.R`). From the
`scripts/` folder:

```bash
RSV_DATA_DIR=/path/to/Data/ \
RSV_VIL_PATH=/path/to/village_characteristics.dta \
Rscript run_all.R
```

`run_all.R` sources scripts `00`–`24` in order. All tables and figures are
written to `scripts/output/`, which is git-ignored.

### Inputs required (not provided in this repository)
- `ari.csv` — LRTI/ARI episodes
- `demographics.csv` — enrolled children
- `lrtideaths.csv` — LRTI deaths
- `village_characteristics.dta` — village survey
- GIS / GPS layers and the Malaria Atlas Project travel-time rasters
  (for the mapping and accessibility scripts)

## Script overview

- `00_setup.R` — paths, packages, shared helpers
- `01_load_data.R` — read raw files
- `02_diagnostics.R` — variable availability and counts
- `03_derive_and_merge.R` — derived variables and merges
- `04`–`06` — the three pre-specified outcomes (facility contact; death
  location; deaths beyond effective care)
- `07`–`08` — village-level access and the individual-episode model + composite
  access-barrier index
- `09`–`10` — Table 1, figures, and maps (Moran's I spatial autocorrelation)
- `11`–`13` — refined outcomes, access-variable vetting (with FDR control),
  seasonality
- `14`–`18` — completeness, under-ascertainment, RSV, concentration index,
  composite-component completeness
- `19`–`20` — external travel-time accessibility and access-measure maps
- `21`–`24` — death location by severity tier; primary Weiss travel-time model;
  wealth-gradient decomposition; first-week / place-of-birth sensitivity
- `run_all.R` — runs the whole pipeline

## Data availability

The cohort data cannot be shared publicly owing to the sensitivity of the study
population (young children; household-level geolocation) and the terms of the
ethical approvals. Requests for access may be directed to the study team
(MAHAN Trust) and are subject to institutional and ethics-committee approval.

## Contact

Priyanka deSouza — priyankadesouza@gmail.com

## License

Code is released under the MIT License (see `LICENSE`). The license applies to
the code only and grants no rights to the underlying data.

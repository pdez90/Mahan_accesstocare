## =============================================================================
## 00_setup.R — paths, packages, and shared helper functions
## Access to Care analysis, MAHAN / Melghat childhood pneumonia cohort
##
## Sourced by every other script. Edit the paths here (or set the environment
## variables RSV_DATA_DIR / RSV_VIL_PATH) and nothing else needs to change.
## =============================================================================

## ---- packages --------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(haven)
  library(broom)
})

## ---- paths -----------------------------------------------------------------
## Defaults point to Priyanka's machine. Override by exporting RSV_DATA_DIR and
## RSV_VIL_PATH in the shell, or by editing the two lines below.
DATA_DIR <- Sys.getenv("RSV_DATA_DIR",
                       "/Users/priyanka/Downloads/RSV_India/Final/Data/")
VIL_PATH <- Sys.getenv("RSV_VIL_PATH",
                       "/Users/priyanka/Downloads/RSV_India/village_characteristics.dta")

ARI_PATH   <- file.path(DATA_DIR, "ari.csv")
DEATH_PATH <- file.path(DATA_DIR, "lrtideaths.csv")
DEM_PATH   <- file.path(DATA_DIR, "demographics.csv")

## Where tables/logs are written (an "output" folder next to the scripts).
## Created if missing.
OUT_DIR <- Sys.getenv("RSV_OUT_DIR", file.path(getwd(), "output"))
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

## ---- helpers ---------------------------------------------------------------

## Section header for console logs
hdr <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}

## Variable-availability table: for each column, class, n/percent non-missing,
## number of distinct values, and up to 3 example values.
availability <- function(df, vars = NULL) {
  if (is.null(vars)) vars <- names(df)
  n <- nrow(df)
  rows <- lapply(vars, function(v) {
    x <- df[[v]]
    nonmiss <- sum(!is.na(x) &
                     !(is.character(x) & trimws(as.character(x)) == ""))
    ex <- paste(format(head(unique(x[!is.na(x)]), 3)), collapse = " | ")
    data.frame(variable = v,
               class = paste(class(x), collapse = "/"),
               n_nonmiss = nonmiss,
               pct_complete = round(100 * nonmiss / n, 1),
               n_distinct = dplyr::n_distinct(x, na.rm = TRUE),
               examples = ex,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

## Frequency table (value, n, pct) for one variable, value coerced to character.
freq <- function(df, v) {
  df %>%
    count(.data[[v]]) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    rename(value = 1) %>%
    mutate(variable = v, value = as.character(value)) %>%
    select(variable, value, n, pct)
}

## Proportion "reached a facility", overall or by a grouping variable.
## Explicit analysable denominator: n_total counts all episodes in the group;
## n (the denominator for pct_reached) counts only episodes with a KNOWN
## outcome, so a missing care location can never silently turn a whole
## group's summary into NA, and numerator and denominator always refer to
## the same episodes.
reached_by <- function(df, group = NULL, lab = "Overall") {
  if (is.null(group)) {
    df <- df %>% mutate(.g = lab)
  } else {
    df <- df %>% mutate(.g = as.character(.data[[group]]))
  }
  df %>%
    group_by(.g) %>%
    summarise(n_total = n(),
              n       = sum(!is.na(reached_facility)),
              reached = sum(reached_facility == 1, na.rm = TRUE),
              pct_reached = round(100 * reached / n, 1), .groups = "drop") %>%
    rename(group = .g)
}

message("00_setup.R loaded. DATA_DIR = ", DATA_DIR)

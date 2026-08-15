## =============================================================================
## 17_concentration_index.R — is the facility-access gap socioeconomically
## patterned, or a universal barrier? Computes the wealth-related concentration
## index of reaching a facility for severe/very-severe LRTI episodes. A value
## near 0 means the shortfall is shared across the wealth distribution (a
## universal barrier) rather than concentrated among the poor (pro-rich).
## Erreygers-corrected for the binary outcome; cluster-bootstrap 95% CI over
## children. Writes: output/17_concentration_index.csv
## =============================================================================
if (!exists("hdr")) source("00_setup.R")
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")

sv <- ari %>% filter(sev_vs) %>%
  left_join(dem %>% select(uid, wealthscore_rural, quintile_rural), by = "uid") %>%
  filter(!is.na(wealthscore_rural), !is.na(reached_facility))

## concentration index on a health variable h ranked by wealth score w
ci_fun <- function(h, w) {
  o <- order(w); h <- h[o]
  n <- length(h)
  R  <- (seq_len(n) - 0.5) / n                 # fractional rank in wealth distribution
  mu <- mean(h)
  cv <- sum((h - mu) * (R - mean(R))) / n      # population covariance
  c(C = 2 / mu * cv, E = 8 * cv, mu = mu)       # standard C and Erreygers E (binary range = 1)
}

pt <- ci_fun(sv$reached_facility, sv$wealthscore_rural)

## cluster bootstrap over children (uid)
set.seed(20260715)
idx <- split(seq_len(nrow(sv)), sv$uid)
B <- 2000
bootE <- replicate(B, {
  s    <- sample(names(idx), length(idx), replace = TRUE)
  rows <- unlist(idx[s], use.names = FALSE)
  ci_fun(sv$reached_facility[rows], sv$wealthscore_rural[rows])["E"]
})
ci95 <- quantile(bootE, c(.025, .975), names = FALSE)

hdr("17 — CONCENTRATION INDEX OF FACILITY CONTACT (wealth-related inequality)")
cat(sprintf("Analytic episodes: %d (children: %d); mean facility contact %.1f%%.\n",
            nrow(sv), length(idx), 100 * pt["mu"]))
cat(sprintf("Standard concentration index C = %+.3f\n", pt["C"]))
cat(sprintf("Erreygers-corrected index    E = %+.3f  (95%% CI %+.3f to %+.3f)\n",
            pt["E"], ci95[1], ci95[2]))
cat(ifelse(ci95[1] <= 0 & ci95[2] >= 0,
    "=> CI spans 0: no significant wealth-related inequality — a universal barrier.\n",
    "=> CI excludes 0: facility contact is socioeconomically patterned.\n"))

## flat-gradient check: facility contact by wealth quintile
byq <- sv %>% group_by(quintile_rural) %>%
  summarise(n = n(), reached = sum(reached_facility),
            pct = round(100 * mean(reached_facility), 1), .groups = "drop")
cat("\nFacility contact by wealth quintile (1=poorest .. 5=least poor):\n")
print(as.data.frame(byq), row.names = FALSE)

out <- data.frame(
  metric = c("concentration_index_C", "erreygers_E", "erreygers_lo95", "erreygers_hi95",
             "mean_facility_contact", "n_episodes", "n_children"),
  value  = c(round(pt["C"], 3), round(pt["E"], 3), round(ci95[1], 3), round(ci95[2], 3),
             round(100 * pt["mu"], 1), nrow(sv), length(idx)))
write_csv(out,  file.path(OUT_DIR, "17_concentration_index.csv"))
write_csv(byq,  file.path(OUT_DIR, "17_facility_by_wealth_quintile.csv"))

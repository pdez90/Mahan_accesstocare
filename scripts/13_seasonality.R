## =============================================================================
## 13_seasonality.R — is the monsoon effect a severity/RSV-season artifact?
##
## Tests whether the higher facility contact during the monsoon reflects a more
## severe case mix (it does not) and characterises the RSV season.
## Writes: output/13_season_summary.csv, output/13_facility_by_severity_season.csv,
##         output/13_by_month.csv, output/fig6_seasonality.png
## =============================================================================
if (!exists("ari") || !"sev_vs" %in% names(ari)) source("03_derive_and_merge.R")
suppressPackageStartupMessages({library(ggplot2)})
if (requireNamespace("lme4", quietly = TRUE)) suppressPackageStartupMessages(library(lme4))

sv <- ari %>% filter(sev_vs) %>%
  left_join(dem %>% select(uid, quintile_rural), by = "uid") %>%
  mutate(vsev    = as.integer(lri_cat == 1),                    # 1 = very severe
         rsv     = as.integer(rsva_episode == 1 | rsvb_episode == 1),
         monsoon = as.integer(ai_month %in% 6:9),
         wealth_q = as.numeric(quintile_rural))

## ---- season summary --------------------------------------------------------
season <- sv %>% group_by(monsoon) %>%
  summarise(n = n(),
            pct_reached = round(100*mean(reached_facility), 1),
            pct_very_severe = round(100*mean(vsev), 1),
            vsev_to_sev = round(sum(vsev)/sum(1-vsev), 2),
            pct_RSV_pos = round(100*mean(rsv), 1), .groups = "drop") %>%
  mutate(season = ifelse(monsoon == 1, "Monsoon (Jun-Sep)", "Rest of year"))
hdr("13 — SEASONALITY: is the monsoon effect a severity/RSV artifact?")
print(as.data.frame(season))
write_csv(season, file.path(OUT_DIR, "13_season_summary.csv"))

## ---- facility contact stratified by severity (the 'normalisation') ---------
## exact reached / total counts by severity x season, plus an "All" row — this
## is the source for the manuscript's Table 2 (nothing here is hand-computed).
strat_sev <- sv %>%
  group_by(severity = ifelse(vsev==1,"very severe","severe"),
           season = ifelse(monsoon==1,"Monsoon (Jun-Sep)","Rest of year")) %>%
  summarise(reached = sum(reached_facility), N = n(),
            pct = round(100*mean(reached_facility),1), .groups="drop")
strat_all <- sv %>% group_by(severity = "All severe/very severe",
           season = ifelse(monsoon==1,"Monsoon (Jun-Sep)","Rest of year")) %>%
  summarise(reached = sum(reached_facility), N = n(),
            pct = round(100*mean(reached_facility),1), .groups="drop")
strat <- bind_rows(strat_sev, strat_all) %>%
  mutate(cell = sprintf("%.1f%% (%d/%s)", pct, reached, format(N, big.mark=","))) %>%
  arrange(match(severity, c("severe","very severe","All severe/very severe")), season)
cat("\nFacility contact by severity x season (severity-normalised; source for Table 2):\n")
print(as.data.frame(strat))
write_csv(strat, file.path(OUT_DIR, "13_facility_by_severity_season.csv"))
cat("\n=> case mix in monsoon is LESS very-severe, yet facility contact is HIGHER\n",
    "   within BOTH strata -> not a severity artifact.\n")

## ---- monsoon effect adjusted for severity ----------------------------------
if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("broom.mixed", quietly = TRUE)) {
  m <- lme4::glmer(reached_facility ~ factor(age_grp) + factor(sexr) + vsev +
                     monsoon + scale(wealth_q) + (1|vcode) + (1|uid),
                   data = sv, family = binomial,
                   control = lme4::glmerControl(optimizer = "bobyqa"))
  ms <- broom.mixed::tidy(m, effects="fixed", exponentiate=TRUE, conf.int=TRUE) %>%
    filter(term %in% c("vsev","monsoon")) %>% select(term, estimate, conf.low, conf.high, p.value)
  cat("\nMonsoon effect ADJUSTED for severity (very severe vs severe):\n")
  print(ms)
  write_csv(ms, file.path(OUT_DIR, "13_monsoon_adj_severity.csv"))
}

## ---- monthly table + figure ------------------------------------------------
mon <- sv %>% group_by(ai_month) %>%
  summarise(n = n(), pct_reached = round(100*mean(reached_facility),1),
            pct_RSV = round(100*mean(rsv),1),
            pct_very_severe = round(100*mean(vsev),1), .groups="drop")
write_csv(mon, file.path(OUT_DIR, "13_by_month.csv"))

MN <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
p <- ggplot(mon, aes(x = ai_month)) +
  annotate("rect", xmin=5.5, xmax=9.5, ymin=-Inf, ymax=Inf, fill="#9ecae1", alpha=0.18) +
  geom_col(aes(y = pct_reached), fill = "#0072B2", width = 0.6) +
  geom_line(aes(y = pct_RSV), colour = "#D55E00", linewidth = 1) +
  geom_point(aes(y = pct_RSV), colour = "#D55E00", size = 2) +
  annotate("text", x = 7.5, y = 46, label = "monsoon", size = 3, colour = "grey30") +
  annotate("text", x = 9, y = 40, label = "RSV positivity", size = 3, colour = "#D55E00", hjust=0) +
  scale_x_continuous(breaks = 1:12, labels = MN) +
  labs(title = "Facility contact and RSV positivity track together by month",
       subtitle = "Bars: % of severe/very-severe episodes reaching a facility. Line: % RSV-positive. Shaded = monsoon.",
       x = NULL, y = "Percent") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 8.5))
ggsave(file.path(OUT_DIR, "fig6_seasonality.png"), p, width = 7.6, height = 4, dpi = 200, bg = "white")
cat("\nWrote fig6_seasonality.png\n")

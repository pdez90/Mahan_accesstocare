## =============================================================================
## 09_tables_figures.R
## Table 1  — characteristics of severe/very-severe episodes, by facility contact
## Figure 1 — facility contact (%) by age group, with 95% CIs
## Figure 2 — village access-barrier index vs % reaching a facility
## Writes: output/09_table1.csv, output/fig1_facility_by_age.png,
##         output/fig2_access_index.png
## =============================================================================
if (!exists("va")) source("08_access_index_and_individual_model.R")
suppressPackageStartupMessages(library(ggplot2))

AGE_LAB <- c("0-6 d","7-27 d","28-89 d","90-179 d","180-364 d","365-730 d")

## analytic set: severe/very-severe episodes with SES + access merged (from 08 `sv`)
sv <- sv %>% mutate(reached_lab = ifelse(reached_facility == 1, "Reached facility",
                                         "Not reached"))

## ---------------------------------------------------------------------------
## TABLE 1
## ---------------------------------------------------------------------------
## helper: counts (col %) of a categorical var within each column + overall + p
cat_rows <- function(var, labels = NULL, name = var) {
  d <- sv %>% filter(!is.na(.data[[var]]))
  tab <- table(d[[var]], d$reached_lab)
  lv  <- rownames(tab)
  labs <- if (is.null(labels)) lv else labels[as.character(lv)]
  pval <- tryCatch(chisq.test(tab)$p.value, error = function(e) NA_real_)
  pfmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  data.frame(
    characteristic = c(name, rep("", length(lv) - 1)),
    level = labs,
    all       = sprintf("%d (%.1f%%)", rowSums(tab), 100*rowSums(tab)/sum(tab)),
    reached   = sprintf("%d (%.1f%%)", tab[,"Reached facility"],
                        100*tab[,"Reached facility"]/sum(tab[,"Reached facility"])),
    notreach  = sprintf("%d (%.1f%%)", tab[,"Not reached"],
                        100*tab[,"Not reached"]/sum(tab[,"Not reached"])),
    p = c(pfmt(pval), rep("", length(lv) - 1)),
    stringsAsFactors = FALSE)
}
## helper: continuous var, median [IQR], Wilcoxon p
cont_row <- function(var, name) {
  d <- sv %>% filter(!is.na(.data[[var]]))
  f <- function(x) sprintf("%.2f [%.2f to %.2f]", median(x), quantile(x,.25), quantile(x,.75))
  pval <- tryCatch(wilcox.test(d[[var]] ~ d$reached_lab)$p.value, error=function(e) NA_real_)
  data.frame(characteristic = name, level = "median [IQR]",
             all = f(d[[var]]),
             reached  = f(d[[var]][d$reached_lab=="Reached facility"]),
             notreach = f(d[[var]][d$reached_lab=="Not reached"]),
             p = ifelse(is.na(pval), "", sprintf("%.3f", pval)),
             stringsAsFactors = FALSE)
}

hdr("09 — TABLE 1: characteristics of severe/very-severe episodes by facility contact")
header_row <- data.frame(
  characteristic = "N episodes", level = "",
  all      = sprintf("%d", nrow(sv)),
  reached  = sprintf("%d", sum(sv$reached_facility==1)),
  notreach = sprintf("%d", sum(sv$reached_facility==0)),
  p = "", stringsAsFactors = FALSE)

table1 <- rbind(
  header_row,
  cat_rows("lri_cat", labels=c("1"="Very severe","2"="Severe"), name="Severity"),
  cat_rows("age_grp", labels=setNames(AGE_LAB, 1:6), name="Age group"),
  cat_rows("sexr",    labels=c("1"="Male","2"="Female"), name="Sex"),
  cat_rows("wealth_q",labels=setNames(paste("Quintile",1:5), 1:5),
           name="Household wealth (DHS quintile)"),
  cat_rows("mother_edu", name="Maternal education"),
  cont_row("access_barrier_z", "Village access-barrier index (SD units)")
)
print(table1, row.names = FALSE)
write_csv(table1, file.path(OUT_DIR, "09_table1.csv"))

## ---------------------------------------------------------------------------
## FIGURE THEME + PALETTE (colourblind-safe Okabe–Ito)
## ---------------------------------------------------------------------------
OKABE <- c(blue="#0072B2", orange="#E69F00", green="#009E73", grey="#7F7F7F")
theme_ms <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 13),
        plot.margin = margin(10,14,10,10))

## ---- Figure 1: facility contact by age, Wilson 95% CI ----------------------
wilson <- function(x, n) {
  p <- x/n; z <- 1.959964; d <- 1 + z^2/n
  ctr <- (p + z^2/(2*n))/d; hw <- z*sqrt(p*(1-p)/n + z^2/(4*n^2))/d
  c(lo = ctr-hw, hi = ctr+hw)
}
f1 <- sv %>% group_by(age_grp) %>%
  summarise(n=n(), r=sum(reached_facility), .groups="drop") %>%
  mutate(pct=100*r/n,
         lo=100*mapply(function(x,n) wilson(x,n)["lo"], r, n),
         hi=100*mapply(function(x,n) wilson(x,n)["hi"], r, n),
         age=factor(AGE_LAB[age_grp], levels=AGE_LAB))

p1 <- ggplot(f1, aes(age, pct)) +
  geom_col(fill = OKABE["blue"], width = 0.68) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.2, linewidth=0.5, colour="grey25") +
  geom_text(aes(y=hi, label=sprintf("%.1f%%", pct)), vjust=-0.7, hjust=0.5, size=3.4) +
  scale_y_continuous(limits=c(0,65), expand=expansion(mult=c(0,.02))) +
  labs(x="Age at episode", y="Reached a health facility (%)",
       title="Facility contact for severe/very-severe LRTI, by age") +
  theme_ms
ggsave(file.path(OUT_DIR,"fig1_facility_by_age.png"), p1, width=6.6, height=4.1, dpi=300)

## ---- Figure 2: village access-barrier index vs % reaching facility ---------
vr <- ari %>% filter(sev_vs) %>% group_by(vcode) %>%
  summarise(n_sv=n(), pct_reached=100*mean(reached_facility), .groups="drop")
f2 <- vr %>% left_join(va %>% select(vcode, access_barrier_z), by="vcode") %>%
  filter(!is.na(access_barrier_z))
rho <- cor(f2$access_barrier_z, f2$pct_reached, method="spearman")
pv  <- suppressWarnings(cor.test(f2$access_barrier_z, f2$pct_reached, method="spearman")$p.value)

p2 <- ggplot(f2, aes(access_barrier_z, pct_reached)) +
  geom_smooth(method="lm", se=TRUE, colour=OKABE["orange"],
              fill=OKABE["orange"], alpha=0.15, linewidth=0.8) +
  geom_point(aes(size=n_sv), colour=OKABE["blue"], alpha=0.6) +
  scale_size_area(max_size=6, name="Severe\nepisodes") +
  annotate("text", x=min(f2$access_barrier_z), y=max(f2$pct_reached),
           hjust=0, vjust=1, size=3.6,
           label=sprintf("Spearman rho = %.2f\np = %.2f (n = %d villages)", rho, pv, nrow(f2))) +
  labs(x="Village access-barrier index (SD units; higher = worse access)",
       y="Severe episodes reaching a facility (%)",
       title="Geographic access and facility contact, by village") +
  theme_ms + theme(panel.grid.major.x = element_line(colour="grey92"))
ggsave(file.path(OUT_DIR,"fig2_access_index.png"), p2, width=6.6, height=4.3, dpi=300)

cat("\nWrote Table 1 and Figures 1–2 to", OUT_DIR, "\n")

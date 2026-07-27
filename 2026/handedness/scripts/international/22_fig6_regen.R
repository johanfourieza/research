# =============================================================================
# 22_fig6_regen.R — regenerate Figure 6 (T20I QTE) with correct reference lines
#
# The committed figure drew its "OLS" reference band from
# table_combined_formats.csv, whose T20I row is the match x innings
# FIXED-EFFECTS estimate (0.186, SE 0.256) — a like-for-unlike benchmark for
# quantile regressions estimated WITHOUT those fixed effects (QC comment 25).
#
# This script draws two correctly labelled references:
#   - the covariate-adjusted NO-FE mean (same control set as the quantile
#     regressions; from scripts/output/feedback_fixes/G_t20i_nofe_mean.csv),
#     with its 95% band — the comparable benchmark;
#   - the match x innings FE estimate (Table 3) as a labelled dashed line.
#
# Headless: Rscript scripts/22_fig6_regen.R
# Overwrites JSE/figures/fig6_qte_t20is.{pdf,png}
# =============================================================================

suppressMessages({ library(tidyverse) })

base_dir    <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
tables_dir  <- file.path(base_dir, "scripts", "output", "tables")
fixes_dir   <- file.path(base_dir, "scripts", "output", "feedback_fixes")
figures_dir <- file.path(base_dir, "JSE", "figures")

leap_colours <- c("plum" = "#5C2346", "blue" = "#3D8EB9", "sage" = "#6B8E5E",
                  "gold" = "#D4A03E", "rose" = "#A34466", "teal" = "#45808B")

theme_leap <- function(base_size = 10, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.title = element_text(size = 10, colour = "#4A4A4A"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8), angle = 90),
      axis.text = element_text(size = 9, colour = "#5A5A5A"),
      axis.line.x = element_line(colour = "#4A4A4A", linewidth = 0.4),
      axis.line.y = element_line(colour = "#4A4A4A", linewidth = 0.4),
      axis.ticks = element_line(colour = "#4A4A4A", linewidth = 0.3),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA)
    )
}

qte <- read_csv(file.path(tables_dir, "table_qte_t20is.csv"), show_col_types = FALSE)
nofe <- read_csv(file.path(fixes_dir, "G_t20i_nofe_mean.csv"), show_col_types = FALSE)

nofe_est <- nofe$est[1]; nofe_se <- nofe$se[1]
fe_est <- 0.186   # Table 3, match x innings FE (preferred)

fig6 <- ggplot(qte, aes(x = tau, y = estimate)) +
  annotate("rect", xmin = 0.05, xmax = 1,
           ymin = nofe_est - 1.96 * nofe_se, ymax = nofe_est + 1.96 * nofe_se,
           fill = unname(leap_colours["sage"]), alpha = 0.15) +
  geom_hline(yintercept = nofe_est, linetype = "dotted",
             colour = unname(leap_colours["sage"]), linewidth = 0.5) +
  geom_hline(yintercept = fe_est, linetype = "dashed",
             colour = unname(leap_colours["blue"]), linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "#AAAAAA",
             linewidth = 0.3) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = unname(leap_colours["plum"]), alpha = 0.15) +
  geom_line(colour = unname(leap_colours["plum"]), linewidth = 0.7) +
  geom_point(data = qte %>% filter(p_value < 0.05),
             colour = unname(leap_colours["plum"]), size = 3.5, shape = 16) +
  geom_point(data = qte %>% filter(p_value >= 0.05),
             colour = "#AAAAAA", size = 2.5, shape = 1) +
  annotate("text", x = 0.06, y = nofe_est + 1.96 * nofe_se + 0.12,
           label = sprintf("Mean, same controls, no FE (%.2f)", nofe_est),
           size = 3, colour = unname(leap_colours["sage"]), hjust = 0) +
  annotate("text", x = 0.06, y = fe_est - 0.14,
           label = sprintf("Mean, match %s innings FE (%.2f)", "×", fe_est),
           size = 3, colour = unname(leap_colours["blue"]), hjust = 0) +
  scale_x_continuous(breaks = qte$tau, labels = sprintf("%.2f", qte$tau)) +
  labs(x = expression(paste("Quantile (", tau, ")")),
       y = "Quantile regression coefficient") +
  theme_leap()

ggsave(file.path(figures_dir, "fig6_qte_t20is.png"), fig6,
       width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig6_qte_t20is.pdf"), fig6,
       width = 7, height = 5, device = cairo_pdf)
cat(sprintf("Figure 6 regenerated: no-FE mean %.3f (SE %.3f), FE line %.3f\n",
            nofe_est, nofe_se, fe_est))

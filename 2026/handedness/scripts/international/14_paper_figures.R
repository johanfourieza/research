# =============================================================================
# 14_paper_figures.R
# Generate all 6 paper figures using LEAP visualisation style.
# Run from RStudio (uses rstudioapi for paths).
# =============================================================================

library(tidyverse)
library(survival)

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
figures_dir  <- file.path(base_dir, "scripts", "output", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

formats       <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# =============================================================================
# LEAP COLOUR PALETTE AND THEME
# =============================================================================

leap_colours <- c(
  "plum"  = "#5C2346",
  "blue"  = "#3D8EB9",
  "sage"  = "#6B8E5E",
  "gold"  = "#D4A03E",
  "rose"  = "#A34466",
  "teal"  = "#45808B",
  "earth" = "#8B6B3D",
  "mint"  = "#97C5B0"
)
leap_cycle <- unname(leap_colours)

theme_leap <- function(base_size = 10, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title       = element_text(size = 11, face = "bold", colour = "#2D2D2D",
                                      margin = margin(b = 8), hjust = 0),
      plot.subtitle    = element_text(size = 10, colour = "#4A4A4A",
                                      margin = margin(b = 10), hjust = 0),
      plot.caption     = element_text(size = 8, colour = "#5A5A5A", hjust = 1),
      axis.title       = element_text(size = 10, colour = "#4A4A4A"),
      axis.title.x     = element_text(margin = margin(t = 8)),
      axis.title.y     = element_text(margin = margin(r = 8)),
      axis.text        = element_text(size = 9, colour = "#5A5A5A"),
      axis.line.x      = element_line(colour = "#4A4A4A", linewidth = 0.4),
      axis.line.y      = element_line(colour = "#4A4A4A", linewidth = 0.4),
      axis.ticks       = element_line(colour = "#4A4A4A", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.title       = element_text(size = 9, face = "bold", colour = "#4A4A4A"),
      legend.text        = element_text(size = 9, colour = "#4A4A4A"),
      legend.position    = "bottom",
      plot.margin = margin(10, 15, 10, 10)
    )
}

scale_colour_leap <- function(...) {
  scale_colour_manual(values = leap_cycle, ...)
}

scale_fill_leap <- function(...) {
  scale_fill_manual(values = leap_cycle, ...)
}

# Hand-combination colours: Plum (LL), Blue (LR), Sage (RR)
hand_colours <- c("LL" = unname(leap_colours["plum"]),
                  "LR" = unname(leap_colours["blue"]),
                  "RR" = unname(leap_colours["sage"]))

# =============================================================================
# LOAD DATA
# =============================================================================

data_list <- list()
for (fmt in formats) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (file.exists(path)) {
    data_list[[fmt]] <- read_csv(path, show_col_types = FALSE) %>%
      filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
      mutate(format = fmt)
  }
}
main_sample <- bind_rows(data_list)

cat(sprintf("Main sample: %d partnerships\n", nrow(main_sample)))

# =============================================================================
# FIGURE 1: Crease diagram (schematic)
# =============================================================================
# Minimal geometric schematic showing the LR switching cost mechanism.
# Created programmatically with ggplot2.

fig1 <- ggplot() +
  # Pitch rectangle
  annotate("rect", xmin = -1.1, xmax = 1.1, ymin = -0.5, ymax = 22.5,
           fill = "#F5F3F0", colour = "#4A4A4A", linewidth = 0.4) +
  # Creases
  annotate("segment", x = -0.8, xend = 0.8, y = 0, yend = 0,
           colour = "#4A4A4A", linewidth = 0.6) +
  annotate("segment", x = -0.8, xend = 0.8, y = 22, yend = 22,
           colour = "#4A4A4A", linewidth = 0.6) +
  # Stumps
  annotate("segment", x = -0.1, xend = 0.1, y = 0, yend = 0,
           colour = "#2D2D2D", linewidth = 2) +
  annotate("segment", x = -0.1, xend = 0.1, y = 22, yend = 22,
           colour = "#2D2D2D", linewidth = 2) +
  # Right-handed striker (batting end) - stance on off-side
  annotate("point", x = 0.35, y = 1.5, size = 5,
           colour = unname(leap_colours["sage"]), shape = 16) +
  annotate("text", x = 0.35, y = 0.3, label = "RH Striker",
           size = 3, colour = unname(leap_colours["sage"]), fontface = "bold") +
  # Left-handed striker (alternative) - stance on leg-side
  annotate("point", x = -0.35, y = 3.5, size = 5,
           colour = unname(leap_colours["plum"]), shape = 17) +
  annotate("text", x = -0.35, y = 4.7, label = "LH Striker",
           size = 3, colour = unname(leap_colours["plum"]), fontface = "bold") +
  # Non-striker end
  annotate("point", x = 0.4, y = 20.5, size = 4,
           colour = "#AAAAAA", shape = 16) +
  annotate("text", x = 0.4, y = 21.7, label = "Non-striker",
           size = 2.8, colour = "#5A5A5A") +
  # Bowler
  annotate("point", x = 0, y = 24.5, size = 4,
           colour = unname(leap_colours["gold"]), shape = 18) +
  annotate("text", x = 0, y = 25.5, label = "Bowler",
           size = 3, colour = unname(leap_colours["gold"]), fontface = "bold") +
  # Bowling line to RH (off-stump)
  annotate("segment", x = 0, xend = 0.15, y = 24, yend = 1.5,
           colour = unname(leap_colours["sage"]),
           linewidth = 0.6, linetype = "dashed",
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  # Bowling line to LH (off-stump mirrors)
  annotate("segment", x = 0, xend = -0.15, y = 24, yend = 3.5,
           colour = unname(leap_colours["plum"]),
           linewidth = 0.6, linetype = "dashed",
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  # Labels for bowling lines
  annotate("text", x = 0.55, y = 12, label = "Line to RH\n(off stump)",
           size = 2.5, colour = unname(leap_colours["sage"]), hjust = 0) +
  annotate("text", x = -0.55, y = 14, label = "Line to LH\n(off stump)",
           size = 2.5, colour = unname(leap_colours["plum"]), hjust = 1) +
  # Annotation: switching cost
  annotate("segment", x = 0.3, xend = -0.3, y = 8, yend = 8,
           colour = unname(leap_colours["blue"]),
           linewidth = 0.5, linetype = "solid",
           arrow = arrow(length = unit(2, "mm"), type = "closed", ends = "both")) +
  annotate("text", x = 0, y = 9.2, label = "Bowler must\nadjust line",
           size = 2.8, colour = unname(leap_colours["blue"]), fontface = "italic") +
  coord_fixed(ratio = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 27)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave(file.path(figures_dir, "fig1_crease_diagram.png"),
       fig1, width = 4, height = 6, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig1_crease_diagram.pdf"),
       fig1, width = 4, height = 6, device = cairo_pdf)
cat("Figure 1 (crease diagram) saved.\n")

# =============================================================================
# FIGURE 2: Mean runs by hand combination (grouped bar chart)
# =============================================================================

fig2_data <- main_sample %>%
  group_by(format, hand_combination) %>%
  summarise(
    mean_runs = mean(runs_scored, na.rm = TRUE),
    se_runs   = sd(runs_scored, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(format = factor(format_labels[format],
                         levels = c("Tests", "ODIs", "T20Is")))

fig2 <- ggplot(fig2_data, aes(x = hand_combination, y = mean_runs,
                               fill = hand_combination)) +
  geom_col(width = 0.6, colour = NA) +
  geom_errorbar(aes(ymin = mean_runs - 1.96 * se_runs,
                     ymax = mean_runs + 1.96 * se_runs),
                width = 0.15, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", mean_runs)),
            vjust = -1.8, size = 3, colour = "#4A4A4A") +
  facet_wrap(~ format, scales = "free_y") +
  scale_fill_manual(values = hand_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Hand combination",
       y = "Mean partnership runs") +
  theme_leap() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"))

ggsave(file.path(figures_dir, "fig2_mean_runs_by_hand.png"),
       fig2, width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig2_mean_runs_by_hand.pdf"),
       fig2, width = 8, height = 4.5, device = cairo_pdf)
cat("Figure 2 (mean runs bar chart) saved.\n")

# =============================================================================
# FIGURE 3: Density of partnership runs by hand combination (3-panel)
# =============================================================================

fig3_data <- main_sample %>%
  mutate(format = factor(format_labels[format],
                         levels = c("Tests", "ODIs", "T20Is")))

fig3 <- ggplot(fig3_data, aes(x = runs_scored, fill = hand_combination,
                               colour = hand_combination)) +
  geom_density(alpha = 0.20, adjust = 1.5, linewidth = 0.6) +
  facet_wrap(~ format, scales = "free", ncol = 3) +
  scale_x_continuous(limits = c(0, NA)) +
  coord_cartesian(xlim = c(0, 150)) +
  scale_fill_manual(values = hand_colours, name = "Hand combination") +
  scale_colour_manual(values = hand_colours, name = "Hand combination") +
  labs(x = "Partnership runs",
       y = "Density") +
  theme_leap() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(file.path(figures_dir, "fig3_density_runs.png"),
       fig3, width = 10, height = 4, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig3_density_runs.pdf"),
       fig3, width = 10, height = 4, device = cairo_pdf)
cat("Figure 3 (density plots) saved.\n")

# =============================================================================
# FIGURE 4: Coefficient plot — is_mixed_hand across formats
# =============================================================================

# Read combined regression results
combined_csv <- file.path(tables_dir, "table_combined_formats.csv")
combined_raw <- read_csv(combined_csv, show_col_types = FALSE)

# Extract is_mixed_hand coefficients
coef_data <- tibble(
  format = factor(c("Tests", "ODIs", "T20Is"), levels = c("Tests", "ODIs", "T20Is"))
)

# Parse estimates and SEs from modelsummary CSV
est_row <- combined_raw %>%
  filter(term == "is_mixed_hand", statistic == "estimate")
se_row  <- combined_raw %>%
  filter(term == "is_mixed_hand", statistic == "std.error")

coef_data$estimate <- as.numeric(c(est_row$Tests, est_row$ODIs, est_row$T20Is))
coef_data$se <- as.numeric(gsub("[()]", "", c(se_row$Tests, se_row$ODIs, se_row$T20Is)))
coef_data <- coef_data %>%
  mutate(ci_low  = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se,
         sig = ifelse(ci_low <= 0 & ci_high >= 0, "ns", "sig"))

fig4 <- ggplot(coef_data, aes(x = format, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#AAAAAA",
             linewidth = 0.4) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  colour = unname(leap_colours["plum"]),
                  size = 0.6, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f\n(%.2f)", estimate, se)),
            vjust = -1.5, size = 3, colour = "#4A4A4A") +
  labs(x = "Format",
       y = "Coefficient on Mixed Hand (LR)") +
  theme_leap() +
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.3))

ggsave(file.path(figures_dir, "fig4_coef_plot.png"),
       fig4, width = 6, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig4_coef_plot.pdf"),
       fig4, width = 6, height = 4.5, device = cairo_pdf)
cat("Figure 4 (coefficient plot) saved.\n")

# =============================================================================
# FIGURE 5: Kaplan-Meier survival curves (3-panel)
# =============================================================================

km_list <- list()

for (fmt in formats) {
  df <- data_list[[fmt]]
  if (is.null(df)) next

  df_surv <- df %>%
    filter(!is.na(is_mixed_hand), balls_faced > 0) %>%
    mutate(
      event = 1L - as.integer(is_censored),
      hand_label = factor(
        ifelse(is_mixed_hand == 1, "Mixed (LR)", "Same hand"),
        levels = c("Same hand", "Mixed (LR)")
      )
    )

  km_fit <- survfit(Surv(balls_faced, event) ~ hand_label, data = df_surv)
  km_df  <- broom::tidy(km_fit) %>%
    mutate(
      hand_label = str_remove(strata, "hand_label="),
      format = format_labels[fmt]
    )

  km_list[[fmt]] <- km_df
}

km_all <- bind_rows(km_list) %>%
  mutate(format = factor(format, levels = c("Tests", "ODIs", "T20Is")))

# Set x-axis limits by format
xlims <- c(Tests = 200, ODIs = 150, T20Is = 80)

fig5 <- ggplot(km_all, aes(x = time, y = estimate,
                            colour = hand_label, fill = hand_label)) +
  geom_step(linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, linetype = 0) +
  facet_wrap(~ format, scales = "free_x", ncol = 3) +
  scale_colour_manual(
    values = c("Same hand"   = unname(leap_colours["plum"]),
               "Mixed (LR)" = unname(leap_colours["blue"])),
    name = "Partnership type"
  ) +
  scale_fill_manual(
    values = c("Same hand"   = unname(leap_colours["plum"]),
               "Mixed (LR)" = unname(leap_colours["blue"])),
    name = "Partnership type"
  ) +
  labs(x = "Balls faced",
       y = "Survival probability") +
  theme_leap() +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(file.path(figures_dir, "fig5_km_survival.png"),
       fig5, width = 10, height = 4, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig5_km_survival.pdf"),
       fig5, width = 10, height = 4, device = cairo_pdf)
cat("Figure 5 (KM survival curves) saved.\n")

# =============================================================================
# FIGURE 6: T20I Quantile Treatment Effects
# =============================================================================

qte_t20i <- read_csv(file.path(tables_dir, "table_qte_t20is.csv"),
                      show_col_types = FALSE)

# CORRECTED 26 Jul 2026 (QC audit): the previous version pulled the T20I row of
# table_combined_formats.csv, which is the match x innings FIXED-EFFECTS
# estimate (0.186), and labelled it "OLS" — a like-for-unlike benchmark for
# quantile regressions estimated without those fixed effects. The comparable
# no-FE covariate-adjusted mean is computed by scripts/21_feedback_fixes.R
# (G_t20i_nofe_mean.csv); scripts/22_fig6_regen.R draws the corrected figure
# with both references labelled. This block is retained for provenance but the
# committed figure now comes from script 22.
ols_est <- coef_data$estimate[coef_data$format == "T20Is"]
ols_se  <- coef_data$se[coef_data$format == "T20Is"]

fig6 <- ggplot(qte_t20i, aes(x = tau, y = estimate)) +
  # OLS reference band
  annotate("rect", xmin = 0.05, xmax = 1,
           ymin = ols_est - 1.96 * ols_se,
           ymax = ols_est + 1.96 * ols_se,
           fill = unname(leap_colours["sage"]), alpha = 0.15) +
  geom_hline(yintercept = ols_est, linetype = "dotted",
             colour = unname(leap_colours["sage"]), linewidth = 0.5) +
  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#AAAAAA",
             linewidth = 0.4) +
  # QTE estimates
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = unname(leap_colours["plum"]), alpha = 0.15) +
  geom_line(colour = unname(leap_colours["plum"]), linewidth = 0.7) +
  geom_point(colour = unname(leap_colours["plum"]), size = 2.5) +
  # Significance markers
  geom_point(data = qte_t20i %>% filter(p_value < 0.05),
             colour = unname(leap_colours["plum"]), size = 3.5, shape = 16) +
  geom_point(data = qte_t20i %>% filter(p_value >= 0.05),
             colour = "#AAAAAA", size = 2.5, shape = 1) +
  # Annotations
  annotate("text", x = 0.95, y = ols_est + 0.3,
           label = "OLS", size = 3, colour = unname(leap_colours["sage"]),
           hjust = 1) +
  scale_x_continuous(breaks = qte_t20i$tau,
                     labels = sprintf("%.2f", qte_t20i$tau)) +
  labs(x = expression(paste("Quantile (", tau, ")")),
       y = "Quantile regression coefficient") +
  theme_leap()

ggsave(file.path(figures_dir, "fig6_qte_t20is.png"),
       fig6, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "fig6_qte_t20is.pdf"),
       fig6, width = 7, height = 5, device = cairo_pdf)
cat("Figure 6 (T20I QTE process) saved.\n")

cat("\n=== All paper figures generated ===\n")

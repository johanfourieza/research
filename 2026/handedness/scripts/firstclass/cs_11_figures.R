# =============================================================================
# cs_11_figures.R  —  CS figures in the LEAP visual identity.
# Rebuilds figures from saved tables / analysis CSVs (no ball-level reprocessing).
# Titles/subtitles are NOT drawn on the images; they live in the paper captions.
# Run with:  Rscript cs_11_figures.R
# =============================================================================
library(tidyverse)

# ----------------------------------------------------------------------------
# LEAP VISUAL IDENTITY
# ----------------------------------------------------------------------------
LEAP_NONSIG_COLOR <- "#AAAAAA"

theme_leap <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text = element_text(family = "sans"),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 11, color = "#4A4A4A"),
      axis.text = element_text(size = 10, color = "#5A5A5A"),
      legend.text = element_text(size = 10),
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      strip.text = element_text(size = 11, face = "bold", color = "#2D2D2D")
    )
}
save_leap_fig <- function(fig_path, plot, width, height, dpi = 600) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

# ----------------------------------------------------------------------------
base_dir   <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), "..", ".."))
ana_dir    <- file.path(base_dir, "data", "analysis")
out_tables <- file.path(base_dir, "CS", "output", "tables")
out_figs   <- file.path(base_dir, "CS", "output", "figures")

sample_levels <- c("Tests (international)", "County Championship", "Sheffield Shield")
# RR = plum (primary), LR = gold (focal/highlight), LL = blue. Distinct under deuteranopia.
hand_cols <- c(RR = "#5C2346", LR = "#D4A03E", LL = "#3D8EB9")

# --- Figure 1: raw means by hand combination (recompute SE from analysis CSVs) ---
fig1_df <- map2_dfr(
  c("tests", "county", "sheffield"),
  sample_levels,
  function(f, lab) {
    df <- read_csv(file.path(ana_dir, paste0("analysis_", f, ".csv")), show_col_types = FALSE) %>%
      filter(hand_known == 1, both_avg_known == 1, either_debutant == 0)
    df %>% group_by(hand_combination) %>%
      summarise(mean_runs = mean(runs_scored), se = sd(runs_scored)/sqrt(n()), .groups = "drop") %>%
      mutate(sample = lab)
  }) %>%
  mutate(hand_combination = factor(hand_combination, levels = c("RR","LR","LL")),
         sample = factor(sample, levels = sample_levels))

p1 <- ggplot(fig1_df, aes(hand_combination, mean_runs, fill = hand_combination)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = mean_runs - 1.96*se, ymax = mean_runs + 1.96*se),
                width = 0.18, colour = "#4A4A4A", linewidth = 0.6) +
  facet_wrap(~sample) +
  scale_fill_manual(values = c(RR = "#5C2346", LR = "#D4A03E", LL = "#3D8EB9"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Partnership hand combination", y = "Mean partnership runs") +
  theme_leap()
save_leap_fig(file.path(out_figs, "fig1_mean_runs.png"), p1, width = 10, height = 5)

# --- Figure 2: P1 mixed-hand coefficient with 95% CI ---
theme_leap_h <- function() theme_leap() +
  theme(panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.5),
        panel.grid.major.y = element_blank())

p1tab <- read_csv(file.path(out_tables, "table2_p1_regressions.csv"), show_col_types = FALSE) %>%
  mutate(sample = factor(sample, levels = sample_levels),
         lo = mixed_b - 1.96*mixed_se, hi = mixed_b + 1.96*mixed_se)
p2 <- ggplot(p1tab, aes(mixed_b, fct_rev(sample))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = LEAP_NONSIG_COLOR) +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#5C2346", linewidth = 1, size = 0.8) +
  labs(x = "Estimated effect of a left-right pair on partnership runs", y = NULL) +
  theme_leap_h()
save_leap_fig(file.path(out_figs, "fig2_p1_coef.png"), p2, width = 10, height = 4)

# --- Figure 3: P2 LR x strike-changed interaction with 95% CI ---
p2tab <- read_csv(file.path(out_tables, "table4_p2_mechanism.csv"), show_col_types = FALSE) %>%
  mutate(sample = factor(sample, levels = sample_levels),
         lo = interact_b - 1.96*interact_se, hi = interact_b + 1.96*interact_se)
p3 <- ggplot(p2tab, aes(interact_b, fct_rev(sample))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = LEAP_NONSIG_COLOR) +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#5C2346", linewidth = 1, size = 0.8) +
  labs(x = "Extra runs per delivery for a left-right pair after a change of strike", y = NULL) +
  theme_leap_h()
save_leap_fig(file.path(out_figs, "fig3_p2_mechanism.png"), p3, width = 10, height = 4)

cat("Figures rebuilt in", out_figs, "\n")

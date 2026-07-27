# =============================================================================
# 06_descriptive_stats.R
# Summary statistics, distribution plots, and correlation matrices
# =============================================================================

library(tidyverse)
library(kableExtra)
library(scales)

# --- Paths ---
base_dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir <- file.path(base_dir, "scripts", "output", "tables")
figures_dir <- file.path(base_dir, "scripts", "output", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# --- Load data ---
data_list <- list()
for (fmt in formats) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (file.exists(path)) {
    df <- read_csv(path, show_col_types = FALSE) %>%
      mutate(format = fmt)
    data_list[[fmt]] <- df
    cat(sprintf("Loaded %s: %d partnerships\n", fmt, nrow(df)))
  } else {
    cat(sprintf("File not found: %s\n", path))
  }
}

all_data <- bind_rows(data_list)

# --- Filter to usable observations ---
# Main sample: known hand combination, both averages known, neither debutant
main_sample <- all_data %>%
  filter(
    hand_known == 1,
    both_avg_known == 1,
    either_debutant == 0
  )

cat(sprintf("\nMain sample: %d partnerships (of %d total)\n",
            nrow(main_sample), nrow(all_data)))

# =============================================================================
# TABLE 1: Sample sizes by format and hand combination
# =============================================================================

table1 <- main_sample %>%
  group_by(format, hand_combination) %>%
  summarise(
    n = n(),
    mean_runs = mean(runs_scored, na.rm = TRUE),
    median_runs = median(runs_scored, na.rm = TRUE),
    sd_runs = sd(runs_scored, na.rm = TRUE),
    mean_balls = mean(balls_faced, na.rm = TRUE),
    mean_quality = mean(avg_partnership_quality, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    format = format_labels[format],
    across(where(is.numeric) & !c(n), ~ round(., 1))
  )

write_csv(table1, file.path(tables_dir, "table1_sample_by_hand.csv"))
cat("\nTable 1: Sample by hand combination\n")
print(table1)

# =============================================================================
# TABLE 2: Summary statistics of all control variables
# =============================================================================

summary_stats_fn <- function(df, fmt_label) {
  vars_to_summarise <- c(
    "runs_scored", "balls_faced", "run_rate",
    "pre_match_avg_1", "pre_match_avg_2", "avg_partnership_quality",
    "pre_match_sr_1", "pre_match_sr_2",
    "pre_match_matches_1", "pre_match_matches_2", "combined_experience",
    "partnership_number", "wickets_at_start", "runs_at_start",
    "batting_position_1", "batting_position_2"
  )

  existing_vars <- vars_to_summarise[vars_to_summarise %in% names(df)]

  df %>%
    select(all_of(existing_vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(format = fmt_label)
}

table2 <- map_dfr(formats, function(fmt) {
  df <- main_sample %>% filter(format == fmt)
  if (nrow(df) > 0) summary_stats_fn(df, format_labels[fmt])
})

write_csv(table2, file.path(tables_dir, "table2_summary_stats.csv"))
cat("\nTable 2 saved.\n")

# =============================================================================
# FIGURE 1: Distribution of partnership runs by hand combination
# =============================================================================

for (fmt in formats) {
  df <- main_sample %>% filter(format == fmt)
  if (nrow(df) == 0) next

  p <- ggplot(df, aes(x = runs_scored, fill = hand_combination)) +
    geom_density(alpha = 0.4, adjust = 1.5) +
    scale_x_continuous(limits = c(0, quantile(df$runs_scored, 0.99))) +
    labs(
      title = paste("Distribution of Partnership Runs:", format_labels[fmt]),
      x = "Runs Scored",
      y = "Density",
      fill = "Hand\nCombination"
    ) +
    scale_fill_manual(values = c(LL = "#E69F00", LR = "#56B4E9", RR = "#009E73")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(figures_dir, paste0("fig1_density_runs_", fmt, ".pdf")),
    p, width = 8, height = 5
  )
  ggsave(
    file.path(figures_dir, paste0("fig1_density_runs_", fmt, ".png")),
    p, width = 8, height = 5, dpi = 300
  )
}

cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2: Mean partnership runs by hand combination and format
# =============================================================================

fig2_data <- main_sample %>%
  group_by(format, hand_combination) %>%
  summarise(
    mean_runs = mean(runs_scored, na.rm = TRUE),
    se_runs = sd(runs_scored, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(format = format_labels[format])

p2 <- ggplot(fig2_data, aes(x = hand_combination, y = mean_runs, fill = hand_combination)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_runs - 1.96 * se_runs,
                     ymax = mean_runs + 1.96 * se_runs),
                width = 0.2) +
  facet_wrap(~ format, scales = "free_y") +
  labs(
    title = "Mean Partnership Runs by Hand Combination",
    x = "Hand Combination",
    y = "Mean Runs",
    fill = "Hand\nCombination"
  ) +
  scale_fill_manual(values = c(LL = "#E69F00", LR = "#56B4E9", RR = "#009E73")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave(file.path(figures_dir, "fig2_mean_runs_by_hand.pdf"), p2, width = 10, height = 5)
ggsave(file.path(figures_dir, "fig2_mean_runs_by_hand.png"), p2, width = 10, height = 5, dpi = 300)
cat("Figure 2 saved.\n")

# =============================================================================
# FIGURE 3: Partnership runs over time by hand combination
# =============================================================================

for (fmt in formats) {
  df <- main_sample %>%
    filter(format == fmt) %>%
    mutate(year = as.integer(year))

  if (nrow(df) == 0) next

  yearly <- df %>%
    group_by(year, hand_combination) %>%
    summarise(
      mean_runs = mean(runs_scored, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= 10)  # require minimum observations per year

  p <- ggplot(yearly, aes(x = year, y = mean_runs, color = hand_combination)) +
    geom_line(linewidth = 0.8) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, span = 0.3) +
    labs(
      title = paste("Mean Partnership Runs Over Time:", format_labels[fmt]),
      x = "Year",
      y = "Mean Runs per Partnership",
      color = "Hand\nCombination"
    ) +
    scale_color_manual(values = c(LL = "#E69F00", LR = "#56B4E9", RR = "#009E73")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(figures_dir, paste0("fig3_time_trend_", fmt, ".pdf")),
    p, width = 8, height = 5
  )
  ggsave(
    file.path(figures_dir, paste0("fig3_time_trend_", fmt, ".png")),
    p, width = 8, height = 5, dpi = 300
  )
}

cat("Figure 3 saved.\n")

# =============================================================================
# TABLE 3: Correlation matrix of key variables
# =============================================================================

for (fmt in formats) {
  df <- main_sample %>% filter(format == fmt)
  if (nrow(df) == 0) next

  cor_vars <- c(
    "runs_scored", "balls_faced", "is_mixed_hand",
    "avg_partnership_quality", "combined_experience",
    "partnership_number", "wickets_at_start", "runs_at_start"
  )
  existing <- cor_vars[cor_vars %in% names(df)]

  cor_mat <- df %>%
    select(all_of(existing)) %>%
    cor(use = "pairwise.complete.obs") %>%
    round(3)

  write.csv(cor_mat, file.path(tables_dir, paste0("table3_correlations_", fmt, ".csv")))
}

cat("Table 3 (correlations) saved.\n")

# =============================================================================
# FIGURE 4: Partnership runs by batting position group
# =============================================================================

fig4_data <- main_sample %>%
  filter(!is.na(position_group)) %>%
  group_by(format, position_group, hand_combination) %>%
  summarise(
    mean_runs = mean(runs_scored, na.rm = TRUE),
    se_runs = sd(runs_scored, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(format = format_labels[format])

p4 <- ggplot(fig4_data, aes(x = position_group, y = mean_runs, fill = hand_combination)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_runs - 1.96 * se_runs, ymax = mean_runs + 1.96 * se_runs),
    position = position_dodge(width = 0.7), width = 0.2
  ) +
  facet_wrap(~ format, scales = "free_y") +
  labs(
    title = "Mean Partnership Runs by Batting Position Group",
    x = "Batting Position Group",
    y = "Mean Runs",
    fill = "Hand\nCombination"
  ) +
  scale_fill_manual(values = c(LL = "#E69F00", LR = "#56B4E9", RR = "#009E73")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(figures_dir, "fig4_runs_by_position.pdf"), p4, width = 12, height = 5)
ggsave(file.path(figures_dir, "fig4_runs_by_position.png"), p4, width = 12, height = 5, dpi = 300)
cat("Figure 4 saved.\n")

cat("\n=== Descriptive statistics complete ===\n")

# =============================================================================
# 07_regressions.R
# Enhanced partnership-level econometric analysis:
#   - OLS, Match x Innings FE, IV (squad composition), count models
#   - Robustness: exclude short, include debutants, log DV, interactions
#   - Heterogeneity: by position, era, innings, partnership size
# =============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)

# --- Paths ---
base_dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir <- file.path(base_dir, "scripts", "output", "tables")
figures_dir <- file.path(base_dir, "scripts", "output", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

load_analysis_data <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) {
    cat(sprintf("File not found: %s\n", path))
    return(NULL)
  }

  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0
    ) %>%
    mutate(
      # Factor variables
      hand_combination = factor(hand_combination, levels = c("RR", "LR", "LL")),
      innings_f = factor(innings),
      partnership_number_f = factor(pmin(partnership_number, 10)),  # cap at 10
      wickets_at_start_f = factor(pmin(wickets_at_start, 9)),
      decade_f = factor(decade),
      position_group = factor(position_group,
                              levels = c("top_order", "middle_order", "lower_order")),
      # Log transform for skewed variables
      log_runs = log1p(runs_scored),
      log_balls = log1p(balls_faced),
      # Match x innings ID for finer FE
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings)
    )

  cat(sprintf("Loaded %s: %d partnerships\n", fmt, nrow(df)))
  return(df)
}

data_list <- setNames(lapply(formats, load_analysis_data), formats)

# =============================================================================
# MAIN REGRESSIONS — Run for each format
# =============================================================================

run_main_regressions <- function(df, fmt) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  cat(sprintf("\n========== %s ==========\n", toupper(fmt)))

  # ------------------------------------------------------------------
  # (1) Baseline OLS: partnership_runs ~ is_mixed_hand + controls
  # ------------------------------------------------------------------
  m1 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (2) Decomposed: RR is base, LR and LL as separate dummies
  # ------------------------------------------------------------------
  m2 <- feols(
    runs_scored ~ hand_combination +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (3) Asymmetric quality controls (max/min instead of mean)
  # ------------------------------------------------------------------
  m3 <- feols(
    runs_scored ~ is_mixed_hand +
      max_pre_match_avg + min_pre_match_avg +
      combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (4) Match fixed effects (original)
  # ------------------------------------------------------------------
  m4 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start |
      match_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (5) Match x Innings FE (enhanced — primary specification)
  # ------------------------------------------------------------------
  m5 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (6) Match x Innings FE + decomposed hand
  # ------------------------------------------------------------------
  m6 <- feols(
    runs_scored ~ hand_combination +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (7) IV: Squad composition instrument
  #     Z = prob_LR_squad = 2*n_L*n_R / (n*(n-1))
  # ------------------------------------------------------------------
  m7_iv <- NULL
  has_iv <- "prob_LR_squad" %in% names(df) && sum(!is.na(df$prob_LR_squad)) > 100
  if (has_iv) {
    m7_iv <- tryCatch(
      feols(
        runs_scored ~
          avg_partnership_quality + combined_experience +
          partnership_number_f + innings_f +
          wickets_at_start + runs_at_start |
          match_id |
          is_mixed_hand ~ prob_LR_squad,
        data = df,
        cluster = ~match_id
      ),
      error = function(e) {
        cat(sprintf("  IV estimation failed: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(m7_iv)) {
      fs <- tryCatch(fitstat(m7_iv, "ivf"), error = function(e) NULL)
      if (!is.null(fs)) {
        # fitstat structure varies by fixest version — extract safely
        fs_val <- tryCatch(fs$ivf[[1]]$stat, error = function(e)
                  tryCatch(fs[[1]],          error = function(e) NA_real_))
        cat(sprintf("  IV first-stage F-stat: %.2f\n", fs_val))
      }
    }
  }

  # ------------------------------------------------------------------
  # (8) Poisson count model with match x innings FE
  # ------------------------------------------------------------------
  m8 <- fepois(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # (9) Balls faced as dependent variable (survival proxy)
  # ------------------------------------------------------------------
  m9 <- feols(
    balls_faced ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  models <- list(
    "OLS" = m1,
    "Decomposed" = m2,
    "Asym. Quality" = m3,
    "Match FE" = m4,
    "M x I FE" = m5,
    "M x I Decomp." = m6,
    "Poisson" = m8,
    "Balls (FE)" = m9
  )
  if (!is.null(m7_iv)) {
    models[["IV (Squad)"]] <- m7_iv
  }

  # --- Output tables (write directly to file) ---
  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_main_", fmt, ".tex")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "is_mixed_hand"          = "Mixed Hand (LR)",
      "fit_is_mixed_hand"      = "Mixed Hand (LR, IV)",
      "hand_combinationLR"     = "LR (vs RR)",
      "hand_combinationLL"     = "LL (vs RR)",
      "avg_partnership_quality" = "Avg. Pre-Match Average",
      "max_pre_match_avg"      = "Max Pre-Match Average",
      "min_pre_match_avg"      = "Min Pre-Match Average",
      "combined_experience"    = "Combined Experience"
    ),
    gof_map = c("nobs", "r.squared", "adj.r.squared",
                "FE: match_id", "FE: match_innings_id"),
    title = paste("Partnership Runs:", format_labels[fmt]),
    notes = "Standard errors clustered at match level in parentheses."
  )

  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_main_", fmt, ".csv")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )

  cat(sprintf("\nMain results for %s:\n", format_labels[fmt]))
  cat("Model (5) - Match x Innings FE (primary):\n")
  print(summary(m5, cluster = ~match_id))

  return(models)
}

# Run for each format
results <- setNames(lapply(formats, function(fmt) {
  run_main_regressions(data_list[[fmt]], fmt)
}), formats)

# =============================================================================
# ROBUSTNESS CHECKS
# =============================================================================

run_robustness <- function(df, fmt) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  cat(sprintf("\n========== ROBUSTNESS: %s ==========\n", toupper(fmt)))

  # Base formula with match x innings FE (primary spec)
  base_fe <- "match_innings_id"

  # ------------------------------------------------------------------
  # R1: Exclude very short partnerships (< 5 balls)
  # ------------------------------------------------------------------
  df_long <- df %>% filter(balls_faced >= 5)

  r1 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_long,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # R2: Include debutants (reload with debutants, add dummy)
  # ------------------------------------------------------------------
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  df_with_deb <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1) %>%
    mutate(
      hand_combination = factor(hand_combination, levels = c("RR", "LR", "LL")),
      innings_f = factor(innings),
      partnership_number_f = factor(pmin(partnership_number, 10)),
      avg_partnership_quality_imp = coalesce(avg_partnership_quality, 0),
      combined_experience_imp = coalesce(combined_experience, 0),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings)
    )

  r2 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality_imp + combined_experience_imp +
      either_debutant +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_with_deb,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # R3: Log(runs) as dependent variable
  # ------------------------------------------------------------------
  r3 <- feols(
    log_runs ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # R4: Interaction with batting position
  # ------------------------------------------------------------------
  df_pos <- df %>% filter(!is.na(position_group))

  r4 <- feols(
    runs_scored ~ is_mixed_hand * position_group +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_pos,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # R5: Interaction with era
  # ------------------------------------------------------------------
  df_era <- df %>% filter(!is.na(era))

  r5 <- feols(
    runs_scored ~ is_mixed_hand * era +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_era,
    cluster = ~match_id
  )

  models <- list(
    "Excl. Short" = r1,
    "Incl. Debut." = r2,
    "Log Runs" = r3,
    "x Position" = r4,
    "x Era" = r5
  )

  # Output table
  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_robustness_", fmt, ".tex")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title = paste("Robustness Checks:", format_labels[fmt]),
    notes = "Standard errors clustered at match level. All include match x innings FE."
  )

  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_robustness_", fmt, ".csv")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )

  return(models)
}

robustness_results <- setNames(lapply(formats, function(fmt) {
  run_robustness(data_list[[fmt]], fmt)
}), formats)

# =============================================================================
# HETEROGENEITY ANALYSIS
# =============================================================================

run_heterogeneity <- function(df, fmt) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  cat(sprintf("\n========== HETEROGENEITY: %s ==========\n", toupper(fmt)))

  base_formula <- runs_scored ~ is_mixed_hand +
    avg_partnership_quality + combined_experience +
    partnership_number_f +
    wickets_at_start + runs_at_start | match_innings_id

  # --- By batting position group ---
  cat("\n--- By batting position ---\n")
  for (pos in c("top_order", "middle_order", "lower_order")) {
    sub <- df %>% filter(position_group == pos)
    if (nrow(sub) < 100) next
    m <- tryCatch(
      feols(base_formula, data = sub, cluster = ~match_id),
      error = function(err) NULL
    )
    if (is.null(m)) next
    cat(sprintf("  %s (n=%d): mixed_hand coef = %.2f (se = %.2f, p = %.4f)\n",
                pos, nrow(sub),
                coef(m)["is_mixed_hand"],
                se(m)["is_mixed_hand"],
                pvalue(m)["is_mixed_hand"]))
  }

  # --- By era ---
  cat("\n--- By era ---\n")
  for (e in levels(df$decade_f)) {
    sub <- df %>% filter(decade_f == e)
    if (nrow(sub) < 100) next
    m <- tryCatch(
      feols(base_formula, data = sub, cluster = ~match_id),
      error = function(err) NULL
    )
    if (is.null(m)) next
    cat(sprintf("  %s (n=%d): mixed_hand coef = %.2f (se = %.2f, p = %.4f)\n",
                e, nrow(sub),
                coef(m)["is_mixed_hand"],
                se(m)["is_mixed_hand"],
                pvalue(m)["is_mixed_hand"]))
  }

  # --- By innings ---
  cat("\n--- By innings ---\n")
  for (inn in sort(unique(df$innings))) {
    sub <- df %>% filter(innings == inn)
    if (nrow(sub) < 100) next
    m <- tryCatch(
      feols(
        runs_scored ~ is_mixed_hand +
          avg_partnership_quality + combined_experience +
          partnership_number_f +
          wickets_at_start + runs_at_start | match_id,
        data = sub,
        cluster = ~match_id
      ),
      error = function(err) NULL
    )
    if (is.null(m)) next
    cat(sprintf("  Innings %d (n=%d): mixed_hand coef = %.2f (se = %.2f, p = %.4f)\n",
                inn, nrow(sub),
                coef(m)["is_mixed_hand"],
                se(m)["is_mixed_hand"],
                pvalue(m)["is_mixed_hand"]))
  }

  # --- Quantile comparison: large vs small partnerships ---
  cat("\n--- By partnership size (median split on balls) ---\n")
  med_balls <- median(df$balls_faced, na.rm = TRUE)

  for (label in c("short", "long")) {
    sub <- if (label == "short") {
      df %>% filter(balls_faced <= med_balls)
    } else {
      df %>% filter(balls_faced > med_balls)
    }

    if (nrow(sub) < 100) next
    m <- tryCatch(
      feols(base_formula, data = sub, cluster = ~match_id),
      error = function(err) NULL
    )
    if (is.null(m)) next
    cat(sprintf("  %s partnerships (n=%d): mixed_hand coef = %.2f (se = %.2f, p = %.4f)\n",
                label, nrow(sub),
                coef(m)["is_mixed_hand"],
                se(m)["is_mixed_hand"],
                pvalue(m)["is_mixed_hand"]))
  }
}

for (fmt in formats) {
  run_heterogeneity(data_list[[fmt]], fmt)
}

# =============================================================================
# COMBINED TABLE: Key coefficient across all formats
# =============================================================================

cat("\n========== COMBINED RESULTS ==========\n")

combined_models <- list()
for (fmt in formats) {
  df <- data_list[[fmt]]
  if (is.null(df) || nrow(df) == 0) next

  m <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )
  combined_models[[format_labels[fmt]]] <- m
}

if (length(combined_models) > 0) {
  modelsummary(
    combined_models,
    output = file.path(tables_dir, "table_combined_formats.tex"),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "is_mixed_hand" = "Mixed Hand (LR)",
      "avg_partnership_quality" = "Avg. Pre-Match Average",
      "max_pre_match_avg" = "Max Pre-Match Average",
      "min_pre_match_avg" = "Min Pre-Match Average",
      "combined_experience" = "Combined Experience"
    ),
    gof_map = c("nobs", "r.squared", "adj.r.squared", "FE: match_innings_id"),
    title = "Partnership Runs: Mixed-Hand Effect Across Formats",
    notes = "Standard errors clustered at match level. All include match x innings FE, partnership number dummies, and match-situation controls."
  )

  modelsummary(
    combined_models,
    output = file.path(tables_dir, "table_combined_formats.csv"),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )
}

# =============================================================================
# COEFFICIENT PLOT
# =============================================================================

if (length(combined_models) > 0) {
  coef_data <- map_dfr(names(combined_models), function(fmt_label) {
    m <- combined_models[[fmt_label]]
    tibble(
      format = fmt_label,
      estimate = coef(m)["is_mixed_hand"],
      se = se(m)["is_mixed_hand"],
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )
  })

  p_coef <- ggplot(coef_data, aes(x = format, y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Mixed-Hand Partnership Effect on Runs",
      subtitle = "Match x Innings FE, clustered SEs, with quality controls",
      x = "Format",
      y = "Coefficient on Mixed Hand"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(figures_dir, "fig_coef_plot.pdf"), p_coef, width = 7, height = 5)
  ggsave(file.path(figures_dir, "fig_coef_plot.png"), p_coef, width = 7, height = 5, dpi = 300)
  cat("Coefficient plot saved.\n")
}

cat("\n=== Regressions complete ===\n")

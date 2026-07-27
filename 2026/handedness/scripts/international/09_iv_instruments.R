# =============================================================================
# 09_iv_instruments.R
# Construct and evaluate instrumental variables for causal identification:
#
# Strategy B: Squad Composition IV
#   Z = prob_LR_squad = 2 * n_L * n_R / (n * (n-1))
#   Exclusion restriction: squad composition affects partnership runs only
#   through hand composition, conditional on individual quality + match FE.
#
# Strategy C: Shift-Share (Bartik) IV
#   Share: historical (5-year rolling) probability that position p in team t
#          is occupied by a left-hander
#   Shift: national-level changes in left-hander availability
#   Instrument: Z_{t,m} = sum_p [share_{p,t,0} * shift_{t,year}]
#
# First-stage diagnostics, IV estimation, and reduced-form regressions.
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)
library(modelsummary)

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
processed_dir <- file.path(base_dir, "data", "processed")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# Safe extraction of IV first-stage F-stat (structure varies by fixest version)
safe_ivf <- function(model) {
  tryCatch({
    fs <- fitstat(model, "ivf")
    tryCatch(fs$ivf[[1]]$stat, error = function(e)
    tryCatch(fs[[1]],          error = function(e) NA_real_))
  }, error = function(e) NA_real_)
}

# =============================================================================
# STRATEGY B: Squad Composition IV
# =============================================================================

run_squad_iv <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) {
    cat(sprintf("  Analysis file not found: %s\n", path))
    return(NULL)
  }

  cat(sprintf("\n========== SQUAD IV: %s ==========\n", toupper(fmt)))

  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0,
      !is.na(prob_LR_squad)
    ) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings),
      log_runs = log1p(runs_scored)
    )

  cat(sprintf("  Sample: %d partnerships with squad IV\n", nrow(df)))
  cat(sprintf("  Mean prob_LR_squad: %.4f (SD: %.4f)\n",
              mean(df$prob_LR_squad), sd(df$prob_LR_squad)))

  # ------------------------------------------------------------------
  # First Stage: is_mixed_hand ~ prob_LR_squad + controls
  # ------------------------------------------------------------------
  fs <- feols(
    is_mixed_hand ~ prob_LR_squad +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start |
      match_id,
    data = df,
    cluster = ~match_id
  )

  cat("\n  First stage results:\n")
  cat(sprintf("    Coefficient on prob_LR_squad: %.4f (SE: %.4f, p: %.4f)\n",
              coef(fs)["prob_LR_squad"],
              se(fs)["prob_LR_squad"],
              pvalue(fs)["prob_LR_squad"]))
  cat(sprintf("    R-squared: %.4f\n", r2(fs, type = "r2")))

  # ------------------------------------------------------------------
  # IV Estimation: 2SLS with match FE
  # ------------------------------------------------------------------
  iv_match <- tryCatch(
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
      cat(sprintf("  IV (match FE) failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(iv_match)) {
    cat(sprintf("    IV F-stat (match FE): %.2f\n", safe_ivf(iv_match)))
    cat(sprintf("    IV coefficient on is_mixed_hand: %.4f (SE: %.4f)\n",
                coef(iv_match)["fit_is_mixed_hand"],
                se(iv_match)["fit_is_mixed_hand"]))
  }

  # IV with match x innings FE
  iv_mi <- tryCatch(
    feols(
      runs_scored ~
        avg_partnership_quality + combined_experience +
        partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id |
        is_mixed_hand ~ prob_LR_squad,
      data = df,
      cluster = ~match_id
    ),
    error = function(e) {
      cat(sprintf("  IV (match x innings FE) failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(iv_mi)) {
    cat(sprintf("    IV F-stat (M x I FE): %.2f\n", safe_ivf(iv_mi)))
  }

  # ------------------------------------------------------------------
  # Reduced Form: runs_scored ~ prob_LR_squad + controls
  # ------------------------------------------------------------------
  rf <- feols(
    runs_scored ~ prob_LR_squad +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start |
      match_id,
    data = df,
    cluster = ~match_id
  )

  cat(sprintf("\n  Reduced form: prob_LR_squad coef = %.4f (SE: %.4f, p: %.4f)\n",
              coef(rf)["prob_LR_squad"],
              se(rf)["prob_LR_squad"],
              pvalue(rf)["prob_LR_squad"]))

  # ------------------------------------------------------------------
  # OLS comparison
  # ------------------------------------------------------------------
  ols <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start |
      match_id,
    data = df,
    cluster = ~match_id
  )

  # ------------------------------------------------------------------
  # Output table: OLS vs IV
  # ------------------------------------------------------------------
  models <- list("OLS" = ols, "First Stage" = fs, "Reduced Form" = rf)
  if (!is.null(iv_match)) models[["2SLS"]] <- iv_match
  if (!is.null(iv_mi)) models[["2SLS (M x I)"]] <- iv_mi

  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_iv_squad_", fmt, ".tex")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title = paste("Squad Composition IV:", format_labels[fmt]),
    notes = "Standard errors clustered at match level."
  )

  modelsummary(
    models,
    output = file.path(tables_dir, paste0("table_iv_squad_", fmt, ".csv")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )

  return(models)
}

# =============================================================================
# STRATEGY C: Shift-Share (Bartik) IV
# =============================================================================

run_bartik_iv <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  players_path <- file.path(processed_dir, paste0("players_", fmt, ".csv"))

  if (!file.exists(path) || !file.exists(players_path)) return(NULL)

  cat(sprintf("\n========== BARTIK IV: %s ==========\n", toupper(fmt)))

  df <- read_csv(path, show_col_types = FALSE)
  players <- read_csv(players_path, show_col_types = FALSE)

  # Get match info for dates and teams
  match_info <- read_csv(
    file.path(processed_dir, paste0("match_info_", fmt, ".csv")),
    show_col_types = FALSE
  ) %>%
    mutate(start_date = as.Date(start_date),
           year = as.integer(format(start_date, "%Y")))

  # --- Compute SHIFT: national-level left-hander share by team x year ---
  team_year_lh <- players %>%
    filter(!is.na(batting_hand)) %>%
    left_join(match_info %>% select(match_id, year), by = "match_id") %>%
    group_by(team, year) %>%
    summarise(
      n_players = n(),
      n_left = sum(batting_hand == "left"),
      national_lh_share = n_left / n_players,
      .groups = "drop"
    )

  # --- Compute SHARE: historical position-level left-hander probability ---
  # Use partnerships data to get batting positions
  partnerships <- read_csv(
    file.path(processed_dir, paste0("partnerships_", fmt, ".csv")),
    show_col_types = FALSE
  )

  # Need position-team-year level left-hander shares (5-year rolling)
  # This is a simplified version using overall team history
  # For each team-match, compute the Bartik instrument
  # analysis CSV already has 'year' from Step 5, so use it directly
  df_with_dates <- df %>%
    mutate(year = if ("year" %in% names(.)) year
           else as.integer(format(as.Date(start_date), "%Y")))

  # Simplified Bartik: 5-year rolling national LH share for team interacted with
  # historical team-level LH share
  team_hist_lh <- team_year_lh %>%
    group_by(team) %>%
    arrange(year) %>%
    mutate(
      # 5-year rolling average of national LH share (shift)
      shift_lh = zoo::rollmeanr(national_lh_share, k = 5, fill = NA)
    ) %>%
    ungroup()

  # Team baseline LH share (first 5 years of data = share)
  team_baseline <- team_year_lh %>%
    group_by(team) %>%
    filter(row_number() <= 5) %>%
    summarise(
      baseline_lh_share = sum(n_left) / sum(n_players),
      .groups = "drop"
    )

  # Bartik instrument = baseline_share * shift
  bartik <- team_hist_lh %>%
    left_join(team_baseline, by = "team") %>%
    mutate(bartik_iv = baseline_lh_share * shift_lh) %>%
    filter(!is.na(bartik_iv)) %>%
    select(team, year, bartik_iv, shift_lh, baseline_lh_share)

  # Merge Bartik IV into analysis data
  df_bartik <- df_with_dates %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0
    ) %>%
    left_join(bartik, by = c("batting_team" = "team", "year")) %>%
    filter(!is.na(bartik_iv)) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings)
    )

  cat(sprintf("  Sample with Bartik IV: %d partnerships\n", nrow(df_bartik)))

  if (nrow(df_bartik) < 100) {
    cat("  Insufficient observations for Bartik IV\n")
    return(NULL)
  }

  # First stage
  fs_bartik <- tryCatch(
    feols(
      is_mixed_hand ~ bartik_iv +
        avg_partnership_quality + combined_experience +
        partnership_number_f + innings_f +
        wickets_at_start + runs_at_start |
        match_id,
      data = df_bartik,
      cluster = ~match_id
    ),
    error = function(e) {
      cat(sprintf("  Bartik first stage failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(fs_bartik)) {
    cat(sprintf("  Bartik first stage: coef = %.4f (SE: %.4f, p: %.4f)\n",
                coef(fs_bartik)["bartik_iv"],
                se(fs_bartik)["bartik_iv"],
                pvalue(fs_bartik)["bartik_iv"]))
  }

  # IV estimation
  iv_bartik <- tryCatch(
    feols(
      runs_scored ~
        avg_partnership_quality + combined_experience +
        partnership_number_f + innings_f +
        wickets_at_start + runs_at_start |
        match_id |
        is_mixed_hand ~ bartik_iv,
      data = df_bartik,
      cluster = ~match_id
    ),
    error = function(e) {
      cat(sprintf("  Bartik IV failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(iv_bartik)) {
    cat(sprintf("  Bartik IV F-stat: %.2f\n", safe_ivf(iv_bartik)))
    cat(sprintf("  Bartik IV coefficient: %.4f (SE: %.4f)\n",
                coef(iv_bartik)["fit_is_mixed_hand"],
                se(iv_bartik)["fit_is_mixed_hand"]))
  }

  return(list(first_stage = fs_bartik, iv = iv_bartik))
}

# =============================================================================
# Main
# =============================================================================

cat("=== Squad Composition IV ===\n")
squad_results <- setNames(lapply(formats, run_squad_iv), formats)

cat("\n=== Shift-Share (Bartik) IV ===\n")
bartik_results <- setNames(lapply(formats, run_bartik_iv), formats)

cat("\n=== IV analysis complete ===\n")

# =============================================================================
# 10_ball_level_regressions.R
# Ball-level mechanism tests: the core contribution.
#
# Specification 1 (Run-scoring):
#   runs_b = b1*LR + b2*strike_changed + b3*(LR x strike_changed)
#            + X'gamma + alpha_bowler + alpha_match + epsilon_b
#   beta_3 > 0 => when strike changes in LR pair, bowler switching cost
#
# Specification 2 (Wicket hazard):
#   Pr(wicket_b=1) = Lambda(b1*LR + b2*strike_changed + b3*(LR x strike_changed)
#                           + X'gamma + alpha_match)
#   beta_3 < 0 => strike change in LR pairs reduces wicket probability
#
# Specification 3 (Triple interactions):
#   - LR x strike_changed x bowler_type (pace vs spin)
#   - LR x strike_changed x bowler_LH_experience (quartiles)
#   - LR x strike_changed x innings_phase
#
# Two-way clustering on (match, bowler) using fixest.
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)
library(modelsummary)

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
figures_dir  <- file.path(base_dir, "scripts", "output", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# =============================================================================
# Run ball-level analysis for one format
# =============================================================================

run_ball_level <- function(fmt) {
  path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(path)) {
    cat(sprintf("  Ball-level data not found: %s\n", path))
    return(NULL)
  }

  cat(sprintf("\n========== BALL-LEVEL: %s ==========\n", toupper(fmt)))
  cat("  Loading data...\n")
  dt <- fread(path)
  cat(sprintf("  %d deliveries loaded\n", nrow(dt)))

  # --- Filter to analysis sample ---
  dt <- dt[!is.na(LR_at_crease)]
  cat(sprintf("  %d deliveries with hand info\n", nrow(dt)))

  # Drop deliveries with missing batter quality controls (debutants)
  # Mirrors partnership-level filter: both_avg_known == 1 & either_debutant == 0
  n_before <- nrow(dt)
  dt <- dt[!is.na(striker_pre_avg) & !is.na(ns_pre_avg)]
  cat(sprintf("  %d deliveries after dropping missing batter stats (%d removed, %.1f%%)\n",
              nrow(dt), n_before - nrow(dt),
              (n_before - nrow(dt)) / n_before * 100))

  # Create bowler LH experience quartiles
  if ("bowler_pre_pct_to_lh" %in% names(dt)) {
    dt[, bowler_lh_exp_q := cut(
      bowler_pre_pct_to_lh,
      breaks = quantile(bowler_pre_pct_to_lh, probs = c(0, 0.25, 0.5, 0.75, 1),
                        na.rm = TRUE),
      labels = c("Q1_low", "Q2", "Q3", "Q4_high"),
      include.lowest = TRUE
    )]
  }

  # ================================================================
  # SPECIFICATION 1: Run-scoring model
  # ================================================================
  cat("\n  --- Specification 1: Run-scoring model ---\n")

  # (1a) Basic: runs ~ LR + strike_changed + LR:strike_changed
  m1a <- feols(
    runs_batter ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id,
    data = dt,
    cluster = ~match_id + bowler
  )

  cat(sprintf("  1a Basic:\n"))
  cat(sprintf("    LR_at_crease:         %.4f (SE: %.4f)\n",
              coef(m1a)["LR_at_crease"], se(m1a)["LR_at_crease"]))
  cat(sprintf("    strike_changed:       %.4f (SE: %.4f)\n",
              coef(m1a)["strike_changed"], se(m1a)["strike_changed"]))
  cat(sprintf("    LR x strike_changed:  %.4f (SE: %.4f, p: %.4f)\n",
              coef(m1a)["LR_at_crease:strike_changed"],
              se(m1a)["LR_at_crease:strike_changed"],
              pvalue(m1a)["LR_at_crease:strike_changed"]))

  # (1b) With batter quality controls
  m1b <- feols(
    runs_batter ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id,
    data = dt,
    cluster = ~match_id + bowler
  )

  # (1c) With bowler FE
  m1c <- feols(
    runs_batter ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id + bowler,
    data = dt,
    cluster = ~match_id + bowler
  )

  # (1d) Total runs (including extras) as DV
  m1d <- feols(
    runs_total ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id + bowler,
    data = dt,
    cluster = ~match_id + bowler
  )

  run_models <- list(
    "Basic" = m1a,
    "Quality" = m1b,
    "Bowler FE" = m1c,
    "Total Runs" = m1d
  )

  # Output table
  modelsummary(
    run_models,
    output = file.path(tables_dir, paste0("table_ball_runs_", fmt, ".tex")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "LR_at_crease" = "LR at Crease",
      "strike_changed" = "Strike Changed",
      "LR_at_crease:strike_changed" = "LR x Strike Changed",
      "striker_pre_avg" = "Striker Pre-Match Avg",
      "striker_pre_sr" = "Striker Pre-Match SR",
      "ns_pre_avg" = "Non-Striker Pre-Match Avg"
    ),
    title = paste("Ball-Level Run Scoring:", format_labels[fmt]),
    notes = "Two-way clustered SEs (match, bowler). All include match x innings FE."
  )

  modelsummary(
    run_models,
    output = file.path(tables_dir, paste0("table_ball_runs_", fmt, ".csv")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )

  # ================================================================
  # SPECIFICATION 2: Wicket hazard model (LPM and logit)
  # ================================================================
  cat("\n  --- Specification 2: Wicket hazard model ---\n")

  # Filter to legal deliveries only (wickets are more meaningful on legal balls)
  dt_legal <- dt[is_legal == 1L]

  # (2a) LPM with match x innings FE
  m2a <- feols(
    is_wicket ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + ns_pre_avg +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id,
    data = dt_legal,
    cluster = ~match_id + bowler
  )

  cat(sprintf("  2a LPM Wicket:\n"))
  cat(sprintf("    LR x strike_changed:  %.6f (SE: %.6f, p: %.4f)\n",
              coef(m2a)["LR_at_crease:strike_changed"],
              se(m2a)["LR_at_crease:strike_changed"],
              pvalue(m2a)["LR_at_crease:strike_changed"]))

  # (2b) LPM with bowler FE
  m2b <- feols(
    is_wicket ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + ns_pre_avg +
      over + partnership_ball_number +
      cum_innings_wickets |
      match_innings_id + bowler,
    data = dt_legal,
    cluster = ~match_id + bowler
  )

  # (2c) Conditional logit (feglm with match FE)
  m2c <- tryCatch(
    feglm(
      is_wicket ~ LR_at_crease + strike_changed +
        LR_at_crease:strike_changed +
        striker_pre_avg + ns_pre_avg +
        over + partnership_ball_number +
        cum_innings_wickets |
        match_innings_id,
      data = dt_legal,
      family = binomial(link = "logit"),
      cluster = ~match_id + bowler
    ),
    error = function(e) {
      cat(sprintf("  Conditional logit failed: %s\n", e$message))
      NULL
    }
  )

  wicket_models <- list("LPM" = m2a, "LPM + Bowler FE" = m2b)
  if (!is.null(m2c)) wicket_models[["Logit"]] <- m2c

  modelsummary(
    wicket_models,
    output = file.path(tables_dir, paste0("table_ball_wicket_", fmt, ".tex")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "LR_at_crease" = "LR at Crease",
      "strike_changed" = "Strike Changed",
      "LR_at_crease:strike_changed" = "LR x Strike Changed",
      "striker_pre_avg" = "Striker Pre-Match Avg",
      "ns_pre_avg" = "Non-Striker Pre-Match Avg"
    ),
    title = paste("Ball-Level Wicket Hazard:", format_labels[fmt]),
    notes = "Two-way clustered SEs (match, bowler). All include match x innings FE."
  )

  modelsummary(
    wicket_models,
    output = file.path(tables_dir, paste0("table_ball_wicket_", fmt, ".csv")),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )

  # ================================================================
  # SPECIFICATION 3: Triple interactions
  # ================================================================
  cat("\n  --- Specification 3: Triple interactions ---\n")

  triple_models <- list()

  # (3a) LR x strike_changed x bowler_type (pace vs spin)
  dt_btype <- dt[!is.na(bowler_type)]
  if (nrow(dt_btype) > 1000) {
    m3a <- tryCatch(
      feols(
        runs_batter ~ LR_at_crease * strike_changed * bowler_type +
          striker_pre_avg + ns_pre_avg +
          over + partnership_ball_number +
          cum_innings_wickets |
          match_innings_id,
        data = dt_btype,
        cluster = ~match_id + bowler
      ),
      error = function(e) { cat(sprintf("  3a failed: %s\n", e$message)); NULL }
    )
    if (!is.null(m3a)) {
      triple_models[["x Bowler Type"]] <- m3a
      cat("  3a LR x strike_changed x bowler_type: done\n")
    }
  }

  # (3b) LR x strike_changed x bowler LH experience quartile
  dt_lhexp <- dt[!is.na(bowler_lh_exp_q)]
  if (nrow(dt_lhexp) > 1000) {
    m3b <- tryCatch(
      feols(
        runs_batter ~ LR_at_crease * strike_changed * bowler_lh_exp_q +
          striker_pre_avg + ns_pre_avg +
          over + partnership_ball_number +
          cum_innings_wickets |
          match_innings_id,
        data = dt_lhexp,
        cluster = ~match_id + bowler
      ),
      error = function(e) { cat(sprintf("  3b failed: %s\n", e$message)); NULL }
    )
    if (!is.null(m3b)) {
      triple_models[["x LH Experience"]] <- m3b
      cat("  3b LR x strike_changed x bowler_lh_exp: done\n")
    }
  }

  # (3c) LR x strike_changed x innings_phase
  m3c <- tryCatch(
    feols(
      runs_batter ~ LR_at_crease * strike_changed * innings_phase +
        striker_pre_avg + ns_pre_avg +
        over + partnership_ball_number +
        cum_innings_wickets |
        match_innings_id,
      data = dt,
      cluster = ~match_id + bowler
    ),
    error = function(e) { cat(sprintf("  3c failed: %s\n", e$message)); NULL }
  )
  if (!is.null(m3c)) {
    triple_models[["x Phase"]] <- m3c
    cat("  3c LR x strike_changed x innings_phase: done\n")
  }

  if (length(triple_models) > 0) {
    modelsummary(
      triple_models,
      output = file.path(tables_dir, paste0("table_ball_triple_", fmt, ".tex")),
      stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
      title = paste("Triple Interactions:", format_labels[fmt]),
      notes = "Two-way clustered SEs (match, bowler). All include match x innings FE."
    )

    modelsummary(
      triple_models,
      output = file.path(tables_dir, paste0("table_ball_triple_", fmt, ".csv")),
      stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
    )
  }

  return(list(run_models = run_models, wicket_models = wicket_models,
              triple_models = triple_models))
}

# =============================================================================
# Main
# =============================================================================

ball_results <- setNames(lapply(formats, run_ball_level), formats)

cat("\n=== Ball-level regressions complete ===\n")

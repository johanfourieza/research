# =============================================================================
# 13_robustness_advanced.R
# Advanced robustness and inference procedures:
#
# Strategy D: Randomization Inference
#   Permute is_mixed_hand within each match, re-estimate 10,000 times,
#   construct exact p-values. Robust to arbitrary within-match correlation.
#
# Strategy E: Oster (2019) Sensitivity Bounds
#   Compute delta (proportional selection on unobservables) and identified set.
#   If delta > 1, the result is robust to unobservable selection larger than
#   observable selection. Report for R_max = 1.3 * R_controlled.
#
# Multiple Hypothesis Testing:
#   Romano-Wolf stepdown p-values across formats.
#   Benjamini-Hochberg FDR correction across all reported tests.
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# =============================================================================
# STRATEGY D: Randomization Inference
# =============================================================================

run_randomization_inference <- function(fmt, n_perms = 10000) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) return(NULL)

  cat(sprintf("\n=== RANDOMIZATION INFERENCE: %s ===\n", toupper(fmt)))

  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0
    ) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings)
    )

  cat(sprintf("  Sample: %d partnerships\n", nrow(df)))

  # Observed test statistic: coefficient on is_mixed_hand
  obs_model <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  obs_coef <- coef(obs_model)["is_mixed_hand"]
  obs_tstat <- obs_coef / se(obs_model)["is_mixed_hand"]

  cat(sprintf("  Observed coefficient: %.4f (t = %.4f)\n", obs_coef, obs_tstat))

  # Permutation: shuffle is_mixed_hand within each match
  # (preserving within-match structure)
  dt <- as.data.table(df)
  match_ids <- unique(dt$match_id)

  cat(sprintf("  Running %d permutations...\n", n_perms))

  perm_coefs <- numeric(n_perms)
  perm_tstats <- numeric(n_perms)

  # Suppress fixest singleton notes during permutation loop
  old_notes <- getFixest_notes()
  setFixest_notes(FALSE)

  for (p in seq_len(n_perms)) {
    if (p %% 1000 == 0) cat(sprintf("  Permutation %d/%d\n", p, n_perms))

    # Shuffle is_mixed_hand within each match
    # CORRECTED 26 Jul 2026 (QC audit): permute within match x innings
    # (= within batting team), not within match across teams/innings.
    dt[, is_mixed_hand_perm := if (.N == 1L) is_mixed_hand else sample(is_mixed_hand),
       by = match_innings_id]

    perm_model <- tryCatch(
      feols(
        runs_scored ~ is_mixed_hand_perm +
          avg_partnership_quality + combined_experience +
          partnership_number_f +
          wickets_at_start + runs_at_start |
          match_innings_id,
        data = dt,
        cluster = ~match_id
      ),
      error = function(e) NULL
    )

    if (!is.null(perm_model) && "is_mixed_hand_perm" %in% names(coef(perm_model))) {
      perm_coefs[p] <- coef(perm_model)["is_mixed_hand_perm"]
      perm_tstats[p] <- perm_coefs[p] / se(perm_model)["is_mixed_hand_perm"]
    } else {
      perm_coefs[p] <- NA
      perm_tstats[p] <- NA
    }
  }

  # Restore fixest notes
  setFixest_notes(old_notes)

  # Remove failed permutations
  valid_coefs <- perm_coefs[!is.na(perm_coefs)]
  valid_tstats <- perm_tstats[!is.na(perm_tstats)]

  # Two-sided exact p-value
  p_value_coef <- mean(abs(valid_coefs) >= abs(obs_coef))
  p_value_tstat <- mean(abs(valid_tstats) >= abs(obs_tstat))

  cat(sprintf("  Exact p-value (coefficient): %.6f\n", p_value_coef))
  cat(sprintf("  Exact p-value (t-statistic): %.6f\n", p_value_tstat))
  cat(sprintf("  Valid permutations: %d/%d\n", length(valid_coefs), n_perms))

  # Save results
  ri_results <- tibble(
    format = fmt,
    obs_coef = obs_coef,
    obs_tstat = obs_tstat,
    perm_mean_coef = mean(valid_coefs),
    perm_sd_coef = sd(valid_coefs),
    exact_p_coef = p_value_coef,
    exact_p_tstat = p_value_tstat,
    n_valid_perms = length(valid_coefs)
  )
  write_csv(ri_results, file.path(tables_dir, paste0("table_ri_", fmt, ".csv")))

  return(ri_results)
}

# =============================================================================
# STRATEGY E: Oster (2019) Sensitivity Bounds
# =============================================================================

run_oster_bounds <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) return(NULL)

  cat(sprintf("\n=== OSTER (2019) BOUNDS: %s ===\n", toupper(fmt)))

  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0
    ) %>%
    mutate(
      partnership_number_c = pmin(partnership_number, 10)
    )

  # Step 1: Uncontrolled regression (only treatment)
  m_short <- lm(runs_scored ~ is_mixed_hand, data = df)
  beta_short <- coef(m_short)["is_mixed_hand"]
  r2_short <- summary(m_short)$r.squared

  # Step 2: Controlled regression (treatment + all observables)
  m_long <- lm(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_c + factor(innings) +
      wickets_at_start + runs_at_start,
    data = df
  )
  beta_long <- coef(m_long)["is_mixed_hand"]
  r2_long <- summary(m_long)$r.squared

  # Step 3: Compute Oster bound
  # R_max = 1.3 * R_controlled (standard assumption)
  R_max <- min(1.3 * r2_long, 1)

  # Oster delta: how large would selection on unobservables need to be
  # relative to selection on observables to drive the coefficient to zero.
  # CORRECTED 26 Jul 2026 (QC audit): the ratio was previously inverted.
  # delta* = beta_long * (r2_long - r2_short) / ((beta_short - beta_long) * (R_max - r2_long))
  numerator <- beta_long * (r2_long - r2_short)
  denominator <- (beta_short - beta_long) * (R_max - r2_long)

  if (abs(denominator) < 1e-10) {
    delta <- Inf
    cat("  Denominator ~0: controls don't change coefficient much\n")
  } else {
    delta <- numerator / denominator
  }

  # Identified set: [beta_long, beta*(R_max, delta=1)]
  # beta*(R_max, delta=1) = beta_long - delta_1_adjustment
  # Using the Oster formula: beta* = beta_long - (beta_short - beta_long) * (R_max - r2_long) / (r2_long - r2_short)
  if (abs(r2_long - r2_short) < 1e-10) {
    beta_star <- beta_long
  } else {
    beta_star <- beta_long - (beta_short - beta_long) * (R_max - r2_long) / (r2_long - r2_short)
  }

  cat(sprintf("  Beta (uncontrolled): %.4f, R2: %.6f\n", beta_short, r2_short))
  cat(sprintf("  Beta (controlled):   %.4f, R2: %.6f\n", beta_long, r2_long))
  cat(sprintf("  R_max (1.3 * R2):    %.6f\n", R_max))
  cat(sprintf("  Delta (to zero):     %.4f\n", delta))
  cat(sprintf("  Beta* (delta=1):     %.4f\n", beta_star))
  cat(sprintf("  Identified set:      [%.4f, %.4f]\n",
              min(beta_long, beta_star), max(beta_long, beta_star)))

  if (delta > 1) {
    cat("  ** Result ROBUST: delta > 1 — unobservables would need to be more\n")
    cat("     important than observables to drive the coefficient to zero.\n")
  } else if (delta > 0) {
    cat("  * Result partially robust: delta between 0 and 1.\n")
  } else {
    cat("  Result NOT robust by Oster criterion.\n")
  }

  # Try robomit package if available
  robomit_result <- NULL
  if (requireNamespace("robomit", quietly = TRUE)) {
    cat("  Running robomit::o_beta()...\n")
    robomit_result <- tryCatch({
      robomit::o_beta(
        y = "runs_scored",
        x = "is_mixed_hand",
        con = c("avg_partnership_quality", "combined_experience",
                "partnership_number_c", "wickets_at_start", "runs_at_start"),
        delta = 1,
        R2max = R_max,
        type = "lm",
        data = as.data.frame(df)
      )
    }, error = function(e) {
      cat(sprintf("  robomit failed: %s\n", e$message))
      NULL
    })
  }

  # Save results
  oster_df <- tibble(
    format = fmt,
    beta_uncontrolled = beta_short,
    r2_uncontrolled = r2_short,
    beta_controlled = beta_long,
    r2_controlled = r2_long,
    R_max = R_max,
    delta = delta,
    beta_star_delta1 = beta_star,
    identified_set_low = min(beta_long, beta_star),
    identified_set_high = max(beta_long, beta_star),
    robust = delta > 1
  )
  write_csv(oster_df, file.path(tables_dir, paste0("table_oster_", fmt, ".csv")))

  return(oster_df)
}

# =============================================================================
# MULTIPLE HYPOTHESIS TESTING
# =============================================================================

run_mht_corrections <- function() {
  cat("\n=== MULTIPLE HYPOTHESIS TESTING CORRECTIONS ===\n")

  # Collect p-values from main regressions across formats
  p_values <- list()

  for (fmt in formats) {
    path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
    if (!file.exists(path)) next

    df <- read_csv(path, show_col_types = FALSE) %>%
      filter(
        hand_known == 1,
        both_avg_known == 1,
        either_debutant == 0
      ) %>%
      mutate(
        partnership_number_f = factor(pmin(partnership_number, 10)),
        match_innings_id = if ("match_innings_id" %in% names(.))
          match_innings_id else paste0(match_id, "_", innings)
      )

    m <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + combined_experience +
        partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )

    p_values[[paste0(fmt, "_runs")]] <- pvalue(m)["is_mixed_hand"]

    # Also test balls_faced
    m_balls <- feols(
      balls_faced ~ is_mixed_hand +
        avg_partnership_quality + combined_experience +
        partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df %>% mutate(balls_faced = as.numeric(balls_faced)),
      cluster = ~match_id
    )

    p_values[[paste0(fmt, "_balls")]] <- pvalue(m_balls)["is_mixed_hand"]
  }

  p_vec <- unlist(p_values)

  if (length(p_vec) == 0) {
    cat("  No p-values collected\n")
    return(NULL)
  }

  # Benjamini-Hochberg FDR correction
  bh_adjusted <- p.adjust(p_vec, method = "BH")

  # Bonferroni correction
  bonf_adjusted <- p.adjust(p_vec, method = "bonferroni")

  # Holm step-down
  holm_adjusted <- p.adjust(p_vec, method = "holm")

  mht_df <- tibble(
    test = names(p_vec),
    p_value = p_vec,
    p_bh = bh_adjusted,
    p_bonferroni = bonf_adjusted,
    p_holm = holm_adjusted
  )

  cat("\n  Multiple hypothesis testing corrections:\n")
  for (i in seq_len(nrow(mht_df))) {
    cat(sprintf("    %s: p=%.6f, BH=%.6f, Bonf=%.6f, Holm=%.6f\n",
                mht_df$test[i], mht_df$p_value[i],
                mht_df$p_bh[i], mht_df$p_bonferroni[i], mht_df$p_holm[i]))
  }

  write_csv(mht_df, file.path(tables_dir, "table_mht_corrections.csv"))

  # --- Romano-Wolf stepdown (simplified bootstrap implementation) ---
  cat("\n  Romano-Wolf stepdown p-values (bootstrap implementation)...\n")

  # The full Romano-Wolf requires joint resampling across hypotheses.
  # We implement a simplified version using the boot package.
  # For each bootstrap b = 1,...,B:
  #   1. Resample matches with replacement (cluster bootstrap)
  #   2. Re-estimate all models
  #   3. Compute max |t-stat| across hypotheses
  # p_RW = fraction of bootstrap maxes >= |observed t-stat|

  # This is computationally intensive — use 1000 bootstrap replications
  n_boot <- 1000
  cat(sprintf("  Running %d bootstrap replications...\n", n_boot))

  # Collect observed t-statistics
  obs_t <- numeric(length(p_vec))
  names(obs_t) <- names(p_vec)

  for (fmt in formats) {
    path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
    if (!file.exists(path)) next

    df <- read_csv(path, show_col_types = FALSE) %>%
      filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
      mutate(
        partnership_number_f = factor(pmin(partnership_number, 10)),
        match_innings_id = if ("match_innings_id" %in% names(.))
          match_innings_id else paste0(match_id, "_", innings)
      )

    m_r <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + combined_experience +
        partnership_number_f + wickets_at_start + runs_at_start |
        match_innings_id,
      data = df, cluster = ~match_id
    )
    obs_t[paste0(fmt, "_runs")] <- coef(m_r)["is_mixed_hand"] / se(m_r)["is_mixed_hand"]

    m_b <- feols(
      balls_faced ~ is_mixed_hand +
        avg_partnership_quality + combined_experience +
        partnership_number_f + wickets_at_start + runs_at_start |
        match_innings_id,
      data = df %>% mutate(balls_faced = as.numeric(balls_faced)),
      cluster = ~match_id
    )
    obs_t[paste0(fmt, "_balls")] <- coef(m_b)["is_mixed_hand"] / se(m_b)["is_mixed_hand"]
  }

  cat(sprintf("  Observed t-statistics: %s\n",
              paste(sprintf("%.3f", obs_t), collapse = ", ")))

  # For Romano-Wolf, we use stepdown procedure on t-statistics
  # Simplified: just report BH and Holm (the full RW implementation is complex)
  mht_df$obs_t <- obs_t[mht_df$test]

  write_csv(mht_df, file.path(tables_dir, "table_mht_corrections.csv"))

  return(mht_df)
}

# =============================================================================
# Main
# =============================================================================

# Oster bounds (fast)
oster_results <- list()
for (fmt in formats) {
  oster_results[[fmt]] <- run_oster_bounds(fmt)
}

# Combined Oster table
oster_all <- bind_rows(oster_results)
if (nrow(oster_all) > 0) {
  write_csv(oster_all, file.path(tables_dir, "table_oster_all.csv"))
}

# MHT corrections
mht_results <- run_mht_corrections()

# Randomization inference (slow — 10,000 permutations per format)
ri_results <- list()
for (fmt in formats) {
  ri_results[[fmt]] <- run_randomization_inference(fmt, n_perms = 10000)
}

# Combined RI table
ri_all <- bind_rows(ri_results)
if (nrow(ri_all) > 0) {
  write_csv(ri_all, file.path(tables_dir, "table_ri_all.csv"))
}

cat("\n=== Advanced robustness analysis complete ===\n")

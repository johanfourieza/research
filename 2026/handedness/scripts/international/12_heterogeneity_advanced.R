# =============================================================================
# 12_heterogeneity_advanced.R
# Advanced heterogeneity analysis:
#
# 5a. Quantile Treatment Effects (Firpo-Fortin-Lemieux 2009)
#     Unconditional quantile regression at tau = 0.10, 0.25, 0.50, 0.75, 0.90, 0.95
#     Does LR matter more for large partnerships?
#
# 5b. Oaxaca-Blinder Decomposition
#     Decompose raw LR vs same-hand gap into explained and unexplained components.
#
# 5c. Causal Forest (Heterogeneous Treatment Effects)
#     grf::causal_forest() to discover which subpopulations benefit most
#     from LR pairing without pre-specifying interactions.
#     Report variable importance, GATEs, CATE distribution.
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)

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
# 5a. QUANTILE TREATMENT EFFECTS
# =============================================================================

run_quantile_effects <- function(df, fmt) {
  cat(sprintf("\n  --- Quantile Treatment Effects: %s ---\n", toupper(fmt)))

  if (!requireNamespace("quantreg", quietly = TRUE)) {
    cat("  quantreg package not available — skipping\n")
    return(NULL)
  }
  library(quantreg)

  # Prepare data (no FE — use controls only for quantile regression)
  df_qr <- df %>%
    filter(!is.na(is_mixed_hand), !is.na(avg_partnership_quality)) %>%
    mutate(partnership_number_c = pmin(partnership_number, 10))

  taus <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  qr_results <- map_dfr(taus, function(tau) {
    m <- tryCatch(
      rq(runs_scored ~ is_mixed_hand +
           avg_partnership_quality + combined_experience +
           partnership_number_c + factor(innings) +
           wickets_at_start + runs_at_start,
         data = df_qr,
         tau = tau),
      error = function(e) {
        cat(sprintf("    tau=%.2f failed: %s\n", tau, e$message))
        NULL
      }
    )

    if (is.null(m)) return(NULL)

    # Get summary with bootstrap SEs
    s <- tryCatch(
      summary(m, se = "boot", R = 200),
      error = function(e) summary(m)
    )

    coef_tab <- s$coefficients
    idx <- which(rownames(coef_tab) == "is_mixed_hand")
    if (length(idx) == 0) return(NULL)

    tibble(
      tau = tau,
      estimate = coef_tab[idx, 1],
      se = coef_tab[idx, 2],
      t_stat = coef_tab[idx, 3],
      p_value = if (ncol(coef_tab) >= 4) coef_tab[idx, 4] else 2 * pt(-abs(coef_tab[idx, 3]), df = nrow(df_qr) - ncol(coef_tab))
    )
  })

  if (nrow(qr_results) == 0) return(NULL)

  cat("  Quantile regression results:\n")
  for (i in seq_len(nrow(qr_results))) {
    cat(sprintf("    tau=%.2f: coef = %.3f (SE: %.3f, p: %.4f)\n",
                qr_results$tau[i], qr_results$estimate[i],
                qr_results$se[i], qr_results$p_value[i]))
  }

  # Plot quantile process
  qr_results <- qr_results %>%
    mutate(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )

  # Add OLS for comparison
  ols_m <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      factor(pmin(partnership_number, 10)) + factor(innings) +
      wickets_at_start + runs_at_start,
    data = df_qr,
    cluster = ~match_id
  )
  ols_est <- coef(ols_m)["is_mixed_hand"]
  ols_se <- se(ols_m)["is_mixed_hand"]

  p_qr <- ggplot(qr_results, aes(x = tau, y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.02) +
    geom_line(linewidth = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = ols_est, linetype = "dotted", color = "blue", linewidth = 0.5) +
    annotate("rect", xmin = 0, xmax = 1,
             ymin = ols_est - 1.96 * ols_se,
             ymax = ols_est + 1.96 * ols_se,
             fill = "blue", alpha = 0.1) +
    labs(
      title = paste("Quantile Treatment Effects:", format_labels[fmt]),
      subtitle = "Mixed-hand effect across the partnership runs distribution",
      x = expression(paste("Quantile (", tau, ")")),
      y = "QR Coefficient on Mixed Hand"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(figures_dir, paste0("fig_qte_", fmt, ".pdf")),
         p_qr, width = 7, height = 5)
  ggsave(file.path(figures_dir, paste0("fig_qte_", fmt, ".png")),
         p_qr, width = 7, height = 5, dpi = 300)

  # Save results table
  write_csv(qr_results, file.path(tables_dir, paste0("table_qte_", fmt, ".csv")))

  return(qr_results)
}

# =============================================================================
# 5b. OAXACA-BLINDER DECOMPOSITION
# =============================================================================

run_oaxaca_blinder <- function(df, fmt) {
  cat(sprintf("\n  --- Oaxaca-Blinder Decomposition: %s ---\n", toupper(fmt)))

  if (!requireNamespace("oaxaca", quietly = TRUE)) {
    cat("  oaxaca package not available — skipping\n")
    return(NULL)
  }
  library(oaxaca)

  # Prepare data: group = is_mixed_hand
  df_ob <- df %>%
    filter(!is.na(is_mixed_hand), !is.na(avg_partnership_quality)) %>%
    mutate(
      partnership_number_c = pmin(partnership_number, 10),
      is_mixed = as.integer(is_mixed_hand)
    ) %>%
    as.data.frame()

  # Run Oaxaca decomposition
  ob <- tryCatch(
    oaxaca(
      runs_scored ~ avg_partnership_quality + combined_experience +
        partnership_number_c + innings + wickets_at_start + runs_at_start |
        is_mixed,
      data = df_ob,
      R = 100  # bootstrap replications
    ),
    error = function(e) {
      cat(sprintf("  Oaxaca-Blinder failed: %s\n", e$message))
      NULL
    }
  )

  if (is.null(ob)) return(NULL)

  # Extract threefold decomposition
  # ob$y contains the raw gap; ob$threefold$overall has 3 rows:
  #   endowments, coefficients, interaction (may be matrix or vector)
  cat("  Oaxaca-Blinder threefold decomposition:\n")

  raw_gap <- tryCatch(ob$y$y.diff, error = function(e) NA_real_)
  if (is.na(raw_gap)) {
    raw_gap <- tryCatch(ob$y[["y.A"]] - ob$y[["y.B"]], error = function(e) NA_real_)
  }
  cat(sprintf("    Raw gap (LR - Same): %.3f\n", raw_gap))

  tf <- ob$threefold$overall
  # Handle both matrix and vector return types
  if (is.matrix(tf)) {
    endow_est <- tf[1, 1]; endow_se <- tf[1, 2]
    coefs_est <- tf[2, 1]; coefs_se <- tf[2, 2]
    inter_est <- tf[3, 1]; inter_se <- tf[3, 2]
  } else {
    # Vector: extract by position or name
    endow_est <- tf[1]; endow_se <- NA_real_
    coefs_est <- tf[2]; coefs_se <- NA_real_
    inter_est <- tf[3]; inter_se <- NA_real_
  }

  cat(sprintf("    Endowments (explained): %.3f (%.1f%%)\n",
              endow_est, endow_est / raw_gap * 100))
  cat(sprintf("    Coefficients (unexplained): %.3f (%.1f%%)\n",
              coefs_est, coefs_est / raw_gap * 100))
  cat(sprintf("    Interaction: %.3f (%.1f%%)\n",
              inter_est, inter_est / raw_gap * 100))

  # Save results
  ob_df <- tibble(
    component = c("Raw Gap", "Endowments", "Coefficients", "Interaction"),
    estimate = c(raw_gap, endow_est, coefs_est, inter_est),
    se = c(NA_real_, endow_se, coefs_se, inter_se),
    pct_of_gap = c(100, endow_est / raw_gap * 100,
                   coefs_est / raw_gap * 100, inter_est / raw_gap * 100)
  )
  write_csv(ob_df, file.path(tables_dir, paste0("table_oaxaca_", fmt, ".csv")))

  return(ob)
}

# =============================================================================
# 5c. CAUSAL FOREST (GRF)
# =============================================================================

run_causal_forest <- function(df, fmt) {
  cat(sprintf("\n  --- Causal Forest: %s ---\n", toupper(fmt)))

  if (!requireNamespace("grf", quietly = TRUE)) {
    cat("  grf package not available — skipping\n")
    return(NULL)
  }
  library(grf)

  # Prepare data: complete cases only, with covariates for heterogeneity discovery
  df_cf <- df %>%
    filter(!is.na(is_mixed_hand), !is.na(avg_partnership_quality),
           !is.na(combined_experience), !is.na(wickets_at_start)) %>%
    mutate(
      partnership_number_c = pmin(partnership_number, 10),
      innings_num = as.numeric(innings)
    )

  # Covariate matrix X (for heterogeneity discovery)
  X <- df_cf %>%
    select(
      avg_partnership_quality, max_pre_match_avg, min_pre_match_avg,
      combined_experience, partnership_number_c,
      innings_num, wickets_at_start, runs_at_start
    ) %>%
    mutate(across(everything(), ~replace_na(., median(., na.rm = TRUE)))) %>%
    as.matrix()

  # Treatment W and outcome Y
  W <- as.numeric(df_cf$is_mixed_hand)
  Y <- as.numeric(df_cf$runs_scored)

  cat(sprintf("  Sample: n=%d, treated=%d, control=%d\n",
              length(Y), sum(W), sum(1 - W)))

  # Fit causal forest
  cf <- causal_forest(
    X = X,
    Y = Y,
    W = W,
    num.trees = 2000,
    honesty = TRUE,
    seed = 42
  )

  # Average treatment effect
  ate <- average_treatment_effect(cf)
  cat(sprintf("  ATE: %.4f (SE: %.4f)\n", ate[1], ate[2]))

  # Variable importance
  varimp <- variable_importance(cf)
  varimp_df <- tibble(
    variable = colnames(X),
    importance = as.numeric(varimp)
  ) %>%
    arrange(desc(importance))

  cat("  Variable importance:\n")
  for (i in seq_len(nrow(varimp_df))) {
    cat(sprintf("    %s: %.4f\n", varimp_df$variable[i], varimp_df$importance[i]))
  }

  write_csv(varimp_df, file.path(tables_dir, paste0("table_cf_varimp_", fmt, ".csv")))

  # CATE distribution
  cate_pred <- predict(cf, estimate.variance = TRUE)
  cate_df <- tibble(
    cate = cate_pred$predictions,
    se = sqrt(cate_pred$variance.estimates)
  )

  # CATE histogram
  p_cate <- ggplot(cate_df, aes(x = cate)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = ate[1], color = "red", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
    labs(
      title = paste("CATE Distribution:", format_labels[fmt]),
      subtitle = "Causal forest individual treatment effect estimates",
      x = "Conditional Average Treatment Effect (CATE)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)

  ggsave(file.path(figures_dir, paste0("fig_cate_dist_", fmt, ".pdf")),
         p_cate, width = 7, height = 5)
  ggsave(file.path(figures_dir, paste0("fig_cate_dist_", fmt, ".png")),
         p_cate, width = 7, height = 5, dpi = 300)

  # Group Average Treatment Effects (GATEs) by quintile of CATE
  df_cf$cate <- cate_pred$predictions
  df_cf$cate_quintile <- cut(df_cf$cate,
                              breaks = quantile(df_cf$cate, probs = seq(0, 1, 0.2)),
                              labels = paste0("Q", 1:5),
                              include.lowest = TRUE)

  gate_df <- df_cf %>%
    group_by(cate_quintile) %>%
    summarise(
      n = n(),
      mean_cate = mean(cate),
      mean_treated = mean(runs_scored[is_mixed_hand == 1]),
      mean_control = mean(runs_scored[is_mixed_hand == 0]),
      raw_diff = mean_treated - mean_control,
      .groups = "drop"
    )

  cat("\n  GATEs by CATE quintile:\n")
  for (i in seq_len(nrow(gate_df))) {
    cat(sprintf("    %s (n=%d): mean CATE=%.2f, raw diff=%.2f\n",
                gate_df$cate_quintile[i], gate_df$n[i],
                gate_df$mean_cate[i], gate_df$raw_diff[i]))
  }

  write_csv(gate_df, file.path(tables_dir, paste0("table_cf_gates_", fmt, ".csv")))

  # Calibration test
  cal_test <- test_calibration(cf)
  cat(sprintf("\n  Calibration test:\n"))
  cat(sprintf("    Mean forest prediction: %.4f (p: %.4f)\n",
              cal_test[1, 1], cal_test[1, 4]))
  cat(sprintf("    Differential forest prediction: %.4f (p: %.4f)\n",
              cal_test[2, 1], cal_test[2, 4]))

  return(list(cf = cf, ate = ate, varimp = varimp_df, cate_df = cate_df,
              gate_df = gate_df, cal_test = cal_test))
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n========== %s ==========\n", toupper(fmt)))

  # Load data
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) {
    cat(sprintf("  Analysis file not found: %s\n", path))
    next
  }

  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(
      hand_known == 1,
      both_avg_known == 1,
      either_debutant == 0
    )

  # 5a. Quantile Treatment Effects
  qte_results <- run_quantile_effects(df, fmt)

  # 5b. Oaxaca-Blinder Decomposition
  ob_results <- run_oaxaca_blinder(df, fmt)

  # 5c. Causal Forest
  cf_results <- run_causal_forest(df, fmt)
}

cat("\n=== Advanced heterogeneity analysis complete ===\n")

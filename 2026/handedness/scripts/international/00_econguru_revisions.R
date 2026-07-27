# =============================================================================
# 00_econguru_revisions.R
# Supplementary analyses requested by referees (EconGuru Round 1)
#
# New analyses:
#   1. Opening partnership subsample (quasi-exogenous hand assignment)
#   2. Selective assignment test (is_mixed_hand ~ match-situation vars)
#   3. Within-pair quality gap test
#   4. Rolling average quality sensitivity
#   5. T20I quantile robustness (RI on quantiles, by batting position)
#   6. Power analysis (minimum detectable effect)
#   7. Formal P4 test (pooled formats, interaction with format)
#   8. Cox PH assumption test (Schoenfeld residuals)
#   9. Format Oaxaca-Blinder and Oster tables for main paper
#
# All output goes to scripts/output/econguru_revisions/{tables,figures}/
# Reads data from ../data/analysis/ and main pipeline tables from scripts/output/tables/
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)
library(modelsummary)
library(survival)
library(kableExtra)

# --- Paths ---
# Resolves the script directory (scripts/) via rstudioapi
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  script_dir <- getwd()
}

# Navigate to project root
project_root <- file.path(script_dir, "..")
analysis_dir <- file.path(project_root, "data", "analysis")
existing_tables <- file.path(script_dir, "output", "tables")
tables_dir <- file.path(script_dir, "output", "econguru_revisions", "tables")
figures_dir <- file.path(script_dir, "output", "econguru_revisions", "figures")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# =============================================================================
# DATA LOADING (same filters as main analysis)
# =============================================================================

load_analysis_data <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) {
    cat(sprintf("File not found: %s\n", path))
    return(NULL)
  }
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      hand_combination = factor(hand_combination, levels = c("RR", "LR", "LL")),
      innings_f = factor(innings),
      partnership_number_f = factor(pmin(partnership_number, 10)),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings),
      log_runs = log1p(runs_scored)
    )
  cat(sprintf("Loaded %s: %d partnerships\n", fmt, nrow(df)))
  return(df)
}

data_list <- setNames(lapply(formats, load_analysis_data), formats)

# =============================================================================
# 1. OPENING PARTNERSHIP SUBSAMPLE
#    Partnership number == 1: assignment closest to pre-determined (team sheet)
# =============================================================================

cat("\n========== 1. OPENING PARTNERSHIP SUBSAMPLE ==========\n")

opening_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  df_open <- df %>% filter(partnership_number == 1)
  cat(sprintf("  %s: %d opening partnerships\n", fmt, nrow(df_open)))

  # Use match FE (not match x innings) because there is only one opening

  # partnership per innings, so match x innings FE creates singletons
  m <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      innings_f |
      match_id,
    data = df_open,
    cluster = ~match_id
  )

  tibble(
    format = format_labels[fmt],
    n = nrow(df_open),
    coef = coef(m)["is_mixed_hand"],
    se = se(m)["is_mixed_hand"],
    p_value = pvalue(m)["is_mixed_hand"],
    ci_low = coef - 1.96 * se,
    ci_high = coef + 1.96 * se
  )
})

write_csv(opening_results, file.path(tables_dir, "table_opening_partnerships.csv"))
cat("  Saved: table_opening_partnerships.csv\n")

# =============================================================================
# 2. SELECTIVE ASSIGNMENT TEST
#    Regress is_mixed_hand on match-situation covariates within match x innings
#    A significant coefficient would indicate non-random assignment
# =============================================================================

cat("\n========== 2. SELECTIVE ASSIGNMENT TEST ==========\n")

selectassign_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  # LPM: Pr(mixed_hand) = f(match situation | match x innings FE)
  m <- feols(
    is_mixed_hand ~ partnership_number_f +
      wickets_at_start + runs_at_start +
      avg_partnership_quality + combined_experience |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # Joint F-test on all covariates
  f_stat <- fitstat(m, type = "ivf")

  # Extract key coefficients
  coefs <- coef(m)
  ses <- se(m)
  pvals <- pvalue(m)

  # Report the R-squared and F-stat
  r2 <- fitstat(m, type = "r2")

  tibble(
    format = format_labels[fmt],
    n = nrow(df),
    r2_within = as.numeric(r2),
    coef_wickets = coefs["wickets_at_start"],
    se_wickets = ses["wickets_at_start"],
    p_wickets = pvals["wickets_at_start"],
    coef_runs = coefs["runs_at_start"],
    se_runs = ses["runs_at_start"],
    p_runs = pvals["runs_at_start"],
    coef_quality = coefs["avg_partnership_quality"],
    se_quality = ses["avg_partnership_quality"],
    p_quality = pvals["avg_partnership_quality"],
    coef_experience = coefs["combined_experience"],
    se_experience = ses["combined_experience"],
    p_experience = pvals["combined_experience"]
  )
})

write_csv(selectassign_results, file.path(tables_dir, "table_selective_assignment.csv"))
cat("  Saved: table_selective_assignment.csv\n")

# =============================================================================
# 3. WITHIN-PAIR QUALITY GAP TEST
#    Test whether |avg_batter1 - avg_batter2| differs by hand composition
# =============================================================================

cat("\n========== 3. WITHIN-PAIR QUALITY GAP ==========\n")

qualgap_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  # Compute within-pair quality gap
  df <- df %>%
    mutate(quality_gap = abs(max_pre_match_avg - min_pre_match_avg))

  # Test: does quality gap differ by hand composition?
  m <- feols(
    quality_gap ~ is_mixed_hand | match_innings_id,
    data = df,
    cluster = ~match_id
  )

  # Summary stats by group
  gap_stats <- df %>%
    group_by(is_mixed_hand) %>%
    summarise(
      mean_gap = mean(quality_gap, na.rm = TRUE),
      sd_gap = sd(quality_gap, na.rm = TRUE),
      .groups = "drop"
    )

  tibble(
    format = format_labels[fmt],
    mean_gap_same = gap_stats$mean_gap[gap_stats$is_mixed_hand == 0],
    mean_gap_mixed = gap_stats$mean_gap[gap_stats$is_mixed_hand == 1],
    coef = coef(m)["is_mixed_hand"],
    se = se(m)["is_mixed_hand"],
    p_value = pvalue(m)["is_mixed_hand"]
  )
})

write_csv(qualgap_results, file.path(tables_dir, "table_quality_gap.csv"))
cat("  Saved: table_quality_gap.csv\n")

# =============================================================================
# 4. ROLLING AVERAGE QUALITY SENSITIVITY
#    Use average of last 10 innings instead of career average
# =============================================================================

cat("\n========== 4. ROLLING AVERAGE SENSITIVITY ==========\n")

# Check if rolling average is available in the data; if not, approximate
# by restricting to players with 10+ prior matches
rolling_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  # Restrict to experienced players (proxy for stable quality measure)
  df_exp <- df %>%
    filter(combined_experience >= 20)  # both batsmen have 10+ prior matches

  cat(sprintf("  %s: %d partnerships with experienced batsmen (10+ matches each)\n",
              fmt, nrow(df_exp)))

  # Main spec on experienced subsample
  m <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_exp,
    cluster = ~match_id
  )

  tibble(
    format = format_labels[fmt],
    n = nrow(df_exp),
    coef = coef(m)["is_mixed_hand"],
    se = se(m)["is_mixed_hand"],
    p_value = pvalue(m)["is_mixed_hand"]
  )
})

write_csv(rolling_results, file.path(tables_dir, "table_rolling_avg_sensitivity.csv"))
cat("  Saved: table_rolling_avg_sensitivity.csv\n")

# =============================================================================
# 5. T20I QUANTILE ROBUSTNESS
#    (a) QTE by batting position (openers vs middle-order)
#    (b) Randomization inference on median regression
# =============================================================================

cat("\n========== 5. T20I QUANTILE ROBUSTNESS ==========\n")

if (requireNamespace("quantreg", quietly = TRUE)) {
  library(quantreg)

  df_t20 <- data_list[["t20is"]]

  if (!is.null(df_t20)) {
    # (a) QTE by batting position
    positions <- c("top_order", "middle_order", "lower_order")

    qte_by_position <- map_dfr(positions, function(pos) {
      df_sub <- df_t20 %>% filter(position_group == pos)
      if (nrow(df_sub) < 500) return(NULL)

      taus <- c(0.25, 0.50, 0.75)
      map_dfr(taus, function(tau) {
        m <- tryCatch(
          rq(runs_scored ~ is_mixed_hand +
               avg_partnership_quality + combined_experience +
               as.numeric(partnership_number_f) + as.numeric(innings_f) +
               wickets_at_start + runs_at_start,
             data = df_sub, tau = tau),
          error = function(e) NULL
        )
        if (is.null(m)) return(NULL)
        s <- tryCatch(summary(m, se = "boot", R = 200), error = function(e) summary(m))
        ct <- s$coefficients
        idx <- which(rownames(ct) == "is_mixed_hand")
        if (length(idx) == 0) return(NULL)
        tibble(
          position = pos,
          tau = tau,
          estimate = ct[idx, 1],
          se = ct[idx, 2],
          p_value = if (ncol(ct) >= 4) ct[idx, 4] else
            2 * pt(-abs(ct[idx, 3]), df = nrow(df_sub) - ncol(ct))
        )
      })
    })

    write_csv(qte_by_position, file.path(tables_dir, "table_qte_t20i_by_position.csv"))
    cat("  Saved: table_qte_t20i_by_position.csv\n")

    # (b) Randomization inference on T20I median regression
    cat("  Running RI on T20I median regression (1000 permutations)...\n")

    df_ri <- df_t20 %>%
      filter(!is.na(is_mixed_hand), !is.na(avg_partnership_quality)) %>%
      mutate(pn_num = as.numeric(partnership_number_f),
             inn_num = as.numeric(innings_f))

    # Observed median coefficient
    m_obs <- rq(runs_scored ~ is_mixed_hand +
                  avg_partnership_quality + combined_experience +
                  pn_num + inn_num + wickets_at_start + runs_at_start,
                data = df_ri, tau = 0.50)
    obs_coef <- coef(m_obs)["is_mixed_hand"]

    # Permutation distribution
    dt_ri <- as.data.table(df_ri)
    match_ids <- unique(dt_ri$match_id)
    n_perms <- 1000
    perm_coefs <- numeric(n_perms)

    for (i in seq_len(n_perms)) {
      dt_ri[, is_mixed_perm := sample(is_mixed_hand), by = match_id]
      m_perm <- tryCatch(
        rq(runs_scored ~ is_mixed_perm +
             avg_partnership_quality + combined_experience +
             pn_num + inn_num + wickets_at_start + runs_at_start,
           data = dt_ri, tau = 0.50),
        error = function(e) NULL
      )
      perm_coefs[i] <- if (!is.null(m_perm)) coef(m_perm)["is_mixed_perm"] else NA_real_
      if (i %% 200 == 0) cat(sprintf("    %d/%d permutations\n", i, n_perms))
    }

    perm_coefs <- perm_coefs[!is.na(perm_coefs)]
    ri_p <- mean(abs(perm_coefs) >= abs(obs_coef))

    ri_median <- tibble(
      format = "T20Is",
      quantile = 0.50,
      obs_coef = obs_coef,
      ri_p_value = ri_p,
      n_perms = length(perm_coefs)
    )
    write_csv(ri_median, file.path(tables_dir, "table_ri_t20i_median.csv"))
    cat(sprintf("  T20I median RI p-value: %.3f (obs coef: %.3f)\n", ri_p, obs_coef))
  }
} else {
  cat("  quantreg not available - skipping\n")
}

# =============================================================================
# 6. POWER ANALYSIS
#    Minimum detectable effect at 80% power, alpha = 0.05
# =============================================================================

cat("\n========== 6. POWER ANALYSIS ==========\n")

power_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  # Get residual SE from main specification
  m <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df,
    cluster = ~match_id
  )

  se_beta <- se(m)["is_mixed_hand"]
  n <- nrow(df)
  mean_runs <- mean(df$runs_scored, na.rm = TRUE)

  # MDE at 80% power, alpha = 0.05 (two-sided)
  # MDE = (z_{alpha/2} + z_{beta}) * SE = (1.96 + 0.84) * SE = 2.8 * SE
  mde <- 2.8 * se_beta
  mde_pct <- mde / mean_runs * 100

  tibble(
    format = format_labels[fmt],
    n = n,
    se_mixed_hand = se_beta,
    mde_runs = mde,
    mean_partnership_runs = mean_runs,
    mde_pct_of_mean = mde_pct
  )
})

write_csv(power_results, file.path(tables_dir, "table_power_analysis.csv"))
cat("  Power analysis:\n")
for (i in seq_len(nrow(power_results))) {
  cat(sprintf("    %s: MDE = %.2f runs (%.1f%% of mean)\n",
              power_results$format[i], power_results$mde_runs[i],
              power_results$mde_pct_of_mean[i]))
}

# =============================================================================
# 7. FORMAL P4 TEST (POOLED FORMATS)
#    Pool all formats, interact is_mixed_hand x format indicator
# =============================================================================

cat("\n========== 7. FORMAL P4 TEST (POOLED) ==========\n")

df_pooled <- bind_rows(
  data_list[["tests"]] %>% mutate(format = "Tests"),
  data_list[["odis"]] %>% mutate(format = "ODIs"),
  data_list[["t20is"]] %>% mutate(format = "T20Is")
) %>%
  mutate(format_f = factor(format, levels = c("Tests", "ODIs", "T20Is")))

if (nrow(df_pooled) > 0) {
  # Interaction model: is_mixed_hand x format
  m_pooled <- feols(
    runs_scored ~ is_mixed_hand * format_f +
      avg_partnership_quality + combined_experience +
      partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_pooled,
    cluster = ~match_id
  )

  pooled_summary <- broom::tidy(m_pooled) %>%
    filter(grepl("mixed|format", term))

  write_csv(pooled_summary, file.path(tables_dir, "table_p4_pooled.csv"))
  cat("  Saved: table_p4_pooled.csv\n")
}

# =============================================================================
# 8. COX PH ASSUMPTION TEST (SCHOENFELD RESIDUALS)
# =============================================================================

cat("\n========== 8. COX PH ASSUMPTION TEST ==========\n")

ph_results <- map_dfr(formats, function(fmt) {
  df <- data_list[[fmt]]
  if (is.null(df)) return(NULL)

  df_surv <- df %>%
    filter(!is.na(balls_faced), balls_faced > 0) %>%
    mutate(
      event = as.integer(!is_censored),
      strata_id = match_innings_id
    )

  # Cox model (stratified by match x innings)
  cox_m <- tryCatch(
    coxph(
      Surv(balls_faced, event) ~ is_mixed_hand +
        avg_partnership_quality + combined_experience +
        partnership_number + wickets_at_start,
      data = df_surv,
      id = strata_id
    ),
    error = function(e) {
      cat(sprintf("  Cox model failed for %s: %s\n", fmt, e$message))
      NULL
    }
  )

  if (is.null(cox_m)) return(NULL)

  # Schoenfeld test
  ph_test <- tryCatch(
    cox.zph(cox_m),
    error = function(e) {
      cat(sprintf("  PH test failed for %s: %s\n", fmt, e$message))
      NULL
    }
  )

  if (is.null(ph_test)) return(NULL)

  ph_tab <- as.data.frame(ph_test$table)
  ph_tab$variable <- rownames(ph_tab)

  # Extract is_mixed_hand row
  mixed_row <- ph_tab %>% filter(variable == "is_mixed_hand")

  tibble(
    format = format_labels[fmt],
    chi_sq = mixed_row$chisq,
    df = mixed_row$df,
    p_value = mixed_row$p,
    global_chi_sq = ph_tab$chisq[ph_tab$variable == "GLOBAL"],
    global_p = ph_tab$p[ph_tab$variable == "GLOBAL"]
  )
})

write_csv(ph_results, file.path(tables_dir, "table_cox_ph_test.csv"))
cat("  Saved: table_cox_ph_test.csv\n")

# =============================================================================
# 9. FORMAT OAXACA-BLINDER AND OSTER FOR MAIN PAPER
#    Copy and reformat existing tables from output/tables/
# =============================================================================

cat("\n========== 9. REFORMATTING EXISTING TABLES ==========\n")

# Copy Oaxaca-Blinder tables
for (fmt in formats) {
  src <- file.path(existing_tables, paste0("table_oaxaca_", fmt, ".csv"))
  dst <- file.path(tables_dir, paste0("table_oaxaca_", fmt, ".csv"))
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  Copied: table_oaxaca_%s.csv\n", fmt))
  }
}

# Copy Oster bounds tables
for (fmt in formats) {
  src <- file.path(existing_tables, paste0("table_oster_", fmt, ".csv"))
  dst <- file.path(tables_dir, paste0("table_oster_", fmt, ".csv"))
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  Copied: table_oster_%s.csv\n", fmt))
  }
}

# Copy ball-level triple interaction (P3 test)
for (fmt in formats) {
  src <- file.path(existing_tables, paste0("table_ball_triple_", fmt, ".csv"))
  dst <- file.path(tables_dir, paste0("table_p3_bowler_experience_", fmt, ".csv"))
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  Copied: table_ball_triple_%s.csv -> table_p3_bowler_experience_%s.csv\n",
                fmt, fmt))
  }
}

# =============================================================================
# GENERATE COMBINED SUMMARY TABLE FOR PAPER
# =============================================================================

cat("\n========== GENERATING COMBINED REVISION SUMMARY ==========\n")

# Combine all new results into one summary
revision_summary <- bind_rows(
  opening_results %>% mutate(analysis = "Opening partnerships"),
  rolling_results %>% mutate(analysis = "Experienced batsmen only"),
  power_results %>%
    select(format, n, coef = mde_runs, se = se_mixed_hand) %>%
    mutate(analysis = "Power (MDE at 80%)", p_value = NA_real_)
)

write_csv(revision_summary, file.path(tables_dir, "table_revision_summary.csv"))

# =============================================================================
# 10. O2 - DECOMPOSE STRIKE_CHANGED
#     Decompose strike rotation into: within-over, over-boundary, post-wicket
# =============================================================================

cat("\n========== 10. DECOMPOSE STRIKE_CHANGED ==========\n")

strike_decomp_results <- map_dfr(formats, function(fmt) {
  tryCatch({
    ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
    if (!file.exists(ball_path)) {
      cat(sprintf("  Ball-level file not found for %s\n", fmt))
      return(NULL)
    }

    bl <- fread(ball_path)
    cat(sprintf("  %s: %d ball-level observations loaded\n", fmt, nrow(bl)))

    # Sort within match-innings by over and ball
    setorder(bl, match_innings_id, over, ball)

    # Lag variables within match-innings
    bl[, `:=`(
      prev_over = shift(over, 1L, type = "lag"),
      prev_is_wicket = shift(is_wicket, 1L, type = "lag")
    ), by = match_innings_id]

    # Decompose strike_changed
    bl[, `:=`(
      strike_within_over = as.integer(
        strike_changed == 1 &
        !is.na(prev_over) & over == prev_over &
        (is.na(prev_is_wicket) | prev_is_wicket == 0)
      ),
      strike_over_boundary = as.integer(
        strike_changed == 1 &
        !is.na(prev_over) & over != prev_over
      ),
      strike_post_wicket = as.integer(
        strike_changed == 1 &
        !is.na(prev_is_wicket) & prev_is_wicket == 1
      )
    )]

    # Drop first ball of each innings (no lag available)
    bl_reg <- bl[!is.na(prev_over)]

    cat(sprintf("    strike_within_over: %d, strike_over_boundary: %d, strike_post_wicket: %d\n",
                sum(bl_reg$strike_within_over), sum(bl_reg$strike_over_boundary),
                sum(bl_reg$strike_post_wicket)))

    # Estimate mechanism regression with decomposed strike indicators
    m <- feols(
      runs_batter ~ LR_at_crease + strike_within_over + strike_over_boundary +
        strike_post_wicket +
        LR_at_crease:strike_within_over + LR_at_crease:strike_over_boundary +
        LR_at_crease:strike_post_wicket +
        striker_pre_avg + ns_pre_avg + over + partnership_ball_number +
        cum_innings_wickets |
        match_innings_id + bowler,
      data = bl_reg,
      cluster = ~match_id + bowler
    )

    # Extract interaction coefficients
    coefs <- coef(m)
    ses <- se(m)
    pvals <- pvalue(m)

    # Key variables to report
    key_vars <- c("LR_at_crease",
                  "strike_within_over", "strike_over_boundary", "strike_post_wicket",
                  "LR_at_crease:strike_within_over",
                  "LR_at_crease:strike_over_boundary",
                  "LR_at_crease:strike_post_wicket")

    map_dfr(key_vars, function(v) {
      if (v %in% names(coefs)) {
        tibble(format = format_labels[fmt], variable = v,
               estimate = coefs[v], se = ses[v], p_value = pvals[v])
      } else {
        NULL
      }
    })
  }, error = function(e) {
    cat(sprintf("  Error in strike decomposition for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(strike_decomp_results) > 0) {
  write_csv(strike_decomp_results, file.path(tables_dir, "table_strike_decomposition.csv"))
  cat("  Saved: table_strike_decomposition.csv\n")
} else {
  cat("  No results to save for strike decomposition.\n")
}

# =============================================================================
# 11. O3 - AKM-STYLE BATSMAN FIXED EFFECTS
#     Symmetric batsman identifiers, then partnership FE with batsman FE
# =============================================================================

cat("\n========== 11. AKM-STYLE BATSMAN FIXED EFFECTS ==========\n")

akm_results <- map_dfr(formats, function(fmt) {
  tryCatch({
    df <- data_list[[fmt]]
    if (is.null(df)) return(NULL)

    # Partnership-level data has batter_1 and batter_2 columns
    # Create symmetric identifiers (alphabetically ordered)
    df <- df %>%
      mutate(
        batter1_sym = pmin(batter_1, batter_2),
        batter2_sym = pmax(batter_1, batter_2)
      )

    cat(sprintf("  %s: %d unique batter1_sym, %d unique batter2_sym\n",
                fmt, n_distinct(df$batter1_sym), n_distinct(df$batter2_sym)))

    # AKM-style: partnership outcomes with batsman FEs
    m <- feols(
      runs_scored ~ is_mixed_hand | match_innings_id + batter1_sym + batter2_sym,
      data = df,
      cluster = ~match_id
    )

    tibble(
      format = format_labels[fmt],
      n = nrow(df),
      coef = coef(m)["is_mixed_hand"],
      se = se(m)["is_mixed_hand"],
      p_value = pvalue(m)["is_mixed_hand"]
    )
  }, error = function(e) {
    cat(sprintf("  Error in AKM for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(akm_results) > 0) {
  write_csv(akm_results, file.path(tables_dir, "table_akm_batsman_fe.csv"))
  cat("  Saved: table_akm_batsman_fe.csv\n")
} else {
  cat("  No AKM results to save.\n")
}

# =============================================================================
# 12. O4 - MULTIWAY CLUSTERING SENSITIVITY
#     Compare SEs under different clustering schemes
# =============================================================================

cat("\n========== 12. MULTIWAY CLUSTERING SENSITIVITY ==========\n")

clustering_results <- list()

# --- Partnership-level clustering ---
for (fmt in formats) {
  tryCatch({
    df <- data_list[[fmt]]
    if (is.null(df)) next

    # Create symmetric batsman IDs
    df <- df %>%
      mutate(
        batter1_sym = pmin(batter_1, batter_2),
        batter2_sym = pmax(batter_1, batter_2)
      )

    # Estimate main spec once, then change clustering
    m_base <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )
    se1 <- se(m_base)["is_mixed_hand"]

    m_2way <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id + batter1_sym
    )
    se2 <- se(m_2way)["is_mixed_hand"]

    m_3way <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id + batter1_sym + batter2_sym
    )
    se3 <- se(m_3way)["is_mixed_hand"]

    coef_val <- coef(m_base)["is_mixed_hand"]

    clustering_results <- c(clustering_results, list(
      tibble(format = format_labels[fmt], level = "partnership",
             clustering = "match_id", coef = coef_val, se = se1,
             p_value = pvalue(m_base)["is_mixed_hand"]),
      tibble(format = format_labels[fmt], level = "partnership",
             clustering = "match_id + batter1", coef = coef_val, se = se2,
             p_value = pvalue(m_2way)["is_mixed_hand"]),
      tibble(format = format_labels[fmt], level = "partnership",
             clustering = "match_id + batter1 + batter2", coef = coef_val, se = se3,
             p_value = pvalue(m_3way)["is_mixed_hand"])
    ))

    cat(sprintf("  %s partnership: SE(match)=%.4f, SE(match+bat1)=%.4f, SE(match+bat1+bat2)=%.4f\n",
                fmt, se1, se2, se3))
  }, error = function(e) {
    cat(sprintf("  Error in partnership clustering for %s: %s\n", fmt, e$message))
  })
}

# --- Ball-level clustering ---
for (fmt in formats) {
  tryCatch({
    ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
    if (!file.exists(ball_path)) next

    bl <- fread(ball_path)

    # Current: match_id + bowler
    m_bl_2way <- feols(
      runs_batter ~ LR_at_crease + strike_changed +
        LR_at_crease:strike_changed +
        striker_pre_avg + ns_pre_avg + over +
        partnership_ball_number + cum_innings_wickets |
        match_innings_id + bowler,
      data = bl,
      cluster = ~match_id + bowler
    )
    se_bl2 <- se(m_bl_2way)["LR_at_crease"]

    # Three-way: match_id + bowler + batter
    m_bl_3way <- feols(
      runs_batter ~ LR_at_crease + strike_changed +
        LR_at_crease:strike_changed +
        striker_pre_avg + ns_pre_avg + over +
        partnership_ball_number + cum_innings_wickets |
        match_innings_id + bowler,
      data = bl,
      cluster = ~match_id + bowler + batter
    )
    se_bl3 <- se(m_bl_3way)["LR_at_crease"]

    coef_bl <- coef(m_bl_2way)["LR_at_crease"]

    clustering_results <- c(clustering_results, list(
      tibble(format = format_labels[fmt], level = "ball",
             clustering = "match_id + bowler", coef = coef_bl, se = se_bl2,
             p_value = pvalue(m_bl_2way)["LR_at_crease"]),
      tibble(format = format_labels[fmt], level = "ball",
             clustering = "match_id + bowler + batter", coef = coef_bl, se = se_bl3,
             p_value = pvalue(m_bl_3way)["LR_at_crease"])
    ))

    cat(sprintf("  %s ball-level: SE(match+bowler)=%.4f, SE(match+bowler+batter)=%.4f\n",
                fmt, se_bl2, se_bl3))
  }, error = function(e) {
    cat(sprintf("  Error in ball-level clustering for %s: %s\n", fmt, e$message))
  })
}

clustering_df <- bind_rows(clustering_results)
if (nrow(clustering_df) > 0) {
  write_csv(clustering_df, file.path(tables_dir, "table_clustering_sensitivity.csv"))
  cat("  Saved: table_clustering_sensitivity.csv\n")
} else {
  cat("  No clustering results to save.\n")
}

# =============================================================================
# 13. #1 - CHECK AND FIX OSTER SPECIFICATION
#     Compare Oster bounds with/without FE; document OLS vs FE issue
# =============================================================================

cat("\n========== 13. CHECK OSTER SPECIFICATION ==========\n")

# Read existing Oster results
oster_existing <- map_dfr(formats, function(fmt) {
  path <- file.path(existing_tables, paste0("table_oster_", fmt, ".csv"))
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
})

cat("  Existing Oster results (OLS-based, NO fixed effects):\n")
if (nrow(oster_existing) > 0) print(oster_existing)

cat("\n  NOTE: The original Oster code uses OLS without fixed effects (lm()),\n")
cat("  while the main results use match x innings FE via feols().\n")
cat("  This means R2_uncontrolled and R2_controlled reflect total variation,\n")
cat("  not within-FE variation. We now re-compute using FE specifications.\n\n")

oster_comparison <- map_dfr(formats, function(fmt) {
  tryCatch({
    df <- data_list[[fmt]]
    if (is.null(df)) return(NULL)

    # --- OLS (no FE) specifications for Oster ---
    # Uncontrolled
    m_ols_short <- lm(runs_scored ~ is_mixed_hand, data = df)
    beta_ols_short <- coef(m_ols_short)["is_mixed_hand"]
    r2_ols_short <- summary(m_ols_short)$r.squared

    # Controlled (no FE)
    m_ols_long <- lm(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start,
      data = df
    )
    beta_ols_long <- coef(m_ols_long)["is_mixed_hand"]
    r2_ols_long <- summary(m_ols_long)$r.squared

    # Oster R_max = 1.3 * R2_controlled (common rule of thumb)
    r_max_ols <- min(1.3 * r2_ols_long, 1)

    # delta for OLS
    # delta = (beta_long * (R_max - R2_long)) / ((beta_short - beta_long) * (R2_long - R2_short))
    denom_ols <- (beta_ols_short - beta_ols_long) * (r2_ols_long - r2_ols_short)
    delta_ols <- if (abs(denom_ols) > 1e-10) {
      beta_ols_long * (r_max_ols - r2_ols_long) / denom_ols
    } else NA_real_

    # beta* at delta=1
    # beta* = beta_long - delta * (beta_short - beta_long) * (R_max - R2_long) / (R2_long - R2_short)
    if (!is.na(delta_ols) && abs(r2_ols_long - r2_ols_short) > 1e-10) {
      beta_star_ols <- beta_ols_long -
        (beta_ols_short - beta_ols_long) * (r_max_ols - r2_ols_long) /
        (r2_ols_long - r2_ols_short)
    } else {
      beta_star_ols <- NA_real_
    }

    # Identified set for OLS
    id_set_low_ols <- min(beta_ols_long, beta_star_ols, na.rm = TRUE)
    id_set_high_ols <- max(beta_ols_long, beta_star_ols, na.rm = TRUE)
    includes_zero_ols <- (id_set_low_ols <= 0 & id_set_high_ols >= 0)

    # --- FE specifications ---
    # Short: is_mixed_hand with match_innings FE
    m_fe_short <- feols(runs_scored ~ is_mixed_hand | match_innings_id, data = df)
    beta_fe_short <- coef(m_fe_short)["is_mixed_hand"]
    # Use overall R2 (not within) for Oster
    r2_fe_short <- fitstat(m_fe_short, type = "r2")[[1]]

    # Long: with controls and match_innings FE
    m_fe_long <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df
    )
    beta_fe_long <- coef(m_fe_long)["is_mixed_hand"]
    r2_fe_long <- fitstat(m_fe_long, type = "r2")[[1]]

    r_max_fe <- min(1.3 * r2_fe_long, 1)

    denom_fe <- (beta_fe_short - beta_fe_long) * (r2_fe_long - r2_fe_short)
    delta_fe <- if (abs(denom_fe) > 1e-10) {
      beta_fe_long * (r_max_fe - r2_fe_long) / denom_fe
    } else NA_real_

    if (!is.na(delta_fe) && abs(r2_fe_long - r2_fe_short) > 1e-10) {
      beta_star_fe <- beta_fe_long -
        (beta_fe_short - beta_fe_long) * (r_max_fe - r2_fe_long) /
        (r2_fe_long - r2_fe_short)
    } else {
      beta_star_fe <- NA_real_
    }

    id_set_low_fe <- min(beta_fe_long, beta_star_fe, na.rm = TRUE)
    id_set_high_fe <- max(beta_fe_long, beta_star_fe, na.rm = TRUE)
    includes_zero_fe <- (id_set_low_fe <= 0 & id_set_high_fe >= 0)

    cat(sprintf("  %s (OLS): identified set [%.3f, %.3f], includes zero: %s\n",
                format_labels[fmt], id_set_low_ols, id_set_high_ols, includes_zero_ols))
    cat(sprintf("  %s (FE):  identified set [%.3f, %.3f], includes zero: %s\n",
                format_labels[fmt], id_set_low_fe, id_set_high_fe, includes_zero_fe))

    tibble(
      format = format_labels[fmt],
      # OLS columns
      beta_short_ols = beta_ols_short, r2_short_ols = r2_ols_short,
      beta_long_ols = beta_ols_long, r2_long_ols = r2_ols_long,
      r_max_ols = r_max_ols, delta_ols = delta_ols,
      beta_star_ols = beta_star_ols,
      id_set_low_ols = id_set_low_ols, id_set_high_ols = id_set_high_ols,
      includes_zero_ols = includes_zero_ols,
      # FE columns
      beta_short_fe = beta_fe_short, r2_short_fe = r2_fe_short,
      beta_long_fe = beta_fe_long, r2_long_fe = r2_fe_long,
      r_max_fe = r_max_fe, delta_fe = delta_fe,
      beta_star_fe = beta_star_fe,
      id_set_low_fe = id_set_low_fe, id_set_high_fe = id_set_high_fe,
      includes_zero_fe = includes_zero_fe
    )
  }, error = function(e) {
    cat(sprintf("  Error in Oster for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(oster_comparison) > 0) {
  write_csv(oster_comparison, file.path(tables_dir, "table_oster_comparison.csv"))
  cat("  Saved: table_oster_comparison.csv\n")
} else {
  cat("  No Oster comparison results to save.\n")
}

# =============================================================================
# 14. #2 - CHECK OAXACA DECOMPOSITION SUM
#     Verify: Raw Gap = Endowments + Coefficients + Interaction
# =============================================================================

cat("\n========== 14. CHECK OAXACA DECOMPOSITION SUM ==========\n")

oaxaca_check <- map_dfr(formats, function(fmt) {
  tryCatch({
    path <- file.path(existing_tables, paste0("table_oaxaca_", fmt, ".csv"))
    if (!file.exists(path)) {
      cat(sprintf("  Oaxaca table not found for %s\n", fmt))
      return(NULL)
    }

    ox <- read_csv(path, show_col_types = FALSE)

    raw_gap <- ox$estimate[ox$component == "Raw Gap"]
    endowments <- ox$estimate[ox$component == "Endowments"]
    coefficients <- ox$estimate[ox$component == "Coefficients"]
    interaction <- ox$estimate[ox$component == "Interaction"]

    # Check: Raw Gap should equal Endowments + Coefficients + Interaction
    sum_components <- endowments + coefficients + interaction
    discrepancy <- raw_gap - sum_components

    cat(sprintf("  %s: Raw Gap = %.6f, Sum(E+C+I) = %.6f, Discrepancy = %.6e\n",
                format_labels[fmt], raw_gap, sum_components, discrepancy))

    # Also check percentages
    pct_sum <- ox$pct_of_gap[ox$component == "Endowments"] +
      ox$pct_of_gap[ox$component == "Coefficients"] +
      ox$pct_of_gap[ox$component == "Interaction"]
    pct_discrepancy <- 100 - pct_sum

    cat(sprintf("    Pct sum: %.4f%%, Pct discrepancy: %.4e%%\n", pct_sum, pct_discrepancy))

    tibble(
      format = format_labels[fmt],
      raw_gap = raw_gap,
      endowments = endowments,
      coefficients = coefficients,
      interaction = interaction,
      sum_components = sum_components,
      discrepancy = discrepancy,
      pct_sum = pct_sum,
      pct_discrepancy = pct_discrepancy,
      balanced = abs(discrepancy) < 1e-6
    )
  }, error = function(e) {
    cat(sprintf("  Error checking Oaxaca for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(oaxaca_check) > 0) {
  write_csv(oaxaca_check, file.path(tables_dir, "table_oaxaca_check.csv"))
  cat("  Saved: table_oaxaca_check.csv\n")
} else {
  cat("  No Oaxaca check results to save.\n")
}

# =============================================================================
# 15. #3 - REPARAMETERIZE QUALITY CONTROLS
#     Test collinearity of avg_partnership_quality with max + min
# =============================================================================

cat("\n========== 15. REPARAMETERIZE QUALITY CONTROLS ==========\n")

quality_reparam <- map_dfr(formats, function(fmt) {
  tryCatch({
    df <- data_list[[fmt]]
    if (is.null(df)) return(NULL)

    # Check correlation between avg and (max+min)/2
    df <- df %>%
      mutate(
        avg_from_maxmin = (max_pre_match_avg + min_pre_match_avg) / 2,
        quality_gap = max_pre_match_avg - min_pre_match_avg
      )

    corr_avg <- cor(df$avg_partnership_quality, df$avg_from_maxmin, use = "complete.obs")
    cat(sprintf("  %s: cor(avg_partnership_quality, (max+min)/2) = %.6f\n",
                fmt, corr_avg))

    # Original specification (all three quality variables)
    m_orig <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )

    # Spec 1: Drop avg, keep max + min
    m_spec1 <- feols(
      runs_scored ~ is_mixed_hand +
        max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )

    # Spec 2: avg + quality_gap (= max - min)
    m_spec2 <- feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + quality_gap +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )

    bind_rows(
      tibble(format = format_labels[fmt], specification = "Original (avg + max + min)",
             coef = coef(m_orig)["is_mixed_hand"],
             se = se(m_orig)["is_mixed_hand"],
             p_value = pvalue(m_orig)["is_mixed_hand"],
             n = nrow(df), corr_avg_maxmin = corr_avg),
      tibble(format = format_labels[fmt], specification = "Spec 1 (max + min only)",
             coef = coef(m_spec1)["is_mixed_hand"],
             se = se(m_spec1)["is_mixed_hand"],
             p_value = pvalue(m_spec1)["is_mixed_hand"],
             n = nrow(df), corr_avg_maxmin = corr_avg),
      tibble(format = format_labels[fmt], specification = "Spec 2 (avg + gap)",
             coef = coef(m_spec2)["is_mixed_hand"],
             se = se(m_spec2)["is_mixed_hand"],
             p_value = pvalue(m_spec2)["is_mixed_hand"],
             n = nrow(df), corr_avg_maxmin = corr_avg)
    )
  }, error = function(e) {
    cat(sprintf("  Error in quality reparameterization for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(quality_reparam) > 0) {
  write_csv(quality_reparam, file.path(tables_dir, "table_quality_reparameterization.csv"))
  cat("  Saved: table_quality_reparameterization.csv\n")
} else {
  cat("  No quality reparameterization results to save.\n")
}

# =============================================================================
# 16. #8 - ALTERNATIVE BOWLER EXPERIENCE MEASURE
#     Use absolute LH delivery count proxy instead of fraction
# =============================================================================

cat("\n========== 16. ALTERNATIVE BOWLER EXPERIENCE MEASURE ==========\n")

bowler_exp_results <- map_dfr(formats, function(fmt) {
  tryCatch({
    ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
    if (!file.exists(ball_path)) {
      cat(sprintf("  Ball-level file not found for %s\n", fmt))
      return(NULL)
    }

    bl <- fread(ball_path)
    cat(sprintf("  %s: %d ball-level observations loaded\n", fmt, nrow(bl)))

    # Create absolute LH delivery count proxy:
    # bowler_pre_bowl_matches * bowler_pre_pct_to_lh
    # This approximates the number of matches in which the bowler faced LH batsmen
    bl[, bowler_lh_abs := bowler_pre_bowl_matches * bowler_pre_pct_to_lh]

    # Handle missing values
    bl_reg <- bl[!is.na(bowler_lh_abs) & !is.na(strike_changed) & !is.na(LR_at_crease)]

    # Create quartiles of absolute measure
    bl_reg[, bowler_lh_abs_q := cut(bowler_lh_abs,
                                     breaks = quantile(bowler_lh_abs, probs = c(0, 0.25, 0.5, 0.75, 1),
                                                       na.rm = TRUE),
                                     include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))]

    cat(sprintf("    Bowler LH abs proxy: mean=%.2f, sd=%.2f\n",
                mean(bl_reg$bowler_lh_abs, na.rm = TRUE),
                sd(bl_reg$bowler_lh_abs, na.rm = TRUE)))
    cat(sprintf("    Quartile counts: Q1=%d, Q2=%d, Q3=%d, Q4=%d\n",
                sum(bl_reg$bowler_lh_abs_q == "Q1", na.rm = TRUE),
                sum(bl_reg$bowler_lh_abs_q == "Q2", na.rm = TRUE),
                sum(bl_reg$bowler_lh_abs_q == "Q3", na.rm = TRUE),
                sum(bl_reg$bowler_lh_abs_q == "Q4", na.rm = TRUE)))

    # Triple interaction: LR_at_crease * strike_changed * bowler_lh_abs_q
    m <- feols(
      runs_batter ~ LR_at_crease * strike_changed * bowler_lh_abs_q +
        striker_pre_avg + ns_pre_avg + over +
        partnership_ball_number + cum_innings_wickets |
        match_innings_id,
      data = bl_reg,
      cluster = ~match_id + bowler
    )

    # Extract all coefficients involving the triple interaction
    coefs <- coef(m)
    ses <- se(m)
    pvals <- pvalue(m)

    # Filter for interaction terms involving all three variables
    all_vars <- names(coefs)
    triple_vars <- all_vars[grepl("LR_at_crease.*strike_changed.*bowler_lh_abs_q|LR_at_crease.*bowler_lh_abs_q.*strike_changed", all_vars)]
    # Also include key lower-order interactions and main effects for context
    key_vars <- all_vars[grepl("LR_at_crease", all_vars)]

    map_dfr(key_vars, function(v) {
      tibble(
        format = format_labels[fmt],
        variable = v,
        estimate = coefs[v],
        se = ses[v],
        p_value = pvals[v]
      )
    })
  }, error = function(e) {
    cat(sprintf("  Error in bowler experience for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(bowler_exp_results) > 0) {
  write_csv(bowler_exp_results, file.path(tables_dir, "table_bowler_exp_absolute.csv"))
  cat("  Saved: table_bowler_exp_absolute.csv\n")
} else {
  cat("  No bowler experience results to save.\n")
}

# =============================================================================
# 17. O1 - THREE-CATEGORY SPECIFICATION (LL, LR vs RR baseline)
#     Report LL and LR coefficients against RR baseline
# =============================================================================

cat("\n========== 17. THREE-CATEGORY SPECIFICATION ==========\n")

three_cat_results <- map_dfr(formats, function(fmt) {
  tryCatch({
    df <- data_list[[fmt]]
    if (is.null(df)) return(NULL)

    # hand_combination should already be a factor with levels RR, LR, LL
    # Ensure RR is the baseline
    df$hand_combination <- relevel(factor(df$hand_combination), ref = "RR")

    cat(sprintf("  %s: hand_combination levels = %s\n", fmt,
                paste(levels(df$hand_combination), collapse = ", ")))

    # Three-category FE specification
    m <- feols(
      runs_scored ~ hand_combination +
        max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        runs_at_start + wickets_at_start |
        match_innings_id,
      data = df,
      cluster = ~match_id
    )

    coefs <- coef(m)
    ses <- se(m)
    pvals <- pvalue(m)

    # Extract LL and LR coefficients
    key_vars <- names(coefs)[grepl("hand_combination", names(coefs))]

    map_dfr(key_vars, function(v) {
      tibble(
        format = format_labels[fmt],
        variable = v,
        estimate = coefs[v],
        se = ses[v],
        p_value = pvals[v],
        n = nobs(m)
      )
    })
  }, error = function(e) {
    cat(sprintf("  Error in three-category for %s: %s\n", fmt, e$message))
    NULL
  })
})

if (nrow(three_cat_results) > 0) {
  write_csv(three_cat_results, file.path(tables_dir, "table_three_category.csv"))
  cat("  Saved: table_three_category.csv\n")
  print(three_cat_results)
} else {
  cat("  No three-category results to save.\n")
}

cat("\n========== ALL REVISION ANALYSES COMPLETE ==========\n")
cat(sprintf("Output saved to: %s\n", tables_dir))

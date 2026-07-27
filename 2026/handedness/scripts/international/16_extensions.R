# =============================================================================
# 16_extensions.R
# Three extensions to strengthen the precisely estimated null:
#   Extension 1: Bowler type heterogeneity (pace vs spin subsample split)
#   Extension 2: Multiple outcomes dashboard (null across all margins)
#   Extension 3: Out-of-position batsmen (quasi-exogenous hand assignment)
#
# Run from RStudio (uses rstudioapi for paths).
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)
library(modelsummary)
library(kableExtra)

# Force modelsummary to use kableExtra (standard booktabs) instead of tabularray
options(modelsummary_factory_latex = "kableExtra")

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

formats       <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# --- Helper: load and filter partnership data (same as Script 15) ---
load_and_estimate <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings),
      log_runs = log1p(runs_scored)
    )
  df
}

# --- Helper: extract coefficient row from fixest model ---
extract_coef <- function(model, coef_name, label) {
  if (is.null(model)) return(NULL)
  est  <- coef(model)[coef_name]
  se_v <- se(model)[coef_name]
  pv   <- pvalue(model)[coef_name]
  if (is.na(est)) return(NULL)
  stars <- case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**", pv < 0.10 ~ "*", TRUE ~ "")
  tibble(
    label    = label,
    estimate = est,
    se       = se_v,
    p_value  = pv,
    coef_str = sprintf("%.3f%s", est, stars),
    se_str   = sprintf("(%.3f)", se_v),
    n        = model$nobs
  )
}

cat("\n####################################################################\n")
cat("# 16_extensions.R — Three extensions\n")
cat("####################################################################\n")


# #########################################################################
# === EXTENSION 1: BOWLER TYPE HETEROGENEITY ===
# #########################################################################

cat("\n\n========== EXTENSION 1: BOWLER TYPE HETEROGENEITY ==========\n")

# --- 1a. Read existing triple interaction CSVs and extract key rows ---
cat("\n--- 1a. Extracting triple interaction coefficients ---\n")

triple_summary <- list()
for (fmt in formats) {
  csv_path <- file.path(tables_dir, paste0("table_ball_triple_", fmt, ".csv"))
  if (!file.exists(csv_path)) {
    cat(sprintf("  Triple CSV not found for %s, skipping.\n", fmt))
    next
  }
  tri <- read_csv(csv_path, show_col_types = FALSE)

  # Key rows: the triple interaction (LR x strike_changed x bowler_type)
  key_terms <- tri %>%
    filter(
      part == "estimates",
      grepl("LR_at_crease.*strike_changed.*bowler_type|LR_at_crease \u00d7 strike_changed \u00d7 bowler_type", term, ignore.case = TRUE)
    )

  if (nrow(key_terms) > 0) {
    est_row <- key_terms %>% filter(statistic == "estimate")
    se_row  <- key_terms %>% filter(statistic == "std.error")
    triple_summary[[fmt]] <- tibble(
      format = format_labels[fmt],
      term   = est_row$term[1],
      coef   = est_row$`x Bowler Type`[1],
      se     = se_row$`x Bowler Type`[1]
    )
    cat(sprintf("  %s triple: %s  %s\n", format_labels[fmt],
                est_row$`x Bowler Type`[1], se_row$`x Bowler Type`[1]))
  }
}

if (length(triple_summary) > 0) {
  triple_df <- bind_rows(triple_summary)
  cat("\n  Triple interaction (LR x strike_changed x bowler_type) across formats:\n")
  print(triple_df)
}

# --- 1b. Compute pct_pace per partnership from ball-level data ---
cat("\n--- 1b. Computing pct_pace per partnership ---\n")

ext1_results <- list()

for (fmt in formats) {
  cat(sprintf("\n  Processing %s...\n", format_labels[fmt]))

  ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(ball_path)) {
    cat(sprintf("  Ball-level data not found for %s, skipping.\n", fmt))
    next
  }

  # Compute pct_pace per partnership
  dt <- fread(ball_path)
  pace_by_partnership <- dt[
    !is.na(bowler_type),
    .(total_deliveries = .N,
      pace_deliveries  = sum(bowler_type == "pace", na.rm = TRUE)),
    by = .(match_id, innings, partnership_id)
  ]
  pace_by_partnership[, pct_pace := pace_deliveries / total_deliveries]

  # Merge into partnership data
  df <- load_and_estimate(fmt)

  # Create a partnership_id to match: match_id + innings + partnership_number
  # The ball-level partnership_id format needs to match
  # Check what partnership_id looks like
  sample_pid <- dt$partnership_id[1]
  cat(sprintf("  Sample partnership_id from ball data: %s\n", sample_pid))

  # Merge using match_id + innings + partnership_number
  pace_agg <- dt[
    !is.na(bowler_type),
    .(total_deliveries = .N,
      pace_deliveries  = sum(bowler_type == "pace", na.rm = TRUE)),
    by = .(match_id, innings)
  ]

  # Actually merge at the partnership level: use match_id + innings + partnership ball range
  # Simpler: aggregate by match_id + innings + partnership_id, then join
  # partnership_id in ball data typically encodes match_id_innings_partnership_number
  # Let's build pct_pace per partnership by grouping on match_innings_id + partnership_id

  pace_partner <- dt[
    !is.na(bowler_type),
    .(pct_pace = mean(bowler_type == "pace", na.rm = TRUE),
      n_balls_bt = .N),
    by = .(match_id, innings, partnership_id)
  ]

  # partnership_id should match: create in df
  # From 03_build_partnerships.R, partnership_id is typically match_id_innings_partnership_number
  # Let's try matching on the components
  pace_partner[, partnership_number := as.integer(
    sub(".*_(\\d+)$", "\\1", partnership_id)
  )]

  df_merged <- df %>%
    left_join(
      as_tibble(pace_partner[, .(match_id, innings, partnership_number, pct_pace)]),
      by = c("match_id", "innings", "partnership_number")
    )

  n_matched <- sum(!is.na(df_merged$pct_pace))
  cat(sprintf("  Merged pct_pace: %d / %d (%.1f%%) partnerships matched\n",
              n_matched, nrow(df_merged), n_matched / nrow(df_merged) * 100))

  if (n_matched < 100) {
    cat("  Too few matches, skipping subsample regressions.\n")
    next
  }

  # Drop unmatched
  df_sub <- df_merged %>% filter(!is.na(pct_pace))

  # --- 1c. Subsample regressions: pace-dominant vs spin-dominant ---
  med_pace <- median(df_sub$pct_pace, na.rm = TRUE)
  cat(sprintf("  Median pct_pace: %.3f\n", med_pace))

  df_pace <- df_sub %>% filter(pct_pace >= med_pace)
  df_spin <- df_sub %>% filter(pct_pace < med_pace)

  cat(sprintf("  Pace-dominant: %d partnerships\n", nrow(df_pace)))
  cat(sprintf("  Spin-dominant: %d partnerships\n", nrow(df_spin)))

  m_pace <- tryCatch(
    feols(runs_scored ~ is_mixed_hand +
            avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
            combined_experience + partnership_number_f +
            wickets_at_start + runs_at_start | match_innings_id,
          data = df_pace, cluster = ~match_id),
    error = function(e) { cat(sprintf("  Pace regression failed: %s\n", e$message)); NULL }
  )

  m_spin <- tryCatch(
    feols(runs_scored ~ is_mixed_hand +
            avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
            combined_experience + partnership_number_f +
            wickets_at_start + runs_at_start | match_innings_id,
          data = df_spin, cluster = ~match_id),
    error = function(e) { cat(sprintf("  Spin regression failed: %s\n", e$message)); NULL }
  )

  ext1_results[[fmt]] <- list(
    pace = extract_coef(m_pace, "is_mixed_hand",
                        paste0(format_labels[fmt], ": Pace-dominant")),
    spin = extract_coef(m_spin, "is_mixed_hand",
                        paste0(format_labels[fmt], ": Spin-dominant"))
  )

  if (!is.null(m_pace)) {
    cat(sprintf("  Pace-dominant:  coef = %.3f (SE = %.3f, p = %.3f)\n",
                coef(m_pace)["is_mixed_hand"],
                se(m_pace)["is_mixed_hand"],
                pvalue(m_pace)["is_mixed_hand"]))
  }
  if (!is.null(m_spin)) {
    cat(sprintf("  Spin-dominant:  coef = %.3f (SE = %.3f, p = %.3f)\n",
                coef(m_spin)["is_mixed_hand"],
                se(m_spin)["is_mixed_hand"],
                pvalue(m_spin)["is_mixed_hand"]))
  }
}

# --- 1d. Build output table ---
cat("\n--- 1d. Building Extension 1 output table ---\n")

ext1_rows <- bind_rows(
  map(ext1_results, ~ bind_rows(.x$pace, .x$spin))
)

if (nrow(ext1_rows) > 0) {
  ext1_tab <- ext1_rows %>%
    select(label, coef_str, se_str, n) %>%
    mutate(n = format(n, big.mark = ","))

  kbl_ext1 <- kbl(ext1_tab,
      format = "latex", booktabs = TRUE,
      col.names = c("Subsample", "Mixed hand (LR)", "SE", "N"),
      align = "lrrr",
      caption = "Bowler type heterogeneity: pace-dominant vs.\\ spin-dominant partnerships",
      label = "ext1_bowler") %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = "Partnerships split at median share of pace deliveries. DV: runs scored. All specifications include match $\\times$ innings FE, quality controls, and match-situation controls. Standard errors clustered at match level.",
      general_title = "Notes:", threeparttable = TRUE
    )

  writeLines(kbl_ext1, file.path(tables_dir, "table_ext1_bowler_type.tex"))
  write_csv(ext1_rows, file.path(tables_dir, "table_ext1_bowler_type.csv"))
  cat("  Extension 1 tables saved.\n")
} else {
  cat("  No Extension 1 results to save.\n")
}


# #########################################################################
# === EXTENSION 2: MULTIPLE OUTCOMES DASHBOARD ===
# #########################################################################

cat("\n\n========== EXTENSION 2: MULTIPLE OUTCOMES DASHBOARD ==========\n")

# --- 2a. Partnership-level: runs, balls, log_runs x 3 formats ---
cat("\n--- 2a. Partnership-level regressions ---\n")

panel_a_rows <- list()

for (fmt in formats) {
  cat(sprintf("  %s...\n", format_labels[fmt]))
  df <- load_and_estimate(fmt)

  # Runs scored (primary spec)
  m_runs <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start | match_innings_id,
    data = df, cluster = ~match_id
  )

  # Balls faced
  m_balls <- feols(
    balls_faced ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start | match_innings_id,
    data = df, cluster = ~match_id
  )

  # log(runs + 1)
  m_log <- feols(
    log_runs ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start | match_innings_id,
    data = df, cluster = ~match_id
  )

  panel_a_rows[[fmt]] <- bind_rows(
    extract_coef(m_runs,  "is_mixed_hand", "Runs scored") %>% mutate(format = fmt),
    extract_coef(m_balls, "is_mixed_hand", "Balls faced") %>% mutate(format = fmt),
    extract_coef(m_log,   "is_mixed_hand", "log(runs + 1)") %>% mutate(format = fmt)
  )
}

panel_a <- bind_rows(panel_a_rows)
cat("  Partnership-level done.\n")

# --- 2b. Ball-level: runs_batter and is_wicket x 3 formats ---
cat("\n--- 2b. Ball-level regressions ---\n")

panel_b_rows <- list()

for (fmt in formats) {
  cat(sprintf("  %s...\n", format_labels[fmt]))

  ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(ball_path)) {
    cat(sprintf("  Ball-level data not found for %s, skipping.\n", fmt))
    next
  }

  dt <- fread(ball_path)
  dt <- dt[!is.na(LR_at_crease) & !is.na(striker_pre_avg) & !is.na(ns_pre_avg)]

  # Runs per delivery (with bowler FE, two-way clustering)
  m_ball_runs <- feols(
    runs_batter ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt, cluster = ~match_id + bowler
  )

  # Wicket hazard (legal deliveries only, with bowler FE)
  dt_legal <- dt[is_legal == 1L]
  m_ball_wicket <- feols(
    is_wicket ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt_legal, cluster = ~match_id + bowler
  )

  panel_b_rows[[fmt]] <- bind_rows(
    extract_coef(m_ball_runs, "LR_at_crease",
                 "LR at crease (runs)") %>% mutate(format = fmt),
    extract_coef(m_ball_wicket, "LR_at_crease",
                 "LR at crease (wicket)") %>% mutate(format = fmt),
    extract_coef(m_ball_runs, "LR_at_crease:strike_changed",
                 "LR x strike changed (runs)") %>% mutate(format = fmt),
    extract_coef(m_ball_wicket, "LR_at_crease:strike_changed",
                 "LR x strike changed (wicket)") %>% mutate(format = fmt)
  )
}

panel_b <- bind_rows(panel_b_rows)
cat("  Ball-level done.\n")

# --- 2c. Build compact dashboard table ---
cat("\n--- 2c. Building dashboard table ---\n")

# Reshape Panel A: rows = outcome, columns = format
pa_wide <- panel_a %>%
  select(label, format, coef_str, se_str) %>%
  pivot_wider(
    names_from = format,
    values_from = c(coef_str, se_str),
    names_glue = "{format}_{.value}"
  ) %>%
  # Interleave: Tests coef, Tests SE, ODIs coef, ODIs SE, T20Is coef, T20Is SE
  select(label,
         tests_coef_str, tests_se_str,
         odis_coef_str, odis_se_str,
         t20is_coef_str, t20is_se_str)

# Reshape Panel B
pb_wide <- panel_b %>%
  select(label, format, coef_str, se_str) %>%
  pivot_wider(
    names_from = format,
    values_from = c(coef_str, se_str),
    names_glue = "{format}_{.value}"
  ) %>%
  select(label,
         tests_coef_str, tests_se_str,
         odis_coef_str, odis_se_str,
         t20is_coef_str, t20is_se_str)

# Combine with panel headers
# For LaTeX, we'll build the table manually using kableExtra

dashboard <- bind_rows(pa_wide, pb_wide)

# Replace NAs with empty strings
dashboard[is.na(dashboard)] <- ""

# Get sample sizes for footer
n_partnership <- panel_a %>%
  filter(label == "Runs scored") %>%
  select(format, n) %>%
  deframe()

n_ball <- panel_b %>%
  filter(label == "LR at crease (runs)") %>%
  select(format, n) %>%
  deframe()

kbl_ext2 <- kbl(dashboard,
    format = "latex", booktabs = TRUE, escape = FALSE,
    col.names = c("Outcome / Coefficient", rep(c("Coef.", "SE"), 3)),
    align = "lrrrrrr",
    caption = "The null across outcome margins",
    label = "ext2_outcomes") %>%
  add_header_above(c(" " = 1, "Tests" = 2, "ODIs" = 2, "T20Is" = 2)) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  pack_rows("Panel A: Partnership level (match x innings FE)", 1, nrow(pa_wide)) %>%
  pack_rows("Panel B: Ball level (match x innings + bowler FE)", nrow(pa_wide) + 1, nrow(dashboard)) %>%
  footnote(
    general = paste0(
      paste0(
        "Panel A: DV as labelled. Match {\\\\times} innings FE, quality controls, match-situation controls. ",
        "Clustered by match. ",
        sprintf("N = %s / %s / %s (Tests/ODIs/T20Is). ",
                format(n_partnership["tests"], big.mark = ","),
                format(n_partnership["odis"], big.mark = ","),
                format(n_partnership["t20is"], big.mark = ",")),
        "Panel B: Ball-level. Match {\\\\times} innings + bowler FE. ",
        "Two-way clustering (match, bowler). ",
        sprintf("N = %s / %s / %s.",
                format(n_ball["tests"], big.mark = ","),
                format(n_ball["odis"], big.mark = ","),
                format(n_ball["t20is"], big.mark = ","))
      )
    ),
    general_title = "Notes:", threeparttable = TRUE
  )

writeLines(kbl_ext2, file.path(tables_dir, "table_ext2_outcomes.tex"))

# Save CSV version
ext2_csv <- bind_rows(
  panel_a %>% mutate(panel = "A: Partnership"),
  panel_b %>% mutate(panel = "B: Ball-level")
) %>%
  select(panel, label, format, estimate, se, p_value, n)

write_csv(ext2_csv, file.path(tables_dir, "table_ext2_outcomes.csv"))
cat("  Extension 2 tables saved.\n")

# Flag notable results
cat("\n  Checking for notable ball-level results:\n")
notable <- panel_b %>% filter(p_value < 0.10)
if (nrow(notable) > 0) {
  for (i in seq_len(nrow(notable))) {
    cat(sprintf("  ** %s in %s: coef = %.4f, p = %.4f\n",
                notable$label[i], format_labels[notable$format[i]],
                notable$estimate[i], notable$p_value[i]))
  }
} else {
  cat("  No significant ball-level coefficients at 10% level.\n")
}


# #########################################################################
# === EXTENSION 3: OUT-OF-POSITION BATSMEN (Tests only) ===
# #########################################################################

cat("\n\n========== EXTENSION 3: OUT-OF-POSITION BATSMEN ==========\n")
cat("  (Tests only — out-of-position batsmen are rare in limited-overs formats)\n")

# --- 3a. Define out-of-position subsample(s) ---
cat("\n--- 3a. Defining out-of-position subsamples ---\n")

df_tests <- load_and_estimate("tests")

# max_bat_pos = max(batting_position_1, batting_position_2) — already in data
# wickets_at_start — already in data
cat(sprintf("  Full Tests sample: %d partnerships\n", nrow(df_tests)))
cat(sprintf("  max_bat_pos range: %d - %d\n",
            min(df_tests$max_bat_pos, na.rm = TRUE),
            max(df_tests$max_bat_pos, na.rm = TRUE)))

# Define cutoff grid for sensitivity
cutoff_grid <- expand.grid(
  pos_cutoff    = c(7, 8, 9),
  wicket_range  = list(2:5, 2:4, 1:5),
  stringsAsFactors = FALSE
) %>%
  mutate(wicket_label = c("2-5", "2-4", "1-5", "2-5", "2-4", "1-5", "2-5", "2-4", "1-5"))

ext3_samples <- list()
ext3_balance <- list()
ext3_results <- list()

for (i in seq_len(nrow(cutoff_grid))) {
  pos_cut  <- cutoff_grid$pos_cutoff[i]
  wick_rng <- cutoff_grid$wicket_range[[i]]
  w_label  <- cutoff_grid$wicket_label[i]
  label    <- sprintf("pos >= %d, wkts %s", pos_cut, w_label)

  df_oop <- df_tests %>%
    filter(max_bat_pos >= pos_cut, wickets_at_start %in% wick_rng)

  n_oop <- nrow(df_oop)
  ext3_samples[[label]] <- tibble(
    cutoff = label, n = n_oop,
    pct_mixed = mean(df_oop$is_mixed_hand, na.rm = TRUE),
    mean_runs = mean(df_oop$runs_scored, na.rm = TRUE),
    mean_balls = mean(df_oop$balls_faced, na.rm = TRUE)
  )

  cat(sprintf("  %s: N = %d, %% mixed = %.1f%%, mean runs = %.1f, mean balls = %.1f\n",
              label, n_oop,
              mean(df_oop$is_mixed_hand, na.rm = TRUE) * 100,
              mean(df_oop$runs_scored, na.rm = TRUE),
              mean(df_oop$balls_faced, na.rm = TRUE)))

  if (n_oop < 200) {
    cat(sprintf("  ** N < 200, flagging as illustrative only.\n"))
  }

  # --- 3b. Balance test: is_mixed_hand ~ quality measures ---
  if (n_oop >= 50) {
    bal <- tryCatch(
      feols(is_mixed_hand ~ avg_partnership_quality + combined_experience +
              wickets_at_start + runs_at_start,
            data = df_oop, cluster = ~match_id),
      error = function(e) NULL
    )

    if (!is.null(bal)) {
      # Joint F-test (Wald test for all regressors)
      f_test <- tryCatch(wald(bal), error = function(e) NULL)
      f_stat <- if (!is.null(f_test)) f_test$stat else NA_real_
      f_pval <- if (!is.null(f_test)) f_test$p else NA_real_

      ext3_balance[[label]] <- tibble(
        cutoff = label, n = n_oop,
        avg_quality_coef = coef(bal)["avg_partnership_quality"],
        avg_quality_se   = se(bal)["avg_partnership_quality"],
        avg_quality_p    = pvalue(bal)["avg_partnership_quality"],
        experience_coef  = coef(bal)["combined_experience"],
        experience_se    = se(bal)["combined_experience"],
        experience_p     = pvalue(bal)["combined_experience"],
        f_stat           = f_stat,
        f_pval           = f_pval
      )

      cat(sprintf("    Balance: avg_quality p = %.3f, experience p = %.3f, joint F p = %.3f\n",
                  pvalue(bal)["avg_partnership_quality"],
                  pvalue(bal)["combined_experience"],
                  ifelse(is.na(f_pval), NA, f_pval)))
    }
  }

  # --- 3c. Primary specification on subsample ---
  if (n_oop >= 100) {
    # Use match FE (not match x innings FE) to preserve DoF
    m_oop_runs <- tryCatch(
      feols(runs_scored ~ is_mixed_hand +
              avg_partnership_quality + combined_experience +
              partnership_number_f + innings_f +
              wickets_at_start + runs_at_start | match_id,
            data = df_oop, cluster = ~match_id),
      error = function(e) { cat(sprintf("    Runs regression failed: %s\n", e$message)); NULL }
    )

    m_oop_balls <- tryCatch(
      feols(balls_faced ~ is_mixed_hand +
              avg_partnership_quality + combined_experience +
              partnership_number_f + innings_f +
              wickets_at_start + runs_at_start | match_id,
            data = df_oop, cluster = ~match_id),
      error = function(e) { cat(sprintf("    Balls regression failed: %s\n", e$message)); NULL }
    )

    runs_row  <- extract_coef(m_oop_runs, "is_mixed_hand", "Runs scored")
    balls_row <- extract_coef(m_oop_balls, "is_mixed_hand", "Balls faced")

    ext3_results[[label]] <- tibble(
      cutoff = label,
      dv = c("Runs scored", "Balls faced"),
      estimate = c(
        ifelse(is.null(runs_row), NA_real_, runs_row$estimate),
        ifelse(is.null(balls_row), NA_real_, balls_row$estimate)
      ),
      se = c(
        ifelse(is.null(runs_row), NA_real_, runs_row$se),
        ifelse(is.null(balls_row), NA_real_, balls_row$se)
      ),
      p_value = c(
        ifelse(is.null(runs_row), NA_real_, runs_row$p_value),
        ifelse(is.null(balls_row), NA_real_, balls_row$p_value)
      ),
      n = n_oop,
      illustrative = n_oop < 500
    )

    if (!is.null(m_oop_runs)) {
      cat(sprintf("    Runs:  coef = %.3f (SE = %.3f, p = %.3f)\n",
                  coef(m_oop_runs)["is_mixed_hand"],
                  se(m_oop_runs)["is_mixed_hand"],
                  pvalue(m_oop_runs)["is_mixed_hand"]))
    }
    if (!is.null(m_oop_balls)) {
      cat(sprintf("    Balls: coef = %.3f (SE = %.3f, p = %.3f)\n",
                  coef(m_oop_balls)["is_mixed_hand"],
                  se(m_oop_balls)["is_mixed_hand"],
                  pvalue(m_oop_balls)["is_mixed_hand"]))
    }
  } else if (n_oop >= 50) {
    # Too small for FE — run simple OLS as illustrative
    m_oop_simple <- tryCatch(
      feols(runs_scored ~ is_mixed_hand +
              avg_partnership_quality + combined_experience +
              wickets_at_start + runs_at_start,
            data = df_oop, cluster = ~match_id),
      error = function(e) NULL
    )

    runs_row <- extract_coef(m_oop_simple, "is_mixed_hand", "Runs scored (OLS)")

    ext3_results[[label]] <- tibble(
      cutoff = label,
      dv = "Runs scored (OLS, no FE)",
      estimate = ifelse(is.null(runs_row), NA_real_, runs_row$estimate),
      se = ifelse(is.null(runs_row), NA_real_, runs_row$se),
      p_value = ifelse(is.null(runs_row), NA_real_, runs_row$p_value),
      n = n_oop,
      illustrative = TRUE
    )
  }
}

# --- 3d. Sensitivity: output tables ---
cat("\n--- 3d. Building Extension 3 output tables ---\n")

# Balance table
ext3_bal_df <- bind_rows(ext3_balance)
if (nrow(ext3_bal_df) > 0) {
  ext3_bal_tab <- ext3_bal_df %>%
    mutate(
      across(c(avg_quality_coef, experience_coef), ~ sprintf("%.4f", .)),
      across(c(avg_quality_se, experience_se), ~ sprintf("(%.4f)", .)),
      across(c(avg_quality_p, experience_p, f_pval), ~ sprintf("%.3f", .)),
      f_stat = sprintf("%.2f", f_stat),
      n = format(n, big.mark = ",")
    ) %>%
    select(cutoff, n, avg_quality_coef, avg_quality_se,
           experience_coef, experience_se, f_stat, f_pval)

  kbl_bal <- kbl(ext3_bal_tab,
      format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Cutoff definition", "N",
                     "Coef.", "SE", "Coef.", "SE",
                     "$F$-stat", "$p$-value"),
      align = "lrrrrrrr",
      caption = "Balance test: is\\_mixed\\_hand on observable quality (out-of-position batsmen, Tests)",
      label = "ext3_balance") %>%
    add_header_above(c(" " = 2, "Avg. quality" = 2, "Experience" = 2, "Joint test" = 2)) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    footnote(
      general = "OLS regression of is\\_mixed\\_hand on quality and match-situation controls. Joint F-test for all regressors. Standard errors clustered by match.",
      general_title = "Notes:", threeparttable = TRUE
    )

  writeLines(kbl_bal, file.path(tables_dir, "table_ext3_balance.tex"))
  write_csv(ext3_bal_df, file.path(tables_dir, "table_ext3_balance.csv"))
  cat("  Balance table saved.\n")
}

# Results table
ext3_res_df <- bind_rows(ext3_results)
if (nrow(ext3_res_df) > 0) {
  ext3_res_tab <- ext3_res_df %>%
    mutate(
      stars = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                        p_value < 0.10 ~ "*", TRUE ~ ""),
      coef_str = ifelse(is.na(estimate), "---",
                        sprintf("%.3f%s", estimate, stars)),
      se_str   = ifelse(is.na(se), "", sprintf("(%.3f)", se)),
      n_str    = format(n, big.mark = ","),
      flag     = ifelse(illustrative, "$\\dagger$", "")
    ) %>%
    select(cutoff, dv, coef_str, se_str, n_str, flag)

  kbl_res <- kbl(ext3_res_tab,
      format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Cutoff", "DV", "Mixed hand", "SE", "N", ""),
      align = "llrrrr",
      caption = "Out-of-position batsmen: effect of mixed-hand pairing (Tests)",
      label = "ext3_oop") %>%
    kable_styling(latex_options = c("hold_position")) %>%
    footnote(
      general = "Out-of-position: lower-order batter at crease with few wickets fallen. Match FE, quality controls, match-situation controls. Standard errors clustered by match. $\\dagger$ = illustrative only (N $<$ 500).",
      general_title = "Notes:", threeparttable = TRUE, escape = FALSE
    )

  writeLines(kbl_res, file.path(tables_dir, "table_ext3_oop_results.tex"))
  write_csv(ext3_res_df, file.path(tables_dir, "table_ext3_oop_results.csv"))
  cat("  OOP results table saved.\n")
}

# Sample size diagnostic
cat("\n--- Extension 3: Sample size summary ---\n")
sample_df <- bind_rows(ext3_samples)
print(sample_df, n = Inf)

primary_n <- sample_df %>% filter(cutoff == "pos >= 8, wkts 2-5") %>% pull(n)
if (length(primary_n) > 0) {
  if (primary_n < 500) {
    cat(sprintf("\n  ** WARNING: Primary cutoff (pos >= 8, wkts 2-5) has N = %d < 500.\n", primary_n))
    cat("  ** Results should be flagged as illustrative only.\n")
  } else {
    cat(sprintf("\n  Primary cutoff (pos >= 8, wkts 2-5): N = %d — adequate for FE specification.\n", primary_n))
  }
}


# #########################################################################
# === SUMMARY ===
# #########################################################################

cat("\n\n####################################################################\n")
cat("# Extension outputs saved to: output/tables/\n")
cat("#\n")
cat("#   Extension 1: table_ext1_bowler_type.{csv,tex}\n")
cat("#   Extension 2: table_ext2_outcomes.{csv,tex}\n")
cat("#   Extension 3: table_ext3_balance.{csv,tex}\n")
cat("#                table_ext3_oop_results.{csv,tex}\n")
cat("#\n")
cat("# Next steps:\n")
cat("#   1. Check sample sizes for Extension 3\n")
cat("#   2. Check pct_pace merge quality (Extension 1)\n")
cat("#   3. Inspect is_wicket LR coefficient (Extension 2)\n")
cat("#   4. Decide which results merit paper vs appendix\n")
cat("####################################################################\n")

cat("\n=== 16_extensions.R complete ===\n")

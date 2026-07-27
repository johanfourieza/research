# =============================================================================
# 15_paper_tables.R
# Format the 8 main paper tables as publication-ready LaTeX.
# Run from RStudio (uses rstudioapi for paths).
# =============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(survival)

# Force modelsummary to use kableExtra (standard booktabs) instead of tabularray
options(modelsummary_factory_latex = "kableExtra")

# --- Paths ---
base_dir     <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
paper_dir    <- file.path(base_dir, "scripts", "output", "tables")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

formats       <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

# Helper: inject \label{} into a modelsummary .tex file after \caption{}
add_table_label <- function(filepath, label) {
  txt <- readLines(filepath)
  cap_idx <- grep("\\\\caption\\{", txt)
  if (length(cap_idx) > 0) {
    txt[cap_idx[1]] <- sub("(\\\\caption\\{.*?\\})",
                           paste0("\\1\\\\label{tab:", label, "}"), txt[cap_idx[1]])
  }
  writeLines(txt, filepath)
}

# Helper: post-process LaTeX table for overfull/underfull fixes
fix_table_tex <- function(filepath, font_size = NULL, tabcolsep = NULL,
                          merge_notes = FALSE) {
  txt <- readLines(filepath)
  # [!h] -> [!ht] (avoids LaTeX float warning)
  txt <- sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[!ht]", txt)
  # Insert font size / tabcolsep after last \centering
  if (!is.null(font_size) || !is.null(tabcolsep)) {
    idx <- max(grep("^\\\\centering", txt))
    ins <- c()
    if (!is.null(font_size)) ins <- c(ins, font_size)
    if (!is.null(tabcolsep)) ins <- c(ins,
      paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"))
    txt <- append(txt, ins, after = idx)
  }
  # Merge separate Notes: title item with content item (fixes underfull)
  if (merge_notes) {
    for (i in seq_len(length(txt) - 1)) {
      if (grepl("\\\\item \\\\textit\\{Notes:\\}", txt[i]) &&
          grepl("^\\\\item ", txt[i + 1])) {
        txt[i] <- paste0("\\item \\textit{Notes:} ",
                         sub("^\\\\item ", "", txt[i + 1]))
        txt <- txt[-(i + 1)]
        break
      }
    }
  }
  writeLines(txt, filepath)
}

# =============================================================================
# TABLE 1: Sample by format and hand combination
# =============================================================================

t1 <- read_csv(file.path(tables_dir, "table1_sample_by_hand.csv"),
               show_col_types = FALSE)

t1_wide <- t1 %>%
  mutate(
    mean_runs = sprintf("%.1f", mean_runs),
    sd_runs   = sprintf("%.1f", sd_runs),
    mean_quality = sprintf("%.1f", mean_quality),
    mean_balls = sprintf("%.1f", mean_balls),
    n = format(n, big.mark = ",")
  ) %>%
  select(format, hand_combination, n, mean_runs, sd_runs, mean_balls, mean_quality)

kbl_t1 <- kbl(t1_wide,
    format = "latex", booktabs = TRUE,
    col.names = c("Format", "Hands", "N", "Mean runs", "SD", "Mean balls", "Mean quality"),
    align = "llrrrrr",
    caption = "Sample composition by format and hand combination",
    label = "sample") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = "Main analysis sample: both batsmen have known hand and at least one prior match. Quality is the average of both batsmen's pre-match batting averages.",
           general_title = "Notes:", threeparttable = TRUE)

writeLines(kbl_t1, file.path(paper_dir, "table1_sample.tex"))
fix_table_tex(file.path(paper_dir, "table1_sample.tex"))
cat("Table 1 saved.\n")

# =============================================================================
# TABLE 2: Summary statistics
# =============================================================================

t2 <- read_csv(file.path(tables_dir, "table2_summary_stats.csv"),
               show_col_types = FALSE)

# Select key variables and reshape
key_vars <- c("runs_scored", "balls_faced", "avg_partnership_quality",
              "combined_experience", "partnership_number",
              "wickets_at_start", "runs_at_start")

var_labels <- c(
  runs_scored = "Partnership runs",
  balls_faced = "Balls faced",
  avg_partnership_quality = "Avg. pre-match batting average",
  combined_experience = "Combined experience (matches)",
  partnership_number = "Partnership number",
  wickets_at_start = "Wickets fallen at start",
  runs_at_start = "Team runs at start"
)

t2_sel <- t2 %>%
  filter(variable %in% key_vars) %>%
  mutate(variable = var_labels[variable]) %>%
  select(format, variable, n, mean, sd, p25, median, p75) %>%
  mutate(across(c(mean, sd, p25, median, p75), ~ sprintf("%.1f", .)))

# Pivot wider: one row per variable, columns grouped by format
t2_tests <- t2_sel %>% filter(format == "Tests") %>%
  select(variable, mean_t = mean, sd_t = sd, median_t = median)
t2_odis <- t2_sel %>% filter(format == "ODIs") %>%
  select(variable, mean_o = mean, sd_o = sd, median_o = median)
t2_t20is <- t2_sel %>% filter(format == "T20Is") %>%
  select(variable, mean_20 = mean, sd_20 = sd, median_20 = median)

t2_final <- t2_tests %>%
  left_join(t2_odis, by = "variable") %>%
  left_join(t2_t20is, by = "variable")

kbl_t2 <- kbl(t2_final,
    format = "latex", booktabs = TRUE,
    col.names = c("Variable", "Mean", "SD", "Median",
                  "Mean", "SD", "Median",
                  "Mean", "SD", "Median"),
    align = "lrrrrrrrrrr",
    caption = "Summary statistics by format",
    label = "summary") %>%
  add_header_above(c(" " = 1, "Tests" = 3, "ODIs" = 3, "T20Is" = 3)) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = "Main analysis sample. Pre-match averages computed from all prior matches in the same format.",
           general_title = "Notes:", threeparttable = TRUE)

writeLines(kbl_t2, file.path(paper_dir, "table2_summary.tex"))
cat("Table 2 saved.\n")

# =============================================================================
# TABLE 3: Main partnership regressions (M x I FE across formats)
# =============================================================================
# Re-estimate to produce clean modelsummary output

load_and_estimate <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings)
    )
  df
}

models_t3 <- list()
for (fmt in formats) {
  df <- load_and_estimate(fmt)

  # (1) OLS with controls
  m_ols <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start,
    data = df, cluster = ~match_id
  )

  # (2) Match x Innings FE (primary)
  m_fe <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start | match_innings_id,
    data = df, cluster = ~match_id
  )

  models_t3[[paste0(format_labels[fmt], " OLS")]] <- m_ols
  models_t3[[paste0(format_labels[fmt], " FE")]]  <- m_fe
}

modelsummary(
  models_t3,
  output = file.path(paper_dir, "table3_main.tex"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "is_mixed_hand"           = "Mixed hand (LR)",
    "avg_partnership_quality"  = "Avg. pre-match average",
    "max_pre_match_avg"        = "Max pre-match average",
    "min_pre_match_avg"        = "Min pre-match average",
    "combined_experience"      = "Combined experience"
  ),
  gof_map = c("nobs", "r.squared", "r2.within",
              "FE: match_innings_id"),
  title = "Effect of mixed-hand partnerships on runs scored",
  notes = c("Standard errors clustered at match level in parentheses.",
            "All specifications include partnership number and match-situation controls.",
            "FE columns include match $\\times$ innings fixed effects.")
)
add_table_label(file.path(paper_dir, "table3_main.tex"), "main")
fix_table_tex(file.path(paper_dir, "table3_main.tex"),
              font_size = "\\small", tabcolsep = "4pt")
cat("Table 3 saved.\n")

# =============================================================================
# TABLE 4: Ball-level mechanism regressions
# =============================================================================
# Read existing CSV outputs and create a combined table

ball_models <- list()
for (fmt in formats) {
  ball_path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(ball_path)) next

  dt <- data.table::fread(ball_path)
  dt <- dt[!is.na(LR_at_crease) & !is.na(striker_pre_avg) & !is.na(ns_pre_avg)]

  m <- feols(
    runs_batter ~ LR_at_crease + strike_changed +
      LR_at_crease:strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt,
    cluster = ~match_id + bowler
  )
  ball_models[[format_labels[fmt]]] <- m
}

modelsummary(
  ball_models,
  output = file.path(paper_dir, "table4_mechanism.tex"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "LR_at_crease"                  = "LR at crease",
    "strike_changed"                = "Strike changed",
    "LR_at_crease:strike_changed"  = "LR $\\times$ Strike changed",
    "striker_pre_avg"               = "Striker pre-match avg.",
    "striker_pre_sr"                = "Striker pre-match SR",
    "ns_pre_avg"                    = "Non-striker pre-match avg."
  ),
  gof_map = c("nobs", "r.squared", "r2.within",
              "FE: match_innings_id", "FE: bowler"),
  title = "Ball-level mechanism: runs per delivery",
  notes = c("Two-way clustered standard errors (match, bowler) in parentheses.",
            "All specifications include match $\\times$ innings and bowler fixed effects.")
)
add_table_label(file.path(paper_dir, "table4_mechanism.tex"), "mechanism")
cat("Table 4 saved.\n")

# =============================================================================
# TABLE 5: IV — First stage and 2SLS
# =============================================================================

iv_models <- list()
for (fmt in formats) {
  df <- load_and_estimate(fmt) %>% filter(!is.na(prob_LR_squad))

  # First stage
  fs <- feols(
    is_mixed_hand ~ prob_LR_squad +
      avg_partnership_quality + combined_experience +
      partnership_number_f + innings_f +
      wickets_at_start + runs_at_start | match_id,
    data = df, cluster = ~match_id
  )

  # 2SLS
  iv <- tryCatch(
    feols(
      runs_scored ~
        avg_partnership_quality + combined_experience +
        partnership_number_f + innings_f +
        wickets_at_start + runs_at_start | match_id |
        is_mixed_hand ~ prob_LR_squad,
      data = df, cluster = ~match_id
    ),
    error = function(e) NULL
  )

  iv_models[[paste0(format_labels[fmt], " FS")]] <- fs
  if (!is.null(iv)) iv_models[[paste0(format_labels[fmt], " 2SLS")]] <- iv
}

modelsummary(
  iv_models,
  output = file.path(paper_dir, "table5_iv.tex"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "prob_LR_squad"           = "Prob(LR) from squad",
    "fit_is_mixed_hand"       = "Mixed hand (LR, IV)",
    "avg_partnership_quality"  = "Avg. pre-match average",
    "combined_experience"      = "Combined experience"
  ),
  gof_map = c("nobs", "r.squared", "r2.within", "FE: match_id"),
  title = "Instrumental variables: squad composition",
  notes = c("Standard errors clustered at match level in parentheses.",
            "Instrument: $Z = 2 n_L n_R / [n(n-1)]$.",
            "All specifications include match fixed effects.")
)
add_table_label(file.path(paper_dir, "table5_iv.tex"), "iv")
fix_table_tex(file.path(paper_dir, "table5_iv.tex"),
              font_size = "\\small", tabcolsep = "4pt")
cat("Table 5 saved.\n")

# =============================================================================
# TABLE 6: Cox PH hazard ratios
# =============================================================================

cox_models <- list()
for (fmt in formats) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0,
           !is.na(is_mixed_hand), balls_faced > 0, !is.na(avg_partnership_quality)) %>%
    mutate(
      event = 1L - as.integer(is_censored),
      partnership_number_c = pmin(partnership_number, 10)
    )

  cox <- coxph(
    Surv(balls_faced, event) ~ is_mixed_hand +
      avg_partnership_quality + combined_experience +
      partnership_number_c + wickets_at_start + runs_at_start +
      strata(match_id),
    data = df
  )
  cox_models[[format_labels[fmt]]] <- cox
}

modelsummary(
  cox_models,
  output = file.path(paper_dir, "table6_cox.tex"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  exponentiate = TRUE,
  coef_map = c(
    "is_mixed_hand"           = "Mixed hand (LR)",
    "avg_partnership_quality"  = "Avg. pre-match average",
    "combined_experience"      = "Combined experience",
    "partnership_number_c"     = "Partnership number",
    "wickets_at_start"        = "Wickets at start",
    "runs_at_start"           = "Runs at start"
  ),
  gof_map = c("nobs", "AIC"),
  title = "Cox proportional hazard models: partnership duration",
  notes = c("Hazard ratios reported (exp($\\beta$)).",
            "Standard errors in parentheses. Stratified by match.")
)
add_table_label(file.path(paper_dir, "table6_cox.tex"), "cox")
cat("Table 6 saved.\n")

# =============================================================================
# TABLE 7: Randomization inference
# =============================================================================

ri <- read_csv(file.path(tables_dir, "table_ri_all.csv"), show_col_types = FALSE)

ri_tab <- ri %>%
  mutate(
    Format = format_labels[format],
    `Obs. coef.` = sprintf("%.3f", obs_coef),
    `Obs. t` = sprintf("%.3f", obs_tstat),
    `Perm. mean` = sprintf("%.3f", perm_mean_coef),
    `Perm. SD` = sprintf("%.3f", perm_sd_coef),
    `p (coef.)` = sprintf("%.3f", exact_p_coef),
    `p (t)` = sprintf("%.3f", exact_p_tstat),
    `N perm.` = format(n_valid_perms, big.mark = ",")
  ) %>%
  select(Format, `Obs. coef.`, `Obs. t`, `Perm. mean`, `Perm. SD`,
         `p (coef.)`, `p (t)`, `N perm.`)

kbl_t7 <- kbl(ri_tab,
    format = "latex", booktabs = TRUE,
    align = "lrrrrrrr",
    caption = "Randomization inference: exact p-values",
    label = "ri") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = "Treatment (is\\_mixed\\_hand) permuted within match. 10,000 permutations per format. Two-sided p-values.",
           general_title = "Notes:", threeparttable = TRUE)

writeLines(kbl_t7, file.path(paper_dir, "table7_ri.tex"))
fix_table_tex(file.path(paper_dir, "table7_ri.tex"),
              font_size = "\\footnotesize", tabcolsep = "3pt")
cat("Table 7 saved.\n")

# =============================================================================
# TABLE 8: T20I Quantile Treatment Effects
# =============================================================================

qte <- read_csv(file.path(tables_dir, "table_qte_t20is.csv"), show_col_types = FALSE)

qte_tab <- qte %>%
  mutate(
    Quantile = sprintf("%.2f", tau),
    Coefficient = sprintf("%.3f", estimate),
    SE = sprintf("%.3f", se),
    `95\\% CI` = sprintf("[%.3f, %.3f]", ci_low, ci_high),
    `p-value` = sprintf("%.3f", p_value),
    Sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Quantile, Coefficient, SE, `95\\% CI`, `p-value`, Sig)

kbl_t8 <- kbl(qte_tab,
    format = "latex", booktabs = TRUE,
    align = "lrrrrr", escape = FALSE,
    caption = "Quantile treatment effects: T20 Internationals",
    label = "qte") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = "Quantile regression of partnership runs on is\\_mixed\\_hand with controls for batsman quality, experience, partnership number, innings, and match situation. Bootstrap standard errors (200 replications).",
           general_title = "Notes:", threeparttable = TRUE)

writeLines(kbl_t8, file.path(paper_dir, "table8_qte.tex"))
fix_table_tex(file.path(paper_dir, "table8_qte.tex"), merge_notes = TRUE)
cat("Table 8 saved.\n")

cat("\n=== All paper tables generated ===\n")

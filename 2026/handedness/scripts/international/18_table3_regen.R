# =============================================================================
# 18_table3_regen.R  —  JSE revise & resubmit (Referee Round 1)
#
# Codex-audit corrections to Table 3 and the openers analysis:
#   1. Regenerate Table 3 without the exact collinearity (the old FE columns
#      included avg + max + min pre-match averages; avg = (max+min)/2).
#      New parameterization: max + min only, in BOTH the OLS and FE columns.
#   2. Estimate OLS and FE on the identical sample (the FE estimation sample)
#      so the OLS -> FE attenuation comparison is clean.
#   3. Pooled opener-interaction test: is_mixed_hand x opener within the full
#      match x innings FE specification (formal test that the opener and
#      non-opener effects differ, rather than comparing significance).
#
# Run as: Rscript scripts/18_table3_regen.R
# Output: scripts/output/referee_revisions/tables/ + JSE/tables/table3_main.tex
# =============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
options(modelsummary_factory_latex = "kableExtra")

base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_dir      <- file.path(base_dir, "scripts", "output", "referee_revisions", "tables")
paper_dir    <- file.path(base_dir, "JSE", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

formats       <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

load_and_estimate <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings),
      is_opener = as.integer(partnership_number == 1)
    )
}

# =============================================================================
# 1 + 2. Table 3 regeneration (max/min quality controls, common sample)
# =============================================================================
cat("========== TABLE 3 REGENERATION ==========\n")

models_t3 <- list()
check <- list()

for (fmt in formats) {
  df <- load_and_estimate(fmt)

  # FE model first (defines the estimation sample)
  m_fe <- feols(
    runs_scored ~ is_mixed_hand +
      max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start | match_innings_id,
    data = df, cluster = ~match_id
  )

  # OLS on the identical estimation sample, identical controls (+ innings_f,
  # which the FE absorb)
  df_fe_sample <- df[unlist(obs(m_fe)), ]
  m_ols <- feols(
    runs_scored ~ is_mixed_hand +
      max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f + innings_f +
      wickets_at_start + runs_at_start,
    data = df_fe_sample, cluster = ~match_id
  )

  models_t3[[paste0(format_labels[fmt], " OLS")]] <- m_ols
  models_t3[[paste0(format_labels[fmt], " FE")]]  <- m_fe

  check[[fmt]] <- tibble(
    format = format_labels[fmt],
    ols_coef = coef(m_ols)["is_mixed_hand"],
    ols_se   = se(m_ols)["is_mixed_hand"],
    ols_p    = pvalue(m_ols)["is_mixed_hand"],
    fe_coef  = coef(m_fe)["is_mixed_hand"],
    fe_se    = se(m_fe)["is_mixed_hand"],
    fe_p     = pvalue(m_fe)["is_mixed_hand"],
    n        = m_fe$nobs
  )
  cat(sprintf("  %s: OLS %.3f (%.3f), FE %.3f (%.3f), N = %d\n",
              format_labels[fmt],
              coef(m_ols)["is_mixed_hand"], se(m_ols)["is_mixed_hand"],
              coef(m_fe)["is_mixed_hand"], se(m_fe)["is_mixed_hand"],
              m_fe$nobs))
}

write_csv(bind_rows(check), file.path(out_dir, "table3_regen_check.csv"))

# Helpers copied from scripts/15_paper_tables.R
add_table_label <- function(filepath, label) {
  txt <- readLines(filepath)
  cap_idx <- grep("\\\\caption\\{", txt)
  if (length(cap_idx) > 0) {
    txt[cap_idx[1]] <- sub("(\\\\caption\\{.*?\\})",
                           paste0("\\1\\\\label{tab:", label, "}"), txt[cap_idx[1]])
  }
  writeLines(txt, filepath)
}
fix_table_tex <- function(filepath, font_size = NULL, tabcolsep = NULL) {
  txt <- readLines(filepath)
  txt <- sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[!ht]", txt)
  # modelsummary escapes \times in notes; restore it
  txt <- gsub("\\$textbackslash\\{\\}times\\$", "$\\\\times$", txt)
  if (!is.null(font_size) || !is.null(tabcolsep)) {
    idx <- max(grep("^\\\\centering", txt))
    ins <- c()
    if (!is.null(font_size)) ins <- c(ins, font_size)
    if (!is.null(tabcolsep)) ins <- c(ins,
      paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"))
    txt <- append(txt, ins, after = idx)
  }
  writeLines(txt, filepath)
}

modelsummary(
  models_t3,
  output = file.path(paper_dir, "table3_main.tex"),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "is_mixed_hand"       = "Mixed hand (LR)",
    "max_pre_match_avg"   = "Max pre-match average",
    "min_pre_match_avg"   = "Min pre-match average",
    "combined_experience" = "Combined experience"
  ),
  gof_map = c("nobs", "r.squared", "r2.within", "FE: match_innings_id"),
  title = "Effect of mixed-hand partnerships on runs scored",
  notes = c("Standard errors clustered at match level in parentheses.",
            "All specifications include partnership number and match-situation controls;",
            "OLS columns additionally include innings indicators.",
            "OLS and FE columns are estimated on the identical sample.",
            "FE columns include match $\\times$ innings fixed effects.")
)
add_table_label(file.path(paper_dir, "table3_main.tex"), "main")
fix_table_tex(file.path(paper_dir, "table3_main.tex"),
              font_size = "\\small", tabcolsep = "4pt")
cat("Table 3 regenerated.\n")

# =============================================================================
# 3. Pooled opener-interaction test
# =============================================================================
cat("\n========== POOLED OPENER INTERACTION ==========\n")

# Match FE (not match x innings): the same identifying variation as the
# openers subsample in Appendix B3, so the opener and non-opener effects and
# their difference are estimated on a like-for-like basis.
res_int <- map_dfr(formats, function(fmt) {
  df <- load_and_estimate(fmt)
  m <- feols(
    runs_scored ~ is_mixed_hand + is_mixed_hand:is_opener +
      max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f + innings_f +
      wickets_at_start + runs_at_start | match_id,
    data = df, cluster = ~match_id
  )
  # The opener main effect is absorbed by the partnership-number dummies.
  tibble(
    format = format_labels[fmt],
    beta_nonopener = coef(m)["is_mixed_hand"],
    se_nonopener   = se(m)["is_mixed_hand"],
    p_nonopener    = pvalue(m)["is_mixed_hand"],
    beta_interaction = coef(m)["is_mixed_hand:is_opener"],
    se_interaction   = se(m)["is_mixed_hand:is_opener"],
    p_interaction    = pvalue(m)["is_mixed_hand:is_opener"],
    n = m$nobs
  )
})
print(as.data.frame(res_int))
write_csv(res_int, file.path(out_dir, "tableA2_opener_interaction.csv"))
cat("Saved: tableA2_opener_interaction.csv\n")

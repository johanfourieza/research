# =============================================================================
# 19_b12_b10_audit.R  —  JSE resubmission integrity fixes (26 July 2026)
#
# Prompted by a Codex audit of the revised manuscript plus checks of our own.
# Four jobs:
#
#   A. Regenerate the multiple-hypothesis-testing table (Appendix B12) from
#      the real estimates. The committed B12 does not correspond to any output
#      in scripts/output/: it shows 12 tests over 4 outcomes with non-monotone
#      "BH adjusted" p-values, while table_mht_corrections.csv holds 6 tests
#      over 2 outcomes with correct monotone BH values. Extend honestly to
#      strike rate so the table covers what the manuscript cites.
#
#   B. Partnership strike rate as dependent variable (manuscript footnote 6).
#      No output for this claim exists anywhere in the project.
#
#   C. Re-estimate the pooled formal P4 test (Appendix B10) with the PREFERRED
#      controls (max + min pre-match average) instead of avg_partnership_quality.
#      Committed B10 reports LR x T20I = +2.57*** and is cited nowhere.
#
#   D. Re-estimate the County / Sheffield corroboration WITHOUT the collinear
#      avg_partnership_quality term (avg = (max+min)/2). Expected: unchanged,
#      because fixest drops the redundant column — but verify, do not assume.
#
# Headless: Rscript scripts/19_b12_b10_audit.R   (never Rscript -e: segfaults)
# Output:   scripts/output/resubmit_audit/
# =============================================================================

library(tidyverse)
library(fixest)
library(survival)

base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_dir      <- file.path(base_dir, "scripts", "output", "resubmit_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260726)

formats <- c("tests", "odis", "t20is")
flab    <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is",
             county = "County Championship", sheffield = "Sheffield Shield")

load_p <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) { cat(sprintf("MISSING: %s\n", path)); return(NULL) }
  read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(partnership_number_f = factor(pmin(partnership_number, 10)),
           match_innings_id = if ("match_innings_id" %in% names(.))
             match_innings_id else paste0(match_id, "_", innings))
}

grab <- function(m, term) {
  ct <- coeftable(m)
  if (!term %in% rownames(ct)) return(tibble(est = NA_real_, se = NA_real_, p = NA_real_))
  tibble(est = ct[term, 1], se = ct[term, 2], p = ct[term, 4])
}

cat("\n=============== LOADING ===============\n")
dat <- set_names(map(formats, load_p), formats)

# =============================================================================
# A + B. Outcome margins: runs, balls, strike rate  (preferred FE spec)
# =============================================================================
cat("\n=============== A/B. OUTCOME MARGINS ===============\n")

margins <- map_dfr(formats, function(fmt) {
  df <- dat[[fmt]]
  if (is.null(df)) return(NULL)
  df <- df %>% mutate(
    partnership_sr = if_else(balls_faced > 0, 100 * runs_scored / balls_faced, NA_real_))

  m_runs <- feols(runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
    combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
    match_innings_id, data = df, cluster = ~match_id)
  m_ball <- feols(balls_faced ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
    combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
    match_innings_id, data = df, cluster = ~match_id)
  m_sr   <- feols(partnership_sr ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
    combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
    match_innings_id, data = df, cluster = ~match_id)

  bind_rows(
    grab(m_runs, "is_mixed_hand") %>% mutate(outcome = "Partnership runs"),
    grab(m_ball, "is_mixed_hand") %>% mutate(outcome = "Balls faced"),
    grab(m_sr,   "is_mixed_hand") %>% mutate(outcome = "Strike rate")
  ) %>% mutate(format = flab[fmt], n = nobs(m_runs))
})

margins <- margins %>%
  mutate(p_bh   = p.adjust(p, method = "BH"),
         p_holm = p.adjust(p, method = "holm"),
         bh_rank = rank(p, ties.method = "first")) %>%
  arrange(bh_rank)

cat("\n--- Outcome margins with BH correction (", nrow(margins), " tests) ---\n", sep = "")
print(as.data.frame(margins %>% select(outcome, format, est, se, p, bh_rank, p_bh)), digits = 4)
cat(sprintf("\n  min BH adjusted p = %.4f\n", min(margins$p_bh)))
cat(sprintf("  BH monotone in rank? %s\n",
            all(diff(margins$p_bh[order(margins$bh_rank)]) >= -1e-12)))
write_csv(margins, file.path(out_dir, "A_outcome_margins_bh.csv"))

# =============================================================================
# C. Pooled formal P4 test — preferred controls vs the committed B10 spec
# =============================================================================
cat("\n=============== C. POOLED P4 TEST (B10) ===============\n")

pooled <- bind_rows(map(formats, function(fmt) {
  df <- dat[[fmt]]
  if (is.null(df)) return(NULL)
  df %>% mutate(fmt = flab[fmt],
                match_innings_id = paste0(fmt, "_", match_innings_id)) %>%
    select(runs_scored, is_mixed_hand, avg_partnership_quality,
           max_pre_match_avg, min_pre_match_avg, combined_experience,
           partnership_number_f, wickets_at_start, runs_at_start,
           match_innings_id, match_id, fmt)
})) %>% mutate(fmt = factor(fmt, levels = c("Tests", "ODIs", "T20Is")))

# (i) the committed B10 specification: avg quality only
m_b10_old <- feols(
  runs_scored ~ is_mixed_hand * fmt + avg_partnership_quality +
    combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
    match_innings_id, data = pooled, cluster = ~match_id)

# (ii) preferred controls: max + min (level and within-pair spread)
m_b10_new <- feols(
  runs_scored ~ is_mixed_hand * fmt + max_pre_match_avg + min_pre_match_avg +
    combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
    match_innings_id, data = pooled, cluster = ~match_id)

cat("\n--- (i) committed B10 spec: avg_partnership_quality only ---\n")
print(coeftable(m_b10_old)[grep("is_mixed_hand", rownames(coeftable(m_b10_old))), ], digits = 4)
cat("\n--- (ii) preferred spec: max + min pre-match average ---\n")
print(coeftable(m_b10_new)[grep("is_mixed_hand", rownames(coeftable(m_b10_new))), ], digits = 4)

p4_old <- grab(m_b10_old, "is_mixed_hand:fmtT20Is") %>% mutate(spec = "avg quality only (committed B10)")
p4_new <- grab(m_b10_new, "is_mixed_hand:fmtT20Is") %>% mutate(spec = "max + min (preferred)")
p4 <- bind_rows(p4_old, p4_new) %>% mutate(term = "is_mixed_hand x T20I", n = nobs(m_b10_old))
print(as.data.frame(p4), digits = 4)
write_csv(p4, file.path(out_dir, "C_pooled_p4.csv"))

# =============================================================================
# D. Domestic corroboration without the collinear avg term
# =============================================================================
cat("\n=============== D. DOMESTIC (COLLINEARITY CHECK) ===============\n")

dom <- map_dfr(c("county", "sheffield"), function(fmt) {
  df <- load_p(fmt)
  if (is.null(df)) return(NULL)

  m_old <- feols(
    runs_scored ~ is_mixed_hand + avg_partnership_quality +
      max_pre_match_avg + min_pre_match_avg + combined_experience +
      partnership_number_f + wickets_at_start + runs_at_start |
      match_innings_id, data = df, cluster = ~match_id)

  m_new <- feols(
    runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f + wickets_at_start + runs_at_start |
      match_innings_id, data = df, cluster = ~match_id)

  bind_rows(
    grab(m_old, "is_mixed_hand") %>% mutate(spec = "with collinear avg (committed)"),
    grab(m_new, "is_mixed_hand") %>% mutate(spec = "max + min only (clean)")
  ) %>% mutate(competition = flab[fmt], n = nobs(m_new),
               mean_runs = mean(df$runs_scored, na.rm = TRUE))
})

print(as.data.frame(dom), digits = 4)
write_csv(dom, file.path(out_dir, "D_domestic.csv"))

cat("\n=============== DONE ===============\n")
cat(sprintf("Output written to %s\n", out_dir))

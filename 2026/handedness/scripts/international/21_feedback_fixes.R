# =============================================================================
# 21_feedback_fixes.R  —  QC-feedback fixes (26 July 2026, evening round)
#
# Responds to the voluntary QC report in JSE/resubmit/feedback-invisible-*.md,
# adjudicated with Codex (gpt-5.4) as adversarial overseer. Components:
#
#   A. Oster delta CORRECTION (C8/C22): the delta column of Table 6 was computed
#      with the ratio (Rmax-R~)/(R~-R0) inverted. Correct approximation:
#      delta* = beta_controlled / (beta_controlled - beta_star_delta1).
#      Arithmetic on the committed CSV; no re-estimation. Also report whether
#      the delta=1 identified set excludes zero (set-based robustness).
#   B. B3 openers RE-RUN (C3): preferred controls (max + min pre-match average,
#      combined experience, innings dummies) with match FE. The committed B3
#      used average quality only.
#   C. Table 8 randomization inference RE-RUN (C2): permute is_mixed_hand
#      within match x innings cells (= within batting team), not within match
#      across teams; observed model = preferred Table 3 FE specification.
#      Monte Carlo permutation p-values, NOT "exact".
#   D. Truncation robustness (Overall/OF5): preferred FE spec on matches from
#      2006-01-01, so career statistics are mostly initialised in-sample
#      (histories start at the 2001 Cricsheet epoch).
#   E. TOST equivalence (OF3/C16): margin +/-1 run (~3-5% of mean partnership
#      runs; the bound the paper already quotes), sensitivity +/-1.5 runs.
#      Runs outcome only, from the Table 3 FE estimates.
#   F. BH multiplicity across the B19/B20 moderation tests (C18/C30), from the
#      committed referee_revisions CSVs. Arithmetic only.
#   G. T20I no-FE covariate-adjusted mean (QTE-comparable benchmark for the
#      corrected Figure 6; C25).
#   H. B8 RE-RUN: the committed B8 filtered combined_experience >= 20 (which is
#      NOT "both batsmen >= 10 matches" as its note claims) and used average
#      quality. Re-run with pre_match_matches_1 >= 10 & pre_match_matches_2
#      >= 10 and the preferred controls.
#
# Headless: run as  Rscript scripts/21_feedback_fixes.R
# (Do NOT use Rscript -e; it segfaults in this environment.)
# Output: scripts/output/feedback_fixes/*.csv
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(fixest)
})

base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
tables_dir   <- file.path(base_dir, "scripts", "output", "tables")
ref_dir      <- file.path(base_dir, "scripts", "output", "referee_revisions", "tables")
out_dir      <- file.path(base_dir, "scripts", "output", "feedback_fixes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260726)

formats <- c("tests", "odis", "t20is")
flab    <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

load_p <- function(fmt) {
  read_csv(file.path(analysis_dir, paste0("analysis_", fmt, ".csv")),
           show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(partnership_number_f = factor(pmin(partnership_number, 10)),
           innings_f = factor(innings),
           match_innings_id = if ("match_innings_id" %in% names(.))
             match_innings_id else paste0(match_id, "_", innings))
}

grab <- function(m, term) {
  ct <- coeftable(m)
  tibble(est = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}

# =============================================================================
# A. Oster delta correction (arithmetic on committed CSV)
# =============================================================================
cat("\n========== A. OSTER DELTA CORRECTION ==========\n")

ost <- read_csv(file.path(tables_dir, "table_oster_all.csv"), show_col_types = FALSE) %>%
  mutate(
    # Zero-crossing delta on the linear path beta*(delta) used for beta_star_delta1:
    # delta* = beta_controlled / (beta_controlled - beta_star(delta=1))
    delta_star = beta_controlled / (beta_controlled - beta_star_delta1),
    set_lo = pmin(beta_controlled, beta_star_delta1),
    set_hi = pmax(beta_controlled, beta_star_delta1),
    set_excludes_zero = (set_lo > 0) | (set_hi < 0)
  )
print(as.data.frame(ost %>% select(format, beta_controlled, beta_star_delta1,
                                   delta_star, set_lo, set_hi, set_excludes_zero)),
      digits = 4)
write_csv(ost, file.path(out_dir, "A_oster_corrected.csv"))

# =============================================================================
# B. B3 openers re-run with preferred controls
# =============================================================================
cat("\n========== B. OPENERS (B3) PREFERRED CONTROLS ==========\n")

dat <- set_names(map(formats, load_p), formats)

b3 <- map_dfr(formats, function(fmt) {
  df <- dat[[fmt]] %>% filter(partnership_number == 1)
  m <- feols(runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
               combined_experience + innings_f | match_id,
             data = df, cluster = ~match_id)
  grab(m, "is_mixed_hand") %>% mutate(format = flab[fmt])
})
print(as.data.frame(b3), digits = 4)
write_csv(b3, file.path(out_dir, "B_openers_preferred.csv"))

# =============================================================================
# C. Randomization inference re-run: within match x innings, preferred spec
# =============================================================================
cat("\n========== C. RI WITHIN MATCH x INNINGS (10,000 PERMS) ==========\n")

old_notes <- getFixest_notes(); setFixest_notes(FALSE)

ri <- map_dfr(formats, function(fmt) {
  df <- dat[[fmt]]
  obs <- feols(runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
                 combined_experience + partnership_number_f +
                 wickets_at_start + runs_at_start | match_innings_id,
               data = df, cluster = ~match_id)
  obs_b <- coef(obs)["is_mixed_hand"]; obs_t <- tstat(obs)["is_mixed_hand"]

  # Permute within match x innings; guard singleton cells (sample(x) on a
  # length-1 numeric draws from 1:x, which would corrupt the permutation).
  shuffle <- function(x) if (length(x) == 1L) x else sample(x)
  dt <- df
  n_perms <- 10000
  perm_b <- numeric(n_perms); perm_t <- numeric(n_perms)
  for (i in seq_len(n_perms)) {
    dt$mh_perm <- ave(dt$is_mixed_hand, dt$match_innings_id, FUN = shuffle)
    pm <- feols(runs_scored ~ mh_perm + max_pre_match_avg + min_pre_match_avg +
                  combined_experience + partnership_number_f +
                  wickets_at_start + runs_at_start | match_innings_id,
                data = dt, cluster = ~match_id)
    perm_b[i] <- coef(pm)["mh_perm"]; perm_t[i] <- tstat(pm)["mh_perm"]
    if (i %% 1000 == 0) cat(sprintf("  %s: %d/%d\n", flab[fmt], i, n_perms))
  }
  tibble(format = flab[fmt], obs_coef = obs_b, obs_t = obs_t,
         perm_mean = mean(perm_b), perm_sd = sd(perm_b),
         p_coef = mean(abs(perm_b) >= abs(obs_b)),
         p_t    = mean(abs(perm_t) >= abs(obs_t)),
         n_perms = n_perms)
})
setFixest_notes(old_notes)
print(as.data.frame(ri), digits = 4)
write_csv(ri, file.path(out_dir, "C_ri_within_innings.csv"))

# =============================================================================
# D. Truncation robustness: matches from 2006 onward
# =============================================================================
cat("\n========== D. TRUNCATION (2006+) ==========\n")

tr <- map_dfr(formats, function(fmt) {
  df <- dat[[fmt]] %>% filter(as.Date(start_date) >= as.Date("2006-01-01"))
  m <- feols(runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
               combined_experience + partnership_number_f +
               wickets_at_start + runs_at_start | match_innings_id,
             data = df, cluster = ~match_id)
  grab(m, "is_mixed_hand") %>% mutate(format = flab[fmt])
})
print(as.data.frame(tr), digits = 4)
write_csv(tr, file.path(out_dir, "D_truncation_2006.csv"))

# =============================================================================
# E. TOST equivalence at +/-1 and +/-1.5 runs (Table 3 FE estimates)
# =============================================================================
cat("\n========== E. TOST EQUIVALENCE ==========\n")

t3 <- tibble(format = c("Tests", "ODIs", "T20Is"),
             b  = c(-0.041, -0.103, 0.186),
             se = c( 0.506,  0.348, 0.256))
tost <- t3 %>%
  crossing(margin = c(1, 1.5)) %>%
  mutate(z_lower = (b + margin) / se,
         z_upper = (margin - b) / se,
         p_tost  = pmax(pnorm(z_lower, lower.tail = FALSE),
                        pnorm(z_upper, lower.tail = FALSE)))
print(as.data.frame(tost), digits = 4)
write_csv(tost, file.path(out_dir, "E_tost.csv"))

# =============================================================================
# F. BH across the B19/B20 moderation tests
# =============================================================================
cat("\n========== F. BH ACROSS B19/B20 ==========\n")

c1 <- read_csv(file.path(ref_dir, "tableC1_bowler_triples.csv"), show_col_types = FALSE)
c2 <- read_csv(file.path(ref_dir, "tableC2_bowler_class_splits.csv"), show_col_types = FALSE)
d  <- read_csv(file.path(ref_dir, "tableD_wides_noballs.csv"), show_col_types = FALSE)

b19 <- bind_rows(c1 %>% select(p_value), c2 %>% select(p_value)) %>%
  mutate(p_bh = p.adjust(p_value, "BH"))
cat(sprintf("  B19 (n=%d moderation tests): min raw p = %.4f, min BH = %.4f\n",
            nrow(b19), min(b19$p_value), min(b19$p_bh)))

b20 <- d %>% filter(coef_of == "LR x strike changed") %>%
  mutate(p_bh = p.adjust(p_value, "BH"))
cat(sprintf("  B20 (n=%d interaction tests): min raw p = %.4f, min BH = %.4f\n",
            nrow(b20), min(b20$p_value), min(b20$p_bh)))

write_csv(b19, file.path(out_dir, "F_bh_b19.csv"))
write_csv(b20, file.path(out_dir, "F_bh_b20.csv"))

# =============================================================================
# G. T20I no-FE covariate-adjusted mean (QTE-comparable, for Figure 6)
# =============================================================================
cat("\n========== G. T20I NO-FE MEAN (QTE CONTROLS) ==========\n")

df_t <- dat[["t20is"]]
m_nofe <- feols(runs_scored ~ is_mixed_hand + avg_partnership_quality +
                  combined_experience + partnership_number_f + innings_f +
                  wickets_at_start + runs_at_start,
                data = df_t, cluster = ~match_id)
g <- grab(m_nofe, "is_mixed_hand") %>% mutate(spec = "no-FE, QTE controls")
print(as.data.frame(g), digits = 4)
write_csv(g, file.path(out_dir, "G_t20i_nofe_mean.csv"))

# =============================================================================
# H. B8 re-run: both batsmen >= 10 prior matches, preferred controls
# =============================================================================
cat("\n========== H. B8 (>=10 MATCHES EACH) PREFERRED CONTROLS ==========\n")

b8 <- map_dfr(formats, function(fmt) {
  df <- dat[[fmt]] %>% filter(pre_match_matches_1 >= 10, pre_match_matches_2 >= 10)
  m <- feols(runs_scored ~ is_mixed_hand + max_pre_match_avg + min_pre_match_avg +
               combined_experience + partnership_number_f +
               wickets_at_start + runs_at_start | match_innings_id,
             data = df, cluster = ~match_id)
  grab(m, "is_mixed_hand") %>% mutate(format = flab[fmt])
})
print(as.data.frame(b8), digits = 4)
write_csv(b8, file.path(out_dir, "H_b8_min10_preferred.csv"))

cat("\n========== DONE ==========\n")
cat(sprintf("Outputs in %s\n", out_dir))

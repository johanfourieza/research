# =============================================================================
# 03_robustness.R -- robustness of the main elasticity
# -----------------------------------------------------------------------------
# 3.1 Leave-one-journal-out
# 3.2 Leave-one-year-out
# 3.3 Paper-level bootstrap of the M3 coefficient
# 3.4 Fast-starter threshold sensitivity
# 3.5 Extended sample with age-3 fallback for the long-run outcome
# 3.6 Post-age-two citation growth (flow outcome; removes cumulative overlap)
# New in this revision:
# 3.7 Poisson pseudo-maximum-likelihood (PPML) on citation levels
# 3.8 Topic fixed effects keeping the 'other' category as a level (N = 1,262)
# 3.9 Sensitivity to the top-institution control (dropped entirely)
#
# Output: results/res_03_robustness.rds
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("03_robustness")
set.seed(SEED_BOOT)

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
est <- ad$est
jn  <- ad$jn

# Reference estimate (M3)
m3 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year, data = est)

# --- 3.1 Leave-one-journal-out --------------------------------------------------
cat("1. Leave-one-journal-out:\n")
lojo <- lapply(unique(est$journal), function(j) {
  m <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year,
            data = est[journal != j])
  data.table(excluded = j, coef = coef(m)["log_early"],
             se = summary(m)$coefficients["log_early", "Std. Error"])
})
lojo <- rbindlist(lojo)
print(lojo)
cat("   Range:", round(min(lojo$coef), 3), "-", round(max(lojo$coef), 3), "\n\n")

# --- 3.2 Leave-one-year-out -----------------------------------------------------
cat("2. Leave-one-year-out:\n")
loyo <- lapply(unique(est$year), function(y) {
  m <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year,
            data = est[year != y])
  data.table(excluded = y, coef = coef(m)["log_early"],
             se = summary(m)$coefficients["log_early", "Std. Error"])
})
loyo <- rbindlist(loyo)
cat("   Range:", round(min(loyo$coef), 3), "-", round(max(loyo$coef), 3), "\n\n")

# --- 3.3 Bootstrap ----------------------------------------------------------------
cat("3. Bootstrap (200 replications):\n")
boot_fn <- function(d, i) {
  m <- tryCatch({
    felm(log_longrun ~ log_early + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no |
           journal + year, data = d[i, ])
  }, error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  coef(m)["log_early"]
}
boot_res <- boot(as.data.frame(est), boot_fn, R = 200)
boot_se <- sd(boot_res$t, na.rm = TRUE)
cat("   Analytical SE:", round(summary(m3)$coefficients["log_early", "Std. Error"], 4), "\n")
cat("   Bootstrap SE:", round(boot_se, 4), "\n\n")

# --- 3.4 Threshold sensitivity ------------------------------------------------------
cat("4. Fast starter threshold sensitivity:\n")
thresh_df <- lapply(c(0.50, 0.60, 0.70, 0.75, 0.80, 0.90), function(th) {
  est[, fs_temp := ifelse(!is.na(cite_early_pctile) & cite_early_pctile > th, 1L, 0L)]
  m <- felm(log_longrun ~ fs_temp + n_authors + any_top_inst +
               log_article_length + title_nchar + article_position + issue_no |
               journal + year, data = est)
  data.table(threshold = paste0("Top ", (1 - th) * 100, "%"),
             coef = coef(m)["fs_temp"],
             se = summary(m)$coefficients["fs_temp", "Std. Error"])
})
thresh_df <- rbindlist(thresh_df)
est[, fs_temp := NULL]
print(thresh_df)
cat("\n")

# --- 3.5 Extended sample with age-3 fallback -----------------------------------------
cat("5. Extended sample with age-3 fallback:\n")

jn[, cite_longrun_ext := ifelse(!is.na(cite_age_8), cite_age_8,
                                 ifelse(!is.na(cite_age_5), cite_age_5,
                                        ifelse(!is.na(cite_age_3), cite_age_3, NA_real_)))]
jn[, longrun_source := ifelse(!is.na(cite_age_8), "age8",
                               ifelse(!is.na(cite_age_5), "age5",
                                      ifelse(!is.na(cite_age_3), "age3", NA_character_)))]

est_ext <- jn[!is.na(cite_early) & !is.na(cite_longrun_ext) &
                cite_early >= 0 & cite_longrun_ext >= 0 &
                n_authors > 0 & is_core == TRUE]
est_ext[, log_early_ext := log(cite_early + 1)]
est_ext[, log_longrun_ext := log(cite_longrun_ext + 1)]

cat("   Main sample (age 5-8):", nrow(est), "papers, years",
    min(est$year), "-", max(est$year), "\n")
cat("   Extended sample (age 3-8):", nrow(est_ext), "papers, years",
    min(est_ext$year), "-", max(est_ext$year), "\n")
cat("   Long-run source breakdown:\n")
print(est_ext[, .N, by = longrun_source][order(longrun_source)])
cat("\n")

m_ext <- felm(log_longrun_ext ~ log_early_ext + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = est_ext)

cat("   Main sample coef (M3):", round(coef(m3)["log_early"], 4),
    "(robust SE:", round(rob_se(m3, "log_early"), 4), ", N =", nrow(est), ")\n")
cat("   Extended sample coef:", round(coef(m_ext)["log_early_ext"], 4),
    "(robust SE:", round(rob_se(m_ext, "log_early_ext"), 4),
    ", N =", nrow(est_ext), ")\n\n")

# --- 3.6 Post-age-two citation GROWTH (removes mechanical overlap) --------------------
# The baseline regresses cumulative long-run citations on cumulative age-two
# citations; the outcome structurally contains the regressor. As a flow-based
# check, regress citations accumulated strictly AFTER age two on early
# citations. A still-strong elasticity rebuts the overlap concern.
cat("6. Post-age-two citation growth (flow outcome, removes overlap):\n")
m_growth <- felm(log_growth ~ log_early + n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est)
cat("   Growth elasticity (post-age-2):", round(coef(m_growth)["log_early"], 4),
    "(robust SE:", round(rob_se(m_growth, "log_early"), 4), ", N =", m_growth$N, ")\n")
cat("   Share of papers with zero post-age-2 growth:",
    round(mean(est$cite_growth == 0) * 100, 1), "%\n\n")

# --- 3.7 PPML on citation levels (new) --------------------------------------------------
# The baseline uses log(1 + citations). As a functional-form check, estimate the
# same specification by Poisson pseudo-maximum likelihood on the citation LEVEL,
# which requires no transformation and handles zeros directly.
cat("7. PPML (Poisson pseudo-maximum likelihood) on citation levels:\n")
m_ppml <- fixest::fepois(cite_longrun ~ log_early + n_authors + any_top_inst +
                           log_article_length + title_nchar + article_position + issue_no |
                           journal + year, data = est, vcov = "hetero")
ppml_ct <- summary(m_ppml)$coeftable
cat("   log_early:", round(ppml_ct["log_early", "Estimate"], 4),
    "(robust SE:", round(ppml_ct["log_early", "Std. Error"], 4),
    ", N =", m_ppml$nobs, ")\n\n")

# --- 3.8 Topic FE keeping 'other' as a level (new) ----------------------------------------
# The submitted version's topic-FE columns drop the 387 papers whose titles
# match no keyword ('other'). Keeping 'other' as its own fixed-effect level
# retains the full sample.
cat("8. Topic FE with 'other' retained as a level:\n")
m4_all <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year + topic, data = est)
cat("   log_early:", round(coef(m4_all)["log_early"], 4),
    "(robust SE:", round(rob_se(m4_all, "log_early"), 4),
    ", N =", m4_all$N, ")  [vs M4 excluding 'other']\n\n")

# --- 3.9 Dropping the top-institution control (new) ----------------------------------------
# The top-institution indicator rests on an 18-name list (see Appendix). The
# main elasticity does not depend on it.
cat("9. M3 without the top-institution control:\n")
m3_noinst <- felm(log_longrun ~ log_early + n_authors +
                    log_article_length + title_nchar + article_position + issue_no |
                    journal + year, data = est)
cat("   log_early:", round(coef(m3_noinst)["log_early"], 4),
    "(robust SE:", round(rob_se(m3_noinst, "log_early"), 4),
    ", N =", m3_noinst$N, ")\n\n")

# --- 3.10 RePEc-matched subsample ----------------------------------------------------------
# Author-quality data cover a subset of the sample; the main elasticity on the
# matched subsample checks that this coverage does not select on the
# relationship of interest.
cat("10. M3 on the RePEc-matched subsample:\n")
m3_repec <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est[!is.na(team_max_seniority)])
cat("   log_early:", round(coef(m3_repec)["log_early"], 4),
    "(robust SE:", round(rob_se(m3_repec, "log_early"), 4),
    ", N =", m3_repec$N, ")\n\n")

# --- Save --------------------------------------------------------------------------------
robustness <- list(
  lojo = lojo, loyo = loyo, boot_se = boot_se, thresh = thresh_df,
  m3 = list(coef = coef(m3)["log_early"], se = rob_se(m3, "log_early"), n = m3$N),
  ext_sample = list(coef = coef(m_ext)["log_early_ext"],
                    se = rob_se(m_ext, "log_early_ext"),
                    n_main = nrow(est), n_ext = nrow(est_ext)),
  growth = list(coef = coef(m_growth)["log_early"],
                se = rob_se(m_growth, "log_early"), n = m_growth$N,
                zero_growth_share = mean(est$cite_growth == 0)),
  ppml = list(coef = ppml_ct["log_early", "Estimate"],
              se = ppml_ct["log_early", "Std. Error"], n = m_ppml$nobs),
  topic_all = list(coef = coef(m4_all)["log_early"],
                   se = rob_se(m4_all, "log_early"), n = m4_all$N),
  no_top_inst = list(coef = coef(m3_noinst)["log_early"],
                     se = rob_se(m3_noinst, "log_early"), n = m3_noinst$N),
  repec_subsample = list(coef = coef(m3_repec)["log_early"],
                         se = rob_se(m3_repec, "log_early"), n = m3_repec$N)
)
saveRDS(robustness, file.path(RESULTS_DIR, "res_03_robustness.rds"))
cat("Saved: results/res_03_robustness.rds\n")

close_log()

# =============================================================================
# 07_heterogeneity.R -- heterogeneity of the elasticity
# -----------------------------------------------------------------------------
# 7.1 By topic (forest plot input)
# 7.2 By authorship mode (solo vs co-authored) and top-institution interaction
# 7.3 NEW in this revision: by publication cohort.
#     The baseline long-run outcome mixes age-8 citations (2012-2018 cohorts)
#     and age-5 citations (2019-2021 cohorts), so a raw cohort comparison would
#     confound cohort with outcome age. The cohort analysis therefore holds the
#     outcome age fixed at FIVE years, which is observable for every cohort
#     2012-2021 given the 2014-2026 snapshots. It reports (a) the elasticity
#     estimated separately by cohort and (b) a pooled linear-trend interaction.
#
# Output: results/res_07_heterogeneity.rds
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("07_heterogeneity")

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
est <- ad$est
mech_file <- file.path(RESULTS_DIR, "mech_data.rds")
mech <- if (file.exists(mech_file)) readRDS(mech_file) else copy(est)

# =============================================================================
# 7.1 By topic
# =============================================================================

m3_ref <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = mech)
overall_coef <- coef(m3_ref)["log_early"]
overall_se   <- summary(m3_ref)$coefficients["log_early", "Std. Error"]

cat("Overall log_early coefficient:", round(overall_coef, 4),
    "(SE:", round(overall_se, 4), ")\n\n")

topic_counts <- mech[!is.na(topic) & topic != "other", .N, by = topic]
cat("Topic frequencies:\n")
print(topic_counts[order(-N)])
cat("\n")

topic_results_list <- list()
topic_fs_list <- list()

for (t in topic_counts[N >= 30, topic]) {
  t_data <- mech[topic == t]

  tryCatch({
    m_t <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                  log_article_length + title_nchar + article_position + issue_no |
                  journal + year, data = t_data)
    topic_results_list[[t]] <- data.table(
      topic = t,
      n = nrow(t_data),
      coef = coef(m_t)["log_early"],
      se = summary(m_t)$coefficients["log_early", "Std. Error"]
    )
  }, error = function(e) {
    cat("  Skipping topic", t, ":", e$message, "\n")
  })

  tryCatch({
    t_fs_data <- t_data[!is.na(fast_starter)]
    if (nrow(t_fs_data) >= 30 && sum(t_fs_data$fast_starter) >= 5) {
      m_fs <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                    log_article_length + title_nchar + article_position + issue_no |
                    journal + year, data = t_fs_data)
      topic_fs_list[[t]] <- data.table(
        topic = t,
        n = nrow(t_fs_data),
        coef = coef(m_fs)["fast_starter"],
        se = summary(m_fs)$coefficients["fast_starter", "Std. Error"]
      )
    }
  }, error = function(e) NULL)
}

topic_results <- rbindlist(topic_results_list)
topic_results[, ci_lo := coef - 1.96 * se]
topic_results[, ci_hi := coef + 1.96 * se]

cat("=== ELASTICITY BY TOPIC ===\n\n")
print(topic_results[order(-coef)])
cat("\n")

topic_fs_results <- if (length(topic_fs_list) > 0) rbindlist(topic_fs_list) else NULL
if (!is.null(topic_fs_results)) {
  cat("Fast-starter effect by topic:\n")
  print(topic_fs_results[order(-coef)])
  cat("\n")
}

# =============================================================================
# 7.2 By authorship mode and institution
# =============================================================================
cat("=== HETEROGENEITY BY AUTHORSHIP AND INSTITUTION ===\n\n")
het_solo  <- felm(log_longrun ~ log_early + any_top_inst + log_article_length +
                    title_nchar + article_position + issue_no | journal + year,
                  data = est[n_authors == 1])
het_co    <- felm(log_longrun ~ log_early + n_authors + any_top_inst + log_article_length +
                    title_nchar + article_position + issue_no | journal + year,
                  data = est[n_authors >= 2])
cat(sprintf("  Solo-authored elasticity:       %.3f (robust SE %.3f, N=%d)\n",
            coef(het_solo)["log_early"], rob_se(het_solo, "log_early"), het_solo$N))
cat(sprintf("  Co-authored elasticity:         %.3f (robust SE %.3f, N=%d)\n",
            coef(het_co)["log_early"], rob_se(het_co, "log_early"), het_co$N))
het_inst <- felm(log_longrun ~ log_early * any_top_inst + n_authors + log_article_length +
                   title_nchar + article_position + issue_no | journal + year, data = est)
cat(sprintf("  log_early x top-institution:    %.3f (robust SE %.3f)  [interaction]\n",
            coef(het_inst)["log_early:any_top_inst"], rob_se(het_inst, "log_early:any_top_inst")))
cat(sprintf("  top-institution main effect:    %.3f (robust SE %.3f)\n\n",
            coef(het_inst)["any_top_inst"], rob_se(het_inst, "any_top_inst")))
heterogeneity_extra <- list(
  solo = c(coef(het_solo)["log_early"], rob_se(het_solo, "log_early"), het_solo$N),
  co   = c(coef(het_co)["log_early"],   rob_se(het_co, "log_early"),   het_co$N),
  inst_interaction = c(coef(het_inst)["log_early:any_top_inst"],
                       rob_se(het_inst, "log_early:any_top_inst")))

# =============================================================================
# 7.3 By publication cohort (new in this revision)
# =============================================================================
cat("=== ELASTICITY BY PUBLICATION COHORT (fixed age-5 outcome) ===\n\n")

coh <- est[!is.na(cite_age_5)]
coh[, log_age5 := log(cite_age_5 + 1)]
cat("Papers with age-5 citations observable:", nrow(coh),
    "of", nrow(est), "estimation-sample papers\n\n")

# (a) Cohort-by-cohort estimates (journal FE within each single-year cohort)
cohort_results <- rbindlist(lapply(sort(unique(coh$year)), function(y) {
  d <- coh[year == y]
  m <- tryCatch(felm(log_age5 ~ log_early + n_authors + any_top_inst +
                       log_article_length + title_nchar + article_position + issue_no |
                       journal, data = d), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  data.table(cohort = y, n = nrow(d),
             coef = coef(m)["log_early"], se = rob_se(m, "log_early"))
}))
cohort_results[, ci_lo := coef - 1.96 * se]
cohort_results[, ci_hi := coef + 1.96 * se]
print(cohort_results)
cat("\n   Range:", round(min(cohort_results$coef), 3), "-",
    round(max(cohort_results$coef), 3), "\n\n")

# (b) Pooled trend test: does the elasticity change linearly across cohorts?
coh[, cohort_c := year - min(year)]
m_trend <- felm(log_age5 ~ log_early * cohort_c + n_authors + any_top_inst +
                  log_article_length + title_nchar + article_position + issue_no |
                  journal + year, data = coh)
cat("Pooled interaction (log_early x cohort, cohort centred at 2012):\n")
cat(sprintf("  log_early (2012 cohort):      %.4f (robust SE %.4f)\n",
            coef(m_trend)["log_early"], rob_se(m_trend, "log_early")))
cat(sprintf("  log_early x cohort trend:     %.4f (robust SE %.4f), N = %d\n\n",
            coef(m_trend)["log_early:cohort_c"], rob_se(m_trend, "log_early:cohort_c"),
            m_trend$N))

# (c) Early vs late half of the sample period
coh[, late_half := as.integer(year >= 2017)]
m_half <- felm(log_age5 ~ log_early * late_half + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = coh)
cat("Two-period comparison (2012-2016 vs 2017-2021):\n")
cat(sprintf("  log_early (2012-2016):        %.4f (robust SE %.4f)\n",
            coef(m_half)["log_early"], rob_se(m_half, "log_early")))
cat(sprintf("  log_early x late-half shift:  %.4f (robust SE %.4f)\n\n",
            coef(m_half)["log_early:late_half"], rob_se(m_half, "log_early:late_half")))

cohort_analysis <- list(
  by_cohort = cohort_results,
  n = nrow(coh),
  trend = list(base = coef(m_trend)["log_early"], base_se = rob_se(m_trend, "log_early"),
               interaction = coef(m_trend)["log_early:cohort_c"],
               interaction_se = rob_se(m_trend, "log_early:cohort_c"), n = m_trend$N),
  half = list(base = coef(m_half)["log_early"], base_se = rob_se(m_half, "log_early"),
              interaction = coef(m_half)["log_early:late_half"],
              interaction_se = rob_se(m_half, "log_early:late_half")))

# CSV export for the appendix table
fwrite(cohort_results, file.path(TAB_DIR, "TableB1_CohortElasticities.csv"))
cat("Saved: TableB1_CohortElasticities.csv\n")

# =============================================================================
# Save
# =============================================================================

saveRDS(list(topic_results = topic_results,
             topic_fs_results = topic_fs_results,
             overall_coef = overall_coef, overall_se = overall_se,
             heterogeneity_extra = heterogeneity_extra,
             cohort = cohort_analysis),
        file.path(RESULTS_DIR, "res_07_heterogeneity.rds"))
cat("Saved: results/res_07_heterogeneity.rds\n")

close_log()

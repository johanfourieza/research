# =============================================================================
# 02_main_results.R -- summary statistics and main regressions
# -----------------------------------------------------------------------------
# Produces:
#   Table 1 (summary statistics)        -> output/tables/Table1_SummaryStats.tex
#   Table 2 (main regressions, M1-M5)   -> output/tables/Table2_MainResults.tex
#   Compact results object              -> results/res_02_main.rds
#
# M1: bivariate OLS. M2: + article controls. M3: + journal and year FE
# (preferred specification). M4: + topic FE (excluding 'other', as in the
# submitted version; the variant keeping 'other' as a level is in script 03).
# M5: binary fast-starter treatment instead of the continuous measure.
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("02_main_results")

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
est <- ad$est

# =============================================================================
# 1. Main regressions (heteroskedasticity-robust SEs)
# =============================================================================

m1 <- felm(log_longrun ~ log_early, data = est)

m2 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no, data = est)

m3 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year, data = est)

m4 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year + topic,
           data = est[topic != "other"])

m5 <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year + topic,
           data = est[topic != "other" & !is.na(fast_starter)])

for (spec in list(list("M1 (OLS)", m1, "log_early"),
                  list("M2 (+ controls)", m2, "log_early"),
                  list("M3 (+ journal/year FE)", m3, "log_early"),
                  list("M4 (+ topic FE)", m4, "log_early"),
                  list("M5 (binary fast starter)", m5, "fast_starter"))) {
  cat(sprintf("%-28s %s = %.4f (robust SE %.4f), N = %d\n",
              spec[[1]], spec[[3]], coef(spec[[2]])[spec[[3]]],
              rob_se(spec[[2]], spec[[3]]),
              if (!is.null(spec[[2]]$N)) spec[[2]]$N else nobs(spec[[2]])))
}
cat("\n")

# Table 2 -- robust SEs passed explicitly so the table matches the text
stargazer(m1, m2, m3, m4, m5,
          type = "latex",
          out = file.path(TAB_DIR, "Table2_MainResults.tex"),
          se = list(rob_se_vec(m1), rob_se_vec(m2), rob_se_vec(m3),
                    rob_se_vec(m4), rob_se_vec(m5)),
          title = "Early citations and long-run citations",
          label = "tab:main",
          dep.var.labels = "Log(long-run citations)",
          covariate.labels = c("Log(early citations)", "Fast starter",
                               "N authors", "Top institution",
                               "Log(article length)", "Title length (characters)",
                               "Article position in issue", "Issue number"),
          add.lines = list(c("Journal FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Year FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Topic FE", "No", "No", "No", "Yes", "Yes")),
          omit.stat = c("f", "ser"),
          notes = "Heteroskedasticity-robust standard errors in parentheses.")
cat("Saved: Table2_MainResults.tex\n\n")

# =============================================================================
# 2. Table 1: summary statistics
# =============================================================================

sum_tbl <- data.frame(
  Variable = c("Early citations (age 2)", "Long-run citations", "Citation growth",
               "N authors", "Top institution", "Fast starter"),
  N = c(sum(!is.na(est$cite_early)), sum(!is.na(est$cite_longrun)),
        sum(!is.na(est$cite_growth)), sum(!is.na(est$n_authors)),
        sum(!is.na(est$any_top_inst)), sum(!is.na(est$fast_starter))),
  Mean = round(c(mean(est$cite_early), mean(est$cite_longrun),
                 mean(est$cite_growth, na.rm = TRUE), mean(est$n_authors),
                 mean(est$any_top_inst), mean(est$fast_starter, na.rm = TRUE)), 2),
  SD = round(c(sd(est$cite_early), sd(est$cite_longrun),
               sd(est$cite_growth, na.rm = TRUE), sd(est$n_authors),
               sd(est$any_top_inst), sd(est$fast_starter, na.rm = TRUE)), 2),
  Min = round(c(min(est$cite_early), min(est$cite_longrun),
                min(est$cite_growth, na.rm = TRUE), min(est$n_authors),
                min(est$any_top_inst), min(est$fast_starter, na.rm = TRUE)), 2),
  Max = round(c(max(est$cite_early), max(est$cite_longrun),
                max(est$cite_growth, na.rm = TRUE), max(est$n_authors),
                max(est$any_top_inst), max(est$fast_starter, na.rm = TRUE)), 2)
)

stargazer(sum_tbl, type = "latex", summary = FALSE, rownames = FALSE,
          out = file.path(TAB_DIR, "Table1_SummaryStats.tex"),
          title = "Summary Statistics",
          label = "tab:sumstats",
          notes = paste0("Estimation sample: ", nrow(est),
                         " papers from the four core economic history journals."))
cat("Saved: Table1_SummaryStats.tex\n\n")

# =============================================================================
# 3. Save compact results (figures and later scripts use numbers, not models)
# =============================================================================

main_models <- data.table(
  model = c("M1", "M2", "M3", "M4", "M5"),
  variable = c(rep("log_early", 4), "fast_starter"),
  coef = c(coef(m1)["log_early"], coef(m2)["log_early"], coef(m3)["log_early"],
           coef(m4)["log_early"], coef(m5)["fast_starter"]),
  se_robust = c(rob_se(m1, "log_early"), rob_se(m2, "log_early"),
                rob_se(m3, "log_early"), rob_se(m4, "log_early"),
                rob_se(m5, "fast_starter")),
  se_classical = c(summary(m1)$coefficients["log_early", "Std. Error"],
                   summary(m2)$coefficients["log_early", "Std. Error"],
                   summary(m3)$coefficients["log_early", "Std. Error"],
                   summary(m4)$coefficients["log_early", "Std. Error"],
                   summary(m5)$coefficients["fast_starter", "Std. Error"]),
  N = c(nobs(m1), nobs(m2), m3$N, m4$N, m5$N),
  r2 = c(summary(m1)$r2, summary(m2)$r2, summary(m3)$r2,
         summary(m4)$r2, summary(m5)$r2)
)
print(main_models)

saveRDS(list(main_models = main_models, sum_tbl = sum_tbl),
        file.path(RESULTS_DIR, "res_02_main.rds"))
cat("\nSaved: results/res_02_main.rds\n")

close_log()

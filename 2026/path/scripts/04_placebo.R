# =============================================================================
# 04_placebo.R -- permutation test for the fast-starter coefficient
# -----------------------------------------------------------------------------
# Reshuffles the fast-starter label within journal-year cells 1,000 times and
# compares the distribution of placebo coefficients with the true estimate.
# (The corresponding conference placebo runs in 05_conference.R, after the
# conference variables exist.)
#
# Output: results/res_04_placebo.rds
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("04_placebo")
set.seed(SEED_PLACEBO)

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
est <- ad$est

N_PERMUTATIONS <- 1000

cat("Placebo fast-starter (", N_PERMUTATIONS, "permutations)...\n")

placebo_data <- est[!is.na(fast_starter)]
true_model <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                     log_article_length + title_nchar + article_position + issue_no |
                     journal + year, data = placebo_data)
true_coef_fs <- coef(true_model)["fast_starter"]
cat("True fast_starter coefficient:", round(true_coef_fs, 4), "\n")

placebo_coefs_fs <- numeric(N_PERMUTATIONS)

for (p in seq_len(N_PERMUTATIONS)) {
  if (p %% 100 == 0) cat("  Permutation", p, "/", N_PERMUTATIONS, "\n")

  perm_data <- copy(placebo_data)
  perm_data[, fast_starter := sample(fast_starter), by = .(journal, year)]

  m_perm <- tryCatch({
    felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no |
           journal + year, data = perm_data)
  }, error = function(e) NULL)

  placebo_coefs_fs[p] <- if (!is.null(m_perm)) coef(m_perm)["fast_starter"] else NA_real_
}

placebo_coefs_fs <- placebo_coefs_fs[!is.na(placebo_coefs_fs)]
emp_p_fs <- mean(placebo_coefs_fs >= true_coef_fs)

cat("\nEmpirical p-value:", formatC(emp_p_fs, format = "f", digits = 4), "\n")
cat("  Mean placebo:", round(mean(placebo_coefs_fs), 4), "\n")
cat("  SD placebo:", round(sd(placebo_coefs_fs), 4), "\n")
cat("  True / SD:", round(true_coef_fs / sd(placebo_coefs_fs), 2), "standard deviations\n\n")

saveRDS(list(placebo_coefs_fs = placebo_coefs_fs,
             true_coef_fs = true_coef_fs,
             emp_p_fs = emp_p_fs,
             n_permutations = N_PERMUTATIONS),
        file.path(RESULTS_DIR, "res_04_placebo.rds"))
cat("Saved: results/res_04_placebo.rds\n")

close_log()

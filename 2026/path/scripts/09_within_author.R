# =============================================================================
# 09_within_author.R -- within-author panel and reverse-causality test
# -----------------------------------------------------------------------------
# 9.1 Paper-year panel from the annual citation snapshots (g14-g26).
# 9.2 Within-author regressions: new citations on conference presentation and
#     the lagged citation stock, with author, calendar-year and PAPER-AGE fixed
#     effects (paper-age FE absorb the citation lifecycle), SEs clustered by
#     paper. Design B tightens to author-year fixed effects.
# 9.3 Reverse causality: probit of conference presentation on long-run
#     citations, plus a presenter/non-presenter balance test.
#
# Outputs: output/tables/Table4_WithinAuthor.tex; results/res_09_panel.rds
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("09_within_author")

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
jn  <- ad$jn
est <- ad$est

flags_file <- file.path(RESULTS_DIR, "conference_flags.rds")
if (file.exists(flags_file)) {
  flags <- readRDS(flags_file)
  jn  <- merge(jn,  flags[, .(id, presented_at_conference)], by = "id", all.x = TRUE)
  est <- merge(est, flags, by = "id", all.x = TRUE)
  jn[is.na(presented_at_conference), presented_at_conference := 0L]
  est[is.na(presented_at_conference), presented_at_conference := 0L]
} else {
  jn[, presented_at_conference := 0L]
  est[, presented_at_conference := 0L]
  cat("NOTE: conference_flags.rds not found -- conference indicator set to 0.\n\n")
}

# =============================================================================
# 9.1 Build paper-year panel from citation snapshots
# =============================================================================
cat("Building paper-year panel...\n")

citation_cols <- paste0("g", 14:26)

panel_id_vars <- c("id", "year", "journal", "title", "n_authors",
                   "any_top_inst", "topic", "region", "author1", "fast_starter",
                   "presented_at_conference")

panel_measure_vars <- intersect(citation_cols, names(jn))

panel <- melt(jn[is_core == TRUE],
              id.vars = intersect(panel_id_vars, names(jn)),
              measure.vars = panel_measure_vars,
              variable.name = "cite_year_var",
              value.name = "cit_cum")

panel[, cite_year := as.integer(paste0("20", gsub("g", "", cite_year_var)))]
panel[, paper_age := cite_year - year]
panel <- panel[paper_age >= 0]

setorder(panel, id, cite_year)
panel[, cit_new := cit_cum - shift(cit_cum, 1, type = "lag"), by = id]
panel[paper_age == 0, cit_new := cit_cum]
panel[, cit_cum_lag1 := shift(cit_cum, 1, type = "lag"), by = id]

panel[, log_cit_new := log(cit_new + 1)]
panel[, log_cit_cum := log(cit_cum + 1)]
panel[, log_cit_cum_lag1 := log(cit_cum_lag1 + 1)]

cat("Panel dimensions:", nrow(panel), "paper-year observations\n")
cat("Unique papers:", uniqueN(panel$id), "\n")
cat("Years covered:", min(panel$cite_year), "-", max(panel$cite_year), "\n\n")

# =============================================================================
# 9.2 Within-author fixed-effects regressions
# =============================================================================
cat("Running within-author FE regression...\n")

panel_author <- panel[cit_new >= 0 & !is.na(author1) & author1 != ""]

author_variety <- panel_author[, .(
  has_presented = any(presented_at_conference == 1, na.rm = TRUE),
  has_not_presented = any(presented_at_conference == 0, na.rm = TRUE)
), by = author1]
n_both <- sum(author_variety$has_presented & author_variety$has_not_presented)

cat("Authors with variation in conference presentation:", n_both, "\n\n")

within_author <- NULL
within_author_yr <- NULL

if (n_both >= 20) {

  # With a cluster in the felm formula, column 2 of the summary is the
  # cluster-robust SE.
  se2 <- function(m, var) {
    ct <- summary(m)$coefficients
    if (!var %in% rownames(ct)) return(NA_real_)
    ct[var, 2]
  }

  cat("Model WA1: Author + paper-age fixed effects (SEs clustered by paper)\n")
  within_author <- felm(log_cit_new ~ presented_at_conference + log_cit_cum_lag1 +
                          n_authors | author1 + cite_year + paper_age | 0 | id,
                        data = panel_author)

  cat("  presented_at_conference:", round(coef(within_author)["presented_at_conference"], 4),
      "(cluster SE:", round(se2(within_author, "presented_at_conference"), 4), ")\n")
  cat("  log_cit_cum_lag1 (reinforcement):", round(coef(within_author)["log_cit_cum_lag1"], 4),
      "(cluster SE:", round(se2(within_author, "log_cit_cum_lag1"), 4), ")\n\n")

  cat("Model WA2: Author-year fixed effects (stricter)\n")
  panel_author[, author_year := paste0(author1, "_", year)]

  ay_variety <- panel_author[, .(
    has_presented = any(presented_at_conference == 1, na.rm = TRUE),
    has_not_presented = any(presented_at_conference == 0, na.rm = TRUE)
  ), by = author_year]
  n_ay_both <- sum(ay_variety$has_presented & ay_variety$has_not_presented)
  cat("  Author-years with variation:", n_ay_both, "\n")

  if (n_ay_both >= 10) {
    within_author_yr <- felm(log_cit_new ~ presented_at_conference + log_cit_cum_lag1 |
                               author_year + cite_year + paper_age | 0 | id,
                             data = panel_author)

    if ("presented_at_conference" %in% names(coef(within_author_yr))) {
      cat("  presented_at_conference:", round(coef(within_author_yr)["presented_at_conference"], 4),
          "(cluster SE:", round(se2(within_author_yr, "presented_at_conference"), 4), ")\n\n")
    } else {
      cat("  Insufficient within-author-year variation\n\n")
    }
  } else {
    cat("  Insufficient author-year variation for strict FE\n\n")
  }

  # Table 4
  tryCatch({
    model_list <- if (!is.null(within_author_yr)) {
      list(within_author, within_author_yr)
    } else {
      list(within_author)
    }
    # float = FALSE writes only the tabular; the manuscript wraps it in its own
    # table environment with caption, label and a notes paragraph (long notes
    # inside the tabular stretch the columns).
    fe_lines <- if (length(model_list) == 2) {
      list(c("Author FE", "Yes", "No"),
           c("Author-by-year FE", "No", "Yes"),
           c("Calendar-year FE", "Yes", "Yes"),
           c("Article-age FE", "Yes", "Yes"))
    } else {
      list(c("Author FE", "Yes"), c("Calendar-year FE", "Yes"),
           c("Article-age FE", "Yes"))
    }
    stargazer(model_list,
              type = "latex",
              out = file.path(TAB_DIR, "Table4_WithinAuthor.tex"),
              float = FALSE,
              dep.var.labels = "Log(new citations)",
              covariate.labels = c("Presented at conference",
                                   "Log(lagged citation stock)",
                                   "Number of authors"),
              add.lines = fe_lines,
              omit.stat = c("f", "ser"))
    cat("Saved: Table4_WithinAuthor.tex\n\n")
  }, error = function(e) {
    cat("Could not save Table4:", e$message, "\n\n")
  })

} else {
  cat("Insufficient author variation for within-author analysis.\n\n")
}

# =============================================================================
# 9.3 Reverse causality
# =============================================================================
cat("--- Reverse causality test ---\n\n")

rc1_out <- NULL; rc2_out <- NULL; balance_out <- NULL
if (sum(!is.na(est$presented_at_conference)) >= 30) {

  rc_data <- est[!is.na(presented_at_conference) & !is.na(log_longrun) &
                   !is.na(log_early) & !is.na(n_authors) & !is.na(any_top_inst)]
  cat("Reverse causality sample:", nrow(rc_data), "\n\n")

  rc1 <- glm(presented_at_conference ~ log_longrun + log_early + n_authors +
               any_top_inst + factor(journal) + factor(year),
             data = rc_data, family = binomial(link = "probit"))
  rc1_coefs <- summary(rc1)$coefficients

  cat("RC1: log_longrun -> presented_at_conference (probit)\n")
  cat("  log_longrun:", round(rc1_coefs["log_longrun", "Estimate"], 4),
      "(SE:", round(rc1_coefs["log_longrun", "Std. Error"], 4),
      ", p:", formatC(rc1_coefs["log_longrun", "Pr(>|z|)"], format = "f", digits = 4), ")\n")
  cat("  log_early:", round(rc1_coefs["log_early", "Estimate"], 4),
      "(SE:", round(rc1_coefs["log_early", "Std. Error"], 4),
      ", p:", formatC(rc1_coefs["log_early", "Pr(>|z|)"], format = "f", digits = 4), ")\n\n")
  rc1_out <- rc1_coefs[c("log_longrun", "log_early"), c("Estimate", "Std. Error", "Pr(>|z|)")]

  rc_data_fs <- rc_data[!is.na(fast_starter)]
  if (nrow(rc_data_fs) >= 30) {
    rc2 <- glm(presented_at_conference ~ fast_starter + n_authors +
                 any_top_inst + factor(journal) + factor(year),
               data = rc_data_fs, family = binomial(link = "probit"))
    rc2_coefs <- summary(rc2)$coefficients
    cat("RC2: fast_starter -> presented_at_conference (probit)\n")
    cat("  fast_starter:", round(rc2_coefs["fast_starter", "Estimate"], 4),
        "(SE:", round(rc2_coefs["fast_starter", "Std. Error"], 4),
        ", p:", formatC(rc2_coefs["fast_starter", "Pr(>|z|)"], format = "f", digits = 4), ")\n\n")
    rc2_out <- rc2_coefs["fast_starter", c("Estimate", "Std. Error", "Pr(>|z|)")]
  }

  cat("=== BALANCE: PRESENTERS vs NON-PRESENTERS ===\n\n")
  balance_out <- rbindlist(lapply(c("n_authors", "any_top_inst", "cite_early",
                                    "log_early", "fast_starter"), function(v) {
    conf_vals <- rc_data[presented_at_conference == 1][[v]]
    nonc_vals <- rc_data[presented_at_conference == 0][[v]]
    tt <- tryCatch(t.test(conf_vals, nonc_vals), error = function(e) NULL)
    if (is.null(tt)) return(NULL)
    cat(sprintf("  %-15s  Presenters: %6.3f  Others: %6.3f  diff: %6.3f  p: %s\n",
                v, mean(conf_vals, na.rm = TRUE), mean(nonc_vals, na.rm = TRUE),
                mean(conf_vals, na.rm = TRUE) - mean(nonc_vals, na.rm = TRUE),
                formatC(tt$p.value, format = "f", digits = 4)))
    data.table(variable = v,
               mean_presenters = mean(conf_vals, na.rm = TRUE),
               mean_others = mean(nonc_vals, na.rm = TRUE),
               p = tt$p.value)
  }))
  cat("\n")
}

# =============================================================================
# Save
# =============================================================================

se2 <- function(m, var) {
  if (is.null(m)) return(NA_real_)
  ct <- summary(m)$coefficients
  if (!var %in% rownames(ct)) return(NA_real_)
  ct[var, 2]
}
res_09 <- list(
  panel_n = nrow(panel), panel_papers = uniqueN(panel$id),
  n_authors_with_variation = n_both,
  wa1 = if (!is.null(within_author)) list(
    conf = coef(within_author)["presented_at_conference"],
    conf_se = se2(within_author, "presented_at_conference"),
    reinf = coef(within_author)["log_cit_cum_lag1"],
    reinf_se = se2(within_author, "log_cit_cum_lag1"),
    n = within_author$N) else NULL,
  wa2 = if (!is.null(within_author_yr)) list(
    conf = coef(within_author_yr)["presented_at_conference"],
    conf_se = se2(within_author_yr, "presented_at_conference"),
    reinf = coef(within_author_yr)["log_cit_cum_lag1"],
    reinf_se = se2(within_author_yr, "log_cit_cum_lag1"),
    n = within_author_yr$N) else NULL,
  rc1 = rc1_out, rc2 = rc2_out, balance = balance_out
)
saveRDS(res_09, file.path(RESULTS_DIR, "res_09_panel.rds"))
cat("Saved: results/res_09_panel.rds\n")

close_log()

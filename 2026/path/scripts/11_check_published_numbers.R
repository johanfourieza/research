# =============================================================================
# 11_check_published_numbers.R -- does the run reproduce the published article?
# -----------------------------------------------------------------------------
# Reads the results objects written by scripts 01-10 and compares them with
# the statistics printed in
#   Fourie, J. (2026) "Testing for path dependence in economic history
#   publications", Cliometrica, https://doi.org/10.1007/s11698-026-00346-w
# at the precision at which the article reports them. Stops with an error on
# the first discrepancy, so a passing run_all.R is a certificate that the
# shipped code and data regenerate the published numbers.
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("11_check_published_numbers")

res03 <- readRDS(file.path(RESULTS_DIR, "res_03_robustness.rds"))
res05 <- readRDS(file.path(RESULTS_DIR, "res_05_conference.rds"))
res06 <- readRDS(file.path(RESULTS_DIR, "res_06_mechanisms.rds"))
res09 <- readRDS(file.path(RESULTS_DIR, "res_09_panel.rds"))
flags <- readRDS(file.path(RESULTS_DIR, "conference_flags.rds"))
ad    <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
ledger <- fread(file.path(DATA_RAW, "conference_match_ledger.csv"), encoding = "UTF-8")

n_fail <- 0L
check <- function(label, got, want, digits = NULL, where = "") {
  ok <- if (is.null(digits)) isTRUE(all.equal(unname(got), want, tolerance = 0)) ||
                                identical(as.numeric(unname(got)), as.numeric(want)) else
        isTRUE(all(round(unname(got), digits) == want))
  shown <- if (is.null(digits)) format(unname(got)) else format(round(unname(got), digits), nsmall = digits)
  cat(sprintf("  [%s] %-58s published %-12s run %s\n", if (ok) "ok" else "FAIL", label,
              paste(format(want), collapse = "/"), paste(shown, collapse = "/")))
  if (!ok) n_fail <<- n_fail + 1L
  invisible(ok)
}

cat("Headline persistence estimate (Section 4.2, Table 2 column 3)\n")
check("elasticity of long-run on early citations", res03$m3$coef, 0.784, 3)
check("robust SE",                                   res03$m3$se,   0.020, 3)

cat("\nConference linkage (Section 3.3, 6.3 and Appendix E)\n")
check("ledger candidates",                      nrow(ledger), 317L)
check("ledger retained",                        ledger[decision == "retain", .N], 278L)
check("ledger rejected",                        ledger[decision == "reject", .N], 20L)
check("ledger uncertain",                       ledger[decision == "uncertain", .N], 19L)
check("retained programme links used",          res05$n_matched_entries, 278L)
check("distinct linked articles",               res05$n_papers_any_conf, 259L)
check("linked via EHA",                         res05$conference_papers$eha, 81L)
check("linked via EHS",                         res05$conference_papers$ehs, 195L)
check("linked at both",                         res05$conference_papers$both, 17L)
check("estimation-sample presenters",           res05$n_presenters_est, 137L)
check("flags agree with retained ledger ids",
      setequal(flags[presented_at_conference == 1, id], ledger[decision == "retain", matched_id]), TRUE)
check("conditional coefficient",                res05$c1$coef, 0.066, 3)
check("conditional robust SE",                  res05$c1$se,   0.043, 3)
check("one-sided permutation p",                res05$placebo$emp_p, 0.075, 3)
check("two-sided permutation p",                res05$placebo$emp_p_two_sided, 0.144, 3)
check("permutation draws",                      length(res05$placebo$coefs), 1000L)
check("coefficient without early-citation control", res05$c1_unconditional$coef, 0.231, 3)
check("its robust SE",                          res05$c1_unconditional$se, 0.072, 3)
ci <- res05$c1$coef + c(-1.96, 1.96) * res05$c1$se
check("95% interval lower bound",               ci[1], -0.018, 3)
check("95% interval upper bound",               ci[2],  0.150, 3)
check("upper bound in 1 + citations (%)",       100 * expm1(ci[2]), 16, 0)
check("EHA pre-lunch presenters",               res05$timing$n_pre, 21L)
check("EHA post-lunch presenters",              res05$timing$n_post, 5L)
sens <- res05$sensitivity
check("sensitivity presenters (4 scenarios)",   sens$presenters, c(137L, 143L, 93L, 78L))
check("sensitivity coefficients range .012-.066", range(round(sens$coef, 3)), c(0.012, 0.066), 3)

cat("\nWithin-author panel (Appendix D, Table 8)\n")
check("column 1 conference coefficient",        res09$wa1$conf, 0.051, 3)
check("column 1 cluster SE",                    res09$wa1$conf_se, 0.027, 3)
check("column 1 lagged-stock coefficient",      res09$wa1$reinf, 0.549, 3)
check("column 1 lagged-stock SE",               res09$wa1$reinf_se, 0.012, 3)
check("column 1 observations",                  res09$wa1$n, 26999L)
check("column 2 conference coefficient",        res09$wa2$conf, 0.332, 3)
check("column 2 cluster SE",                    res09$wa2$conf_se, 0.133, 3)
check("column 2 lagged-stock coefficient",      res09$wa2$reinf, 0.100, 3)
check("column 2 observations",                  res09$wa2$n, 27000L)
check("first-author-by-year groups with variation", res09$n_author_years_with_variation, 25L)
check("same, among estimated observations",     res09$n_author_years_estimated, 25L)

cat("\nSelection and probits (Appendix E)\n")
b <- res09$balance
check("early citations, presenters",            b[variable == "cite_early", mean_presenters], 9.55, 2)
check("early citations, others",                b[variable == "cite_early", mean_others], 6.82, 2)
check("early citations, p",                     b[variable == "cite_early", p], 0.009, 3)
check("top institution, presenters",            b[variable == "any_top_inst", mean_presenters], 0.33, 2)
check("top institution, others",                b[variable == "any_top_inst", mean_others], 0.22, 2)
check("top institution, p",                     b[variable == "any_top_inst", p], 0.008, 3)
check("fast starters, presenters",              b[variable == "fast_starter", mean_presenters], 0.36, 2)
check("fast starters, others",                  b[variable == "fast_starter", mean_others], 0.24, 2)
check("fast starters, p",                       b[variable == "fast_starter", p], 0.005, 3)
check("author counts, presenters",              b[variable == "n_authors", mean_presenters], 1.85, 2)
check("author counts, others",                  b[variable == "n_authors", mean_others], 1.78, 2)
check("author counts, p",                       b[variable == "n_authors", p], 0.341, 3)
check("probit fast starter -> matched, coef",   res09$rc2["Estimate"], 0.305, 3)
check("probit fast starter -> matched, SE",     res09$rc2["Std. Error"], 0.104, 3)
check("probit long-run -> matched, coef",       res09$rc1["log_longrun", "Estimate"], 0.207, 3)
check("probit long-run -> matched, SE",         res09$rc1["log_longrun", "Std. Error"], 0.098, 3)
check("probit long-run -> matched, p",          res09$rc1["log_longrun", "Pr(>|z|)"], 0.034, 3)

cat("\nCitation network after the metadata screen (Section 6.2, Appendix A.3, Fig. 3)\n")
nw <- res06$network
check("links in frozen cache",                  nw$n_links_cache, 72695L)
check("links after screen",                     nw$n_links_screened, 72465L)
check("links removed by screen",                nw$n_links_cache - nw$n_links_screened, 230L)
check("corpus articles covered",                nw$n_source_articles, 1610L)
check("citing works",                           nw$n_citing_works, 37767L)
check("links with primary discipline",          nw$n_primary_field, 72167L)
check("links with root tags",                   nw$n_root_tags, 72333L)
check("links with document type",               nw$n_typed, 72465L)
check("within-field share of root-tagged links (%)", 100 * res06$within_share, 78, 0)
check("non-article share of typed links (%)",   100 * nw$nonarticle_share, 41, 0)
check("estimation articles with classified citers", res06$s1a$n, 593L)
check("within-field count elasticity",          res06$s1a$coef, 0.70, 2)
check("cross-field count elasticity",           res06$s1b$coef, 0.51, 2)
dd <- fread(file.path(TAB_DIR, "Fig3_discipline_data.csv"))
ff <- fread(file.path(TAB_DIR, "Fig3_document_type_data.csv"))
check("Fig. 3(a) classified links",             sum(dd$N), 72167L)
check("Fig. 3(a) economics share (%)",          dd[disc == "Economics", pct], 50, 0)
check("Fig. 3(a) other social sciences (%)",    dd[disc == "Other social sciences", pct], 30, 0)
check("Fig. 3(b) typed links",                  sum(ff$N), 72465L)
check("Fig. 3(b) journal articles (%)",         ff[fmt == "Journal article", pct], 59, 0)
check("Fig. 3(b) books and chapters (%)",       ff[fmt == "Book or chapter", pct], 28, 0)
check("Fig. 3(b) preprints and working papers (%)", ff[fmt == "Preprint / working paper", pct], 8, 0)

cat("\nUnchanged network-position and self-citation estimates (Section 6.4)\n")
check("self-citation rate (%)",                 100 * res06$selfcite$rate, 16, 0)
check("cascade depth coefficient",              res06$cascade$cd1$coef, 0.19, 2)
check("cascade depth SE",                       res06$cascade$cd1$se, 0.14, 2)
check("indirect citers coefficient",            res06$cascade$cd2$coef, 0.15, 2)
check("indirect citers SE",                     res06$cascade$cd2$se, 0.09, 2)
check("Herfindahl coefficient",                 res06$hhi$coef, 0.011, 3)
check("Herfindahl SE",                          res06$hhi$se, 0.013, 3)

cat("\nSample\n")
check("estimation sample size",                 nrow(ad$est), 1262L)
check("corpus size",                            nrow(ad$jn), 3250L)

cat("\n")
if (n_fail > 0) {
  close_log()
  stop(n_fail, " published statistic(s) not reproduced; see output/logs/11_check_published_numbers.log")
}
cat("ALL CHECKS PASSED: the run reproduces the statistics reported in the published article.\n")
close_log()

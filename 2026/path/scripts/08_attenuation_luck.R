# =============================================================================
# 08_attenuation_luck.R -- fast-starter attenuation; unpredictability and
#                          amplification; within-issue position checks
# -----------------------------------------------------------------------------
# 8.1 Fast-starter attenuation: the raw threshold premium collapses once the
#     continuous early-citation level and author quality are controlled.
# 8.2 Predictability of early citations (R-squared ladder).
# 8.3 Decomposition: early citations split into the component explained by
#     fundamentals and the residual; both regressed on long-run citations.
#     NEW in this revision: paired-bootstrap standard errors that account for
#     the generated regressor (the residual is estimated in a first step).
# 8.4 Candidate visibility measures (first stages) and the within-issue
#     position REDUCED FORMS. No instrumental-variable estimate is computed:
#     the balance tests below reject conditional random assignment (the joint
#     negative-control test) and the exclusion check finds a direct effect of
#     position on long-run citations conditional on early citations, so a
#     position IV is not identified. The paper reports reduced forms only.
# 8.5 Position design checks: negative-control-outcome battery (Danieli et al.
#     2026), functional-form check, by-journal reduced forms, exclusion check,
#     and reduced forms under successively tighter fixed effects.
#
# Output: results/res_08_luck.rds; output/tables/Table3_FastStarterAttenuation.csv
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("08_attenuation_luck")
set.seed(SEED_LUCK)

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
jn  <- ad$jn
est <- ad$est

# =============================================================================
# 8.1 Fast-starter attenuation
# =============================================================================
# A double-machine-learning version was explored in an earlier draft and is NOT
# used: RePEc author quality is missing for ~42% of the sample and OpenAlex
# topic data have no coverage on 2012-2021 papers, so the required imputation
# destabilises the nuisance estimates. The transparent OLS sequence is reported.

cat("--- 8.1 Fast-starter attenuation (OLS) ---\n\n")

fs_raw <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = est)
fs_early <- felm(log_longrun ~ fast_starter + log_early + n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est)
fs_quality <- felm(log_longrun ~ fast_starter + log_early + n_authors + any_top_inst +
                     log_article_length + title_nchar + article_position + issue_no +
                     team_max_seniority + team_max_hindex + paper_won_prize +
                     author_won_dissertation_prize | journal + year, data = est)

attenuation <- data.table(
  spec = c("Raw (no early-cite control)",
           "+ control for log(early citations)",
           "+ author quality (seniority, h-index, prizes)"),
  coef = c(coef(fs_raw)["fast_starter"], coef(fs_early)["fast_starter"],
           coef(fs_quality)["fast_starter"]),
  se   = c(rob_se(fs_raw, "fast_starter"), rob_se(fs_early, "fast_starter"),
           rob_se(fs_quality, "fast_starter")),
  N    = c(fs_raw$N, fs_early$N, fs_quality$N))

cat("Fast-starter threshold attenuation (robust SEs):\n")
print(attenuation)
cat("\n")

fwrite(attenuation, file.path(TAB_DIR, "Table3_FastStarterAttenuation.csv"))
cat("Saved: Table3_FastStarterAttenuation.csv\n\n")

# =============================================================================
# 8.2 Predictability of early citations
# =============================================================================
cat("--- 8.2 Predictability of early citations ---\n\n")

ly <- est$log_early
pred_fe  <- felm(log_early ~ 1 | journal + year + topic, data = est)
pred_obs <- felm(log_early ~ article_position + issue_no + n_authors + any_top_inst +
                   log_article_length + title_nchar | journal + year + topic, data = est)
# Complete-case subsample for the author-quality specification, so that the
# reported R2 (1 - SSR/SST) and N refer to the SAME set of papers. The h-index
# is missing for a few RePEc-matched articles, so restricting to non-missing
# seniority alone (732) would compute SSR over the 724 papers felm actually
# fits but SST over 732 -- an inconsistent R2. The article-quality controls
# require all of the following to be present:
qual_vars <- c("article_position", "issue_no", "n_authors", "any_top_inst",
               "log_article_length", "title_nchar", "team_max_seniority",
               "team_max_hindex", "paper_won_prize", "author_won_dissertation_prize")
sub_q <- est[complete.cases(est[, ..qual_vars])]
pred_qual <- felm(log_early ~ article_position + issue_no + n_authors + any_top_inst +
                    log_article_length + title_nchar + team_max_seniority + team_max_hindex +
                    paper_won_prize + author_won_dissertation_prize | journal + year + topic,
                  data = sub_q)
r2_fe   <- R2manual(pred_fe, ly)
r2_obs  <- R2manual(pred_obs, ly)
r2_qual <- R2manual(pred_qual, sub_q$log_early)
cat("Predictability of log(early citations) -- share of variance explained:\n")
cat(sprintf("  Journal+year+topic FE only      : R2 = %.3f\n", r2_fe))
cat(sprintf("  + article observables           : R2 = %.3f\n", r2_obs))
cat(sprintf("  + author quality (N=%d)        : R2 = %.3f\n", nrow(sub_q), r2_qual))
cat(sprintf("  -> fundamentals explain ~%.0f%% of early-citation variation.\n\n",
            100 * r2_obs))

# =============================================================================
# 8.3 Decomposition: does the unexplained component propagate?
# =============================================================================
cat("--- 8.3 Decomposition of early citations ---\n\n")

est[, early_resid := as.numeric(pred_obs$residuals)]
est[, early_pred  := log_early - early_resid]
decomp <- felm(log_longrun ~ early_pred + early_resid | journal + year, data = est)
sd_resid <- sd(est$early_resid)
resid_1sd_pct <- (exp(coef(decomp)["early_resid"] * sd_resid) - 1) * 100
cat("Propagation of the explained vs unexplained component (long-run outcome):\n")
cat(sprintf("  explained component  : %.3f (robust SE %.3f)\n",
            coef(decomp)["early_pred"], rob_se(decomp, "early_pred")))
cat(sprintf("  unexplained component: %.3f (robust SE %.3f)\n",
            coef(decomp)["early_resid"], rob_se(decomp, "early_resid")))
cat(sprintf("  SD(unexplained) = %.3f -> a 1-SD shock = %.0f%% more long-run citations.\n\n",
            sd_resid, resid_1sd_pct))

# Same decomposition on post-age-2 GROWTH (no cumulative overlap)
decomp_growth <- felm(log_growth ~ early_pred + early_resid | journal + year, data = est)
cat("  [growth outcome] explained  :", round(coef(decomp_growth)["early_pred"], 3),
    "(rse", round(rob_se(decomp_growth, "early_pred"), 3), ")\n")
cat("  [growth outcome] unexplained:", round(coef(decomp_growth)["early_resid"], 3),
    "(rse", round(rob_se(decomp_growth, "early_resid"), 3), ")\n\n")

# --- Paired bootstrap for the two-step decomposition (new in this revision) ---
# The residual regressor is generated in a first step, so the analytic SEs of
# the second step understate uncertainty. Resample papers with replacement,
# re-run BOTH steps on each draw, and report percentile SEs.
cat("Bootstrapping the two-step decomposition (999 replications)...\n")
B <- 999
boot_pred  <- numeric(B); boot_resid <- numeric(B)
boot_gpred <- numeric(B); boot_gresid <- numeric(B)
n_est <- nrow(est)
for (b in seq_len(B)) {
  idx <- sample.int(n_est, n_est, replace = TRUE)
  d <- est[idx]
  p1 <- tryCatch(felm(log_early ~ article_position + issue_no + n_authors + any_top_inst +
                        log_article_length + title_nchar | journal + year + topic, data = d),
                 error = function(e) NULL)
  if (is.null(p1)) { boot_pred[b] <- NA; boot_resid[b] <- NA
                     boot_gpred[b] <- NA; boot_gresid[b] <- NA; next }
  d[, br := as.numeric(p1$residuals)]
  d[, bp := log_early - br]
  d2 <- tryCatch(felm(log_longrun ~ bp + br | journal + year, data = d),
                 error = function(e) NULL)
  d3 <- tryCatch(felm(log_growth ~ bp + br | journal + year, data = d),
                 error = function(e) NULL)
  boot_pred[b]   <- if (!is.null(d2)) coef(d2)["bp"] else NA
  boot_resid[b]  <- if (!is.null(d2)) coef(d2)["br"] else NA
  boot_gpred[b]  <- if (!is.null(d3)) coef(d3)["bp"] else NA
  boot_gresid[b] <- if (!is.null(d3)) coef(d3)["br"] else NA
  if (b %% 200 == 0) cat("  replication", b, "/", B, "\n")
}
bse <- function(x) sd(x, na.rm = TRUE)
cat(sprintf("\n  Bootstrap SEs (long-run):  explained %.3f, unexplained %.3f\n",
            bse(boot_pred), bse(boot_resid)))
cat(sprintf("  Bootstrap SEs (growth)  :  explained %.3f, unexplained %.3f\n\n",
            bse(boot_gpred), bse(boot_gresid)))

# =============================================================================
# 8.4 Candidate visibility measures and position reduced forms
# =============================================================================
cat("--- 8.4 Candidate visibility measures ---\n\n")

isz <- jn[, .(issue_size = .N), by = .(journal, vol, issue_raw)]
est <- merge(est, isz, by = c("journal", "vol", "issue_raw"), all.x = TRUE)
est[, lead_article := as.integer(article_position == 1)]
.surname <- function(s) { w <- strsplit(trimws(tolower(gsub("[^a-z ]", " ", s))), "\\s+")[[1]]
  w <- w[nchar(w) > 0]; if (!length(w)) NA_character_ else tail(w, 1) }
est[, fa_initial := vapply(author1, function(s) { ln <- .surname(s)
  if (is.na(ln) || !nchar(ln)) NA_integer_ else as.integer(utf8ToInt(substr(ln, 1, 1)) - 96L) }, integer(1))]
est[fa_initial < 1 | fa_initial > 26, fa_initial := NA_integer_]
est[, na_author := as.integer(grepl("North America", continent, ignore.case = TRUE))]

cat("First stage of each candidate measure on log(early citations):\n")
cands <- c("article_position", "lead_article", "issue_size", "fa_initial", "na_author")
fs_tab <- rbindlist(lapply(cands, function(v) {
  fs <- tryCatch(felm(as.formula(paste0("log_early ~ ", v,
        " + n_authors + any_top_inst + log_article_length | journal + year")), data = est),
        error = function(e) NULL)
  if (is.null(fs)) return(NULL)
  data.table(measure = v, coef = coef(fs)[v], se = rob_se(fs, v), F = Fstat(fs, v))
}))
print(fs_tab)
fwrite(fs_tab, file.path(TAB_DIR, "TableC1_VisibilityFirstStages.csv"))

cat("\nBalance of article_position on author quality | journal+year:\n")
balance_tab <- rbindlist(lapply(c("any_top_inst", "team_max_seniority", "team_max_hindex"),
  function(q) {
    bb <- felm(as.formula(paste0("article_position ~ ", q, " | journal + year")),
               data = est[!is.na(get(q))])
    cat(sprintf("  article_position ~ %-18s : %.4f (robust SE %.4f)\n",
                q, coef(bb)[q], rob_se(bb, q)))
    data.table(quality = q, coef = coef(bb)[q], se = rob_se(bb, q))
  }))

# Position reduced forms (the quantities the paper reports)
rf  <- felm(log_longrun ~ article_position + n_authors + any_top_inst + log_article_length |
              journal + year, data = est)
fs_pos <- felm(log_early ~ article_position + n_authors + any_top_inst + log_article_length |
                 journal + year, data = est)
cat(sprintf("\n  Position -> early citations : %.4f (rse %.4f), F = %.1f\n",
            coef(fs_pos)["article_position"], rob_se(fs_pos, "article_position"),
            Fstat(fs_pos, "article_position")))
cat(sprintf("  Position -> long-run        : %.4f (rse %.4f)\n\n",
            coef(rf)["article_position"], rob_se(rf, "article_position")))

# =============================================================================
# 8.5 Position design checks (why reduced forms only)
# =============================================================================
cat("--- 8.5 Position design checks ---\n\n")

est[, issue_id := paste(journal, vol, issue_raw, sep = "_")]
est[, regime := paste(journal, year, sep = "_")]   # journal x year

# (a) Reduced forms under successively tighter fixed effects
cat("Position gradients under tighter conditioning:\n")
fe_specs <- list(
  `journal + year`          = "journal + year",
  `journal x year (regime)` = "regime",
  `issue (journal-vol-no)`  = "issue_id")
gradient_ladder <- rbindlist(lapply(names(fe_specs), function(lbl) {
  fe <- fe_specs[[lbl]]
  fsm <- tryCatch(felm(as.formula(paste0(
    "log_early ~ article_position + n_authors + any_top_inst + log_article_length | ", fe)),
    data = est), error = function(e) NULL)
  rfm <- tryCatch(felm(as.formula(paste0(
    "log_longrun ~ article_position + n_authors + any_top_inst + log_article_length | ", fe)),
    data = est), error = function(e) NULL)
  if (is.null(fsm) || is.null(rfm)) return(NULL)
  data.table(conditioning = lbl,
             pos_to_early = coef(fsm)["article_position"],
             early_se = rob_se(fsm, "article_position"),
             pos_to_longrun = coef(rfm)["article_position"],
             longrun_se = rob_se(rfm, "article_position"),
             N = rfm$N)
}))
print(gradient_ladder)
cat("\n")

# (b) Negative-control-outcome battery (Danieli et al. 2026): predetermined
# author-quality proxies regressed on position | journal x year.
ncos <- c("team_max_seniority", "team_max_hindex", "any_top_inst",
          "paper_won_prize", "author_won_dissertation_prize")
cat("NCO tests: predetermined quality proxy on position | journal x year\n")
nco_tab <- rbindlist(lapply(ncos, function(v) {
  m <- tryCatch(felm(as.formula(paste0(v, " ~ article_position | regime")),
                     data = est[!is.na(get(v))]), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  data.table(nco = v, coef = coef(m)["article_position"], se = rob_se(m, "article_position"),
             N = m$N)
}))
print(nco_tab)
fwrite(nco_tab, file.path(TAB_DIR, "TableC2_NCObattery.csv"))

# Joint test: regress position on all NCOs | regime, F-test that all are zero.
joint <- tryCatch({
  m7 <- felm(article_position ~ team_max_seniority + team_max_hindex + any_top_inst +
               paper_won_prize + author_won_dissertation_prize | regime,
             data = est[!is.na(team_max_seniority) & !is.na(team_max_hindex)])
  w <- lfe::waldtest(m7, ~ team_max_seniority + team_max_hindex + any_top_inst +
                       paper_won_prize + author_won_dissertation_prize)
  list(F = unname(w["F"]), p = unname(w["p.F"]), N = m7$N)
}, error = function(e) { cat("  (joint waldtest failed:", conditionMessage(e), ")\n"); NULL })
if (!is.null(joint))
  cat(sprintf("\n  Joint NCO F-test (sorting on observable quality): F = %.2f, p = %.3f, N = %d\n",
              joint$F, joint$p, joint$N))
cat("  -> A rejection means editors place observably stronger papers earlier;\n")
cat("     position is NOT conditionally random, so no IV estimate is reported.\n\n")

# (c) Functional-form check: quadratic-in-position NCO for h-index.
mq <- tryCatch(felm(team_max_hindex ~ article_position + I(article_position^2) | regime,
                    data = est[!is.na(team_max_hindex)]), error = function(e) NULL)
if (!is.null(mq)) cat(sprintf("  h-index ~ position + position^2 | regime: lin %.4f, quad %.5f\n\n",
                              coef(mq)["article_position"], coef(mq)["I(article_position^2)"]))

# (d) By-journal reduced forms: broad-based or driven by one journal?
cat("By-journal first stage (position -> early) and reduced form (position -> long-run):\n")
byj <- rbindlist(lapply(sort(unique(est$journal)), function(j) {
  d <- est[journal == j]
  fs <- tryCatch(felm(log_early ~ article_position + n_authors + any_top_inst + log_article_length | year, data = d), error = function(e) NULL)
  rf <- tryCatch(felm(log_longrun ~ article_position + n_authors + any_top_inst + log_article_length | year, data = d), error = function(e) NULL)
  nco <- tryCatch(felm(team_max_hindex ~ article_position | year, data = d[!is.na(team_max_hindex)]), error = function(e) NULL)
  if (is.null(fs) || is.null(rf)) return(NULL)
  data.table(journal = j, N = nrow(d),
             first_stage = coef(fs)["article_position"], fs_se = rob_se(fs, "article_position"),
             reduced_form = coef(rf)["article_position"], rf_se = rob_se(rf, "article_position"),
             nco_hindex = if (!is.null(nco)) coef(nco)["article_position"] else NA_real_)
}))
print(byj)
fwrite(byj, file.path(TAB_DIR, "TableC3_ByJournal.csv"))
cat("\n")

# (e) Exclusion check: does position predict long-run citations conditional on
# early citations? A non-zero coefficient indicates a direct channel, which is
# the second reason no IV estimate is reported.
excl <- felm(log_longrun ~ article_position + log_early + n_authors + any_top_inst +
               log_article_length | regime, data = est)
cat(sprintf("Exclusion check: position -> long-run | early citations = %.4f (rse %.4f)\n\n",
            coef(excl)["article_position"], rob_se(excl, "article_position")))

# =============================================================================
# Save
# =============================================================================

res_08 <- list(
  attenuation = attenuation,
  r2 = list(fe = r2_fe, obs = r2_obs, qual = r2_qual, n_qual = nrow(sub_q)),
  decomp = list(
    pred = coef(decomp)["early_pred"], pred_se = rob_se(decomp, "early_pred"),
    resid = coef(decomp)["early_resid"], resid_se = rob_se(decomp, "early_resid"),
    sd_resid = sd_resid, resid_1sd_pct = resid_1sd_pct,
    growth_pred = coef(decomp_growth)["early_pred"],
    growth_pred_se = rob_se(decomp_growth, "early_pred"),
    growth_resid = coef(decomp_growth)["early_resid"],
    growth_resid_se = rob_se(decomp_growth, "early_resid"),
    boot = list(B = B,
                pred_se = bse(boot_pred), resid_se = bse(boot_resid),
                growth_pred_se = bse(boot_gpred), growth_resid_se = bse(boot_gresid))),
  first_stages = fs_tab,
  balance = balance_tab,
  position = list(
    fs = coef(fs_pos)["article_position"], fs_se = rob_se(fs_pos, "article_position"),
    fs_F = Fstat(fs_pos, "article_position"),
    rf = coef(rf)["article_position"], rf_se = rob_se(rf, "article_position")),
  gradient_ladder = gradient_ladder,
  nco = nco_tab, joint = joint, by_journal = byj,
  excl = list(coef = coef(excl)["article_position"], se = rob_se(excl, "article_position")),
  est_decomp = est[, .(id, early_pred, early_resid, log_longrun)]  # for Fig 11
)
saveRDS(res_08, file.path(RESULTS_DIR, "res_08_luck.rds"))
cat("Saved: results/res_08_luck.rds\n")

close_log()

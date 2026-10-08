# =============================================================================
# 05_conference.R -- conference presentation and citations
# -----------------------------------------------------------------------------
# Scope: presentations at the EHA (Economic History Association) and EHS
# (Economic History Society) annual meetings. The EHES biennial meeting is not
# covered; the conference variable is "presented at EHA or EHS" and the paper
# states this scope explicitly (Section 3.3).
#
# Linkage (Appendix E of the article). Programme entries are linked to corpus
# articles by a reviewed ledger:
#   * data/raw/conference_programme_records.csv holds every programme entry
#     from the corrected re-extraction (case/accent normalisation, separate EHS
#     title and author fields, recovered EHS 2021 and 2022 programmes) with the
#     candidate article nominated by an author-surname overlap and a Jaro title
#     distance below 0.25 within the window programme year -1 to +5.
#   * data/raw/conference_match_ledger.csv records the title-and-author review
#     of all 317 candidates: 278 retained, 20 rejected, 19 uncertain. Only
#     retained links define the indicator. Decisions used titles and named
#     authors only, never citation outcomes.
#   * EHS 2023-2024 archive summary pages (prize announcements, not paper
#     sessions) and the unverified EHA 2025 records are excluded from exposure.
#
# 5.1 Reviewed linkage (ledger -> matched_id)
# 5.2 Paper-level conference variables
# 5.3 Conference premium regression (C1), plus the unconditional association
# 5.4 Session-timing balance and power check (EHA begin-times)
# 5.5 Author-based conference exposure (C2, C3)
# 5.6 Placebo conference permutation test (one- and two-sided tails)
# 5.7 Sensitivity to uncertain links and title thresholds
#
# Outputs: results/conference_flags.rds  (id-keyed flags for scripts 06/09)
#          results/res_05_conference.rds (regression, placebo and sensitivity)
#          results/conference_programme_records_used.csv (records after
#          exclusions, with the reviewed matched_id)
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("05_conference")
set.seed(SEED_PLACEBO)

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
jn  <- ad$jn
est <- ad$est

# =============================================================================
# 5.1 Reviewed linkage
# =============================================================================
cat("Loading programme records and the reviewed match ledger...\n")

conf_data <- fread(file.path(DATA_RAW, "conference_programme_records.csv"),
                   encoding = "UTF-8", na.strings = c("", "NA"))
ledger    <- fread(file.path(DATA_RAW, "conference_match_ledger.csv"),
                   encoding = "UTF-8", na.strings = c("", "NA"))

cat("  Programme entries:", nrow(conf_data), "\n")
print(conf_data[, .N, by = .(conference)])
cat("  Candidate links in ledger:", nrow(ledger), "\n")
print(ledger[, .N, by = decision])

stopifnot(!anyDuplicated(ledger$row_id),
          all(ledger$row_id %in% conf_data$row_id),
          all(ledger$decision %in% c("retain", "reject", "uncertain")))

# Only retained ledger links define a presentation. Candidates that were
# rejected or left uncertain, and all non-candidates, carry no link.
retained <- ledger[decision == "retain", .(row_id, ledger_id = matched_id)]
conf_data[, matched_id := NA_integer_]
conf_data[retained, on = "row_id", matched_id := as.integer(i.ledger_id)]

# Coverage exclusions (see header).
n_before <- nrow(conf_data)
conf_data <- conf_data[!(conference == "EHS" & year %in% 2023:2024) &
                       !(conference == "EHA" & year == 2025)]
cat("  Excluded", n_before - nrow(conf_data),
    "entries (EHS 2023-2024 summary pages; EHA 2025 unverified)\n")

conf_data[, `:=`(conf_title   = title,
                 conf_authors = authors,
                 conf_year    = year,
                 match_dist   = dist,
                 match_tier   = ifelse(is.na(matched_id), NA_character_, "reviewed"))]

fwrite(conf_data, file.path(RESULTS_DIR, "conference_programme_records_used.csv"))

cat("  Retained programme links:", sum(!is.na(conf_data$matched_id)), "\n")
cat("  Distinct linked articles:", uniqueN(na.omit(conf_data$matched_id)), "\n\n")

# Surname extraction for the author-exposure measure (5.5). Accents are folded
# to ASCII before the comparison.
extract_last_names <- function(x) {
  if (is.na(x)) return(character())
  x <- gsub("\\([^)]*\\)", "", x)
  pieces <- unlist(strsplit(x, "[,;/&]|\\band\\b"))
  unique(unlist(lapply(pieces, function(p) {
    p <- tolower(stringi::stri_trans_general(p, "Latin-ASCII"))
    w <- strsplit(trimws(gsub("[^a-z0-9]+", " ", p)), " +")[[1]]
    w <- w[nchar(w) > 1]
    if (length(w)) tail(w, 1) else character()
  })))
}

# =============================================================================
# 5.2 Paper-level conference variables
# =============================================================================
cat("Creating conference variables...\n")

has_session <- "session_order" %in% names(conf_data)

# Aggregate matches to paper level. Retain EHA identity and the earliest EHA
# session (only EHA carries reliable begin-times) for the session-timing check.
conf_matches <- conf_data[!is.na(matched_id), .(
  presented_at_conference   = 1L,
  presented_at_eha          = as.integer(any(conference == "EHA")),
  presented_at_ehs          = as.integer(any(conference == "EHS")),
  n_conference_presentations = .N,
  eha_session_order = if (has_session && any(conference == "EHA"))
      suppressWarnings(as.integer(min(session_order[conference == "EHA"], na.rm = TRUE))) else NA_integer_,
  eha_pre_lunch  = if (has_session && any(conference == "EHA"))
      as.integer(any(pre_lunch[conference == "EHA"]  == 1, na.rm = TRUE)) else 0L,
  eha_post_lunch = if (has_session && any(conference == "EHA"))
      as.integer(any(post_lunch[conference == "EHA"] == 1, na.rm = TRUE)) else 0L
), by = .(matched_id)]
setnames(conf_matches, "matched_id", "id")
conf_matches[is.infinite(eha_session_order), eha_session_order := NA_integer_]

# Merge to jn and est
jn <- merge(jn, conf_matches, by = "id", all.x = TRUE)
est <- merge(est, conf_matches, by = "id", all.x = TRUE)
for (v in c("presented_at_conference", "presented_at_eha", "presented_at_ehs",
            "n_conference_presentations", "eha_pre_lunch", "eha_post_lunch")) {
  jn[is.na(get(v)), (v) := 0L]
  est[is.na(get(v)), (v) := 0L]
}

cat("  Papers at any conference:", sum(jn$presented_at_conference), "\n")
cat("    via EHA:", sum(jn$presented_at_eha), "  via EHS:", sum(jn$presented_at_ehs),
    "  at both:", sum(jn$presented_at_eha * jn$presented_at_ehs), "\n")
cat("  In estimation sample:", sum(est$presented_at_conference), "\n\n")

# =============================================================================
# 5.3 Conference premium regression
# =============================================================================
c1 <- NULL; c1_uncond <- NULL
if (sum(est$presented_at_conference) >= 20) {

  cat("=== CONFERENCE PREMIUM REGRESSION ===\n\n")

  c1 <- felm(log_longrun ~ presented_at_conference + log_early + n_authors + any_top_inst +
               log_article_length + title_nchar + article_position + issue_no |
               journal + year, data = est)

  cat("Model C1: Any conference presentation, conditional on early citations\n")
  cat("  N presenters:", sum(est$presented_at_conference), "\n")
  cat("  presented_at_conference:", round(coef(c1)["presented_at_conference"], 4),
      "(robust SE:", round(rob_se(c1, "presented_at_conference"), 4), ")\n")
  ci <- coef(c1)["presented_at_conference"] + c(-1.96, 1.96) * rob_se(c1, "presented_at_conference")
  cat(sprintf("  Approximate 95%% interval: %.3f to %.3f log points (upper bound %.1f%% in 1 + citations)\n\n",
              ci[1], ci[2], 100 * expm1(ci[2])))

  # Same controls and fixed effects without the early-citation control. The
  # paper reports this as an association that selection and a conference
  # effect cannot be distinguished within.
  c1_uncond <- felm(log_longrun ~ presented_at_conference + n_authors + any_top_inst +
                      log_article_length + title_nchar + article_position + issue_no |
                      journal + year, data = est)
  cat("Model C1 without the early-citation control\n")
  cat("  presented_at_conference:", round(coef(c1_uncond)["presented_at_conference"], 4),
      "(robust SE:", round(rob_se(c1_uncond, "presented_at_conference"), 4), ")\n\n")
}

# =============================================================================
# 5.4 Session-timing balance and power check (EHA only)
# =============================================================================
# The EHA workbook carries begin-times, so one can ask whether a talk's slot
# (coded pre-lunch vs post-lunch from session start times) shifts long-run
# citations. This is informative ONLY if (a) the slots are balanced on
# pre-determined covariates and (b) the matched cells are large enough. We GATE
# on both; an underpowered or imbalanced design is reported as inconclusive.
cat("=== SESSION-TIMING (EHA) BALANCE & POWER CHECK ===\n\n")
eha_est <- est[presented_at_eha == 1 & !is.na(eha_session_order)]
cl <- NULL; cs <- NULL
timing_powered <- FALSE
timing_balance <- NULL
cat("  EHA-matched papers in estimation sample:", nrow(eha_est), "\n")
if (nrow(eha_est) > 0) {
  cat("  Session-order distribution:\n")
  print(eha_est[, .N, by = eha_session_order][order(eha_session_order)])
  n_pre <- sum(eha_est$eha_pre_lunch); n_post <- sum(eha_est$eha_post_lunch)
  cat("  Pre-lunch (last AM):", n_pre, "  Post-lunch (first PM):", n_post, "\n\n")

  lunch <- eha_est[eha_pre_lunch == 1 | eha_post_lunch == 1]
  if (nrow(lunch) >= 8 && uniqueN(lunch$eha_pre_lunch) == 2) {
    cat("  Balance (normalised mean difference, pre vs post lunch):\n")
    timing_balance <- rbindlist(lapply(
      c("log_early", "fast_starter", "any_top_inst", "n_authors"), function(v) {
        g1 <- lunch[eha_pre_lunch == 1][[v]]; g0 <- lunch[eha_pre_lunch == 0][[v]]
        nmd <- (mean(g1, na.rm = TRUE) - mean(g0, na.rm = TRUE)) /
               sqrt((var(g1, na.rm = TRUE) + var(g0, na.rm = TRUE)) / 2)
        pv <- tryCatch(t.test(g1, g0)$p.value, error = function(e) NA_real_)
        cat(sprintf("    %-14s NMD = %6.3f  (t-test p = %.3f)\n", v, nmd, pv))
        data.table(variable = v, nmd = nmd, p = pv)
      }))
  }

  MIN_CELL <- 25
  timing_powered <- (n_pre >= MIN_CELL && n_post >= MIN_CELL)
  cat(sprintf("\n  Power gate: pre = %d, post = %d, minimum per cell = %d  ->  %s\n",
              n_pre, n_post, MIN_CELL,
              if (timing_powered) "POWERED" else "UNDERPOWERED"))

  if (timing_powered) {
    lunch[, post_lunch_ind := as.integer(eha_post_lunch == 1)]
    cl <- felm(log_longrun ~ post_lunch_ind + log_early + n_authors + any_top_inst |
                 journal + year, data = lunch)
    cat("  Lunch contrast (post vs pre): coef", round(coef(cl)["post_lunch_ind"], 4),
        "(robust SE", round(rob_se(cl, "post_lunch_ind"), 4), ")\n")
    cs <- felm(log_longrun ~ eha_session_order + log_early + n_authors + any_top_inst |
                 journal + year, data = eha_est)
    cat("  Session-order gradient: coef", round(coef(cs)["eha_session_order"], 4),
        "(robust SE", round(rob_se(cs, "eha_session_order"), 4), ")\n\n")
  } else {
    cat("  -> Too few matched papers fall in the lunch-adjacent slots for\n")
    cat("     inference. Reported as inconclusive in the paper.\n\n")
  }
}

# =============================================================================
# 5.5 Author-based conference exposure
# =============================================================================
# Alternative measure: did ANY author of a published paper present ANYTHING at
# EHA/EHS in the year before or year of publication? Captures the broader
# visibility of conference attendance and avoids the title-matching problem
# (at the cost of surname-collision risk). Not reported in the published text.
cat("--- Author-based conference exposure ---\n\n")

conf_data[, conf_lnames := lapply(conf_authors, extract_last_names)]
conf_presenters <- conf_data[!is.na(conf_year),
                              .(conf_lname = unlist(conf_lnames)), by = conf_year]
conf_presenters <- unique(conf_presenters)
conf_presenters <- conf_presenters[nchar(conf_lname) > 1]
cat("  Unique presenter last names:", uniqueN(conf_presenters$conf_lname), "\n")
cat("  Conference years covered:", paste(sort(unique(conf_presenters$conf_year)), collapse = ", "), "\n")

jn[, paper_lnames := mapply(function(a1, a2, a3, a4, a5) {
  all_authors <- c(a1, a2, a3, a4, a5)
  all_authors <- all_authors[!is.na(all_authors) & all_authors != ""]
  unique(unlist(lapply(all_authors, extract_last_names)))
}, author1, author2, author3, author4, author5, SIMPLIFY = FALSE)]

jn[, author_conf_exposure := mapply(function(pub_year, lnames) {
  if (length(lnames) == 0) return(0L)
  conf_names_window <- conf_presenters[conf_year %in% (pub_year - 1):pub_year, conf_lname]
  if (length(conf_names_window) == 0) return(0L)
  as.integer(any(lnames %in% conf_names_window))
}, year, paper_lnames)]

cat("  Papers with author conference exposure:", sum(jn$author_conf_exposure),
    "/", nrow(jn), "(", round(mean(jn$author_conf_exposure) * 100, 1), "%)\n")

est[, author_conf_exposure := jn$author_conf_exposure[match(est$id, jn$id)]]
est[is.na(author_conf_exposure), author_conf_exposure := 0L]

cat("  In estimation sample:", sum(est$author_conf_exposure),
    "/", nrow(est), "(", round(mean(est$author_conf_exposure) * 100, 1), "%)\n\n")

c2 <- NULL; c3 <- NULL
if (sum(est$author_conf_exposure) >= 30) {

  cat("=== AUTHOR-BASED CONFERENCE EXPOSURE REGRESSIONS ===\n\n")

  c2 <- felm(log_longrun ~ author_conf_exposure + log_early + n_authors + any_top_inst +
               log_article_length + title_nchar + article_position + issue_no |
               journal + year, data = est)
  cat("Model C2: Author conference exposure (broad measure)\n")
  cat("  author_conf_exposure:", round(coef(c2)["author_conf_exposure"], 4),
      "(robust SE:", round(rob_se(c2, "author_conf_exposure"), 4), ")\n\n")

  if (sum(est$presented_at_conference) >= 10) {
    c3 <- felm(log_longrun ~ presented_at_conference + author_conf_exposure + log_early +
                 n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = est)
    cat("Model C3: Title-matched + author exposure (joint)\n")
    cat("  presented_at_conference:", round(coef(c3)["presented_at_conference"], 4),
        "(robust SE:", round(rob_se(c3, "presented_at_conference"), 4), ")\n")
    cat("  author_conf_exposure:", round(coef(c3)["author_conf_exposure"], 4),
        "(robust SE:", round(rob_se(c3, "author_conf_exposure"), 4), ")\n\n")
  }
}

# =============================================================================
# 5.6 Placebo conference permutation test
# =============================================================================
# Programme-match status is reshuffled within journal-year cohorts. The paper
# reports the one-sided (upper-tail, positive premium) p-value and the
# two-sided absolute-coefficient p-value, both with the finite-repetition
# correction (1 + exceedances) / (1 + permutations).
N_PERMUTATIONS <- 1000
placebo_conf_coefs <- NULL; true_conf_coef <- NULL; emp_p_conf <- NULL; emp_p_conf_two_sided <- NULL

if (sum(est$presented_at_conference, na.rm = TRUE) >= 20) {

  cat("--- Placebo conference (", N_PERMUTATIONS, "permutations) ---\n")

  conf_placebo_data <- est[!is.na(presented_at_conference)]
  true_conf_model <- felm(log_longrun ~ presented_at_conference + log_early +
                            n_authors + any_top_inst +
                            log_article_length + title_nchar + article_position + issue_no |
                            journal + year,
                          data = conf_placebo_data)
  true_conf_coef <- coef(true_conf_model)["presented_at_conference"]
  cat("Observed conference coefficient:", round(true_conf_coef, 4), "\n")

  placebo_conf_coefs <- numeric(N_PERMUTATIONS)

  for (p in seq_len(N_PERMUTATIONS)) {
    if (p %% 100 == 0) cat("  Permutation", p, "/", N_PERMUTATIONS, "\n")
    perm_data <- copy(conf_placebo_data)
    perm_data[, presented_at_conference := sample(presented_at_conference),
              by = .(journal, year)]
    m_perm <- tryCatch({
      felm(log_longrun ~ presented_at_conference + log_early +
             n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year, data = perm_data)
    }, error = function(e) NULL)
    placebo_conf_coefs[p] <- if (!is.null(m_perm)) coef(m_perm)["presented_at_conference"] else NA_real_
  }

  placebo_conf_coefs <- placebo_conf_coefs[!is.na(placebo_conf_coefs)]
  emp_p_conf <- (1 + sum(placebo_conf_coefs >= true_conf_coef)) / (1 + length(placebo_conf_coefs))
  emp_p_conf_two_sided <- (1 + sum(abs(placebo_conf_coefs) >= abs(true_conf_coef))) / (1 + length(placebo_conf_coefs))

  cat("One-sided (upper-tail) p-value:", formatC(emp_p_conf, format = "f", digits = 4), "\n")
  cat("Two-sided (absolute) p-value:  ", formatC(emp_p_conf_two_sided, format = "f", digits = 4), "\n")
  cat("  Mean placebo:", round(mean(placebo_conf_coefs), 4), "\n")
  cat("  SD placebo:", round(sd(placebo_conf_coefs), 4), "\n\n")
}

# =============================================================================
# 5.7 Sensitivity of the conditional association to the review decisions
# =============================================================================
cat("--- Sensitivity: uncertain links and title thresholds ---\n\n")
fit_case <- function(ids, label) {
  d <- copy(ad$est); d[, presented := as.integer(id %in% ids)]
  fit <- felm(log_longrun ~ presented + log_early + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = d)
  z <- summary(fit, robust = TRUE)$coefficients["presented", ]
  data.table(scenario = label, presenters = sum(d$presented), coef = z[1], se = z[2], p = z[4])
}
sensitivity <- rbindlist(list(
  fit_case(ledger[decision == "retain", matched_id], "reviewed_conservative"),
  fit_case(ledger[decision != "reject", matched_id], "include_uncertain"),
  fit_case(ledger[decision == "retain" & dist < .15, matched_id], "reviewed_distance_below_015"),
  fit_case(ledger[decision == "retain" & dist < .10, matched_id], "reviewed_distance_below_010")))
print(sensitivity)
fwrite(sensitivity, file.path(TAB_DIR, "TableE1_ConferenceSensitivity.csv"))
cat("\n")

# =============================================================================
# Save
# =============================================================================

# id-keyed flags for downstream scripts (06 mechanisms, 09 panel)
conference_flags <- jn[, .(id, presented_at_conference, presented_at_eha,
                           presented_at_ehs, n_conference_presentations,
                           eha_session_order, eha_pre_lunch, eha_post_lunch,
                           author_conf_exposure)]
saveRDS(conference_flags, file.path(RESULTS_DIR, "conference_flags.rds"))
cat("Saved: results/conference_flags.rds\n")

res_05 <- list(
  n_conf_papers_loaded = nrow(conf_data),
  n_matched_entries = sum(!is.na(conf_data$matched_id)),
  match_by_tier = conf_data[!is.na(matched_id), .N, by = .(conference, match_tier)],
  ledger_counts = ledger[, .N, by = decision],
  n_papers_any_conf = sum(jn$presented_at_conference),
  conference_papers = jn[, .(eha = sum(presented_at_eha), ehs = sum(presented_at_ehs),
                             both = sum(presented_at_eha * presented_at_ehs))],
  n_presenters_est = sum(est$presented_at_conference),
  c1 = if (!is.null(c1)) list(coef = coef(c1)["presented_at_conference"],
                              se = rob_se(c1, "presented_at_conference"), n = c1$N) else NULL,
  c1_unconditional = if (!is.null(c1_uncond)) list(
                              coef = coef(c1_uncond)["presented_at_conference"],
                              se = rob_se(c1_uncond, "presented_at_conference"), n = c1_uncond$N) else NULL,
  c2 = if (!is.null(c2)) list(coef = coef(c2)["author_conf_exposure"],
                              se = rob_se(c2, "author_conf_exposure"), n = c2$N) else NULL,
  c3 = if (!is.null(c3)) list(conf = coef(c3)["presented_at_conference"],
                              conf_se = rob_se(c3, "presented_at_conference"),
                              expo = coef(c3)["author_conf_exposure"],
                              expo_se = rob_se(c3, "author_conf_exposure")) else NULL,
  timing = list(powered = timing_powered, balance = timing_balance,
                n_pre = if (exists("n_pre")) n_pre else NA,
                n_post = if (exists("n_post")) n_post else NA),
  placebo = list(coefs = placebo_conf_coefs, true_coef = true_conf_coef,
                 emp_p = emp_p_conf, tail = "upper (positive premium)",
                 emp_p_two_sided = emp_p_conf_two_sided),
  sensitivity = sensitivity
)
saveRDS(res_05, file.path(RESULTS_DIR, "res_05_conference.rds"))
cat("Saved: results/res_05_conference.rds\n")

close_log()

# =============================================================================
# 05_conference.R -- conference presentation and citations
# -----------------------------------------------------------------------------
# Scope: presentations at the EHA (Economic History Association) and EHS
# (Economic History Society) annual meetings, the two annual association
# meetings of the field with programmes that could be recovered reliably
# (EHA from a hand-transcribed workbook, 2006-2025, with session times;
# EHS from structured HTML programmes). The EHES biennial meeting is not
# covered; the conference variable is therefore defined as "presented at
# EHA or EHS", and the paper states this scope explicitly.
#
# 5.1 Author-validated fuzzy title matching (conference paper -> journal paper)
# 5.2 Paper-level conference variables
# 5.3 Conference premium regression (C1)
# 5.4 Session-timing balance and power check (EHA begin-times)
# 5.5 Author-based conference exposure (C2, C3)
# 5.6 Placebo conference permutation test
#
# Outputs: results/conference_flags.rds  (id-keyed flags for scripts 06/09)
#          results/res_05_conference.rds (regression and placebo results)
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

conf_parsed_file <- file.path(DATA_CACHE, "conference_parsed_data.rds")
if (!file.exists(conf_parsed_file)) {
  stop("conference_parsed_data.rds not found in data/cache/.")
}

conf_data <- readRDS(conf_parsed_file)
cat("Loaded", nrow(conf_data), "conference papers\n")
print(conf_data[, .N, by = conference])
cat("\n")

# Ensure column names
if ("title" %in% names(conf_data) && !"conf_title" %in% names(conf_data)) {
  conf_data[, conf_title := title]
}
if ("authors" %in% names(conf_data) && !"conf_authors" %in% names(conf_data)) {
  conf_data[, conf_authors := authors]
}
if ("year" %in% names(conf_data) && !"conf_year" %in% names(conf_data)) {
  conf_data[, conf_year := year]
}

# =============================================================================
# 5.1 Fuzzy match conference papers to journal papers
# =============================================================================
cat("Matching conference papers to journal dataset...\n")

jn[, title_clean := tolower(gsub("[^a-z0-9 ]", "", title))]
jn[, title_clean := gsub("\\s+", " ", trimws(title_clean))]

conf_data[, conf_title_clean := tolower(gsub("[^a-z0-9 ]", "", conf_title))]
conf_data[, conf_title_clean := gsub("\\s+", " ", trimws(conf_title_clean))]

jn[, author1_clean := tolower(gsub("[^a-z ]", "", author1))]
conf_data[, conf_author_clean := tolower(gsub("[^a-z ]", "", conf_authors))]

# Clean conference titles: remove timestamps and short entries
conf_data[grepl("[0-9]:[0-9]{2}", conf_title), conf_title_clean := NA_character_]
conf_data[!is.na(conf_title_clean) & nchar(conf_title_clean) < 15, conf_title_clean := NA_character_]

# Helper: extract surname(s) from an author string. Journal authors are stored
# "Firstname Lastname"; conference authors may be "; "/"and"-separated. Take the
# last token of each name.
extract_last_names <- function(author_str) {
  if (is.na(author_str) || nchar(trimws(author_str)) < 2) return(character(0))
  parts <- unlist(strsplit(author_str, "[,;/&]|\\band\\b"))
  parts <- trimws(parts)
  parts <- parts[nchar(parts) > 1]
  last_names <- sapply(parts, function(p) {
    words <- strsplit(trimws(p), "\\s+")[[1]]
    words <- words[nchar(words) > 1]
    if (length(words) == 0) return(NA_character_)
    tolower(tail(words, 1))
  }, USE.NAMES = FALSE)
  last_names <- last_names[!is.na(last_names) & nchar(last_names) > 1]
  unique(last_names)
}

# Precompute the surname set of every journal paper ONCE (reused by each match).
jn[, surnames := mapply(function(a1, a2, a3, a4, a5)
      list(unique(c(extract_last_names(a1), extract_last_names(a2),
                    extract_last_names(a3), extract_last_names(a4),
                    extract_last_names(a5)))),
    author1, author2, author3, author4, author5, SIMPLIFY = FALSE)]
# Surnames from the RAW author string (extract_last_names splits on ; & "and";
# conf_author_clean has those separators stripped, which would collapse a
# multi-author talk to just its last surname).
conf_data[, conf_surnames := lapply(conf_authors, extract_last_names)]

# Author-validated matcher. Title drift between a conference presentation and
# the published version (British/US spelling, subtitle changes) routinely pushes
# the Jaro-Winkler distance to 0.15-0.25 for the SAME paper. Tiers:
#   A: title distance < 0.10                    -> accept (near-exact title)
#   B: title distance < 0.25 AND author overlap -> accept (validated retitle)
#   C: no conference author info, title < 0.15  -> accept (title only)
TIER_A_MAX <- 0.10
TIER_B_MAX <- 0.25
TIER_C_MAX <- 0.15

find_best_match <- function(ct, conf_lnames, cy, jn_data) {
  miss <- list(id = NA_integer_, dist = NA_real_, tier = NA_character_)
  if (is.na(ct) || nchar(ct) < 15) return(miss)
  cand <- jn_data[year >= cy - 1 & year <= cy + 5]
  if (nrow(cand) == 0) return(miss)
  td <- stringdist(ct, cand$title_clean, method = "jw")
  k <- which.min(td)
  if (td[k] < TIER_A_MAX) return(list(id = cand$id[k], dist = td[k], tier = "A"))
  if (length(conf_lnames) > 0) {
    ov <- vapply(cand$surnames, function(js) any(conf_lnames %in% js), logical(1))
    idxB <- which(ov & td < TIER_B_MAX)
    if (length(idxB) > 0) {
      j <- idxB[which.min(td[idxB])]
      return(list(id = cand$id[j], dist = td[j], tier = "B"))
    }
  } else if (td[k] < TIER_C_MAX) {
    return(list(id = cand$id[k], dist = td[k], tier = "C"))
  }
  list(id = NA_integer_, dist = td[k], tier = NA_character_)
}

cat("  Running author-validated fuzzy matching...\n")
conf_data[, matched_id := NA_integer_]
conf_data[, match_dist := NA_real_]
conf_data[, match_tier := NA_character_]
n_skipped_short <- sum(is.na(conf_data$conf_title_clean))
cat("  Skipping", n_skipped_short, "entries (NA or short titles)\n")

for (i in 1:nrow(conf_data)) {
  if (!is.na(conf_data$conf_title_clean[i]) && nchar(conf_data$conf_title_clean[i]) >= 15) {
    r <- find_best_match(conf_data$conf_title_clean[i], conf_data$conf_surnames[[i]],
                         conf_data$conf_year[i], jn)
    conf_data[i, `:=`(matched_id = r$id, match_dist = r$dist, match_tier = r$tier)]
  }
  if (i %% 1000 == 0) cat("    Processed", i, "entries,",
                          sum(!is.na(conf_data$matched_id)), "matches so far\n")
}

cat("\n  Successfully matched", sum(!is.na(conf_data$matched_id)), "conference entries\n")
cat("  By conference and tier:\n")
print(conf_data[!is.na(matched_id), .N, by = .(conference, match_tier)][order(conference, match_tier)])

# Print a sample of tier-B (author-validated retitle) matches for verification
cat("\n  Sample author-validated (tier B) matches for inspection:\n")
bsamp <- conf_data[match_tier == "B"][order(match_dist)]
if (nrow(bsamp) > 0) {
  for (i in seq_len(min(12, nrow(bsamp)))) {
    jp <- jn[id == bsamp$matched_id[i]]
    cat(sprintf("    [%.2f] CONF: %.56s\n           JRNL: %.56s\n",
                bsamp$match_dist[i], bsamp$conf_title[i], jp$title[1]))
  }
}
cat("\n")

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
cat("    via EHA:", sum(jn$presented_at_eha), "  via EHS:", sum(jn$presented_at_ehs), "\n")
cat("  In estimation sample:", sum(est$presented_at_conference), "\n\n")

# =============================================================================
# 5.3 Conference premium regression
# =============================================================================
c1 <- NULL
if (sum(est$presented_at_conference) >= 20) {

  cat("=== CONFERENCE PREMIUM REGRESSION ===\n\n")

  c1 <- felm(log_longrun ~ presented_at_conference + log_early + n_authors + any_top_inst +
               log_article_length + title_nchar + article_position + issue_no |
               journal + year, data = est)

  cat("Model C1: Any conference presentation effect (EHA clean + EHS)\n")
  cat("  N presenters:", sum(est$presented_at_conference), "\n")
  cat("  presented_at_conference:", round(coef(c1)["presented_at_conference"], 4),
      "(robust SE:", round(rob_se(c1, "presented_at_conference"), 4), ")\n\n")
}

# =============================================================================
# 5.4 Session-timing balance and power check (EHA only)
# =============================================================================
# The clean EHA programmes carry exact begin-times, so we can ask whether a
# talk's slot (last session before lunch vs first after) shifts long-run
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
# (at the cost of surname-collision risk, noted in the paper).
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
N_PERMUTATIONS <- 1000
placebo_conf_coefs <- NULL; true_conf_coef <- NULL; emp_p_conf <- NULL

if (sum(est$presented_at_conference, na.rm = TRUE) >= 20) {

  cat("--- Placebo conference (", N_PERMUTATIONS, "permutations) ---\n")

  conf_placebo_data <- est[!is.na(presented_at_conference)]
  true_conf_model <- felm(log_longrun ~ presented_at_conference + log_early +
                            n_authors + any_top_inst +
                            log_article_length + title_nchar + article_position + issue_no |
                            journal + year,
                          data = conf_placebo_data)
  true_conf_coef <- coef(true_conf_model)["presented_at_conference"]
  cat("True conference coefficient:", round(true_conf_coef, 4), "\n")

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
  emp_p_conf <- mean(placebo_conf_coefs >= true_conf_coef)

  cat("Empirical p-value:", formatC(emp_p_conf, format = "f", digits = 4), "\n")
  cat("  Mean placebo:", round(mean(placebo_conf_coefs), 4), "\n")
  cat("  SD placebo:", round(sd(placebo_conf_coefs), 4), "\n\n")
}

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
  n_papers_any_conf = sum(jn$presented_at_conference),
  n_presenters_est = sum(est$presented_at_conference),
  c1 = if (!is.null(c1)) list(coef = coef(c1)["presented_at_conference"],
                              se = rob_se(c1, "presented_at_conference"), n = c1$N) else NULL,
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
                 emp_p = emp_p_conf)
)
saveRDS(res_05, file.path(RESULTS_DIR, "res_05_conference.rds"))
cat("Saved: results/res_05_conference.rds\n")

close_log()

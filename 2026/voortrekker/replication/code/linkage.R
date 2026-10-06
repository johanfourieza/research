# ============================================================================
# LINKAGE final
# ============================================================================
# Record linkage:
#   - spouse evidence against EVERY genealogy spouse married by the census year;
#   - evidence states: agrees / not_comparable / contradicts;
#   - RF features without one-sided wife-presence indicators, plus jw_surname;
#   - grouped 5-fold CV (connected components of persons and households),
#     with a 15% held-out audit sample never used for tuning;
#   - threshold T maximising F0.5; increments for the not-comparable state (d2)
#     and for cross-district links (d1) chosen to match the precision of the
#     spouse-agrees state;
#   - proposals, review band and competing identities for the decision layer.
# A wife-blind variant (spouse = FALSE) drops all spouse information.
# ============================================================================

suppressPackageStartupMessages({library(dplyr); library(stringdist); library(randomForest)})

FEATURES_BASE <- c("jw_male_full", "jw_male_first", "lv_male_full", "jw_initials", "initials_exact",
                      "both_multi_name", "len_ratio", "word_diff", "exact_first_match", "exact_full_match",
                      "surname_freq_log", "name_pair_freq_log", "name_is_rare", "is_primary_district", "jw_surname")
FEATURES_SPOUSE <- c("both_have_wife", "wife_best_surname", "wife_best_first", "jw_wife_vs_husband_surname")
CENSUS_YEAR <- function(d) ifelse(d == "Cradock", 1823, ifelse(d %in% c("Clanwilliam", "Worcester"), 1824, 1825))

# Every recorded spouse of each genealogy row: up to four marriages.
vt_spouse_table <- function(vt_path = "data/raw/Voortrekkers 2.xlsx") {
  raw <- suppressMessages(readxl::read_excel(vt_path, sheet = "Main", col_names = FALSE, col_types = "text"))
  # Columns (1-based, row 1 = header): date, first names, surname for each marriage.
  spec <- list(c(21, 23, 24), c(39, 40, 41), c(50, 51, 52), c(60, 61, 62))
  yr_col <- 22
  out <- list()
  for (k in seq_along(spec)) {
    s <- spec[[k]]
    date <- raw[[s[1]]][-1]; first <- raw[[s[2]]][-1]; sur <- raw[[s[3]]][-1]
    year <- suppressWarnings(as.integer(stringr::str_extract(date, "(1[6-9][0-9]{2})(?!.*[0-9]{4})")))
    if (k == 1) year <- dplyr::coalesce(suppressWarnings(as.integer(raw[[yr_col]][-1])), year)
    out[[k]] <- tibble::tibble(vt_source_row = seq_along(first) + 1L, spouse_k = k,
                               sp_first = first, sp_surname = sur, sp_year = year)
  }
  bind_rows(out) %>%
    mutate(across(c(sp_first, sp_surname), ~ { x <- stringr::str_squish(toupper(.)); ifelse(is.na(x) | x %in% c("", "NA"), NA_character_, x) })) %>%
    filter(!is.na(sp_first) | !is.na(sp_surname)) %>%
    mutate(sp_surname = stringr::str_replace(sp_surname, "^V\\.?\\s*D\\.?\\s+", "VAN DER "),
           sp_surname = stringr::str_replace(sp_surname, "^V\\.?\\s+", "VAN "),
           sp_first_only = stringr::str_extract(sp_first, "^\\S+"))
}

# Similarity of a census first name (one token, possibly an initial) to the
# best-matching of a spouse's given names.
first_name_sim <- function(census_first, spouse_given) {
  mapply(function(c, g) {
    if (is.na(c) || is.na(g)) return(NA_real_)
    toks <- strsplit(gsub("[^A-Z ]", " ", g), "\\s+")[[1]]; toks <- toks[toks != ""]
    c0 <- gsub("[^A-Z]", "", c)
    if (!length(toks) || c0 == "") return(NA_real_)
    if (nchar(c0) == 1) return(if (c0 %in% substr(toks, 1, 1)) 0.90 else 0.30)
    max(1 - stringdist(c0, toks, method = "jw", p = 0.1))
  }, census_first, spouse_given, USE.NAMES = FALSE)
}

# Spouse evidence for each candidate pair, over spouses married by the census year
# (marriage year unknown counts as possibly married).
spouse_evidence <- function(cand, spouses) {
  key <- cand %>% distinct(row_id, census_id, vt_source_row, search_district, vt_surname_std,
                           census_wife_surname_std, census_wife_first_only)
  x <- key %>% inner_join(spouses, by = "vt_source_row", relationship = "many-to-many") %>%
    filter(is.na(sp_year) | sp_year <= CENSUS_YEAR(search_district), !is.na(census_wife_surname_std) | !is.na(census_wife_first_only)) %>%
    mutate(
      married_variant = !is.na(census_wife_surname_std) &
        (1 - stringdist(gsub("[^A-Z]", "", census_wife_surname_std), gsub("[^A-Z]", "", vt_surname_std), method = "jw", p = 0.1)) >= 0.85,
      s_sur = ifelse(!is.na(sp_surname) & !is.na(census_wife_surname_std),
                     1 - stringdist(gsub("[^A-Z]", "", sp_surname), gsub("[^A-Z]", "", census_wife_surname_std), method = "jw", p = 0.1), NA_real_),
      s_sur_eff = ifelse(married_variant, 1, s_sur),
      # Census first name against ALL of the spouse's given names; a census
      # initial ("J.") is compared with the spouse's initials.
      s_first = first_name_sim(census_wife_first_only, sp_first),
      agree = !is.na(s_first) & s_first >= 0.80 & !is.na(s_sur_eff) & s_sur_eff >= 0.85,
      # An affirmative conflict on either name (a clearly different maiden
      # surname, or a clearly different first name or initial) counts as a
      # conflict even if the other name agrees: such pairs go to review
      # ("contradicts"), never to the missing-evidence state.
      sur_bad = !is.na(s_sur) & !married_variant & s_sur < 0.75,
      first_bad = !is.na(s_first) & s_first < 0.65,
      conflict = sur_bad | first_bad)
  ev <- x %>% group_by(row_id, census_id) %>%
    summarise(wife_best_surname = suppressWarnings(max(s_sur_eff, na.rm = TRUE)),
              wife_best_first = suppressWarnings(max(s_first, na.rm = TRUE)),
              any_agree = any(agree), all_conflict = all(conflict), n_spouses = n(), .groups = "drop") %>%
    mutate(across(c(wife_best_surname, wife_best_first), ~ ifelse(is.finite(.), ., 0)))
  cand %>% select(-any_of(c("wife_best_surname", "wife_best_first", "evidence_state", "both_have_wife"))) %>%
    left_join(ev, by = c("row_id", "census_id")) %>%
    mutate(both_have_wife = as.integer(!is.na(n_spouses)),
           # no one-sided wife information: zero unless both sides record a wife
           jw_wife_vs_husband_surname = ifelse(both_have_wife == 1, coalesce(jw_wife_vs_husband_surname, 0), 0),
           wife_best_surname = coalesce(wife_best_surname, 0), wife_best_first = coalesce(wife_best_first, 0),
           evidence_state = case_when(any_agree %in% TRUE ~ "agrees",
                                      all_conflict %in% TRUE ~ "contradicts",
                                      TRUE ~ "not_comparable")) %>%
    select(-any_agree, -all_conflict, -n_spouses)
}

# Connected components over persons (row_id) and households (census_id).
pair_components <- function(row_id, census_id) {
  uf <- new.env()
  for (n in unique(c(paste0("p", row_id), paste0("h", census_id)))) assign(n, n, envir = uf)
  find <- function(a) { while (get(a, envir = uf) != a) a <- get(a, envir = uf); a }
  for (i in seq_along(row_id)) {
    a <- find(paste0("p", row_id[i])); b <- find(paste0("h", census_id[i]))
    if (a != b) assign(a, b, envir = uf)
  }
  unname(vapply(paste0("p", row_id), find, character(1)))
}

prf <- function(score, label, thr) {
  pred <- score >= thr
  tp <- sum(pred & label == 1); fp <- sum(pred & label == 0); fn <- sum(!pred & label == 1)
  prec <- if (tp + fp) tp / (tp + fp) else NA_real_; rec <- if (tp + fn) tp / (tp + fn) else NA_real_
  f05 <- if (!is.na(prec) && !is.na(rec) && prec + rec > 0) 1.25 * prec * rec / (0.25 * prec + rec) else 0
  c(tp = tp, fp = fp, fn = fn, precision = prec, recall = rec, f0.5 = f05)
}
wilson <- function(k, n, z = 1.96) {
  if (!n) return(c(NA, NA)); p <- k / n; d <- 1 + z^2 / n
  c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
    (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d)
}

# Train, cross-validate and set thresholds. labels: row_id, census_id, label (0/1).
fit_linkage <- function(cand, labels, spouse = TRUE, seed = 42, outdir = "output/tables", tag = "spouse") {
  feats <- if (spouse) c(FEATURES_BASE, FEATURES_SPOUSE) else FEATURES_BASE
  lab <- cand %>% select(-any_of("label")) %>%
    inner_join(labels %>% select(row_id, census_id, label), by = c("row_id", "census_id"))
  cat(sprintf("[%s] labelled pairs found among candidates: %d of %d\n", tag, nrow(lab), nrow(labels)))
  lab$component <- pair_components(lab$row_id, lab$census_id)
  set.seed(seed)
  comps <- unique(lab$component)
  holdout <- sample(comps, round(0.15 * length(comps)))
  lab$holdout <- lab$component %in% holdout
  tr <- lab %>% filter(!holdout)
  cfold <- setNames(sample(rep(1:5, length.out = length(unique(tr$component)))), unique(tr$component))
  tr$fold <- cfold[tr$component]
  X <- as.data.frame(tr[, feats]) %>% mutate(across(everything(), ~ coalesce(as.numeric(.), 0))); y <- factor(tr$label, levels = c(0, 1))
  rf_args <- function(yy) list(ntree = 500, mtry = floor(sqrt(length(feats))),
                               classwt = c("0" = 1, "1" = sum(yy == "0") / max(1, sum(yy == "1"))))
  tr$oof <- NA_real_
  for (k in 1:5) {
    i <- tr$fold != k
    m <- do.call(randomForest, c(list(x = X[i, ], y = y[i]), rf_args(y[i])))
    tr$oof[!i] <- predict(m, X[!i, ], type = "prob")[, "1"]
  }
  grid <- seq(0.30, 0.80, by = 0.05)
  cv <- do.call(rbind, lapply(grid, function(t) c(threshold = t, prf(tr$oof, tr$label, t))))
  T0 <- grid[which.max(cv[, "f0.5"])]
  # State-specific increments: precision of the not-comparable state (and of
  # cross-district pairs) must reach the precision of the spouse-agrees state at T.
  # Each increment is calibrated on the pairs its rule can accept; if no
  # increment up to 0.30 reaches the target precision, that rule accepts nothing
  # automatically (increment Inf: such pairs can only enter through review).
  ref_sel <- tr$evidence_state == "agrees" & tr$is_primary_district
  ref <- prf(tr$oof[ref_sel], tr$label[ref_sel], T0)["precision"]
  inc_for <- function(sel) {
    if (!spouse || !any(sel)) return(if (spouse) Inf else 0)
    for (d in seq(0, 0.30, by = 0.05)) {
      p <- prf(tr$oof[sel], tr$label[sel], T0 + d)
      if (!is.na(p["precision"]) && !is.na(ref) && p["precision"] >= ref) return(d)
    }
    Inf
  }
  d2 <- inc_for(tr$evidence_state == "not_comparable" & tr$is_primary_district)
  d1 <- if (spouse) inc_for(tr$evidence_state == "agrees" & !tr$is_primary_district) else Inf
  set.seed(seed)
  model <- do.call(randomForest, c(list(x = X, y = y, importance = TRUE), rf_args(y)))
  # Held-out audit sample: precision and recall at the chosen rule.
  ho <- lab %>% filter(holdout)
  ho$score <- predict(model, as.data.frame(ho[, feats]) %>% mutate(across(everything(), ~ coalesce(as.numeric(.), 0))), type = "prob")[, "1"]
  ho$accept <- accept_rule(ho, T0, d2, d1, spouse)
  hp <- prf(as.numeric(ho$accept), ho$label, 0.5)
  diag <- list(
    cv = as.data.frame(cv), T = T0, d2 = d2, d1 = d1, features = feats,
    cv_by_state = tr %>% mutate(score = oof) %>% mutate(pred = accept_rule(., T0, d2, d1, spouse)) %>%
      group_by(evidence_state) %>% summarise(n = n(), positives = sum(label == 1), accepted = sum(pred),
                                             true_pos = sum(pred & label == 1), .groups = "drop") %>%
      mutate(precision = true_pos / accepted, recall = true_pos / positives),
    holdout = data.frame(n_pairs = nrow(ho), n_components = length(holdout), positives = sum(ho$label == 1),
                         accepted = sum(ho$accept), true_pos = hp[["tp"]], precision = hp[["precision"]],
                         prec_lo = wilson(hp[["tp"]], sum(ho$accept))[1], prec_hi = wilson(hp[["tp"]], sum(ho$accept))[2],
                         recall = hp[["recall"]]),
    importance = data.frame(variable = rownames(model$importance), gini = model$importance[, "MeanDecreaseGini"]))
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  write.csv(diag$cv, file.path(outdir, paste0("cv_diagnostics_", tag, ".csv")), row.names = FALSE)
  write.csv(diag$cv_by_state, file.path(outdir, paste0("cv_by_state_", tag, ".csv")), row.names = FALSE)
  write.csv(diag$holdout, file.path(outdir, paste0("holdout_audit_", tag, ".csv")), row.names = FALSE)
  write.csv(data.frame(T = T0, d2 = d2, d1 = d1, spouse = spouse), file.path(outdir, paste0("thresholds_", tag, ".csv")), row.names = FALSE)
  cat(sprintf("[%s] T = %.2f, d2 = %.2f, d1 = %s; holdout precision %.3f [%.3f, %.3f], recall %.3f\n", tag, T0, d2,
              format(d1), diag$holdout$precision, diag$holdout$prec_lo, diag$holdout$prec_hi, diag$holdout$recall))
  list(model = model, diag = diag, labelled = lab %>% select(row_id, census_id, label, component, holdout))
}

# Acceptance rule by evidence state (spouse-assisted) or a single threshold (wife-blind).
accept_rule <- function(d, T0, d2, d1, spouse) {
  if (!spouse) return(d$score >= T0 & d$is_primary_district)
  thr <- ifelse(d$evidence_state == "agrees", T0, T0 + d2)
  thr <- ifelse(d$is_primary_district, thr, ifelse(d$evidence_state == "agrees", T0 + d1, Inf))
  d$score >= thr & d$evidence_state != "contradicts"
}

# Score all candidates and build proposals plus the review band.
propose_links <- function(cand, fit, spouse = TRUE, band = 0.15) {
  feats <- fit$diag$features
  cand$score <- predict(fit$model, as.data.frame(cand[, feats]) %>% mutate(across(everything(), ~ coalesce(as.numeric(.), 0))), type = "prob")[, "1"]
  T0 <- fit$diag$T; d2 <- fit$diag$d2; d1 <- fit$diag$d1
  cand$threshold <- if (spouse) ifelse(cand$is_primary_district,
                                       ifelse(cand$evidence_state == "agrees", T0, T0 + d2),
                                       ifelse(cand$evidence_state == "agrees", T0 + d1, Inf)) else
    ifelse(cand$is_primary_district, T0, Inf)
  cand$accept <- accept_rule(cand, T0, d2, d1, spouse)
  # Proposal: each person's highest-scoring candidate, if acceptable. Review
  # status is assigned to EVERY scored pair, not only to each person's top one.
  cand <- cand %>% group_by(row_id) %>% arrange(desc(score), census_id, .by_group = TRUE) %>%
    mutate(is_top = row_number() == 1) %>% ungroup()
  # Review band: within `band` of the pair's own threshold; where the rule has
  # no finite threshold (contradictions, unattainable increments), within
  # `band` of T.
  ref_thr <- ifelse(is.finite(cand$threshold), cand$threshold, T0)
  cand$status <- dplyr::case_when(
    cand$is_top & cand$accept ~ "proposed",
    spouse & cand$evidence_state == "contradicts" & cand$score >= T0 - band ~ "review_contradiction",
    cand$score >= ref_thr - band ~ "review_band",
    TRUE ~ "not_linked")
  review_pairs <- cand %>% filter(status %in% c("review_band", "review_contradiction"))
  top <- cand %>% filter(is_top)
  # One person per census head: competing proposals go to review together.
  prop <- top %>% filter(status == "proposed") %>% group_by(census_id) %>%
    mutate(competing = n() > 1) %>% ungroup()
  top <- top %>% left_join(prop %>% select(row_id, competing), by = "row_id") %>%
    mutate(competing = coalesce(competing, FALSE))
  list(scored = cand, top = top, review = review_pairs)
}

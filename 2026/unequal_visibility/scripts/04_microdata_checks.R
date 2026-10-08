# =============================================================================
#  04_microdata_checks.R
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Rebuilds the paper's person-level results from the microdata in
#  data/microdata/ and asserts, cell by cell, that they equal (a) the aggregate
#  files in data/ and (b) the numbers typeset in the manuscript, which are
#  shipped in docs/generated_numbers/ exactly as the analysis wrote them.
#    1. the death flags, rebuilt from the accepted links in the two death-link
#       registers (MOOC8 documents and Stellenbosch compilation schedules)
#    2. Table 1, the cohort by adult slaves recorded
#    3. Table 2, probate, widow and combined death records with Wilson
#       intervals, and the recording-model numbers
#    4. the comparison restricted to men with a wife named
#    5. the alternative resource index
#    6. the alternative documentary (source) rules
#    7. the identity scenarios, the ambiguity bounds and the repeated-name
#       denominators
#    8. the record-continuation cells, the common-weight contrasts and their
#       clustered standard errors, from the person-level panel
#  The genealogical-presence (SAF) column of Table 2 cannot be rebuilt: it
#  needs the South African Families links, which are not redistributed. The
#  script reads that column from data/death_records_by_group.csv and says so.
#
#  HOW TO USE IT
#  From the package root:  Rscript scripts/04_microdata_checks.R
# =============================================================================

source(file.path(if (dir.exists("scripts")) "scripts" else ".", "00_setup.R"))
MICRO <- file.path(DATA, "microdata")
GEN   <- file.path(ROOT, "docs", "generated_numbers")
read_micro <- function(file, ...) read_csv(file.path(MICRO, file), ...)
read_register <- function(file) read_csv(file.path(MICRO, file), col_types = cols(.default = "c"),
                                         na = "")
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)

n_checks <- 0L
same <- function(label, got, expected, tol = 0) {
  got <- unname(got); expected <- unname(expected)
  ok <- length(got) == length(expected) &&
    if (is.numeric(got) && is.numeric(expected)) all(abs(got - expected) <= tol + 1e-12)
    else identical(as.character(got), as.character(expected))
  n_checks <<- n_checks + length(expected)
  cat(sprintf("  %-66s %s\n", label, if (ok) "ok" else "MISMATCH"))
  if (!ok) {
    print(list(recomputed = got, expected = expected))
    stop("Microdata check failed: ", label)
  }
}
# The aggregate files store rates and intervals to six decimals
TOL6 <- 5e-7 + 1e-9

# Manuscript numbers: \newcommand macros and generated table rows
macros <- local({
  x <- unlist(lapply(list.files(GEN, "macros\\.tex$", full.names = TRUE), readLines))
  setNames(sub("^\\\\newcommand\\{\\\\[A-Za-z]+\\}\\{(.*)\\}$", "\\1", x),
           sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", x))
})
tex_rows <- function(file) trimws(readLines(file.path(GEN, file)))
mac <- function(names) macros[names]

LABEL <- c(low = "0 recorded", middle = "1--4", high = "5+")
SLAVE_LABEL <- c(low = "0", middle = "1-4", high = "5+")

# -----------------------------------------------------------------------------
#  0. Files and the exclusion rule
# -----------------------------------------------------------------------------
cat("\n0. Microdata files\n")
cohort   <- read_micro("cohort_1712.csv", col_types = cols(.default = "?",
                       earliest_evidence_date = "c", earliest_document_date = "c", wife_name = "c",
                       blank_asset_cells = "c", event_interval = "c", date_precision = "c"))
baseline <- read_register("baseline_rows.csv")
links    <- bind_rows(read_register("death_links.csv") |> mutate(frame = "MOOC8"),
                      read_register("stellenbosch_death_links.csv") |> mutate(frame = "Stellenbosch"))
candidates <- read_register("additional_widow_candidates.csv")
panel    <- read_micro("continuation_panel.csv", col_types = cols(.default = "?",
                       name_candidate_examples = "c", fallback_candidate_examples = "c"))
banned <- c("individual_id", "couple_id", "family_id", "saf", "saf_present", "persid")
for (f in list.files(MICRO, "\\.csv$"))
  same(paste("no SAF field in", f), any(tolower(names(read_csv(file.path(MICRO, f), n_max = 0))) %in% banned), FALSE)

same("Raw male-name rows in the 1712 register", nrow(baseline), as.integer(mac("RawEntries")))
same("Cohort = register rows with decision 'include'",
     sort(cohort$hhobs), sort(as.integer(baseline$hhobs[baseline$decision == "include"])))
same("Excluded rows", sum(baseline$decision != "include"), as.integer(mac("ExcludedEntries")))
same("Cohort size", nrow(cohort), as.integer(mac("CohortN")))

# -----------------------------------------------------------------------------
#  1. Death flags rebuilt from the accepted links
# -----------------------------------------------------------------------------
cat("\n1. Death flags from the accepted links\n")
links <- links |> mutate(baseline_hhobs = as.integer(baseline_hhobs),
                         year = as.integer(substr(date_value, 1, 4)))
accepted <- links |> filter(decision == "accept")
stopifnot(all(accepted$event_interval == "after1712_by1714"), all(accepted$year %in% 1713:1714))
flags <- function(acc, ids = cohort$hhobs) tibble(
  hhobs = ids,
  probate = ids %in% acc$baseline_hhobs[acc$channel %in% c("probate", "probate_incidental")],
  widow   = ids %in% acc$baseline_hhobs[acc$channel == "widow"]) |>
  mutate(any = probate | widow)
f <- flags(accepted)
same("probate_record equals the rebuilt flag", cohort$probate_record, as.integer(f$probate))
same("widow_record equals the rebuilt flag", cohort$widow_record, as.integer(f$widow))
same("any_record equals the rebuilt flag", cohort$any_record, as.integer(f$any))
same("Confirmed deaths", sum(f$any), as.integer(mac("ConfirmedN")))
# Earliest evidence: the earliest year over accepted links, to the day only
# when every accepted link in that year is a dated MOOC8 document.
ev <- accepted |> group_by(hhobs = baseline_hhobs) |>
  filter(year == min(year)) |>
  summarise(date = if (all(nchar(date_value) == 8))
              format(as.Date(min(date_value), "%Y%m%d"), "%Y-%m-%d") else as.character(first(year)),
            precision = if (all(nchar(date_value) == 8)) "day" else "year")
chk <- cohort |> filter(any_record == 1) |> left_join(ev, by = "hhobs")
same("earliest_evidence_date rebuilt from the links", chk$earliest_evidence_date, chk$date)
same("date_precision rebuilt from the links", chk$date_precision, chk$precision)
docd <- accepted |> filter(nchar(date_value) == 8) |> group_by(hhobs = baseline_hhobs) |>
  summarise(doc = format(as.Date(min(date_value), "%Y%m%d"), "%Y-%m-%d"))
chk <- cohort |> left_join(docd, by = "hhobs")
same("earliest_document_date rebuilt from the links", coalesce(chk$earliest_document_date, ""), coalesce(chk$doc, ""))

# -----------------------------------------------------------------------------
#  2. Table 1. Blank asset cells are zero (see asset_column_coverage_1712.csv).
# -----------------------------------------------------------------------------
cat("\n2. Table 1: the 1712 cohort\n")
same("adult_slaves = slave_men + slave_women", cohort$adult_slaves, cohort$slave_men + cohort$slave_women)
same("cattle = cattle_cows + cattle_work", cohort$cattle, cohort$cattle_cows + cohort$cattle_work)
same("resource_index = slaves + 0.5 (cattle + horses) + 0.1 sheep", cohort$resource_index,
     cohort$adult_slaves + 0.5 * (cohort$cattle + cohort$horses) + 0.1 * cohort$sheep, 1e-9)
grp <- with(cohort, if_else(adult_slaves == 0, "low", if_else(adult_slaves <= 4, "middle", "high")))
same("resource_group follows 0 / 1-4 / 5+", cohort$resource_group, grp)
cohort <- cohort |> mutate(group = factor(resource_group, GROUP_LEVELS))

t1 <- cohort |> group_by(group) |>
  summarise(men = n(), mean_adult_slaves = mean(adult_slaves), mean_cattle = mean(cattle),
            mean_sheep = mean(sheep), mean_resource_index = mean(resource_index),
            wife_named = sum(wife_named)) |>
  mutate(share = men / sum(men))
agg1 <- read_data("cohort_by_resource_group.csv") |> mutate(group = factor(group, GROUP_LEVELS)) |>
  arrange(group)
same("Table 1 men", t1$men, agg1$men)
same("Table 1 wife named", t1$wife_named, agg1$wife_named)
for (v in c("share", "mean_adult_slaves", "mean_cattle", "mean_sheep", "mean_resource_index"))
  same(paste("Table 1", v), t1[[v]], agg1[[v]], TOL6)
same("Table 1 rows in the manuscript",
     sprintf("%s & %d & %s & %s & %s & %s & %d \\\\", LABEL[as.character(t1$group)], t1$men,
             fmt(100 * t1$share), fmt(t1$mean_adult_slaves), fmt(t1$mean_cattle),
             fmt(t1$mean_sheep, 0), t1$wife_named),
     tex_rows("descriptive_rows.tex"))

# -----------------------------------------------------------------------------
#  3. Table 2 and the recording model
# -----------------------------------------------------------------------------
cat("\n3. Table 2: death records by group\n")
RECORDS <- c(probate = "estate_document", widow = "widow_entry", any = "combined_death_record")
rates <- function(d, flagdf, by = "group") {
  x <- d |> select(hhobs, g = all_of(by)) |> left_join(flagdf, by = "hhobs")
  bind_rows(lapply(names(RECORDS), function(r) x |> group_by(group = g) |>
    summarise(men = n(), events = sum(.data[[r]])) |> mutate(record = RECORDS[[r]]))) |>
    mutate(rate = events / men, wilson(events, men)) |> rename(ci_lo = lo, ci_hi = hi)
}
agg2 <- read_data("death_records_by_group.csv")
compare_rates <- function(label, got, want) {
  want <- want |> mutate(group = as.character(group))
  got  <- got |> mutate(group = as.character(group)) |>
    inner_join(want, by = c("record", "group"), suffix = c("", ".agg"))
  stopifnot(nrow(got) == nrow(want))
  same(paste(label, "men"), got$men, got$men.agg)
  same(paste(label, "events"), got$events, got$events.agg)
  for (v in c("rate", "ci_lo", "ci_hi")) same(paste(label, v), got[[v]], got[[paste0(v, ".agg")]], TOL6)
}
r_all <- rates(cohort, f)
compare_rates("Table 2", r_all, agg2 |> filter(sample == "all_men", record != "genealogical_presence"))

# Genealogical presence (SAF) is NOT reproducible from the release: taken from the aggregate file.
saf <- agg2 |> filter(sample == "all_men", record == "genealogical_presence") |>
  mutate(group = factor(group, GROUP_LEVELS)) |> arrange(group)
cat("  SAF presence column: read from data/death_records_by_group.csv (not reproducible)\n")
cell <- function(e, n, rate, lo, hi) sprintf("\\shortstack{%d/%d\\\\%s [%s, %s]}", e, n,
                                             fmt(100 * rate), fmt(100 * lo), fmt(100 * hi))
t2_rows <- vapply(GROUP_LEVELS, function(g) {
  x <- r_all |> filter(group == g)
  cells <- vapply(RECORDS, function(r) with(x[x$record == r, ], cell(events, men, rate, ci_lo, ci_hi)), "")
  s <- saf[saf$group == g, ]
  paste0(LABEL[[g]], " & ", paste(c(cells, cell(s$events, s$men, s$rate, s$ci_lo, s$ci_hi)), collapse = " & "),
         " \\\\ \\addlinespace[0.3em]")
}, "")
same("Table 2 rows in the manuscript", t2_rows, tex_rows("capture_rows.tex"))
title <- c(low = "Low", middle = "Middle", high = "High")
ch <- c(estate_document = "Probate", widow_entry = "Widow", combined_death_record = "Union")
for (g in GROUP_LEVELS) for (r in names(ch)) {
  x <- r_all[r_all$group == g & r_all$record == r, ]
  same(sprintf("Macros %s%s events and rate", title[[g]], ch[[r]]),
       c(x$events, fmt(100 * x$rate)),
       mac(paste0(title[[g]], ch[[r]], c("Events", "Rate"))))
}
for (g in GROUP_LEVELS) same(sprintf("Macros %sN and %sShare", title[[g]], title[[g]]),
  c(t1$men[t1$group == g], fmt(100 * t1$share[t1$group == g])), mac(paste0(title[[g]], c("N", "Share"))))

u  <- r_all |> filter(record == "combined_death_record")
xL <- u$events[u$group == "low"]; nL <- u$men[u$group == "low"]
xH <- u$events[u$group == "high"]; nH <- u$men[u$group == "high"]
r  <- (xL / nL) / (xH / nH)
se <- sqrt(1 / xL - 1 / nL + 1 / xH - 1 / nH)
same("Recording model macros",
     c(fmt(r, 2), fmt(r, 3), fmt(exp(log(r) - qnorm(.975) * se), 2), fmt(exp(log(r) + qnorm(.975) * se), 2),
       fmt(nH / xH, 2), fmt(1 / r, 1), fmt(r / .5, 2), fmt(r / .25, 2)),
     mac(c("UnionRatio", "UnionRatioThree", "UnionRatioLo", "UnionRatioHi", "UnionUpper",
           "CaptureEqualityMultiple", "HalfCaptureRatio", "QuarterCaptureRatio")))

# -----------------------------------------------------------------------------
#  4. Men with a wife named
# -----------------------------------------------------------------------------
cat("\n4. Men with a wife named\n")
r_wife <- rates(cohort |> filter(wife_named == 1), f)
compare_rates("Wife named", r_wife, agg2 |> filter(sample == "wife_named", record != "genealogical_presence"))
for (g in GROUP_LEVELS) {
  x <- r_wife |> filter(group == g, record == "combined_death_record")
  same(sprintf("Macros Wife%s N, events, rate", title[[g]]), c(x$men, x$events, fmt(100 * x$rate)),
       mac(paste0("Wife", title[[g]], c("N", "Events", "Rate"))))
}

# -----------------------------------------------------------------------------
#  5. Alternative resource index: empirical thirds, equal values kept together
# -----------------------------------------------------------------------------
cat("\n5. Alternative resource index\n")
cuts <- quantile(cohort$resource_index, c(1/3, 2/3), names = FALSE, type = 7)
ig <- with(cohort, if_else(resource_index <= cuts[1], "low", if_else(resource_index <= cuts[2], "middle", "high")))
same("resource_index_group follows the tertile cuts", cohort$resource_index_group, ig)
agg5 <- read_data("resource_index_death_records.csv")
same("Index ranges", unique(agg5$index_range[order(match(agg5$group, GROUP_LEVELS))]),
     c(paste("<=", cuts[1]), paste(">", cuts[1], "and <=", cuts[2]), paste(">", cuts[2])))
r_idx <- rates(cohort, f, by = "resource_index_group")
compare_rates("Resource index", r_idx, agg5 |> filter(record != "genealogical_presence") |> select(-index_range))
iu <- r_idx |> filter(record == "combined_death_record")
for (g in GROUP_LEVELS) same(sprintf("Macros Wealth%s N and rate", title[[g]]),
  c(iu$men[iu$group == g], fmt(100 * iu$rate[iu$group == g])), mac(paste0("Wealth", title[[g]], c("N", "Rate"))))
same("Macro WealthUnionRatio", fmt(iu$rate[iu$group == "low"] / iu$rate[iu$group == "high"], 2),
     mac("WealthUnionRatio"))

# -----------------------------------------------------------------------------
#  6. Alternative documentary rules
# -----------------------------------------------------------------------------
cat("\n6. Source rules\n")
rules <- list(records_dated_1713 = accepted |> filter(year == 1713),
              records_dated_1713_14 = accepted,
              excluding_incidental_mentions = accepted |> filter(channel != "probate_incidental"))
agg6 <- read_data("source_rule_death_records.csv")
rule_label <- c(records_dated_1713 = "Records dated 1713", records_dated_1713_14 = "Records dated 1713--14",
                excluding_incidental_mentions = "1713--14 excluding incidental mentions")
rows6 <- character()
for (s in names(rules)) {
  x <- rates(cohort, flags(rules[[s]])) |> filter(record == "combined_death_record")
  compare_rates(paste("Rule", s), x, agg6 |> filter(source_rule == s) |>
                  mutate(record = "combined_death_record") |> select(-source_rule, -adult_slaves))
  x <- x |> mutate(group = factor(group, GROUP_LEVELS)) |> arrange(group)
  rows6 <- c(rows6, paste0(rule_label[[s]], " & ",
    paste(sprintf("%d/%d (%s)", x$events, x$men, fmt(100 * x$rate)), collapse = " & "), " \\\\"))
}
same("Source-rule rows in the manuscript", rows6, tex_rows("ascertainment_rows.tex"))

# -----------------------------------------------------------------------------
#  7. Identity scenarios, ambiguity bounds and repeated-name denominators
# -----------------------------------------------------------------------------
cat("\n7. Identity scenarios and denominators\n")
# Ambiguous links that would add a person: the Lombart widow
ambiguous_people <- setdiff(links$baseline_hhobs[links$decision == "ambiguous"], accepted$baseline_hhobs)
same("People added only by ambiguous links", length(ambiguous_people), as.integer(mac("AmbiguousPeople")))
possible <- flags(links |> filter(decision %in% c("accept", "ambiguous")))
amb <- cohort |> select(hhobs, group) |> left_join(possible, by = "hhobs") |> left_join(
  f |> select(hhobs, confirmed = any), by = "hhobs") |> group_by(group) |>
  summarise(n = n(), confirmed = sum(confirmed), possible = sum(any))
lo <- with(amb, (confirmed[group == "low"] / n[group == "low"]) / (possible[group == "high"] / n[group == "high"]))
hi <- with(amb, (possible[group == "low"] / n[group == "low"]) / (confirmed[group == "high"] / n[group == "high"]))
same("Ambiguity ratio bounds", c(fmt(lo, 2), fmt(hi, 2)), mac(c("AmbiguityRatioLo", "AmbiguityRatioHi")))

stopifnot(length(unique(candidates$widow_hhobs)) == 1, length(ambiguous_people) == 1)
cand <- candidates |> mutate(baseline_hhobs = as.integer(baseline_hhobs),
  assignment = if_else(grepl("de jonge", candidate), "younger_candidate", "older_candidate"))
scen <- bind_rows(lapply(c("unassigned", cand$assignment), function(a) bind_rows(lapply(c(FALSE, TRUE), function(lb) {
  extra <- c(cand$baseline_hhobs[cand$assignment == a], if (lb) ambiguous_people)
  d <- cohort |> mutate(e = any_record == 1 | hhobs %in% extra) |> group_by(group) |>
    summarise(n = n(), events = sum(e))
  ratio <- (d$events[d$group == "low"] / d$n[d$group == "low"]) / (d$events[d$group == "high"] / d$n[d$group == "high"])
  tibble(roemond_assignment = a, lombart_added = lb, confirmed_total = sum(d$events),
         events_low = d$events[d$group == "low"], events_middle = d$events[d$group == "middle"],
         events_high = d$events[d$group == "high"], ratio_low_high = ratio,
         equal_mortality_threshold = 1 / ratio)
  }))))
agg7 <- read_data("identity_scenarios.csv")
key <- c("roemond_assignment", "lombart_added")
cmp <- scen |> inner_join(agg7, by = key, suffix = c("", ".agg"))
stopifnot(nrow(cmp) == nrow(agg7), nrow(agg7) == 6)
for (v in c("confirmed_total", "events_low", "events_middle", "events_high"))
  same(paste("Identity scenarios", v), cmp[[v]], cmp[[paste0(v, ".agg")]])
for (v in c("ratio_low_high", "equal_mortality_threshold"))
  same(paste("Identity scenarios", v), cmp[[v]], cmp[[paste0(v, ".agg")]], TOL6)
id_label <- c(unassigned = "Unassigned", older_candidate = "Older Michiel", younger_candidate = "Younger Michiel")
scen_tex <- scen |> mutate(o = match(roemond_assignment, names(id_label))) |> arrange(o, lombart_added)
# round(., 9): 27/8 = 3.375 is 3.37499999... in floating point and must print as 3.38
same("Identity rows in the manuscript",
     sprintf("%s%s & %d/%d & %d/%d & %d/%d & %s & %s \\\\", id_label[scen_tex$roemond_assignment],
             if_else(scen_tex$lombart_added, " + Lombart", ""), scen_tex$events_low, t1$men[1],
             scen_tex$events_middle, t1$men[2], scen_tex$events_high, t1$men[3],
             fmt(scen_tex$ratio_low_high, 3), fmt(round(scen_tex$equal_mortality_threshold, 9), 2)),
     tex_rows("identity_rows.tex"))
same("Identity macros",
     c(fmt(min(scen$ratio_low_high), 3), fmt(max(scen$ratio_low_high), 3),
       fmt(min(scen$equal_mortality_threshold), 2), fmt(max(scen$equal_mortality_threshold), 2)),
     mac(c("IdentityRatioLo", "IdentityRatioHi", "IdentityThresholdLo", "IdentityThresholdHi")))

dup <- baseline |> filter(decision == "exclude_identity_unresolved")
stopifnot(all(dup$group == "low"), !any(as.integer(dup$hhobs) %in% links$baseline_hhobs[links$decision != "reject"]))
den <- tibble(specification = c("primary", "one_person_per_unresolved_pair", "two_people_per_unresolved_pair"),
              extra = c(0, length(unique(dup$duplicate_set)), nrow(dup))) |>
  mutate(men = nrow(cohort) + extra, men_low = nL + extra, ratio_low_high = (xL / men_low) / (xH / nH))
agg7b <- read_data("denominator_sensitivity.csv")
same("Denominator men", den$men, agg7b$men)
same("Denominator men_low", den$men_low, agg7b$men_low)
same("Denominator ratio", den$ratio_low_high, agg7b$ratio_low_high, TOL6)
same("Denominator macros", c(fmt(min(den$ratio_low_high), 3), fmt(max(den$ratio_low_high), 3)),
     mac(c("DenomMinRatio", "DenomMaxRatio")))

# -----------------------------------------------------------------------------
#  8. Record continuation: cells, contrasts and clustered standard errors.
#     A flag is NOT a death.
#
#  For flags y1 (and a second rule y2 for the rule contrasts), each window w
#  and group g (low or owner) has rates p1 = mean(y1), p2 = mean(y2). The
#  common-weight contrast is theta = sum_wg c_wg log(p1_wg / p2_wg), with
#  c = +1 for 1713 and -1/4 for the four other windows, times +1 for low and
#  -1 for owner. Each entry i in cell wg contributes the influence
#    c_wg ((y1_i - p1_wg) / p1_wg - (y2_i - p2_wg) / p2_wg) / n_wg,
#  which pairs the two rules within the entry. The influences are summed by
#  full-name cluster key across windows, and
#    se = sqrt(G / (G - 1) * sum_k U_k^2)   (G = number of clusters).
#  This is the estimator in R/current_panel.R and Online Appendix D.
# -----------------------------------------------------------------------------
cat("\n8. Record continuation\n")
RULES <- c(guarded_names = "flag_guarded_names", permissive_fallback = "flag_permissive_fallback",
           unique_target = "flag_unique_target")
cells <- bind_rows(lapply(names(RULES), function(r) panel |>
  group_by(specification, window, group) |>
  summarise(entries = n(), flags = sum(.data[[RULES[[r]]]])) |> mutate(rule = r)))
agg8 <- read_data("continuation_cells.csv")
cmp <- agg8 |> left_join(cells, by = c("specification", "window", "group", "rule"), suffix = c(".agg", ""))
stopifnot(nrow(cmp) == nrow(cells), nrow(cmp) == 120)
same("Continuation entries (120 cells)", cmp$entries, cmp$entries.agg)
same("Continuation flags (120 cells)", cmp$flags, cmp$flags.agg)
same("Continuation rates (120 cells)", cmp$flags / cmp$entries, cmp$rate, TOL6)
win <- read_data("continuation_windows.csv")
base_years <- panel |> distinct(specification, window, baseline_year) |>
  arrange(baseline_year) |> group_by(specification, window) |>
  summarise(baseline_rolls = paste(baseline_year, collapse = ";"))
cmpw <- win |> left_join(base_years, by = c("specification", "window"), suffix = c(".agg", ""))
same("Baseline rolls per window", cmpw$baseline_rolls, cmpw$baseline_rolls.agg)

prim <- cells |> ungroup() |> filter(specification == "primary") |>
  mutate(cell = paste0(flags, "/", entries), col = paste(rule, group)) |>
  select(window, col, cell) |> pivot_wider(names_from = col, values_from = cell) |> arrange(window)
same("Continuation cell rows in the manuscript",
     with(prim, sprintf("%d & %s & %s & %s & %s & %s & %s \\\\", window, `guarded_names low`,
                        `guarded_names owner`, `permissive_fallback low`, `permissive_fallback owner`,
                        `unique_target low`, `unique_target owner`)),
     tex_rows("panel_cell_rows.tex"))
p13 <- panel |> filter(specification == "primary", window == 1713)
reclass <- p13 |> filter(flag_guarded_names == 1, flag_permissive_fallback == 0)
same("Panel macros: baseline N, fallback reclassified, multiple candidates",
     c(nrow(p13), nrow(reclass), sum(reclass$fallback_candidate_keys > 1)),
     as.integer(mac(c("PanelBaselineN", "PanelFallbackReclassified", "PanelFallbackMultiple"))))
same("Nested rules: permissive <= guarded <= unique",
     all(panel$flag_permissive_fallback <= panel$flag_guarded_names &
           panel$flag_guarded_names <= panel$flag_unique_target), TRUE)

contrast <- function(d, y1, y2 = NULL) {
  d <- d |> mutate(y1 = .data[[y1]], y2 = if (is.null(y2)) 1 else .data[[y2]])
  c <- d |> group_by(window, group) |> summarise(n = n(), p1 = mean(y1), p2 = mean(y2), .groups = "drop") |>
    mutate(weight = if_else(window == 1713, 1, -0.25) * if_else(group == "low", 1, -1))
  stopifnot(all(c$p1 > 0), all(c$p2 > 0), n_distinct(c$window) == 5)
  theta <- sum(c$weight * log(c$p1 / c$p2))
  u <- d |> inner_join(c, by = c("window", "group")) |>
    mutate(infl = weight * ((y1 - p1) / p1 - (if (is.null(y2)) 0 else (y2 - p2) / p2)) / n) |>
    group_by(cluster_key) |> summarise(u = sum(infl))
  G <- nrow(u); se <- sqrt(sum(u$u^2) * G / (G - 1))
  tibble(estimate = theta, se = se, ci_lo = theta - 1.96 * se, ci_hi = theta + 1.96 * se,
         p = 2 * pnorm(-abs(theta / se)), clusters = G)
}
est <- bind_rows(lapply(unique(panel$specification), function(s) {
  d <- panel |> filter(specification == s)
  bind_rows(
    contrast(d, "flag_guarded_names") |> mutate(quantity = "DD_name"),
    contrast(d, "flag_permissive_fallback") |> mutate(quantity = "DD_permissive"),
    contrast(d, "flag_unique_target") |> mutate(quantity = "DD_unique"),
    contrast(d, "flag_permissive_fallback", "flag_guarded_names") |> mutate(quantity = "permissive_minus_name"),
    contrast(d, "flag_unique_target", "flag_guarded_names") |> mutate(quantity = "unique_minus_name")) |>
    mutate(specification = s)
}))
agg8e <- read_data("continuation_estimates.csv")
cmp <- agg8e |> left_join(est, by = c("specification", "quantity"), suffix = c(".agg", ""))
stopifnot(nrow(cmp) == 20)
for (v in c("estimate", "se", "ci_lo", "ci_hi", "p"))
  same(paste("Continuation", v, "(20 contrasts)"), cmp[[v]], cmp[[paste0(v, ".agg")]], TOL6)
same("Continuation clusters (20 contrasts)", cmp$clusters, cmp$clusters.agg)

q_label <- c(DD_name = "Guarded names", DD_permissive = "Names plus permissive fallback",
             DD_unique = "Unique target name", permissive_minus_name = "Permissive minus guarded",
             unique_minus_name = "Unique minus guarded")
e1 <- est |> filter(specification == "primary") |> arrange(match(quantity, names(q_label)))
same("Continuation rows in the manuscript",
     sprintf("%s & %s & %s & %s \\\\", q_label[e1$quantity], fmt(e1$estimate, 3), fmt(e1$se, 3), fmt(e1$p, 3)),
     tex_rows("panel_rows.tex"))
spec_label <- c(primary = "Latest roll, two-year search", three_year_baseline = "Three-year baseline, two-year search",
                long_search = "Latest roll, longer search",
                three_year_baseline_long_search = "Three-year baseline, longer search")
e2 <- est |> filter(quantity == "permissive_minus_name") |> arrange(match(specification, names(spec_label)))
same("Continuation sensitivity rows in the manuscript",
     sprintf("%s & %s & %s & %s \\\\", spec_label[e2$specification], fmt(e2$estimate, 3), fmt(e2$se, 3), fmt(e2$p, 3)),
     tex_rows("panel_sensitivity_rows.tex"))
prefix <- c(DD_name = "PanelName", DD_permissive = "PanelPermissive", DD_unique = "PanelUnique",
            permissive_minus_name = "PanelContrast", unique_minus_name = "PanelUniqueContrast")
for (q in names(prefix)) {
  x <- e1[e1$quantity == q, ]
  same(paste("Macros", prefix[[q]]), fmt(c(x$estimate, x$se, x$p, x$ci_lo, x$ci_hi), 3),
       mac(paste0(prefix[[q]], c("Estimate", "SE", "P", "Lo", "Hi"))))
}

cat(sprintf("\nAll %d microdata checks match the aggregate files and the manuscript.\n", n_checks))
cat("Not reproducible from this package: the SAF genealogical-presence column (read from\n",
    "data/death_records_by_group.csv) and the source reading behind each register decision.\n", sep = "")

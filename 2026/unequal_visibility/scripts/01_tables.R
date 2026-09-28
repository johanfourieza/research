# =============================================================================
#  01_tables.R
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Rebuilds every table and every number in the main text from the aggregate
#  files in data/, and checks each one against the value printed in the paper:
#    - the two administrative series (Section 2.3)
#    - Table 1, the 1712 cohort by recorded adult slaveholding
#    - Table 2, confirmed death records and genealogical presence, with the
#      Wilson intervals recomputed from the counts
#    - the comparison restricted to men with a wife named (Section 3.2)
#    - the recording model (Section 4): the recorded-death ratio, its interval,
#      the equal-mortality threshold, the implied ratios at half and quarter
#      recovery, and the bound under C_L <= C_H
#    - the online-appendix sensitivity checks (Tables S1 and S2, the resource
#      index and the repeated-name denominators)
#  Tables are written to output/tables/.
#
#  HOW TO USE IT
#  From the package root:  Rscript scripts/01_tables.R
# =============================================================================

source(file.path(if (dir.exists("scripts")) "scripts" else ".", "00_setup.R"))

# -----------------------------------------------------------------------------
#  1. The epidemic in the administrative series (Figure 1 and Section 2.3).
#     These series use no person linkage.
# -----------------------------------------------------------------------------
cat("\n1. Administrative series\n")
docs    <- read_data("probate_documents_by_year.csv")
widows  <- read_data("widow_entries_by_year.csv")

docs_1713  <- docs$documents[docs$year == 1713]
neighbours <- docs$documents[docs$year %in% c(1710:1712, 1714:1716)]
stopifnot(length(neighbours) == 6)
w1713      <- widows[widows$year == 1713, ]
w_baseline <- widows[widows$year %in% 1708:1712, ]

check("MOOC8 documents dated 1713", docs_1713, 54, 0)
check("Mean documents in the three years either side", mean(neighbours), 14.5, 1)
check("Ratio of 1713 to that mean", docs_1713 / mean(neighbours), 3.7, 1)
check("Widow entries in 1713", w1713$widows, 21, 0)
check("Enumerated entries in 1713", w1713$entries, 305, 0)
check("Widow share in 1713 (%)", 100 * w1713$widows / w1713$entries, 6.9, 1)
check("Pooled widow share 1708-12 (%)",
      100 * sum(w_baseline$widows) / sum(w_baseline$entries), 2.6, 1)

# -----------------------------------------------------------------------------
#  2. Table 1. The cohort is 348 individually named men from the 1712 roll.
#     Blank asset cells are zero under the transcription's documented rule;
#     data/asset_column_coverage_1712.csv shows that every asset column used
#     has positive entries in both districts, so the rule applies.
# -----------------------------------------------------------------------------
cat("\n2. Table 1: the 1712 cohort\n")
cohort <- read_data("cohort_by_resource_group.csv") |>
  mutate(group = factor(group, GROUP_LEVELS)) |> arrange(group)
coverage <- read_data("asset_column_coverage_1712.csv")
stopifnot(all(coverage$positive_entries > 0))

table1 <- cohort |>
  transmute(`Adult slaves` = adult_slaves, Men = men,
            `Share (%)` = round(100 * share, 1),
            `Mean slaves` = round(mean_adult_slaves, 1),
            `Mean cattle` = round(mean_cattle, 1),
            `Mean sheep` = round(mean_sheep, 0),
            `Wife named` = wife_named)
print(as.data.frame(table1), row.names = FALSE)
write_csv(table1, file.path(OUT_TAB, "table1_cohort.csv"))
check("Men in the cohort", sum(cohort$men), 348, 0)
check("Share with no adult slaves recorded (%)", 100 * cohort$share[1], 69.8, 1)

# -----------------------------------------------------------------------------
#  3. Table 2. A man counts once in the combined record even when both the
#     estate papers and a widow entry confirm his death. Genealogical presence
#     is an existing South African Families link: it is not a death outcome.
# -----------------------------------------------------------------------------
cat("\n3. Table 2: confirmed death records by group\n")
records <- read_data("death_records_by_group.csv") |>
  mutate(group = factor(group, GROUP_LEVELS))

# Recompute every Wilson interval from the counts and confirm the released values
recomputed <- records |> mutate(wilson(events, men))
stopifnot(max(abs(recomputed$lo - recomputed$ci_lo)) < 1e-6,
          max(abs(recomputed$hi - recomputed$ci_hi)) < 1e-6)

all_men <- records |> filter(sample == "all_men")
cell <- function(r) sprintf("%d/%d; %.1f [%.1f, %.1f]", r$events, r$men,
                            100 * r$rate, 100 * r$ci_lo, 100 * r$ci_hi)
table2 <- all_men |>
  mutate(cell = cell(pick(everything()))) |>
  select(adult_slaves, group, record, cell) |>
  pivot_wider(names_from = record, values_from = cell) |>
  arrange(group) |>
  select(`Adult slaves` = adult_slaves, `MOOC8 death confirmation` = estate_document,
         `Widow entry` = widow_entry, `Combined death record` = combined_death_record,
         `SAF presence` = genealogical_presence)
print(as.data.frame(table2), row.names = FALSE)
write_csv(table2, file.path(OUT_TAB, "table2_death_records.csv"))

rate <- function(df, rec, grp) df$rate[df$record == rec & df$group == grp]
count <- function(df, rec, grp, what) df[[what]][df$record == rec & df$group == grp]
check("Confirmed deaths, all groups",
      sum(all_men$events[all_men$record == "combined_death_record"]), 33, 0)
check("Combined rate, 5+ (%)", 100 * rate(all_men, "combined_death_record", "high"), 22.2, 1)
check("Combined rate, 1-4 (%)", 100 * rate(all_men, "combined_death_record", "middle"), 11.7, 1)
check("Combined rate, 0 (%)", 100 * rate(all_men, "combined_death_record", "low"), 6.6, 1)
check("Estate documents, 0 (%)", 100 * rate(all_men, "estate_document", "low"), 5.8, 1)
check("Estate documents, 5+ (%)", 100 * rate(all_men, "estate_document", "high"), 17.8, 1)
check("Widow entries, 0 (%)", 100 * rate(all_men, "widow_entry", "low"), 1.6, 1)
check("Widow entries, 5+ (%)", 100 * rate(all_men, "widow_entry", "high"), 11.1, 1)
check("SAF presence, 0 (%)", 100 * rate(all_men, "genealogical_presence", "low"), 3.7, 1)
check("SAF presence, 5+ (%)", 100 * rate(all_men, "genealogical_presence", "high"), 26.7, 1)

# -----------------------------------------------------------------------------
#  4. Men with a wife named (Section 3.2). The restriction holds one aspect of
#     family documentation more nearly constant; it is not an adjusted
#     mortality comparison.
# -----------------------------------------------------------------------------
cat("\n4. Men with a wife named\n")
wife <- records |> filter(sample == "wife_named")
check("Wife named, 0 group (men)", count(wife, "combined_death_record", "low", "men"), 74, 0)
check("Wife named, 5+ group (men)", count(wife, "combined_death_record", "high", "men"), 41, 0)
check("Combined rate among wife named, 0 (%)",
      100 * rate(wife, "combined_death_record", "low"), 14.9, 1)
check("Combined rate among wife named, 5+ (%)",
      100 * rate(wife, "combined_death_record", "high"), 22.0, 1)

# -----------------------------------------------------------------------------
#  5. The recording model (Section 4 and Online Appendix B).
#     D_w = M_w * C_w: the recorded-death probability is mortality times the
#     probability that a death is recovered. The mortality ratio is therefore
#     theta = (D_L / D_H) / kappa, where kappa = C_L / C_H.
# -----------------------------------------------------------------------------
cat("\n5. The recording model\n")
xL <- count(all_men, "combined_death_record", "low", "events")
nL <- count(all_men, "combined_death_record", "low", "men")
xH <- count(all_men, "combined_death_record", "high", "events")
nH <- count(all_men, "combined_death_record", "high", "men")
DL <- xL / nL; DH <- xH / nH
r  <- DL / DH                                   # observed recorded-death ratio

# Delta-method interval on the log scale, conditional on the accepted links
se_log <- sqrt(1 / xL - 1 / nL + 1 / xH - 1 / nH)
ci <- exp(log(r) + c(-1, 1) * qnorm(0.975) * se_log)

check("Recorded-death ratio, low / high", r, 0.296, 3)
check("  95% interval, lower", ci[1], 0.14, 2)
check("  95% interval, upper", ci[2], 0.61, 2)
# Equal mortality (theta = 1) requires kappa = r: higher-resource deaths must be
# 1 / r times as likely to be recovered. This is a threshold, not an estimate.
check("Equal-mortality threshold, 1 / r", 1 / r, 3.4, 1)
check("Implied mortality ratio if kappa = 1/2", r / 0.5, 0.59, 2)
check("Implied mortality ratio if kappa = 1/4", r / 0.25, 1.19, 2)
# Imposing C_L <= C_H gives the sharp set [r, 1 / D_H] (Online Appendix B)
check("Bound under C_L <= C_H, lower", r, 0.296, 3)
check("Bound under C_L <= C_H, upper", 1 / DH, 4.50, 2)

model <- tibble(quantity = c("recorded_ratio", "ratio_ci_lo", "ratio_ci_hi",
                             "equal_mortality_threshold", "theta_half_recovery",
                             "theta_quarter_recovery", "bound_lower", "bound_upper"),
                value = c(r, ci, 1 / r, r / 0.5, r / 0.25, r, 1 / DH))
write_csv(model, file.path(OUT_TAB, "recording_model.csv"))

# -----------------------------------------------------------------------------
#  6. Online appendix sensitivity checks
# -----------------------------------------------------------------------------
cat("\n6. Online appendix: alternative resource index\n")
# Index = adult slaves + 0.5 x (cattle + horses) + 0.1 x sheep, cut at empirical
# thirds (the ranges are in index_range). It is a scale index, not market wealth.
idx <- read_data("resource_index_death_records.csv") |> filter(record == "combined_death_record")
check("Men per index group: low", idx$men[idx$group == "low"], 151, 0)
check("Men per index group: middle", idx$men[idx$group == "middle"], 84, 0)
check("Men per index group: high", idx$men[idx$group == "high"], 113, 0)
check("Index low / high ratio",
      idx$rate[idx$group == "low"] / idx$rate[idx$group == "high"], 0.07, 2)

cat("\n   Table S1: combined record under alternative source rules\n")
rules <- read_data("source_rule_death_records.csv") |>
  mutate(group = factor(group, GROUP_LEVELS),
         cell = sprintf("%d/%d (%.1f)", events, men, 100 * rate)) |>
  select(source_rule, group, cell) |>
  pivot_wider(names_from = group, values_from = cell)
print(as.data.frame(rules), row.names = FALSE)
write_csv(rules, file.path(OUT_TAB, "tableS1_source_rules.csv"))

cat("\n   Table S2: unresolved widow assignments\n")
ids <- read_data("identity_scenarios.csv")
print(as.data.frame(ids), row.names = FALSE)
write_csv(ids, file.path(OUT_TAB, "tableS2_identity_scenarios.csv"))
check("Lowest ratio across identity scenarios", min(ids$ratio_low_high), 0.269, 3)
check("Highest ratio across identity scenarios", max(ids$ratio_low_high), 0.315, 3)
check("Primary scenario threshold",
      ids$equal_mortality_threshold[ids$roemond_assignment == "unassigned" &
                                      !ids$lombart_added], 3.375, 3)

cat("\n   Repeated-name denominators\n")
print(as.data.frame(read_data("denominator_sensitivity.csv")), row.names = FALSE)

cat("\nAll table values match the paper.\n")

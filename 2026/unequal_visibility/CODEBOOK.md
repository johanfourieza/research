# Codebook — 2026/unequal_visibility

All data are **aggregate** (counts, rates and estimates); no file contains
personal names or individual source rows. A machine-readable version of this
dictionary is in `docs/variable_definitions.csv`.

## Conventions

- **Cohort.** 348 individually named men in the 1712 Stellenbosch–Drakenstein
  tax roll. Six of 354 male-name rows are excluded: a joint entry, an estate
  representative and two unresolved repeated-name pairs (see
  `denominator_sensitivity.csv`).
- **`group`** is the primary resource grouping by adult slaves recorded in
  1712: `low` = none, `middle` = 1–4, `high` = 5 or more. `adult_slaves` gives
  the same grouping as a label (`0`, `1-4`, `5+`). Blank asset cells are zero
  under the transcription's documented rule (see
  `asset_column_coverage_1712.csv`).
- **`record`** names the evidence:
  - `estate_document`: a MOOC8 estate document dated 1713–14 that confirms
    the man's death, including explicit mentions of his death in someone
    else's estate papers. Repeated documents count once per man.
  - `widow_entry`: a 1713 or 1714 roll entry for his widow.
  - `combined_death_record`: either of the two; each man counts once.
  - `genealogical_presence`: an existing South African Families link. **Not a
    death outcome.**
- **Rates and intervals.** `rate` = `events` / `men`. `ci_lo` and `ci_hi` are
  95 per cent Wilson intervals under a binomial reference model, conditional on
  the cohort and the accepted links. They do not cover unobserved deaths or
  uncertain identities.
- A man without a confirmed death is **unclassified**, not a survivor.

## probate_documents_by_year.csv
Dated MOOC8 documents per year across the Cape (Figure 1a). Documents, not
deaths: one death can produce several documents.
- `year`, `documents`

## widow_entries_by_year.csv
Widow entries in the Stellenbosch–Drakenstein rolls, 1705–1725 (Figure 1b).
Years without a surviving roll are absent, not zero.
- `year`
- `entries` — enumerated entries with a male name or widow status
- `widows` — entries with widow status
- `widow_share` — `widows` / `entries`

## cohort_by_resource_group.csv
The 1712 cohort (Table 1). One row per group. A row in the roll need not be an
independent household.
- `group`, `adult_slaves`, `men`
- `share` — share of the 348 men
- `mean_adult_slaves`, `mean_cattle` (cows and oxen), `mean_sheep`
- `mean_resource_index` — mean of adult slaves + 0.5 × (cattle + horses) + 0.1 × sheep
- `wife_named` — men with a wife recorded (a count of entries, not of marriages)

## death_records_by_group.csv
Confirmed death records and genealogical presence by group (Table 2 and
Section 3.2).
- `sample` — `all_men` (Table 2) or `wife_named` (men with a wife recorded)
- `record`, `group`, `adult_slaves`, `men`, `events`, `rate`, `ci_lo`, `ci_hi`

## resource_index_death_records.csv
The same outcomes under the alternative resource index, cut at empirical
thirds with equal values kept together (Online Appendix C).
- `record`, `group` (`low`/`middle`/`high` index group)
- `index_range` — the index values in the group
- `men`, `events`, `rate`, `ci_lo`, `ci_hi`

## source_rule_death_records.csv
The combined record under alternative documentary rules (Table S1).
- `source_rule` — `records_dated_1713` (evidence dated in 1713 only);
  `records_dated_1713_14` (primary); `excluding_incidental_mentions` (drops
  deaths known only from other people's estate papers)
- `group`, `adult_slaves`, `men`, `events`, `rate`, `ci_lo`, `ci_hi`

## identity_scenarios.csv
Mutually exclusive assignments of two unresolved widow entries (Table S2).
Identity scenarios, not confidence limits.
- `roemond_assignment` — the surname-only 1714 entry "wed Roemond" left
  `unassigned`, or assigned to the `older_candidate` (a baseline man whose
  resources match the widow's) or the `younger_candidate` (a namesake qualified
  *de jonge*); never both
- `lombart_added` — `true` if the unresolved Lombart widow is also assigned
- `confirmed_total`, `events_low`, `events_middle`, `events_high`
- `ratio_low_high` — low-to-high combined recorded-death ratio
- `equal_mortality_threshold` — 1 / `ratio_low_high`

## denominator_sensitivity.csv
The two unresolved repeated-name pairs, which have no death link, added to the
denominators.
- `specification` — `primary`; `one_person_per_unresolved_pair`;
  `two_people_per_unresolved_pair`
- `men`, `men_low`, `ratio_low_high`

## continuation_cells.csv
Record-continuation flags (Online Appendix D; Table S4 is the `primary` rows).
A flag means no later name met the rule's continuation criterion in the search
rolls. **A flag is not a death.**
- `specification` — `primary` (latest roll, two-year search);
  `three_year_baseline`; `long_search`; `three_year_baseline_long_search`
- `window` — 1709, 1710, 1713, 1718 or 1721
- `group` — `low` (no adult slaves recorded) or `owner` (at least one). Note
  that this grouping differs from the three-group `group` above.
- `rule` — `guarded_names`; `permissive_fallback` (also accepts a similar first
  name with adult slaveholding within two); `unique_target` (requires exactly one
  distinct candidate name)
- `entries`, `flags`, `rate`

## continuation_estimates.csv
Common-weight contrasts (Tables S3 and S5).
- `specification` — as above
- `quantity` — `DD_name`, `DD_permissive`, `DD_unique`: the 1713 low-to-owner
  log flag-rate gap minus the mean gap in 1709, 1710, 1718 and 1721, for each
  rule; `permissive_minus_name`, `unique_minus_name`: differences between rules
- `estimate`, `se`, `ci_lo`, `ci_hi`, `p`
- `clusters` — full-name-key clusters used for the standard errors

## continuation_windows.csv
The rolls used for each window.
- `specification`, `window`
- `baseline_rule` — `last_roll` or `three_year`
- `baseline_rolls`, `search_rolls` — semicolon-separated years
- `n_search_rolls`

## asset_column_coverage_1712.csv
Entries in each asset column of the 1712 transcription. When a column has
entries somewhere in a census, the transcription codes its blank cells as
zero; every column used here has positive entries in both districts.
- `district`, `variable`, `positive_entries`, `nonmissing`

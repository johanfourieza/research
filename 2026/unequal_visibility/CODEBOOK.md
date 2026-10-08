# Codebook — 2026/unequal_visibility

The files in `data/` are **aggregates** (counts, rates and estimates). The
files in `data/microdata/` are **person-level**: they name the men of the 1712
roll, their wives and the documents linked to them. No file holds a South
African Families field or identifier, or a source transcription. A
machine-readable version of this dictionary is in
`docs/variable_definitions.csv`.

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

# Person-level microdata (`data/microdata/`)

All files are UTF-8 CSV. Blank cells are missing. `hhobs` is the row
identifier of the Cape of Good Hope Panel transcription of the tax rolls; it
links the cohort, the registers and the continuation panel. Where a register
note cited a South African Families lineage code, the code is replaced by
`[genealogical identifier removed]`.

**Two source frames for estate documents.** `death_links.csv` and
`document_screening.csv` cite MOOC8 documents by XML file (`source_file`),
`div_id` (for example `MOOC8/3.56`) and an 8-digit date `YYYYMMDD`.
`stellenbosch_death_links.csv` and `stellenbosch_document_screening.csv` cite
schedules in the Stellenbosch compilation (`source_file` = `Vol I-V.doc`) by
the compilation's own record identifier (`STBVR_...`) and line range
(`STBV_<file hash>_P<paragraph>_L<line>`), with the schedule's year only. These
are **not** MOOC8 identifiers. Both frames feed the same estate-document flag;
a man counts once.

## cohort_1712.csv
One row per man in the cohort (348 rows: the `include` rows of
`baseline_rows.csv`).
- `hhobs` — panel row identifier (linkage key)
- `roll_year` — 1712
- `district`, `folio` — district and folio of the entry
- `source_workbook`, `source_sheet`, `raw_excel_row` — the transcription
  workbook, sheet and spreadsheet row of the entry
- `archive_reference` — archival reference of the roll
- `name` — the man's name as transcribed
- `wife_named` — 1 if a wife is recorded on the entry, else 0 (an entry, not a
  marriage record; a blank does not prove he was unmarried)
- `wife_name` — the wife's name as transcribed; blank if none
- `slave_men`, `slave_women` — adult enslaved men and women recorded
- `adult_slaves` — `slave_men` + `slave_women`
- `cattle_cows`, `cattle_work`, `cattle` — cows, oxen and their sum
- `horses`, `sheep`
- `blank_asset_cells` — the asset columns that are blank in the transcription
  (semicolon-separated). **Zero-coding rule:** a blank asset cell is coded 0,
  because every asset column used has entries in both districts in 1712 and the
  transcription leaves zeros blank (see `data/asset_column_coverage_1712.csv`).
  All asset counts above already apply the rule.
- `resource_group` — `low` (0 adult slaves), `middle` (1–4), `high` (5+)
- `resource_index` — `adult_slaves` + 0.5 × (`cattle` + `horses`) + 0.1 ×
  `sheep`; a scale index, not market wealth
- `resource_index_group` — empirical thirds of `resource_index` (type-7
  quantiles; equal values kept together: ≤ 0.5, ≤ 20, > 20)
- `probate_record` — 1 if an accepted link to an estate document dated 1713–14
  confirms his death (own estate, or an explicit mention of his death in
  someone else's estate papers; MOOC8 or Stellenbosch frame)
- `widow_record` — 1 if an accepted widow entry in the 1713 or 1714 roll is
  his widow
- `any_record` — `probate_record` or `widow_record` (the combined death record)
- `earliest_evidence_date` — the earliest accepted evidence of death. The year
  is the earliest year among his accepted links. The date is given to the day
  (`YYYY-MM-DD`) only when every accepted link in that year is a day-dated MOOC8
  document; otherwise it is the year (`YYYY`). **This is not a date of death.**
  An estate document can follow the death by months, and a widow entry shows
  only that the death preceded that year's roll.
- `date_precision` — `day` or `year`
- `earliest_document_date` — the earliest day-dated MOOC8 document among his
  accepted links, if any (`YYYY-MM-DD`)
- `event_interval` — `after1712_by1714` for every man with a death record:
  the death fell after the 1712 roll and no later than the 1714 evidence
- A man with `any_record` = 0 is **unclassified**, not a survivor.

## baseline_rows.csv
The reviewed decision for each of the 354 male-name rows of the 1712 roll.
- `nr`, `raw_order` — row order in the roll
- `names_men`, `names_women` — names as transcribed
- `hhobs`, `h_key` (normalised name key), `group`, `slaves`
- `raw_excel_row`, `district`, `folio`, `source_workbook`, `source_sheet`,
  `archive` — provenance
- `decision` — `include`; `exclude_joint` (a joint entry); `exclude_proxy` (an
  estate representative); `exclude_identity_unresolved` (two unresolved
  repeated-name pairs)
- `duplicate_set` — the repeated-name pair, where relevant
- `evidence` — the reason for the decision

## death_links.csv and stellenbosch_death_links.csv
Every candidate link between a baseline man and death evidence, with the
decision taken after reading the source. One row per man and document (or
widow entry); one death can have several rows.
- `baseline_hhobs`, `head`, `baseline_wife` — the man, as in the 1712 roll
- `channel` — `probate` (his own estate), `probate_incidental` (his death
  mentioned in someone else's estate papers) or `widow` (a later widow entry)
- `source_file`, `div_id`, `date_value` — the document reference. For widow
  links `source_file` and `div_id` are blank and `date_value` is the roll year
- `widow_hhobs` — the widow's row in the 1713 or 1714 roll
- `decision` — `accept`, `ambiguous` (kept apart from confirmed deaths) or
  `reject`
- `evidence` — a short note of what the source says; not a transcription
- `event_interval` — `after1712_by1714` for accepted links; otherwise the
  reason the link does not date a new death (`prebaseline_widow`,
  `not_head_death`, `wrong_person` or `timing_uncertain`)

## document_screening.csv
Every MOOC8 document dated 1713–14 (90 rows) and its screening disposition.
- `source_file`, `div_id`, `date_value`
- `head` — the opening words of the document heading (date and principal name)
- `screened` — TRUE when read
- `screen` — the disposition

## stellenbosch_document_screening.csv
Every Stellenbosch compilation schedule dated 1713–14 (41 rows).
- `record_id` — compilation record identifier (`STBVR_...`; not a MOOC8 id)
- `year`, `subject` (the estate subject as indexed), `screened`
- `accepted_hhobs`, `rejected_hhobs` — baseline men accepted or rejected
- `disposition` — the reading of the schedule
- `start_line_id`, `end_line_id` — line range in the compilation

## widow_screening.csv
Every widow entry in the 1713 and 1714 rolls (33 rows).
- `year`, `hhobs`, `names_women`, `widow_of` (the husband's name as given)
- `screened`, `screen` — the disposition, citing the baseline `hhobs`
  accepted, rejected or left ambiguous

## additional_widow_candidates.csv
The two mutually exclusive candidate husbands of the surname-only 1714 widow
"wed Roemond" (used in `identity_scenarios.csv`).
- `widow_hhobs`, `year`, `baseline_hhobs`, `candidate`, `decision`, `evidence`

## continuation_panel.csv
The record-continuation diagnostic at entry level (Online Appendix D): one row
per baseline entry, window and specification (9,708 rows). Entries are named
source rows, not reviewed individuals; repeated names are kept. **A flag is
not a death.** No death record and no genealogical field enters the panel.
- `specification` — `primary`, `three_year_baseline`, `long_search`,
  `three_year_baseline_long_search` (see `continuation_windows.csv`)
- `window` — 1709, 1710, 1713, 1718 or 1721
- `baseline_year` — roll year of the baseline entry
- `hhobs`, `name` — the baseline entry
- `cluster_key` — normalised full-name key; the standard errors cluster on it
  across windows
- `adult_slaves`; `group` — `low` (none) or `owner` (at least one)
- `flag_guarded_names`, `flag_permissive_fallback`, `flag_unique_target` —
  1 if no later name met the rule's continuation criterion (paired outcomes
  for the same entry; nested: permissive ≤ guarded ≤ unique)
- `name_candidate_keys`, `name_candidate_rows` — distinct name keys and rows
  meeting the guarded-name rule
- `fallback_candidate_keys`, `fallback_candidate_rows` — the same for the
  permissive first-name fallback
- `name_candidate_examples`, `fallback_candidate_examples` — up to five
  candidate name keys (semicolon-separated)

## docs/generated_numbers/
The numbers and table rows typeset in the paper (LaTeX `\newcommand` macros
and table rows), exactly as the analysis wrote them. `04_microdata_checks.R`
compares its results with these files.

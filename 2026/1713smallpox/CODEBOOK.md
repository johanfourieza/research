# Codebook — 2026/1713smallpox

All data are **aggregate** (counts, rates, and per-entry categorical labels); no file
contains personal names. Districts are `Cape` (Cape District) and `StelDrak`
(Stellenbosch–Drakenstein); `pooled` is the two combined. A machine-readable version of
this dictionary is in `docs/variable_definitions.csv`.

## slave_crosssection_by_district_year.csv
Opgaaf tax-census counts by district and year (settlers and slaves, by age/sex).
- `district`, `year`
- `n_households` — enumerated household rows
- `slave_men`, `slave_women`, `slave_boys`, `slave_girls`, `slave_total` — enslaved counts
- `knechts` — contracted servants
- `settler_total`, `settler_men`, `settler_women`, `settler_sons`, `settler_daught` — settler counts

## raw_stocks_by_year.csv
Colony-wide enslaved and settler stocks by year (from the linked opgaaf panel).
- `year`, `n_household_rows`
- `slave_men`, `slave_women`, `slave_children`, `slaves_total`
- `settler_men`, `settler_women`, `settler_children`, `settlers_total`

## balanced_enslaved_change.csv
Change in enslaved stock over the epidemic window on a balanced household panel.
- `to_year`, `n_hh_in_both` (households present in both years)
- `slaves_total_prev/now`, `g_slaves_total` (growth %); same for `slave_men` and `slave_women`

## epidemic_window_summary.csv
- `metric`, `value` — baseline vs 1712→1713 growth summary statistics

## age_structure_change.csv
1712→1714 change by group, district, and age class (1713 excluded as disrupted).
- `group` (settler / slave), `district`
- `adults_1712`, `adults_1714`, `adults_pct` (% change)
- `children_1712`, `children_1714`, `children_pct` (% change)
- `age_differential_pp` — children % minus adults % (percentage points)

## age_structure_indexed.csv
Indexed series for Figure 3.
- `group`, `district`, `age` (adults / children), `year`
- `index_1712_100` — level indexed to 1712 = 100

## dagregister_year_counts.csv
Daily-journal entry counts per year by theme (keyword pass).
- `year`, `total` (entries that year)
- `disease`, `smallpox`, `khoe_disease`, `slave_disease`, `medicine`, `batavia_medicine`
- `disease_rate_per1000`

## primary_labels_1700_1720.csv
Machine (cheap-classifier) labels for every journal entry, 1700–1720. **No entry text.**
- `id` — journal entry identifier; `date`, `year`
- `has_text` — 1 if the entry has body text
- `V1` — concerns human sickness or death (0/1)
- `V3` — names smallpox (0/1)
- `V4` — records treatment / import of medicine for the epidemic (0/1)
- `who` — group the sickness/death entry concerns (e.g. settlers, cslave, slave,
  khoesan, soldiers_sailors, general); see `data/journal_labelling_codebook.md`

## gold_labels.csv
Hand-coded "gold" labels for the 393-entry random validation sample. **No entry text.**
- `id` — journal entry identifier (joins to `primary_labels_1700_1720.csv`)
- `gV1`, `gV3`, `gV4` — human labels for V1, V3, V4 (0/1)
- `gWho` — human `who` label

## primary_year_rates.csv
- `year`, `n`, `V1_disease`, `V3_smallpox`, `V4_medicine` (counts), `V1_rate`, `V3_rate`

## debiased_estimates.csv
Validation-debiased estimates (Ludwig–Mullainathan–Rambachan) per concept.
- `concept`, `plugin` (cheap mean), `debiased`, `ci_lo`, `ci_hi` (95% bootstrap)
- `valid_only` (gold mean on validation)
- `tp`, `fp`, `fn`, `tn`, `precision`, `recall` — cheap-classifier error vs gold

## mortality_comparison.csv
Cape 1713 mortality beside comparable epidemics (Table 1 / Figure 4).
- `population`, `pop_mortality_pct`, `pm_lo`, `pm_hi` (population-mortality range)
- `cfr_lo`, `cfr_hi` (implied case-fatality range), `source`, `group` (cape / comparison)

## probate_by_year.csv
- `yr`, `n_inv` — number of Orphan Chamber probate inventories drawn up that year

## journal_1700_1720/ (dagregister corpus)
One markdown file per year, 1700–1720, plus `index.md`. Each file holds the daily
journal entries for that year under `## YYYY-MM-DD` day headings (Dutch text). This is
the source corpus the labels above are drawn from; see the README *Sources* note.

## journal_labelling_codebook.md
The frozen coding scheme (concepts V1–V4 and the `who` categories, with rules and
estimation targets) used for both the machine and gold labels.

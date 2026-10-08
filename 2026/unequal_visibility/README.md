# 2026/unequal_visibility

Replication data and code for:

> Fourie, J. (2026). *The Unequal Visibility of Epidemic Death: Smallpox at the
> Cape, 1713.* Working paper, Department of Economics, Stellenbosch University.

- Working paper: [johanfourie.com/files/wp/JF_TheUnequalVisibility_v1.pdf](https://johanfourie.com/files/wp/JF_TheUnequalVisibility_v1.pdf) (copy in [`paper/`](paper/))
- Online appendix: [johanfourie.com/files/wp/JF_TheUnequalVisibility_sup_v1.pdf](https://johanfourie.com/files/wp/JF_TheUnequalVisibility_sup_v1.pdf) (copy in [`paper/`](paper/))

This package holds **what is needed to reproduce every table, figure and
number in the paper**: the aggregate files behind each table and figure, and
the **person-level microdata** underneath them, namely the 1712 cohort of 348
named men, the reviewed decision registers that link them to later death
evidence, and the record-continuation panel. Everyone named in these files
lived in the early eighteenth century. Two kinds of material are left out:
fields and identifiers from the South African Families genealogy, and full
source transcriptions; see *Excluded*.

## The question

Around the Cape Colony's 1713 smallpox epidemic, deaths are documented for
22.2 per cent of men with five or more adult slaves recorded, against 6.6 per
cent of men with none. Does that gap reflect mortality, recording, or both? The
property and family relationships that could help a household survive an
epidemic also create the records through which a death becomes visible: an
estate for the Orphan Chamber to administer, a widow for the next tax roll to
enumerate.

## What the paper finds

1. **A reviewed death register.** Linking 348 men named in the 1712
   Stellenbosch–Drakenstein tax roll to the Orphan Chamber's estate documents
   (MOOC8), to the district's estate schedules in the Stellenbosch compilation
   and to later widow entries confirms 33 all-cause deaths through
   1714. Every link was read in its source: living declarants are not counted
   as dead, and deaths named inside other people's estate papers are recovered.
2. **Unequal visibility.** Both death channels order the groups the same way,
   and so does presence in reconstructed genealogies, which is not a death
   outcome. Restricting to men with a wife named narrows the gap from 6.6 vs
   22.2 per cent to 14.9 vs 22.0 per cent, which links the gap to family
   circumstances.
3. **A benchmark, not an estimate.** Recorded deaths are mortality times the
   probability that a death is recovered. Equal mortality would require
   higher-resource deaths to be about 3.4 times as likely to be recovered. The
   sources do not estimate that recovery difference. Assuming only that
   higher-resource deaths were at least as recoverable leaves a mortality ratio
   between 0.296 and 4.50, which admits either ranking.

## Contents

```
load_data.R                          walk-through of the central argument (base R only)
data/microdata/
  cohort_1712.csv                    the 348 men of the 1712 roll: provenance, names, assets, death records
  baseline_rows.csv                  decision register: all 354 male-name rows of the 1712 roll
  death_links.csv                    decision register: links to MOOC8 documents and widow entries
  stellenbosch_death_links.csv       decision register: links to Stellenbosch compilation schedules
  document_screening.csv             screening of the 90 MOOC8 documents dated 1713-14
  stellenbosch_document_screening.csv  screening of the 41 Stellenbosch schedules dated 1713-14
  widow_screening.csv                screening of the 33 widow entries in the 1713 and 1714 rolls
  additional_widow_candidates.csv    the two candidate husbands of one surname-only widow
  continuation_panel.csv             record-continuation panel: one row per entry, window and specification
data/
  probate_documents_by_year.csv      dated MOOC8 estate documents per year (Figure 1a)
  widow_entries_by_year.csv          widow entries in the district rolls (Figure 1b)
  cohort_by_resource_group.csv       the 1712 cohort by adult slaves recorded (Table 1)
  death_records_by_group.csv         confirmed records by group, all men and wife named (Table 2)
  resource_index_death_records.csv   alternative resource index (Online Appendix C)
  source_rule_death_records.csv      alternative documentary rules (Table S1)
  identity_scenarios.csv             unresolved widow assignments (Table S2)
  denominator_sensitivity.csv        unresolved repeated-name pairs
  continuation_cells.csv             record-continuation flags by window and group (Table S4)
  continuation_estimates.csv         common-weight contrasts and clustered SEs (Tables S3, S5)
  continuation_windows.csv           baseline and search rolls for each window
  asset_column_coverage_1712.csv     support for the zero-coding rule for blank asset cells
scripts/
  00_setup.R                         packages, paths, Wilson interval, LEAP figure style
  01_tables.R                        every table and main-text number, checked against the paper
  02_figures.R                       Figures 1, 2, S1 and S2
  03_continuation.R                  Online Appendix D, including the point estimates from the cells
  04_microdata_checks.R              rebuilds the results from data/microdata/ and checks them cell by cell
  run_all.R                          runs 01-04
  source_pipeline/                   the scripts that built data/ from the restricted sources
docs/
  variable_definitions.csv           machine-readable data dictionary
  generated_numbers/                 numbers and table rows typeset in the paper, as the analysis wrote them
paper/                               the working paper and its online appendix
output/                              tables and figures land here when the scripts run
CODEBOOK.md, LICENSE
```

See `CODEBOOK.md` and `docs/variable_definitions.csv` for every column.

## Reproduce

Requirements: **R 4.1 or later** with `readr`, `dplyr`, `tidyr`, `ggplot2` and
`scales`. From the package root:

```
Rscript load_data.R          # the argument in five steps, base R only
Rscript scripts/run_all.R    # every table and figure; a few seconds
```

`01_tables.R` and `03_continuation.R` compare each recomputed number with the
value printed in the paper and stop on any mismatch. `04_microdata_checks.R`
then starts again from the person-level files. It rebuilds the death flags from
the accepted links, Tables 1 and 2, the wife-named comparison, the resource
index, the source rules, the identity scenarios, the repeated-name
denominators, the continuation cells and the clustered standard errors, and
asserts that every cell equals the aggregate files and the numbers in
`docs/generated_numbers/`. Key numbers that should
appear: 33 confirmed deaths among 348 men; combined record rates of 6.6, 11.7
and 22.2 per cent; a low-to-high ratio of 0.296 (95 per cent interval
0.14–0.61); an equal-mortality threshold of 3.375; the range 0.296–4.50 under
`C_L <= C_H`; 54 MOOC8 documents dated 1713, 3.7 times the neighbouring mean.

The clustered standard errors in Online Appendix D pair rules within each entry
and cluster on full-name keys across windows. `03_continuation.R` reads them
from `continuation_estimates.csv`; `04_microdata_checks.R` recomputes them from
`continuation_panel.csv`.

Two things cannot be reproduced from this package, and the scripts say so
where they arise:

- **The genealogical-presence column of Table 2.** It counts existing links to
  South African Families, whose identifiers are not released.
  `04_microdata_checks.R` reads that column from `death_records_by_group.csv`.
- **The reading of the sources.** Each register decision is a judgment made by
  reading the MOOC8 document, Stellenbosch schedule or roll entry it cites. The
  registers record the decision, the source reference and a short note of the
  evidence, not the source text, so checking a decision means going back to the
  source. Likewise, the continuation flags were computed from the full linked
  tax-roll panel for 1705-1725, which is not redistributed;
  `continuation_panel.csv` holds the flags and candidate counts it produced.

**Evidence dates are not death dates.** `earliest_evidence_date` in
`cohort_1712.csv` is the date of the earliest accepted document or roll entry.
An estate document can follow a death by weeks or months, and a widow entry
shows only that the death fell before that year's roll.

## Sources (raw data not redistributed here)

The aggregates are derived from these individual-level sources, which contain
personal names and are **not** included in this release:

- **Tax censuses (*opgaafrolle*)**: the Stellenbosch–Drakenstein returns
  (transcription of the Hague and Cape archives, June 2022 version; 1712 roll
  at NL-HaNA, VOC 1.04.02, inv. 4068, pp. 220–243), part of the Cape of Good
  Hope Panel (Fourie & Green, 2018, *The History of the Family* 23(3),
  493–502; Fourie et al., 2024, *South African Historical Journal* 76(4),
  420–446).
- **Estate documents**, in two source frames:
  - the MOOC8 series of the Cape Orphan Chamber, Western Cape Archives and
    Records Service, transcribed by the TEPC project. Documents are identified
    by XML file, `div_id` (for example `MOOC8/3.56`) and date;
  - the Stellenbosch district estate schedules in the five-volume compilation
    (*Vol I-V*). Schedules carry the compilation's own identifiers
    (`STBVR_...`, with line references `STBV_..._P..._L...`), which are **not**
    MOOC8 identifiers. A schedule can describe the same estate as a MOOC8
    document; a man counts once whichever frame confirms his death.
- **Genealogy**: South African Families (SAF), used only for a 0/1 presence
  indicator and to distinguish namesakes. SAF is a redistribution-restricted
  third-party dataset; none of its fields or identifiers is released.
- **Daily register (*dagregister*)**: the VOC Cape daily register, in the
  Tracing History Trust transcription, for the dating of the outbreak.

The scripts in `scripts/source_pipeline/` document how the aggregates and the
microdata were built from these sources; they need the restricted files to run.

## Excluded

Two kinds of material are deliberately omitted:

- **South African Families fields and identifiers.** No SAF column (presence
  flag, individual or couple identifier, linkage score) is released. Where a
  register note cited an SAF lineage code, the code is replaced by
  `[genealogical identifier removed]`; this affects one note in
  `death_links.csv`.
- **Full source transcriptions.** The registers cite each MOOC8 document and
  Stellenbosch schedule by identifier and summarise the evidence in a short
  note; they do not reproduce the transcribed text. The complete tax-roll
  transcription is not copied either: the package holds the 1712 rows and the
  columns the analysis uses, and the derived flags of the continuation panel.

Before release, every text field was scanned for SAF lineage codes, for every
identifier in the genealogy, and for runs of more than 40 words shared with the
source transcriptions. The companion package
[`2026/1713smallpox`](../1713smallpox/) follows the same minimal-release
practice.

## Licence

Creative Commons Attribution 4.0 International (CC BY 4.0); see `LICENSE`. The
licence covers the derived data, the decision registers and the code, not the
underlying archival records or their transcriptions.

## Contact

Johan Fourie — johanf@sun.ac.za — https://www.johanfourie.com
ORCID: https://orcid.org/0000-0002-7341-017X

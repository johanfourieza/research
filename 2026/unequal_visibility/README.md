# 2026/unequal_visibility

Replication data and code for:

> Fourie, J. (2026). *The Unequal Visibility of Epidemic Death: Smallpox at the
> Cape, 1713.* Working paper, Department of Economics, Stellenbosch University.

- Working paper: [johanfourie.com/files/wp/JF_TheUnequalVisibility_v1.pdf](https://johanfourie.com/files/wp/JF_TheUnequalVisibility_v1.pdf) (copy in [`paper/`](paper/))
- Online appendix: [johanfourie.com/files/wp/JF_TheUnequalVisibility_sup_v1.pdf](https://johanfourie.com/files/wp/JF_TheUnequalVisibility_sup_v1.pdf) (copy in [`paper/`](paper/))

This package holds **only what is needed to reproduce every table, figure and
number in the paper**, at an **aggregate level with no personal data**. The
individual-level sources, which carry names, are not redistributed; see
*Sources*.

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
   (MOOC8) and to later widow entries confirms 33 all-cause deaths through
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
  run_all.R                          runs 01-03
  source_pipeline/                   the scripts that built data/ from the restricted sources
docs/
  variable_definitions.csv           machine-readable data dictionary
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
value printed in the paper and stop on any mismatch. Key numbers that should
appear: 33 confirmed deaths among 348 men; combined record rates of 6.6, 11.7
and 22.2 per cent; a low-to-high ratio of 0.296 (95 per cent interval
0.14–0.61); an equal-mortality threshold of 3.375; the range 0.296–4.50 under
`C_L <= C_H`; 54 MOOC8 documents dated 1713, 3.7 times the neighbouring mean.

Two quantities cannot be rebuilt from aggregates, and the package says so
where they appear. The clustered standard errors in Online Appendix D pair
rules within each entry and cluster on full-name keys across windows, which
needs the individual-level panel; `03_continuation.R` reproduces every point
estimate from the cell counts and reads the standard errors from
`continuation_estimates.csv`. The death-link decisions themselves are judgments
made by reading each source document; the aggregates record their outcome.

## Sources (raw data not redistributed here)

The aggregates are derived from these individual-level sources, which contain
personal names and are **not** included in this release:

- **Tax censuses (*opgaafrolle*)**: the Stellenbosch–Drakenstein returns
  (transcription of the Hague and Cape archives, June 2022 version; 1712 roll
  at NL-HaNA, VOC 1.04.02, inv. 4068, pp. 220–243), part of the Cape of Good
  Hope Panel (Fourie & Green, 2018, *The History of the Family* 23(3),
  493–502; Fourie et al., 2024, *South African Historical Journal* 76(4),
  420–446).
- **Estate documents**: the MOOC8 series of the Cape Orphan Chamber, Western
  Cape Archives and Records Service, transcribed by the TEPC project.
- **Genealogy**: South African Families (SAF), used only for a 0/1 presence
  indicator and to distinguish namesakes. SAF is a redistribution-restricted
  third-party dataset.
- **Daily register (*dagregister*)**: the VOC Cape daily register, in the
  Tracing History Trust transcription, for the dating of the outbreak.

The scripts in `scripts/source_pipeline/` document how the aggregates were
built from these sources; they need the restricted files to run.

## Excluded (privacy and minimality)

Deliberately omitted: the individual-level opgaaf rows (names), the MOOC8
transcriptions (names of the deceased and heirs), the reviewed decision
registers (which name each man, his wife and the linked documents), the linked
genealogy panel, and the individual-level continuation panel. All named
individuals lived in the early eighteenth century; the omission follows the
minimal-release practice of the companion package
[`2026/1713smallpox`](../1713smallpox/). Researchers with access to the sources
can contact the author about the decision registers.

## Licence

Creative Commons Attribution 4.0 International (CC BY 4.0); see `LICENSE`. The
licence covers the derived aggregates and code, not the underlying archival
records.

## Contact

Johan Fourie — johanf@sun.ac.za — https://www.johanfourie.com
ORCID: https://orcid.org/0000-0002-7341-017X

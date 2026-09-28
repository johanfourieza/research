# Source pipeline (reference only)

These are the scripts that built the files in `data/` from the individual-level
sources. They are included for transparency: they show every rule and check
that stands between the archival records and the released aggregates. They
cannot run from this package, because the sources carry names and are not
redistributed (see the main `README.md`). File paths refer to the author's
working layout.

| Order | Script | What it does |
|---|---|---|
| 0 | `00_setup.R` | Packages, paths, the resource index and name-matching helpers. |
| 1 | `01_probate_documents.R` | Parses the MOOC8 XML volumes and counts dated estate documents. |
| 2 | `02_check_sources.R` | Reconciles every retained 1712 row to the original workbook and checks asset-column coverage. |
| 3 | `03_event_series.R` | Builds the two unlinked series in Figure 1. |
| 4 | `04_static_analysis.R` | Applies the reviewed decisions; builds the cohort, Tables 1–2, the recording model and the sensitivity checks. |
| 5 | `05_identity_scenarios.R` | The unresolved widow assignments in Table S2. |
| 6 | `06_continuation_panel.R` | The record-continuation diagnostic in Online Appendix D, with clustered standard errors. |

`helpers/` holds the shared functions: reporting and the Wilson interval
(`current_reporting.R`), the name parser that keeps full given names and
qualifiers (`current_names.R`, `name_standardize.R`), source and identity
exclusions (`death_record_checks.R`) and the MOOC8 XML parser (`xml_helpers.R`).

## How the death register was built

The central inputs are not in these scripts but in four **decision registers**
that record a reviewed judgment for every source-person link: the retained
baseline rows, the accepted, rejected and unresolved death links, and the
screening disposition of all 90 MOOC8 documents dated 1713–14 and all 33 widow
entries. `04_static_analysis.R` reads those registers as inputs and never
infers a death from a fuzzy name match or from the order of names in a
document heading. The registers name each man, his wife and the linked
documents, so they are withheld with the other individual-level material.

## Checks built into the pipeline

- Every retained row reconciles to the original workbook row number and names.
- Every accepted link references a unique document (file, identifier and date,
  because identifiers are not always unique within a volume) or an original
  roll row.
- Each widow entry is assigned to at most one man; each man counts once.
- Results are invariant to the order of the input rows.
- The continuation contrasts are checked against a separately constructed
  saturated Poisson model and its clustered covariance matrix.

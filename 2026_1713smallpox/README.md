# 2026_1713smallpox

Replication data and code for:

> Fourie, J. (2026). *A Disease Never Seen Here: Measuring the Severity of the
> 1713 Smallpox Epidemic at the Cape.* (Submitted to *The History of the Family*.)

The paper measures the severity of the 1713 Cape smallpox epidemic for the groups
the colony actually counted — settlers and the Dutch East India Company's slaves,
by age — using the annual tax censuses (*opgaafrolle*), the probate inventories of
the Orphan Chamber, and the daily journal (*dagregister*), and places the Cape in
the comparative demography of smallpox.

This package holds **only what is needed to reproduce the figures, tables, and
headline numbers**, at an **aggregate level with no personal data**. The raw,
individual-level sources (which carry names) are not redistributed; see *Sources*.

## Contents

```
data/
  slave_crosssection_by_district_year.csv   settler & slave counts by district x year x age/sex (opgaaf)
  raw_stocks_by_year.csv                     colony-wide enslaved/settler stocks by year
  balanced_enslaved_change.csv               balanced-panel enslaved change, epidemic window
  epidemic_window_summary.csv                baseline vs 1712->1713 growth summary
  age_structure_change.csv                   1712->1714 change by group x age x district
  age_structure_indexed.csv                  indexed series (1712=100), 1708-1718
  dagregister_year_counts.csv                journal entries per year by theme (disease, smallpox, ...)
  primary_labels_1700_1720.csv               machine labels for all 7,670 journal entries (V1,V3,V4,who)
  gold_labels.csv                            393 hand-coded gold labels (validation sample)
  primary_year_rates.csv                     yearly disease/smallpox rates (cheap classifier)
  debiased_estimates.csv                     validation-debiased estimates + precision/recall
  mortality_comparison.csv                   Cape mortality beside comparable epidemics (Table 1 / Fig 4)
  probate_by_year.csv                        Orphan Chamber probate inventories per year (Fig 1)
  journal_labelling_codebook.md              frozen codebook for the journal labels (V1-V4, who)
  journal_1700_1720/                         the daily journal (dagregister) entries, 1700-1720 (one .md per year)
scripts/
  06_debias.py, 07_figures.py, 08_mortality_comparison.py, 11_age_structure.py
                                             reproduce the debiased estimates and Figures 1-4 from data/
  source_pipeline/                           the upstream builders (need the archival sources; see below)
docs/
  variable_definitions.csv                   machine-readable data dictionary
output/                                       figures/tables land here when the scripts run
CODEBOOK.md, LICENSE
```

See `CODEBOOK.md` and `docs/variable_definitions.csv` for every column.

## Reproduce

Requirements: **Python 3.9+** and **matplotlib** (the scripts use only the standard
library plus matplotlib — no pandas/numpy needed). From the `scripts/` folder:

```
python 07_figures.py               # Figure 1 (probate) and Figure 2 (dagregister)
python 11_age_structure.py         # Figure 3 (age structure) + age_structure_*.csv
python 08_mortality_comparison.py  # Figure 4 + mortality_comparison.csv (Table 1)
python 06_debias.py                # validation-debiased estimates + precision/recall
```

Outputs (PNG + PDF figures, and regenerated CSVs) are written to `output/`. Key
numbers that should appear: 1713 probate = 54; Company slaves ≈ 35% population
mortality (implied CFR 37–59%); enslaved children −24.5% (Cape) / −23.6%
(Stellenbosch–Drakenstein); naive disease attention 45.9% corrected to 12.6%;
journal disease entries 71% ship crews, 19% Company slaves, 4% settlers, 1.9% Khoesan.

The method follows Ludwig, Mullainathan, and Rambachan (2026, *Annual Review of
Economics*): a cheap classifier over the whole journal corrected against the
hand-coded gold sample (`06_debias.py`).

## Sources (raw data not redistributed here)

The aggregate tables above are derived from these individual-level sources, which
contain personal names and are **not** included in this release:

- **Tax censuses (*opgaafrolle*)** — the Cape and Stellenbosch–Drakenstein
  annual returns, from the Cape of Good Hope Panel (Fourie & Green, 2018,
  *The History of the Family* 23(3), 493–502) and the Hague & Cape archives.
- **Probate inventories** — the MOOC8 series of the Cape Orphan Chamber, Western
  Cape Archives and Records Service, transcribed via the TANAP project.
- **Daily journal (*dagregister*)** — the Council of Policy journal, Cape of Good
  Hope; Western Cape Archives and Records Service / Nationaal Archief, The Hague;
  transcriptions via TANAP (www.tanap.net). The `data/journal_1700_1720/` corpus
  is the author's cleaned compilation of the 1700–1720 entries of this public
  archival record; the CC BY licence covers this compilation, not the underlying
  archival text.

The scripts in `scripts/source_pipeline/` document how the aggregates were built
from these sources; they require the archival files (not shipped) to run.

## Excluded (privacy / minimality)

Deliberately omitted: the individual-level opgaaf workbooks (settler names), the
MOOC8 probate transcriptions (names of the deceased and heirs), the linked
longitudinal genealogy panel (South African Families), and the free-text `evidence`
column of the machine labels. All named individuals are from the early eighteenth
century, but the linked genealogy is a redistribution-restricted third-party dataset
and is excluded on that basis.

## Licence

Creative Commons Attribution 4.0 International (CC BY 4.0); see `LICENSE`.

## Contact

Johan Fourie — johanf@sun.ac.za — https://www.johanfourie.com
ORCID: https://orcid.org/0000-0002-7341-017X

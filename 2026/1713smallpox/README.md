# 2026/1713smallpox (version 2, revised manuscript)

Replication materials for:

> Fourie, J. (2026). *A disease never seen here: Smallpox and family reconstruction at the Cape, 1713–1714.* Revised manuscript, *The History of the Family*.

**This version replaces the July 2026 release** that accompanied the submitted manuscript (*Measuring the Severity of the 1713 Smallpox Epidemic at the Cape*). The submitted version's validated-classifier estimates, corrected journal shares, group shares, and mortality-comparison table are withdrawn. In particular, the submitted manuscript's statement that two human readers agreed with the reference labels on all 79 re-coded entries was incorrect; `data/validation/validation_agreement.csv` reports the actual comparisons.

The package is aggregate-level. Individual-level opgaaf rows and probate registers, which carry names, are not redistributed. The underlying transcriptions remain governed by their custodians (see *Sources*).

## Contents

```
data/census/       recorded tax-unit stocks by district and year (census_annual.csv), two-year changes,
                   adult-proportional child benchmark, missing-cell counts, the enslaved-girls ('Meijsies')
                   mapping ledger and the release reconciliation
data/probate/      Orphan Chamber (MOOC8) documents per heading year, 1695-1720, and baseline ratios
data/journal/      full-corpus lexical counts per year (1700-1720), the 18 smallpox candidates with decisions,
                   the 17-entry chronology, 94 medical candidates, care-context register, image concordance,
                   retrieval protocol (exact expressions) and summary results
data/company/      the Company stock-flow ledger from the 26 August 1713 journal entry
data/validation/   hand-coded sample compared with the reference labels
figures/           Figures 1-3 of the article and Figure A1 of the supplement (PDF and PNG)
scripts/           the full pipeline (run_revision.py calls 01, 02, 04, 05, 10, 11 and 06)
```

## Reproduce

The scripts are the exact pipeline used for the paper. They read the custodial source files (opgaaf workbooks, Households v1 release, MOOC8 XML, journal workbook), which are not included; input paths and SHA-256 hashes are recorded in the scripts' manifests. With those inputs in place, `python -X utf8 scripts/run_revision.py` rebuilds every table and figure. Python 3.13; packages in `scripts/requirements.txt`.

Headline values: 1713 probate documents 54 (1714: 36; 1708–1712 mean 10.8). Recorded settlers, Cape District and Stellenbosch–Drakenstein combined, 1,967 (1712) → 1,488 (1714). Privately enslaved children 143 → 108 (Cape) and 89 → 68 (Stellenbosch–Drakenstein). Journal: 7,670 daily identifiers, 7,666 usable texts; 18 smallpox candidates, 17 retained (16 in 1713, one in February 1714).

## Sources

- **Tax returns (*opgaafrolle*)**: Cape of Good Hope Panel transcriptions (Fourie & Green, 2018, *The History of the Family* 23(3), 493–502; Fourie et al., 2024, *South African Historical Journal* 76(4)); archival originals NA, VOC 4068 and 4073.
- **Estate papers (MOOC8)**: transcribed by the TEPC Transcription Project (2004–2008) at the Western Cape Archives and Records Service; public rendering by GLOBALISE (TANAP Resources).
- **Daily journal (*dagregister*)**: transcription by the Tracing History Trust, shared by Helena Liebenberg; originals NA, VOC 1.04.02, inv. 10730–10733.

## Licence

Code and derived data: CC BY 4.0 (see `LICENSE`). Quoted source passages remain subject to the custodians' terms.

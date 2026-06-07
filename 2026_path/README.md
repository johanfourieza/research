# Path Dependence in Economic History Publications

Replication data and code for Fourie (2026), submitted to *Cliometrica*.

## Overview

This package asks whether economic history's own knowledge production is
path-dependent: do small, partly idiosyncratic differences in a paper's *early*
citations translate into persistent differences in its *long-run* influence?
The core dataset is a hand-coded census of articles in economic history's
leading journals, each with annual Google Scholar citation snapshots, paired
with hand-verified conference programmes.

The headline result is a within-journal-year elasticity of long-run on
age-two citations of about 0.78, which survives when the outcome is restricted
to citations accrued strictly after the early window. The package contains the
curated data and the analysis code needed to reproduce the paper's results.

## Citation

> Fourie, J. (2026). *Path Dependence in Economic History Publications.*
> Submitted to *Cliometrica*.

Please cite the paper if you use these data or code.

## Principal investigator

- **Johan Fourie**, LEAP, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za) — corresponding author

The journal and conference data were hand-coded over more than a decade by
successive cohorts of LEAP research assistants, to whom the dataset is, more
than most, attributed.

## Data

| File | Rows | Description |
|---|---|---|
| [`data/Journals_2026_clean.csv`](data/Journals_2026_clean.csv) | 3,748 | Hand-coded census of articles in six economic history journals (JEH, EHR, EREH, Explorations, Cliometrica, EHDR). One row per article. Columns `Google14`–`Google26` are cumulative Google Scholar citation counts observed in calendar years 2014–2026. The paper analyses the 1,262 articles in the four leading journals (JEH, EHR, EREH, Explorations) published 2012–2021 that have a complete age-two-to-age-five/eight citation trajectory. See [`CODEBOOK.md`](CODEBOOK.md). |
| [`data/Conference_Papers.xlsx`](data/Conference_Papers.xlsx) | 1,006 | Raw, hand-transcribed programmes of the Economic History Association (EHA) annual meetings, 2006–2025, including exact session begin/end times. Transcribed by hand because the published multi-column PDFs are unreliable when parsed automatically. |
| [`data/conference_parsed_data.csv`](data/conference_parsed_data.csv) / [`.rds`](data/conference_parsed_data.rds) | 3,627 | Cleaned, combined conference-programme dataset used by the pipeline: EHA (1,006, from the workbook above) plus EHS — Economic History Society annual meetings, 2,621 entries parsed from the Society's published HTML programmes. One row per presentation. |

All CSVs are UTF-8. The `.rds` is provided for exact, type-stable loading in R;
the `.csv` is the same data for portability.

### What is *not* included

Two derived inputs used by parts of the analysis are **not redistributed**, in
line with their providers' terms — they are reproducible from the public APIs:

- **OpenAlex citation-network data** (citing works, their disciplines, and the
  derived cross-field / cascade / concentration measures used in Section 6).
- **RePEc author-quality data** (seniority and h-index, used in the
  author-quality robustness checks).

The raw EHS programme HTML is likewise not included; the cleaned EHS entries are
already in `conference_parsed_data`.

## How to reproduce

From the `2026_path/` folder, in R:

```r
source("scripts/load_data.R")     # loads `journals` (and `conferences`)
```

- **Core persistence results** (the 0.78 elasticity, the fast-starter and
  unpredictability analyses, within-issue position) reproduce from
  `Journals_2026_clean.csv` alone via `scripts/01_analysis.R` and
  `scripts/02_figures.R`.
- The **conference** analysis additionally uses `conference_parsed_data.rds`
  (read through `scripts/conference_data_helpers.R`).
- The **citation-source / mechanism** sections (Section 6, including
  `scripts/fig_citation_source.R`) and the **author-quality** robustness checks
  additionally require the OpenAlex and RePEc derived data noted above; rerun
  the collection step against those public APIs to regenerate them.

Scripts resolve their own paths and expect to be run from this folder.

## License

CC BY 4.0. See [`LICENSE`](LICENSE). Note that the Google Scholar citation
counts and the excluded OpenAlex/RePEc data carry their providers' terms.

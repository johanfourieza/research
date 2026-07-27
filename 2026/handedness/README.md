# Invisible Handedness: The Myth of Left–Right Batting Partnerships

Replication data and code for Fourie and Siebrits (2026), currently under
review at the *Journal of Sports Economics*.

## Overview

Cricket's conventional wisdom holds that left–right batting partnerships
disrupt bowlers by forcing constant line-and-length adjustments. The paper
tests this claim with ball-by-ball data on all men's international cricket —
96,000 partnerships and 3.4 million deliveries across Tests, ODIs and T20Is —
plus the English County Championship and the Australian Sheffield Shield.
Controlling for batsman quality and match × innings fixed effects, the
mixed-hand premium in partnership runs is a precisely estimated zero in every
format; the raw premium is largely explained by the superior individual
quality of left-handed batsmen.

## ⚠ What this release contains — and what it cannot

**We are not permitted to redistribute the full ball-by-ball data.** This
release therefore contains:

1. **All analysis code** (33 R scripts — the complete pipeline, from raw-data
   download to every table and figure in the paper and online appendix).
2. **The five partnership-level analysis files** (aggregated to one row per
   partnership), which are sufficient to reproduce the paper's main results.

The delivery-level (ball-by-ball) files are **not** included, but they are
fully reconstructible: `scripts/international/01_download_data.R` downloads
the raw data directly from [Cricsheet](https://cricsheet.org/) (Rushe, 2026),
and scripts 02–08 rebuild every dataset used in the paper, including the
ball-level files. `scripts/firstclass/cs_01_download.R` onward does the same
for the County Championship and Sheffield Shield.

### What reproduces from the included files alone

- The partnership-level results: Tables 1–3, 5–10 and Figures 1–6 of the
  main paper, and most online-appendix tables (B1–B4, B6–B14, B16, B18).

### What requires rebuilding the ball-by-ball data first (scripts 01–08)

- The ball-level mechanism results: Table 4 and Online Appendix Tables B5,
  B15, B17, B19 and B20, and the first-class ball-level corroboration.

## Citation

> Fourie, J. and Siebrits, K. (2026). "Invisible Handedness: The Myth of
> Left–Right Batting Partnerships." Stellenbosch University. Under review,
> *Journal of Sports Economics*.

## Principal investigators

- **Johan Fourie**, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za)
- **Krige Siebrits**, Department of Economics, Stellenbosch University
  (krigesiebrits@sun.ac.za)

## Data

All data are plain UTF-8 CSV. Missing values are empty strings.

### `data/analysis/` — partnership-level analysis files

| File | Rows | Description |
|---|---|---|
| `analysis_tests.csv` | 28,989 | One row per Test-match batting partnership, 2001–2025. |
| `analysis_odis.csv` | 39,362 | One row per ODI batting partnership, 2001–2025. |
| `analysis_t20is.csv` | 45,184 | One row per T20I batting partnership, 2005–2025. |
| `analysis_county.csv` | 43,911 | One row per County Championship partnership (Cricsheet coverage). |
| `analysis_sheffield.csv` | 6,875 | One row per Sheffield Shield partnership (Cricsheet coverage). |

All five files share the same 67 columns. The paper's main sample applies the
filter `hand_known == 1 & both_avg_known == 1 & either_debutant == 0`.

### `scripts/international/` — the full pipeline (24 scripts)

`00`–`18` build the data and produce every result in the originally submitted
paper (download, parse, partnership construction, player histories, analysis
data, descriptives, regressions, ball-level data and regressions, IV,
survival, heterogeneity, robustness, figures, tables, extensions, referee
revisions, Table 3 regeneration). `19`–`22` implement the final-revision
corrections and additions (corrected Oster deltas, within-innings permutation
inference, equivalence tests, truncation robustness, Figure 6 regeneration).

### `scripts/firstclass/` — County Championship and Sheffield Shield (9 scripts)

The parallel pipeline for the two domestic first-class competitions used in
the corroboration analysis (Section 6.3 of the paper).

## Data collection

All match data originate from [Cricsheet](https://cricsheet.org/) (Stephen
Rushe), which provides structured ball-by-ball JSON for men's international
cricket from 2001 and for the two domestic competitions; batting-hand
metadata come from the `cricketdata` R package (Hyndman et al., 2025) via
ESPNcricinfo player profiles. Career statistics are computed from all prior
matches within the Cricsheet coverage window (players active before 2001
enter with truncated histories; see Section 3.3 of the paper).

## Universe and sample

The unit of analysis is the batting partnership: an unordered pair of batsmen
at the crease, tracked over every delivery of every innings. The universe is
all men's international cricket (Tests, ODIs, T20Is) from December 2001 to
2025, plus all County Championship and Sheffield Shield matches with
Cricsheet ball-by-ball coverage. Partnership construction rules (wickets,
retirements, innings ends, extras, legal deliveries) are codified in Section
3.2 of the paper and implemented in `03_build_partnerships.R`.

## Loading the data in R

```r
library(readr)
tests     <- read_csv("data/analysis/analysis_tests.csv")
odis      <- read_csv("data/analysis/analysis_odis.csv")
t20is     <- read_csv("data/analysis/analysis_t20is.csv")
county    <- read_csv("data/analysis/analysis_county.csv")
sheffield <- read_csv("data/analysis/analysis_sheffield.csv")
```

A helper is at `scripts/load_data.R`.

## Documentation

Variable definitions are in [`CODEBOOK.md`](CODEBOOK.md) and, in
machine-readable form, in
[`docs/variable_definitions.csv`](docs/variable_definitions.csv).

## License

CC BY 4.0. See [`LICENSE`](LICENSE). The underlying match data are
© Cricsheet; please attribute Cricsheet when reusing them.

## Funding

This research received no dedicated external funding.

## Contact

Johan Fourie, johanf@sun.ac.za

# Defining and Identifying All-Rounders in Cricket

Replication code and aggregate results for Fourie (2026), under review at the
*Journal of Sports Analytics*.

## Overview

Cricket analysts use the term all-rounder in three senses: a player given both
batting and bowling work, a player whose output is balanced between the two,
and a player who bats and bowls as well as the specialists. The paper compares
these definitions using men's scorecards for Tests, ODIs, T20Is, the English
County Championship and the Sheffield Shield through 31 December 2025. The
accepted sample contains 27,952 matches, 613,532 appearances and 17,507
player–competition records.

The definitions select very different players. In Test cricket, 154 players
carry a cumulative dual workload, but at most three exceed the median
specialist in both disciplines with a model-based probability above one half.
The Balassa index of revealed comparative advantage is an exact rescaling of a
player's batting share. Early playing frequency and output per year carry most
of the information for forecasting later output. Domestic dual-role players
reach Test cricket about twice as often as batters, but the data cannot
separate the label from the bowling record that defines it.

## ⚠ What this release contains — and what it cannot

**The scorecard data are not redistributed.** The paper uses the DAFT
first-class and international scorecard database compiled by Ric Finlay and
Jim Palfreyman, which is licensed and may not be shared. This release
therefore contains:

1. **All analysis code** (`revision/scripts/`, 41 R scripts): source audit,
   identity corrections, construction of the analysis sample, and every
   estimate, table and figure.
2. **Aggregate results** (`revision/results/`, 52 CSV files): the counts,
   estimates, intervals, sensitivity grids and forecast metrics behind the
   paper's exhibits. None contains player–match rows.
3. **Documentation and checks**: a field dictionary of the source tables
   (`revision/config/field_dictionary.csv`), the SHA-256 hashes of the 35
   DAFT source files used (`revision/audit/baseline_manifest.csv`), the
   Cricsheet archive manifest used for independent source checks
   (`revision/supplement/`), the 72 final verification checks
   (`revision/audit/execution/final_verification.csv`) and the R session
   information.

Not included: DAFT rows (raw, staged, corrected or analysis-level), and nine
small configuration files that list DAFT player keys (an identity crosswalk
for the Cricsheet pilot, the verified identity rules, cleared identity keys,
the five implementation-pilot samples and the death-date review). Licensed DAFT users can obtain these from
the author.

## Reproducing the results

### Without the licensed data

```r
Rscript revision/supplement/synthetic_checks.R
```

This runs small self-contained examples that need no proprietary inputs. They
show why pure batting passes the old RCA band, why dependence between batting
and bowling output affects threshold classification, and why match completion
controls which features are available to a prospective forecast. The
aggregate files in `revision/results/` can be read directly.

### With the licensed data

1. Place the 35 DAFT CSV files under `data/raw/daft/` in this folder, using the
   paths and hashes listed in `revision/audit/baseline_manifest.csv`.
2. For the independent source checks, set the environment variable
   `ALLROUNDERS_AUDIT_ARCHIVE` to a folder holding the Cricsheet archive at
   `raw/cricsheet/all_json.zip` (see `revision/supplement/CRICSHEET_NOTICE.md`).
3. Add the identity configuration files to `revision/config/`.
4. From this folder, run

```r
Rscript revision/scripts/run_revision.R --full-source-audit --licensed-replication
```

The runner executes every step in order and stops at the first error. Figures
and LaTeX tables are written to `revision/paper/`. The `--licensed-replication`
flag verifies the DAFT source hashes and all substantive checks, but not the
hashes of the author's other working files.

R 4.5.2 was used, with data.table, jsonlite, digest, lme4, survival,
clubSandwich, mvtnorm, ggplot2, pdftools and xml2. Seed: 2412.

## Data sources

- **DAFT** (Data Analysis For cricket Tragics), Ric Finlay and Jim
  Palfreyman. Licensed; not redistributed.
- **Cricsheet** (https://cricsheet.org/), used for independent source checks.
  The included match manifest is supplied under the Open Data Commons
  Attribution License 1.0.

## Citation

> Fourie, J. (2026). "Defining and Identifying All-Rounders in Cricket."
> Stellenbosch University. Under review, *Journal of Sports Analytics*.

## License

Code and aggregate results: Creative Commons Attribution 4.0 (see
`LICENSE.txt`). The licence does not extend to the DAFT database, and the
Cricsheet manifest remains under its own licence.

## Contact

Johan Fourie — johanf@sun.ac.za — https://www.johanfourie.com

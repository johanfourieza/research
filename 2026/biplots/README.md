# Biplots for Historical Household Data: Evidence from Cape Tax Records

Replication code and aggregate results for Fourie, Lubbe, Nienkemper-Swanepoel
and von Fintel (2026), circulated as a LEAP working paper and under review at
*Historical Methods: A Journal of Quantitative and Interdisciplinary History*.

## Overview

Historical tax lists, inventories and censuses record many quantities for each
household, but historians usually summarise them one quantity at a time. The
paper is a practical guide to principal component analysis (PCA) biplots for
such records. It uses 20,913 household-year returns from the Stellenbosch and
Drakenstein tax censuses (*opgaafrollen*), in eligible years within 1804–1825,
as a worked example. Two dimensions retain 70.1% of the standardised,
log-transformed variation. The paper shows how source decisions and
transformations shape the display, how to read calibrated axes, and how to
test whether the picture can be trusted.

## What this release contains, and what it cannot

**We are not permitted to redistribute the underlying transcriptions.** The
raw tax-census files, household links and household-level extracts are held by
their custodians and require permission to access. This release therefore
contains:

1. **All analysis code** (`code/`): the complete pipeline from the raw
   transcriptions to every table, figure and number in the paper.
2. **Aggregate results** (`data/results/`): every summary table the pipeline
   produces, as CSV. Files that contain record references or source-cell text
   are withheld.
3. **Figures** (`docs/figures/`) in PDF and 300 dpi PNG.
4. **Documentation** (`CODEBOOK.md`, `docs/`): variable definitions, the
   SHA-256 hashes of the required input files, and the R package versions used.

The aggregate results contain the numbers behind every table and figure in
the paper. Regenerating them requires authorised access to the source files
listed in `docs/input_manifest.csv`.

## Reproducing the analysis

1. Obtain the source files `stellenbosch_temp.csv` and `1825 series.xlsx` from
   their custodians and place them in `data/raw/`. The SHA-256 hashes in
   `docs/input_manifest.csv` identify the exact files used.
2. Install R and the package versions listed in
   `docs/dependency_manifest.csv`.
3. From this folder, run:

```
Rscript code/00_run.R --verify --analysis-only
```

This runs `01_clean.R`, `02_analysis.R` and `03_figures.R` twice in fresh
processes and checks that the outputs are identical. Random seeds are fixed in
the scripts.

## Citation

> Fourie, Johan, Sugnet Lubbe, Johané Nienkemper-Swanepoel, and Dieter von
> Fintel. 2026. "Biplots for historical household data: Evidence from Cape tax
> records." Working Paper, Department of Economics, Stellenbosch University.

## Principal investigators

- **Johan Fourie**, LEAP, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za)
- **Sugnet Lubbe**, MuViSU, Department of Statistics and Actuarial Science,
  Stellenbosch University; NITheCS
- **Johané Nienkemper-Swanepoel**, MuViSU, Department of Statistics and
  Actuarial Science, Stellenbosch University
- **Dieter von Fintel**, LEAP, Department of Economics, Stellenbosch University

## Funding

Riksbankens Jubileumsfond, Cape of Good Hope Panel grant (M20-0041).

## License

Code and aggregate results are released under CC BY 4.0 (see `LICENSE`). The
licence does not extend to the restricted source transcriptions.

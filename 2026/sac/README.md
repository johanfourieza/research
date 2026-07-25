# Discrimination After Hiring: Within-Firm Sorting, Slow Employer Learning, and the Cost to the Sorted Worker

Replication data and code for Fourie, Inwood and Mariotti (2026), submitted to
*Labour Economics*.

## Overview

This package contains the cleaned analysis dataset and the full analysis code
for "Discrimination After Hiring: Within-Firm Sorting, Slow Employer
Learning, and the Cost to the Sorted Worker." The paper uses personnel
records from the South African Constabulary (SAC), a mounted police force
that employed Afrikaner recruits alongside British, colonial and other
workers in the decade after the Anglo-Boer War.

The institutional setting is a within-rank, within-class fixed pay schedule:
any discriminatory premium has to operate through which job a recruit is
placed in, not through the wage paid once placed. The data jointly observe
initial assignment, supervisor character ratings, and separation outcomes
for the same workers, so the paper can ask whether the within-firm
assignment penalty was later validated by performance and how the
organisation updated its beliefs over the course of a recruit's tenure.

## Citation

> Fourie, J., Inwood, K. and Mariotti, M. (2026). *Discrimination After
> Hiring: Within-Firm Sorting, Slow Employer Learning, and the Cost to
> the Sorted Worker.* Submitted to Labour Economics.

The underlying archival transcriptions of the SAC enlistment registers were
originally compiled for:

> Fourie, J., Grundlingh, A. and Mariotti, M. (2017). "'Poor South Africa!
> Will no nice English people ever come out here?' The South African
> Constabulary of the Anglo-Boer War." *Journal of Imperial and Commonwealth
> History* 45 (4): 580–606.

Please cite both works if you use the data.

## Principal investigators

- **Johan Fourie**, LEAP, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za) — corresponding author
- **Kris Inwood**, Department of Economics, University of Guelph
  (kinwood@uoguelph.ca)
- **Martine Mariotti**, Research School of Economics, Australian National
  University (martine.mariotti@anu.edu.au)

## Data

All data are plain UTF-8 CSV. Missing values are empty strings.

| File | Rows | Description |
|---|---|---|
| [`data/sac_clean.csv`](data/sac_clean.csv) | 10,399 | Cleaned, analysis-ready dataset of SAC recruits enlisted between 1900 and 1908 (with a small tail running into 1909). One row per recruit-spell. 89 variables. This is the file that powers every table and figure in the paper. |
| [`data/afrikaner_manual_review.csv`](data/afrikaner_manual_review.csv) | — | Row-level manual review of the Afrikaner classification on disputed cases. The cleaning script merges this back into the analysis dataset by `row + id` to produce the reviewed-baseline indicator (`afrikaans_reviewed`). |

### What is *not* included

The underlying archival transcriptions of the South African Constabulary
enlistment registers (`SAC final.xlsx`, `BoerSurnames.xlsx`) are held subject
to the National Archives of South Africa's conditions on reuse and are not
redistributed here. They are available from the authors on request. The
cleaning script in `scripts/01_clean_data.R` documents the transformation
from raw to cleaned for transparency, but it requires access to those raw
inputs to run end-to-end.

The full analysis script `scripts/paper2_analysis.R` operates on
`data/sac_clean.csv` and reproduces every table and figure in the paper
without needing the raw inputs.

## Universe and sample

Unit of analysis: one row per recruit-spell. 8,831 unique recruit ids; some
recruits enlisted more than once and contribute multiple spells.

The cleaned sample contains 10,399 records. Under the reviewed-baseline
Afrikaner classification (`afrikaans_reviewed`), 354 recruits are classified
as Afrikaner. Of these, 300 enlisted after the Treaty of Vereeniging
(31 May 1902); the post-treaty subsample is the basis for the cost
calculation in the paper.

## Loading the data in R

```r
library(readr)
sac <- read_csv("data/sac_clean.csv")
```

A helper script that handles type coercion (numeric and date columns) and
sets the reviewed Afrikaner indicator as the working `afrikaans` column,
matching the conventions used inside `paper2_analysis.R`, is provided at
[`scripts/load_data.R`](scripts/load_data.R).

## Reproducing the paper

The full analysis pipeline is:

```r
# From the package root, with sac_clean.csv already in data/:
source("scripts/load_data.R")        # loads sac as a tibble
source("scripts/paper2_analysis.R")  # produces every table and figure
```

`paper2_analysis.R` writes its outputs into sibling `tables/`, `figures/`
and `models/` folders that the script creates if they do not already exist.

To reproduce the cleaning step from raw, you also need the raw inputs and
must run:

```r
source("scripts/01_clean_data.R")    # raw .xlsx -> sac_clean.rds
```

## Documentation

Variable definitions are in [`CODEBOOK.md`](CODEBOOK.md) and, in
machine-readable form, in
[`docs/variable_definitions.csv`](docs/variable_definitions.csv).

## License

The data and code in this subfolder are released under the Creative Commons
Attribution 4.0 International License (CC BY 4.0). See [`LICENSE`](LICENSE).

## Funding

This research did not receive any specific grant from funding agencies in
the public, commercial, or not-for-profit sectors.

## Contact

Johan Fourie, Stellenbosch University: johanf@sun.ac.za

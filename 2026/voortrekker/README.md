# Demographic Pressure, Emancipation and Selection into the Great Trek

Replication data for Fourie and Links (2026), *European Review of Economic History*.

## Overview

This repository contains the data underlying the paper "Demographic Pressure,
Emancipation and Selection into the Great Trek." The paper links Voortrekker
genealogical records to the 1825 Cape Colony census and to the British slave
compensation records of 1833–34 in order to study selection into the Great
Trek (1835–1840).

The central empirical question is who left, and whether those households
looked different from those who stayed. The main finding is that selection
operated on household composition rather than on wealth: Voortrekker
households were larger, more fertile and more densely composed of working-age
men, but held no more wealth and fewer slaves than households that stayed.

## Citation

> Fourie, J. and Links, C. (2026). *Demographic Pressure, Emancipation and
> Selection into the Great Trek.* European Review of Economic History
> (forthcoming).

## Principal investigators

- **Johan Fourie**, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za)
- **Calumet Links**, Department of Economics, Stellenbosch University

## Data

All data are plain UTF-8 CSV. Missing values are empty strings.

### `data/raw/` — source datasets

| File | Rows | Description |
|---|---|---|
| `cape_census_1825.csv` | 10,420 | Household-level 1825 Cape Colony census (*opgaafrolle*). One row per household, compiled from the eleven district returns. |
| `voortrekkers.csv` | 2,702 | Voortrekker genealogical records. Individual-level entries compiled from published genealogies. The matching sample is restricted to 917 adult males born before 1810 with valid names. |
| `slave_compensation.csv` | 36,419 | Slave compensation records at the individual-slave level, from [Ekama (2021)](https://datafirst.uct.ac.za/dataportal/index.php/catalog/848). Includes valuation, compensation and owner details. |

### `data/linked/` — record-linkage results

| File | Rows | Description |
|---|---|---|
| `voortrekker_census_matches.csv` | 558 | Accepted Voortrekker-to-census links from the Random Forest + expert-review pipeline. 536 unique matched census households. |
| `voortrekker_emancipation_matches.csv` | 577 | Voortrekker-to-slave-compensation links on owner name and district. |

### `data/analysis/` — analysis-ready file

| File | Rows | Description |
|---|---|---|
| `analysis_dataset.csv` | 10,420 | Household-level analysis file. The 1825 census enriched with a Voortrekker-match flag (`is_voortrekker`), a composite `wealth_index`, derived household-composition variables, and Voortrekker genealogical fields for matched households. This is the file used to produce the regression tables in the paper. |

## Data collection

- **1825 census (*opgaafrolle*)**: colonial tax returns transcribed from the
  Western Cape Archives and Records Service. Coverage: Cape, Stellenbosch,
  Graaff-Reinet, Swellendam, Albany, Beaufort, Clanwilliam, Cradock, George,
  Worcester and Uitenhage districts. Somerset data come from the Cradock 1823
  returns; Colesberg is included in Graaff-Reinet; Clanwilliam is included in
  Worcester.
- **Voortrekker genealogies**: compiled from published genealogical sources
  on Voortrekker families.
- **Slave compensation**: from [Ekama (2021)](https://datafirst.uct.ac.za/dataportal/index.php/catalog/848),
  who digitised the 1833–34 Cape compensation claims held at the UK National
  Archives.

## Universe and sample

Unit of analysis: household, with the household head identified by name and
district in the 1825 census.

Matching sample: 917 Voortrekker men (adult, born before 1810, with valid
names) matched against 10,420 census households across the ten main districts
that supplied Voortrekkers. Final linked sample: 536 unique matched
Voortrekker census households versus 9,884 non-Voortrekker households.

## Loading the data in R

```r
library(readr)

census   <- read_csv("data/raw/cape_census_1825.csv")
vt       <- read_csv("data/raw/voortrekkers.csv")
slaves   <- read_csv("data/raw/slave_compensation.csv")

links_c  <- read_csv("data/linked/voortrekker_census_matches.csv")
links_e  <- read_csv("data/linked/voortrekker_emancipation_matches.csv")

analysis <- read_csv("data/analysis/analysis_dataset.csv")
```

A helper script is provided at `scripts/load_data.R`.

## Documentation

Variable definitions are in [`CODEBOOK.md`](CODEBOOK.md) and, in
machine-readable form, in [`docs/variable_definitions.csv`](docs/variable_definitions.csv).

## License

The data are released under the Creative Commons Attribution 4.0
International License (CC BY 4.0). See [`LICENSE`](LICENSE).

## Funding

This work was supported by LEAP (Laboratory for the Economics of Africa's
Past) at Stellenbosch University.

## Contact

Johan Fourie, Stellenbosch University: johanf@sun.ac.za

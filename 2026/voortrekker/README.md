# Selection into the Great Trek

Replication data and code for Johan Fourie and Calumet Links, "Selection into the Great Trek," *European Review of Economic History* (forthcoming).

## The study

Between 1835 and 1840, some 12,000 to 14,000 Dutch-speaking colonists left the Cape Colony for the southern African interior. The paper links Voortrekker genealogies to the 1825 Cape Colony census (*opgaafrolle*) and to the British slave compensation records of 1833–34 to ask which households left.

Within districts, linked Trekker households were larger, with more children, similar recorded assets, and fewer slaves, a difference that depends on the linkage. Among linked households, much of the demographic difference reflects the overrepresentation of established married households. Within couples, the differences are smaller and depend on the linkage. Owners with greater emancipation losses were not detectably more likely to trek.

## Contents

| Folder | Contents |
|---|---|
| `data/raw/` | The three sources as plain CSV: the 1825 census, the Voortrekker genealogies and the slave compensation records. |
| `data/linked/` | The record linkage: the final census links, every reviewed link decision, the training labels, the compensation links, and a crosswalk from the linkage sample to the genealogy. |
| `data/analysis/` | The household-level analysis dataset used for the paper's estimates. |
| `docs/` | `variable_definitions.csv`, a machine-readable list of every variable in `data/`. |
| `scripts/` | `load_data.R`, which loads the CSV files, and `build_data_release.R`, which rebuilds `data/` from a replication run. |
| `replication/` | The full analysis: source workbooks, linkage inputs, R code, the model reviews of the linkage (`reviews/`), and the linkage protocol and its departures (`PROTOCOL.md`). |

Variable definitions are in [`CODEBOOK.md`](CODEBOOK.md). All CSV files are UTF-8, with missing values left empty.

## Quick start

From the `2026/voortrekker` folder:

```r
source("scripts/load_data.R")   # loads all CSV files and prints their sizes
analysis %>% filter(is_voortrekker) %>% count(district)
```

## Replicating the paper

Requirements: R 4.5 with tidyverse, clubSandwich, digest, data.table, readxl, writexl, janitor, stringdist, randomForest, xgboost, sandwich, lmtest, MatchIt, nnet, broom, stargazer, ggplot2, patchwork, scales, sf, maps and jsonlite. `replication/output/session_info.txt` lists exact package versions after a run.

From the `2026/voortrekker/replication` folder:

```
Rscript code/run_all.R
```

This runs four steps in turn, taking about 20 minutes:

1. The pipeline with the wife-blind linkage. Outputs go to `output_wife_blind/`.
2. The pipeline with the final linkage. Outputs go to `output/`: tables in `output/tables/`, LaTeX table fragments in `output/tables/tex/` and figures in `output/figures/`.
3. The married-household analyses on both linkages. Outputs go to `output/couples_analysis/`.
4. The exploratory checks reported in the Online Appendix. Outputs go to `output/refine_analyses/`.

The pipeline checks the checksums of the census workbook and the census link decisions before the census analysis, and those of the compensation scope decisions and the owner-link decisions before the compensation analysis. To regenerate `data/` from the run, execute `Rscript scripts/build_data_release.R` from the `2026/voortrekker` folder.

## How the linkage works

The linkage is documented in Appendix A of the paper. In brief:

1. **Parsing.** A parser (`code/parse_names.R`) separates the household head and the wife in every district return, and flags widows and female heads.
2. **Candidate pairs.** Each Voortrekker is compared with census heads in the relevant districts whose surname matches exactly or approximately.
3. **Classifier.** A random forest (`code/linkage.R`) scores each pair on name similarity, the wives' names, surname frequency and district. It was trained on hand labels that two language models reviewed blind to household characteristics, with the authors deciding disagreements.
4. **Review.** Proposals and pairs near the threshold were reviewed by the same two models, which saw identity evidence only. A proposal accepted by both models was linked; the authors decided every other pair that at least one model accepted.

5. **Compensation owners.** Every genealogy record with a candidate owner (same surname, relevant districts) was reviewed blind by two language models, which saw identity evidence only and chose one owner or none. A record is linked to the owner both chose; records on which they differ, or whose owner is also chosen for another record, are not linked, and the owners concerned are excluded from the owner sample (`data/inputs/owner_link_decisions.csv`, `owner_exclusions.csv`).

`data/linked/link_decisions.csv` records every census-link decision. `replication/reviews/` contains the evidence packets the models saw, their verdicts, and the reconciliation files. `replication/PROTOCOL.md` sets out the linkage protocol, fixed before estimation, and every departure from it.

A second linkage uses no spouse information: a classifier without spouse features, whose proposals are taken without review. It serves as a sensitivity check (Section 8.3 of the paper) and is run by setting `VT_LINKAGE=blind`.

## Sources

- **1825 census** (*opgaafrolle*): colonial tax returns for the eleven districts of the Cape Colony, transcribed from the Western Cape Archives and Records Service. The Somerset data come from the Cradock 1823 returns.
- **Voortrekker genealogies**: compiled from published genealogical sources on Voortrekker families.
- **Slave compensation records**: [Ekama (2021)](https://datafirst.uct.ac.za/dataportal/index.php/catalog/848), from the 1833–34 Cape compensation claims at the UK National Archives.

## Citation

Fourie, J. and Links, C. (forthcoming). Selection into the Great Trek. *European Review of Economic History*.

## License

Data and code are released under the Creative Commons Attribution 4.0 International License (CC BY 4.0); see [`LICENSE`](LICENSE).

## Contact

Johan Fourie, Department of Economics, Stellenbosch University: johanf@sun.ac.za. Calumet Links, Department of Economics, Stellenbosch University. Supported by LEAP (Laboratory for the Economics of Africa's Past).

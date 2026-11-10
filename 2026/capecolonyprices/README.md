# Inflation in the long eighteenth-century Cape Colony

Replication package for Johan Fourie (2026), *Inflation in the long eighteenth-century Cape Colony*, submitted to *Studies in Economics and Econometrics*.

The paper reconstructs annual nominal provisioning costs at the Cape from 1700 to 1834. Nine commodity prices enter a fixed-quantity basket, excluding housing, equipment and enslaved-person sale prices. The reference series falls before the 1770s, rises more than threefold between 1795 and the early 1820s, and partly reverses thereafter. The magnitude and timing depend on source-offset and monetary assumptions. The reference index has arithmetic mean 100 in 1770–1779.

## Contents

| Location | Contents |
|---|---|
| `data/inputs/` | Accepted candidate price ledger, audit corrections, monetary assumptions, starting values, accepted companion observations, source-audit decisions, excluded-asset counts and historical event dates. |
| `data/inputs/validation/` | Accepted 49-fold holdout memberships and source-channel mapping. |
| `data/series/` | Headline annual series, nine commodity summaries, basket weights, 43 alternative annual series, historical relatives, decadal intervals and peak diagnostics. |
| `data/series/annual_index_joint_draws.csv.gz` | 135,000 observations: 135 years × 1,000 complete conditional reference trajectories. |
| `data/series/offsets_constant_joint_draws.csv.gz` | Constant-offset trajectories required for Figure 7. |
| `replication/R/` | Estimation, numerical diagnostics, companions, tables, figures, validation and simulation code, plus the shared LEAP theme. |
| `replication/results/` | Minimal frozen summaries of expensive calculations, including the accepted validation and simulation results. |
| `docs/` | Variable definitions, data inventory, figure-input map, paper-output map and dependency versions. |
| `scripts/load_data.R` | Loads every distributed data and frozen-result CSV, including compressed draws, and fails on parsing problems. |

See [CODEBOOK.md](CODEBOOK.md), [variable definitions](docs/variable_definitions.csv), [data inventory](docs/data_inventory.csv) and [paper-output map](docs/output_map.csv). Missing values are empty strings in UTF-8 CSV files. No RDS is required in the download; the runner creates R model objects as intermediates.

## Quick start

Run R from this project folder, where `.here` is located:

```r
source("scripts/load_data.R")
head(annual)
```

For a historical ratio, retain the same `draw_id` across years:

```r
library(dplyr)
library(tidyr)
ratio <- joint_draws |>
  filter(year %in% c(1795, 1822)) |>
  pivot_wider(names_from = year, values_from = index) |>
  mutate(relative = `1822` / `1795`)
quantile(ratio$relative, c(.05, .5, .95))
```

Deflate nominal rixdollar amounts as `nominal_amount / (index / 100)`. For changes, decadal means or regressions, repeat the calculation within each entire draw path and then summarise across draws. Dividing marginal interval endpoints does not produce a valid ratio interval. Draw identifiers in different models do not imply paired cross-model covariance.

The point index is the normalised sum of quantities times exponentiated smoothed log prices. It need not equal the median of the simulated basket. All 90% bands condition on fitted variance parameters, source priors, units, currencies and specification. They omit parameter and structural uncertainty. The fixed-path simulation finds poor coverage for some historical relatives despite near-nominal coverage averaged over model-generated histories. See the appendix simulation outputs before interpreting the bands as historical accuracy.

## Replicating the paper

Tested with R 4.5.2 on Windows. Install the dependencies once:

```r
install.packages(c("tidyverse", "here", "KFAS", "igraph", "patchwork",
                  "scales", "ragg", "pdftools", "digest", "fixest"))
```

The graphics packages require their normal system libraries on Linux. Package versions used in the clean-room test are in `docs/package_versions.csv`. No workspace files, network requests or credentials are used during replication.

From the package root:

```sh
Rscript replication/run_all.R
```

1. **Primary estimation:** applies accepted corrections, fits the nine-good state-space model, simulates 1,000 joint paths, calculates annual inflation and source connectivity, and refreshes the public primary series. The clean-room test took 42.25 seconds; allow several minutes on other machines.
2. **Presentation:** rebuilds equipment and enslaved-person companions, tables, text-number summaries and all twelve figures, using the frozen summaries for expensive analyses. The clean-room test took 35.56 seconds. Numerical results are in `replication/work/analysis/`, LaTeX table bodies in `replication/work/generated_tables/`, and PNG/PDF figures in `replication/work/figures/`.
3. **Optional expensive estimation:** add the flags below. Stage 3 runs after primary estimation and before presentation, replacing the relevant frozen summaries in the working directory. The distribution itself remains compact; a full simulation run can create large intermediate files and take many hours.

| Command | Additional calculations |
|---|---|
| `Rscript replication/run_all.R --stage1-only` | Primary estimation and headline exports only. |
| `Rscript replication/run_all.R --stage2-only` | Presentation after a completed primary run. |
| `Rscript replication/run_all.R --variants` | All 43 alternatives, constant-offset fit, full-period peak diagnostics and numerical starting-value checks. |
| `Rscript replication/run_all.R --bridges` | Five bridge omissions and fixed-parameter source-wedge/true-step calculations. |
| `Rscript replication/run_all.R --validation` | 98 fits across the accepted 49 blocked holdouts, then matched-support summaries. |
| `Rscript replication/run_all.R --simulation` | 1,020 scheduled calibration/stress jobs, 200 paired clean-reference jobs and tighter optimizer checks. |
| `Rscript replication/run_all.R --all-expensive` | All optional calculations above. |

Flags can be combined, including with `--stage2-only` after a primary fit. Checkpointed expensive fits resume automatically. Frozen summaries are copied into `replication/work/analysis/` only if an output does not exist, so completed reruns take precedence. To restore the published baseline, use a fresh copy of the package. Every run writes per-script measured elapsed times to `replication/work/run_times.csv`. The distributed timing record covers the tested default run; the optional full workloads were not re-executed for this release. The original base simulation alone recorded approximately 21 hours of summed job time, before the clean control and refinements; this is not a new measured wall-clock runtime.

For the optional variants, all joint matrices are regenerated locally under `replication/work/analysis/variants/`. Only the reference and constant-offset portable draws are distributed, to keep the download small. The other 42 alternative models have annual conditional summaries and historical/peak summaries in the package.

## Tables, figures and numbers

| Paper output | Scripts and generated files |
|---|---|
| Main Table 1: source coverage | `results_registry.R`, `paper_tables.R` → `coverage_rows.tex`. |
| Main Table 2: blocked validation | `validation_tables.R` → `validation_main_compact_rows.tex`; refits use the `validation_*` scripts. |
| Main Table 3: historical relatives | `fit_primary.R`, `extended_tables.R` → `historical_relative_rows.tex`. |
| Main Figures 1, 2, 3, 6 and 7 | `figures_v2.R`; primary series, source map, alternative summaries, historical events and joint draws. |
| Appendix Figures 4, 5 and 8–12 | `figures_v2.R`; companion, commodity, variant, pass-through, validation and simulation diagnostics. |
| Appendix generated table bodies | `paper_tables.R`, `extended_tables.R`, `validation_tables.R`, `simulation_tables.R`. |
| Appendix tables originally typed in the manuscript | `appendix_tables.R` produces numerical CSVs and table bodies for audit bounds, connectivity, parameters, numerical checks, allocations, published comparisons, decadal companions and excluded assets. |
| Main-text numerical claims | `results_registry.R`, `conclusion_rates.R`, `text_numbers.R`; `paper_claims_registry.csv`, `verified_period_rates.csv`, `largest_annual_increases.csv`, `additional_text_numbers.csv`. |

The [complete output map](docs/output_map.csv) gives appendix table labels and producer scripts. Tables contain numerical bodies; explanatory prose, captions and symbolic model definitions remain in the paper. This package does not compile the manuscript.

## Sources and reconstruction boundary

The primary accepted ledger preserves 1,119 candidate rows before three explicit audit additions. It yields 1,122 candidates after corrections, of which 661 consumption rows aggregate to 632 channel-year cells in 69 channels. Source channels are compiler columns, roles and specifications, not independently verified archival documents. The accepted-ledger boundary includes earlier screening decisions; it does not recreate the original XML-to-candidate screening or claim a new archival reread.

For published compilations, `source_label`, `source_family`, `source_id`, `product_sheet`, `sheet`, `cell`, `source_column`, `source_locator` and `original_file` preserve attribution. `native_amount`, `native_currency` and `native_unit` contain transcribed information; `currency_factor`, `physical_factor`, `conversion_rule`, `physical_rule` and `price_rd_standard` record the reconstruction's transformations. The inherited monetary and basket inputs are accounting comparators, not independently validated price series.

For Orphan Chamber records, `document`, `source_locator`, `observation_id`, `n_lots`, `specification`, `product` and `language` identify accepted document-level wheat and soap observations. `accepted_individual_sales.csv` contains only the historical sale identifiers, prices and recorded characteristics needed for the companion regression. `household_observed_annual.csv` contains observed annual prices and auction counts for eight household goods. The excluded-asset input contains only annual lot counts. Names, full descriptions, XML and unrelated selection outputs are omitted.

Source families and full references:

- **GPIH Cape commodity compilation and VOC ledger transcriptions:** de Zwart, Pim. 2013. “Real Wages at the Cape of Good Hope: A Long-Term Perspective, 1652–1912.” *Tijdschrift voor Sociale en Economische Geschiedenis* 10(2): 28–58. DOI: [10.18352/tseg.198](https://doi.org/10.18352/tseg.198). Workbook sheet/cell locators are preserved; the underlying VOC manuscripts were not newly inspected.
- **Company salary and price comparisons:** Du Plessis, Sophia, and Stan Du Plessis. 2012. “Happy in the Service of the Company: The Purchasing Power of VOC Salaries at the Cape in the 18th Century.” *Economic History of Developing Regions* 27(1): 125–149. DOI: [10.1080/20780389.2012.682398](https://doi.org/10.1080/20780389.2012.682398).
- **Eighteenth-century compiled prices:** van Duin, Pieter, and Robert Ross. 1987. *The Economy of the Cape Colony in the Eighteenth Century*. Intercontinenta 7. Leiden: Centre for the History of European Expansion.
- **Neumark-attributed workbook columns:** Neumark, S. Daniel. 1957. *Economic Influences on the South African Frontier, 1652–1836*. Stanford: Stanford University Press. Bibliographic details checked against the [National Archives catalogue](https://www.nationalarchives.gov.za/node/3791620); the printed price tables were not inspected. Twelve butter dates are corrected against the raw workbook transcription, not the printed book.
- **Printed colonial records:** Theal, George McCall, ed. 1901. *Records of the Cape Colony from March 1811 to October 1812*, vol. VIII; 1904. *Records of the Cape Colony from November to End of December 1825*, vol. XXIV; 1905. *Records of the Cape Colony from December 1827 to April 1831*, vol. XXXV. London: William Clowes and Sons for the Government of the Cape Colony. Source-audit ledgers preserve the inspected locators; scans are not distributed.
- **Cape Blue Books:** Cape of Good Hope. *Blue Book for the Colony of the Cape of Good Hope*, annual returns for the years cited in the accepted ledger, including 1828 and 1833. These are compiler-transcribed entries, not newly inspected originals. See the [UCT Libraries series guide](https://libguides.lib.uct.ac.za/c.php?g=434393&p=2978215). Exact original table/page locators and the provenance of sterling denominations remain unresolved.
- **Estate auctions and probate cloth:** Western Cape Archives and Records Service, Cape Town Archives Repository, Orphan Chamber, MOOC 10 (vendu rolls) and compiler-transcribed MOOC cloth records. Document/row identifiers in the inputs specify the retained records. Source interpretation uses accepted digital transcriptions, not new paleographic verification.

The 37 retained sterling-labelled observations remain conditional on inherited currency assumptions. Missing or uncertain units, source roles and dates are not cured by the statistical model. The accepted XML audit decisions were generated by an AI-assisted reread and are not a blind human validation sample. `appendix_tables.R` recomputes their design-based summaries without redistributing review transcripts.

## Licence and citation

The author's data assembly and code are released under **CC BY 4.0**; see [LICENSE](LICENSE), copied verbatim from the repository's 1713smallpox package. This licence does not grant rights in third-party compilations or archival transcriptions. Prices taken from published compilations are reproduced for scholarly replication with full attribution; please cite the original sources listed above when you use them.

Suggested citation: Fourie, Johan. 2026. “Inflation in the long eighteenth-century Cape Colony.” Manuscript submitted to *Studies in Economics and Econometrics*. Replication package, `2026/capecolonyprices`, [research repository](https://github.com/johanfourieza/research/tree/main/2026/capecolonyprices).

Contact: Johan Fourie, Department of Economics, Stellenbosch University, [johanf@sun.ac.za](mailto:johanf@sun.ac.za).

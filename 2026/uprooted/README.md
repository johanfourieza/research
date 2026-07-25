# Uprooted: Migration, Coercion, and the Roots of Social Connectedness

Replication code and data-source documentation for Fourie (2026), submitted to
*Economic Inquiry* (special issue *Honoring the Contributions of Louis Putterman*).

## Overview

The paper asks whether shared ancestry — the demographic legacy of five
centuries of migration — predicts the strength of modern digital social
networks between places, and whether the answer depends on *how* people moved.
Combining the Putterman–Weil World Migration Matrix with Meta's Social
Connectedness Index, it shows that voluntary settlement leaves a strong
homeland-oriented social trace, indentured migration an intermediate one, and
coerced migration (slavery in the Cape Colony and the Atlantic, convict
transportation to Australia) little or none.

This folder provides the **full analysis code** and a **documented list of every
data source used**, with links. Because almost all of the data are third-party
public datasets, they are **not redistributed here**; the table below points to
each original source. Two restricted datasets are available from the author on
request (see *Restricted data*).

## Citation

> Fourie, J. (2026). *Uprooted: Migration, Coercion, and the Roots of Social
> Connectedness.* Manuscript submitted to Economic Inquiry.

## Principal investigator

- **Johan Fourie**, Department of Economics, Stellenbosch University
  (johanf@sun.ac.za)

## Code

All analysis is in `scripts/`. The single consolidated replication script
reproduces the full pipeline; the remaining scripts are the data-preparation
steps it draws on, plus one standalone robustness check.

| Script | Purpose |
|---|---|
| `uprooted_finalscript.R` | **Master script.** Inlines the data-preparation steps, runs the full 31-step analysis, and runs the referee-response additions. Reproduces every table and figure in the paper. |
| `meta_history_v4.R` | Superseded modular analysis script, retained for reference. |
| `prepare_huguenot_surnames.R`, `prepare_huguenot_settler_data.R`, `prepare_settler_owner_surnames.R` | Cape Colony settler-surname dictionaries and crosswalks. |
| `prepare_indentured_data.R` | Natal Indian indentured-records processor. |
| `prepare_au_convicts.R` | Australian colony-level convict-intensity measure. |
| `prepare_openflights.R` | Bilateral direct-route counts from OpenFlights. |
| `generate_slavevoyages_corridors.R`, `run_slavevoyages_country_models.R`, `generate_slavevoyages_regression_table.R` | Slave Voyages corridor construction and regressions. |
| `generate_forced_migration_external_validity_figure.R`, `regenerate_leap_figures.R` | Figure production. |
| `us_county_hard_infrastructure_analysis.R` | Standalone post-referee robustness check (historical post office and railroad controls). |
| `classify_farms.py`, `extract_census_controls.py`, `extract_census_language.py`, `process_nz_data.py` | Python preparation steps (Cape farm-name classification, SA Census 2022 controls and language shares, NZ 1881 borough → GADM mapping). |
| `slavevoyages_country_crosswalk.csv` | Hand-built crosswalk from historical Slave Voyages place codes to modern countries. |

The scripts read their inputs from a local `Data/` directory (not included here)
and detect paths by walking up to the project root. To reproduce the analysis,
obtain the datasets from the sources below and place them as described in each
script's header.

## Data sources

Every primary dataset is publicly available from the source listed here.

| Dataset | Use in the paper | Source |
|---|---|---|
| Meta Social Connectedness Index (country, GADM1, GADM2; 2021 release) | Dependent variable throughout | Meta Data for Good / Humanitarian Data Exchange — https://data.humdata.org/dataset/social-connectedness-index-county-county |
| Putterman & Weil, World Migration Matrix 1500–2000 (v1.1) | Shared-ancestry measure | Louis Putterman, Brown University — https://sites.google.com/site/econlouisputterman/world-migration-matrix-1500-2000 |
| CEPII Gravity database | Gravity controls (distance, contiguity, language, colonial ties) | CEPII — http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8 |
| CEPII TRADHIST (historical bilateral trade, 1827–2014) | Historical-trade horse-race channel | CEPII — http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=32 |
| Spolaore & Wacziarg genetic distance (F_ST) | Genetic-distance horse-race channel | Enrico Spolaore, Tufts University — https://sites.tufts.edu/enricospolaore/research-data/ |
| Ashraf & Galor (2013), out-of-Africa migratory distance and predicted diversity | Robustness (deep-history proxy) | American Economic Review replication data — https://www.aeaweb.org/articles?id=10.1257/aer.103.1.1 |
| Linguistic, religious and cultural distance | Horse-race channels | Spolaore & Wacziarg data (as above) and the World Values Survey — https://www.worldvaluessurvey.org/ |
| Michalopoulos & Xue (2021), Folklore motif catalogue | Folklore-similarity horse-race channel | Quarterly Journal of Economics replication data — https://doi.org/10.1093/qje/qjab003 |
| UN DESA International Migrant Stock (2020/2024) | Contemporary migrant-stock control | United Nations DESA — https://www.un.org/development/desa/pd/content/international-migrant-stock |
| American Community Survey 2018–2022 (tables B04006, B02015, B03001, B01003) | US county ancestry shares | US Census Bureau — https://www.census.gov/programs-surveys/acs (accessed via the Census API) |
| GADM administrative boundaries (v4.1) | Sub-national geography | GADM — https://gadm.org |
| Slave Voyages: Trans-Atlantic and Intra-American Slave Trade Databases | Atlantic external-validity test | SlaveVoyages.org — https://www.slavevoyages.org |
| 1881 Census of New Zealand (borough birthplace tables) | New Zealand settlement composition | Statistics New Zealand historical census returns — https://www.stats.govt.nz |
| 1901 Census of the Australian colonies | Australian settlement composition | Historical and Colonial Census Data Archive (HCCDA) — https://hccda.ada.edu.au/ |
| South Africa Census 2022 (and 2011, 1911) | SA district population, language and controls | Statistics South Africa — https://www.statssa.gov.za |
| Ekama (2021), Cape slave emancipation records | Cape settler surnames and slave origins | DataFirst, University of Cape Town — https://datafirst.uct.ac.za/dataportal/index.php/catalog/848 |
| OpenFlights airports and routes (2017 snapshot) | Bilateral air-traffic robustness | OpenFlights — https://openflights.org/data.html |
| Aneja & Xu (2024); Donaldson & Hornbeck (2016) | US county hard-infrastructure robustness | Authors' published replication packages (cited in the paper) |

A machine-readable version of this table is in
[`docs/data_sources.csv`](docs/data_sources.csv).

## Restricted data

Two datasets are not publicly redistributable. They are available from the
author (johanf@sun.ac.za) on reasonable request:

- **Cape Colony farm boundaries (the *Bewaarders* polygons, c. 1850).** Used to
  geolocate farms for the Cape settler-surname and slave-origin analyses.
- **Natal Indian indentured records.** The digitised indentured-labourer
  records used in the South African indenture analysis.

## License

The code and documentation in this folder are released under the Creative
Commons Attribution 4.0 International License (CC BY 4.0). See [`LICENSE`](LICENSE).
The third-party datasets linked above remain subject to the licenses and terms
of their original providers.

## Funding

This work was supported by LEAP (Laboratory for the Economics of Africa's Past)
at Stellenbosch University.

## Contact

Johan Fourie, Stellenbosch University: johanf@sun.ac.za — https://www.johanfourie.com

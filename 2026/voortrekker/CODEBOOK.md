# Codebook

All files are UTF-8 encoded CSV with a single header row. Missing values
are represented as empty strings. Counts are non-negative integers; monetary
values are in pounds sterling unless stated otherwise.

A machine-readable version of this codebook is at
[`docs/variable_definitions.csv`](docs/variable_definitions.csv).

---

## `data/raw/cape_census_1825.csv`

Household-level 1825 Cape Colony census. One row per household. 10,420 rows.

### Identifiers and geography

| Variable | Type | Description |
|---|---|---|
| `record_nr` | integer | Sequential record number within the source district return. |
| `name_raw` | string | Household head name as transcribed from the census. |
| `wife_name_raw` | string | Wife's name as transcribed from the census, where recorded. |
| `sublocation` | string | Sub-district or place name, where recorded. |
| `district` | string | Census district. One of: Albany, Beaufort, Cape, Clanwilliam, Cradock, George, Graaff-Reinet, Stellenbosch, Swellendam, Uitenhage, Worcester. |
| `census_id` | integer | Unique household identifier constructed within the pipeline. Used to join to `voortrekker_census_matches.csv` and `analysis_dataset.csv`. |

### Household composition (persons)

Separate counts by race/status category and by gender/age. `_men` and
`_women` are adults; `_sons` and `_daughters` are children under 16.

| Variable | Description |
|---|---|
| `settler_men`, `settler_women` | Adult European settler men and women. |
| `settler_sons`, `settler_daughters` | European settler children. |
| `khoe_men`, `khoe_women`, `khoe_sons`, `khoe_daughters` | Khoekhoe workers resident on the household. |
| `freeblacks_men`, `freeblacks_women`, `freeblacks_sons`, `freeblacks_daughters` | Free Black persons. |
| `prize_men`, `prize_women`, `prize_sons`, `prize_daughters` | Prize Negroes (liberated from intercepted slave ships). |
| `slaves_men`, `slaves_women`, `slaves_sons`, `slaves_daughters` | Enslaved persons on the household. |

### Livestock

| Variable | Description |
|---|---|
| `horses_saddle`, `horses_breeding` | Saddle and breeding horses. |
| `cattle_oxen`, `cattle_breeding` | Draught oxen and breeding cattle. |
| `sheep_wethers`, `sheep_breeding`, `sheep_spanish` | Sheep by type; `sheep_spanish` refers to Merino sheep. |
| `donkeys`, `goats`, `pigs` | Other livestock. |

### Agriculture

| Variable | Description |
|---|---|
| `wheat_sown`, `barley_sown`, `oats_sown`, `rye_sown` | Grain sown, in muids. |
| `wheat_reaped`, `barley_reaped`, `rye_reaped`, `oats_reaped` | Grain reaped, in muids. |
| `wine` | Wine produced, in leaguers. |
| `brandy` | Brandy produced, in leaguers. |
| `hay` | Hay produced, where recorded. |

---

## `data/raw/voortrekkers.csv`

Voortrekker genealogical records. 2,702 rows; matching sample is 917 adult
males born before 1810 with valid names.

| Variable | Description |
|---|---|
| `surname_oorspronklik` | Surname as recorded in the original genealogical source (Afrikaans spelling). |
| `surname_no_spaces` | Surname with spacing removed. |
| `surname` | Surname, cleaned. |
| `name` | First name, original. |
| `name_proper` | First name, standardised. |
| `number_original`, `number_a1_inserted` | Genealogical reference numbers from the source. |
| `id` | Individual identifier within the Voortrekker dataset. |
| `birth_place` | Place of birth. |
| `dob` | Date of birth, where recorded. |
| `birth_year` | Year of birth. |
| `baptised_place`, `baptise_date` | Place and date of baptism. |
| `birthyear`, `babtise_year` | Year of birth/baptism, parsed. |
| `place` | Place of death (where recorded). |
| `birth_or_baptise_year` | Year used in the analysis (birth if available, else baptism). |
| `dod` | Date of death. |
| `death_year` | Year of death. |
| `m_place`, `m_date`, `marry_year` | Marriage place, date, year (first marriage). |
| `m_to` | Spouse (first marriage). |
| `surname.1` | Wife's surname. |
| `wyk` | Sub-district (*wyk*). |
| `distrik` | District. |
| `move_on` | Indicator for whether the individual joined the Great Trek. |
| `move_year` | Year of departure on the Trek. |
| `move_with` | Trek leader or party. |
| `move_to` | Destination. |
| `s_m_place` | Second marriage place, where recorded. |
| `notes` | Free-text notes from the genealogical source. |
| `leaders` | Indicator for known Voortrekker leaders. |

---

## `data/raw/slave_compensation.csv`

Slave compensation records, individual-slave level. 36,419 rows. Source:
[Ekama (2021)](https://datafirst.uct.ac.za/dataportal/index.php/catalog/848).

| Variable | Description |
|---|---|
| `name` | Slave's given name as recorded. |
| `age`, `age_2` | Age as recorded; `age_2` is a cleaned version. |
| `gender` | Gender. |
| `occupation`, `occ_cat`, `hisco` | Occupation as recorded, coarse category, and HISCO occupational code. |
| `origin`, `origin_exact`, `origin_reg` | Origin as recorded, cleaned, and regional category. |
| `ucl` | Identifier from the UCL Legacies of British Slave-Ownership database. |
| `owner_surname`, `owner_name` | Owner name. |
| `owner_brit`, `owner_hugenoot`, `owner_exslave`, `owner_minor`, `owner_deceased` | Owner attribute flags. |
| `owner_note` | Free-text owner notes. |
| `place_a`, `place_b` | Place of registration. |
| `valuation` | Appraised valuation in pounds sterling. |
| `compensation` | Compensation actually paid in pounds sterling. |
| `district_name`, `district_num`, `dist_type` | Administrative district. |
| `comments` | Free-text comments. |
| `biblical`, `calendar`, `classical`, `dutch`, `english`, `diminutive`, `european`, `facetious`, `geographical`, `muslim`, `occupational`, `other` | Name-category indicators (exclusive one-hot). |
| `num_slaves` | Number of slaves held by the same owner (owner-level summary). |
| `log_valuation` | Natural logarithm of valuation. |

---

## `data/linked/voortrekker_census_matches.csv`

Accepted Voortrekker-to-census linkage pairs. 558 rows; 536 unique matched
census households.

| Variable | Description |
|---|---|
| `row_id` | Row identifier within the Voortrekker dataset. |
| `vt_surname` | Voortrekker surname. |
| `vt_name` | Voortrekker first name. |
| `name_raw` | Matched census household name. |
| `district` | Census district. |
| `match_score` | Random Forest match probability, or equivalent for expert-review cases. |
| `match_quality` | Tier: `high`, `medium`, `low-accepted`, or `review-accepted`. |
| `census_id` | Join key to `cape_census_1825.csv` and `analysis_dataset.csv`. |

---

## `data/linked/voortrekker_emancipation_matches.csv`

Voortrekker-to-slave-compensation matches on owner name and district. 577
rows.

| Variable | Description |
|---|---|
| `vt_row_id` | Row identifier within the Voortrekker dataset. |
| `census_id` | Join key to the census (NA for Voortrekkers not matched to a census household). |
| `census_corroborated` | 1 if the match is corroborated by an independent census link. |
| `census_match_score` | Match score for the census link, where applicable. |
| `vt_surname`, `vt_name`, `vt_district` | Voortrekker name and district. |
| `owner_surname`, `owner_name`, `owner_district` | Matched slave-owner details. |
| `total_valuation` | Total appraised value of the owner's slaves, in pounds sterling. |
| `total_compensation` | Total compensation paid, in pounds sterling. |
| `num_slaves` | Number of slaves held by the matched owner. |
| `loss` | `total_valuation − total_compensation`, in pounds sterling. |
| `loss_pct` | Loss as a fraction of valuation. |
| `mean_slave_value` | Mean appraised value per slave. |
| `match_score` | Name-and-district match score. |
| `owner_key` | Owner identifier within the compensation dataset. |

---

## `data/analysis/analysis_dataset.csv`

Household-level analysis file. 10,420 rows. Combines the cleaned 1825 census
with a Voortrekker-match flag, a composite wealth index, derived
household-composition variables, and Voortrekker genealogical fields for
matched households. This is the file used to produce the main regression
tables in the paper.

All variables from `cape_census_1825.csv` are included. The additional
variables are:

| Variable | Description |
|---|---|
| `horses` | Sum of saddle and breeding horses. |
| `cattle` | Sum of draught oxen and breeding cattle. |
| `sheep` | Sum of all sheep categories. |
| `total_slaves` | Sum of enslaved men, women, sons and daughters. |
| `total_khoe` | Sum of Khoekhoe workers across gender and age categories. |
| `total_grain_sown` | Sum of wheat, barley, oats and rye sown (in muids). |
| `total_grain_reaped` | Sum of grain reaped (in muids). |
| `wealth_index` | Composite wealth index standardised within district (mean 0, SD 1). Principal component of livestock, slaves, grain and wine/brandy. |
| `wealth_simple` | Simple additive wealth score, unstandardised. |
| `settler_children` | `settler_sons + settler_daughters`. |
| `settler_adults` | `settler_men + settler_women`. |
| `household_size` | Total settler household size (`settler_adults + settler_children`). |
| `children_ratio` | `settler_children / household_size`. |
| `is_voortrekker` | 1 if the household is linked to a Voortrekker record, 0 otherwise. |
| `name_clean`, `has_comma`, `census_surname`, `census_first`, `census_surname_std`, `census_first_std`, `census_first_only` | Cleaned and standardised name fields used in the linkage pipeline. |
| `wife_name_clean`, `wife_has_comma`, `census_wife_surname`, `census_wife_first`, `census_wife_surname_std`, `census_wife_first_std`, `census_wife_first_only` | Cleaned and standardised wife-name fields. |

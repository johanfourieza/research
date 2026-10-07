# Codebook

All files are UTF-8 CSV with one header row; missing values are empty. Counts of persons and animals are integers; grain is in muids, wine and brandy in leaguers, money in pounds sterling. [`docs/variable_definitions.csv`](docs/variable_definitions.csv) lists every variable with its type and number of missing values.

Join keys: `census_id` identifies a census household in every file. `row_id` (`vt_row_id` in the compensation links) identifies one of the 1,220 men in the linkage sample; `data/linked/genealogy_crosswalk.csv` maps it to the genealogy (`source_row` in `data/raw/voortrekkers.csv`).

---

## `data/raw/cape_census_1825.csv`

The 1825 Cape Colony census (*opgaafrolle*), one row per household: 10,783 households in eleven districts. The Somerset data come from the Cradock returns of 1823 and the Clanwilliam and Worcester data from 1824. Source-data handling and excluded entries are documented in `replication/data/inputs/`.

### Identifiers and names

| Variable | Description |
|---|---|
| `census_id` | Household identifier. |
| `district` | Census district: Albany, Beaufort, Cape, Clanwilliam, Cradock, George, Graaff-Reinet, Stellenbosch, Swellendam, Uitenhage or Worcester. |
| `record_nr` | Running number of the household within its district return. |
| `source_row`, `source_key` | Row of the entry in the source workbook, and its workbook, sheet and row. |
| `sublocation` | Field cornetcy or place, where recorded. |
| `name_raw` | Name of the household head used in the linkage (equal to `head_name_raw`). |
| `wife_name_raw` | Name of the head's wife used in the linkage (equal to `spouse_name_raw`), where recorded. |
| `head_role` | Head of household as parsed: `male`, `female` or `unresolved`. |
| `head_name_raw` | Name of the head as parsed. |
| `spouse_name_raw` | Name of the wife of a male head, as parsed. |
| `spouse_source_row` | Source row from which the wife's name was read (the same row or a continuation row). |
| `husband_named_absent` | For a female head (for example, a widow entered under her late husband's name), the man's name in the entry. |
| `annotation` | Annotation in the return, such as a widow marker. |

### Persons

`_men` and `_women` are adults; `_sons` and `_daughters` are children, generally sons under 16 and daughters under 20.

| Variable | Description |
|---|---|
| `settler_men`, `settler_women`, `settler_sons`, `settler_daughters` | European settlers. |
| `khoe_men`, `khoe_women`, `khoe_sons`, `khoe_daughters` | Khoekhoe workers resident with the household. |
| `freeblacks_men`, `freeblacks_women`, `freeblacks_sons`, `freeblacks_daughters` | Free Black persons. |
| `prize_men`, `prize_women`, `prize_sons`, `prize_daughters` | Prize Negroes (people liberated from intercepted slave ships and indentured). |
| `slaves_men`, `slaves_women`, `slaves_sons`, `slaves_daughters` | Enslaved persons. |

### Livestock and production

| Variable | Description |
|---|---|
| `horses_saddle`, `horses_breeding` | Saddle and breeding horses. |
| `cattle_oxen`, `cattle_breeding` | Draught oxen and breeding cattle. |
| `sheep_wethers`, `sheep_breeding`, `sheep_spanish` | Sheep; `sheep_spanish` are merinos. |
| `donkeys`, `goats`, `pigs` | Other livestock. |
| `wheat_sown`, `barley_sown`, `oats_sown`, `rye_sown` | Grain sown (muids). |
| `wheat_reaped`, `barley_reaped`, `oats_reaped`, `rye_reaped` | Grain reaped (muids). |
| `hay` | Hay, where recorded. |
| `wine`, `brandy` | Wine and brandy produced (leaguers). |

---

## `data/raw/voortrekkers.csv`

Voortrekker genealogical records: 2,702 rows, one per person entry, with individual, spouse and trek fields. The linkage uses the 1,220 named men born before 1810 or with unknown birth year.

| Variable | Description |
|---|---|
| `source_row` | Row in the source workbook (row 1 is the header); joins to `data/linked/genealogy_crosswalk.csv`. |
| `surname_oorspronklik`, `surname_no_spaces`, `surname` | Surname as in the source, without spaces, and standardised. |
| `name`, `name_proper` | First names as in the source and standardised. |
| `number_original`, `number_a1_inserted`, `id` | Genealogical reference numbers and the person identifier of the source. |
| `birth_place`, `dob`, `birth_year`, `birthyear` | Place, date and year of birth. |
| `baptised_place`, `baptise_date`, `babtise_year` | Place, date and year of baptism. |
| `birth_or_baptise_year` | Birth year, or baptism year where the birth year is missing. |
| `place`, `dod`, `death_year` | Place, date and year of death. |
| `m_place`, `m_date`, `marry_year`, `m_to`, `surname.1` | First marriage: place, date and year, and the wife's first names and surname. |
| `s_m_place` | Place of a second marriage. |
| `wyk`, `distrik` | Ward and district of residence before the Trek. |
| `move_on`, `move_year`, `move_with`, `move_to` | Trek participation, year of departure, trek leader or party, and destination. |
| `notes`, `leaders` | Notes from the source, and trek leaders named in it. |

---

## `data/raw/slave_compensation.csv`

The Cape slave compensation records compiled by [Ekama (2021)](https://datafirst.uct.ac.za/dataportal/index.php/catalog/848): 36,417 rows, one per enslaved person.

| Variable | Description |
|---|---|
| `name`, `gender`, `age`, `age_2` | The enslaved person's name, gender, and age as recorded and cleaned. |
| `occupation`, `occ_cat`, `hisco` | Occupation as recorded, category, and HISCO code. |
| `origin`, `origin_exact`, `origin_reg` | Origin as recorded, cleaned, and by region. |
| `owner_surname`, `owner_name` | Owner. |
| `owner_brit`, `owner_hugenoot`, `owner_exslave`, `owner_minor`, `owner_deceased`, `owner_note` | Owner characteristics and notes. |
| `valuation`, `compensation`, `log_valuation` | Appraised value, compensation paid, and log appraised value (pounds sterling). |
| `num_slaves` | Number of enslaved persons on the same claim. |
| `place_a`, `place_b`, `district_name`, `district_num`, `dist_type` | Place and district of registration. |
| `ucl` | Identifier in the UCL Legacies of British Slavery database. |
| `comments` | Comments. |
| `biblical`, `calendar`, `classical`, `dutch`, `english`, `diminutive`, `european`, `facetious`, `geographical`, `muslim`, `occupational`, `other` | Category of the enslaved person's name (one indicator per category). |

---

## `data/linked/voortrekker_census_matches.csv`

The final census links: 569 rows, one per linked Voortrekker and census household.

| Variable | Description |
|---|---|
| `row_id`, `census_id` | The linked genealogy row and census household. |
| `vt_surname`, `vt_name` | Voortrekker name. |
| `census_name`, `census_district` | Census head and district. |
| `block_type` | How the pair entered the candidate set: `exact` surname, approximate surname (`fuzzy`), `cross_district` search, or `decided_pair` (outside the candidate set, decided by the authors). |
| `evidence_state` | Spouse evidence: `agrees` (wives' names agree), `not_comparable` (missing or inconclusive), `contradicts`. |
| `classifier_score` | Random forest match probability. |
| `classifier_status` | Status of the pair in the classifier output: `proposed`, `review_band` (within 0.15 of its threshold), `review_contradiction`, `review` and `not_linked` (other pairs submitted for review), or `not proposed (supplementary candidate)`. |
| `review_claude`, `review_codex` | Verdicts of the two blind model reviews: `ACCEPT`, `REJECT` or `UNCERTAIN`. |
| `decided_by` | `Both models` (classifier proposal accepted by both reviews) or `Author adjudicated`. |

## `data/linked/link_decisions.csv`

Every reviewed pair: 923 rows, of which 569 are retained.

| Variable | Description |
|---|---|
| `pair_id` | `row_id|census_id`. |
| `row_id`, `census_id` | The reviewed pair. |
| `decision` | `retain` or `reject`. |
| `basis` | Reason for the decision. |
| `final_quality` | For retained pairs, `Both models` (classifier proposal accepted by both reviews) or `Author adjudicated`; empty for rejected pairs. |
| `identity_ambiguous` | Whether the identity remains ambiguous (false for every pair). |
| `classifier` | Status of the pair in the classifier output, as `classifier_status` above. |
| `spouse_evidence` | Spouse-evidence state, as `evidence_state` above; empty for pairs outside the scored candidate set. |
| `claude`, `codex` | Verdicts of the two blind model reviews. |
| `supplementary_candidate` | The pair was identified by hand and entered the review as a supplementary candidate. |

## `data/linked/training_labels.csv`

The 972 resolved labels (160 matches). Of these, 910 pairs arise as candidates in the linkage: 813 are used for training and cross-validation and 97 form the held-out audit sample. The remaining 62 pairs are not candidates and do not enter the classifier.

| Variable | Description |
|---|---|
| `row_id`, `census_id` | The labeled pair. |
| `label` | 1 = match, 0 = non-match. |
| `label_source` | `both model reviews` (the two blind reviews agreed with each other and, for hand-labeled pairs, with the hand label) or `authors` (decided by the authors). |
| `sample` | `hand-labeled pair` (from the four labelers) or `supplementary sample` (drawn from the candidate set). |
| `hand_label` | The labeler's label, for hand-labeled pairs. |

## `data/linked/voortrekker_emancipation_matches.csv`

Voortrekker links to slave owners in the compensation records: 229 rows, one per owner, the links on which two blind model reviews agree (`replication/reviews/owner_links/`).

| Variable | Description |
|---|---|
| `vt_row_id` | Genealogy row. |
| `census_id`, `census_corroborated`, `census_match_score` | The person's census household, whether the census link corroborates the identity, and its score. |
| `vt_surname`, `vt_name`, `vt_district` | Voortrekker name and district. |
| `owner_surname`, `owner_name`, `owner_district`, `owner_key` | Matched owner and owner identifier. |
| `total_valuation`, `total_compensation` | Owner's total appraised value and compensation. |
| `num_slaves`, `mean_slave_value` | Number of enslaved persons across the owner's claims, and their mean appraised value. |
| `loss`, `loss_pct` | Valuation minus compensation, and 100 × loss / valuation (percent). |
| `match_score` | Name-and-district similarity score of the decided owner (reported only; the links are the review decisions). |

## `data/linked/genealogy_crosswalk.csv`

Links the 1,220 men of the linkage sample to the genealogy: 1,220 rows.

| Variable | Description |
|---|---|
| `row_id` | Identifier of the man in the linkage sample, as `row_id` and `vt_row_id` in the files above. |
| `source_row` | His row in `data/raw/voortrekkers.csv` (column `source_row`) and in the source workbook. |
| `id` | His genealogical identifier (column `id` of `voortrekkers.csv`; not unique there, because some persons appear in several entries). |

---

## `data/analysis/analysis_dataset.csv`

The household-level dataset used for the paper's estimates: the 10,783 census households with all variables of `cape_census_1825.csv`, plus:

| Variable | Description |
|---|---|
| `is_voortrekker` | Household linked to a Voortrekker (569 households). |
| `married_couple` | Male head with a named wife (6,380 households). |
| `settler_children` | `settler_sons + settler_daughters`. |
| `settler_adults` | `settler_men + settler_women`. |
| `household_size` | Settler adults and children. |
| `children_ratio` | `settler_children / household_size`. |
| `horses`, `cattle`, `sheep` | Sums of the horse, cattle and sheep categories. |
| `total_slaves`, `total_khoe` | Enslaved persons and Khoekhoe workers. |
| `total_grain_sown`, `total_grain_reaped` | Wheat, barley, oats and rye sown and reaped. |
| `wealth_index` | First principal component of horses, cattle, sheep, goats, pigs, slaves, Khoekhoe workers, wheat reaped and wine (mean 0, standard deviation 1.87; missing where an input is missing). |
| `wealth_simple` | Sum of the standardised horses, cattle, sheep, slaves, wheat reaped and wine. |
| `census_surname`, `census_first`, `census_surname_std`, `census_first_std`, `census_first_only` | Head's name split and standardised for linkage. |
| `census_wife_surname`, `census_wife_first`, `census_wife_surname_std`, `census_wife_first_std`, `census_wife_first_only` | Wife's name split and standardised for linkage. |

# Codebook

All files are plain UTF-8 CSV. Missing values are empty strings.

## `data/papers.csv`: 8,502 rows, one per working paper

Papers issued from January 2023 to October 2026 that enter at least one
regression.

### Identifiers and groups

| Variable | Type | Description |
|---|---|---|
| `uid` | string | Public identifier of the paper, `source:series:paper` (e.g. `nber:nber:w32946`). Unique. Also the paper cluster for standard errors. |
| `series` | string | Working-paper series (23 values in this file; `arxiv/economics` is the economics collection of arXiv). |
| `issue_month` | string | Month of issue, `YYYY-MM`. |
| `year` | integer | Year of issue. |
| `source_quarter` | string | Series, year and quarter of issue (e.g. `nber 2024 2` is NBER, 2024, April–June). Fixed-effect group. |
| `field` | string | Broad field code derived from the paper's JEL classification (letters `A` to `R` and `Z`), or `unknown` when no field could be assigned. Fixed-effect group. |

### Outcomes

An outcome is empty when it could not be measured for the paper. Empty is not
zero.

| Variable | Type | Description |
|---|---|---|
| `any_ra` | 0/1 | 1 if the acknowledgements credit any research assistance, named or collective. |
| `ra_count` | integer | Number of research assistants thanked by name. |
| `author_count` | integer | Number of authors printed on the paper. |
| `pages` | integer | Pages in the PDF. |
| `main_pages` | integer | Pages before the first appendix heading. Empty for 9 papers whose page text could not be verified. |
| `appendix_pages` | integer | Pages from the first appendix heading to the end; `main_pages + appendix_pages = pages`. |
| `log_ra` | number | log(1 + `ra_count`). |
| `ra_tail_limited` | integer | `ra_count` capped at 8. |
| `method_breadth` | integer | Number of documented method attributes. |
| `solo` | 0/1 | 1 if the paper has one author. |
| `no_assistance_solo` | 0/1 | 1 if the paper has one author and the acknowledgements can be assessed and credit no assistance. |
| `any_reader` | 0/1 | 1 if the acknowledgements credit any reader or adviser. |

The letter reports the first four outcomes and the two page components. The
last six outcomes complete the registered family of ten and are needed only
for the Romano–Wolf adjustment.

### AI indicators and controls

| Variable | Type | Description |
|---|---|---|
| `disclosed` | 0/1 | 1 if the paper states that its authors used generative AI in producing it. |
| `task_label` | string | `no-disclosure`; `operational-or-mixed` (any reported task is coding, data preparation, search, analysis or derivation); `writing-only` (every reported task is drafting, editing, translation or typesetting); `unlabelled` (a disclosure whose tasks are not stated). |
| `marker` | 0/1 | Writing flag: 1 if the strict rule of the econ-ai-detector flags the paper's prose. |
| `log_words` | number | Natural log of the number of prose words the detector scored. |
| `log_windows` | number | Natural log of the number of text windows the detector scored. |

## `data/researcher_links.csv`: 143,295 rows, one per outcome, paper and researcher

| Variable | Type | Description |
|---|---|---|
| `outcome` | string | One of the ten outcome names above. |
| `uid` | string | Paper identifier; matches `papers.csv`. |
| `researcher` | string | Anonymous researcher code, `r0001` to `r5285`. The researcher fixed effect and the researcher cluster. The same person has the same code for every outcome. |
| `weight` | number | Regression weight: 1 divided by the number of the paper's researchers retained for this outcome. Weights sum to 1 within paper and outcome. |

The rows for an outcome are that outcome's estimation sample. A researcher is
retained for an outcome with at least two papers on which the outcome is
measured, after single observations within any fixed-effect group have been
removed repeatedly until none remains. Samples therefore differ by outcome:
7,243 papers for `any_ra`, 7,242 for `ra_count`, 8,502 for `author_count` and
`pages`.

## `data/half_year_shares.csv`: 80 rows, one per scope, indicator and half-year

Computed on papers with supported timing, including papers outside the
regression sample.

| Variable | Type | Description |
|---|---|---|
| `scope` | string | `institutional` (the 23 series other than arXiv; Figure 2), `all_sources`, `arxiv`, `nber`. |
| `outcome` | string | `any_ra` or `disclosed`. |
| `year`, `half` | integer | Year and half-year of issue (1 = January–June, 2 = July–December). |
| `half_year` | string | Label, e.g. `2024 H1`. |
| `time` | number | `year` for the first half and `year + 0.5` for the second. |
| `incomplete` | logical | TRUE for the second half of 2026, which covers July to October. |
| `papers` | integer | Papers on which the indicator can be measured. |
| `share_pct` | number | Percentage of those papers with the indicator equal to 1. |

## `data/registered_tests.csv`: 20 rows, one per registered test

| Variable | Type | Description |
|---|---|---|
| `outcome` | string | Outcome name. |
| `indicator` | string | `marker` (writing flag) or `disclosed`. |
| `coefficient`, `se` | number | Estimate and two-way clustered standard error, in the outcome's units (0/1 outcomes are not multiplied by 100). |
| `raw_p` | number | Unadjusted p-value. |
| `t_observed` | number | `coefficient / se`. |
| `papers`, `researchers`, `rows` | integer | Papers, researchers and paper–researcher rows in the regression. |
| `holm_p` | number | Holm-adjusted p-value across the 20 tests. |
| `rw_p` | number | Romano–Wolf step-down p-value across the 20 tests. |
| `bootstrap_draws` | integer | Bootstrap draws behind `rw_p` (1,999). |

## `data/series_frame.csv`: 24 rows, one per series

| Variable | Type | Description |
|---|---|---|
| `series` | string | Working-paper series. |
| `records` | integer | Paper records in the frame, January 2015 to October 2026. The total is 56,585. |

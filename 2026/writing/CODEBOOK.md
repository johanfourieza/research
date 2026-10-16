# Codebook

All files are plain UTF-8 CSV. Missing values are empty strings.

## `data/papers.csv`: 42,704 rows, one per working paper

| Variable | Type | Description |
|---|---|---|
| `paper_id` | string | Public identifier of the paper, `source:series:paper` (e.g. `nber:nber:w30000`). Unique. |
| `series` | string | Working-paper series (32 values; `arxiv/economics` is the economics collection of arXiv). Used for fixed effects and clustering. |
| `period` | integer | Half-year of issue, coded `2 × year + half`, where half is 0 for January–June and 1 for July–December. 4030 = January–June 2015; 4053 = July–September 2026 (partial). |
| `marker` | 0/1 | 1 if the economics-prose detector flags the paper. Empty if the paper could not be assessed. |
| `colon_flag` | 0/1 | 1 if colons per 1,000 words of selected prose exceed 8.905. Empty if not assessable. |
| `disclosed` | 0/1 | 1 if the paper states that its authors used AI. Empty if not assessable. |
| `group` | string | Affiliation group for papers issued since January 2023: `English`, `Mixed` or `Non-English` (see README). Empty for earlier papers and for papers with no classified affiliation. |

Each indicator is analysed on the papers on which it can be measured, so the
denominators differ by indicator.

## `data/affiliation_classification.csv`: 1,838 rows, one per distinct affiliation

| Variable | Type | Description |
|---|---|---|
| `item_key` | string | Sequential key (`A0001`…). Not linked to any name or affiliation text. |
| `kind` | string | `inst` = an institution name matched in the source records; `aff` = a printed affiliation string classified by its first named institution. |
| `codex` | string | Country assigned by the first classifier (Codex): ISO 3166-1 alpha-2 code, `INTL` (international organisation) or `UNKNOWN`. |
| `claude` | string | Country assigned by the second classifier (Claude), same codes. |
| `final` | string | Final code: the shared code where the classifiers agree; otherwise the adjudicated code. |
| `used_in_groups` | logical | FALSE for `INTL`, `UNKNOWN`, the NBER and the CEPR, which do not enter the affiliation groups. |

## `results/`

Outputs of the scripts. Shares are in percent; estimates and confidence limits
are in percentage points.

| File | Contents |
|---|---|
| `trends.csv` | Share and number of assessable papers by half-year (`time` = year + 0.5 for July–December) and indicator. |
| `correlations.csv` | Paper-level correlations among papers assessable on all three indicators, for papers issued since January 2023 and since January 2025. |
| `shares_by_group.csv` | Share and n by indicator and affiliation group, 2023–2026. |
| `shares_by_group_year.csv` | The same by year. |
| `within_series_estimates.csv` | Linear probability models of each indicator on affiliation group (reference: English) with series × half-year fixed effects and standard errors clustered by series: all papers, excluding arXiv, and 2023 only. |
| `figure1.*` | Figure 1 of the paper. |
| `session_info.txt` | R and package versions of the last run. |

# Codebook

All dates are ISO 8601 (`YYYY-MM-DD`). There are no missing values in the two
data files except where noted.

## `data/article_histories.csv`

One row per article in the 2024 to 2026 issues of *Explorations in Economic
History* (140 rows), sorted by date received.

| Variable | Type | Description |
|---|---|---|
| `title` | text | Article title as printed by the journal. |
| `received` | date | Date the journal received the submission. |
| `revised` | date | Date of the last revision recorded by the journal. |
| `accepted` | date | Date of acceptance. |
| `available_online` | date | Date the article became available online. |

## `data/model_releases.csv`

One row per selected model release announcement (36 rows).

| Variable | Type | Description |
|---|---|---|
| `date` | date | Announcement date printed on the provider's official page. |
| `provider` | text | `Anthropic` or `OpenAI`. |
| `label` | text | Model name shown in the figure; models announced together share one row. |
| `source_url` | text | Official announcement page. |
| `date_source_url` | text | Page from which the date was taken, where the announcement page omits it; otherwise the same as `source_url`. |
| `note` | text | Qualification, such as a preview, phased rollout or joint announcement. |

## `figures/review_by_year.csv`

Written by `scripts/plot_review_lexis.R`. One row per year of acceptance.

| Variable | Type | Description |
|---|---|---|
| `year` | integer | Year in which the articles were accepted. |
| `articles` | integer | Number of articles accepted in that year. |
| `median_months` | number | Median months from received to accepted (days divided by 30.44). |
| `median_releases` | integer | Median number of release announcements dated after an article was received and on or before its acceptance. |

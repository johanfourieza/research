# Large language models and economic history: A comment on Ferrara (2026)

Data and code to reproduce Figure 1 and the review-time statistics in
Section 3 of Fourie (2026), forthcoming in *Explorations in Economic History*.

## Overview

The comment extends Ferrara's guide to large language models in economic
history from data production to the rest of the research process. Section 3
argues that organising research around these models is a recurring decision,
because the models change while a paper is under review. Figure 1 sets the
publication histories of 140 articles in the 2024 to 2026 issues of
*Explorations in Economic History* against selected model release
announcements by Anthropic and OpenAI.

This release contains only what is needed to reproduce that figure and the
numbers quoted with it. The comment has no other quantitative results.

## Citation

> Fourie, Johan. 2026. "Large language models and economic history: A comment
> on Ferrara (2026)." *Explorations in Economic History*, forthcoming.

## Contents

| File | Rows | Description |
|---|---|---|
| `data/article_histories.csv` | 140 | One row per article: title and the dates it was received, revised, accepted and made available online. |
| `data/model_releases.csv` | 36 | One row per selected model release announcement, with its date, provider and official source. |
| `scripts/plot_review_lexis.R` | | Draws Figure 1 and computes the statistics. |
| `figures/review_lexis.pdf`, `.png` | | Figure 1 as published. |
| `figures/review_by_year.csv` | 5 | Median months to acceptance and median number of releases, by year of acceptance. |

Data are plain UTF-8 CSV. Dates are ISO 8601 (`YYYY-MM-DD`). Variable
definitions are in [`CODEBOOK.md`](CODEBOOK.md).

## Reproducing the figure

```
Rscript scripts/plot_review_lexis.R
```

The script can be run from any directory. It requires R with `tidyverse` and
`ggrepel` (the figure was produced with R 4.5.2) and runs in a few seconds. It
rewrites the three files in `figures/` and prints the statistics quoted in the
comment:

- 140 articles, with a median of 8.0 months from submission to acceptance;
- a median of five release announcements between submission and acceptance,
  ranging from one to 17;
- by year of acceptance, a median of two announcements in 2023, four in 2024,
  six in 2025 and ten in 2026.

## Data collection

**Article histories.** The received, revised, accepted and available-online
dates are those the journal prints on each article's page. They were
transcribed for the articles in the 2024 to 2026 issues and consulted on
4 October 2026. One article was received in October 2020; all others were
received from June 2022 onwards. Months are calendar days divided by 30.44.

**Model releases.** Dates are those printed on the providers' official
announcement pages, checked on 23 September 2026. The list is a selection, not
a census: it follows prominent general-purpose releases and named version
changes from ChatGPT's public preview in November 2022, groups models
announced together and includes previews and phased rollouts. It omits many
lower-cost variants, API snapshots and specialised models. The dates therefore
show how often the available tools changed; they do not support comparisons of
the two providers' total release counts. A release counts for an article if it
was announced after the article was received and on or before the day it was
accepted.

## License

CC BY 4.0. See [`LICENSE`](LICENSE).

## Funding

No specific grant from a public, commercial or not-for-profit funding agency
supported this research.

## Contact

Johan Fourie, LEAP, Department of Economics, Stellenbosch University —
johanf@sun.ac.za

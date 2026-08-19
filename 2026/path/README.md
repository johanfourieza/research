# Replication package: Testing for path dependence in economic history publications

This package reproduces every table, figure and in-text statistic in

> Fourie, J. "Testing for path dependence in economic history publications."
> *Cliometrica* (under review).

Canonical public home of this package:
https://github.com/johanfourieza/research/tree/main/2026/path

The paper documents strong persistence between a journal article's early
citations and its long-run citations in the four core economic history
journals, and examines how much of that persistence can be attributed to
observable fundamentals.

## Contents

```
replication/
├── README.md            this file
├── CODEBOOK.md          variable-level documentation for every data file
├── LICENSE              MIT (code) + CC BY 4.0 (data)
├── run_all.R            one-command reproduction (Rscript run_all.R)
├── data/
│   ├── raw/             hand-collected inputs
│   │   ├── Journals_2026_clean.csv     3,250 articles, 4 core journals, 1997-2025,
│   │   │                               with annual Google Scholar snapshots
│   │   └── Conference_Papers.xlsx      hand-transcribed EHA programmes
│   │                                   2006-2025 + dissertation prizes
│   └── cache/           API-derived files (shipped so no network is needed)
│       ├── openalex_paper_matches.rds  paper -> OpenAlex work ID (3,241/3,250 core)
│       ├── network_citation_data.rds   72,695 citation links (53 MB)
│       ├── network_paper_metrics.rds   per-paper network metrics
│       ├── citing_field_data.rds       discipline of 37,853 citing works
│       ├── citing_field_linked.rds     link-level discipline merge
│       ├── repec_author_data.rds       RePEc author seniority / h-index
│       ├── conference_parsed_data.rds  EHA (workbook) + EHS (HTML) programmes
│       ├── prize_paper_data.rds        Cole / Ashton / Figuerola matches
│       └── prize_dissertation_data.rds Gerschenkron / Nevins recipients
├── scripts/             the pipeline (see "Script map" below)
├── results/             intermediate .rds objects (created by the run)
└── output/
    ├── tables/  figures/  logs/        created by the run
```

## Requirements

- R (the published run used R 4.5.2 on Windows; `output/logs/sessionInfo.txt`
  records the exact environment).
- Packages: `data.table`, `lfe`, `fixest`, `stargazer`, `boot`, `stringdist`,
  `ggplot2`, `scales`, `igraph`, `readxl`, `patchwork`.
  Install with:

  ```r
  install.packages(c("data.table", "lfe", "fixest", "stargazer", "boot",
                     "stringdist", "ggplot2", "scales", "igraph", "readxl",
                     "patchwork"))
  ```

- No internet connection and no API credentials are required: scripts 01-10
  run entirely from `data/raw/` and `data/cache/`.

## How to reproduce

From the `replication/` directory:

```
Rscript run_all.R
```

This runs scripts 01-10 in order, each in a fresh R session, and writes all
tables to `output/tables/`, all figures to `output/figures/` (PNG and PDF),
and one log per script to `output/logs/`. Expected runtime: about 3 minutes
on a standard desktop; the permutation tests (scripts 04 and 05) and the
decomposition bootstrap (script 08) account for most of it.

Randomness: every stochastic script sets its own seed at the top
(constants defined in `scripts/_setup.R`), so results are reproducible
script-by-script and independent of run order.

## Script map

| Script | Purpose | Outputs used in the paper |
|---|---|---|
| `00_data_collection.R` | one-time API collection (OpenAlex, RePEc, conference programmes, prizes). NOT needed to replicate; requires `OPENALEX_EMAIL` (+ optional `OPENALEX_API_KEY`) and `REPEC_API_KEY` environment variables | the files in `data/cache/` |
| `00b_citing_fields.R` | one-time OpenAlex discipline query for all citing works | `citing_field_data.rds` |
| `00c_rebuild_conference_cache.R` | offline rebuild of the conference cache from the EHA workbook + parsed EHS rows | `conference_parsed_data.rds`, `prize_dissertation_data.rds` |
| `01_build_sample.R` | variables, estimation sample, attrition table, topic dictionary, OpenAlex linkage diagnostics | Table A1 (attrition), Table A2 (topic dictionary), linkage statistics |
| `02_main_results.R` | summary statistics and main regressions | Table 1, Table 2 |
| `03_robustness.R` | leave-one-out, bootstrap, thresholds, extended sample, growth outcome, PPML, keep-"other" topics, no-top-institution | Table 2 discussion, Appendix B |
| `04_placebo.R` | fast-starter permutation test | Appendix B figure |
| `05_conference.R` | conference matching, premium, session timing, author exposure, conference placebo | Section 6.3, Appendix E |
| `06_mechanisms.R` | citing-discipline decomposition, self-citations, cascades, concentration | Sections 6.2 and 6.4 |
| `07_heterogeneity.R` | by topic, by authorship, by institution, by publication cohort | Section 4, Appendix B (incl. Table B1) |
| `08_attenuation_luck.R` | fast-starter attenuation; predictability and decomposition of early citations (with bootstrap SEs); within-issue position reduced forms and design checks | Table 3, Section 5, Appendix C |
| `09_within_author.R` | paper-year panel, within-author regressions, reverse causality | Table 4, Appendix D/E |
| `10_figures.R` | all figures from the saved results | all paper figures |

The mapping from these outputs to the numbered figures/tables in the
manuscript is recorded in `paper/sync_outputs.R` (one level up, not part of
the public package) and in the manuscript's replication note.

## Data provenance and licences

- **Google Scholar citation snapshots** (`Journals_2026_clean.csv`, columns
  `Google14`-`Google26`): cumulative citation counts collected by hand by
  research assistants at LEAP (Stellenbosch University) in **February-March of
  each year from 2014 to 2026**. Each column holds the cumulative count
  observed in that year's snapshot; a zero means no citations were recorded at
  that date.
- **OpenAlex-derived files** (`openalex_paper_matches.rds`,
  `network_citation_data.rds`, `network_paper_metrics.rds`,
  `citing_field_data.rds`, `citing_field_linked.rds`): built from the OpenAlex
  API (https://openalex.org, data released under CC0) in 2026. OpenAlex
  contents change over time; re-running `00_data_collection.R` will not
  reproduce these files exactly, which is why they are shipped.
- **RePEc-derived file** (`repec_author_data.rds`): aggregate author-level
  statistics (first publication year, h-index, NBER working-paper indicator)
  retrieved from the RePEc API in 2026.
- **Conference programmes** (`conference_parsed_data.rds`,
  `Conference_Papers.xlsx`): bibliographic facts (titles, authors, sessions)
  from publicly posted EHA programmes (https://eh.net, hand-transcribed
  2006-2025 with session times) and EHS programmes
  (https://ehs.org.uk/society/resources/ehs-annual-conference-archive/, HTML
  parse, 2003-2024). The raw programme files are not redistributed. The EHES
  biennial meeting is not covered (see the paper, Section 3, for the scope
  statement).
- **Prizes** (`prize_paper_data.rds`, `prize_dissertation_data.rds`): winner
  lists scraped from eh.net (Cole, Gerschenkron, Nevins), ehs.org.uk (Ashton)
  and uc3m.es (Figuerola).

## What is deliberately NOT shipped

- API keys and credentials (use your own; see `00_data_collection.R`).
- Raw conference programme PDFs/HTML (bulk; URLs above).
- `openalex_topic_data.rds` (an earlier side-file with zero coverage on the
  2012-2021 estimation sample; the associated controls are not used in the
  paper and are set to NA by `01_build_sample.R`).

## Known caveats

- The conference analysis rests on 85 matched presenters in the estimation
  sample; the paper reports it as imprecise.
- Google Scholar counts occasionally decline between snapshots; the growth
  outcome clamps negative growth at zero before taking logs (documented in
  `01_build_sample.R`).

## Citation

If you use these data, please cite the article above and, for the citation
links, OpenAlex (Priem, Piwowar and Orr 2022, arXiv:2205.01833).

Contact: Johan Fourie, Stellenbosch University (johanf@sun.ac.za).

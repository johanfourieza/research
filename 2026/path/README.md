# Replication package: Testing for path dependence in economic history publications

This package reproduces every table, figure and in-text statistic in

> Fourie, J. (2026). "Testing for path dependence in economic history
> publications." *Cliometrica*. https://doi.org/10.1007/s11698-026-00346-w

Canonical public home of this package:
https://github.com/johanfourieza/research/tree/main/2026/path

**Version 2.0 (October 2026) matches the published article.** It supersedes
the accepted-manuscript package (commit `49e0afc`, tag
`2026-path-accepted-2026-08`), which is preserved in the repository history.
See `CHANGELOG.md` for what changed and why. In short: a replication audit
during proof correction found errors in the automated conference matching and
inaccuracies in the description of two auxiliary datasets; the conference
analysis now rests on a reviewed match ledger, the citation-source analysis on
a metadata-screened network, and a final script checks the regenerated
results against the published numbers. The main persistence results are
unchanged.

The paper documents strong persistence between a journal article's early
citations and its long-run citations in the four core economic history
journals, and examines how much of that persistence can be attributed to
observable fundamentals.

## Contents

```
2026/path/
├── README.md            this file
├── CHANGELOG.md         version history (what changed between accepted and published)
├── CODEBOOK.md          variable-level documentation for every data file
├── LICENSE              MIT (code) + CC BY 4.0 (data)
├── run_all.R            one-command reproduction (Rscript run_all.R)
├── data/
│   ├── raw/             hand-collected and hand-reviewed inputs
│   │   ├── Journals_2026_clean.csv            3,250 articles, 4 core journals, 1997-2025,
│   │   │                                      with annual Google Scholar snapshots
│   │   ├── Conference_Papers.xlsx             hand-transcribed EHA programmes
│   │   │                                      2006-2025 + dissertation prizes
│   │   ├── conference_programme_records.csv   3,565 EHA/EHS programme entries from the
│   │   │                                      corrected re-extraction, with candidate links
│   │   └── conference_match_ledger.csv        review decision for each of the 317 candidates
│   └── cache/           API-derived files (shipped so no network is needed)
│       ├── openalex_paper_matches.rds  paper -> OpenAlex work ID (3,241/3,250 core)
│       ├── openalex_metadata_screen.csv title/year verification of those assignments
│       ├── network_citation_data.rds   72,695 citation links (53 MB)
│       ├── network_paper_metrics.rds   per-paper network metrics
│       ├── citing_field_data.rds       discipline of 37,853 citing works
│       ├── citing_field_linked.rds     link-level discipline merge
│       ├── repec_author_data.rds       RePEc author seniority / h-index
│       ├── conference_parsed_data.rds  EHA + EHS programmes as parsed for the accepted
│       │                               version (used only by the archived matcher)
│       ├── prize_paper_data.rds        Cole / Ashton / Figuerola matches
│       └── prize_dissertation_data.rds Gerschenkron / Nevins recipients
├── scripts/             the pipeline (see "Script map" below)
│   ├── archive/         the accepted version's automated conference matcher (not run)
│   └── provenance/      audit scripts that built the reviewed records and the screen (not run)
├── results/             intermediate .rds objects (created by the run)
└── output/
    ├── tables/  figures/  logs/        created by the run
```

## Requirements

- R (the published run used R 4.5.2 on Windows; `output/logs/sessionInfo.txt`
  records the exact environment).
- Packages: `data.table`, `lfe`, `fixest`, `stargazer`, `boot`, `stringdist`,
  `stringi`, `ggplot2`, `scales`, `igraph`, `readxl`, `patchwork`.
  Install with:

  ```r
  install.packages(c("data.table", "lfe", "fixest", "stargazer", "boot",
                     "stringdist", "stringi", "ggplot2", "scales", "igraph",
                     "readxl", "patchwork"))
  ```

- No internet connection and no API credentials are required: scripts 01-11
  run entirely from `data/raw/` and `data/cache/`.

## How to reproduce

From the `2026/path/` directory:

```
Rscript run_all.R
```

This runs scripts 01-10 in order, each in a fresh R session, writes all
tables to `output/tables/`, all figures to `output/figures/` (PNG and PDF)
and one log per script to `output/logs/`, and then runs
`11_check_published_numbers.R`, which compares the regenerated results with
the statistics reported in the published article and stops with an error if
any is not reproduced. Expected runtime: a few minutes on a standard desktop;
the permutation tests (scripts 04 and 05) and the decomposition bootstrap
(script 08) account for most of it.

Randomness: every stochastic script sets its own seed at the top
(constants defined in `scripts/_setup.R`), so results are reproducible
script-by-script and independent of run order.

## Script map

| Script | Purpose | Outputs used in the paper |
|---|---|---|
| `00_data_collection.R` | one-time API collection (OpenAlex, RePEc, conference programmes, prizes). NOT needed to replicate; requires `OPENALEX_EMAIL` (+ optional `OPENALEX_API_KEY`) and `REPEC_API_KEY` environment variables | the files in `data/cache/` |
| `00b_citing_fields.R` | one-time OpenAlex discipline query for all citing works | `citing_field_data.rds` |
| `00c_rebuild_conference_cache.R` | offline rebuild of the accepted version's conference cache from the EHA workbook + parsed EHS rows | `conference_parsed_data.rds`, `prize_dissertation_data.rds` |
| `01_build_sample.R` | variables, estimation sample, attrition table, topic dictionary, OpenAlex linkage diagnostics | Table 5 (attrition), Table 6 (topic dictionary), linkage statistics |
| `02_main_results.R` | summary statistics and main regressions | Table 1, Table 2 |
| `03_robustness.R` | leave-one-out, bootstrap, thresholds, extended sample, growth outcome, PPML, keep-"other" topics, no-top-institution | Section 4.3, Appendix B |
| `04_placebo.R` | fast-starter permutation test | Fig. 5 |
| `05_conference.R` | reviewed conference linkage, conditional and unconditional association, session timing, author exposure, permutation test, sensitivity | Section 6.3, Appendix E, Table E1 (sensitivity CSV) |
| `06_mechanisms.R` | citing-discipline decomposition on the screened network, self-citations, cascades, concentration | Sections 6.2 and 6.4, Appendix A.3 |
| `07_heterogeneity.R` | by topic, by authorship, by institution, by publication cohort | Section 4.3, Appendix B, Fig. 6, Fig. 7 |
| `08_attenuation_luck.R` | fast-starter attenuation; predictability and decomposition of early citations (with bootstrap SEs); within-issue position reduced forms and design checks | Table 3, Table 4, Table 7, Section 5, Appendix C |
| `09_within_author.R` | paper-year panel, within-first-author regressions, reverse causality and balance | Table 8, Appendix D, Appendix E |
| `10_figures.R` | all figures from the saved results | Figs. 1-8 |
| `11_check_published_numbers.R` | asserts that the run reproduces the published statistics | `output/logs/11_check_published_numbers.log` |

Output file names keep the manuscript's working names. Published numbering:
`Fig_Persistence` = Fig. 1, `Fig_Unpredictability` = Fig. 2,
`Fig_CitationSource` = Fig. 3, `FigB_LOJO` = Fig. 4,
`FigB_PlaceboFastStarter` = Fig. 5, `FigB_TopicHeterogeneity` = Fig. 6,
`FigB_CohortElasticity` = Fig. 7, `FigE_PlaceboConference` = Fig. 8;
`TableA1_Attrition` = Table 5, `TableA2_TopicDictionary` = Table 6,
`TableC*` = Table 7, `Table4_WithinAuthor` = Table 8.

## Conference linkage in the published version

The indicator "presented at EHA or EHS" is built in `05_conference.R` from two
shipped files, not from a matching algorithm run at replication time:

1. `data/raw/conference_programme_records.csv`: one row per programme entry
   (EHA 2006-2025 from the hand-transcribed workbook; EHS 2003-2019 and
   2021-2022 from a re-extraction of the archived HTML and PDF programmes
   that separates title and author fields and normalises case and accents).
   For each entry, the best corpus article with an overlapping author surname
   and a Jaro title distance below 0.25, published from one year before to
   five years after the programme year, is recorded as a candidate
   (`matched_id`, `dist`, `tier`, `overlap`, `article_title`,
   `article_authors`, `publication_year`, `in_estimation`); 317 entries have a
   candidate.
2. `data/raw/conference_match_ledger.csv`: the title-and-author review of
   those 317 candidates (`decision` = retain / reject / uncertain, with a
   `reason`). 278 are retained, 20 rejected (incompatible topics or authors,
   prize announcements), 19 uncertain and excluded. Decisions used programme
   and article titles and named authors only, never citation outcomes.

Script 05 keeps only retained links, drops the EHS 2023-2024 archive summary
pages (prize announcements, not paper sessions) and the unverified EHA 2025
records, and aggregates to the article level: 259 distinct articles (81 via
EHA, 195 via EHS, 17 at both), 137 of them in the estimation sample. A zero
means no retained link in these sources, not evidence that the paper was never
presented; neither the extraction nor the linkage is exhaustive.

The scripts that produced the re-extraction, the candidate nominations and the
metadata screen are in `scripts/provenance/` for inspection. They ran inside
the audit tree and need the raw programme files, which are not redistributed
(URLs below), so they are not part of `run_all.R`.

## Data provenance and licences

- **Google Scholar citation snapshots** (`Journals_2026_clean.csv`, columns
  `Google14`-`Google26`): cumulative citation counts collected by hand by
  research assistants at LEAP (Stellenbosch University) in **February-March of
  each year from 2014 to 2026**. Each column holds the cumulative count
  observed in that year's snapshot; a zero means no citations were recorded at
  that date.
- **OpenAlex-derived files** (`openalex_paper_matches.rds`,
  `openalex_metadata_screen.csv`, `network_citation_data.rds`,
  `network_paper_metrics.rds`, `citing_field_data.rds`,
  `citing_field_linked.rds`): built from the OpenAlex API
  (https://openalex.org, data released under CC0) in 2026. OpenAlex contents
  change over time; re-running `00_data_collection.R` will not reproduce
  these files exactly, which is why they are shipped. The paper-to-work
  assignment is automated (title search with year verification); the screen
  file records a title/year re-verification of every assignment made in
  September 2026 and flags 222 records. The frozen citation-network cache
  covers 1,624 corpus articles published 1997-2018 and contains no links to
  articles published from 2019 onwards; after the screen, 72,465 links to
  1,610 articles remain. Assignment rates are not validated correct-link
  rates.
- **RePEc-derived file** (`repec_author_data.rds`): aggregate author-level
  statistics (first publication year, h-index, NBER working-paper indicator)
  retrieved from the RePEc API in 2026.
- **Conference programmes** (`conference_programme_records.csv`,
  `conference_parsed_data.rds`, `Conference_Papers.xlsx`): bibliographic
  facts (titles, authors, sessions) from publicly posted EHA programmes
  (https://eh.net, hand-transcribed 2006-2025 with session times) and EHS
  programmes
  (https://ehs.org.uk/society/resources/ehs-annual-conference-archive/; the
  2021 programme PDF and the 2022 provisional programme page were recovered
  separately; `source_file`, `source_paragraph` and `source_url` in the
  records file point to the origin of each entry). The raw programme files
  are not redistributed. The EHES biennial meeting is not covered (see the
  paper, Section 3.3, for the scope statement).
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

- The conference indicator covers recovered EHA and EHS programme records,
  not conference exposure in general, and remains subject to incomplete
  coverage and linkage error. The conditional cross-sectional association is
  imprecise (0.066, robust SE 0.043, 137 presenters); the stricter
  first-author-by-publication-year panel estimate is positive but rests on
  25 groups with variation. The paper reports these as neither establishing
  nor excluding a conference effect.
- The top-institution indicator is a substring match on affiliation strings
  and produces some false positives ("Smith" matches "MIT"; "Oxford Brookes"
  matches "Oxford"). The main estimate is unchanged when it is dropped.
- Google Scholar counts occasionally decline between snapshots; the growth
  outcome clamps negative growth at zero before taking logs (documented in
  `01_build_sample.R`).

## Citation

If you use these data, please cite the article above and, for the citation
links, OpenAlex (Priem, Piwowar and Orr 2022, arXiv:2205.01833).

Contact: Johan Fourie, Stellenbosch University (johanf@sun.ac.za).

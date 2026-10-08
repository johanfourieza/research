# CODEBOOK

Variable-level documentation for the data files in this replication package.
See README.md for provenance and licences.

## 1. data/raw/Journals_2026_clean.csv

One row per article. 3,250 rows. Coverage: the four core generalist economic
history journals only, namely the Journal of Economic History (JEH), the
Economic History Review (EHR), Explorations in Economic History (EEH, coded
"Explorations") and the European Review of Economic History (EREH). Articles
from other journals are not included in this dataset. Publication years
1997-2025. The file is the cleaned version of a hand-coded database maintained
at LEAP, Stellenbosch University, since 2013.

| Column | Description |
|---|---|
| `ID` | Unique article identifier (integer, 1..N; reassigned during cleaning) |
| `Journal` | Journal short name: `JEH`, `EHR`, `Explorations`, `EREH`, ... |
| `Year` | Publication year |
| `Vol`, `No` | Volume and issue |
| `Paper_ID` | Sequence number of the article within its journal-volume-issue |
| `Title`, `TitleCaps` | Article title (mixed case / uppercase) |
| `Divergence`, `Depression` | Hand-coded indicator flags for two recurring themes (Great Divergence; Great Depression); not used in the paper |
| `Comments` | Free-text coding notes |
| `Google14` ... `Google26` | Cumulative Google Scholar citation count in the snapshot of calendar year 2014 ... 2026. Snapshots were collected by hand in February-March of each year. A zero means no citations recorded at that date |
| `WebScience15` | Web of Science citation count, 2015 snapshot (not used) |
| `Pagestart`, `Pageend` | First and last page |
| `Characters` | Character count of the title (legacy; the analysis recomputes it) |
| `Continent1` | Hand-coded geographic focus of the article (e.g. "Western Europe", "North America", "Global") |
| `No of authors` | Number of authors |
| `Author1` ... `Author 11` | Author full names, in byline order |
| `Author1 university` ... | Affiliation of each author as recorded at publication |
| `Author1 country` ... | Country of each affiliation |
| `ID_original` | The article's identifier before the cleaning pass |
| `duplicate_entry` | Flag set during cleaning for suspected duplicate titles (2 rows; verified distinct articles) |

## 2. Derived variables (constructed in scripts/01_build_sample.R)

| Variable | Definition |
|---|---|
| `cite_age_k` (k = 1,2,3,5,8) | Cumulative citations at age k = the `Google(Year+k)` snapshot. Age is publication year to snapshot year; because snapshots are taken in February-March, age k corresponds to roughly k years of exposure for an article published early in the year and k-1 for one published late |
| `cite_early` | `cite_age_2` (the age-2 citation count) |
| `cite_longrun` | `cite_age_8` if observable, otherwise `cite_age_5` |
| `cite_growth` | `cite_longrun - cite_early` (citations accrued strictly after age 2) |
| `log_early`, `log_longrun` | log(1 + count) |
| `log_growth` | log(1 + max(growth, 0)); negative growth (snapshot noise) clamped at zero |
| `is_core` | Article in JEH, EHR, Explorations (EEH) or EREH |
| `fast_starter` | 1 if `cite_early` is above the 75th percentile of its journal-year cohort (percentile computed with average ranks) |
| `fast_starter_strict` | Same with the 90th percentile |
| `topic` | One of 16 keyword-defined topics, assigned by counting keyword matches in the lower-cased title and taking the topic with the most matches; `other` if no keyword matches. Full dictionary in output/tables/TableA2_TopicDictionary.tex and the paper's appendix |
| `any_top_inst` | 1 if any of the first five authors' affiliation strings contains one of: Harvard, MIT, Stanford, Berkeley, Yale, Princeton, Chicago, Northwestern, Columbia, Penn, UCLA, Michigan, NYU, Oxford, Cambridge, the London School of Economics (matched by both the full name and the abbreviation "LSE"), Warwick (case-insensitive substring match). These are seventeen distinct institutions ("LSE" and "London School of Economics" are the same place, kept as two spellings; the OR indicator does not double-count) |
| `region` | Continent1 grouped into Africa / Europe / Americas / Asia & Oceania / Global |
| `article_length` | Pageend - Pagestart + 1 (set missing if <= 0 or > 200) |
| `article_position` | Rank of the article's first page within its journal-year-volume-issue (1 = first article) |
| `issue_no` | Numeric issue number |
| `team_max_seniority` | Publication year minus the earliest RePEc first-publication year across the first five authors (matched by cleaned name) |
| `team_max_hindex` | Maximum RePEc h-index across the first five authors |
| `author_nber_wp` | 1 if any matched author has an NBER working paper on RePEc |
| `paper_won_prize` | 1 if the article was matched to a Cole, Ashton or Figuerola prize |
| `author_won_dissertation_prize` | 1 if any author won the Gerschenkron or Nevins dissertation prize before the article's publication year |
| `presented_at_conference` | 1 if the article has a retained link in `data/raw/conference_match_ledger.csv` to an EHA or EHS programme entry (script 05; see section 3b below). EHS 2023-2024 summary pages and EHA 2025 entries are excluded before aggregation |
| `presented_at_eha`, `presented_at_ehs`, `n_conference_presentations` | conference identity and number of retained programme links per article |
| `eha_session_order`, `eha_pre_lunch`, `eha_post_lunch` | earliest EHA session order and the pre-/post-lunch coding from the EHA begin times (EHA links only) |
| `author_conf_exposure` | 1 if any author surname appears among EHA/EHS presenters in the publication year or the year before |

Missing control values (`log_article_length`, `title_nchar`,
`article_position`, `issue_no`) are imputed with the full-sample median.

**Estimation sample** (N = 1,262): `is_core`, age-2 AND age-5/8 citations
observable (publication years 2012-2021 given the 2014-2026 snapshots),
non-missing author count, non-negative citation counts. The step-by-step
attrition is in output/tables/TableA1_Attrition.tex.

## 3. data/raw/Conference_Papers.xlsx

Sheet **Conferences** (hand-transcribed EHA annual-meeting programmes,
2006-2025): `Type` (conference), `Year`, `City`, `Title`, `Authors` (count),
`Author1`-`Author10`, `Institution...` columns (affiliations), `Begin time` /
`End time` (HHMM integers, e.g. 1330). Read by
`read_eha_conferences()` in `scripts/conference_data_helpers.R`, which derives
session order and the pre/post-lunch flags from the begin times.

Sheet **Prizes**: `Prize` (Gerschenkron / Nevins), `Year`, `Author1`
(recipient), `Title` (dissertation title). Read by
`read_dissertation_prizes()`.

## 3b. data/raw/conference_programme_records.csv

One row per EHA or EHS programme entry from the corrected re-extraction
(3,565 rows: EHA 2006-2025 from the hand-transcribed workbook; EHS 2003-2019,
2021 and 2022 from the archived HTML programmes, the 2021 programme PDF and the
2022 provisional programme page; EHS 2020 was cancelled). Built by the scripts
in `scripts/provenance/` (`audit_conference.R`, `recover_missing_years.R`,
`extend_conference.R`). Candidate links were nominated for entries with an
author surname overlapping a corpus article and a normalised Jaro title
distance below 0.25, with the article published from one year before to five
years after the programme year.

| Column | Description |
|---|---|
| `row_id` | Entry identifier (key to the ledger) |
| `conference`, `year` | `EHA` or `EHS`; programme year |
| `title`, `authors`, `affiliations`, `author_count` | Programme entry as extracted |
| `matched_id` | Candidate corpus article `ID` (NA if no candidate; 317 entries have one). Script 05 keeps it only where the ledger decision is `retain` |
| `dist` | Normalised Jaro distance between the cleaned programme and article titles (0 = identical) |
| `tier` | Nomination tier: `A` (distance below 0.10) or `B` (below 0.25 with surname overlap) |
| `overlap` | TRUE if a programme author surname matches an article author surname |
| `article_title`, `article_authors`, `publication_year`, `in_estimation` | The candidate article's title, authors, year and estimation-sample membership |
| `day`, `time`, `session`, `session_order`, `city`, `begin_time`, `end_time`, `begin_dec`, `pre_lunch`, `post_lunch` | EHA session details from the workbook (NA for EHS) |
| `conf_title`, `conf_authors`, `conf_year` | Copies of title, authors, year as used by the matcher |
| `raw_text` | The raw programme paragraph (EHS re-extraction) |
| `source_file`, `source_paragraph`, `source_url` | Provenance of each EHS entry (file, paragraph index, archive URL) |

## 3c. data/raw/conference_match_ledger.csv

One row per candidate link (317 rows), with all the columns of the records
file plus the review:

| Column | Description |
|---|---|
| `decision` | `retain` (278), `reject` (20) or `uncertain` (19). Only `retain` defines a presentation |
| `reason` | Basis for the decision (same normalised title; consistent titles and compatible authors; or the specific reason for rejection/uncertainty) |
| `review_basis` | Statement that the review used programme and article titles and named authors only, with no inference from citation outcomes |

Script 05 writes the records actually used after the coverage exclusions, with
the reviewed `matched_id`, to `results/conference_programme_records_used.csv`.

## 4. data/cache/ (API-derived; shipped for offline reproduction)

| File | Unit | Key columns |
|---|---|---|
| `openalex_paper_matches.rds` | one row per matched article; `01_build_sample.R` restricts these to the corpus, where 3,241 of the 3,250 core articles were assigned an OpenAlex identifier (99.7%). The cache was built on a wider hand-coded ID range, so it also carries matches for articles outside the four core journals; those are filtered out on load. Assignment is automated and the rate is not a validated correct-link rate | `id` (article ID), `openalex_id`, `oa_cited_by_count` (OpenAlex citation count at download), `oa_year` |
| `openalex_metadata_screen.csv` | one row per assigned article (3,241): the title, year, DOI, authors and journal that OpenAlex returned for the assigned work in September 2026, compared with the hand-coded record. `flag` is TRUE for 222 records with missing returned metadata, `title_distance` (normalised Jaro) above 0.15 or a publication-year discrepancy above one year. Scripts 06 and 10 drop citation links to flagged source articles (230 links; 72,465 links to 1,610 articles remain) | `id`, `openalex_id`, `oa_cited_by_count`, `oa_year`, `verified_title`, `verified_year`, `doi`, `verified_authors`, `verified_journal`, `year`, `journal`, `title`, `author1`, `title_distance`, `flag` |
| `network_citation_data.rds` | one row per citation link (72,695, to 1,624 corpus articles published 1997-2018; no links to articles published from 2019 onwards) | `cited_id` (our article ID), `cited_oa_id`, `citing_oa_id`, `citing_year`, `citing_top_concept`, further citing-work metadata |
| `network_paper_metrics.rds` | one row per article with network metrics | `id`, `pagerank`, `cite_concentration` (Herfindahl of citations across years), further centrality measures |
| `citing_field_data.rds` | one row per unique citing work (37,853) | `citing_oa_id`, `type` (article / book-chapter / preprint / ...), `pt_field` / `pt_subfield` / `pt_domain` (OpenAlex primary-topic taxonomy), `l0_concepts` (semicolon-separated level-0 concept names), `venue` |
| `citing_field_linked.rds` | link-level merge of the two files above | |
| `repec_author_data.rds` | one row per matched author | `author_name`, `first_pub_year`, `hindex`, `has_nber_wp` |
| `conference_parsed_data.rds` | one row per programme entry from the first-pass parse (3,627: EHA 1,006 + EHS 2,621). Not used by the pipeline, which reads `data/raw/conference_programme_records.csv`; retained as the output of `00c_rebuild_conference_cache.R` | `conference`, `year`, `title`, `authors`, `affiliations`, `session_order`, `pre_lunch`, `post_lunch`, `begin_time` (EHA only) |
| `prize_paper_data.rds` | one row per paper-prize award | `prize_name`, `paper_title`, `prize_year`, `matched_id` |
| `prize_dissertation_data.rds` | one row per dissertation-prize award | `prize_name`, `recipient`, `recipient_clean`, `prize_year` |

## 5. results/ objects (created by the pipeline)

`analysis_data.rds` (list: `jn` all core-journal articles, `est` estimation sample,
`topic_dict`, `top_inst`, `attrition`, `oa_validation`),
`conference_flags.rds` (id-keyed conference indicators),
`conference_programme_records_used.csv` (programme records after the coverage
exclusions, with the reviewed `matched_id`), `mech_data.rds`
(estimation sample with mechanism variables), and one compact `res_XX_*.rds`
per analysis script containing the coefficients, standard errors and sample
sizes reported in the paper. `res_05_conference.rds` also holds the ledger
counts, the unconditional association, both permutation tails and the
sensitivity table; `res_06_mechanisms.rds$network` holds the screened-network
coverage counts quoted in Appendix A.3; `res_09_panel.rds` holds the number of
first-author-by-publication-year groups with variation in conference status.
`scripts/11_check_published_numbers.R` reads these objects and compares them
with the published article.

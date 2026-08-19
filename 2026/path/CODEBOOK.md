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
| `presented_at_conference` | 1 if the article was matched (author-validated fuzzy title match, script 05) to an EHA or EHS programme entry |
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

## 4. data/cache/ (API-derived; shipped for offline reproduction)

| File | Unit | Key columns |
|---|---|---|
| `openalex_paper_matches.rds` | one row per matched article; `01_build_sample.R` restricts these to the corpus, where 3,241 of the 3,250 core articles match (99.7%). The cache was built on a wider hand-coded ID range, so it also carries matches for articles outside the four core journals; those are filtered out on load | `id` (article ID), `openalex_id`, `oa_cited_by_count` (OpenAlex citation count at download), `oa_year` |
| `network_citation_data.rds` | one row per citation link (72,695) | `cited_id` (our article ID), `cited_oa_id`, `citing_oa_id`, `citing_year`, `citing_top_concept`, further citing-work metadata |
| `network_paper_metrics.rds` | one row per article with network metrics | `id`, `pagerank`, `cite_concentration` (Herfindahl of citations across years), further centrality measures |
| `citing_field_data.rds` | one row per unique citing work (37,853) | `citing_oa_id`, `type` (article / book-chapter / preprint / ...), `pt_field` / `pt_subfield` / `pt_domain` (OpenAlex primary-topic taxonomy), `l0_concepts` (semicolon-separated level-0 concept names), `venue` |
| `citing_field_linked.rds` | link-level merge of the two files above | |
| `repec_author_data.rds` | one row per matched author | `author_name`, `first_pub_year`, `hindex`, `has_nber_wp` |
| `conference_parsed_data.rds` | one row per programme entry (3,627: EHA 1,006 + EHS 2,621) | `conference`, `year`, `title`, `authors`, `affiliations`, `session_order`, `pre_lunch`, `post_lunch`, `begin_time` (EHA only) |
| `prize_paper_data.rds` | one row per paper-prize award | `prize_name`, `paper_title`, `prize_year`, `matched_id` |
| `prize_dissertation_data.rds` | one row per dissertation-prize award | `prize_name`, `recipient`, `recipient_clean`, `prize_year` |

## 5. results/ objects (created by the pipeline)

`analysis_data.rds` (list: `jn` all core-journal articles, `est` estimation sample,
`topic_dict`, `top_inst`, `attrition`, `oa_validation`),
`conference_flags.rds` (id-keyed conference indicators), `mech_data.rds`
(estimation sample with mechanism variables), and one compact `res_XX_*.rds`
per analysis script containing the coefficients, standard errors and sample
sizes reported in the paper.

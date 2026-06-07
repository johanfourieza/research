# Codebook — 2026_path

Variable definitions for the curated datasets in this package. The analysis in
`scripts/01_analysis.R` derives further working variables (e.g. age-specific
citations, fast-starter status, article length) from these raw columns.

## `data/Journals_2026_clean.csv`

One row per article. 3,748 rows.

### Identifiers and bibliographic fields

| Column | Description |
|---|---|
| `ID` | Unique article identifier (used to link to other objects). |
| `Journal` | Journal abbreviation: `JEH` (Journal of Economic History), `EHR` (Economic History Review), `EREH` (European Review of Economic History), `Explorations` (Explorations in Economic History), `Cliometrica`, `EHDR` (Economic History of Developing Regions). The paper's analysis uses the first four. |
| `Year` | Publication year. |
| `Vol`, `No` | Volume and issue number. |
| `Paper_ID` | Within-source running paper identifier. |
| `Title`, `TitleCaps` | Article title (and an upper-cased copy). |
| `Pagestart`, `Pageend` | First and last page. The analysis derives article length as `Pageend − Pagestart + 1`. |
| `Characters` | Hand-recorded character-length field (not used in the analysis; the length control is derived from page numbers and from `nchar(Title)`). |
| `Comments` | Free-text coder notes. |
| `ID_original`, `duplicate_entry` | Provenance: original id before de-duplication, and a duplicate flag. |

### Citation snapshots

| Column | Description |
|---|---|
| `Google14` … `Google26` | Cumulative Google Scholar citation count for the article as observed in calendar years 2014 … 2026, respectively. Age-specific citations (e.g. age-two "early" and age-five/eight "long-run") are constructed by aligning each paper's `Year` with these snapshot years. |
| `WebScience15` | Cumulative Web of Science citation count observed in 2015 (Clarivate). |

### Authors and institutions

| Column | Description |
|---|---|
| `No of authors` | Number of authors. |
| `Author1` … `Author11` | Author names (up to 11; later authors appear under slightly varied headers such as `Author 6`). |
| `Author1 university` … | Author affiliation (institution) for each author. |
| `Author1 country` … | Author country for each author. |
| `Continent1` | Continent of the first author's institution. |

### Hand-coded topic flags

| Column | Description |
|---|---|
| `Divergence`, `Depression` | Legacy hand-coded topic indicators (e.g. whether the article concerns the "Great Divergence" / the Great Depression). The paper's topic classification is instead built by keyword matching on titles into 13 broad categories. |

## `data/Conference_Papers.xlsx` (EHA, raw)

Hand-transcribed Economic History Association programmes, 2006–2025. Sheet
columns include the conference and year, session label and order, exact begin
and end times, presentation title, and presenter name(s). This is the raw input
for the EHA portion of `conference_parsed_data`.

## `data/conference_parsed_data.{csv,rds}` (EHA + EHS, cleaned)

One row per presentation; 3,627 rows (`EHA` 1,006, `EHS` 2,621). Key columns:

| Column | Description |
|---|---|
| `conference` | `EHA` or `EHS`. |
| `year` | Meeting year. |
| `day`, `time`, `session`, `session_order` | Programme placement: day, time block, session label, and order within session. |
| `title`, `authors`, `affiliations`, `author_count` | Presentation title, presenter name(s), affiliation(s), and number of presenters. |
| `begin_time`, `end_time`, `begin_dec`, `hour`, `time_slot`, `pre_lunch`, `post_lunch` | Session timing (EHA only, from the hand-coded begin/end times); used for the session-timing analysis. |
| `conf_title`, `conf_authors`, `conf_title_clean`, `conf_author_clean` | Normalised title/author fields used for fuzzy matching to the journal dataset. |
| `matched_id` | `ID` of the matched journal article, where a confident title/author match exists (otherwise missing). |
| `city` | Host city (where recorded). |

# Changelog

## 2.0 — published version (October 2026)

Matches the article as published in *Cliometrica*
(https://doi.org/10.1007/s11698-026-00346-w; accepted 20 August 2026, proofs
corrected September–October 2026). A replication audit carried out during
proof correction found implementation errors in the automated conference
matching of the accepted version and inaccuracies in the description of two
auxiliary datasets. The main persistence results were unaffected and
reproduce to the last decimal. This release changes the package as follows.

**Conference linkage (Section 3.3, 6.3, Appendices D and E; Table 8; Fig. 8)**

- `scripts/05_conference.R` no longer runs a fuzzy title matcher. It reads
  `data/raw/conference_programme_records.csv` (every EHA and EHS programme
  entry from a corrected re-extraction, including the recovered EHS 2021 and
  2022 programmes, with the candidate article nominated by author-surname
  overlap and Jaro title distance below 0.25) and
  `data/raw/conference_match_ledger.csv` (the title-and-author review of all
  317 candidates: 278 retained, 20 rejected, 19 uncertain). Only retained
  links define the indicator. EHS 2023–2024 archive summary pages and the
  unverified EHA 2025 records are excluded.
- Result: 137 estimation-sample presenters (was 85); conditional coefficient
  0.066 (robust SE 0.043), one-sided permutation p = 0.075, two-sided
  p = 0.144 (was 0.070, SE 0.056, p = 0.16 one-sided, unlabelled); within
  first-author panel 0.051 (0.027) and 0.332 (0.133) with 25 varying groups
  (was 0.017 and 0.545 with 13). The script now also reports the association
  without the early-citation control and a sensitivity table
  (`output/tables/TableE1_ConferenceSensitivity.csv`).
- The permutation p-values use the finite-repetition correction
  (1 + exceedances)/(1 + 1000) and the one-sided tail is labelled.
- `scripts/09_within_author.R`: Table 8 row labels are now
  "First-author FE" and "First-author-by-publication-year FE" (the fixed
  effects are on the first-listed author, as they always were); the dependent
  variable label is "Log(1 + new citations)".
- The accepted version's matcher is kept, unused, as
  `scripts/archive/05_conference_automated_accepted_2026-08.R`. Its known
  errors: capital letters were stripped before lower-casing titles, surname
  lists were nested, EHS author fields were malformed, and the EHS 2021–2022
  programmes were missing. `data/cache/conference_parsed_data.rds` (its
  input) is retained for that script only.

**OpenAlex citation network (Section 6.2, Appendix A.3; Fig. 3)**

- `data/cache/openalex_metadata_screen.csv` records, for each of the 3,241
  automated OpenAlex assignments, the title and year returned by the API and a
  flag for 222 records with missing metadata, a normalised Jaro title
  distance above 0.15, or a publication-year discrepancy above one year.
- `scripts/06_mechanisms.R` (citation-source decomposition) and
  `scripts/10_figures.R` (Fig. 3) exclude links to flagged source articles:
  72,465 links to 1,610 corpus articles published 1997–2018 remain (230 links
  to 14 articles removed). The classified estimation subsample is 593
  articles (was 597) and the within/cross-field elasticities are 0.70/0.51
  (were 0.69/0.50). Rounded shares in Fig. 3 are unchanged. The network cache
  contains no links to articles published from 2019 onwards; the paper now
  says so.
- The self-citation, cascade and concentration estimates of Section 6.4 use
  the unscreened within-corpus network, as published, and are unchanged.

**Other**

- `scripts/01_build_sample.R` reads the article CSV with an explicit UTF-8
  encoding, so title-length controls no longer depend on the session locale.
- `scripts/11_check_published_numbers.R` (run last by `run_all.R`) compares
  the regenerated results with every statistic reported in the published
  article and stops with an error on any discrepancy.
- `scripts/provenance/` holds the audit scripts that produced the shipped
  programme records, candidate links and metadata screen. They are not run by
  `run_all.R` and need the raw programme files, which are not redistributed.
- README and CODEBOOK revised accordingly; the paper citation now carries the
  DOI.

The accepted-version package is preserved at commit
`49e0afc1b4d431c5a2d1a719974547ce1ea2abee` (tag `2026-path-accepted-2026-08`).

## 1.1 — 19 August 2026

Fixed an inconsistent R² in Table 3 Panel A (author-quality row). Second-round
resubmission to *Cliometrica*.

## 1.0 — 8 July 2026

First public release with the first-round revision.

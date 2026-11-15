# AI and entry-level knowledge work: Evidence from economics research assistants

Replication data and code for Fourie (2026), submitted to *Economics Letters*.

## Overview

Payroll and résumé studies find lower entry-level hiring in occupations exposed
to AI, but they do not observe the junior workers credited in finished work.
Economics working papers record them, because research assistants are thanked
in the acknowledgements. The letter links those acknowledgements to two
indicators of AI involvement: an explicit statement that the authors used
generative AI, and prose that a detector flags as AI-like.

The frame is **56,585 paper records in 24 economics working-paper series**,
January 2015 to October 2026. The regressions compare papers by the same
economist issued from January 2023: **8,502 papers by 5,285 researchers**, of
which 544 disclose AI use. Within an economist's portfolio, disclosing papers
credit 0.548 more named assistants and are 5.35 pages longer. Both differences
appear in disclosures of coding, data and analysis ('operational'), and neither
appears in disclosures of writing help alone or in papers with AI-like prose.

## What this release contains

The release contains **the derived variables needed to reproduce Table 1,
Figures 1 and 2, and the numbers in the text of the letter**, and the code that
produces them. It does not contain the working papers, their text, or the
acknowledgement and disclosure passages. The papers are available from the
institutions that issue them and from arXiv.

| File | Rows | Description |
|---|---|---|
| `data/papers.csv` | 8,502 | One row per paper in the regressions: outcomes, the two AI indicators, the disclosure's task label, controls and fixed-effect groups. |
| `data/researcher_links.csv` | 143,295 | One row per outcome, paper and linked researcher, with the regression weight. This file defines each outcome's sample. |
| `data/half_year_shares.csv` | 80 | Share of papers crediting any assistant, and share disclosing AI use, by half-year, 2022 to 2026 (Figure 2). |
| `data/registered_tests.csv` | 20 | The 20 registered tests (two indicators, ten outcomes) with raw, Holm and Romano–Wolf p-values. |
| `data/series_frame.csv` | 24 | The 24 series and their record counts. |

Variables are defined in [`CODEBOOK.md`](CODEBOOK.md).

**No names are included, but papers keep their public identifiers.** The files
hold no author names, no assistant names and no text from any paper. Each
researcher is an anonymous code (`r0001` to `r5285`) that cannot be linked to a
name without the original identifiers, which are not released. Each paper keeps
its public series identifier (for example `nber:nber:w32946`), so a reader can
look up the paper, and therefore its authors. A paper's indicators are
measurements from its text: a disclosure records a statement the authors
printed, and the writing flag records resemblance to AI-generated prose, not
verified AI use.

## Reproducing the results

```
Rscript run_all.R
```

`run_all.R` runs scripts 01 to 04 and writes `results/session_info.txt`.
`04_check_numbers.R` stops with an error if any of 64 numbers differs from the
letter. Runtime is under a minute. Requires R 4.3 or later with `dplyr`,
`readr`, `fixest` and `ggplot2`.

| Script | Produces |
|---|---|
| `scripts/00_functions.R` | Functions shared by the other scripts: data loading, the regression of Eq. (3), the equality test. |
| `scripts/01_table1.R` | `results/table1_estimates.csv` (every coefficient in Table 1), `table1_samples.csv` (sample sizes), `group_means.csv` (outcome means by disclosure group). |
| `scripts/02_page_split.R` | `results/page_split_estimates.csv`: main-text and appendix pages on the 8,493 papers with verified page text. |
| `scripts/03_figures.R` | `results/Figure_1.{pdf,png}` and `results/Figure_2.{pdf,png}`. |
| `scripts/04_check_numbers.R` | A check of the numbers in the letter. |
| `scripts/05_romano_wolf.R` | `results/romano_wolf.csv`: Romano–Wolf step-down p-values from 1,999 wild cluster bootstrap draws. Optional and slower; run it separately. It stops with an error if a p-value differs from the stored one. |
| `scripts/05a_bootstrap_outcome.R` | The bootstrap for one outcome; called by `05_romano_wolf.R`. |

### The regression

Each paper enters once for each linked researcher who has at least two papers
in the sample, with weight one divided by the number of such researchers, so
every paper has total weight one. The model has researcher, source-quarter and
field fixed effects, the writing flag and the disclosure indicator entered
together, and two controls for the length of the text the detector scored.
Standard errors are clustered by researcher and by paper. Columns 3 to 5 of
Table 1 replace the disclosure indicator with indicators for operational,
writing-only and unlabelled disclosures.

## What this release does not reproduce

- **The construction of the variables.** Collecting the papers, extracting
  acknowledgements, classifying disclosures with a language model, scoring the
  prose and linking researchers are not part of this release, because those
  steps need the full text of the papers and author records.
- **Three descriptive statements made on a wider set of papers** than the
  regression sample: that about four in five assessable disclosures are
  operational, that about one in seven disclosing papers has the writing flag,
  and the half-year shares in `data/half_year_shares.csv`, which are supplied
  as computed.
- **The sensitivity analyses** summarised in the last paragraphs of Section 4
  and reported in the Supplementary Appendix.

## How the variables were constructed

- **Disclosure (`disclosed`).** An explicit statement in the paper that the
  authors used generative AI in producing it. Candidate passages were found by
  full-text search for tool and model terms and classified in context by a
  language model; every positive record was reviewed a second time. The measure
  has no independent human validation. Absence of a statement does not
  establish non-use.
- **Task label (`task_label`).** A disclosure is *operational or mixed* when
  any passage reports coding, data preparation, search, analysis or derivation,
  and *writing-only* when every reported task is drafting, editing, translation
  or typesetting. Disclosures whose tasks are not stated are *unlabelled*.
- **Writing flag (`marker`).** The strict rule of the *econ-ai-detector*
  (Goldsmith-Pinkham, 2026), applied to the introduction and one body section.
- **Assistants.** `any_ra` records whether the acknowledgements credit any
  research assistance; `ra_count` counts the assistants thanked by name.
- **Researchers.** Authors were linked across papers principally with OpenAlex
  author records. The release replaces each identifier with an anonymous code.

**Limits.** The coefficients are conditional associations across projects
within an economist's recent output. Disclosure is voluntary. Acknowledgements
record credited contributions to completed papers, not hours, wages or hiring.

## Citation

> Fourie, J. (2026). "AI and entry-level knowledge work: Evidence from economics
> research assistants." Stellenbosch University. Submitted to *Economics Letters*.

Detector: Goldsmith-Pinkham, P. (2026). *econ-ai-detector*. Software
repository, https://github.com/paulgp/econ-ai-detector.

## Principal investigator

**Johan Fourie**, Department of Economics, Stellenbosch University
(johanf@sun.ac.za)

## Funding

This research did not receive any specific grant from funding agencies in the
public, commercial, or not-for-profit sectors.

## License

CC BY 4.0. See `LICENSE`.

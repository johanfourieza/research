# Writing Is Not (Necessarily) Thinking

Replication data and code for the working-paper evidence in Section 8 and
Figure 1 of Fourie (2026), currently under revision at *Kyklos*.

## Overview

The paper argues that generative AI can raise the quality of the manuscript an
author produces today while lowering what the author can do unaided tomorrow,
and that the effect depends on which writing task is delegated. Its first
prediction is that cheaper drafting helps most those authors for whom writing
in English is most costly. Section 8 examines this prediction in economics
working papers.

The data cover **42,704 economics working papers issued between January 2015
and September 2026 in 32 series**: 31 series of university departments,
central banks and policy institutions in North America, Europe, Japan,
Australia, Chile and South Africa, and the economics collection of arXiv.
Before 2023 a detector of AI-like economics prose flagged fewer than one paper
in 200; by the first half of 2026 it flagged 15.6%. Among 6,939 papers issued
since January 2023, it flagged 8.3% of papers whose classified affiliations all
lie outside the six majority-English-speaking countries, against 5.0% of papers
whose affiliations all lie within them (2.8 percentage points within series and
half-year, 95% CI 1.5 to 4.1).

## What this release contains

This release contains **only the variables needed to reproduce Figure 1 and
every number in Section 8**:

1. `data/papers.csv`: one row per working paper, with its series, half-year of
   issue, the two prose indicators, disclosed AI use and, for papers issued
   since 2023, the affiliation group.
2. `data/affiliation_classification.csv`: the country codes the two
   classifiers assigned to each of the 1,838 distinct affiliations, and the
   final code. It contains no affiliation text.
3. The four scripts that produce every result, and their outputs in `results/`.

**The data are author-name-free but identifier-retaining.** No author names,
email addresses or affiliation strings are included. Each paper keeps its
public series identifier (for example `nber:nber:w30000`), so a reader can look
up the paper and therefore its authors.

The paper-level indicators were constructed in a separate project on research
teams in economics (Fourie, 2026, data release v14 of 11 October 2026). The
working papers themselves are available from the institutions that issue them
and from arXiv, and are not redistributed here.

## Reproducing the results

```
Rscript run_all.R
```

`run_all.R` runs `scripts/01_trends.R`, `02_groups.R` and `03_figure.R`, then
`04_check_numbers.R`, which stops with an error if any number reported in
Section 8 or Figure 1 is not reproduced. It writes `results/session_info.txt`.
Requires R (4.3 or later) with `dplyr`, `tidyr`, `fixest`, `ggplot2`,
`patchwork` and `svglite`. Runtime is under a minute.

| Script | Produces |
|---|---|
| `01_trends.R` | `results/trends.csv` (Figure 1, panel A) and `results/correlations.csv` (detector flag and disclosed use: r = 0.07 on 13,103 papers) |
| `02_groups.R` | `results/shares_by_group.csv`, `results/shares_by_group_year.csv` (panel B) and `results/within_series_estimates.csv` |
| `03_figure.R` | `results/figure1.{pdf,svg,png}` |
| `04_check_numbers.R` | a check of every Section 8 number |

## How the variables were constructed

- **Detector flag (`marker`).** The economics-prose detector of
  Goldsmith-Pinkham (2026), applied to selected passages from the introduction
  and one body section of each paper. The detector is deliberately simple. In
  its developer's comparison on 2026 NBER papers it recovered about a third of
  the papers that a commercial detector flagged, so it probably misses much
  AI-assisted writing. Its performance on second-language prose is not
  calibrated.
- **High colon rate (`colon_flag`).** Colons per 1,000 words of selected prose
  above a fixed threshold of 8.905, calibrated as the 2015–2021 mean plus two
  standard deviations in the original calibration sample. This is a cruder
  indicator of the same habit.
- **Disclosed AI use (`disclosed`).** A statement in the paper that the authors
  used AI.
- **Affiliation group (`group`).** For papers issued since January 2023, the
  institutions printed with each author's name were assigned to countries.
  Where an author's record matched known institutions, all of them were kept.
  Otherwise the printed affiliation was classified by the first institution it
  names. International organisations, affiliations that could not be
  identified, and the NBER and CEPR (research networks, not employers) were set
  aside. A paper is *English* if every classified author is affiliated only in
  the United States, the United Kingdom, Ireland, Canada, Australia or New
  Zealand; *Non-English* if every classified author is affiliated only
  elsewhere; and *Mixed* otherwise. Two independent classifiers (Claude and
  Codex) agreed on 1,811 of the 1,838 distinct affiliations (98.5%); the
  remaining 27 were adjudicated from the printed context. The script that
  builds the groups reads author-level files containing names and is not
  released.

**Limits.** The indicators measure resemblance to AI-generated prose, not AI
use. An affiliation country is a rough proxy for writing in a second language
and says little about an individual author's command of English. The sample of
classified papers covers only papers whose affiliations could be classified.

## Citation

> Fourie, J. (2026). "Writing Is Not (Necessarily) Thinking." Stellenbosch
> University. Under revision, *Kyklos*.

Detector: Goldsmith-Pinkham, P. (2026). *econ-ai-detector*. Software
repository, https://github.com/paulgp/econ-ai-detector.

## Principal investigator

**Johan Fourie**, Department of Economics, Stellenbosch University
(johanf@sun.ac.za)

## License

CC BY 4.0. See `LICENSE`.

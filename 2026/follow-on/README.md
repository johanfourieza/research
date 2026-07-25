# Do Cricket Captains Maximise Winning?

Replication and teaching materials for the paper of that title, by Johan
Fourie (LEAP, Stellenbosch University).

The data cover **22,113 first-class matches played between 1900 and 2025**: men's
Test cricket and the domestic first-class competitions of England, Australia,
New Zealand, South Africa, the West Indies, Pakistan and Ireland.

## What is here

| File | Contents |
|---|---|
| `followon_matches.csv` | The analysis data: one row per match. |
| `CODEBOOK.md` | Definition of every variable. |
| `load_data.R` | Loads the data and reproduces the headline estimates. |
| `LICENSE.txt` | Creative Commons Attribution 4.0. |

## The question

In first-class cricket, the side that bats first may make its opponent bat
again immediately, a choice called enforcing the follow-on, but only if its
first-innings lead reaches a margin set by the Laws of Cricket. Captains have
enforced less and less often over time: about 77 per cent of eligible captains
enforced in 1950-1979, against about 55 per cent since 2010. Does enforcing
actually help, and if it does, why has it fallen out of use?

## What the paper finds

1. **The conventional RD estimate favours enforcing.** It associates enforcing
   with a 24.0-point increase in winning and a 12.2-point fall in losing.
   Sorting below the cutoff matters: manipulation-robust bounds include zero,
   so the causal sign requires a continuity assumption that the observed
   sorting calls into question.
2. **The feared disaster is rare.** Sides that enforced lost 1.3 per cent of
   domestic matches and 0.9 per cent of Tests. Sides that were eligible and
   declined lost 9.1 and 1.1 per cent. These comparisons are descriptive.
3. **The behavioural response appears short-lived.** Enforcement falls by
   25.2 points in the first post-event year (randomisation p = 0.053), then
   returns towards its earlier level. The five-year average is -3.5 points
   (randomisation p = 0.498), so the first-year result is suggestive.

## Why this is a good teaching example

The statutory margin creates a sharp, legally binding discontinuity. Two
matches whose first-innings leads differ by one run are almost identical in
every respect, except that in one the captain has an option and in the other
he does not. This is a textbook fuzzy regression discontinuity design, with a
first stage that is visible to the naked eye: enforcement is absent just
below the line and about 55 per cent just above it.

It is also a good example because the design is not perfect, and the flaws are
instructive rather than fatal:

1. **Manipulation of the running variable.** The side batting second knows the
   margin and bats to stay below it, which cricket calls saving the follow-on.
   There is excess mass just below the cutoff and a density test rejects.
   Donut estimates remain positive, but formal manipulation bounds include
   zero. The paper therefore treats the causal interpretation as conditional.
2. **A discrete running variable.** Leads are whole numbers of runs, so there
   are many ties and conventional confidence intervals can be too narrow. The
   paper reports bias-aware honest intervals alongside the usual ones.
3. **A rule that changes.** The Test margin moved from 150 to 200 runs in 1980
   while the domestic margin stayed at 150. That gives a placebo test, and it
   is also a trap: the margin depends on the scheduled length of the match, not
   on whether it is a Test.

## A mistake worth knowing about

Only the side that **batted first** may enforce the follow-on. If the side
batting second scores more in the first innings, no follow-on is possible,
whatever the size of the gap. Identifying the leading side by whichever team
scored more, which is the natural thing to do, counts matches as eligible in
which no decision existed. It overstates the number of eligible matches by two
thirds and halves the apparent enforcement rate. The variable
`team_batting_first` in this file already handles it.

## Reproducing the results

```r
source("load_data.R")
```

This requires the `rdrobust` package. It walks through the first stage, the
naive comparison that gets the answer wrong, the regression discontinuity
estimate, the manipulation test and the donut check, in that order.

The full pipeline, including the behavioural event study, is in `scripts/` in
the project repository and runs end to end from `scripts/run_all.R`.

## Data source

The underlying scorecards come from the DAFT first-class match database,
compiled by Ric Finlay and Jim Palfreyman. This release contains only derived
variables and public scorecard facts, not the source database.

## Citation

Fourie, J. Do cricket captains maximise winning? Evidence from 125 years of
the follow-on rule. *Applied Economics Letters*.

Repository: https://github.com/johanfourieza/research/tree/main/2026/follow-on

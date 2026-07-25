# =============================================================================
#  09_release.R  —  Build the public teaching and replication package
#
#  This writes a self-contained folder that a student can download and use to
#  reproduce every number in the paper. It contains the DERIVED match-level
#  file only: constructed variables and public scorecard facts. No raw table
#  from the commercial source database is copied or redistributed.
#
#  OUTPUT
#     release/followon_matches.csv
#     release/CODEBOOK.md
#     release/README.md
#     release/LICENSE.txt
#     release/load_data.R
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)

REL <- file.path(PROJ, "release")
if (!dir.exists(REL)) dir.create(REL, recursive = TRUE)

d <- readRDS(file.path(DDIR, "matches.rds"))

# The released file is the analysis sample: the optional era, main competitions,
# competitive fixtures. We keep the variables needed to reproduce the paper.
rel <- d %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive) %>%
  transmute(
    match_id, competition = comp, season = yr,
    start_date, days_scheduled = days_for_match,
    team_batting_first = lead_team, team_batting_second = trail_team,
    captain_batting_first = lead_captain,
    runs_first_innings_1 = lead_runs, runs_first_innings_2 = trail_runs,
    lead, statutory_margin = margin, dist,
    eligible, enforced, near_miss,
    winner, result_type, is_draw,
    first_side_won = lead_win, first_side_lost = lead_loss,
    decisive_result = result_decisive, venue_country) %>%
  arrange(season, match_id)

write_csv(rel, file.path(REL, "followon_matches.csv"))
cat(sprintf("released %d matches, %d variables\n", nrow(rel), ncol(rel)))

# ---- codebook ---------------------------------------------------------------
codebook <- c(
"# Codebook: followon_matches.csv",
"",
sprintf("One row per first-class cricket match. %d matches, %s to %s.",
        nrow(rel), min(rel$season), max(rel$season)),
"",
"Every variable below is either a public scorecard fact or a variable we",
"constructed from one. See README.md for how the sample was built.",
"",
"| Variable | Type | Definition |",
"|---|---|---|",
"| `match_id` | text | Unique match identifier. |",
"| `competition` | text | Test, County, SheffieldShield, NZ-FC, RSA-FC, WI-FC, Pak-FC, Ire-FC, Zim-FC. |",
"| `season` | integer | Year in which the season began. |",
"| `start_date` | date | First scheduled day of the match. |",
"| `days_scheduled` | integer | Days the match was scheduled to last. Determines the statutory margin. 99 denotes a timeless match. |",
"| `team_batting_first` | text | The side that batted first. Only this side may enforce the follow-on. |",
"| `team_batting_second` | text | The side that batted second. |",
"| `captain_batting_first` | text | Captain of the side batting first, the person who took the decision. |",
"| `runs_first_innings_1` | integer | First-innings score of the side batting first. |",
"| `runs_first_innings_2` | integer | First-innings score of the side batting second. |",
"| `lead` | integer | `runs_first_innings_1` minus `runs_first_innings_2`. Negative if the side batting first fell behind. |",
"| `statutory_margin` | integer | Runs required to enforce the follow-on, from the Laws of Cricket. 150 for matches of 3+ days before 1980; from 1980, 200 for 5+ days and 150 for 3 or 4 days. |",
"| `dist` | integer | `lead` minus `statutory_margin`. The running variable. Eligibility begins at 0. |",
"| `eligible` | 0/1 | 1 if `lead >= statutory_margin`, so the captain had the option. |",
"| `enforced` | 0/1 | 1 if the follow-on was enforced. The treatment. |",
  "| `near_miss` | 0/1 | 1 if the follow-on was enforced, the opponent erased the first-innings deficit in its second innings and the enforcing side did not lose. |",
"| `winner` | text | Winning team, or missing for a draw. |",
"| `result_type` | text | How the match ended. |",
"| `is_draw` | logical | TRUE if the match ended without a result. |",
"| `first_side_won` | 0/1 | 1 if the side batting first won. Main outcome. |",
"| `first_side_lost` | 0/1 | 1 if the side batting first lost. |",
  "| `decisive_result` | 0/1 | 1 if the match did not end in a draw. |",
"| `venue_country` | text | Country in which the match was played. |",
"",
"## Notes for students",
"",
"1. **`eligible` is not a choice.** It is fixed by the Laws once the two first",
"   innings are complete. That is what makes the regression discontinuity design",
"   possible: `dist` crosses zero and the captain's option appears.",
"2. **`enforced` is a choice**, made only when `eligible == 1`. Enforcement is",
"   almost never recorded when `eligible == 0`, and where it is, the record is an",
"   error and the match has been removed.",
"3. **Only the side batting first can enforce.** If you rebuild this file from",
"   scorecards, do not define the leading side as whichever team scored more.",
"   That mistake inflates the number of eligible matches by about two thirds.",
"4. The running variable is a whole number of runs, so it has many ties. This",
"   matters for inference; see the appendix of the paper.")
writeLines(codebook, file.path(REL, "CODEBOOK.md"))

# ---- README -----------------------------------------------------------------
readme <- c(
"# Do Cricket Captains Maximise Winning?",
"",
"Replication and teaching materials for the paper of that title, by Johan",
"Fourie (LEAP, Stellenbosch University).",
"",
sprintf("The data cover **%s first-class matches played between %s and %s**: men's",
        format(nrow(rel), big.mark = ","), min(rel$season), max(rel$season)),
"Test cricket and the domestic first-class competitions of England, Australia,",
"New Zealand, South Africa, the West Indies, Pakistan and Ireland.",
"",
"## What is here",
"",
"| File | Contents |",
"|---|---|",
"| `followon_matches.csv` | The analysis data: one row per match. |",
"| `CODEBOOK.md` | Definition of every variable. |",
"| `load_data.R` | Loads the data and reproduces the headline estimates. |",
"| `LICENSE.txt` | Creative Commons Attribution 4.0. |",
"",
"## The question",
"",
"In first-class cricket, the side that bats first may make its opponent bat",
"again immediately, a choice called enforcing the follow-on, but only if its",
"first-innings lead reaches a margin set by the Laws of Cricket. Captains have",
"enforced less and less often over time: about 77 per cent of eligible captains",
  "enforced in 1950-1979, against about 55 per cent since 2010. Does enforcing",
"actually help, and if it does, why has it fallen out of use?",
"",
"## What the paper finds",
"",
  "1. **The conventional RD estimate favours enforcing.** It associates enforcing",
  "   with a 24.0-point increase in winning and a 12.2-point fall in losing.",
  "   Sorting below the cutoff matters: manipulation-robust bounds include zero,",
  "   so the causal sign requires a continuity assumption that the observed",
  "   sorting calls into question.",
"2. **The feared disaster is rare.** Sides that enforced lost 1.3 per cent of",
  "   domestic matches and 0.9 per cent of Tests. Sides that were eligible and",
  "   declined lost 9.1 and 1.1 per cent. These comparisons are descriptive.",
  "3. **The behavioural response appears short-lived.** Enforcement falls by",
  "   25.2 points in the first post-event year (randomisation p = 0.053), then",
  "   returns towards its earlier level. The five-year average is -3.5 points",
  "   (randomisation p = 0.498), so the first-year result is suggestive.",
"",
"## Why this is a good teaching example",
"",
"The statutory margin creates a sharp, legally binding discontinuity. Two",
"matches whose first-innings leads differ by one run are almost identical in",
"every respect, except that in one the captain has an option and in the other",
"he does not. This is a textbook fuzzy regression discontinuity design, with a",
  "first stage that is visible to the naked eye: enforcement is absent just",
  "below the line and about 55 per cent just above it.",
"",
"It is also a good example because the design is not perfect, and the flaws are",
"instructive rather than fatal:",
"",
"1. **Manipulation of the running variable.** The side batting second knows the",
"   margin and bats to stay below it, which cricket calls saving the follow-on.",
  "   There is excess mass just below the cutoff and a density test rejects.",
  "   Donut estimates remain positive, but formal manipulation bounds include",
  "   zero. The paper therefore treats the causal interpretation as conditional.",
"2. **A discrete running variable.** Leads are whole numbers of runs, so there",
"   are many ties and conventional confidence intervals can be too narrow. The",
"   paper reports bias-aware honest intervals alongside the usual ones.",
"3. **A rule that changes.** The Test margin moved from 150 to 200 runs in 1980",
"   while the domestic margin stayed at 150. That gives a placebo test, and it",
"   is also a trap: the margin depends on the scheduled length of the match, not",
"   on whether it is a Test.",
"",
"## A mistake worth knowing about",
"",
"Only the side that **batted first** may enforce the follow-on. If the side",
"batting second scores more in the first innings, no follow-on is possible,",
"whatever the size of the gap. Identifying the leading side by whichever team",
"scored more, which is the natural thing to do, counts matches as eligible in",
"which no decision existed. It overstates the number of eligible matches by two",
"thirds and halves the apparent enforcement rate. The variable",
"`team_batting_first` in this file already handles it.",
"",
"## Reproducing the results",
"",
"```r
source(\"load_data.R\")
```",
"",
"This requires the `rdrobust` package. It walks through the first stage, the",
"naive comparison that gets the answer wrong, the regression discontinuity",
"estimate, the manipulation test and the donut check, in that order.",
"",
"The full pipeline, including the behavioural event study, is in `scripts/` in",
"the project repository and runs end to end from `scripts/run_all.R`.",
"",
"## Data source",
"",
"The underlying scorecards come from the DAFT first-class match database,",
"compiled by Ric Finlay and Jim Palfreyman. This release contains only derived",
"variables and public scorecard facts, not the source database.",
"",
"## Citation",
"",
"Fourie, J. Do cricket captains maximise winning? Evidence from 125 years of",
"the follow-on rule. *Applied Economics Letters*.",
"",
"Repository: https://github.com/johanfourieza/research/tree/main/2026/follow-on")
writeLines(readme, file.path(REL, "README.md"))

# ---- licence ----------------------------------------------------------------
writeLines(c(
"Creative Commons Attribution 4.0 International (CC BY 4.0)",
"",
"You are free to share and adapt this material for any purpose, including",
"commercially, provided you give appropriate credit.",
"",
"Full text: https://creativecommons.org/licenses/by/4.0/legalcode"),
file.path(REL, "LICENSE.txt"))

# ---- loader / worked example ------------------------------------------------
loader <- c(
"# =============================================================================",
"#  load_data.R  —  Load the data and reproduce the headline estimates.",
"#  Run this first. It is written to be read line by line.",
"# =============================================================================",
"",
"library(readr)",
"args <- commandArgs(trailingOnly = FALSE)",
"file_arg <- grep(\"^--file=\", args, value = TRUE)",
"if (length(file_arg)) {",
"  script_dir <- dirname(normalizePath(sub(\"^--file=\", \"\", file_arg[1])))",
"} else {",
"  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)",
"  script_dir <- if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))",
"}",
"d <- read_csv(file.path(script_dir, \"followon_matches.csv\"), show_col_types = FALSE)",
"cat(\"matches:\", nrow(d), \"| eligible:\", sum(d$eligible),",
"    \"| enforced:\", sum(d$enforced), \"\\n\")",
"",
"# -----------------------------------------------------------------------------",
"#  STEP 1. Look at the rule working.",
"#  Enforcement should be near zero below the statutory margin and jump above it.",
"# -----------------------------------------------------------------------------",
"below <- d[d$dist >= -10 & d$dist < 0, ]",
"above <- d[d$dist >= 0 & d$dist <= 10, ]",
"cat(\"\\nenforcement rate 10 runs below the line:\", round(mean(below$enforced), 3), \"\\n\")",
"cat(  \"enforcement rate 10 runs above the line:\", round(mean(above$enforced), 3), \"\\n\")",
"",
"# -----------------------------------------------------------------------------",
"#  STEP 2. The naive comparison, which is WRONG.",
"#  Captains choose whether to enforce, so this compares different situations.",
"# -----------------------------------------------------------------------------",
"el <- d[d$eligible == 1, ]",
"cat(\"\\nNaive comparison among eligible matches:\\n\")",
"print(round(tapply(el$first_side_won, el$enforced, mean), 3))",
"",
"# -----------------------------------------------------------------------------",
"#  STEP 3. The regression discontinuity estimate, which is the right one.",
"#  Eligibility is used as an instrument for enforcement.",
"# -----------------------------------------------------------------------------",
"library(rdrobust)",
"",
"cat(\"\\nFIRST STAGE: does crossing the line change behaviour?\\n\")",
"summary(rdrobust(y = d$enforced, x = d$dist, c = 0, masspoints = \"adjust\"))",
"",
"cat(\"\\nFUZZY RD: the effect of enforcing on winning\\n\")",
"summary(rdrobust(y = d$first_side_won, x = d$dist, c = 0,",
"                 fuzzy = d$enforced, masspoints = \"adjust\"))",
"",
"cat(\"\\nFUZZY RD: the effect of enforcing on LOSING\\n\")",
"summary(rdrobust(y = d$first_side_lost, x = d$dist, c = 0,",
"                 fuzzy = d$enforced, masspoints = \"adjust\"))",
"",
"# -----------------------------------------------------------------------------",
"#  STEP 4. A check you should always run: is the running variable manipulated?",
"#  The side batting second bats to stay below the margin, so there is excess",
"#  mass just below zero. Compare the counts.",
"# -----------------------------------------------------------------------------",
"cat(\"\\nmatches within 3 runs below the line:\", sum(d$dist >= -3 & d$dist < 0), \"\\n\")",
"cat(  \"matches within 3 runs above the line:\", sum(d$dist >= 0 & d$dist < 3), \"\\n\")",
"bt <- binom.test(sum(d$dist >= 0 & d$dist < 3),",
"                 sum(d$dist >= -3 & d$dist < 3), 0.5)",
"cat(\"binomial test p-value:\", format.pval(bt$p.value, digits = 3,",
"                                        eps = 0.0001), \"\\n\")",
"cat(\"  Excess mass BELOW the line. The side batting second is batting to stay\\n\")",
"cat(\"  under the margin and avoid the follow-on. It is the side that does NOT\\n\")",
"cat(\"  make the decision we are studying.\\n\")",
"",
"# -----------------------------------------------------------------------------",
"#  STEP 5. How much does that manipulation matter? Drop the affected matches.",
"# -----------------------------------------------------------------------------",
"dd <- d[abs(d$dist) > 2, ]",
"cat(\"\\nExcluding matches within 2 runs of the cutoff:\\n\")",
"summary(rdrobust(y = dd$first_side_won, x = dd$dist, c = 0,",
"                 fuzzy = dd$enforced, masspoints = \"adjust\"))")
writeLines(loader, file.path(REL, "load_data.R"))

cat("\nrelease package written to", REL, "\n")
print(list.files(REL))

# Codebook: followon_matches.csv

One row per first-class cricket match. 22113 matches, 1900 to 2025.

Every variable below is either a public scorecard fact or a variable we
constructed from one. See README.md for how the sample was built.

| Variable | Type | Definition |
|---|---|---|
| `match_id` | text | Unique match identifier. |
| `competition` | text | Test, County, SheffieldShield, NZ-FC, RSA-FC, WI-FC, Pak-FC, Ire-FC, Zim-FC. |
| `season` | integer | Year in which the season began. |
| `start_date` | date | First scheduled day of the match. |
| `days_scheduled` | integer | Days the match was scheduled to last. Determines the statutory margin. 99 denotes a timeless match. |
| `team_batting_first` | text | The side that batted first. Only this side may enforce the follow-on. |
| `team_batting_second` | text | The side that batted second. |
| `captain_batting_first` | text | Captain of the side batting first, the person who took the decision. |
| `runs_first_innings_1` | integer | First-innings score of the side batting first. |
| `runs_first_innings_2` | integer | First-innings score of the side batting second. |
| `lead` | integer | `runs_first_innings_1` minus `runs_first_innings_2`. Negative if the side batting first fell behind. |
| `statutory_margin` | integer | Runs required to enforce the follow-on, from the Laws of Cricket. 150 for matches of 3+ days before 1980; from 1980, 200 for 5+ days and 150 for 3 or 4 days. |
| `dist` | integer | `lead` minus `statutory_margin`. The running variable. Eligibility begins at 0. |
| `eligible` | 0/1 | 1 if `lead >= statutory_margin`, so the captain had the option. |
| `enforced` | 0/1 | 1 if the follow-on was enforced. The treatment. |
| `near_miss` | 0/1 | 1 if the follow-on was enforced, the opponent erased the first-innings deficit in its second innings and the enforcing side did not lose. |
| `winner` | text | Winning team, or missing for a draw. |
| `result_type` | text | How the match ended. |
| `is_draw` | logical | TRUE if the match ended without a result. |
| `first_side_won` | 0/1 | 1 if the side batting first won. Main outcome. |
| `first_side_lost` | 0/1 | 1 if the side batting first lost. |
| `decisive_result` | 0/1 | 1 if the match did not end in a draw. |
| `venue_country` | text | Country in which the match was played. |

## Notes for students

1. **`eligible` is not a choice.** It is fixed by the Laws once the two first
   innings are complete. That is what makes the regression discontinuity design
   possible: `dist` crosses zero and the captain's option appears.
2. **`enforced` is a choice**, made only when `eligible == 1`. Enforcement is
   almost never recorded when `eligible == 0`, and where it is, the record is an
   error and the match has been removed.
3. **Only the side batting first can enforce.** If you rebuild this file from
   scorecards, do not define the leading side as whichever team scored more.
   That mistake inflates the number of eligible matches by about two thirds.
4. The running variable is a whole number of runs, so it has many ties. This
   matters for inference; see the appendix of the paper.

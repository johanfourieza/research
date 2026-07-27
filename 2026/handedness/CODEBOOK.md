# Codebook

All five `data/analysis/analysis_*.csv` files share the same 67 columns; one
row is one batting partnership. Missing values are empty strings. The paper's
main estimation sample applies `hand_known == 1 & both_avg_known == 1 &
either_debutant == 0`.

## Identifiers and match context

| Variable | Type | Description |
|---|---|---|
| `match_id` | character | Cricsheet match identifier. |
| `match_innings_id` | character | Match × innings identifier (`match_id` + innings); the fixed-effect cell of the preferred specification. |
| `innings` | integer | Innings number within the match (1–4 in first-class/Tests, 1–2 in limited-overs). |
| `batting_team` | character | Team batting during the partnership. |
| `batter_1_team` | character | Team of batter 1 (equals `batting_team`; retained as a build check). |
| `team_1`, `team_2` | character | The two sides in the match. |
| `venue`, `city` | character | Ground and city, as recorded by Cricsheet. |
| `event_name` | character | Competition or series name. |
| `start_date` | date | Match start date. |
| `year`, `decade`, `era` | integer/character | Calendar year of the match and derived period groupings. |
| `toss_winner`, `toss_decision` | character | Toss outcome and the winner's choice (bat/field). |
| `winner` | character | Match winner (empty for draws/ties/no-results). |

## Partnership composition

| Variable | Type | Description |
|---|---|---|
| `partnership_number` | integer | Position of the partnership in the innings (1 = opening pair). |
| `batter_1`, `batter_2` | character | The two batsmen (Cricsheet names). |
| `batter_1_hand`, `batter_2_hand` | character | Batting hand of each batsman (`left`/`right`), from `cricketdata`/ESPNcricinfo. |
| `hand_combination` | character | `LL`, `LR` or `RR`. |
| `is_mixed_hand` | integer | 1 if the pair is left–right (`LR`); the treatment variable. |
| `hand_known` | integer | 1 if both batsmen's hands are known (sample filter). |
| `batting_position_1`, `batting_position_2` | integer | Batting-order positions of the two batsmen. |
| `max_bat_pos`, `min_bat_pos` | integer | Higher and lower of the two batting positions; `max_bat_pos` defines the position split (1–2 / 3–6 / 7–11). |
| `position_group` | character | Top/middle/lower-order grouping. |

## Partnership outcomes

| Variable | Type | Description |
|---|---|---|
| `runs_scored` | integer | Total partnership runs, including all extras (the main dependent variable). |
| `batter_runs` | integer | Runs off the bat only (excluding extras). |
| `balls_faced` | integer | Legal deliveries faced (excludes wides and no-balls). |
| `deliveries` | integer | All deliveries including illegal ones. |
| `run_rate` | numeric | Runs per 100 balls faced (partnership strike rate). |
| `end_reason` | character | `wicket`, `innings_end` or `retired`. |
| `is_censored` | integer | 1 if the partnership ended without a wicket (right-censored in the survival analysis only). |
| `short_partnership` | integer | 1 if fewer than 5 balls (robustness exclusion flag). |

## Match situation at partnership start

| Variable | Type | Description |
|---|---|---|
| `runs_at_start` | integer | Team score when the partnership began. |
| `wickets_at_start` | integer | Team wickets down when the partnership began. |

## Pre-match career statistics (quality controls)

Computed per batsman from all *prior* matches in the same format within the
Cricsheet coverage window (suffix `_1`/`_2` = batter 1/2).

| Variable | Type | Description |
|---|---|---|
| `pre_match_matches_*` | integer | Prior matches played. |
| `pre_match_innings_*` | integer | Prior innings batted. |
| `pre_match_runs_*` | integer | Prior career runs. |
| `pre_match_balls_*` | integer | Prior balls faced. |
| `pre_match_dismissals_*` | integer | Prior dismissals. |
| `pre_match_avg_*` | numeric | Prior batting average (runs per dismissal). |
| `pre_match_sr_*` | numeric | Prior strike rate (runs per 100 balls). |
| `is_debutant_*` | integer | 1 if the batsman had no prior match. |
| `either_debutant` | integer | 1 if either batsman is a debutant (excluded from the main sample). |
| `both_avg_known` | integer | 1 if both pre-match averages are defined (sample filter). |
| `max_pre_match_avg`, `min_pre_match_avg` | numeric | Higher and lower of the two pre-match averages (the preferred quality controls). |
| `avg_partnership_quality` | numeric | Mean of the two pre-match averages (legacy control; exact linear combination of max and min). |
| `avg_pre_match_sr` | numeric | Mean of the two pre-match strike rates. |
| `combined_experience` | integer | Sum of the two batsmen's prior matches. |

## Squad composition (instrument)

| Variable | Type | Description |
|---|---|---|
| `n_left`, `n_right`, `n_players` | integer | Left-handed, right-handed and total batsmen in the playing XI. |
| `prob_LR_squad` | numeric | Random-pairing probability 2·n_left·n_right/[n(n−1)] — the instrument of Equation 4. |

---

Machine-readable column inventory (with per-file types, row counts and
missing counts): [`docs/variable_definitions.csv`](docs/variable_definitions.csv).

# =============================================================================
# 04_player_histories.R
# Compute pre-match cumulative career statistics for each player.
#
# BATTING histories (per player-match):
#   - pre_match_innings, pre_match_runs, pre_match_dismissals
#   - pre_match_average (runs / dismissals)
#   - pre_match_balls, pre_match_strike_rate
#   - pre_match_matches
#   - is_debutant flag
#
# BOWLING histories (per bowler-match):
#   - pre_match_overs_bowled, pre_match_runs_conceded, pre_match_wickets_taken
#   - pre_match_economy (runs conceded per over)
#   - pre_match_bowling_avg (runs conceded / wickets)
#   - pre_match_deliveries_to_LH, pre_match_pct_del_to_LH
#
# Computed per format (Test stats are separate from ODI stats).
# =============================================================================

library(tidyverse)
library(data.table)

# --- Paths ---
base_dir      <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
processed_dir <- file.path(base_dir, "data", "processed")

formats <- c("tests", "odis", "t20is")

# =============================================================================
# Compute per-player per-match batting stats from deliveries
# =============================================================================

compute_player_match_stats <- function(deliveries_dt, match_info_df) {
  # Get match dates
  dates <- match_info_df %>%
    select(match_id, start_date) %>%
    mutate(start_date = as.Date(start_date))

  # Per batter per innings stats
  batter_stats <- deliveries_dt[,
    .(runs        = sum(runs_batter),
      balls_faced = sum(is_legal),
      total_del   = .N),
    by = .(match_id, innings, batter)
  ]

  # Build set of actual dismissals (exclude retired hurt etc.)
  wicket_rows <- deliveries_dt[is_wicket == 1L &
    !dismissal_type %in% c("retired hurt", "retired not out", "retired out")]
  actual_dismissals <- unique(wicket_rows[, .(match_id, innings, player_dismissed)])

  # Mark if batter was dismissed
  batter_stats <- merge(
    batter_stats,
    actual_dismissals[, .(match_id, innings, batter = player_dismissed, was_dismissed = 1L)],
    by = c("match_id", "innings", "batter"),
    all.x = TRUE
  )
  batter_stats[is.na(was_dismissed), was_dismissed := 0L]

  # Aggregate to match level (player may bat in multiple innings in Tests)
  match_level <- batter_stats[,
    .(innings_batted = uniqueN(innings),
      runs           = sum(runs),
      balls_faced    = sum(balls_faced),
      dismissals     = sum(was_dismissed)),
    by = .(match_id, batter)
  ]
  setnames(match_level, "batter", "player_name")

  # Merge dates
  match_level <- merge(match_level, as.data.table(dates), by = "match_id", all.x = TRUE)

  match_level
}

# =============================================================================
# Compute cumulative pre-match histories
# =============================================================================

compute_cumulative_histories <- function(match_level_dt) {
  setorder(match_level_dt, player_name, start_date, match_id)

  # Cumulative sums within player
  match_level_dt[, `:=`(
    cum_innings    = cumsum(innings_batted),
    cum_runs       = cumsum(runs),
    cum_balls      = cumsum(balls_faced),
    cum_dismissals = cumsum(dismissals),
    cum_matches    = seq_len(.N)
  ), by = player_name]

  # Shift by 1 to get pre-match values
  match_level_dt[, `:=`(
    pre_match_innings    = shift(cum_innings,    1L, fill = 0L),
    pre_match_runs       = shift(cum_runs,       1L, fill = 0L),
    pre_match_balls      = shift(cum_balls,      1L, fill = 0L),
    pre_match_dismissals = shift(cum_dismissals, 1L, fill = 0L),
    pre_match_matches    = shift(cum_matches,    1L, fill = 0L)
  ), by = player_name]

  # Derived stats
  match_level_dt[, pre_match_average := fifelse(
    pre_match_dismissals > 0,
    pre_match_runs / pre_match_dismissals,
    NA_real_
  )]
  match_level_dt[, pre_match_strike_rate := fifelse(
    pre_match_balls > 0,
    (pre_match_runs / pre_match_balls) * 100,
    NA_real_
  )]

  # Debutant flag
  match_level_dt[, is_debutant := fifelse(pre_match_matches == 0, 1L, 0L)]

  # Select output columns
  match_level_dt[, .(
    match_id, player_name, start_date,
    pre_match_matches, pre_match_innings, pre_match_runs,
    pre_match_balls, pre_match_dismissals,
    pre_match_average, pre_match_strike_rate,
    is_debutant
  )]
}

# =============================================================================
# Compute per-bowler per-match bowling stats from deliveries
# =============================================================================

compute_bowler_match_stats <- function(deliveries_dt, match_info_df, players_df = NULL) {
  # Get match dates
  dates <- match_info_df %>%
    select(match_id, start_date) %>%
    mutate(start_date = as.Date(start_date))

  # Build batter hand lookup from players CSV (if available)
  batter_hand_map <- NULL
  if (!is.null(players_df) && "batting_hand" %in% names(players_df)) {
    batter_hand_map <- players_df %>%
      filter(!is.na(batting_hand)) %>%
      count(player_name, batting_hand) %>%
      group_by(player_name) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player_name, batting_hand)
  }

  # Per bowler per match stats
  bowler_stats <- deliveries_dt[,
    .(deliveries_bowled = .N,
      legal_deliveries  = sum(is_legal),
      runs_conceded     = sum(runs_total) - sum(runs_batter == 0L) * 0L,
      runs_off_bat      = sum(runs_batter),
      extras_conceded   = sum(runs_extras),
      wickets_taken     = sum(is_wicket == 1L &
        !dismissal_type %in% c("retired hurt", "retired not out", "retired out",
                                "run out", "obstructing the field"))),
    by = .(match_id, bowler)
  ]

  # Correct runs_conceded: total runs scored off the bowler (bat + extras from bowler)
  # Re-compute more carefully: runs_batter + wides + no-balls are bowler's responsibility
  bowler_stats_v2 <- deliveries_dt[,
    .(deliveries_bowled = .N,
      legal_deliveries  = sum(is_legal),
      runs_conceded     = sum(runs_batter) + sum(extras_wides) + sum(extras_noballs),
      wickets_taken     = sum(is_wicket == 1L &
        !dismissal_type %in% c("retired hurt", "retired not out", "retired out",
                                "run out", "obstructing the field"))),
    by = .(match_id, bowler)
  ]

  # Count deliveries to left-handers (need batter hand info)
  if (!is.null(batter_hand_map)) {
    # Merge batter hand into deliveries
    del_with_hand <- merge(
      deliveries_dt[, .(match_id, batter, bowler, is_legal)],
      as.data.table(batter_hand_map),
      by.x = "batter", by.y = "player_name",
      all.x = TRUE
    )

    lh_stats <- del_with_hand[,
      .(deliveries_to_lh = sum(batting_hand == "left", na.rm = TRUE),
        deliveries_hand_known = sum(!is.na(batting_hand))),
      by = .(match_id, bowler)
    ]

    bowler_stats_v2 <- merge(bowler_stats_v2, lh_stats,
                             by = c("match_id", "bowler"), all.x = TRUE)
  } else {
    bowler_stats_v2[, `:=`(deliveries_to_lh = NA_integer_,
                           deliveries_hand_known = NA_integer_)]
  }

  setnames(bowler_stats_v2, "bowler", "player_name")

  # Merge dates
  bowler_stats_v2 <- merge(bowler_stats_v2, as.data.table(dates),
                           by = "match_id", all.x = TRUE)

  bowler_stats_v2
}

# =============================================================================
# Compute cumulative pre-match bowling histories
# =============================================================================

compute_cumulative_bowling_histories <- function(bowler_match_dt) {
  setorder(bowler_match_dt, player_name, start_date, match_id)

  # Cumulative sums within bowler
  bowler_match_dt[, `:=`(
    cum_deliveries      = cumsum(legal_deliveries),
    cum_runs_conceded   = cumsum(runs_conceded),
    cum_wickets         = cumsum(wickets_taken),
    cum_del_to_lh       = cumsum(fifelse(is.na(deliveries_to_lh), 0L, deliveries_to_lh)),
    cum_del_hand_known  = cumsum(fifelse(is.na(deliveries_hand_known), 0L, deliveries_hand_known)),
    cum_bowl_matches    = seq_len(.N)
  ), by = player_name]

  # Shift by 1 to get pre-match values
  bowler_match_dt[, `:=`(
    pre_match_bowl_deliveries   = shift(cum_deliveries,     1L, fill = 0L),
    pre_match_runs_conceded     = shift(cum_runs_conceded,  1L, fill = 0L),
    pre_match_wickets_taken     = shift(cum_wickets,        1L, fill = 0L),
    pre_match_del_to_lh         = shift(cum_del_to_lh,      1L, fill = 0L),
    pre_match_del_hand_known    = shift(cum_del_hand_known,  1L, fill = 0L),
    pre_match_bowl_matches      = shift(cum_bowl_matches,   1L, fill = 0L)
  ), by = player_name]

  # Derived stats
  bowler_match_dt[, pre_match_economy := fifelse(
    pre_match_bowl_deliveries > 0,
    (pre_match_runs_conceded / pre_match_bowl_deliveries) * 6,  # per over
    NA_real_
  )]
  bowler_match_dt[, pre_match_bowling_avg := fifelse(
    pre_match_wickets_taken > 0,
    pre_match_runs_conceded / pre_match_wickets_taken,
    NA_real_
  )]
  bowler_match_dt[, pre_match_pct_del_to_lh := fifelse(
    pre_match_del_hand_known > 0,
    (pre_match_del_to_lh / pre_match_del_hand_known) * 100,
    NA_real_
  )]

  # Select output columns
  bowler_match_dt[, .(
    match_id, player_name, start_date,
    pre_match_bowl_matches, pre_match_bowl_deliveries,
    pre_match_runs_conceded, pre_match_wickets_taken,
    pre_match_economy, pre_match_bowling_avg,
    pre_match_del_to_lh, pre_match_pct_del_to_lh
  )]
}

# =============================================================================
# Process one format
# =============================================================================

process_format <- function(format_name) {
  deliveries_path <- file.path(processed_dir, paste0("deliveries_", format_name, ".csv"))
  match_info_path <- file.path(processed_dir, paste0("match_info_", format_name, ".csv"))
  players_path    <- file.path(processed_dir, paste0("players_", format_name, ".csv"))

  if (!file.exists(deliveries_path)) {
    cat(sprintf("  Deliveries not found: %s\n", deliveries_path))
    return(invisible(NULL))
  }
  if (!file.exists(match_info_path)) {
    cat(sprintf("  Match info not found: %s\n", match_info_path))
    return(invisible(NULL))
  }

  cat("  Loading data...\n")
  deliveries <- fread(deliveries_path)
  match_info <- read_csv(match_info_path, show_col_types = FALSE)
  players_df <- if (file.exists(players_path)) {
    read_csv(players_path, show_col_types = FALSE)
  } else {
    NULL
  }
  cat(sprintf("  %d deliveries, %d matches\n", nrow(deliveries), nrow(match_info)))

  # --- Batting histories ---
  cat("  Computing per-match batting stats...\n")
  match_level <- compute_player_match_stats(deliveries, match_info)
  cat(sprintf("  %d player-match observations\n", nrow(match_level)))

  cat("  Computing cumulative pre-match batting histories...\n")
  histories <- compute_cumulative_histories(match_level)

  # Summary
  n_debutants <- sum(histories$is_debutant)
  n_with_avg  <- sum(!is.na(histories$pre_match_average))
  cat(sprintf("  %d player-match records\n", nrow(histories)))
  cat(sprintf("  %d debutant appearances (%.1f%%)\n",
              n_debutants, n_debutants / nrow(histories) * 100))
  cat(sprintf("  %d with defined pre-match average\n", n_with_avg))

  # Sanity check: top averages
  experienced <- histories[pre_match_matches >= 20]
  if (nrow(experienced) > 0) {
    top <- experienced[order(-pre_match_average)] %>%
      distinct(player_name, .keep_all = TRUE) %>%
      head(5)
    cat("  Top 5 pre-match averages (20+ matches):\n")
    for (j in seq_len(nrow(top))) {
      cat(sprintf("    %s: %.2f (%d matches)\n",
                  top$player_name[j], top$pre_match_average[j],
                  top$pre_match_matches[j]))
    }
  }

  out_path <- file.path(processed_dir, paste0("player_histories_", format_name, ".csv"))
  fwrite(histories, out_path)
  cat(sprintf("  Saved batting histories to %s\n", out_path))

  # --- Bowling histories ---
  cat("  Computing per-match bowling stats...\n")
  bowler_match <- compute_bowler_match_stats(deliveries, match_info, players_df)
  cat(sprintf("  %d bowler-match observations\n", nrow(bowler_match)))

  cat("  Computing cumulative pre-match bowling histories...\n")
  bowl_histories <- compute_cumulative_bowling_histories(bowler_match)

  n_bowl <- nrow(bowl_histories)
  n_with_econ <- sum(!is.na(bowl_histories$pre_match_economy))
  n_with_lh   <- sum(!is.na(bowl_histories$pre_match_pct_del_to_lh))
  cat(sprintf("  %d bowler-match records\n", n_bowl))
  cat(sprintf("  %d with defined pre-match economy\n", n_with_econ))
  cat(sprintf("  %d with defined pct_del_to_LH\n", n_with_lh))

  bowl_out_path <- file.path(processed_dir, paste0("bowler_histories_", format_name, ".csv"))
  fwrite(bowl_histories, bowl_out_path)
  cat(sprintf("  Saved bowling histories to %s\n", bowl_out_path))
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n=== Computing player histories for %s ===\n", toupper(fmt)))
  process_format(fmt)
}

cat("\nDone.\n")

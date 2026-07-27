# =============================================================================
# 08_ball_level_data.R
# Build ball-level analysis dataset for mechanism tests.
#
# For each delivery, construct:
#   - batter_hand, non_striker_hand, bowler_hand, bowler_type (pace/spin)
#   - LR_at_crease: 1 if batter and non-striker have different hands
#   - strike_changed: did the striker change from previous delivery?
#   - bowler_faces_different_hand: LR_at_crease * strike_changed
#   - Pre-match stats for striker, non-striker, bowler
#   - Partnership context (ball number, partnership_id)
#   - Innings phase indicators
#
# Output: data/analysis/ball_level_{format}.csv
# =============================================================================

library(tidyverse)
library(data.table)

# --- Paths ---
base_dir      <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
processed_dir <- file.path(base_dir, "data", "processed")
analysis_dir  <- file.path(base_dir, "data", "analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")

# =============================================================================
# Build hand lookups from players CSV
# =============================================================================

load_hand_lookups <- function(format_name) {
  players_path <- file.path(processed_dir, paste0("players_", format_name, ".csv"))
  df <- read_csv(players_path, col_types = cols(cricsheet_id = col_character()),
                 show_col_types = FALSE)

  # Batting hand: most common per player name
  bat_hand <- df %>%
    filter(!is.na(batting_hand)) %>%
    count(player_name, batting_hand) %>%
    group_by(player_name) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_name, batting_hand)

  # Bowling hand and type
  bowl_hand <- df %>%
    filter(!is.na(bowling_hand)) %>%
    count(player_name, bowling_hand) %>%
    group_by(player_name) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_name, bowling_hand)

  bowl_type <- df %>%
    filter(!is.na(bowling_type)) %>%
    count(player_name, bowling_type) %>%
    group_by(player_name) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_name, bowling_type)

  list(
    batting_hand  = deframe(bat_hand),
    bowling_hand  = deframe(bowl_hand),
    bowling_type  = deframe(bowl_type)
  )
}

# =============================================================================
# Process one format
# =============================================================================

process_format <- function(format_name) {
  deliveries_path <- file.path(processed_dir, paste0("deliveries_", format_name, ".csv"))
  histories_path  <- file.path(processed_dir, paste0("player_histories_", format_name, ".csv"))
  bowl_hist_path  <- file.path(processed_dir, paste0("bowler_histories_", format_name, ".csv"))
  match_info_path <- file.path(processed_dir, paste0("match_info_", format_name, ".csv"))

  if (!file.exists(deliveries_path)) {
    cat(sprintf("  Deliveries not found: %s\n", deliveries_path))
    return(invisible(NULL))
  }

  cat("  Loading data...\n")
  dt <- fread(deliveries_path)
  match_info <- fread(match_info_path)
  cat(sprintf("  %d deliveries\n", nrow(dt)))

  # Load hand lookups
  cat("  Loading hand lookups...\n")
  lookups <- load_hand_lookups(format_name)

  # Load player histories (batting)
  hist_dt <- NULL
  if (file.exists(histories_path)) {
    hist_dt <- fread(histories_path)
    cat(sprintf("  %d batting history records\n", nrow(hist_dt)))
  }

  # Load bowler histories
  bowl_dt <- NULL
  if (file.exists(bowl_hist_path)) {
    bowl_dt <- fread(bowl_hist_path)
    cat(sprintf("  %d bowling history records\n", nrow(bowl_dt)))
  }

  # --- Merge hand information ---
  cat("  Merging hand info...\n")

  # Map batter hand
  bat_hand_dt <- data.table(
    player_name = names(lookups$batting_hand),
    batter_hand = unname(lookups$batting_hand)
  )
  dt <- merge(dt, bat_hand_dt, by.x = "batter", by.y = "player_name", all.x = TRUE)

  # Map non-striker hand
  ns_hand_dt <- data.table(
    player_name = names(lookups$batting_hand),
    non_striker_hand = unname(lookups$batting_hand)
  )
  dt <- merge(dt, ns_hand_dt, by.x = "non_striker", by.y = "player_name", all.x = TRUE)

  # Map bowler hand and type
  bh_dt <- data.table(
    player_name = names(lookups$bowling_hand),
    bowler_hand = unname(lookups$bowling_hand)
  )
  dt <- merge(dt, bh_dt, by.x = "bowler", by.y = "player_name", all.x = TRUE)

  bt_dt <- data.table(
    player_name = names(lookups$bowling_type),
    bowler_type = unname(lookups$bowling_type)
  )
  dt <- merge(dt, bt_dt, by.x = "bowler", by.y = "player_name", all.x = TRUE)

  # --- Construct key mechanism variables ---
  cat("  Computing mechanism variables...\n")

  # Sort within each innings
  setorder(dt, match_id, innings, over, ball)

  # LR_at_crease: are batter and non-striker different-handed?
  dt[, LR_at_crease := fifelse(
    !is.na(batter_hand) & !is.na(non_striker_hand),
    fifelse(batter_hand != non_striker_hand, 1L, 0L),
    NA_integer_
  )]

  # strike_changed: did the striker change from previous delivery within same innings?
  dt[, prev_batter := shift(batter, 1L, type = "lag"), by = .(match_id, innings)]
  dt[, strike_changed := fifelse(
    !is.na(prev_batter),
    fifelse(batter != prev_batter, 1L, 0L),
    0L  # first delivery of innings
  )]

  # bowler_faces_different_hand: key interaction for mechanism test
  dt[, bowler_faces_different_hand := fifelse(
    !is.na(LR_at_crease),
    LR_at_crease * strike_changed,
    NA_integer_
  )]

  # Partnership tracking: build partnership_id within each innings
  dt[, pair := paste0(pmin(batter, non_striker), "|", pmax(batter, non_striker))]
  dt[, pair_changed := fifelse(
    !is.na(shift(pair, 1L, type = "lag")) & pair != shift(pair, 1L, type = "lag"),
    1L, 0L
  ), by = .(match_id, innings)]
  dt[, pair_changed := fifelse(is.na(pair_changed), 0L, pair_changed)]
  dt[, partnership_id := cumsum(pair_changed) + 1L, by = .(match_id, innings)]

  # Partnership ball number (legal deliveries only)
  dt[, partnership_ball_number := cumsum(is_legal), by = .(match_id, innings, partnership_id)]

  # Match x innings ID
  dt[, match_innings_id := paste0(match_id, "_", innings)]

  # Cumulative runs and wickets in innings (up to current delivery)
  dt[, cum_innings_runs := cumsum(runs_total), by = .(match_id, innings)]
  dt[, cum_innings_wickets := cumsum(is_wicket), by = .(match_id, innings)]

  # Over number (already in data as 'over')
  # Ball within over (already in data as 'ball')

  # --- Innings phase ---
  if (format_name == "tests") {
    # Tests: new ball indicator (first 20 overs, and after over 80)
    dt[, new_ball := fifelse(over < 20 | over >= 80, 1L, 0L)]
    dt[, innings_phase := fifelse(over < 20, "new_ball_1",
                           fifelse(over < 40, "early_middle",
                           fifelse(over < 60, "late_middle",
                           fifelse(over < 80, "old_ball", "new_ball_2"))))]
  } else if (format_name == "odis") {
    # ODIs: powerplay (0-9), middle (10-39), death (40-49)
    dt[, innings_phase := fifelse(over < 10, "powerplay",
                           fifelse(over < 40, "middle", "death"))]
  } else {
    # T20Is: powerplay (0-5), middle (6-14), death (15-19)
    dt[, innings_phase := fifelse(over < 6, "powerplay",
                           fifelse(over < 15, "middle", "death"))]
  }

  # --- Merge pre-match batting histories for striker ---
  if (!is.null(hist_dt)) {
    cat("  Merging batter pre-match stats...\n")
    bat_stats <- hist_dt[, .(match_id, player_name,
                             striker_pre_avg = pre_match_average,
                             striker_pre_sr  = pre_match_strike_rate,
                             striker_pre_matches = pre_match_matches)]
    dt <- merge(dt, bat_stats,
                by.x = c("match_id", "batter"),
                by.y = c("match_id", "player_name"),
                all.x = TRUE)

    # Non-striker pre-match stats
    ns_stats <- hist_dt[, .(match_id, player_name,
                            ns_pre_avg = pre_match_average,
                            ns_pre_sr  = pre_match_strike_rate)]
    dt <- merge(dt, ns_stats,
                by.x = c("match_id", "non_striker"),
                by.y = c("match_id", "player_name"),
                all.x = TRUE)
  }

  # --- Merge bowler pre-match histories ---
  if (!is.null(bowl_dt)) {
    cat("  Merging bowler pre-match stats...\n")
    bowl_stats <- bowl_dt[, .(match_id, player_name,
                              bowler_pre_economy   = pre_match_economy,
                              bowler_pre_bowl_avg  = pre_match_bowling_avg,
                              bowler_pre_pct_to_lh = pre_match_pct_del_to_lh,
                              bowler_pre_bowl_matches = pre_match_bowl_matches)]
    dt <- merge(dt, bowl_stats,
                by.x = c("match_id", "bowler"),
                by.y = c("match_id", "player_name"),
                all.x = TRUE)
  }

  # --- Merge match date ---
  match_dates <- match_info[, .(match_id, start_date)]
  dt <- merge(dt, match_dates, by = "match_id", all.x = TRUE)

  # Clean up helper columns
  dt[, c("prev_batter", "pair", "pair_changed") := NULL]

  # --- Summary ---
  cat(sprintf("  Ball-level dataset: %d rows\n", nrow(dt)))
  cat(sprintf("    With batter_hand: %d (%.1f%%)\n",
              sum(!is.na(dt$batter_hand)),
              sum(!is.na(dt$batter_hand)) / nrow(dt) * 100))
  cat(sprintf("    With LR_at_crease: %d (%.1f%%)\n",
              sum(!is.na(dt$LR_at_crease)),
              sum(!is.na(dt$LR_at_crease)) / nrow(dt) * 100))
  cat(sprintf("    Strike changes: %d (%.1f%%)\n",
              sum(dt$strike_changed == 1L, na.rm = TRUE),
              sum(dt$strike_changed == 1L, na.rm = TRUE) / nrow(dt) * 100))
  cat(sprintf("    bowler_faces_different_hand = 1: %d\n",
              sum(dt$bowler_faces_different_hand == 1L, na.rm = TRUE)))
  cat(sprintf("    With bowler_hand: %d (%.1f%%)\n",
              sum(!is.na(dt$bowler_hand)),
              sum(!is.na(dt$bowler_hand)) / nrow(dt) * 100))
  cat(sprintf("    With bowler_type: %d (%.1f%%)\n",
              sum(!is.na(dt$bowler_type)),
              sum(!is.na(dt$bowler_type)) / nrow(dt) * 100))

  # Save
  out_path <- file.path(analysis_dir, paste0("ball_level_", format_name, ".csv"))
  fwrite(dt, out_path)
  cat(sprintf("  Saved to %s\n", out_path))
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n=== Building ball-level data for %s ===\n", toupper(fmt)))
  process_format(fmt)
}

cat("\nDone.\n")

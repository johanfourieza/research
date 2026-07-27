# =============================================================================
# 05_build_analysis_data.R
# Merge partnerships, player histories, and match info into regression-ready
# datasets (one per format).
#
# Creates: analysis_{format}.csv in data/analysis/
#
# Variables constructed:
#   - avg_partnership_quality: mean of both batsmen's pre-match averages
#   - max_pre_match_avg, min_pre_match_avg
#   - combined_experience: sum of pre-match matches
#   - year, decade, era
#   - position_group (top/middle/lower order)
#   - match_innings_id: unique match x innings identifier (for match x innings FE)
#   - prob_LR_squad: squad composition IV = 2*n_L*n_R / (n*(n-1))
#   - end_reason, is_censored: partnership censoring info (from Step 3)
#   - Various filter flags (debutant, short partnership, etc.)
# =============================================================================

library(tidyverse)

# --- Paths ---
base_dir      <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), "..", ".."))
processed_dir <- file.path(base_dir, "data", "processed")
analysis_dir  <- file.path(base_dir, "data", "analysis")
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("county", "sheffield")  # CS paper: longer-form domestic first-class

# =============================================================================
# Process one format
# =============================================================================

process_format <- function(format_name) {
  partnerships_path <- file.path(processed_dir, paste0("partnerships_", format_name, ".csv"))
  histories_path    <- file.path(processed_dir, paste0("player_histories_", format_name, ".csv"))
  match_info_path   <- file.path(processed_dir, paste0("match_info_", format_name, ".csv"))
  players_path      <- file.path(processed_dir, paste0("players_", format_name, ".csv"))

  for (info in list(
    list(partnerships_path, "partnerships"),
    list(histories_path,    "player_histories"),
    list(match_info_path,   "match_info")
  )) {
    if (!file.exists(info[[1]])) {
      cat(sprintf("  %s not found: %s\n", info[[2]], info[[1]]))
      return(invisible(NULL))
    }
  }

  # Load data
  cat("  Loading data...\n")
  partnerships <- read_csv(partnerships_path, show_col_types = FALSE)
  histories    <- read_csv(histories_path, show_col_types = FALSE)
  match_info   <- read_csv(match_info_path, show_col_types = FALSE)
  players      <- if (file.exists(players_path)) {
    read_csv(players_path, show_col_types = FALSE)
  } else {
    NULL
  }

  cat(sprintf("  %d partnerships, %d player-match records, %d matches\n",
              nrow(partnerships), nrow(histories), nrow(match_info)))

  # --- Merge player histories for batter_1 ---
  hist_1 <- histories %>%
    select(
      match_id,
      batter_1            = player_name,
      pre_match_matches_1 = pre_match_matches,
      pre_match_innings_1 = pre_match_innings,
      pre_match_runs_1    = pre_match_runs,
      pre_match_balls_1   = pre_match_balls,
      pre_match_dismissals_1 = pre_match_dismissals,
      pre_match_avg_1     = pre_match_average,
      pre_match_sr_1      = pre_match_strike_rate,
      is_debutant_1       = is_debutant
    )

  df <- partnerships %>%
    left_join(hist_1, by = c("match_id", "batter_1"))

  # --- Merge player histories for batter_2 ---
  hist_2 <- histories %>%
    select(
      match_id,
      batter_2            = player_name,
      pre_match_matches_2 = pre_match_matches,
      pre_match_innings_2 = pre_match_innings,
      pre_match_runs_2    = pre_match_runs,
      pre_match_balls_2   = pre_match_balls,
      pre_match_dismissals_2 = pre_match_dismissals,
      pre_match_avg_2     = pre_match_average,
      pre_match_sr_2      = pre_match_strike_rate,
      is_debutant_2       = is_debutant
    )

  df <- df %>%
    left_join(hist_2, by = c("match_id", "batter_2"))

  # --- Merge match info ---
  df <- df %>%
    left_join(
      match_info %>% select(match_id, start_date, venue, city, team_1, team_2,
                            toss_winner, toss_decision, winner, event_name),
      by = "match_id"
    )

  # --- Construct squad composition IV ---
  # prob_LR_squad = 2 * n_L * n_R / (n * (n-1))
  # where n_L = left-handers in XI, n_R = right-handers in XI
  if (!is.null(players)) {
    squad_hand <- players %>%
      filter(!is.na(batting_hand)) %>%
      group_by(match_id, team) %>%
      summarise(
        n_players = n(),
        n_left    = sum(batting_hand == "left"),
        n_right   = sum(batting_hand == "right"),
        .groups = "drop"
      ) %>%
      mutate(
        prob_LR_squad = if_else(
          n_players >= 2,
          2 * n_left * n_right / (n_players * (n_players - 1)),
          NA_real_
        )
      ) %>%
      select(match_id, team, n_left, n_right, n_players, prob_LR_squad)

    # Merge squad IV using batting_team
    df <- df %>%
      left_join(squad_hand, by = c("match_id", "batting_team" = "team"))
  }

  # --- Construct analysis variables ---

  # Match x innings ID for finer FE
  df <- df %>%
    mutate(match_innings_id = paste0(match_id, "_", innings))

  # Quality measures
  df <- df %>%
    mutate(
      avg_partnership_quality = (pre_match_avg_1 + pre_match_avg_2) / 2,
      max_pre_match_avg = pmax(pre_match_avg_1, pre_match_avg_2, na.rm = TRUE),
      min_pre_match_avg = pmin(pre_match_avg_1, pre_match_avg_2, na.rm = TRUE)
    )

  # Experience
  df <- df %>%
    mutate(
      combined_experience = replace_na(pre_match_matches_1, 0) +
                            replace_na(pre_match_matches_2, 0)
    )

  # Strike rates
  df <- df %>%
    mutate(avg_pre_match_sr = (pre_match_sr_1 + pre_match_sr_2) / 2)

  # Time variables
  df <- df %>%
    mutate(
      start_date = as.Date(start_date),
      year   = as.integer(format(start_date, "%Y")),
      decade = (year %/% 10L) * 10L
    )

  # Era categories
  df <- df %>%
    mutate(
      era = case_when(
        year < 2000 ~ "pre_2000",
        year < 2010 ~ "2000_2010",
        TRUE        ~ "post_2010"
      )
    )

  # Batting position group
  df <- df %>%
    mutate(
      max_bat_pos = pmax(batting_position_1, batting_position_2, na.rm = TRUE),
      min_bat_pos = pmin(batting_position_1, batting_position_2, na.rm = TRUE),
      position_group = case_when(
        max_bat_pos <= 3  ~ "top_order",
        max_bat_pos <= 6  ~ "middle_order",
        max_bat_pos <= 12 ~ "lower_order",
        TRUE              ~ NA_character_
      )
    )

  # Team lookup for batter_1 (if players table available)
  if (!is.null(players)) {
    player_teams <- players %>%
      select(match_id, player_name, team) %>%
      distinct()

    df <- df %>%
      left_join(
        player_teams %>% rename(batter_1 = player_name, batter_1_team = team),
        by = c("match_id", "batter_1")
      )
  }

  # Debutant flags
  df <- df %>%
    mutate(
      either_debutant = as.integer(
        replace_na(is_debutant_1, 0) == 1 | replace_na(is_debutant_2, 0) == 1
      )
    )

  # Short partnership flag
  df <- df %>%
    mutate(short_partnership = as.integer(balls_faced < 5))

  # Partnership run rate
  df <- df %>%
    mutate(run_rate = if_else(balls_faced > 0, runs_scored / balls_faced * 6, NA_real_))

  # Filter flags (no rows dropped — let R analysis scripts handle filtering)
  df <- df %>%
    mutate(
      hand_known     = as.integer(!is.na(hand_combination)),
      both_avg_known = as.integer(!is.na(pre_match_avg_1) & !is.na(pre_match_avg_2))
    )

  # --- Summary ---
  n_total <- nrow(df)
  n_missing_hand <- sum(is.na(df$hand_combination))
  cat(sprintf("  %d partnerships with missing hand combination (%.1f%%)\n",
              n_missing_hand, n_missing_hand / n_total * 100))

  cat(sprintf("\n  Analysis dataset summary:\n"))
  cat(sprintf("    Total partnerships: %d\n", n_total))
  cat(sprintf("    With known hands: %d\n", sum(df$hand_known)))
  cat(sprintf("    With both averages known: %d\n", sum(df$both_avg_known)))
  cat(sprintf("    Neither debutant: %d\n", sum(df$either_debutant == 0)))

  # Censoring summary
  if ("end_reason" %in% names(df)) {
    cat("\n    End reason breakdown:\n")
    for (reason in sort(unique(df$end_reason))) {
      n_r <- sum(df$end_reason == reason, na.rm = TRUE)
      cat(sprintf("      %s: %d (%.1f%%)\n", reason, n_r, n_r / n_total * 100))
    }
    cat(sprintf("    Censored: %d (%.1f%%)\n",
                sum(df$is_censored, na.rm = TRUE),
                sum(df$is_censored, na.rm = TRUE) / n_total * 100))
  }

  # Squad IV summary
  if ("prob_LR_squad" %in% names(df)) {
    cat(sprintf("    With squad IV: %d\n", sum(!is.na(df$prob_LR_squad))))
    cat(sprintf("    Mean prob_LR_squad: %.3f\n", mean(df$prob_LR_squad, na.rm = TRUE)))
  }

  known <- df %>% filter(hand_known == 1)
  if (nrow(known) > 0) {
    cat("\n    Hand combination breakdown (known only):\n")
    for (combo in c("RR", "LR", "LL")) {
      sub <- known %>% filter(hand_combination == combo)
      cat(sprintf("      %s: n=%d, mean runs=%.1f\n",
                  combo, nrow(sub), mean(sub$runs_scored, na.rm = TRUE)))
    }
  }

  # Save
  out_path <- file.path(analysis_dir, paste0("analysis_", format_name, ".csv"))
  write_csv(df, out_path)
  cat(sprintf("\n  Saved %d rows to %s\n", nrow(df), out_path))
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n=== Building analysis dataset for %s ===\n", toupper(fmt)))
  process_format(fmt)
}

cat("\nDone.\n")

# =============================================================================
# 03_build_partnerships.R
# Reconstruct batting partnerships from ball-by-ball delivery data.
#
# For each partnership, compute:
#   - match_id, innings, partnership_number
#   - batter_1, batter_2 (ordered alphabetically)
#   - batting hand for each batter
#   - runs scored, balls faced
#   - hand combination (LR, LL, RR), is_mixed_hand
#   - batting positions, match situation at start
#   - end_reason: "wicket", "innings_end", "retired", "pair_change"
#   - is_censored: 1 if partnership did not end with a wicket (right-censored)
#
# Edge cases handled:
#   - Retired hurt -> partnership end; new partnership with replacement
#   - Declarations -> partnership ends (censored)
#   - Super overs -> excluded (already excluded in parsing)
#   - Wides/no-balls -> count toward runs but not balls faced
# =============================================================================

library(tidyverse)
library(data.table)

# --- Paths ---
base_dir      <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
processed_dir <- file.path(base_dir, "data", "processed")

formats <- c("tests", "odis", "t20is")

# =============================================================================
# Load batting hand lookup
# =============================================================================

load_hand_lookup <- function(format_name) {
  players_path <- file.path(processed_dir, paste0("players_", format_name, ".csv"))
  df <- read_csv(players_path, col_types = cols(cricsheet_id = col_character()))

  # Use the most common hand for each player name
  df %>%
    filter(!is.na(batting_hand)) %>%
    count(player_name, batting_hand) %>%
    group_by(player_name) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_name, batting_hand) %>%
    deframe()
}

# =============================================================================
# Build partnerships for a single innings
# =============================================================================

build_partnerships_for_innings <- function(inn_dt, hand_lookup) {
  if (nrow(inn_dt) == 0) return(data.table())

  # Sort by over and ball
  setorder(inn_dt, over, ball)

  match_id    <- inn_dt$match_id[1]
  innings_num <- inn_dt$innings[1]
  batting_team <- inn_dt$batting_team[1]

  partnerships   <- list()
  batting_order  <- character(0)

  # State tracking
  team_runs    <- 0L
  team_wickets <- 0L

  current_pair          <- NULL  # character vector of length 2 (sorted)
  partnership_runs      <- 0L
  partnership_balls     <- 0L
  partnership_batter_runs <- 0L
  partnership_start_runs    <- 0L
  partnership_start_wickets <- 0L
  partnership_deliveries    <- 0L

  get_batting_position <- function(player) {
    idx <- match(player, batting_order)
    if (is.na(idx)) {
      batting_order <<- c(batting_order, player)
      idx <- length(batting_order)
    }
    idx
  }

  save_partnership <- function(pair, runs, balls, batter_runs,
                               start_runs, start_wickets, n_del,
                               end_reason = "innings_end") {
    # Order alphabetically
    b1 <- min(pair)
    b2 <- max(pair)

    h1 <- hand_lookup[b1]
    h2 <- hand_lookup[b2]

    if (!is.na(h1) && !is.na(h2)) {
      combo <- paste0(sort(c(substr(toupper(h1), 1, 1),
                              substr(toupper(h2), 1, 1))), collapse = "")
      if (combo == "LR") {
        hand_combination <- "LR"
      } else if (combo == "LL") {
        hand_combination <- "LL"
      } else {
        hand_combination <- "RR"
      }
      is_mixed <- if (hand_combination == "LR") 1L else 0L
    } else {
      h1 <- NA_character_; h2 <- NA_character_
      hand_combination <- NA_character_
      is_mixed <- NA_integer_
    }

    # Censoring: partnership is right-censored if it didn't end with a wicket
    is_censored <- if (end_reason == "wicket") 0L else 1L

    partnerships[[length(partnerships) + 1]] <<- list(
      match_id           = match_id,
      innings            = innings_num,
      batting_team       = batting_team,
      partnership_number = length(partnerships) + 1L,
      batter_1           = b1,
      batter_2           = b2,
      batter_1_hand      = as.character(hand_lookup[b1]),
      batter_2_hand      = as.character(hand_lookup[b2]),
      hand_combination   = hand_combination,
      is_mixed_hand      = is_mixed,
      runs_scored        = runs,
      batter_runs        = batter_runs,
      balls_faced        = balls,
      deliveries         = n_del,
      batting_position_1 = get_batting_position(b1),
      batting_position_2 = get_batting_position(b2),
      runs_at_start      = start_runs,
      wickets_at_start   = start_wickets,
      end_reason         = end_reason,
      is_censored        = is_censored
    )
  }

  for (i in seq_len(nrow(inn_dt))) {
    row <- inn_dt[i, ]
    batter      <- row$batter
    non_striker <- row$non_striker
    this_pair   <- sort(c(batter, non_striker))

    # Register batting positions
    get_batting_position(batter)
    get_batting_position(non_striker)

    # Check if partnership has changed
    if (is.null(current_pair)) {
      # First delivery of innings
      current_pair          <- this_pair
      partnership_runs      <- 0L
      partnership_balls     <- 0L
      partnership_batter_runs <- 0L
      partnership_start_runs    <- team_runs
      partnership_start_wickets <- team_wickets
      partnership_deliveries    <- 0L
    } else if (!identical(this_pair, current_pair)) {
      # Partnership changed (retired hurt / substitution) — save the old one
      save_partnership(current_pair, partnership_runs, partnership_balls,
                       partnership_batter_runs, partnership_start_runs,
                       partnership_start_wickets, partnership_deliveries,
                       end_reason = "retired")
      # Start new partnership
      current_pair          <- this_pair
      partnership_runs      <- 0L
      partnership_balls     <- 0L
      partnership_batter_runs <- 0L
      partnership_start_runs    <- team_runs
      partnership_start_wickets <- team_wickets
      partnership_deliveries    <- 0L
    }

    # Accumulate
    partnership_runs        <- partnership_runs + row$runs_total
    partnership_batter_runs <- partnership_batter_runs + row$runs_batter
    partnership_deliveries  <- partnership_deliveries + 1L
    if (row$is_legal == 1L) {
      partnership_balls <- partnership_balls + 1L
    }

    team_runs <- team_runs + row$runs_total

    # Check for wicket
    if (row$is_wicket == 1L) {
      dismissal <- row$dismissal_type
      if (!dismissal %in% c("retired hurt", "retired not out", "retired out")) {
        team_wickets <- team_wickets + 1L
        save_partnership(current_pair, partnership_runs, partnership_balls,
                         partnership_batter_runs, partnership_start_runs,
                         partnership_start_wickets, partnership_deliveries,
                         end_reason = "wicket")
        current_pair <- NULL
      }
    }
  }

  # Save final unfinished partnership (innings ended without wicket = censored)
  if (!is.null(current_pair) && partnership_deliveries > 0) {
    save_partnership(current_pair, partnership_runs, partnership_balls,
                     partnership_batter_runs, partnership_start_runs,
                     partnership_start_wickets, partnership_deliveries,
                     end_reason = "innings_end")
  }

  rbindlist(partnerships)
}

# =============================================================================
# Process one format
# =============================================================================

process_format <- function(format_name) {
  deliveries_path <- file.path(processed_dir, paste0("deliveries_", format_name, ".csv"))
  if (!file.exists(deliveries_path)) {
    cat(sprintf("  Deliveries file not found: %s\n", deliveries_path))
    return(invisible(NULL))
  }

  cat("  Loading deliveries...\n")
  dt <- fread(deliveries_path)
  cat(sprintf("  %d deliveries loaded\n", nrow(dt)))

  cat("  Loading hand lookup...\n")
  hand_lookup <- load_hand_lookup(format_name)
  cat(sprintf("  %d players with known batting hand\n", length(hand_lookup)))

  # Process each match-innings
  groups <- dt[, .(grp_idx = .GRP), by = .(match_id, innings)]
  n_groups <- nrow(groups)
  cat(sprintf("  Processing %d innings...\n", n_groups))

  all_partnerships <- list()
  unique_keys <- unique(dt[, .(match_id, innings)])

  for (i in seq_len(nrow(unique_keys))) {
    if (i %% 2000 == 0) cat(sprintf("  Processed %d/%d innings...\n", i, n_groups))

    key <- unique_keys[i, ]
    inn_dt <- dt[match_id == key$match_id & innings == key$innings]
    partnerships <- build_partnerships_for_innings(inn_dt, hand_lookup)
    if (nrow(partnerships) > 0) {
      all_partnerships[[length(all_partnerships) + 1]] <- partnerships
    }
  }

  df_partnerships <- rbindlist(all_partnerships)
  cat(sprintf("  Built %d partnerships\n", nrow(df_partnerships)))

  # Summary
  if ("hand_combination" %in% names(df_partnerships)) {
    cat("  Hand combinations:\n")
    counts <- df_partnerships[!is.na(hand_combination), .N, by = hand_combination]
    for (j in seq_len(nrow(counts))) {
      cat(sprintf("    %s: %d\n", counts$hand_combination[j], counts$N[j]))
    }
    n_missing <- sum(is.na(df_partnerships$hand_combination))
    if (n_missing > 0) cat(sprintf("    Missing: %d\n", n_missing))
  }

  out_path <- file.path(processed_dir, paste0("partnerships_", format_name, ".csv"))
  fwrite(df_partnerships, out_path)
  cat(sprintf("  Saved to %s\n", out_path))
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n=== Building partnerships for %s ===\n", toupper(fmt)))
  process_format(fmt)
}

cat("\nDone.\n")

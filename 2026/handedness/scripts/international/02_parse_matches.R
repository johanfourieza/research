# =============================================================================
# 02_parse_matches.R
# Parse Cricsheet JSON match files into structured CSVs:
#   - match_info_{format}.csv
#   - players_{format}.csv      (with batting_hand, bowling_hand, bowling_type)
#   - deliveries_{format}.csv
# =============================================================================

library(jsonlite)
library(tidyverse)

# cricketdata provides player_meta with BattingStyle scraped from ESPNCricinfo.
# Install with: install.packages("cricketdata")
if (!requireNamespace("cricketdata", quietly = TRUE)) {
  install.packages("cricketdata")
}
library(cricketdata)

# --- Paths ---
base_dir      <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..")
raw_dir       <- file.path(base_dir, "data", "raw")
processed_dir <- file.path(base_dir, "data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")

# =============================================================================
# Parse a single JSON file
# =============================================================================

parse_match <- function(filepath) {
  match <- fromJSON(filepath, simplifyVector = FALSE)
  info     <- match$info %||% list()
  match_id <- tools::file_path_sans_ext(basename(filepath))

  # Filter: men's cricket only
  gender <- info$gender %||% "male"
  if (gender != "male") return(NULL)

  # --- Match info ---
  dates      <- info$dates %||% list()
  start_date <- if (length(dates) > 0) dates[[1]] else NA
  teams      <- info$teams %||% list()
  toss       <- info$toss %||% list()
  outcome    <- info$outcome %||% list()
  event      <- info$event %||% list()
  event_name <- if (is.list(event)) (event$name %||% "") else as.character(event)

  winner      <- outcome$winner %||% ""
  result_type <- outcome$result %||% ""
  if (winner == "" && result_type != "") winner <- result_type

  match_row <- tibble(
    match_id      = match_id,
    start_date    = as.character(start_date),
    match_type    = info$match_type %||% "",
    gender        = gender,
    venue         = info$venue %||% "",
    city          = info$city %||% "",
    team_1        = if (length(teams) >= 1) teams[[1]] else "",
    team_2        = if (length(teams) >= 2) teams[[2]] else "",
    toss_winner   = toss$winner %||% "",
    toss_decision = toss$decision %||% "",
    winner        = winner,
    result_type   = result_type,
    event_name    = event_name
  )

  # --- Player registry ---
  registry     <- info$registry$people %||% list()
  players_info <- info$players %||% list()

  player_rows <- map_dfr(names(players_info), function(team) {
    player_list <- players_info[[team]]
    tibble(
      match_id    = match_id,
      player_name = unlist(player_list),
      cricsheet_id = map_chr(unlist(player_list), function(pn) {
        as.character(registry[[pn]] %||% "")
      }),
      team = team
    )
  })

  # --- Deliveries ---
  innings_data <- match$innings %||% list()
  delivery_rows <- list()

  for (inn_idx in seq_along(innings_data)) {
    innings     <- innings_data[[inn_idx]]
    innings_num <- inn_idx
    batting_team <- innings$team %||% ""

    # Skip super overs
    if (isTRUE(innings$super_over)) next

    overs <- innings$overs %||% list()
    for (over_data in overs) {
      over_num   <- over_data$over %||% 0
      deliveries <- over_data$deliveries %||% list()

      for (ball_idx in seq_along(deliveries)) {
        d <- deliveries[[ball_idx]]

        batter      <- d$batter %||% ""
        non_striker <- d$non_striker %||% ""
        bowler      <- d$bowler %||% ""

        runs         <- d$runs %||% list()
        runs_batter  <- runs$batter %||% 0
        runs_extras  <- runs$extras %||% 0
        runs_total   <- runs$total %||% 0

        extras         <- d$extras %||% list()
        extras_wides   <- extras$wides %||% 0
        extras_noballs <- extras$noballs %||% 0
        extras_byes    <- extras$byes %||% 0
        extras_legbyes <- extras$legbyes %||% 0
        extras_penalty <- extras$penalty %||% 0

        # Wicket info
        wickets <- d$wickets %||% list()
        is_wicket <- if (length(wickets) > 0) 1L else 0L
        dismissal_type   <- ""
        player_dismissed <- ""
        if (length(wickets) > 0) {
          w <- wickets[[1]]
          dismissal_type   <- w$kind %||% ""
          player_dismissed <- w$player_out %||% ""
        }

        is_wide  <- if (extras_wides > 0) 1L else 0L
        is_noball <- if (extras_noballs > 0) 1L else 0L
        is_legal <- if (is_wide == 0 && is_noball == 0) 1L else 0L

        delivery_rows[[length(delivery_rows) + 1]] <- list(
          match_id         = match_id,
          innings          = innings_num,
          batting_team     = batting_team,
          over             = over_num,
          ball             = ball_idx,
          batter           = batter,
          non_striker      = non_striker,
          bowler           = bowler,
          runs_batter      = runs_batter,
          runs_extras      = runs_extras,
          runs_total       = runs_total,
          extras_wides     = extras_wides,
          extras_noballs   = extras_noballs,
          extras_byes      = extras_byes,
          extras_legbyes   = extras_legbyes,
          extras_penalty   = extras_penalty,
          is_wide          = is_wide,
          is_noball        = is_noball,
          is_legal         = is_legal,
          is_wicket        = is_wicket,
          dismissal_type   = dismissal_type,
          player_dismissed = player_dismissed
        )
      }
    }
  }

  deliveries_df <- bind_rows(delivery_rows)

  list(match_info = match_row, players = player_rows, deliveries = deliveries_df)
}

# =============================================================================
# Process all files for one format
# =============================================================================

process_format <- function(format_name) {
  fmt_dir    <- file.path(raw_dir, format_name)
  json_files <- sort(list.files(fmt_dir, pattern = "\\.json$", full.names = TRUE))

  if (length(json_files) == 0) {
    cat(sprintf("  No JSON files found in %s\n", fmt_dir))
    return(invisible(NULL))
  }

  cat(sprintf("  Found %d JSON files\n", length(json_files)))

  all_matches    <- list()
  all_players    <- list()
  all_deliveries <- list()
  skipped <- 0

  for (i in seq_along(json_files)) {
    if (i %% 500 == 0) cat(sprintf("  Processed %d/%d files...\n", i, length(json_files)))

    result <- tryCatch(parse_match(json_files[i]), error = function(e) {
      cat(sprintf("  Error parsing %s: %s\n", json_files[i], e$message))
      NULL
    })

    if (is.null(result)) {
      skipped <- skipped + 1
      next
    }

    all_matches[[length(all_matches) + 1]]       <- result$match_info
    all_players[[length(all_players) + 1]]        <- result$players
    all_deliveries[[length(all_deliveries) + 1]]  <- result$deliveries
  }

  cat(sprintf("  Parsed %d matches (%d skipped)\n",
              length(all_matches), skipped))

  df_matches    <- bind_rows(all_matches)
  df_players    <- bind_rows(all_players)
  df_deliveries <- bind_rows(all_deliveries)

  cat(sprintf("  %d player-match observations\n", nrow(df_players)))
  cat(sprintf("  %d deliveries\n", nrow(df_deliveries)))

  write_csv(df_matches,    file.path(processed_dir, paste0("match_info_", format_name, ".csv")))
  write_csv(df_players,    file.path(processed_dir, paste0("players_", format_name, ".csv")))
  write_csv(df_deliveries, file.path(processed_dir, paste0("deliveries_", format_name, ".csv")))
  cat(sprintf("  Saved to %s\n", processed_dir))
}

# =============================================================================
# Merge batting/bowling style using cricketdata::player_meta + Cricsheet people
#
# Strategy:
#   1. cricketdata::player_meta has BattingStyle and BowlingStyle from ESPNCricinfo.
#   2. Cricsheet people.csv maps cricsheet_id → key_cricinfo (ESPNCricinfo ID).
#   3. Join: players CSV → people.csv (on cricsheet_id) → player_meta (on key_cricinfo).
#
# Adds to players CSV:
#   - batting_hand: "left" / "right"
#   - bowling_hand: "left" / "right"
#   - bowling_type: "pace" / "spin"
# =============================================================================

merge_handedness <- function(formats_to_merge) {
  # --- Build lookup from cricketdata + people register ---
  people_csv <- file.path(raw_dir, "people.csv")
  if (!file.exists(people_csv)) {
    cat("  people.csv not found — skipping handedness merge\n")
    return(invisible(NULL))
  }

  people <- read_csv(people_csv, show_col_types = FALSE)
  cat(sprintf("  People register: %d entries\n", nrow(people)))

  # Get ESPNCricinfo player metadata from cricketdata package
  meta <- cricketdata::player_meta
  cat(sprintf("  cricketdata::player_meta: %d entries\n", nrow(meta)))

  # Detect column names — different cricketdata versions use different names:
  #   Newer: cricinfo_id, batting_hand, bowling_hand, bowling_style
  #   Older: ID, BattingStyle, BowlingStyle
  meta_cols <- names(meta)
  cat(sprintf("  player_meta columns: %s\n", paste(meta_cols, collapse = ", ")))

  # --- Resolve the ID column ---
  id_col <- if ("cricinfo_id" %in% meta_cols) "cricinfo_id" else
            if ("ID" %in% meta_cols) "ID" else
            stop("Cannot find player ID column in player_meta")

  # --- Resolve batting hand ---
  # "batting_hand" (newer) contains "left"/"right" directly
  # "batting_style" / "BattingStyle" (older) contains "Right hand Bat" etc.
  bat_col <- if ("batting_hand" %in% meta_cols) "batting_hand" else
             if ("batting_style" %in% meta_cols) "batting_style" else
             if ("BattingStyle" %in% meta_cols) "BattingStyle" else
             NA_character_

  if (is.na(bat_col)) {
    cat("  WARNING: No batting style/hand column found in player_meta\n")
    meta_bat <- tibble(cricinfo_id = character(), batting_hand = character())
  } else {
    meta_bat <- meta %>%
      mutate(cricinfo_id = as.character(.data[[id_col]])) %>%
      filter(!is.na(.data[[bat_col]])) %>%
      mutate(batting_hand = case_when(
        str_detect(tolower(.data[[bat_col]]), "left")  ~ "left",
        str_detect(tolower(.data[[bat_col]]), "right") ~ "right",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(batting_hand)) %>%
      select(cricinfo_id, batting_hand) %>%
      distinct(cricinfo_id, .keep_all = TRUE)
  }

  cat(sprintf("  ESPNCricinfo batting hand available for %d players\n", nrow(meta_bat)))

  # --- Resolve bowling style ---
  # "bowling_style" / "BowlingStyle": e.g. "Right-arm fast", "Left-arm orthodox"
  # "bowling_hand" (newer): may already contain "left"/"right"
  bowl_style_col <- if ("bowling_style" %in% meta_cols) "bowling_style" else
                    if ("BowlingStyle" %in% meta_cols) "BowlingStyle" else
                    NA_character_
  bowl_hand_col  <- if ("bowling_hand" %in% meta_cols) "bowling_hand" else
                    NA_character_

  if (is.na(bowl_style_col) && is.na(bowl_hand_col)) {
    cat("  WARNING: No bowling style/hand column found in player_meta\n")
    meta_bowl <- tibble(cricinfo_id = character(),
                        bowling_hand = character(), bowling_type = character())
  } else {
    meta_bowl <- meta %>%
      mutate(cricinfo_id = as.character(.data[[id_col]]))

    # Derive bowling_hand from style string if no dedicated column
    if (!is.na(bowl_hand_col)) {
      meta_bowl <- meta_bowl %>%
        mutate(bowl_hand_out = as.character(.data[[bowl_hand_col]]))
    } else {
      meta_bowl <- meta_bowl %>% mutate(bowl_hand_out = NA_character_)
    }

    if (!is.na(bowl_style_col)) {
      meta_bowl <- meta_bowl %>%
        filter(!is.na(.data[[bowl_style_col]])) %>%
        mutate(
          bowl_style_lower = tolower(.data[[bowl_style_col]]),
          # Extract hand from style string if not already available
          bowl_hand_out = if_else(
            is.na(bowl_hand_out),
            case_when(
              str_detect(bowl_style_lower, "left")  ~ "left",
              str_detect(bowl_style_lower, "right") ~ "right",
              TRUE ~ NA_character_
            ),
            bowl_hand_out
          ),
          # Classify pace vs spin
          bowling_type = case_when(
            str_detect(bowl_style_lower,
                       "fast|medium|pace|seam") ~ "pace",
            str_detect(bowl_style_lower,
                       "spin|orthodox|offbreak|legbreak|chinaman|slow|wrist") ~ "spin",
            TRUE ~ NA_character_
          )
        )
    } else {
      meta_bowl <- meta_bowl %>%
        mutate(bowling_type = NA_character_)
    }

    meta_bowl <- meta_bowl %>%
      rename(bowling_hand = bowl_hand_out) %>%
      filter(!is.na(bowling_hand) | !is.na(bowling_type)) %>%
      select(cricinfo_id, bowling_hand, bowling_type) %>%
      distinct(cricinfo_id, .keep_all = TRUE)
  }

  cat(sprintf("  ESPNCricinfo bowling style available for %d players\n", nrow(meta_bowl)))

  # Join: cricsheet_id → key_cricinfo → batting/bowling attributes
  if (!"key_cricinfo" %in% names(people)) {
    cat("  key_cricinfo column not found in people.csv\n")
    cat(sprintf("  Available columns: %s\n", paste(names(people), collapse = ", ")))
    return(invisible(NULL))
  }

  people_ids <- people %>%
    mutate(
      identifier = as.character(identifier),
      cricinfo_id = as.character(key_cricinfo)
    ) %>%
    filter(!is.na(cricinfo_id), cricinfo_id != "")

  # Build batting hand map
  bat_map <- people_ids %>%
    left_join(meta_bat, by = "cricinfo_id") %>%
    filter(!is.na(batting_hand)) %>%
    select(identifier, batting_hand) %>%
    distinct(identifier, .keep_all = TRUE)

  # Build bowling style map
  bowl_map <- people_ids %>%
    left_join(meta_bowl, by = "cricinfo_id") %>%
    filter(!is.na(bowling_hand) | !is.na(bowling_type)) %>%
    select(identifier, bowling_hand, bowling_type) %>%
    distinct(identifier, .keep_all = TRUE)

  cat(sprintf("  Mapped cricsheet_id → batting_hand for %d players\n", nrow(bat_map)))
  cat(sprintf("  Mapped cricsheet_id → bowling_style for %d players\n", nrow(bowl_map)))

  # --- Merge into each format's players CSV ---
  for (fmt in formats_to_merge) {
    players_path <- file.path(processed_dir, paste0("players_", fmt, ".csv"))
    if (!file.exists(players_path)) next

    df <- read_csv(players_path, col_types = cols(cricsheet_id = col_character()))

    # Remove any existing columns (in case of re-run)
    df <- df %>% select(-any_of(c("batting_hand", "bowling_hand", "bowling_type")))

    # Merge batting hand
    df <- df %>%
      left_join(bat_map, by = c("cricsheet_id" = "identifier"))

    # Merge bowling style
    df <- df %>%
      left_join(bowl_map, by = c("cricsheet_id" = "identifier"))

    n_total    <- nrow(df)
    n_bat      <- sum(!is.na(df$batting_hand))
    n_bowl     <- sum(!is.na(df$bowling_hand))
    n_bowltype <- sum(!is.na(df$bowling_type))
    cat(sprintf("  %s: batting_hand %d/%d, bowling_hand %d/%d, bowling_type %d/%d\n",
                fmt, n_bat, n_total, n_bowl, n_total, n_bowltype, n_total))

    write_csv(df, players_path)
  }
}

# =============================================================================
# Main
# =============================================================================

for (fmt in formats) {
  cat(sprintf("\n=== Parsing %s ===\n", toupper(fmt)))
  process_format(fmt)
}

cat("\n=== Merging batting hand info from people register ===\n")
merge_handedness(formats)

cat("\nDone.\n")

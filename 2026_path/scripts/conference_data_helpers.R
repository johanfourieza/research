# =============================================================================
# conference_data_helpers.R
# -----------------------------------------------------------------------------
# Readers that turn the hand-entered, clean conference workbook
# (data/Conference_Papers.xlsx) into the canonical objects used by the pipeline.
#
# Why this exists: the EHA programmes are multi-column PDFs that pdftools
# garbles (titles/authors swapped, room names read as papers). Those data were
# unusable and only ~78 of ~4,878 entries ever matched. The workbook is a
# manual transcription of the EHA programmes, 2006-2025, WITH exact session
# begin/end times -- which also revives the session-timing ("visibility shock")
# analysis. EHS continues to come from the (reliable) HTML parse.
#
# Sourced by:
#   - 00_data_collection.R          (canonical, documented build)
#   - rebuild_conference_data.R     (fast, network-free refresh after data entry)
#
# Functions:
#   clean_person_name()        - one canonical name-cleaner used everywhere
#   read_eha_conferences()     - Conferences sheet -> canonical EHA data.table
#   read_dissertation_prizes() - Prizes sheet      -> prize_dissertation_data.rds schema
# =============================================================================

suppressMessages({
  library(data.table)
  library(readxl)
})

# -----------------------------------------------------------------------------
# clean_person_name(): lowercase, strip everything but [a-z] and spaces, squeeze
# whitespace. Used for BOTH conference/prize recipients and journal authors so
# that equality comparisons are meaningful. (Single source of truth: the prize
# and author-exposure matching in 01_analysis.R must use this same function.)
# -----------------------------------------------------------------------------
clean_person_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z ]", " ", x)      # drop digits, punctuation, accents-as-? etc.
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# -----------------------------------------------------------------------------
# session_order_from_begin(): map an HHMM integer start time (e.g. 1330) to an
# ordinal within-day session block. Boundaries chosen to match the ACTUAL EHA
# timetable observed in the data:
#   begin < 09:00            -> 1  early morning   (08:00-08:30 block)
#   09:00 <= begin < 12:00    -> 2  late morning    (10:15-10:30 block; PRE-lunch)
#   12:00 <= begin < 14:00    -> 3  early afternoon (13:00-13:30 block; POST-lunch)
#   14:00 <= begin < 16:00    -> 4  mid afternoon   (14:15-15:30 block)
#   begin >= 16:00            -> 5  late afternoon  (16:30-17:45 block)
# The lunch contrast (pre_lunch vs post_lunch) uses the two lunch-adjacent
# blocks only and EXCLUDES the small, ambiguous 12:00 noon slot.
# -----------------------------------------------------------------------------
session_order_from_begin <- function(begin_hhmm) {
  dec <- (begin_hhmm %/% 100) + (begin_hhmm %% 100) / 60
  fifelse(is.na(dec), NA_integer_,
  fifelse(dec < 9,  1L,
  fifelse(dec < 12, 2L,
  fifelse(dec < 14, 3L,
  fifelse(dec < 16, 4L, 5L)))))
}

# -----------------------------------------------------------------------------
# read_eha_conferences(): build the canonical EHA conference data.table from the
# "Conferences" sheet. Output columns are a superset of what the existing EHS
# rows carry, so the two rbind cleanly (fill = TRUE) downstream.
# -----------------------------------------------------------------------------
read_eha_conferences <- function(xlsx_path) {

  raw <- as.data.table(read_excel(xlsx_path, sheet = "Conferences"))

  # Author-NAME columns are exactly "Author1".."Author10" (the count column is
  # "Authors", excluded by the \\d+$ anchor). Institutions repeat as duplicated
  # headers that readxl renames Institution...9, Institution...11, ...
  author_cols <- grep("^Author[0-9]+$", names(raw), value = TRUE)
  inst_cols   <- grep("^Institution", names(raw), value = TRUE)

  join_nonempty <- function(vals) {
    vals <- vals[!is.na(vals) & trimws(vals) != ""]
    if (length(vals) == 0) return(NA_character_)
    paste(trimws(vals), collapse = "; ")
  }

  authors_str <- apply(raw[, ..author_cols], 1, join_nonempty)
  affil_str   <- apply(raw[, ..inst_cols],   1, join_nonempty)

  begin <- as.integer(raw[["Begin time"]])
  end   <- as.integer(raw[["End time"]])

  dt <- data.table(
    conference     = as.character(raw[["Type"]]),
    year           = as.integer(raw[["Year"]]),
    day            = NA_character_,
    time           = sprintf("%02d:%02d", begin %/% 100, begin %% 100),
    session        = NA_character_,
    title          = as.character(raw[["Title"]]),
    authors        = authors_str,
    affiliations   = affil_str,
    author_count   = as.integer(raw[["Authors"]]),
    raw_text       = NA_character_,
    city           = as.character(raw[["City"]]),
    begin_time     = begin,
    end_time       = end,
    begin_dec      = (begin %/% 100) + (begin %% 100) / 60,
    session_order  = session_order_from_begin(begin),
    matched_id     = NA_integer_
  )

  # Lunch-shock flags (EHA only): the two blocks bracketing the lunch break.
  # pre_lunch  = late-morning block (session_order 2, ~10:15-10:30 start)
  # post_lunch = early-afternoon block (session_order 3, start >= 12:45)
  dt[, time_slot  := fifelse(begin_dec < 12, "morning", "afternoon")]
  dt[, hour       := begin %/% 100]
  dt[, pre_lunch  := as.integer(session_order == 2L)]
  dt[, post_lunch := as.integer(session_order == 3L & begin_dec >= 12.74)]
  dt[is.na(pre_lunch),  pre_lunch  := 0L]
  dt[is.na(post_lunch), post_lunch := 0L]

  # Mirror the columns 01_analysis.R derives, so nothing downstream is surprised.
  dt[, conf_title   := title]
  dt[, conf_authors := authors]
  dt[, conf_year    := year]

  dt[]
}

# -----------------------------------------------------------------------------
# read_dissertation_prizes(): build the prize_dissertation_data.rds schema from
# the "Prizes" sheet. Keeps the dissertation prizes (Gerschenkron, Nevins) only;
# the recipient is Author1 (dissertations are single-authored). matched_author
# is set to the cleaned recipient name so the analysis can match it by exact
# cleaned-name equality against journal-paper authors (with prize_year < pub_year).
# -----------------------------------------------------------------------------
read_dissertation_prizes <- function(xlsx_path) {

  raw <- as.data.table(read_excel(xlsx_path, sheet = "Prizes"))

  raw[, prize_short := fifelse(grepl("Gerschenkron", Prize, ignore.case = TRUE), "Gerschenkron",
                       fifelse(grepl("Nevins",       Prize, ignore.case = TRUE), "Nevins",
                               NA_character_))]
  diss <- raw[!is.na(prize_short)]

  out <- data.table(
    prize_name         = diss[["prize_short"]],
    recipient          = as.character(diss[["Author1"]]),
    dissertation_title = as.character(diss[["Title"]]),
    prize_year         = as.integer(diss[["Year"]]),
    source_url         = NA_character_
  )
  # Drop rows without a usable recipient name.
  out <- out[!is.na(recipient) & nchar(trimws(recipient)) >= 3]
  out[, recipient_clean := clean_person_name(recipient)]
  out[, matched_author  := recipient_clean]
  out[, match_distance  := 0]
  out[]
}

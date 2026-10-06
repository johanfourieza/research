# ============================================================================
# CENSUS NAME PARSER
# ============================================================================
# Reads the husband, wife and annotation cells of every eligible 1825 census
# household directly from the workbook, keyed on the pinned (district,
# source_row) list in data/inputs/census_numeric_review.csv. Counts are not
# touched: they come from the numeric review as before.
#
# Two layouts occur in the digitized returns:
#   columns       husband and wife on the same row, in separate columns
#                 (Stellenbosch, Graaff-Reinet, Clanwilliam, Albany, Beaufort,
#                 George, Uitenhage, Cradock, Worcester);
#   continuation  one name column; a wife is listed on the row immediately
#                 below her husband, with no counts (Cape, Swellendam).
#
# Head role: in the columns layout a man's name in the husband column makes a
# male head (the wife column gives his spouse); a woman's name alone makes a
# female head, never a husband and never her own spouse. In the continuation
# layout the role follows the settler counts (a man counted -> male head).
# A continuation row is attached as spouse only if the head is a couple
# (settler men >= 1 and settler women >= 1) and the next row is named, not
# itself an eligible household, and has no count in any count column.
#
# Returns one row per eligible household with: district, source_row,
# male_name_raw, female_name_raw, head_role, head_name_raw, spouse_name_raw,
# spouse_source_row, annotation, and audit flags.
# ============================================================================

parse_census_names <- function(workbook, numeric_review) {
  # count_from:count_to are the population columns (persons of every category,
  # adults and children); used only to decide whether a continuation row is a
  # person in her own right. Wife rows can carry landed-property entries
  # (loan places, quitrent, place names), which are not counts of persons.
  spec <- tibble::tribble(
    ~district,       ~sheet,               ~layout,        ~male, ~female, ~info, ~count_from, ~count_to,
    "Stellenbosch",  "Stellenbosch 1825",  "columns",       2L,    3L,     NA,     NA,          NA,
    "Graaff-Reinet", "Graaff-Reinet 1825", "columns",       2L,    3L,     NA,     NA,          NA,
    "Clanwilliam",   "Clanwilliam 1824",   "columns",       3L,    4L,     2L,     NA,          NA,
    "Albany",        "Albany 1825",        "columns",       4L,    5L,     3L,     NA,          NA,
    "Beaufort",      "Beaufort 1825",      "columns",       4L,    5L,     3L,     NA,          NA,
    "George",        "George 1825",        "columns",       4L,    5L,     3L,     NA,          NA,
    "Uitenhage",     "Uitenhage 1825",     "columns",       4L,    5L,     3L,     NA,          NA,
    "Cradock",       "Cradock 1823",       "columns",       4L,    5L,     3L,     NA,          NA,
    "Worcester",     "Worcester 1824",     "columns",       4L,    5L,     3L,     NA,          NA,
    "Cape",          "Cape district 1825", "continuation",  2L,    NA,     NA,     4L,          24L,
    "Swellendam",    "Swellendam 1825",    "continuation",  2L,    NA,     NA,     4L,          24L
  )
  # Transcription placeholders ("No '78' listed", "No name listed") are not names.
  txt <- function(x) {
    x <- trimws(as.character(x))
    ifelse(is.na(x) | x == "" | grepl("^no\\b.*\\blisted$", x, ignore.case = TRUE), NA_character_, x)
  }
  widow_re <- "widow(?!er)|weduw(?!naar|enaar)|\\bwed\\."   # not widower / weduwnaar
  out <- list()
  for (k in seq_len(nrow(spec))) {
    s <- spec[k, ]
    # Same call as the district blocks in pipeline.R, so row indices agree.
    d <- suppressMessages(readxl::read_excel(workbook, sheet = s$sheet, col_names = FALSE))
    el <- numeric_review[numeric_review$district == s$district, c("source_row", "settler_men", "settler_women")]
    r <- el$source_row
    stopifnot(all(r >= 1 & r <= nrow(d)))
    eligible_rows <- r
    has_counts <- function(i) {
      if (i > nrow(d)) return(TRUE)
      v <- unlist(d[i, s$count_from:s$count_to])
      v <- trimws(as.character(v)); v <- v[!is.na(v) & v != ""]
      any(!(v %in% c("0", "0.0")))
    }
    if (s$layout == "columns") {
      male <- txt(d[[s$male]][r]); female <- txt(d[[s$female]][r])
      info <- if (is.na(s$info)) rep(NA_character_, length(r)) else txt(d[[s$info]][r])
      men <- el$settler_men; women <- el$settler_women
      man_counted <- !is.na(men) & men >= 1
      # A man's name makes a male head only if a man is counted and the entry
      # is not a widow's. Widows are often entered under the late husband's
      # name ("(the Widow)", "(widow of)", "weduwe"), with her own name in the
      # women's column; a man counted there is another adult (e.g. a son).
      # A widow can also be marked in the annotation column ("De weduwee van ...")
      # or in the women's column ("The widow, Jeremias Jesaias Bouwer").
      widow_in_female <- !is.na(female) & grepl("^the widow\\b", female, ignore.case = TRUE)
      widow_entry <- (!is.na(male) & grepl(widow_re, male, ignore.case = TRUE, perl = TRUE)) |
        (!is.na(info) & grepl(widow_re, info, ignore.case = TRUE, perl = TRUE)) | widow_in_female
      if (any(widow_in_female)) {                       # husband named, widow unnamed
        male <- ifelse(widow_in_female & is.na(male), sub("^the widow,?\\s*", "", female, ignore.case = TRUE), male)
        female <- ifelse(widow_in_female, NA_character_, female)
      }
      role <- ifelse(!is.na(male) & man_counted & !widow_entry, "male",
                     ifelse((!man_counted | widow_entry) & !is.na(women) & women >= 1, "female", "unresolved"))
      res <- tibble::tibble(
        district = s$district, source_row = r, male_name_raw = male, female_name_raw = female,
        head_role = role,
        head_name_raw = ifelse(role == "male", male, ifelse(role == "female", female, NA_character_)),
        spouse_name_raw = ifelse(role == "male", female, NA_character_),
        spouse_source_row = ifelse(role == "male" & !is.na(female), r, NA_integer_),
        husband_named_absent = ifelse(role == "female", male, NA_character_),
        annotation = info)
    } else {
      name <- txt(d[[s$male]][r])
      men <- el$settler_men; women <- el$settler_women
      widow_entry <- !is.na(name) & grepl(widow_re, name, ignore.case = TRUE, perl = TRUE)
      role <- ifelse(!is.na(men) & men >= 1 & !widow_entry, "male",
                     ifelse(!is.na(women) & women >= 1, "female", "unresolved"))
      spouse <- rep(NA_character_, length(r)); spouse_row <- rep(NA_integer_, length(r))
      for (j in seq_along(r)) {
        i <- r[j]
        if (role[j] == "male" && !is.na(women[j]) && women[j] >= 1 && i < nrow(d)) {
          nxt <- txt(d[[s$male]][i + 1L])
          nr_next <- trimws(as.character(d[[1]][i + 1L]))
          numbered_next <- !is.na(nr_next) && nr_next != ""   # a numbered row starts a new entry
          if (!is.na(nxt) && !numbered_next && !((i + 1L) %in% eligible_rows) && !has_counts(i + 1L) &&
              !grepl("^FOLIO", nxt, ignore.case = TRUE)) {
            spouse[j] <- nxt; spouse_row[j] <- i + 1L
          }
        }
      }
      res <- tibble::tibble(
        district = s$district, source_row = r,
        male_name_raw = ifelse(role == "male", name, NA_character_),
        female_name_raw = ifelse(role == "female", name, spouse),
        head_role = role, head_name_raw = name, spouse_name_raw = spouse,
        spouse_source_row = spouse_row, husband_named_absent = NA_character_, annotation = NA_character_)
    }
    res$settler_men_check <- el$settler_men; res$settler_women_check <- el$settler_women
    out[[k]] <- res
  }
  names_tbl <- dplyr::bind_rows(out)
  # Audit flags: disagreements between names and counts are reported, not corrected.
  names_tbl <- names_tbl %>%
    dplyr::mutate(
      flag_widow_entry        = head_role == "female" & !is.na(husband_named_absent),
      flag_male_name_no_man   = head_role == "male" & !is.na(settler_men_check) & settler_men_check == 0,
      flag_woman_only_but_man = head_role == "female" & !is.na(settler_men_check) & settler_men_check >= 1,
      flag_couple_no_spouse   = head_role == "male" & !is.na(settler_women_check) & settler_women_check >= 1 & is.na(spouse_name_raw),
      flag_spouse_no_woman    = !is.na(spouse_name_raw) & !is.na(settler_women_check) & settler_women_check == 0,
      flag_unresolved         = head_role == "unresolved") %>%
    dplyr::select(-settler_men_check, -settler_women_check)
  stopifnot(nrow(names_tbl) == nrow(numeric_review),
            !anyDuplicated(names_tbl[c("district", "source_row")]),
            # each continuation row belongs to at most one household
            !anyDuplicated(stats::na.omit(paste(names_tbl$district, names_tbl$spouse_source_row)[
              names_tbl$spouse_source_row != names_tbl$source_row])))
  names_tbl
}

# Shared name standardisation for husbands, wives and female heads.
# "Surname, First names" -> surname_std, first_std, first_only (upper case).
std_census_name <- function(x) {
  x <- as.character(x)
  clean <- stringr::str_replace_all(x, "\\(.*?\\)", "")
  clean <- stringr::str_replace_all(clean, "\\?", "")
  clean <- trimws(clean)
  # "Munro. Andrew" / "Botha. E. M.": a period after the first word used as the comma.
  # Other names without a comma keep the whole string as surname (word order varies).
  clean <- ifelse(!is.na(clean) & !grepl(",", clean) & grepl("^[A-Za-z' -]{3,}\\.\\s+\\S", clean),
                  sub("\\.\\s+", ", ", clean), clean)
  comma <- !is.na(clean) & stringr::str_detect(clean, ",")
  surname <- ifelse(comma, trimws(stringr::str_extract(clean, "^[^,]+")), clean)
  first <- ifelse(comma, trimws(stringr::str_replace(clean, "^[^,]+,\\s*", "")), NA_character_)
  first <- stringr::str_replace_all(first, "(?i)\\b(sr\\.?|jr\\.?|senior|junior|snr\\.?|jnr\\.?)\\b", "")
  surname_std <- stringr::str_squish(toupper(surname))
  first_std <- stringr::str_squish(toupper(first))
  surname_std <- stringr::str_replace(surname_std, "^V\\.?\\s*D\\.?\\s+", "VAN DER ")
  surname_std <- stringr::str_replace(surname_std, "^V\\.?\\s+D\\.?\\s*", "VAN D")
  surname_std <- stringr::str_replace(surname_std, "^V\\.?\\s+", "VAN ")
  surname_std[!is.na(surname_std) & surname_std == ""] <- NA_character_
  first_std[!is.na(first_std) & first_std == ""] <- NA_character_
  tibble::tibble(surname = surname, first = first, surname_std = surname_std, first_std = first_std,
                 first_only = stringr::str_extract(first_std, "^\\S+"))
}

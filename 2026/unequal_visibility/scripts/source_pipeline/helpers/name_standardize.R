# ==============================================================================
# name_standardize.R — Cape Dutch name standardisation
# ==============================================================================

# Common Cape Dutch surname mappings (archaic -> modern)
.surname_map <- c(
  "jansz"     = "jansen",
  "janse"     = "jansen",
  "janszen"   = "jansen",
  "janz"      = "jansen",
  "pietersz"  = "pietersen",
  "pietersze" = "pietersen",
  "claesz"    = "claasen",
  "claasz"    = "claasen",
  "hendriksz" = "hendriksen",
  "hendricksz"= "hendriksen",
  "willemsz"  = "willemsen",
  "jacobsz"   = "jacobsen",
  "gerritsz"  = "gerritsen",
  "cornelisz" = "cornelissen",
  "harmensz"  = "harmensen",
  "harmansz"  = "harmansen",
  "dircksz"   = "dirksen",
  "dirksz"    = "dirksen"
)

# Common firstname normalizations
.firstname_map <- c(
  "jan"       = "jan",
  "johann"    = "johannes",
  "johan"     = "johannes",
  "joh"       = "johannes",
  "joh:s"     = "johannes",
  "joannes"   = "johannes",
  "pieter"    = "pieter",
  "peter"     = "pieter",
  "hendrick"  = "hendrik",
  "hendk"     = "hendrik",
  "hendr"     = "hendrik",
  "hendr:"    = "hendrik",
  "willem"    = "willem",
  "willm"     = "willem",
  "gerrit"    = "gerrit",
  "gerrt"     = "gerrit",
  "jacobus"   = "jacobus",
  "jacob"     = "jacob",
  "cornelis"  = "cornelis",
  "corns"     = "cornelis",
  "corn:s"    = "cornelis",
  "christiaan"= "christiaan",
  "christn"   = "christiaan",
  "chr:n"     = "christiaan",
  "daniel"    = "daniel",
  "danl"      = "daniel",
  "francois"  = "francois",
  "frans"     = "francois",
  "stephanus" = "stephanus",
  "steph:s"   = "stephanus",
  "maria"     = "maria",
  "catharina" = "catharina",
  "catrina"   = "catharina",
  "catrijn"   = "catharina",
  "anna"      = "anna",
  "johanna"   = "johanna",
  "elizabeth" = "elizabeth",
  "elisabeth"  = "elizabeth",
  "elizabet"  = "elizabeth",
  "susanna"   = "susanna",
  "magdalena" = "magdalena"
)

#' Standardize a Cape Dutch name (from "reg" attribute format: "Surname, Firstname")
#' @param name Character string in "Surname, Firstname" or "Firstname Surname" format
#' @return Named list with surname, firstname (lowercase, standardised)
standardize_name <- function(name) {
  if (is.na(name) || !nzchar(trimws(name))) {
    return(list(surname = NA_character_, firstname = NA_character_,
                surname_std = NA_character_, firstname_std = NA_character_))
  }

  name <- trimws(name)

  # Handle "Surname, Firstname" format (from reg= attribute)
  if (grepl(",", name)) {
    parts <- strsplit(name, ",\\s*")[[1]]
    surname <- trimws(parts[1])
    firstname <- trimws(paste(parts[-1], collapse = " "))
  } else {
    # "Firstname Surname" format - take last word as surname
    words <- strsplit(trimws(name), "\\s+")[[1]]
    # Handle "van", "de", "du", "van der", "van den" prefixes
    prefix_words <- c("van", "de", "du", "den", "der", "ten", "von", "la", "le")
    surname_start <- length(words)
    for (i in seq_along(words)) {
      if (i < length(words) && tolower(words[i]) %in% prefix_words) {
        surname_start <- i
        break
      }
    }
    if (surname_start == 1 && length(words) > 1) {
      surname_start <- length(words)
    }
    firstname <- paste(words[1:max(1, surname_start - 1)], collapse = " ")
    surname <- paste(words[surname_start:length(words)], collapse = " ")
  }

  surname_lc <- tolower(surname)
  firstname_lc <- tolower(firstname)

  # Apply surname standardisation
  # Strip trailing -sz, -sze type patronymic endings for lookup
  surname_base <- sub("s?ze?n?$", "", surname_lc)
  surname_std <- surname_lc
  if (surname_lc %in% names(.surname_map)) {
    surname_std <- .surname_map[surname_lc]
  }

  # Apply firstname standardisation
  first_word <- strsplit(firstname_lc, "\\s+")[[1]][1]
  firstname_std <- firstname_lc
  if (!is.na(first_word) && first_word %in% names(.firstname_map)) {
    firstname_std <- sub(paste0("^", gsub("([.:])", "\\\\\\1", first_word)),
                         .firstname_map[first_word], firstname_lc)
  }

  # Remove common abbreviation artifacts
  surname_std  <- gsub("[.:;]", "", surname_std)
  firstname_std <- gsub("[.:;]", "", firstname_std)

  list(
    surname       = surname,
    firstname     = firstname,
    surname_std   = trimws(surname_std),
    firstname_std = trimws(firstname_std)
  )
}

#' Vectorised name standardisation
#' @param names Character vector
#' @return data.table with surname, firstname, surname_std, firstname_std
standardize_names <- function(names) {
  unique_names <- unique(names)
  results <- lapply(unique_names, standardize_name)
  ans <- data.table(
    surname       = vapply(results, `[[`, character(1), "surname"),
    firstname     = vapply(results, `[[`, character(1), "firstname"),
    surname_std   = vapply(results, `[[`, character(1), "surname_std"),
    firstname_std = vapply(results, `[[`, character(1), "firstname_std")
  )
  ans[match(names, unique_names)]
}

#' Extract surname prefix for blocking (first 2 lowercase chars)
#' @param surname Character
#' @return Character, 2-char prefix
surname_block <- function(surname) {
  s <- tolower(trimws(surname))
  s <- gsub("[^a-z]", "", s)
  substr(s, 1, 2)
}

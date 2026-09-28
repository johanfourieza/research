# ==============================================================================
# xml_helpers.R — XML extraction utilities for MOOC8 files
# ==============================================================================

#' Extract file version from filename
#' @param filename e.g. "MOOC8_1.01-85_v3.1.00.xml"
#' @return Numeric version, e.g. 3.1
extract_file_version <- function(filename) {
  m <- regmatches(filename, regexpr("v([0-9]+\\.[0-9]+)", filename))
  if (length(m) == 0) return(NA_real_)
  as.numeric(sub("^v", "", m))
}

#' Extract volume number from filename
#' @param filename e.g. "MOOC8_1.01-85_v3.1.00.xml"
#' @return Integer volume number
extract_volume <- function(filename) {
  m <- regmatches(filename, regexpr("MOOC8_([0-9]+)", filename))
  if (length(m) == 0) return(NA_integer_)
  as.integer(sub("MOOC8_", "", m))
}

#' Parse a single MOOC8 XML file into inventories and items tables
#' @param filepath Path to XML file
#' @return List with two data.tables: inventories, items
parse_mooc8_file <- function(filepath, inventories_only = FALSE) {
  filename <- basename(filepath)
  file_version <- extract_file_version(filename)
  volume <- extract_volume(filename)

  doc <- xml2::read_xml(filepath)
  divs <- xml2::xml_find_all(doc, ".//div[not(@type)]")
  # Also get top-level divs that have n= attribute
  if (length(divs) == 0) {
    divs <- xml2::xml_find_all(doc, ".//div[@n]")
  }

  inv_list <- list()
  item_list <- list()

  for (div in divs) {
    div_id <- xml2::xml_attr(div, "n")
    if (is.na(div_id)) next

    # --- Extract head info ---
    head_node <- xml2::xml_find_first(div, "./head")

    # Date
    date_node <- xml2::xml_find_first(head_node, ".//date")
    date_value <- xml2::xml_attr(date_node, "value")
    date_text <- xml2::xml_text(date_node, trim = TRUE)

    # Person names (can be multiple)
    name_nodes <- xml2::xml_find_all(head_node,
                                     ".//name[@type='person']")
    person_reg <- vapply(name_nodes, function(n) {
      r <- xml2::xml_attr(n, "reg")
      if (is.na(r)) xml2::xml_text(n, trim = TRUE) else r
    }, character(1))
    person_text <- vapply(name_nodes, function(n) {
      xml2::xml_text(n, trim = TRUE)
    }, character(1))

    person_name_1 <- if (length(person_reg) >= 1) person_reg[1] else NA_character_
    person_name_2 <- if (length(person_reg) >= 2) person_reg[2] else NA_character_
    person_text_1 <- if (length(person_text) >= 1) person_text[1] else NA_character_
    person_text_2 <- if (length(person_text) >= 2) person_text[2] else NA_character_

    # Geographic names
    geo_nodes <- xml2::xml_find_all(div,
                                    ".//name[@type='geographical']")
    geo_names <- paste(unique(vapply(geo_nodes, function(n) {
      xml2::xml_text(n, trim = TRUE)
    }, character(1))), collapse = "; ")
    if (!nzchar(geo_names)) geo_names <- NA_character_

    inv_list[[length(inv_list) + 1]] <- data.table(
      div_id        = div_id,
      source_file   = filename,
      file_version  = file_version,
      volume        = volume,
      date_value    = date_value,
      date_text     = date_text,
      person_name_1 = person_name_1,
      person_name_2 = person_name_2,
      person_text_1 = person_text_1,
      person_text_2 = person_text_2,
      geo_names     = geo_names
    )

    if (inventories_only) next
    # --- Extract items from tables ---
    tables <- xml2::xml_find_all(div, ".//table")
    for (tbl in tables) {
      tbl_head_node <- xml2::xml_find_first(tbl, "./head")
      tbl_head <- if (!is.na(tbl_head_node)) {
        xml2::xml_text(tbl_head_node, trim = TRUE)
      } else {
        NA_character_
      }

      rows <- xml2::xml_find_all(tbl, "./row")
      for (row in rows) {
        row_role <- xml2::xml_attr(row, "role")
        if (is.na(row_role)) row_role <- NA_character_

        cells <- xml2::xml_find_all(row, "./cell")
        if (length(cells) == 0) next

        # First cell is typically the description
        cell_text <- xml2::xml_text(cells[[1]], trim = TRUE)

        # Value cell(s) - typically the last non-empty cell
        cell_value <- NA_character_
        if (length(cells) >= 2) {
          for (ci in seq(length(cells), 2, -1)) {
            val <- xml2::xml_text(cells[[ci]], trim = TRUE)
            if (nzchar(val) && val != "ƒ" && val != "Rd:s" &&
                val != "Rds" && val != "Rx") {
              cell_value <- val
              break
            }
          }
        }

        # Skip empty rows and header rows
        if (!nzchar(cell_text) && is.na(cell_value)) next

        item_list[[length(item_list) + 1]] <- data.table(
          div_id     = div_id,
          table_head = tbl_head,
          row_role   = row_role,
          cell_text  = cell_text,
          cell_value = cell_value
        )
      }
    }
  }

  list(
    inventories = rbindlist(inv_list, fill = TRUE),
    items       = rbindlist(item_list, fill = TRUE)
  )
}

#' Parse date_value string into a Date or year
#' @param date_val Character, e.g. "16731020" or "YYYY"
#' @return Integer year, or NA
parse_mooc_year <- function(date_val) {
  if (is.na(date_val) || date_val == "YYYY" || !nzchar(date_val)) {
    return(NA_integer_)
  }
  # Standard format: YYYYMMDD
  yr <- suppressWarnings(as.integer(substr(date_val, 1, 4)))
  if (!is.na(yr) && yr >= 1600 && yr <= 1900) return(yr)
  NA_integer_
}

parse_mooc_years <- function(date_vals) {
  vapply(date_vals, parse_mooc_year, integer(1), USE.NAMES = FALSE)
}

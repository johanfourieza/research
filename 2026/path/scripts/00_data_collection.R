# =============================================================================
#
#   00_data_collection.R
#   Path Dependence in Economic History — Replication Package
#
#   PURPOSE:
#   Run ONCE to collect all external data from APIs and websites.
#   This script is expensive (several hours) and should not be re-run unless
#   you need to refresh external data. All sections check for cached output
#   and skip if the file already exists.
#
#   OUTPUTS (all saved to data/cache/):
#     - openalex_paper_matches.rds     (Section 1: paper-to-OpenAlex ID map)
#     - network_citation_data.rds      (Section 1: citing works for each paper)
#     - openalex_topic_data.rds        (Section 2: topic classifications;
#                                       no coverage on the 2012-2021 estimation
#                                       sample -- not used in the paper)
#     - repec_author_data.rds          (Section 3: author quality metrics)
#     - conference_parsed_data.rds     (Section 4: conference program papers,
#                                       EHA + EHS)
#     - prize_paper_data.rds           (Section 5: paper prize winners)
#     - prize_dissertation_data.rds    (Section 5: dissertation prize winners)
#
#   NOTE: this script is NOT needed to reproduce the paper. Every analysis
#   script runs offline from the caches shipped in data/cache/. Re-run this
#   only to refresh the external data; API results will have changed since
#   the shipped caches were collected (2026).
#
#   PREREQUISITES:
#     - data/raw/Journals_2026_clean.csv must exist
#     - Internet access for API calls and web scraping
#     - Environment variables OPENALEX_EMAIL (required) and REPEC_API_KEY
#       (required for Section 3); see README.md
#     - R packages listed in Section 0
#
#   ESTIMATED RUNTIME: 4-8 hours (mainly Section 1 citation fetching)
#
# =============================================================================


# =============================================================================
# SECTION 0: SETUP
# =============================================================================

rm(list = ls())

cat("\n")
cat("====================================================================\n")
cat("  PATH DEPENDENCE — DATA COLLECTION\n")
cat("  Run once to fetch all external data\n")
cat("====================================================================\n\n")

# --- Paths (relative to the replication/ root) ---
# Run from the replication/ directory.
DATA_DIR    <- "data/cache"      # API-derived outputs
RAW_DIR     <- "data/raw"        # hand-collected inputs
RESULTS_DIR <- "results"
SCRIPTS_DIR <- "scripts"
CONF_DIR    <- file.path(RAW_DIR, "Conference_Programs")

# Create directories if they don't exist
for (d in c(DATA_DIR, RESULTS_DIR, CONF_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Input data
CLEAN_DATA <- file.path(RAW_DIR, "Journals_2026_clean.csv")
if (!file.exists(CLEAN_DATA)) {
  stop("Input file not found: ", CLEAN_DATA,
       "\nPlace Journals_2026_clean.csv in data/raw/ before running.")
}

# --- API credentials (from environment variables; never hard-code keys) ---
# OpenAlex uses a "polite pool" with higher rate limits for identified users:
# an email address is sufficient; an API key is optional. RePEc requires an
# API key (request one from the RePEc team).
OPENALEX_EMAIL   <- Sys.getenv("OPENALEX_EMAIL")
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY")   # optional
REPEC_API_KEY    <- Sys.getenv("REPEC_API_KEY")

if (!nzchar(OPENALEX_EMAIL)) {
  stop("Set OPENALEX_EMAIL before running this script, e.g.\n",
       '  Sys.setenv(OPENALEX_EMAIL = "you@example.org")\n',
       "This script is NOT needed to reproduce the paper -- all analyses\n",
       "run offline from the shipped caches in data/cache/.")
}
if (!nzchar(REPEC_API_KEY)) {
  message("REPEC_API_KEY not set -- Section 3 (RePEc author data) will be skipped.")
}

# --- Rate Limiting ---
OPENALEX_DELAY  <- 0.12   # Seconds between OpenAlex requests (polite pool)
REPEC_DELAY     <- 0.5    # Seconds between REPEC requests
API_MAX_RETRIES <- 3      # Retries on failure

# --- Packages ---
packages <- c(
  "httr", "jsonlite",     # API calls

  "data.table",           # Fast data manipulation
  "stringdist",           # Fuzzy title/author matching
  "pdftools",             # Parse conference program PDFs
  "rvest", "xml2"         # Parse conference program HTML
)

new_pkg <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkg)) {
  stop("Missing packages: ", paste(new_pkg, collapse = ", "),
       "\nInstall them with: install.packages(c(",
       paste0('"', new_pkg, '"', collapse = ", "), "))")
}
invisible(lapply(packages, library, character.only = TRUE))

# The repec package (CRAN, July 2025) is required for Section 3 only.
repec_available <- requireNamespace("repec", quietly = TRUE) && nzchar(REPEC_API_KEY)
if (repec_available) {
  library(repec)
} else {
  message("repec package or REPEC_API_KEY unavailable -- Section 3 will be skipped.")
}

set.seed(42)
script_start <- Sys.time()

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x

# --- Logging ---
log_message <- function(msg) {
  cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n"))
}


# =============================================================================
# SECTION 1: OPENALEX — MATCH PAPERS AND FETCH CITATIONS
# =============================================================================
#
# For each paper in the journal dataset:
#   1. Match to an OpenAlex work ID by title (bulk + individual fallback)
#   2. For each matched paper, fetch all citing works
#   3. Extract: citing paper ID, top concept, institution country, references
#
# Outputs:
#   data/openalex_paper_matches.rds  — paper ID to OpenAlex ID mapping
#   data/network_citation_data.rds     — all citation links with metadata
#
# This is the most expensive section (hours for ~3700 papers).
# =============================================================================

cat("\n")
cat("====================================================================\n")
cat("  SECTION 1: OPENALEX — MATCH PAPERS AND FETCH CITATIONS\n")
cat("====================================================================\n\n")

CACHE_MATCHES   <- file.path(DATA_DIR, "openalex_paper_matches.rds")
CACHE_CITATIONS <- file.path(DATA_DIR, "network_citation_data.rds")

# ---------------------------------------------------------------------------
# 1.0 API helper functions
# ---------------------------------------------------------------------------

#' Polite, rate-limited GET with retry and exponential backoff
#' @param url API endpoint URL
#' @param params Named list of query parameters
#' @param retries Number of retry attempts
#' @return Parsed JSON list or NULL on failure
api_get <- function(url, params = list(), retries = API_MAX_RETRIES) {
  params$mailto <- OPENALEX_EMAIL
  if (nchar(OPENALEX_API_KEY) > 0) params$api_key <- OPENALEX_API_KEY

  for (attempt in 1:retries) {
    Sys.sleep(OPENALEX_DELAY)
    resp <- tryCatch(GET(url, query = params), error = function(e) NULL)

    if (!is.null(resp) && status_code(resp) == 200) {
      return(fromJSON(rawToChar(resp$content), flatten = TRUE))
    }

    # Rate limited: exponential backoff
    if (!is.null(resp) && status_code(resp) == 429) {
      Sys.sleep(2^attempt)
      next
    }

    if (attempt < retries) Sys.sleep(1)
  }
  return(NULL)
}

#' Normalize a title for matching: lowercase, strip punctuation, collapse spaces
normalize_title <- function(t) {
  t <- tolower(t)
  t <- gsub("[^a-z0-9 ]", "", t)
  t <- gsub("\\s+", " ", trimws(t))
  t
}

#' Match a single paper to OpenAlex by title search
#' @param title Paper title
#' @param year Publication year (optional, for disambiguation)
#' @return List with openalex_id, oa_cited_by_count, oa_year; or NULL
match_paper <- function(title, year = NULL) {
  title_clean <- gsub("[\"'\\(\\)]", "", title)
  title_clean <- trimws(title_clean)
  if (nchar(title_clean) > 120) title_clean <- substr(title_clean, 1, 120)

  result <- api_get("https://api.openalex.org/works",
                    params = list(search = title_clean, per_page = 5))

  if (is.null(result) || is.null(result$results) ||
      !is.data.frame(result$results) || nrow(result$results) == 0) {
    # Retry with shorter title (first 6 long words)
    words <- strsplit(title_clean, "\\s+")[[1]]
    words <- words[nchar(words) > 3]
    if (length(words) >= 4) {
      short <- paste(head(words, 6), collapse = " ")
      result <- api_get("https://api.openalex.org/works",
                        params = list(search = short, per_page = 5))
    }
  }

  if (is.null(result) || is.null(result$results) ||
      !is.data.frame(result$results) || nrow(result$results) == 0) {
    return(NULL)
  }

  df <- result$results

  # Prefer year match
  if (!is.null(year) && "publication_year" %in% names(df)) {
    nearby <- !is.na(df$publication_year) &
      df$publication_year >= (year - 1) & df$publication_year <= (year + 1)
    if (any(nearby)) df <- df[nearby, , drop = FALSE]
  }

  list(
    openalex_id       = df$id[1],
    oa_cited_by_count = if ("cited_by_count" %in% names(df)) df$cited_by_count[1] else NA,
    oa_year           = if ("publication_year" %in% names(df)) df$publication_year[1] else NA
  )
}

#' Fetch all works that cite a given OpenAlex work (paginated)
#' @param openalex_id Full OpenAlex work ID (URL format)
#' @return data.table of citing works, or NULL
get_citing_works <- function(openalex_id) {
  oa_id <- gsub("https://openalex.org/", "", openalex_id)

  all_results <- list()
  cursor <- "*"
  page <- 0

  repeat {
    page <- page + 1
    result <- api_get("https://api.openalex.org/works",
                      params = list(
                        filter   = paste0("cites:", oa_id),
                        per_page = 200,
                        cursor   = cursor,
                        select   = "id,publication_year,cited_by_count,concepts,authorships,referenced_works"
                      ))

    if (is.null(result) || is.null(result$results) ||
        !is.data.frame(result$results) || nrow(result$results) == 0) break

    all_results[[page]] <- result$results

    # Check for next page
    if (is.null(result$meta$next_cursor) || result$meta$next_cursor == "") break
    cursor <- result$meta$next_cursor

    # Safety: max 50 pages (10,000 citations)
    if (page >= 50) break
  }

  if (length(all_results) == 0) return(NULL)
  rbindlist(all_results, fill = TRUE)
}

# ---------------------------------------------------------------------------
# 1.1 Match papers to OpenAlex
# ---------------------------------------------------------------------------

if (file.exists(CACHE_MATCHES)) {
  oa_matches <- readRDS(CACHE_MATCHES)
  log_message(paste("Loaded cached OpenAlex matches:", nrow(oa_matches), "papers"))
} else {

  jn <- fread(CLEAN_DATA)
  # Standardise column names to lowercase
  if ("ID" %in% names(jn)) setnames(jn, "ID", "id")
  if ("Title" %in% names(jn)) setnames(jn, "Title", "title")
  if ("Year" %in% names(jn)) setnames(jn, "Year", "year")
  if ("Journal" %in% names(jn)) setnames(jn, "Journal", "journal")

  papers <- jn[, .(id, title, year, journal)]
  n_total <- nrow(papers)
  log_message(paste("Total papers to match:", n_total))

  # --- Step 1: Look up journal source IDs ---
  # Hardcoded OpenAlex source IDs for the six economic history journals
  journal_oa_source <- c(
    "Cliometrica"       = "https://openalex.org/S7241030",
    "EHDR"              = "https://openalex.org/S2764654660",
    "EHR"               = "https://openalex.org/S165492527",
    "EREH"              = "https://openalex.org/S176820844",
    "Explorations"      = "https://openalex.org/S145471836",
    "Explorations 2024" = "https://openalex.org/S145471836",
    "JEH"               = "https://openalex.org/S122073985"
  )

  papers[, journal := trimws(journal)]
  journals_unique <- unique(papers$journal)
  journals_unique <- journals_unique[!is.na(journals_unique) & journals_unique != ""]

  source_ids <- c()
  for (j_name in journals_unique) {
    if (j_name %in% names(journal_oa_source)) {
      sid <- gsub("https://openalex.org/", "", journal_oa_source[[j_name]])
      source_ids <- c(source_ids, sid)
      cat("  ", j_name, " -> ", journal_oa_source[[j_name]], "\n", sep = "")
    } else {
      cat("  ", j_name, " -> NOT FOUND (no hardcoded ID)\n", sep = "")
    }
  }
  source_ids <- unique(source_ids)

  # --- Step 2: Bulk download all works from these journals ---
  log_message("Bulk downloading all works from matched journals...")

  source_filter <- paste(source_ids, collapse = "|")
  year_min <- min(papers$year, na.rm = TRUE)
  year_max <- max(papers$year, na.rm = TRUE)

  filter_str <- paste0("primary_location.source.id:", source_filter,
                        ",publication_year:", year_min, "-", year_max)

  all_works <- list()
  cursor <- "*"
  page <- 0

  repeat {
    page <- page + 1
    result <- api_get("https://api.openalex.org/works",
                      params = list(
                        filter   = filter_str,
                        per_page = 200,
                        cursor   = cursor,
                        select   = "id,display_name,publication_year,cited_by_count"
                      ))

    if (is.null(result) || is.null(result$results) ||
        !is.data.frame(result$results) || nrow(result$results) == 0) break

    all_works[[page]] <- result$results
    n_fetched <- sum(sapply(all_works, nrow))

    if (page %% 10 == 0 || page == 1) {
      cat("\r  Page", page, "- works fetched:", n_fetched, "   ")
    }

    if (is.null(result$meta$next_cursor) || result$meta$next_cursor == "") break
    cursor <- result$meta$next_cursor
    if (page >= 500) break
  }
  cat("\n")

  if (length(all_works) == 0) {
    oa_works <- data.table()
    log_message("WARNING: Bulk download returned no results.")
  } else {
    oa_works <- rbindlist(all_works, fill = TRUE)
    log_message(paste("Downloaded", nrow(oa_works), "works from OpenAlex"))
  }

  # --- Step 3: Match by normalized title ---
  log_message("Matching titles...")

  bulk_matches <- data.table(
    id = character(0), openalex_id = character(0),
    oa_cited_by_count = numeric(0), oa_year = numeric(0)
  )

  if (nrow(oa_works) > 0) {
    oa_works_dt <- data.table(
      openalex_id       = oa_works$id,
      oa_display_name   = oa_works$display_name,
      oa_year           = oa_works$publication_year,
      oa_cited_by_count = oa_works$cited_by_count
    )
    oa_works_dt <- oa_works_dt[!is.na(oa_display_name) & oa_display_name != ""]
    oa_works_dt[, title_norm := normalize_title(oa_display_name)]

    papers[, title_norm := normalize_title(title)]

    # Merge on normalized title
    matched <- merge(papers[, .(id, year, title_norm)],
                     oa_works_dt[, .(openalex_id, oa_year, oa_cited_by_count, title_norm)],
                     by = "title_norm", allow.cartesian = TRUE)

    # If multiple OA matches for same paper, prefer closest year
    if (nrow(matched) > 0) {
      matched[, year_diff := abs(oa_year - year)]
      setorder(matched, id, year_diff)
      matched <- matched[!duplicated(id)]
      bulk_matches <- matched[, .(id, openalex_id, oa_cited_by_count, oa_year)]
    }

    log_message(paste("Bulk matched:", nrow(bulk_matches), "/", n_total, "papers"))
    papers[, title_norm := NULL]
  }

  # --- Step 4: Individual fallback search for unmatched papers ---
  unmatched_papers <- papers[!id %in% bulk_matches$id]

  if (nrow(unmatched_papers) > 0) {
    log_message(paste("Individual search for", nrow(unmatched_papers), "unmatched papers..."))

    fallback_matches <- list()
    for (i in seq_len(nrow(unmatched_papers))) {
      if (i %% 50 == 0 || i == nrow(unmatched_papers)) {
        n_found <- sum(!sapply(fallback_matches, is.null))
        cat("\r  Progress:", i, "/", nrow(unmatched_papers),
            "- Found:", n_found, "   ")
      }

      m <- tryCatch(
        match_paper(unmatched_papers$title[i], unmatched_papers$year[i]),
        error = function(e) NULL
      )
      if (!is.null(m)) {
        fallback_matches[[length(fallback_matches) + 1]] <- data.table(
          id              = unmatched_papers$id[i],
          openalex_id     = m$openalex_id,
          oa_cited_by_count = m$oa_cited_by_count,
          oa_year         = m$oa_year
        )
      }
    }
    cat("\n")

    if (length(fallback_matches) > 0) {
      fallback_dt <- rbindlist(fallback_matches, fill = TRUE)
      log_message(paste("Fallback matched:", nrow(fallback_dt), "additional papers"))
    } else {
      fallback_dt <- data.table(
        id = character(0), openalex_id = character(0),
        oa_cited_by_count = numeric(0), oa_year = numeric(0)
      )
    }

    oa_matches <- rbindlist(list(bulk_matches, fallback_dt), fill = TRUE)
  } else {
    oa_matches <- bulk_matches
  }

  log_message(paste("Final match rate:", nrow(oa_matches), "/", n_total,
                    "(", round(nrow(oa_matches) / n_total * 100, 1), "%)"))

  saveRDS(oa_matches, CACHE_MATCHES)
  log_message(paste("Saved:", CACHE_MATCHES))
}

# ---------------------------------------------------------------------------
# 1.2 Fetch citation data for all matched papers
# ---------------------------------------------------------------------------

if (file.exists(CACHE_CITATIONS)) {
  cite_raw <- readRDS(CACHE_CITATIONS)
  log_message(paste("Loaded cached citation data:", nrow(cite_raw), "citation links"))
} else {

  # Check for batch progress files from partial runs
  batch_dir <- file.path(DATA_DIR, "citation_batch_cache")
  if (!dir.exists(batch_dir)) dir.create(batch_dir, recursive = TRUE)

  existing_batches <- list.files(batch_dir, pattern = "^batch_\\d+\\.rds$",
                                  full.names = TRUE)
  cached_results <- list()
  cached_ids <- character(0)

  if (length(existing_batches) > 0) {
    log_message(paste("Found", length(existing_batches), "cached batch files. Loading..."))
    for (bf in existing_batches) {
      batch_dt <- tryCatch(readRDS(bf), error = function(e) NULL)
      if (!is.null(batch_dt) && is.data.table(batch_dt) && nrow(batch_dt) > 0) {
        cached_results[[length(cached_results) + 1]] <- batch_dt
        cached_ids <- c(cached_ids, unique(batch_dt$cited_id))
      }
    }
    cached_ids <- unique(cached_ids)
    log_message(paste("  Already completed:", length(cached_ids), "papers from cache"))
  }

  # Determine remaining work
  remaining <- oa_matches[!id %in% cached_ids]

  # Skip zero-citation papers (no citers to fetch)
  n_zero <- sum(remaining$oa_cited_by_count == 0, na.rm = TRUE)
  remaining_work <- remaining[is.na(oa_cited_by_count) | oa_cited_by_count > 0]

  log_message(paste("Total matched papers:", nrow(oa_matches)))
  log_message(paste("  Already cached:", length(cached_ids)))
  log_message(paste("  Zero citations (skipped):", n_zero))
  log_message(paste("  Remaining to fetch:", nrow(remaining_work)))

  if (nrow(remaining_work) > 0) {

    n_remaining <- nrow(remaining_work)
    batch_size <- 25
    batch_indices <- split(seq_len(n_remaining),
                           ceiling(seq_len(n_remaining) / batch_size))
    n_batches <- length(batch_indices)

    # Assign batch numbers that don't collide with existing cache files
    existing_nums <- as.integer(gsub("batch_(\\d+)\\.rds", "\\1",
                                     basename(existing_batches)))
    batch_start_num <- if (length(existing_nums) > 0) max(existing_nums) + 1L else 1L

    log_message(paste("Processing", n_remaining, "papers in", n_batches, "batches..."))
    cat("  (Batch files cached to '", batch_dir, "/' for resumability)\n\n", sep = "")

    for (b in seq_along(batch_indices)) {
      idx <- batch_indices[[b]]
      batch_num <- batch_start_num + b - 1L
      batch_file <- file.path(batch_dir, paste0("batch_", batch_num, ".rds"))

      # Skip if this batch was already saved
      if (file.exists(batch_file)) next

      batch_citations <- list()

      for (k in seq_along(idx)) {
        paper_idx <- idx[k]
        paper_id <- remaining_work$id[paper_idx]
        oa_id    <- remaining_work$openalex_id[paper_idx]

        citing <- tryCatch(get_citing_works(oa_id), error = function(e) NULL)

        if (!is.null(citing) && nrow(citing) > 0) {
          cite_dt <- data.table(
            cited_id              = paper_id,
            cited_oa_id           = oa_id,
            citing_oa_id          = citing$id,
            citing_year           = if ("publication_year" %in% names(citing))
              citing$publication_year else NA_integer_,
            citing_cited_by_count = if ("cited_by_count" %in% names(citing))
              citing$cited_by_count else NA_integer_
          )

          # Top concept
          if ("concepts" %in% names(citing)) {
            cite_dt$citing_top_concept <- sapply(seq_len(nrow(citing)), function(j) {
              concepts <- citing$concepts[[j]]
              if (is.data.frame(concepts) && nrow(concepts) > 0 &&
                  "display_name" %in% names(concepts)) {
                concepts$display_name[1]
              } else NA_character_
            })
          } else {
            cite_dt$citing_top_concept <- NA_character_
          }

          # First author country
          if ("authorships" %in% names(citing)) {
            cite_dt$citing_country <- sapply(seq_len(nrow(citing)), function(j) {
              auth <- citing$authorships[[j]]
              if (is.data.frame(auth) && nrow(auth) > 0 &&
                  "institutions" %in% names(auth)) {
                inst <- auth$institutions[[1]]
                if (is.data.frame(inst) && "country_code" %in% names(inst) &&
                    nrow(inst) > 0) {
                  return(inst$country_code[1])
                }
              }
              NA_character_
            })
          } else {
            cite_dt$citing_country <- NA_character_
          }

          # Referenced works (semicolon-delimited string)
          if ("referenced_works" %in% names(citing)) {
            cite_dt$citing_references <- sapply(citing$referenced_works, function(refs) {
              if (is.null(refs) || length(refs) == 0) return(NA_character_)
              paste(refs, collapse = ";")
            })
          } else {
            cite_dt$citing_references <- NA_character_
          }

          batch_citations[[k]] <- cite_dt
        }
      }

      # Save batch to disk
      if (length(batch_citations) > 0) {
        batch_result <- rbindlist(batch_citations, fill = TRUE)
      } else {
        batch_result <- data.table(
          cited_id = character(0), cited_oa_id = character(0),
          citing_oa_id = character(0), citing_year = integer(0),
          citing_cited_by_count = integer(0), citing_top_concept = character(0),
          citing_country = character(0), citing_references = character(0)
        )
      }
      saveRDS(batch_result, batch_file)
      cached_results[[length(cached_results) + 1]] <- batch_result

      # Progress reporting
      papers_done <- min(b * batch_size, n_remaining)
      cat("\r  Batch", b, "/", n_batches,
          " — Papers:", papers_done, "/", n_remaining, "   ")
    }
    cat("\n")
  }

  # Combine all results (cached + new)
  cite_raw <- rbindlist(cached_results, fill = TRUE)

  log_message(paste("Total citation links:", nrow(cite_raw)))
  log_message(paste("Unique citing papers:", uniqueN(cite_raw$citing_oa_id)))

  saveRDS(cite_raw, CACHE_CITATIONS)
  log_message(paste("Saved:", CACHE_CITATIONS))
}

cat("\n")


# =============================================================================
# SECTION 2: OPENALEX — TOPIC DATA
# =============================================================================
#
# For each paper in the journal dataset, query OpenAlex for:
#   - Primary topic, subfield, field, domain
#   - Referenced works count
#   - Cited-by count
#   - Match confidence score
#
# Output: data/openalex_topic_data.rds
# =============================================================================

cat("====================================================================\n")
cat("  SECTION 2: OPENALEX — TOPIC DATA\n")
cat("====================================================================\n\n")

CACHE_TOPICS <- file.path(DATA_DIR, "openalex_topic_data.rds")

if (file.exists(CACHE_TOPICS)) {
  openalex_data <- readRDS(CACHE_TOPICS)
  log_message(paste("Loaded cached topic data:", nrow(openalex_data), "papers"))
} else {

  # Load journal data
  jn <- fread(CLEAN_DATA)
  if ("ID" %in% names(jn)) setnames(jn, "ID", "id")
  if ("Title" %in% names(jn)) setnames(jn, "Title", "title")
  if ("Year" %in% names(jn)) setnames(jn, "Year", "year")

  papers_to_query <- jn[, .(id, title, year)]
  n_total <- nrow(papers_to_query)

  log_message(paste("Querying OpenAlex topics for", n_total, "papers..."))

  # Topic query function
  query_openalex_topic <- function(title, year = NULL) {

    na_result <- list(
      oa_id = NA_character_, oa_topic_id = NA_character_,
      oa_topic_name = NA_character_, oa_subfield = NA_character_,
      oa_field = NA_character_, oa_domain = NA_character_,
      referenced_works = character(0), cited_by_count = NA_integer_,
      topics = list(), match_confidence = NA_real_
    )

    if (is.na(title) || nchar(trimws(title)) < 10) return(na_result)

    tryCatch({
      title_clean <- gsub("[^a-zA-Z0-9 ]", "", title)
      title_clean <- gsub("\\s+", " ", trimws(title_clean))

      base_url <- "https://api.openalex.org/works"
      data <- NULL

      # Step 1: title.search filter (more precise)
      response <- httr::GET(base_url,
        query = list(filter = paste0("title.search:", title),
                     per_page = 10, mailto = OPENALEX_EMAIL,
                     api_key = OPENALEX_API_KEY),
        httr::add_headers("Accept" = "application/json"))
      Sys.sleep(OPENALEX_DELAY)

      if (httr::status_code(response) == 200) {
        d <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
        if (!is.null(d$results) && length(d$results) > 0 &&
            d$meta$count > 0 && d$meta$count < 500) {
          data <- d
        }
      }

      # Step 2: Fallback to full-text search
      if (is.null(data)) {
        Sys.sleep(OPENALEX_DELAY)
        response <- httr::GET(base_url,
          query = list(search = title_clean, per_page = 10,
                       mailto = OPENALEX_EMAIL,
                       api_key = OPENALEX_API_KEY),
          httr::add_headers("Accept" = "application/json"))
        Sys.sleep(OPENALEX_DELAY)

        if (httr::status_code(response) == 200) {
          d <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
          if (!is.null(d$results) && length(d$results) > 0 &&
              d$meta$count > 0 && d$meta$count < 5000) {
            data <- d
          }
        }
      }

      if (is.null(data)) return(na_result)

      # Find best match by title similarity + year proximity
      results_df <- data$results
      results_df$similarity <- stringdist::stringsim(
        tolower(title_clean),
        tolower(results_df$title %||% ""),
        method = "jw"
      )

      if (!is.null(year) && !is.na(year)) {
        results_df$year_ok <- abs(results_df$publication_year - year) <= 2
      } else {
        results_df$year_ok <- TRUE
      }

      valid <- results_df[results_df$year_ok & results_df$similarity >= 0.7, ]
      if (nrow(valid) == 0) {
        na_result$match_confidence <- max(results_df$similarity, na.rm = TRUE)
        return(na_result)
      }

      best_idx <- which.max(valid$similarity)
      best <- valid[best_idx, ]

      # Extract primary topic hierarchy
      primary_topic <- best$primary_topic

      oa_topic_id <- NA_character_
      oa_topic_name <- NA_character_
      oa_subfield <- NA_character_
      oa_field <- NA_character_
      oa_domain <- NA_character_

      if (!is.null(primary_topic) && length(primary_topic) > 0) {
        if (is.data.frame(primary_topic)) {
          oa_topic_id   <- primary_topic$id[1] %||% NA_character_
          oa_topic_name <- primary_topic$display_name[1] %||% NA_character_

          if (!is.null(primary_topic$subfield)) {
            if (is.data.frame(primary_topic$subfield)) {
              oa_subfield <- primary_topic$subfield$display_name[1] %||% NA_character_
            } else if (is.list(primary_topic$subfield)) {
              oa_subfield <- primary_topic$subfield[[1]]$display_name %||% NA_character_
            }
          }
          if (!is.null(primary_topic$field)) {
            if (is.data.frame(primary_topic$field)) {
              oa_field <- primary_topic$field$display_name[1] %||% NA_character_
            } else if (is.list(primary_topic$field)) {
              oa_field <- primary_topic$field[[1]]$display_name %||% NA_character_
            }
          }
          if (!is.null(primary_topic$domain)) {
            if (is.data.frame(primary_topic$domain)) {
              oa_domain <- primary_topic$domain$display_name[1] %||% NA_character_
            } else if (is.list(primary_topic$domain)) {
              oa_domain <- primary_topic$domain[[1]]$display_name %||% NA_character_
            }
          }
        }
      }

      # Referenced works
      referenced_works <- character(0)
      if (!is.null(best$referenced_works) && length(best$referenced_works) > 0) {
        if (is.list(best$referenced_works)) {
          referenced_works <- unlist(best$referenced_works[[1]])
        } else {
          referenced_works <- as.character(best$referenced_works)
        }
      }

      # All topics (for topic uniqueness calculation)
      all_topics <- list()
      if (!is.null(best$topics) && length(best$topics) > 0) {
        all_topics <- best$topics
      }

      return(list(
        oa_id = best$id %||% NA_character_,
        oa_topic_id = oa_topic_id,
        oa_topic_name = oa_topic_name,
        oa_subfield = oa_subfield,
        oa_field = oa_field,
        oa_domain = oa_domain,
        referenced_works = referenced_works,
        cited_by_count = as.integer(best$cited_by_count %||% NA),
        topics = all_topics,
        match_confidence = best$similarity
      ))

    }, error = function(e) {
      return(na_result)
    })
  }

  # Query each paper
  openalex_data <- data.table(
    paper_id = integer(), oa_id = character(),
    oa_topic_id = character(), oa_topic_name = character(),
    oa_subfield = character(), oa_field = character(),
    oa_domain = character(), n_references = integer(),
    cited_by_count = integer(), match_confidence = numeric(),
    query_time = as.POSIXct(character())
  )

  # Use a cache file for resumability
  topic_cache_file <- file.path(DATA_DIR, "openalex_topic_data_cache.rds")
  if (file.exists(topic_cache_file)) {
    openalex_data <- readRDS(topic_cache_file)
    already_queried <- openalex_data$paper_id
    papers_to_query <- papers_to_query[!(id %in% already_queried)]
    log_message(paste("Resuming — already have:", length(already_queried),
                      "papers. Remaining:", nrow(papers_to_query)))
  }

  if (nrow(papers_to_query) > 0) {

    all_referenced_works <- list()

    for (i in seq_len(nrow(papers_to_query))) {
      paper <- papers_to_query[i]

      result <- query_openalex_topic(title = paper$title, year = paper$year)

      new_row <- data.table(
        paper_id         = paper$id,
        oa_id            = result$oa_id,
        oa_topic_id      = result$oa_topic_id,
        oa_topic_name    = result$oa_topic_name,
        oa_subfield      = result$oa_subfield,
        oa_field         = result$oa_field,
        oa_domain        = result$oa_domain,
        n_references     = length(result$referenced_works),
        cited_by_count   = result$cited_by_count,
        match_confidence = result$match_confidence,
        query_time       = Sys.time()
      )

      openalex_data <- rbindlist(list(openalex_data, new_row), use.names = TRUE)

      if (length(result$referenced_works) > 0) {
        all_referenced_works[[as.character(paper$id)]] <- result$referenced_works
      }

      # Progress + periodic cache save
      if (i %% 100 == 0 || i == nrow(papers_to_query)) {
        cat("\r  Fetching paper", i, "of", nrow(papers_to_query), "  ")
        saveRDS(openalex_data, topic_cache_file)
      }
    }
    cat("\n")

    # Save referenced works for cross-citation analysis
    saveRDS(all_referenced_works, file.path(DATA_DIR, "referenced_works_raw.rds"))
  }

  # Save final output
  saveRDS(openalex_data, CACHE_TOPICS)
  log_message(paste("Papers matched:", sum(!is.na(openalex_data$oa_id)),
                    "/ Match rate:", round(mean(!is.na(openalex_data$oa_id)) * 100, 1), "%"))
  log_message(paste("Saved:", CACHE_TOPICS))
}

cat("\n")


# =============================================================================
# SECTION 3: REPEC — AUTHOR QUALITY
# =============================================================================
#
# For each unique author in the journal dataset, query the REPEC API for:
#   - REPEC author ID (via fuzzy name match)
#   - h-index
#   - First publication year (proxy for seniority)
#   - Supervisor ID (for advisor-network analysis)
#   - Whether supervisor is "prolific" (h-index > 20)
#   - Working paper count and NBER affiliation
#
# Output: data/repec_author_data.rds
# =============================================================================

cat("====================================================================\n")
cat("  SECTION 3: REPEC — AUTHOR QUALITY\n")
cat("====================================================================\n\n")

CACHE_REPEC <- file.path(DATA_DIR, "repec_author_data.rds")

if (file.exists(CACHE_REPEC)) {
  repec_data <- readRDS(CACHE_REPEC)
  log_message(paste("Loaded cached REPEC data:", nrow(repec_data), "authors"))
} else if (!repec_available) {
  log_message("REPEC package not available. Skipping Section 3.")
  log_message("Install with: install.packages('repec')")
  repec_data <- NULL
} else {

  # Load journal data and extract unique authors
  jn <- fread(CLEAN_DATA)
  author_cols <- intersect(c("Author1", "Author2", "Author3", "Author4", "Author5"),
                           names(jn))
  all_authors <- unique(na.omit(unlist(jn[, ..author_cols])))
  all_authors <- all_authors[nchar(trimws(all_authors)) >= 3]
  all_authors <- all_authors[all_authors != "."]

  log_message(paste("Unique authors to query:", length(all_authors)))

  # ---------------------------------------------------------------------------
  # 3.1 REPEC author search function
  # ---------------------------------------------------------------------------

  #' Search REPEC for an author by name
  #' @param author_name Character string (e.g. "John Smith")
  #' @return List with repec_id, first_pub_year, hindex, supervisor_id, match_confidence
  search_repec_author <- function(author_name) {

    na_result <- list(
      repec_id = NA_character_, first_pub_year = NA_integer_,
      hindex = NA_real_, supervisor_id = NA_character_,
      match_confidence = NA_real_
    )

    if (is.na(author_name) || nchar(trimws(author_name)) < 3) return(na_result)

    author_clean <- trimws(author_name)
    author_clean <- gsub("[^a-zA-Z .-]", "", author_clean)

    tryCatch({
      # Step 1: Search for author short-ID
      search_result <- repec::getauthorshortid(author_clean, code = REPEC_API_KEY)
      Sys.sleep(REPEC_DELAY)

      if (is.null(search_result) || !is.data.frame(search_result) ||
          nrow(search_result) == 0) {
        return(na_result)
      }

      # Find best match by name similarity
      name_col <- if ("name" %in% names(search_result)) "name" else {
        char_cols <- names(search_result)[sapply(search_result, is.character)]
        char_cols[!char_cols %in% c("shortid", "short_id")][1]
      }

      id_col <- if ("shortid" %in% names(search_result)) "shortid" else
                if ("short_id" %in% names(search_result)) "short_id" else
                names(search_result)[1]

      if (is.na(name_col)) {
        author_id <- as.character(search_result[[id_col]][1])
        match_conf <- NA_real_
      } else {
        search_result$similarity <- stringdist::stringsim(
          tolower(author_clean),
          tolower(as.character(search_result[[name_col]])),
          method = "jw"
        )
        best_idx <- which.max(search_result$similarity)
        author_id <- as.character(search_result[[id_col]][best_idx])
        match_conf <- search_result$similarity[best_idx]
      }

      if (is.na(author_id) || nchar(author_id) < 2) return(na_result)

      # Step 2: Get h-index
      hindex <- tryCatch({
        h <- repec::gethindex(author_id, code = REPEC_API_KEY)
        Sys.sleep(REPEC_DELAY)
        as.numeric(h)
      }, error = function(e) NA_real_)

      # Step 3: Get first publication year
      first_pub_year <- tryCatch({
        yr <- repec::getfirstpubyear(author_id, code = REPEC_API_KEY)
        Sys.sleep(REPEC_DELAY)
        as.integer(yr)
      }, error = function(e) NA_integer_)

      # Step 4: Get genealogy (supervisor)
      supervisor_id <- tryCatch({
        gen <- repec::getgenealogy(author_id, code = REPEC_API_KEY)
        Sys.sleep(REPEC_DELAY)
        if (is.data.frame(gen) && nrow(gen) > 0) {
          advisor_col <- intersect(names(gen), c("advisor_id", "advisor", "advisorid"))
          if (length(advisor_col) > 0) as.character(gen[[advisor_col[1]]][1]) else NA_character_
        } else if (is.list(gen) && !is.null(gen$advisor_id)) {
          as.character(gen$advisor_id)
        } else {
          NA_character_
        }
      }, error = function(e) NA_character_)

      return(list(
        repec_id = author_id, first_pub_year = first_pub_year,
        hindex = hindex, supervisor_id = supervisor_id,
        match_confidence = match_conf
      ))

    }, error = function(e) {
      return(na_result)
    })
  }

  # ---------------------------------------------------------------------------
  # 3.2 Query all authors
  # ---------------------------------------------------------------------------

  # Resumable via cache file
  repec_cache_file <- file.path(DATA_DIR, "repec_author_data_cache.rds")
  if (file.exists(repec_cache_file)) {
    repec_data <- readRDS(repec_cache_file)
    already_queried <- repec_data$author_name
    authors_to_query <- setdiff(all_authors, already_queried)
    log_message(paste("Resuming — already have:", length(already_queried),
                      ". Remaining:", length(authors_to_query)))
  } else {
    repec_data <- data.table(
      author_name = character(), repec_id = character(),
      first_pub_year = integer(), hindex = numeric(),
      supervisor_id = character(), match_confidence = numeric(),
      query_time = as.POSIXct(character())
    )
    authors_to_query <- all_authors
  }

  if (length(authors_to_query) > 0) {
    log_message(paste("Querying REPEC for", length(authors_to_query), "authors..."))

    for (i in seq_along(authors_to_query)) {
      author <- authors_to_query[i]
      result <- search_repec_author(author)

      new_row <- data.table(
        author_name      = author,
        repec_id         = result$repec_id,
        first_pub_year   = result$first_pub_year,
        hindex           = result$hindex,
        supervisor_id    = result$supervisor_id,
        match_confidence = result$match_confidence,
        query_time       = Sys.time()
      )

      repec_data <- rbindlist(list(repec_data, new_row), use.names = TRUE)

      # Progress + periodic cache save
      if (i %% 50 == 0 || i == length(authors_to_query)) {
        cat("\r  Fetching author", i, "of", length(authors_to_query),
            " — matched:", sum(!is.na(repec_data$repec_id)), "  ")
        saveRDS(repec_data, repec_cache_file)
      }
    }
    cat("\n")
  }

  log_message(paste("REPEC authors matched:", sum(!is.na(repec_data$repec_id)),
                    "/", nrow(repec_data),
                    "(", round(mean(!is.na(repec_data$repec_id)) * 100, 1), "%)"))

  # ---------------------------------------------------------------------------
  # 3.3 Check for prolific supervisors (h-index > 20)
  # ---------------------------------------------------------------------------

  supervisors <- unique(na.omit(repec_data$supervisor_id))

  if (length(supervisors) > 0) {
    log_message(paste("Checking", length(supervisors), "unique supervisors..."))

    supervisor_prolific <- data.table(
      supervisor_id = character(), is_prolific = logical()
    )

    for (sup in supervisors) {
      is_prolific <- tryCatch({
        h <- repec::gethindex(sup, code = REPEC_API_KEY)
        Sys.sleep(REPEC_DELAY)
        h_val <- as.numeric(h)
        if (is.na(h_val)) NA else (h_val > 20)
      }, error = function(e) NA)

      supervisor_prolific <- rbindlist(list(
        supervisor_prolific,
        data.table(supervisor_id = sup, is_prolific = is_prolific)
      ))
    }

    repec_data <- merge(repec_data, supervisor_prolific,
                        by = "supervisor_id", all.x = TRUE)

    log_message(paste("Prolific supervisors found:",
                      sum(supervisor_prolific$is_prolific, na.rm = TRUE)))
  }

  # ---------------------------------------------------------------------------
  # 3.4 Working paper and NBER data
  # ---------------------------------------------------------------------------

  matched_authors <- repec_data[!is.na(repec_id)]

  if (nrow(matched_authors) > 0) {
    log_message(paste("Fetching working paper records for",
                      nrow(matched_authors), "matched authors..."))

    wp_cache_file <- file.path(DATA_DIR, "repec_wp_data_cache.rds")
    if (file.exists(wp_cache_file)) {
      wp_cache <- readRDS(wp_cache_file)
      remaining_wp <- matched_authors[!(repec_id %in% wp_cache$repec_id)]
      log_message(paste("  Resuming — already have:", nrow(wp_cache),
                        ". Remaining:", nrow(remaining_wp)))
    } else {
      wp_cache <- data.table(
        repec_id = character(), n_working_papers = integer(),
        has_nber_wp = logical(), nber_wp_years = character()
      )
      remaining_wp <- matched_authors
    }

    if (nrow(remaining_wp) > 0) {
      for (i in seq_len(nrow(remaining_wp))) {
        author <- remaining_wp[i]

        wp_result <- tryCatch({
          record <- repec::getauthorrecordraw(author$repec_id)

          n_wp <- 0L
          has_nber <- FALSE
          nber_years <- c()

          if (!is.null(record) && is.list(record)) {
            papers_df <- NULL
            if ("paper" %in% names(record) && is.data.frame(record$paper)) {
              papers_df <- record$paper
            } else if ("papers" %in% names(record) && is.data.frame(record$papers)) {
              papers_df <- record$papers
            }

            if (!is.null(papers_df) && nrow(papers_df) > 0) {
              n_wp <- nrow(papers_df)

              handle_col <- intersect(c("handle", "Handle", "id", "ID"), names(papers_df))
              if (length(handle_col) > 0) {
                handles <- papers_df[[handle_col[1]]]
                nber_mask <- grepl("nbr:nberwo|nbr:nberch", handles, ignore.case = TRUE)
                has_nber <- any(nber_mask)

                if (has_nber) {
                  year_col <- intersect(c("year", "Year", "creation-date", "date"),
                                        names(papers_df))
                  if (length(year_col) > 0) {
                    nber_years <- as.integer(substr(papers_df[[year_col[1]]][nber_mask], 1, 4))
                    nber_years <- sort(unique(na.omit(nber_years)))
                  }
                }
              }
            }
          }

          list(n_wp = n_wp, has_nber = has_nber,
               nber_years = paste(nber_years, collapse = ";"))
        }, error = function(e) {
          list(n_wp = 0L, has_nber = FALSE, nber_years = NA_character_)
        })

        wp_cache <- rbindlist(list(
          wp_cache,
          data.table(
            repec_id         = author$repec_id,
            n_working_papers = as.integer(wp_result$n_wp),
            has_nber_wp      = as.logical(wp_result$has_nber),
            nber_wp_years    = wp_result$nber_years
          )
        ), use.names = TRUE, fill = TRUE)

        if (i %% 50 == 0) saveRDS(wp_cache, wp_cache_file)
        Sys.sleep(REPEC_DELAY)
      }
      saveRDS(wp_cache, wp_cache_file)
    }

    # Merge WP data back
    repec_data <- merge(repec_data, wp_cache, by = "repec_id", all.x = TRUE)
    repec_data[is.na(n_working_papers), n_working_papers := 0L]
    repec_data[is.na(has_nber_wp), has_nber_wp := FALSE]

    log_message(paste("Authors with WPs:", sum(repec_data$n_working_papers > 0, na.rm = TRUE),
                      "| NBER:", sum(repec_data$has_nber_wp, na.rm = TRUE)))
  }

  # Save final output
  saveRDS(repec_data, CACHE_REPEC)
  log_message(paste("Saved:", CACHE_REPEC))
}

cat("\n")


# =============================================================================
# SECTION 4: CONFERENCE PROGRAMS
# =============================================================================
#
# Download and parse conference programs for:
#   - EHA (Economic History Association): PDFs, 2006-2024
#   - EHS (Economic History Society): HTML pages, 2003-2024
#
# The EHES biennial meeting is NOT covered: its programmes are not archived in
# a consistently parsable form, so the paper defines conference exposure as
# "presented at EHA or EHS" and states that scope explicitly.
#
# NOTE: the EHA PDF parse below is retained for provenance, but the shipped
# conference_parsed_data.rds uses a hand-transcribed EHA workbook
# (data/raw/Conference_Papers.xlsx, read by conference_data_helpers.R) because
# pdftools garbles the multi-column EHA programmes. EHS comes from the HTML
# parse below.
#
# Extracts: paper titles, authors, affiliations, session info, day, time
#
# Output: data/cache/conference_parsed_data.rds
# =============================================================================

cat("====================================================================\n")
cat("  SECTION 4: CONFERENCE PROGRAMS\n")
cat("====================================================================\n\n")

CACHE_CONF <- file.path(DATA_DIR, "conference_parsed_data.rds")

if (file.exists(CACHE_CONF)) {
  conf_data <- readRDS(CACHE_CONF)
  log_message(paste("Loaded cached conference data:", nrow(conf_data), "papers"))
} else {

  # ---------------------------------------------------------------------------
  # 4.1 Define conference program URLs
  # ---------------------------------------------------------------------------

  # EHA program URLs (2006-2024) — Economic History Association (US)
  eha_programs <- data.table(
    year = 2006:2024,
    conference = "EHA",
    url = c(
      "https://eh.net/wp-content/uploads/2024/03/EHA-2006-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2007-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2008-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2009-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2010-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2011-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2012-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2013-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2014-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2015-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2016-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2017-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2018-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2019-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2020-Program-Virtual.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2021-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2022-Program.pdf",
      "https://eh.net/wp-content/uploads/2024/03/EHA-2023-Program.pdf",
      "https://eh.net/wp-content/uploads/2025/08/2024_EHA_AM_Program_ONLINE.pdf"
    )
  )
  eha_programs[, local_file := file.path(CONF_DIR, paste0("EHA_", year, ".pdf"))]

  # EHS conference page URLs (2003-2024) — Economic History Society (UK/Europe)
  ehs_programs <- data.table(
    year = 2003:2024,
    conference = "EHS",
    url = c(
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2003-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2004-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2005-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2006-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2007-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2008-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2009-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2010-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2011-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2012-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2013-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2014-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2015-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2016-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2017-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2018-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2019-ehs-annual-conference/",
      NA_character_,
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2021-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2022-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2023-ehs-annual-conference/",
      "https://ehs.org.uk/society/resources/ehs-annual-conference-archive/2024-ehs-annual-conference/"
    )
  )
  ehs_programs[, local_file := file.path(CONF_DIR, paste0("EHS_", year, ".html"))]

  # ---------------------------------------------------------------------------
  # 4.2 Download conference programs
  # ---------------------------------------------------------------------------

  log_message("Downloading conference programs...")

  # EHA PDFs
  cat("  EHA Programs:\n")
  for (i in 1:nrow(eha_programs)) {
    if (!file.exists(eha_programs$local_file[i])) {
      cat("    Downloading EHA", eha_programs$year[i], "...")
      tryCatch({
        download.file(eha_programs$url[i], eha_programs$local_file[i],
                      mode = "wb", quiet = TRUE)
        cat(" done\n")
      }, error = function(e) {
        cat(" FAILED:", e$message, "\n")
      })
      Sys.sleep(0.5)
    }
  }

  # EHS HTML pages
  cat("  EHS Programs:\n")
  for (i in 1:nrow(ehs_programs)) {
    if (!is.na(ehs_programs$url[i]) && !file.exists(ehs_programs$local_file[i])) {
      cat("    Fetching EHS", ehs_programs$year[i], "...")
      tryCatch({
        response <- GET(ehs_programs$url[i])
        if (status_code(response) == 200) {
          writeLines(rawToChar(response$content), ehs_programs$local_file[i])
          cat(" done\n")
        } else {
          cat(" FAILED: HTTP", status_code(response), "\n")
        }
      }, error = function(e) {
        cat(" FAILED:", e$message, "\n")
      })
      Sys.sleep(0.5)
    }
  }

  # ---------------------------------------------------------------------------
  # 4.3 Helper functions for parsing
  # ---------------------------------------------------------------------------

  #' Clean and normalize text
  clean_text <- function(x) {
    if (is.na(x) || is.null(x)) return(NA_character_)
    x <- gsub("[\r\n\t]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub("[\u201c\u201d]", '"', x)
    x <- gsub("[\u2018\u2019\u0092]", "'", x)
    x <- gsub("\u2013|\u2014", "-", x)
    if (nchar(x) == 0) return(NA_character_)
    return(x)
  }

  #' Extract session order from time string
  get_session_order <- function(time_str) {
    if (is.na(time_str) || time_str == "") return(NA_integer_)

    hour <- NA_integer_

    match <- regmatches(time_str, regexpr("\\b([0-9]{1,2}):([0-9]{2})", time_str))
    if (length(match) > 0 && nchar(match) > 0) {
      hour <- as.integer(sub(":.*", "", match))
      if (grepl("p\\.?m\\.?", time_str, ignore.case = TRUE) && hour != 12 && hour < 12) {
        hour <- hour + 12
      }
    } else {
      match <- regmatches(time_str, regexpr("\\b([0-9]{4})\\b", time_str))
      if (length(match) > 0 && nchar(match) == 4) {
        hour <- as.integer(substr(match, 1, 2))
      }
    }

    if (is.na(hour)) return(NA_integer_)

    if (hour <= 8)  return(1L)
    if (hour <= 10) return(2L)
    if (hour <= 12) return(3L)
    if (hour <= 14) return(4L)
    if (hour <= 16) return(5L)
    return(6L)
  }

  #' Extract authors and affiliations from author string
  extract_authors_affiliations <- function(author_string) {
    if (is.na(author_string) || author_string == "") {
      return(list(authors = NA_character_, affiliations = NA_character_, author_count = 0L))
    }

    author_string <- clean_text(author_string)

    affil_matches <- gregexpr("\\([^)]+\\)", author_string)[[1]]
    num_affils <- sum(affil_matches > 0)

    institution_keywords <- "University|College|Institute|School|NBER|Federal Reserve|MIT$|UCLA|NYU|LSE|Harvard|Stanford|Yale|Princeton|Berkeley|Oxford|Cambridge|Warwick|Rutgers|Columbia|Chicago|CUNY|SUNY"

    if (num_affils == 0 && grepl(paste0(",\\s*(", institution_keywords, ")"),
                                  author_string, ignore.case = TRUE)) {
      split_pattern <- paste0(",\\s*(?=(", institution_keywords, "))")
      parts <- strsplit(author_string, split_pattern, perl = TRUE)[[1]]

      if (length(parts) >= 2) {
        author_name <- trimws(parts[1])
        affiliation <- trimws(paste(parts[-1], collapse = ", "))
        return(list(authors = author_name, affiliations = affiliation, author_count = 1L))
      }
    }

    authors <- c()
    affiliations <- c()

    if (num_affils <= 1) {
      affil_match <- regmatches(author_string, regexpr("\\(([^)]+)\\)\\s*$", author_string))
      if (length(affil_match) > 0 && nchar(affil_match) > 0) {
        shared_affil <- gsub("^\\(|\\)$", "", affil_match)
        author_part <- sub("\\s*\\([^)]+\\)\\s*$", "", author_string)
      } else {
        shared_affil <- NA_character_
        author_part <- author_string
      }

      author_list <- strsplit(author_part,
        "\\s+and\\s+|\\s*&\\s*|,\\s+and\\s+|,\\s+(?=[A-Z])", perl = TRUE)[[1]]
      author_list <- trimws(author_list)
      author_list <- author_list[nchar(author_list) > 0]

      for (a in author_list) {
        if (grepl("\\([^)]+\\)$", a)) {
          a_affil <- gsub(".*\\(([^)]+)\\)$", "\\1", a)
          a_name <- sub("\\s*\\([^)]+\\)$", "", a)
          authors <- c(authors, trimws(a_name))
          affiliations <- c(affiliations, trimws(a_affil))
        } else {
          authors <- c(authors, trimws(a))
          affiliations <- c(affiliations, shared_affil)
        }
      }
    } else {
      pattern <- "([A-Za-z][^(]+?)\\s*\\(([^)]+)\\)"
      matches <- gregexpr(pattern, author_string, perl = TRUE)

      if (matches[[1]][1] > 0) {
        match_data <- regmatches(author_string, matches)[[1]]
        for (m in match_data) {
          name_part <- sub("\\s*\\([^)]+\\)$", "", m)
          affil_part <- gsub(".*\\(([^)]+)\\)$", "\\1", m)
          name_part <- gsub("^\\s*(,|and|&)\\s*", "", name_part)
          name_part <- gsub("\\s*(,|and|&)\\s*$", "", name_part)
          if (nchar(trimws(name_part)) > 1) {
            authors <- c(authors, trimws(name_part))
            affiliations <- c(affiliations, trimws(affil_part))
          }
        }
      }
    }

    authors <- authors[!is.na(authors) & nchar(authors) > 1]
    affiliations <- affiliations[seq_along(authors)]

    if (length(authors) == 0) {
      return(list(authors = author_string, affiliations = NA_character_, author_count = 1L))
    }

    return(list(
      authors      = paste(authors, collapse = "; "),
      affiliations = paste(affiliations, collapse = "; "),
      author_count = length(authors)
    ))
  }

  # ---------------------------------------------------------------------------
  # 4.4 Parse EHA PDF programs
  # ---------------------------------------------------------------------------

  parse_eha_pdf <- function(pdf_file, conf_year) {

    tryCatch({
      pdf_text_raw <- pdf_text(pdf_file)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(pdf_text_raw) || length(pdf_text_raw) == 0) return(NULL)

    full_text <- paste(pdf_text_raw, collapse = "\n")

    # Find Paper Abstracts section
    abstract_start <- regexpr("Paper Abstracts", full_text, ignore.case = TRUE)
    if (abstract_start > 0) {
      full_text <- substring(full_text, abstract_start)
    } else {
      schedule_start <- regexpr("Detailed Schedule", full_text, ignore.case = TRUE)
      if (schedule_start > 0) full_text <- substring(full_text, schedule_start)
    }

    lines <- strsplit(full_text, "\n")[[1]]
    lines <- trimws(lines)

    current_day <- NA_character_
    current_time <- NA_character_
    current_session <- NA_character_

    papers <- list()

    day_pattern <- "\\b(Friday|Saturday|Sunday|Thursday)\\b.*\\b(September|October|November|April|March)\\b"
    time_pattern <- "\\b([0-9]{1,2}:[0-9]{2})\\s*(a\\.?m\\.?|p\\.?m\\.?)?|\\b([0-9]{4})-([0-9]{4})\\b"
    author_affil_pattern <- "^[A-Z][^(]{3,}\\([^)]+\\)"
    alt_author_pattern <- "^[A-Z][A-Za-z\\.\\s]+,\\s*(University|College|Institute|School|NBER|Federal Reserve|MIT|UCLA|NYU|LSE|Harvard|Stanford|Yale|Princeton|Berkeley|Oxford|Cambridge)"
    numbered_paper_pattern <- "^[0-9]+\\.\\s+[A-Z][A-Za-z\\s\\.]+,"

    i <- 1
    while (i <= length(lines)) {
      line <- lines[i]

      if (nchar(line) < 3) { i <- i + 1; next }
      if (grepl("^(Discussant|Chair|Discussion)s?:", line, ignore.case = TRUE)) { i <- i + 1; next }
      if (grepl("^[0-9]+$", line) || grepl("^Paper Abstracts$", line, ignore.case = TRUE)) { i <- i + 1; next }

      # Detect day
      if (grepl(day_pattern, line, ignore.case = TRUE)) {
        day_match <- regmatches(line, regexpr("\\b(Friday|Saturday|Sunday|Thursday)\\b",
                                               line, ignore.case = TRUE))
        if (length(day_match) > 0) current_day <- tools::toTitleCase(tolower(day_match))
        i <- i + 1; next
      }

      # Detect time
      time_match <- regmatches(line, regexpr(time_pattern, line, ignore.case = TRUE))
      if (length(time_match) > 0 && nchar(time_match) > 0) current_time <- time_match

      # Detect session
      if (grepl("^Session\\s*:?\\s*[0-9]|^[0-9]{1,2}[\\.:]\\s+[A-Z]", line, ignore.case = TRUE)) {
        current_session <- clean_text(line)
      }

      # Numbered paper entries (EHA 2020 virtual format)
      if (grepl(numbered_paper_pattern, line) && nchar(line) > 20) {
        author_part <- sub("^[0-9]+\\.\\s*", "", line)
        title_part <- NA_character_

        j <- i + 1
        while (j <= min(i + 5, length(lines))) {
          next_line <- trimws(lines[j])
          if (grepl('^"', next_line) || grepl('^\u201c', next_line)) {
            title_part <- gsub('^["\u201c]', '', next_line)
            title_part <- gsub('["\u201d]$', '', title_part)
            if (!grepl('["\u201d]', next_line)) {
              k <- j + 1
              while (k <= min(j + 3, length(lines))) {
                cont_line <- trimws(lines[k])
                if (grepl('["\u201d]', cont_line)) {
                  title_part <- paste(title_part, gsub('["\u201d].*$', '', cont_line))
                  break
                } else if (grepl("^Discussant:", cont_line, ignore.case = TRUE)) {
                  break
                } else {
                  title_part <- paste(title_part, cont_line)
                }
                k <- k + 1
              }
            }
            break
          } else if (grepl("^Discussant:", next_line, ignore.case = TRUE)) {
            break
          }
          j <- j + 1
        }

        author_part <- clean_text(author_part)
        title_part <- clean_text(title_part)

        author_segments <- strsplit(author_part, ";")[[1]]
        authors <- c()
        affils <- c()

        for (seg in author_segments) {
          seg <- trimws(seg)
          if (nchar(seg) > 3) {
            inst_match <- regexpr(",\\s*(University|College|Institute|School|NBER|Federal Reserve|MIT|UCLA|Harvard|Stanford|Princeton|Sciences Po|LSE|Oxford|Cambridge|Board of Governors)",
                                  seg, ignore.case = TRUE)
            if (inst_match > 0) {
              authors <- c(authors, trimws(substring(seg, 1, inst_match - 1)))
              affils <- c(affils, trimws(gsub("^,\\s*", "", substring(seg, inst_match + 1))))
            } else {
              authors <- c(authors, seg)
              affils <- c(affils, NA_character_)
            }
          }
        }

        if (length(authors) > 0) {
          papers[[length(papers) + 1]] <- data.table(
            conference = "EHA", year = conf_year, day = current_day,
            time = current_time, session = current_session,
            session_order = get_session_order(current_time),
            title = title_part,
            authors = paste(authors, collapse = "; "),
            affiliations = paste(affils, collapse = "; "),
            author_count = length(authors), raw_text = line
          )
        }
        i <- i + 1; next
      }

      # Standard author-affiliation pattern
      if ((grepl(author_affil_pattern, line, perl = TRUE) ||
           grepl(alt_author_pattern, line, ignore.case = TRUE)) && nchar(line) > 25) {

        if (grepl("^(Discussant|Chair)", line, ignore.case = TRUE)) { i <- i + 1; next }

        if (grepl(',\\s*"', line)) {
          parts <- strsplit(line, ',\\s*"', perl = TRUE)[[1]]
          author_part <- parts[1]
          title_part <- if (length(parts) > 1) gsub('".*$', '', parts[2]) else NA_character_
        } else if (grepl("\\),\\s*[A-Z\u201c]", line)) {
          split_pos <- regexpr("\\),\\s*", line)
          if (split_pos > 0) {
            author_part <- substring(line, 1, split_pos)
            title_part <- substring(line, split_pos + attr(split_pos, "match.length"))
            title_part <- gsub('^["\u201c]|["\u201d]$', '', title_part)
          } else {
            author_part <- line; title_part <- NA_character_
          }
        } else {
          author_part <- line; title_part <- NA_character_

          # Look backward for title
          if (i > 1) {
            prev_line <- trimws(lines[i - 1])
            if (nchar(prev_line) > 15 &&
                !grepl(author_affil_pattern, prev_line, perl = TRUE) &&
                !grepl(alt_author_pattern, prev_line, ignore.case = TRUE) &&
                !grepl("^(Discussant|Chair|Session|[0-9])", prev_line, ignore.case = TRUE) &&
                !grepl("^[A-Z]\\.", prev_line) &&
                !grepl("p\\.m\\.|a\\.m\\.", prev_line, ignore.case = TRUE)) {
              title_part <- clean_text(prev_line)
            }
          }

          # Look forward for title
          if (is.na(title_part)) {
            j <- i + 1
            while (j <= min(i + 3, length(lines))) {
              next_line <- trimws(lines[j])
              if (nchar(next_line) > 20 &&
                  !grepl("^(Discussant|Chair)", next_line, ignore.case = TRUE) &&
                  !grepl(author_affil_pattern, next_line, perl = TRUE)) {
                if (grepl('^"', next_line) || grepl('^\u201c', next_line)) {
                  title_part <- gsub('^["\u201c]|["\u201d].*$', '', next_line)
                  if (grepl('["\u201d]', lines[j])) {
                    title_end <- regexpr('["\u201d]', lines[j])
                    title_part <- substring(lines[j], 2, title_end - 1)
                  }
                } else if (!grepl("^[0-9]|^Session|Chair|Discussant", next_line, ignore.case = TRUE)) {
                  title_part <- next_line
                }
                break
              }
              j <- j + 1
            }
          }
        }

        author_part <- clean_text(author_part)
        title_part <- clean_text(title_part)
        author_part <- gsub(",\\s*$", "", author_part)

        author_info <- extract_authors_affiliations(author_part)

        if (!is.na(author_info$authors) && nchar(author_info$authors) > 2) {
          papers[[length(papers) + 1]] <- data.table(
            conference = "EHA", year = conf_year, day = current_day,
            time = current_time, session = current_session,
            session_order = get_session_order(current_time),
            title = title_part,
            authors = author_info$authors,
            affiliations = author_info$affiliations,
            author_count = author_info$author_count,
            raw_text = line
          )
        }
      }

      i <- i + 1
    }

    if (length(papers) == 0) return(NULL)
    rbindlist(papers, fill = TRUE)
  }

  # ---------------------------------------------------------------------------
  # 4.5 Parse EHS HTML programs
  # ---------------------------------------------------------------------------

  parse_ehs_html <- function(html_file, conf_year) {

    tryCatch({
      doc <- read_html(html_file, encoding = "UTF-8")
    }, error = function(e) {
      return(NULL)
    })

    # Check for linked programme pages
    links <- html_nodes(doc, "a")
    programme_link <- NULL

    for (link in links) {
      link_text <- html_text(link, trim = TRUE)
      link_href <- html_attr(link, "href")
      if (!is.na(link_text) && !is.na(link_href) &&
          grepl("programme|program", link_text, ignore.case = TRUE) &&
          grepl("ehs.org.uk", link_href) &&
          !grepl("\\.pdf$", link_href, ignore.case = TRUE)) {
        programme_link <- link_href
        break
      }
    }

    if (!is.null(programme_link)) {
      tryCatch({
        programme_doc <- read_html(programme_link, encoding = "UTF-8")
        prog_paragraphs <- html_nodes(programme_doc, "p")
        paper_paras <- sapply(prog_paragraphs, function(p) {
          grepl("<em>|<i>", as.character(p), ignore.case = TRUE)
        })
        if (sum(paper_paras) > 10) doc <- programme_doc
      }, error = function(e) NULL)
    }

    paragraphs <- html_nodes(doc, "p")
    if (length(paragraphs) == 0) return(NULL)

    current_day <- NA_character_
    current_time <- NA_character_
    current_session <- NA_character_

    papers <- list()

    for (p in paragraphs) {
      p_text <- html_text(p, trim = TRUE)
      p_html <- as.character(p)

      if (is.na(p_text) || nchar(p_text) < 5) next
      if (grepl("^\\([A-Z]+/[0-9]+/[0-9]+[a-z]?\\)$", p_text)) next

      # Detect day
      if (grepl("\\b(Friday|Saturday|Sunday)\\b.*\\b(April|March|September)\\b.*[0-9]{4}",
                p_text, ignore.case = TRUE)) {
        day_match <- regmatches(p_text, regexpr("\\b(Friday|Saturday|Sunday)\\b",
                                                 p_text, ignore.case = TRUE))
        if (length(day_match) > 0) current_day <- tools::toTitleCase(tolower(day_match))
        next
      }

      # Detect time
      time_match <- regmatches(p_text,
        regexpr("\\b[0-9]{4}-[0-9]{4}\\b|\\b[0-9]{1,2}:[0-9]{2}\\s*-\\s*[0-9]{1,2}:[0-9]{2}", p_text))
      if (length(time_match) > 0 && nchar(time_match) > 0) current_time <- time_match

      # Detect session header
      if (grepl("<strong>|<b>", p_html, ignore.case = TRUE)) {
        if (grepl("^[A-Z]{2,}[0-9]?[A-Z]?:|\\bSession\\b|\\bAcademic Session\\b",
                  p_text, ignore.case = TRUE)) {
          current_session <- clean_text(p_text)
          current_session <- gsub("\\s*\\([A-Z]+/[0-9]+/[0-9]+[a-z]?\\)\\s*", "", current_session)
          next
        }
      }

      if (grepl("^\\s*\\(chair:", p_text, ignore.case = TRUE)) next

      # Detect paper: look for <em> or <i> tags (titles in EHS format)
      if (grepl("<em>|<i>", p_html, ignore.case = TRUE)) {
        title_nodes <- html_nodes(p, "em, i")

        if (length(title_nodes) > 0) {
          title_text <- html_text(title_nodes[[1]], trim = TRUE)

          if (grepl("^\\([A-Z]+/[0-9]+/[0-9]+[a-z]?\\)$", title_text)) next
          if (grepl("^chair:", title_text, ignore.case = TRUE)) next

          title_text <- clean_text(title_text)
          if (is.na(title_text) || nchar(title_text) < 10) next

          # Get author from paragraph (text minus title)
          author_text <- gsub(title_text, "", p_text, fixed = TRUE)
          author_text <- clean_text(author_text)
          author_text <- gsub("^[\\s,;:]+|[\\s,;:]+$", "", author_text)
          author_text <- gsub("\\s*\\(chair:\\s*[^)]+\\)\\s*$", "", author_text,
                              ignore.case = TRUE)

          # If no author text, look at next paragraph
          if (is.na(author_text) || nchar(trimws(author_text)) < 3) {
            p_idx <- which(sapply(paragraphs, function(x) identical(x, p)))
            if (length(p_idx) > 0 && p_idx[1] < length(paragraphs)) {
              next_p <- paragraphs[[p_idx[1] + 1]]
              next_p_text <- html_text(next_p, trim = TRUE)
              if (!is.na(next_p_text) && grepl("\\([^)]+\\)", next_p_text) &&
                  !grepl("<em>|<i>", as.character(next_p))) {
                author_text <- clean_text(next_p_text)
                author_text <- gsub("\\s*\\(chair:\\s*[^)]+\\)\\s*$", "",
                                    author_text, ignore.case = TRUE)
              }
            }
          }

          if (!is.na(author_text) && nchar(author_text) > 0 &&
              grepl("\\([^)]+\\)\\s*$", author_text)) {
            author_info <- extract_authors_affiliations(author_text)
          } else if (!is.na(author_text) && nchar(author_text) > 3) {
            author_info <- list(authors = author_text, affiliations = NA_character_,
                                author_count = 1L)
          } else {
            author_info <- list(authors = NA_character_, affiliations = NA_character_,
                                author_count = 0L)
          }

          if (!is.na(title_text) && nchar(title_text) >= 10) {
            papers[[length(papers) + 1]] <- data.table(
              conference = "EHS", year = conf_year, day = current_day,
              time = current_time, session = current_session,
              session_order = get_session_order(current_time),
              title = title_text,
              authors = author_info$authors,
              affiliations = author_info$affiliations,
              author_count = author_info$author_count,
              raw_text = clean_text(p_text)
            )
          }
        }
      }
    }

    if (length(papers) == 0) return(NULL)
    result <- rbindlist(papers, fill = TRUE)
    result <- unique(result, by = c("year", "title"))
    result
  }

  # ---------------------------------------------------------------------------
  # 4.6 Parse all conference programs
  # ---------------------------------------------------------------------------

  all_conf_papers <- list()

  log_message("Parsing EHA programs...")
  for (i in 1:nrow(eha_programs)) {
    yr <- eha_programs$year[i]
    pdf_file <- eha_programs$local_file[i]

    if (file.exists(pdf_file)) {
      cat("  EHA", yr, "...")
      result <- parse_eha_pdf(pdf_file, yr)
      if (!is.null(result) && nrow(result) > 0) {
        all_conf_papers[[paste0("EHA_", yr)]] <- result
        cat(" found", nrow(result), "entries\n")
      } else {
        cat(" no papers found\n")
      }
    }
  }

  log_message("Parsing EHS programs...")
  for (i in 1:nrow(ehs_programs)) {
    yr <- ehs_programs$year[i]
    html_file <- ehs_programs$local_file[i]

    if (!is.na(ehs_programs$url[i]) && file.exists(html_file)) {
      cat("  EHS", yr, "...")
      result <- parse_ehs_html(html_file, yr)
      if (!is.null(result) && nrow(result) > 0) {
        all_conf_papers[[paste0("EHS_", yr)]] <- result
        cat(" found", nrow(result), "entries\n")
      } else {
        cat(" no papers found\n")
      }
    }
  }

  # Combine all results
  if (length(all_conf_papers) > 0) {
    conf_data <- rbindlist(all_conf_papers, fill = TRUE)
    log_message(paste("Total conference papers extracted:", nrow(conf_data)))
    cat("  EHA:", nrow(conf_data[conference == "EHA"]), "\n")
    cat("  EHS:", nrow(conf_data[conference == "EHS"]), "\n")
  } else {
    conf_data <- data.table()
    log_message("WARNING: No conference papers extracted.")
  }

  saveRDS(conf_data, CACHE_CONF)
  log_message(paste("Saved:", CACHE_CONF))
}

cat("\n")


# =============================================================================
# SECTION 5: PRIZE DATA
# =============================================================================
#
# Scrape prize winner lists from economic history society websites:
#   Paper prizes:
#     - Cole Prize (JEH best article, eh.net)
#     - Ashton Prize (EHR best article, ehs.org.uk)
#     - Figuerola Prize (EREH best article, uc3m.es)
#   Dissertation prizes:
#     - Gerschenkron Prize (EHA best dissertation, eh.net)
#     - Nevins Prize (EHA best dissertation, eh.net)
#
# Outputs:
#   data/prize_paper_data.rds        — paper prize winners
#   data/prize_dissertation_data.rds — dissertation prize winners
# =============================================================================

cat("====================================================================\n")
cat("  SECTION 5: PRIZE DATA\n")
cat("====================================================================\n\n")

CACHE_PRIZE_PAPERS <- file.path(DATA_DIR, "prize_paper_data.rds")
CACHE_PRIZE_DISS   <- file.path(DATA_DIR, "prize_dissertation_data.rds")

if (file.exists(CACHE_PRIZE_PAPERS) && file.exists(CACHE_PRIZE_DISS)) {
  prize_papers <- readRDS(CACHE_PRIZE_PAPERS)
  prize_dissertations <- readRDS(CACHE_PRIZE_DISS)
  log_message(paste("Loaded cached prize data:",
                    nrow(prize_papers), "paper prizes,",
                    nrow(prize_dissertations), "dissertation prizes"))
} else {

  # ---------------------------------------------------------------------------
  # 5.0 Helper: safely read an HTML page with retry
  # ---------------------------------------------------------------------------

  safe_read_html <- function(url, max_retries = 3) {
    for (attempt in 1:max_retries) {
      result <- tryCatch({
        Sys.sleep(1)
        read_html(url)
      }, error = function(e) {
        if (attempt < max_retries) {
          cat("  Retry", attempt, "for", url, "\n")
          Sys.sleep(2)
        }
        NULL
      })
      if (!is.null(result)) return(result)
    }
    return(NULL)
  }

  clean_prize_text <- function(x) {
    if (is.na(x) || is.null(x)) return(NA_character_)
    x <- gsub("[\r\n\t]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub("^\"|\"$", "", x)
    x <- gsub("^\u201c|\u201d$", "", x)
    x
  }

  # ---------------------------------------------------------------------------
  # 5.1 Scrape paper prizes
  # ---------------------------------------------------------------------------

  prize_papers <- data.table(
    prize_name = character(), paper_title = character(),
    authors = character(), journal = character(),
    prize_year = integer(), source_url = character()
  )

  # --- Cole Prize (JEH) ---
  cat("Scraping Cole Prize (JEH)...\n")
  cole_url <- "https://eh.net/cole-annual-prize/"
  cole_doc <- safe_read_html(cole_url)

  if (!is.null(cole_doc)) {
    cole_content <- html_nodes(cole_doc, "div.entry-content, article, .post-content, main")
    if (length(cole_content) == 0) cole_content <- html_nodes(cole_doc, "body")

    cole_text <- html_text(cole_content, trim = TRUE)
    cole_text <- paste(cole_text, collapse = "\n")
    cole_lines <- strsplit(cole_text, "\n")[[1]]
    cole_lines <- trimws(cole_lines)
    cole_lines <- cole_lines[nchar(cole_lines) > 0]

    current_year <- NA_integer_
    for (line in cole_lines) {
      year_match <- regmatches(line, regexpr("^(19[0-9]{2}|20[0-9]{2})\\b", line))
      if (length(year_match) > 0) {
        current_year <- as.integer(year_match)
        line <- trimws(sub("^[0-9]{4}[:\\s]*", "", line))
      }

      if (!is.na(current_year) && nchar(line) > 10) {
        title_match <- regmatches(line,
          regexpr("\u201c[^\u201d]+\u201d|\"[^\"]+\"|'[^']+'", line))

        if (length(title_match) > 0) {
          paper_title <- gsub("[\u201c\u201d\"']", "", title_match[1])
          author_part <- trimws(sub(title_match[1], "", line, fixed = TRUE))
          author_part <- gsub("[,;\\s]+$", "", author_part)
          author_part <- gsub("^[,;\\s]+", "", author_part)
        } else {
          paper_title <- clean_prize_text(line)
          author_part <- NA_character_
        }

        if (!is.na(paper_title) && nchar(paper_title) > 5) {
          prize_papers <- rbindlist(list(prize_papers, data.table(
            prize_name = "Cole", paper_title = clean_prize_text(paper_title),
            authors = clean_prize_text(author_part), journal = "JEH",
            prize_year = current_year, source_url = cole_url
          )), use.names = TRUE, fill = TRUE)
        }
      }
    }
    cat("  Cole Prize entries found:", nrow(prize_papers[prize_name == "Cole"]), "\n")
  } else {
    cat("  WARNING: Could not fetch Cole Prize page\n")
  }

  # --- Ashton Prize (EHR) ---
  cat("Scraping Ashton Prize (EHR)...\n")
  ashton_url <- "https://ehs.org.uk/award/t-s-ashton-prize/"
  ashton_doc <- safe_read_html(ashton_url)

  if (!is.null(ashton_doc)) {
    ashton_content <- html_nodes(ashton_doc, "div.entry-content, article, .post-content, main")
    if (length(ashton_content) == 0) ashton_content <- html_nodes(ashton_doc, "body")

    ashton_text <- html_text(ashton_content, trim = TRUE)
    ashton_text <- paste(ashton_text, collapse = "\n")
    ashton_lines <- strsplit(ashton_text, "\n")[[1]]
    ashton_lines <- trimws(ashton_lines)
    ashton_lines <- ashton_lines[nchar(ashton_lines) > 0]

    current_year <- NA_integer_
    for (line in ashton_lines) {
      year_match <- regmatches(line, regexpr("^(19[0-9]{2}|20[0-9]{2})\\b", line))
      if (length(year_match) > 0) {
        current_year <- as.integer(year_match)
        line <- trimws(sub("^[0-9]{4}[:\\s]*", "", line))
      }

      if (!is.na(current_year) && nchar(line) > 10) {
        title_match <- regmatches(line,
          regexpr("\u201c[^\u201d]+\u201d|\"[^\"]+\"|'[^']+'", line))

        if (length(title_match) > 0) {
          paper_title <- gsub("[\u201c\u201d\"']", "", title_match[1])
          author_part <- trimws(sub(title_match[1], "", line, fixed = TRUE))
          author_part <- gsub("[,;\\s]+$", "", author_part)
          author_part <- gsub("^[,;\\s]+", "", author_part)
        } else {
          paper_title <- clean_prize_text(line)
          author_part <- NA_character_
        }

        if (!is.na(paper_title) && nchar(paper_title) > 5) {
          prize_papers <- rbindlist(list(prize_papers, data.table(
            prize_name = "Ashton", paper_title = clean_prize_text(paper_title),
            authors = clean_prize_text(author_part), journal = "EHR",
            prize_year = current_year, source_url = ashton_url
          )), use.names = TRUE, fill = TRUE)
        }
      }
    }
    cat("  Ashton Prize entries found:", nrow(prize_papers[prize_name == "Ashton"]), "\n")
  } else {
    cat("  WARNING: Could not fetch Ashton Prize page\n")
  }

  # --- Figuerola Prize (EREH) ---
  cat("Scraping Figuerola Prize (EREH)...\n")
  figuerola_url <- "https://www.uc3m.es/ss/Satellite/Figuerola/en/TextoMixta/1371326205617/"
  figuerola_doc <- safe_read_html(figuerola_url)

  if (!is.null(figuerola_doc)) {
    fig_content <- html_nodes(figuerola_doc, "div.contenido, div.entry-content, article, main")
    if (length(fig_content) == 0) fig_content <- html_nodes(figuerola_doc, "body")

    fig_text <- html_text(fig_content, trim = TRUE)
    fig_text <- paste(fig_text, collapse = "\n")
    fig_lines <- strsplit(fig_text, "\n")[[1]]
    fig_lines <- trimws(fig_lines)
    fig_lines <- fig_lines[nchar(fig_lines) > 0]

    current_year <- NA_integer_
    for (line in fig_lines) {
      year_match <- regmatches(line, regexpr("(19[0-9]{2}|20[0-9]{2})", line))
      if (length(year_match) > 0 && grepl("^[0-9]{4}|[Pp]rize.*[0-9]{4}|[Aa]ward.*[0-9]{4}", line)) {
        current_year <- as.integer(year_match[1])
        line <- trimws(gsub("[0-9]{4}", "", line))
      }

      if (!is.na(current_year) && nchar(line) > 10 &&
          !grepl("^(The |This |About|Contact|Copyright|Home)", line, ignore.case = TRUE)) {
        title_match <- regmatches(line,
          regexpr("\u201c[^\u201d]+\u201d|\"[^\"]+\"|'[^']+'", line))

        if (length(title_match) > 0) {
          paper_title <- gsub("[\u201c\u201d\"']", "", title_match[1])
          author_part <- trimws(sub(title_match[1], "", line, fixed = TRUE))
          author_part <- gsub("[,;\\s]+$", "", author_part)
          author_part <- gsub("^[,;\\s]+", "", author_part)
        } else {
          paper_title <- clean_prize_text(line)
          author_part <- NA_character_
        }

        if (!is.na(paper_title) && nchar(paper_title) > 5) {
          prize_papers <- rbindlist(list(prize_papers, data.table(
            prize_name = "Figuerola", paper_title = clean_prize_text(paper_title),
            authors = clean_prize_text(author_part), journal = "EREH",
            prize_year = current_year, source_url = figuerola_url
          )), use.names = TRUE, fill = TRUE)
        }
      }
    }
    cat("  Figuerola Prize entries found:", nrow(prize_papers[prize_name == "Figuerola"]), "\n")
  } else {
    cat("  WARNING: Could not fetch Figuerola Prize page\n")
  }

  cat("\nTotal paper prize entries:", nrow(prize_papers), "\n\n")

  # ---------------------------------------------------------------------------
  # 5.2 Scrape dissertation prizes
  # ---------------------------------------------------------------------------

  prize_dissertations <- data.table(
    prize_name = character(), recipient = character(),
    dissertation_title = character(), prize_year = integer(),
    source_url = character()
  )

  # --- Gerschenkron Prize ---
  cat("Scraping Gerschenkron Prize...\n")
  gersch_url <- "https://eh.net/gerschenkron-prize-2/"
  gersch_doc <- safe_read_html(gersch_url)

  if (!is.null(gersch_doc)) {
    gersch_content <- html_nodes(gersch_doc, "div.entry-content, article, .post-content, main")
    if (length(gersch_content) == 0) gersch_content <- html_nodes(gersch_doc, "body")

    gersch_text <- html_text(gersch_content, trim = TRUE)
    gersch_text <- paste(gersch_text, collapse = "\n")
    gersch_lines <- strsplit(gersch_text, "\n")[[1]]
    gersch_lines <- trimws(gersch_lines)
    gersch_lines <- gersch_lines[nchar(gersch_lines) > 0]

    current_year <- NA_integer_
    for (line in gersch_lines) {
      year_match <- regmatches(line, regexpr("^(19[0-9]{2}|20[0-9]{2})\\b", line))
      if (length(year_match) > 0) {
        current_year <- as.integer(year_match)
        line <- trimws(sub("^[0-9]{4}[:\\s\u2013-]*", "", line))
      }

      if (!is.na(current_year) && nchar(line) > 5) {
        title_match <- regmatches(line,
          regexpr("\u201c[^\u201d]+\u201d|\"[^\"]+\"|'[^']+'", line))

        if (length(title_match) > 0) {
          diss_title <- gsub("[\u201c\u201d\"']", "", title_match[1])
          recipient <- trimws(sub(title_match[1], "", line, fixed = TRUE))
          recipient <- gsub("[,;\\s]+$", "", recipient)
          recipient <- gsub("^[,;\\s]+", "", recipient)
        } else {
          recipient <- clean_prize_text(line)
          diss_title <- NA_character_
        }

        if (!is.na(recipient) && nchar(recipient) > 3 &&
            !grepl("^(The |This |About|Copyright)", recipient, ignore.case = TRUE)) {
          prize_dissertations <- rbindlist(list(prize_dissertations, data.table(
            prize_name = "Gerschenkron", recipient = clean_prize_text(recipient),
            dissertation_title = clean_prize_text(diss_title),
            prize_year = current_year, source_url = gersch_url
          )), use.names = TRUE, fill = TRUE)
        }
      }
    }
    cat("  Gerschenkron Prize entries found:",
        nrow(prize_dissertations[prize_name == "Gerschenkron"]), "\n")
  } else {
    cat("  WARNING: Could not fetch Gerschenkron Prize page\n")
  }

  # --- Nevins Prize ---
  cat("Scraping Nevins Prize...\n")
  nevins_url <- "https://eh.net/nevins-prize/"
  nevins_doc <- safe_read_html(nevins_url)

  if (!is.null(nevins_doc)) {
    nevins_content <- html_nodes(nevins_doc, "div.entry-content, article, .post-content, main")
    if (length(nevins_content) == 0) nevins_content <- html_nodes(nevins_doc, "body")

    nevins_text <- html_text(nevins_content, trim = TRUE)
    nevins_text <- paste(nevins_text, collapse = "\n")
    nevins_lines <- strsplit(nevins_text, "\n")[[1]]
    nevins_lines <- trimws(nevins_lines)
    nevins_lines <- nevins_lines[nchar(nevins_lines) > 0]

    current_year <- NA_integer_
    for (line in nevins_lines) {
      year_match <- regmatches(line, regexpr("^(19[0-9]{2}|20[0-9]{2})\\b", line))
      if (length(year_match) > 0) {
        current_year <- as.integer(year_match)
        line <- trimws(sub("^[0-9]{4}[:\\s\u2013-]*", "", line))
      }

      if (!is.na(current_year) && nchar(line) > 5) {
        title_match <- regmatches(line,
          regexpr("\u201c[^\u201d]+\u201d|\"[^\"]+\"|'[^']+'", line))

        if (length(title_match) > 0) {
          diss_title <- gsub("[\u201c\u201d\"']", "", title_match[1])
          recipient <- trimws(sub(title_match[1], "", line, fixed = TRUE))
          recipient <- gsub("[,;\\s]+$", "", recipient)
          recipient <- gsub("^[,;\\s]+", "", recipient)
        } else {
          recipient <- clean_prize_text(line)
          diss_title <- NA_character_
        }

        if (!is.na(recipient) && nchar(recipient) > 3 &&
            !grepl("^(The |This |About|Copyright)", recipient, ignore.case = TRUE)) {
          prize_dissertations <- rbindlist(list(prize_dissertations, data.table(
            prize_name = "Nevins", recipient = clean_prize_text(recipient),
            dissertation_title = clean_prize_text(diss_title),
            prize_year = current_year, source_url = nevins_url
          )), use.names = TRUE, fill = TRUE)
        }
      }
    }
    cat("  Nevins Prize entries found:",
        nrow(prize_dissertations[prize_name == "Nevins"]), "\n")
  } else {
    cat("  WARNING: Could not fetch Nevins Prize page\n")
  }

  cat("\nTotal dissertation prize entries:", nrow(prize_dissertations), "\n")

  # Save outputs
  saveRDS(prize_papers, CACHE_PRIZE_PAPERS)
  saveRDS(prize_dissertations, CACHE_PRIZE_DISS)
  log_message(paste("Saved:", CACHE_PRIZE_PAPERS))
  log_message(paste("Saved:", CACHE_PRIZE_DISS))
}

cat("\n")


# =============================================================================
# SUMMARY
# =============================================================================

elapsed <- round(difftime(Sys.time(), script_start, units = "mins"), 1)

cat("====================================================================\n")
cat("  DATA COLLECTION COMPLETE\n")
cat("====================================================================\n\n")

cat("Elapsed time:", elapsed, "minutes\n\n")

cat("Output files in", DATA_DIR, "/:\n")

output_files <- c(
  CACHE_MATCHES, CACHE_CITATIONS, CACHE_TOPICS,
  CACHE_REPEC, CACHE_CONF, CACHE_PRIZE_PAPERS, CACHE_PRIZE_DISS
)

for (f in output_files) {
  if (file.exists(f)) {
    sz <- round(file.size(f) / 1024^2, 1)
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    n_rows <- if (is.data.frame(obj) || is.data.table(obj)) nrow(obj) else "?"
    cat("  ", basename(f), " — ", n_rows, " rows (", sz, " MB)\n", sep = "")
  } else {
    cat("  ", basename(f), " — NOT FOUND\n", sep = "")
  }
}

cat("\nNext step: Run 01_data_preparation.R\n")
cat("====================================================================\n")

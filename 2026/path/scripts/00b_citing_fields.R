# =============================================================================
# 00b_citing_fields.R -- discipline classification of all citing works
# -----------------------------------------------------------------------------
# Queries OpenAlex for every unique citing work in the citation-link cache and
# classifies it by DISCIPLINE, from three angles:
#   A) modern curated taxonomy: primary_topic.field / .subfield / .domain
#   B) level-0 concept ancestors (the 19 OpenAlex root fields)
#   C) document type (article vs preprint/dissertation/working paper) + venue
#
# Classifying citers by their single top concept -- as in an earlier draft --
# badly under-counts economics, because top concepts are mostly topic-level
# labels ("Inequality", "Human capital") or noise. The level-0/primary-field
# classification used here is what scripts 06 and 10 consume.
#
# Output: data/cache/citing_field_data.rds (~30-60 minutes on the polite pool).
# NOT needed to reproduce the paper: the cache is shipped.
# =============================================================================

suppressMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
})

SCRIPT_DIR <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
DATA_DIR <- normalizePath(file.path(SCRIPT_DIR, "..", "data", "cache"), mustWork = FALSE)

# Credentials from environment variables (see README.md); never hard-code keys.
OPENALEX_EMAIL   <- Sys.getenv("OPENALEX_EMAIL")
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY")   # optional
if (!nzchar(OPENALEX_EMAIL)) {
  stop("Set OPENALEX_EMAIL before running this script. It is NOT needed to\n",
       "reproduce the paper -- data/cache/citing_field_data.rds is shipped.")
}
DELAY            <- 0.10
BATCH            <- 50      # OpenAlex OR-filter: up to 50 ids per request
CACHE            <- file.path(DATA_DIR, "citing_field_data.rds")

set.seed(42)

api_get <- function(url, params = list(), retries = 4) {
  params$mailto <- OPENALEX_EMAIL
  if (nchar(OPENALEX_API_KEY) > 0) params$api_key <- OPENALEX_API_KEY
  for (attempt in 1:retries) {
    Sys.sleep(DELAY)
    resp <- tryCatch(GET(url, query = params), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200)
      return(fromJSON(rawToChar(resp$content), flatten = TRUE))
    if (!is.null(resp) && status_code(resp) == 429) { Sys.sleep(2^attempt); next }
    if (attempt < retries) Sys.sleep(1.5)
  }
  NULL
}

# Pull one batch of up to 50 works by id, return a tidy data.table.
fetch_batch <- function(ids) {
  flt <- paste0("openalex_id:", paste(ids, collapse = "|"))
  res <- api_get("https://api.openalex.org/works",
                 params = list(
                   filter   = flt,
                   per_page = BATCH,
                   select   = "id,type,primary_topic,primary_location,concepts"
                 ))
  if (is.null(res) || is.null(res$results) || !is.data.frame(res$results) ||
      nrow(res$results) == 0) return(NULL)
  df <- res$results

  getcol <- function(nm) if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))

  # Level-0 concept names per work (concepts is a list-column of data.frames)
  l0 <- vapply(seq_len(nrow(df)), function(i) {
    cc <- df$concepts[[i]]
    if (is.null(cc) || !is.data.frame(cc) || nrow(cc) == 0) return(NA_character_)
    lv <- if ("level" %in% names(cc)) cc$level else NA
    nm <- if ("display_name" %in% names(cc)) cc$display_name else NA
    paste(unique(nm[!is.na(lv) & lv == 0]), collapse = ";")
  }, character(1))

  # Max economics concept score (any level) — a soft within-field signal
  econ_score <- vapply(seq_len(nrow(df)), function(i) {
    cc <- df$concepts[[i]]
    if (is.null(cc) || !is.data.frame(cc) || nrow(cc) == 0) return(NA_real_)
    if (!all(c("display_name", "score") %in% names(cc))) return(NA_real_)
    s <- cc$score[cc$display_name == "Economics"]
    if (length(s) == 0) return(0) else max(s, na.rm = TRUE)
  }, numeric(1))

  data.table(
    citing_oa_id = gsub("https://openalex.org/", "", getcol("id")),
    type         = getcol("type"),
    pt_field     = getcol("primary_topic.field.display_name"),
    pt_subfield  = getcol("primary_topic.subfield.display_name"),
    pt_domain    = getcol("primary_topic.domain.display_name"),
    venue        = getcol("primary_location.source.display_name"),
    l0_concepts  = l0,
    econ_score   = econ_score
  )
}

# ---------------------------------------------------------------------------
# Load unique citing works and fetch (resumable)
# ---------------------------------------------------------------------------
cr <- readRDS(file.path(DATA_DIR, "network_citation_data.rds")); setDT(cr)
cr[, citing_short := gsub("https://openalex.org/", "", citing_oa_id)]
all_ids <- unique(cr$citing_short)
all_ids <- all_ids[!is.na(all_ids) & nchar(all_ids) > 0]
cat("Unique citing works to resolve:", length(all_ids), "\n")

done <- data.table()
if (file.exists(CACHE)) {
  done <- readRDS(CACHE); setDT(done)
  cat("Resuming: cache already has", nrow(done), "works\n")
}
todo <- setdiff(all_ids, done$citing_oa_id)
cat("Remaining to fetch:", length(todo), "\n\n")

if (length(todo) > 0) {
  batches <- split(todo, ceiling(seq_along(todo) / BATCH))
  acc <- list(done)
  t0 <- Sys.time()
  for (b in seq_along(batches)) {
    out <- fetch_batch(batches[[b]])
    if (!is.null(out)) acc[[length(acc) + 1L]] <- out
    if (b %% 25 == 0 || b == length(batches)) {
      cur <- rbindlist(acc, fill = TRUE, use.names = TRUE)
      cur <- unique(cur, by = "citing_oa_id")
      saveRDS(cur, CACHE)
      el <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
      cat(sprintf("  batch %d/%d  cached=%d  elapsed=%.1f min\n",
                  b, length(batches), nrow(cur), el))
    }
  }
}

fd <- readRDS(CACHE); setDT(fd)
cat("\nResolved works in cache:", nrow(fd), "/", length(all_ids),
    sprintf(" (%.1f%%)\n\n", 100 * nrow(fd) / length(all_ids)))

# ---------------------------------------------------------------------------
# Merge field data back onto the citation links
# ---------------------------------------------------------------------------
lk <- merge(cr, fd, by.x = "citing_short", by.y = "citing_oa_id", all.x = TRUE)
cat("Links with resolved field data:",
    sprintf("%d / %d (%.1f%%)\n\n", sum(!is.na(lk$pt_field) | !is.na(lk$l0_concepts)),
            nrow(lk), 100 * mean(!is.na(lk$pt_field) | !is.na(lk$l0_concepts))))

has_l0 <- function(s, names) {
  if (is.na(s)) return(NA)
  parts <- trimws(strsplit(s, ";")[[1]])
  any(tolower(parts) %in% tolower(names))
}

# ---- Original (broken) classifier, reproduced for the baseline -------------
eh_concepts <- c("Economics","History","Economic history","Development economics",
  "Political economy","Economic growth","Finance","Demography","Sociology",
  "Political science","Economic geography","Agricultural economics","Labour economics",
  "Labor economics","International economics","Monetary economics","Public economics",
  "Macroeconomics","Microeconomics","Econometrics","Statistics")
eh_pat <- paste0("(?i)^(", paste(unique(tolower(eh_concepts)), collapse = "|"), ")$")
lk[, wf_orig := grepl(eh_pat, tolower(citing_top_concept), perl = TRUE)]

# ---- ANGLE A: modern curated primary_topic taxonomy ------------------------
# History sits as a subfield under the "Arts and Humanities" field; economics
# is the field "Economics, Econometrics and Finance".
lk[, wf_A_narrow := (pt_field == "Economics, Econometrics and Finance") |
                    (pt_subfield == "History")]
lk[, wf_A_broad := wf_A_narrow |
     (pt_field %in% c("Business, Management and Accounting", "Social Sciences")) |
     (pt_subfield %in% c("Economics and Econometrics", "Finance",
                         "Sociology and Political Science", "Geography, Planning and Development",
                         "Development"))]

# ---- ANGLE B: level-0 concept ancestors ------------------------------------
lk[, wf_B_narrow := vapply(l0_concepts, has_l0, logical(1),
                           names = c("Economics", "History"))]
lk[, wf_B_broad := vapply(l0_concepts, has_l0, logical(1),
                          names = c("Economics", "History", "Sociology",
                                    "Political science", "Geography", "Business"))]
# soft signal: any non-trivial economics concept score
lk[, wf_B_econscore := !is.na(econ_score) & econ_score >= 0.3]

share <- function(x) {
  v <- x[!is.na(x)]
  sprintf("%.1f%% within  (n classified = %d)", 100 * mean(v), length(v))
}

cat("=====================================================================\n")
cat("  WITHIN-FIELD SHARE BY METHOD (link level)\n")
cat("=====================================================================\n")
cat("Original (top-concept whitelist)     :", share(lk$wf_orig), "\n")
cat("A. primary_topic  (narrow: Econ+Hist):", share(lk$wf_A_narrow), "\n")
cat("A. primary_topic  (broad social sci) :", share(lk$wf_A_broad), "\n")
cat("B. level-0 concepts (narrow)         :", share(lk$wf_B_narrow), "\n")
cat("B. level-0 concepts (broad)          :", share(lk$wf_B_broad), "\n")
cat("B. econ concept score >= 0.3         :", share(lk$wf_B_econscore), "\n\n")

# Agreement between the two independent classifiers (narrow)
both <- lk[!is.na(wf_A_narrow) & !is.na(wf_B_narrow)]
cat("A/B narrow agreement:",
    sprintf("%.1f%% of %d links\n\n", 100 * mean(both$wf_A_narrow == both$wf_B_narrow), nrow(both)))

# ---- ANGLE C: format / document type ---------------------------------------
cat("=====================================================================\n")
cat("  ANGLE C: DOCUMENT TYPE (format hypothesis)\n")
cat("=====================================================================\n")
cat("Citation links by citing work type:\n")
print(lk[!is.na(type), .N, by = type][order(-N)])
cat("\nWithin-field (A narrow) share, by type:\n")
print(lk[!is.na(type) & !is.na(wf_A_narrow),
         .(n = .N, within_pct = round(100 * mean(wf_A_narrow), 1)), by = type][order(-n)])
nonart <- lk[!is.na(type), mean(type != "article")]
cat(sprintf("\nNon-article share of all classified links: %.1f%%\n", 100 * nonart))
cat("Within-field share among ARTICLES only (A narrow):",
    share(lk[type == "article"]$wf_A_narrow), "\n\n")

cat("Top 30 citing venues:\n")
print(lk[!is.na(venue) & venue != "", .N, by = .(venue, pt_field)][order(-N)][1:30])

# ---- HUMAN AUDIT: 50 'cross-field' links per Angle A -----------------------
cat("\n=====================================================================\n")
cat("  AUDIT SAMPLE: 50 links classed CROSS-field by Angle A (narrow)\n")
cat("  (inspect venue/topic to judge true discipline)\n")
cat("=====================================================================\n")
aud <- lk[wf_A_narrow == FALSE & !is.na(venue),
          .(citing_top_concept, pt_field, pt_subfield, type, venue)]
set.seed(7)
aud <- aud[sample(.N, min(50, .N))]
print(aud, nrow = 50)

cat("\nDONE.\n")

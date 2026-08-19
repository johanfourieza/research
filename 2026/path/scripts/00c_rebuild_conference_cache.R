# =============================================================================
# 00c_rebuild_conference_cache.R -- rebuild the conference cache (offline)
# -----------------------------------------------------------------------------
# Documents the provenance of data/cache/conference_parsed_data.rds and
# data/cache/prize_dissertation_data.rds, and rebuilds them if the underlying
# workbook changes. Network-free.
#
# The shipped conference cache combines:
#   - EHA 2006-2025: hand-transcribed from the official programme PDFs into
#     data/raw/Conference_Papers.xlsx (with exact session begin/end times).
#     The automated EHA PDF parse in 00_data_collection.R is unusable because
#     pdftools garbles the multi-column layout; the workbook replaces it.
#   - EHS 2003-2024: the reliable HTML parse from 00_data_collection.R
#     (rows reused from the existing cache).
#
# The dissertation-prize file (Gerschenkron, Nevins) comes from the workbook's
# "Prizes" sheet.
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

XLSX <- file.path(DATA_RAW, "Conference_Papers.xlsx")
stopifnot(file.exists(XLSX))

cat("Source workbook:", XLSX, "\n\n")

# --- 1. Clean EHA from the workbook ---------------------------------------------
cat("Reading clean EHA conferences from workbook...\n")
eha <- read_eha_conferences(XLSX)
cat("  EHA papers:", nrow(eha), " years:", min(eha$year), "-", max(eha$year), "\n")
cat("  EHA rows with session_order:", sum(!is.na(eha$session_order)), "\n\n")

# --- 2. Reuse parsed EHS rows from the existing cache -----------------------------
existing_path <- file.path(DATA_CACHE, "conference_parsed_data.rds")
stopifnot(file.exists(existing_path))
existing <- as.data.table(readRDS(existing_path))
ehs <- existing[conference == "EHS"]
cat("Reusing parsed EHS rows:", nrow(ehs), "\n\n")

# --- 3. Combine (fill = TRUE: EHS rows get NA for EHA-only timing columns) --------
conf <- rbind(ehs, eha, fill = TRUE)
cat("Combined conference object:", nrow(conf), "rows\n")
print(conf[, .N, by = conference])
cat("\n  EHA session_order distribution:\n")
print(eha[, .N, by = session_order][order(session_order)])
cat("  EHA lunch-adjacent: pre_lunch =", sum(eha$pre_lunch),
    " post_lunch =", sum(eha$post_lunch), "\n\n")

# --- 4. Dissertation prizes from the workbook --------------------------------------
cat("Reading clean dissertation prizes from workbook...\n")
prize_diss <- read_dissertation_prizes(XLSX)
cat("  Dissertation-prize recipients:", nrow(prize_diss), "\n")
print(prize_diss[, .N, by = prize_name])
cat("\n")

# --- 5. Write --------------------------------------------------------------------
saveRDS(conf,       file.path(DATA_CACHE, "conference_parsed_data.rds"))
saveRDS(prize_diss, file.path(DATA_CACHE, "prize_dissertation_data.rds"))
cat("Wrote conference_parsed_data.rds and prize_dissertation_data.rds to data/cache/\n")

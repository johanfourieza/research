# =============================================================================
# 01_download_data.R
# Download Cricsheet bulk JSON ZIP files for Tests, ODIs, and T20Is.
# Extract into data/raw/{format}/
# =============================================================================

library(utils)

# --- Paths ---
## Headless path detection: CS/scripts/<this>.R -> project root is two levels up
.cs_args <- commandArgs(trailingOnly = FALSE)
.cs_file <- sub("^--file=", "", .cs_args[grep("^--file=", .cs_args)])
base_dir <- if (length(.cs_file) > 0) normalizePath(file.path(dirname(.cs_file), "..", "..")) else normalizePath(file.path("..", ".."))
raw_dir  <- file.path(base_dir, "data", "raw")

## CS paper: longer-form DOMESTIC first-class only (County Championship + Sheffield Shield)
formats <- list(
  county    = "https://cricsheet.org/downloads/cch_json.zip",
  sheffield = "https://cricsheet.org/downloads/ssh_json.zip"
)

# --- Download and extract each format ---
for (fmt in names(formats)) {
  url      <- formats[[fmt]]
  dest_dir <- file.path(raw_dir, fmt)
  zip_path <- file.path(raw_dir, paste0(fmt, "_json.zip"))

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  cat(sprintf("\n=== %s ===\n", toupper(fmt)))

  # Download
  if (file.exists(zip_path)) {
    cat(sprintf("  ZIP already exists: %s, skipping download.\n", zip_path))
  } else {
    cat(sprintf("  Downloading %s ...\n", url))
    download.file(url, zip_path, mode = "wb", quiet = FALSE)
    cat(sprintf("  Saved to %s\n", zip_path))
  }

  # Extract — only JSON files not already present
  cat(sprintf("  Extracting to %s ...\n", dest_dir))
  all_in_zip <- unzip(zip_path, list = TRUE)$Name
  json_files <- all_in_zip[grepl("\\.json$", all_in_zip)]
  existing   <- list.files(dest_dir, pattern = "\\.json$")
  to_extract <- json_files[!basename(json_files) %in% existing]

  if (length(to_extract) == 0) {
    cat(sprintf("  All %d JSON files already extracted.\n", length(json_files)))
  } else {
    unzip(zip_path, files = to_extract, exdir = dest_dir, junkpaths = TRUE)
    cat(sprintf("  Extracted %d new JSON files (%d total in ZIP).\n",
                length(to_extract), length(json_files)))
  }

  n_files <- length(list.files(dest_dir, pattern = "\\.json$"))
  cat(sprintf("  %d JSON files in %s\n", n_files, dest_dir))
}

# --- Download Cricsheet people register (for batting hand info) ---
people_csv <- file.path(raw_dir, "people.csv")
if (!file.exists(people_csv)) {
  cat("\nDownloading Cricsheet people register (for batting hand info)...\n")
  download.file("https://cricsheet.org/register/people.csv", people_csv,
                mode = "wb", quiet = FALSE)
  cat(sprintf("  Saved to %s\n", people_csv))
} else {
  cat(sprintf("\nPeople register already exists: %s\n", people_csv))
}

cat("\nDone.\n")

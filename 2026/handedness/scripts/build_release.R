# Build the GitHub data release for "Invisible Handedness". Idempotent.
#
# Copies the five partnership-level analysis files into the release folder,
# rewriting them as UTF-8 CSV with empty strings for missing values, and
# generates docs/variable_definitions.csv. The ball-by-ball files and the raw
# Cricsheet JSON are deliberately NOT part of the release (see README.md);
# they are reconstructed by scripts/international/01_download_data.R through
# 08_ball_level_data.R and scripts/firstclass/cs_01_download.R onward.
#
# Run headless:  Rscript scripts/build_release.R   (from the Github/ folder's
# parent, or anywhere — paths are absolute).

library(readr)
library(dplyr)

project_root <- "C:/Users/johanf/Dropbox/0Claude0/1Research/FourieSiebrits_Cricket/handedness"
analysis_in  <- file.path(project_root, "data", "analysis")
release_root <- file.path(project_root, "Github")
analysis_out <- file.path(release_root, "data", "analysis")
docs_out     <- file.path(release_root, "docs")

dir.create(analysis_out, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_out, recursive = TRUE, showWarnings = FALSE)

files <- c("analysis_tests.csv", "analysis_odis.csv", "analysis_t20is.csv",
           "analysis_county.csv", "analysis_sheffield.csv")

defs <- list()
for (f in files) {
  df <- read_csv(file.path(analysis_in, f), show_col_types = FALSE)
  write_csv(df, file.path(analysis_out, f), na = "")
  defs[[f]] <- tibble(
    file      = f,
    variable  = names(df),
    type      = vapply(df, function(x) class(x)[1], character(1)),
    n_rows    = nrow(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1))
  )
  cat(sprintf("  %-24s %6d rows  %2d cols  written\n", f, nrow(df), ncol(df)))
}

write_csv(bind_rows(defs), file.path(docs_out, "variable_definitions.csv"), na = "")
cat("variable_definitions.csv written\nDONE.\n")

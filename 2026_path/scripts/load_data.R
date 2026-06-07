###############################################################################
# load_data.R
#
# Loads the curated journal dataset (data/Journals_2026_clean.csv) as a tibble
# named `journals`, with sensible column types, and prints a short summary.
# Optionally loads the combined conference-programme dataset as `conferences`.
#
# Run this from the 2026_path/ folder.
###############################################################################

library(readr)
library(dplyr)

# --- Journal dataset --------------------------------------------------------
journals <- read_csv(
  "data/Journals_2026_clean.csv",
  show_col_types = FALSE,
  guess_max = 5000
)

cat(sprintf("Loaded journals: %d rows x %d cols\n", nrow(journals), ncol(journals)))
cat(sprintf("  Journals: %s\n", paste(sort(unique(journals$Journal)), collapse = ", ")))
cat(sprintf("  Year range: %d-%d\n", min(journals$Year, na.rm = TRUE),
            max(journals$Year, na.rm = TRUE)))

# The annual Google Scholar citation snapshots are the columns Google14..Google26
# (cumulative citations observed in calendar years 2014..2026). The analysis in
# scripts/01_analysis.R derives age-specific citations (e.g. age-two, age-five)
# from these by aligning each paper's publication year with the snapshot years.

# --- Conference programmes (optional) ---------------------------------------
# Combined, analysis-ready EHA (hand-coded) + EHS (parsed) programme entries.
if (file.exists("data/conference_parsed_data.csv")) {
  conferences <- read_csv("data/conference_parsed_data.csv", show_col_types = FALSE)
  cat(sprintf("Loaded conferences: %d rows (%s)\n", nrow(conferences),
              paste(sprintf("%s=%d", names(table(conferences$conference)),
                            as.integer(table(conferences$conference))),
                    collapse = ", ")))
}

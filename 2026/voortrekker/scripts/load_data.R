# Load the released data. Run from the 2026/voortrekker folder.
suppressPackageStartupMessages({library(readr); library(dplyr)})
rd <- function(f) read_csv(f, show_col_types = FALSE, guess_max = 100000)

census          <- rd("data/raw/cape_census_1825.csv")
voortrekkers    <- rd("data/raw/voortrekkers.csv")
compensation    <- rd("data/raw/slave_compensation.csv")
links           <- rd("data/linked/voortrekker_census_matches.csv")
link_decisions  <- rd("data/linked/link_decisions.csv")
training_labels <- rd("data/linked/training_labels.csv")
owner_links     <- rd("data/linked/voortrekker_emancipation_matches.csv")
crosswalk       <- rd("data/linked/genealogy_crosswalk.csv")
analysis        <- rd("data/analysis/analysis_dataset.csv")

for (nm in c("census", "voortrekkers", "compensation", "links", "link_decisions", "training_labels", "owner_links", "crosswalk", "analysis"))
  cat(sprintf("%-16s %7d rows\n", nm, nrow(get(nm))))

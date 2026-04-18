# Minimal loader for the replication data.
# Works from the repository root.

library(readr)

census     <- read_csv("data/raw/cape_census_1825.csv")
voortrek   <- read_csv("data/raw/voortrekkers.csv")
slaves     <- read_csv("data/raw/slave_compensation.csv")

links_c    <- read_csv("data/linked/voortrekker_census_matches.csv")
links_e    <- read_csv("data/linked/voortrekker_emancipation_matches.csv")

analysis   <- read_csv("data/analysis/analysis_dataset.csv")

cat("cape_census_1825:                 ", nrow(census),  "rows\n")
cat("voortrekkers:                     ", nrow(voortrek),"rows\n")
cat("slave_compensation:               ", nrow(slaves),  "rows\n")
cat("voortrekker_census_matches:       ", nrow(links_c), "rows\n")
cat("voortrekker_emancipation_matches: ", nrow(links_e), "rows\n")
cat("analysis_dataset:                 ", nrow(analysis),"rows\n")

# Example: households linked to Voortrekkers
# analysis %>% filter(is_voortrekker == 1) %>% count(district)

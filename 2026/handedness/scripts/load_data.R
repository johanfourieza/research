# Minimal loader for the "Invisible Handedness" replication data.
# Run from the release root (the folder containing data/).

library(readr)

tests     <- read_csv("data/analysis/analysis_tests.csv")
odis      <- read_csv("data/analysis/analysis_odis.csv")
t20is     <- read_csv("data/analysis/analysis_t20is.csv")
county    <- read_csv("data/analysis/analysis_county.csv")
sheffield <- read_csv("data/analysis/analysis_sheffield.csv")

cat("analysis_tests.csv:    ", nrow(tests),     "rows\n")  # expect 28,989
cat("analysis_odis.csv:     ", nrow(odis),      "rows\n")  # expect 39,362
cat("analysis_t20is.csv:    ", nrow(t20is),     "rows\n")  # expect 45,184
cat("analysis_county.csv:   ", nrow(county),    "rows\n")  # expect 43,911
cat("analysis_sheffield.csv:", nrow(sheffield), "rows\n")  # expect  6,875

# The main-sample filter used throughout the paper:
#   hand_known == 1, both_avg_known == 1, either_debutant == 0
# reproduces the Ns reported in Table 1 (e.g. 26,140 Test partnerships).

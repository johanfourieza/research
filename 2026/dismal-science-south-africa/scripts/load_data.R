# Load every file in this package into a named list.
# Usage (from the package folder): source("scripts/load_data.R"); names(dismal)
library(readr)
files <- list.files("data", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
dismal <- lapply(files, read_csv, show_col_types = FALSE)
names(dismal) <- sub("\\.csv$", "", basename(files))

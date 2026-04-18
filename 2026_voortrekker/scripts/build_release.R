# Build the GitHub data release.
# Run from the EREH_submission/Github/ directory.

library(readxl)
library(readr)
library(dplyr)

project_root <- "C:/Users/johanf/Dropbox/0Claude0/1Research/FourieLinks_Voortrekkers"
data_in     <- file.path(project_root, "WorkingPaper/Data")
output_in   <- file.path(project_root, "WorkingPaper/Output")
release_root <- file.path(project_root, "EREH_submission/Github")

raw_out      <- file.path(release_root, "data/raw")
linked_out   <- file.path(release_root, "data/linked")
analysis_out <- file.path(release_root, "data/analysis")

# ---- 1. Raw: 1825 Cape census (one row per household) -----------------------
# Source: the analysis pipeline has already combined + cleaned the 11 district
# sheets into a household-level dataset. We strip the Voortrekker-specific
# derived columns so the file is a clean census of all 10,420 households.
census <- read_csv(file.path(output_in, "analysis_dataset.csv"),
                   show_col_types = FALSE)

drop_cols <- c("is_voortrekker",
               "wealth_index", "wealth_simple",
               "settler_children", "settler_adults",
               "household_size", "children_ratio",
               "horses", "cattle", "sheep",
               "total_slaves", "total_khoe",
               "total_grain_sown", "total_grain_reaped",
               "name_clean", "has_comma",
               "census_surname", "census_first",
               "census_surname_std", "census_first_std",
               "census_first_only",
               "wife_name_clean", "wife_has_comma",
               "census_wife_surname", "census_wife_first",
               "census_wife_surname_std", "census_wife_first_std",
               "census_wife_first_only")

census_raw <- census %>% select(-any_of(drop_cols))
write_csv(census_raw, file.path(raw_out, "cape_census_1825.csv"), na = "")
cat("cape_census_1825.csv:", nrow(census_raw), "rows,", ncol(census_raw), "cols\n")

# ---- 2. Raw: Voortrekker genealogy (individual + spouse columns only) -------
vt <- read_excel(file.path(data_in, "Voortrekkers 2.xlsx"), sheet = "Main")

# Keep the first 24 columns (individual + spouse), 33-40 (WYK, DISTRIK, MOVE),
# and 69-70 (NOTES, LEADERS). Drop parents/children wide block.
keep_idx <- c(1:24, 32:38, 69:70)
vt_clean <- vt[, keep_idx]

# Tidy column names: collapse whitespace, lowercase, underscore-separated.
clean_name <- function(x) {
  x <- gsub("\\.\\.\\.\\d+$", "", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}
names(vt_clean) <- make.unique(clean_name(names(vt_clean)))

write_csv(vt_clean, file.path(raw_out, "voortrekkers.csv"), na = "")
cat("voortrekkers.csv:", nrow(vt_clean), "rows,", ncol(vt_clean), "cols\n")

# ---- 3. Raw: Slave compensation ---------------------------------------------
slaves <- read_excel(file.path(data_in, "Slave Emancipation Dataset.xlsx"),
                     sheet = "Sheet1")
names(slaves) <- clean_name(names(slaves))
write_csv(slaves, file.path(raw_out, "slave_compensation.csv"), na = "")
cat("slave_compensation.csv:", nrow(slaves), "rows,", ncol(slaves), "cols\n")

# ---- 4. Linked: record-linkage results --------------------------------------
file.copy(file.path(output_in, "voortrekker_matches.csv"),
          file.path(linked_out, "voortrekker_census_matches.csv"),
          overwrite = TRUE)
file.copy(file.path(output_in, "voortrekker_emancipation_matches.csv"),
          file.path(linked_out, "voortrekker_emancipation_matches.csv"),
          overwrite = TRUE)
cat("linked files copied\n")

# ---- 5. Analysis: household-level merged dataset ----------------------------
file.copy(file.path(output_in, "analysis_dataset.csv"),
          file.path(analysis_out, "analysis_dataset.csv"),
          overwrite = TRUE)
cat("analysis_dataset.csv copied\n")

cat("\nDONE.\n")

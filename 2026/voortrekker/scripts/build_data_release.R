# Build the data release (data/) from a completed replication run.
# Run from the 2026/voortrekker folder, after `Rscript code/run_all.R` in replication/:
#   Rscript scripts/build_data_release.R
suppressPackageStartupMessages({library(readxl); library(readr); library(dplyr)})

rep <- "replication"
out_tab <- file.path(rep, "output/tables")
stopifnot(file.exists(file.path(out_tab, "analysis_dataset.csv")))
for (d in c("data/raw", "data/linked", "data/analysis")) dir.create(d, recursive = TRUE, showWarnings = FALSE)

clean_name <- function(x) {
  x <- gsub("\\.\\.\\.\\d+$", "", x); x <- gsub("[^A-Za-z0-9]+", "_", x); tolower(gsub("^_+|_+$", "", x))
}

# ---- analysis dataset: household level, with Voortrekker flag and derived variables
analysis <- read_csv(file.path(out_tab, "analysis_dataset.csv"), show_col_types = FALSE) %>%
  mutate(is_voortrekker = toupper(as.character(is_voortrekker)) %in% c("TRUE", "1"),
         married_couple = head_role %in% "male" & !is.na(spouse_name_raw) & spouse_name_raw != "")
write_csv(analysis, "data/analysis/analysis_dataset.csv", na = "")

# ---- census: the same households without linkage-derived or constructed variables
derived <- c("is_voortrekker", "married_couple", "wealth_index", "wealth_simple", "settler_children", "settler_adults",
             "household_size", "children_ratio", "horses", "cattle", "sheep", "total_slaves", "total_khoe",
             "total_grain_sown", "total_grain_reaped", "census_surname", "census_first", "census_surname_std",
             "census_first_std", "census_first_only", "census_wife_surname", "census_wife_first",
             "census_wife_surname_std", "census_wife_first_std", "census_wife_first_only")
write_csv(analysis %>% select(-any_of(derived)), "data/raw/cape_census_1825.csv", na = "")

# ---- Voortrekker genealogy (individual and spouse columns)
vt <- read_excel(file.path(rep, "data/raw/Voortrekkers 2.xlsx"), sheet = "Main")
vt <- vt[, c(1:24, 32:38, 69:70)]
names(vt) <- make.unique(clean_name(names(vt)))
vt <- bind_cols(tibble(source_row = seq_len(nrow(vt)) + 1L), vt)   # row in the source workbook (row 1 = header)
write_csv(vt, "data/raw/voortrekkers.csv", na = "")

# ---- slave compensation records (Ekama 2021)
slaves <- read_excel(file.path(rep, "data/raw/Slave Emancipation Dataset.xlsx"), sheet = "Sheet1") %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))
names(slaves) <- clean_name(names(slaves))
write_csv(slaves, "data/raw/slave_compensation.csv", na = "")

# ---- links: final census links with provenance, all review decisions, training labels
# The replication inputs are checksum-pinned and stay as they are; the public copies use
# descriptive labels.
neutral <- function(x) {
  x <- gsub("old link", "manual-linkage pair", x, fixed = TRUE)
  x <- gsub("Johan adjudicated", "Author adjudicated", x, fixed = TRUE)
  x <- gsub("not proposed (old link)", "not proposed (manual-linkage pair)", x, fixed = TRUE)
  x
}
decisions <- read_csv(file.path(rep, "data/linkage/link_decisions.csv"), show_col_types = FALSE) %>%
  mutate(across(c(basis, final_quality, classifier), neutral)) %>%
  rename(manual_linkage_pair = old_link)
labels <- read_csv(file.path(rep, "data/linkage/training_labels.csv"), show_col_types = FALSE) %>%
  transmute(row_id, census_id, label,
            label_source = recode(label_source, "both models" = "both model reviews", "Johan" = "authors"),
            sample = recode(set, "original training pair" = "hand-labeled pair", "new sample" = "supplementary sample"),
            hand_label = original_label)
pairs <- read_csv(file.path(out_tab, "final_pairs_complete.csv"), show_col_types = FALSE)
links <- pairs %>%
  transmute(row_id, census_id, vt_surname, vt_name, census_name = name_raw, census_district = enumeration_district,
            block_type, evidence_state, classifier_score = match_score) %>%
  left_join(decisions %>% filter(decision == "retain") %>%
              select(row_id, census_id, decided_by = final_quality, classifier_status = classifier,
                     review_claude = claude, review_codex = codex),
            by = c("row_id", "census_id"))
stopifnot(nrow(links) == sum(decisions$decision == "retain"), !anyNA(links$decided_by))
write_csv(links, "data/linked/voortrekker_census_matches.csv", na = "")
write_csv(decisions, "data/linked/link_decisions.csv", na = "")
write_csv(labels, "data/linked/training_labels.csv", na = "")
write_csv(read_csv(file.path(out_tab, "voortrekker_emancipation_matches.csv"), show_col_types = FALSE),
          "data/linked/voortrekker_emancipation_matches.csv", na = "")
# row_id (the linkage sample of 1,220 men) to the genealogy: source_row and id in data/raw/voortrekkers.csv
write_csv(read_csv(file.path(rep, "data/inputs/genealogy_row_crosswalk.csv"), show_col_types = FALSE) %>%
            transmute(row_id, source_row = vt_source_row, id = vt_id),
          "data/linked/genealogy_crosswalk.csv", na = "")

# ---- machine-readable variable list
files <- list.files("data", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
vars <- bind_rows(lapply(files, function(f) {
  d <- read_csv(f, show_col_types = FALSE, guess_max = 100000)
  data.frame(file = f, variable = names(d), type = vapply(d, function(x) if (is.numeric(x)) "numeric" else
    if (is.logical(x)) "logical" else "string", ""), n_rows = nrow(d), n_missing = vapply(d, function(x) sum(is.na(x)), 0L))
}))
dir.create("docs", showWarnings = FALSE)
write_csv(vars, "docs/variable_definitions.csv")
for (f in files) cat(sprintf("%-45s %6d rows\n", f, nrow(read_csv(f, show_col_types = FALSE))))

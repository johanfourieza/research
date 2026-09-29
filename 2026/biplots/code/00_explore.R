# Source inspection for revision decisions. No source file is changed.
suppressPackageStartupMessages({library(readr); library(dplyr); library(tidyr); library(readxl)})
dir.create("docs/execution", recursive = TRUE, showWarnings = FALSE)
pkgs <- c("tidyverse", "biplotEZ", "moveEZ", "gifski", "magick", "pdftools",
          "Rtsne", "uwot", "dbscan", "renv", "digest", "xml2", "zip")
print(data.frame(package = pkgs, installed = vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)))
d <- read_csv("data/raw/stellenbosch_temp.csv", show_col_types = FALSE)
cat("RAW", nrow(d), ncol(d), "\n")
print(names(d))
vars <- c("slave_men", "slave_women", "slave_sons", "slave_daughters", "slave_children",
          "cattle_bull", "cattle_work", "cattle_breeding", "cattle_cows", "cattle_heifers", "cattle_calves",
          "horses", "horse_riding", "horse_breeding", "sheep", "sheep_breeding", "sheep_wether", "sheep_wool",
          "vines", "wine", "brandy", "wheat_vol", "barley_vol", "rye_vol", "oat_vol",
          "khoe_men", "khoe_women", "khoe_sons", "khoe_daughters", "wagons", "goats", "pigs", "rifle", "swords", "pistols")
a <- d %>% group_by(year) %>% summarise(n = n(), across(all_of(vars), ~sum(!is.na(.x))), .groups = "drop")
write_csv(a, "docs/execution/raw_coverage_by_year.csv")
print(as.data.frame(a[a$year >= 1790 & a$year <= 1829, ]), row.names = FALSE)
extreme <- d %>% select(hhobs, hhid, year, all_of(vars)) %>% pivot_longer(all_of(vars), names_to="variable", values_to="value") %>%
  filter(!is.na(value)) %>% group_by(variable) %>% slice_max(value, n=4, with_ties=FALSE) %>% ungroup()
write_csv(extreme, "docs/execution/raw_extremes.csv")
cat("Duplicate hhobs:", sum(duplicated(d$hhobs)), "; duplicate household/year:", sum(duplicated(d[c("hhid", "year")])), "\n")
f <- "data/raw/1825 series.xlsx"
for (s in excel_sheets(f)) {
  h <- suppressMessages(read_excel(f, sheet=s, col_names=FALSE, n_max=6, col_types="text"))
  txt <- vapply(h, function(x) paste(na.omit(x), collapse=" / "), character(1))
  cat("\nSHEET", s, "\n")
  print(data.frame(column=seq_along(txt), header=txt), row.names=FALSE)
}
if (requireNamespace("pdftools", quietly=TRUE)) {
  cat("\nPDF extraction available\n")
  text <- pdftools::pdf_text("literature/pdfs/Fourie_etal_2024_StellenboschTaxCensuses.pdf")
  writeLines(text, "docs/execution/tax_census_source_text.txt")
}

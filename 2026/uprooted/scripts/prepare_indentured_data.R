library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(janitor)
library(purrr)

script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(script_dir, "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_root, "Data")
derived_dir <- file.path(script_dir, "generated")
if (!dir.exists(derived_dir)) dir.create(derived_dir, recursive = TRUE)

indentured_dir <- file.path(data_dir, "Indian Indentured")
indentured_files <- list.files(indentured_dir, pattern = "\\.xls$", full.names = TRUE)

read_one_indentured_file <- function(path) {
  sheet_name <- excel_sheets(path)[1]
  read_excel(path, sheet = sheet_name) |>
    clean_names() |>
    rename_with(~"arrival", .cols = any_of(c("arri", "arrival"))) |>
    mutate(source_file = basename(path))
}

indentured_raw <- map_dfr(indentured_files, read_one_indentured_file) |>
  mutate(
    zillah = str_squish(str_to_upper(as.character(zillah))),
    thanna = str_squish(str_to_upper(as.character(thanna))),
    village = str_squish(str_to_upper(as.character(village))),
    employer = str_squish(as.character(employer)),
    employer_upper = str_to_upper(employer)
  )

# Rough district mapping from employer-location strings.
# This is intentionally conservative: only employers with an identifiable
# KwaZulu-Natal place name are assigned.
indentured_clean <- indentured_raw |>
  mutate(
    gadm2_region = case_when(
      str_detect(employer_upper, "DURBAN|ISIPINGO|ADDINGTON|UMGENI|AVOCA|LA LUCIA|UMHLANGA|LA MERCY|TONGAAT|OTTAWA|MOUNT EDGECOMBE|MT\\. EDGECOMBE|SEA COW|EFFINGHAM|CORNUBIA|CLARE ESTATE|DURBAN CORPORATION") ~ "ZAF.4.2_1", # eThekwini
      str_detect(employer_upper, "VERULAM|STANGER|KEARSNEY|NONOTI|BLACKBURN|ADDINGTON SUGAR ESTATE STANGER") ~ "ZAF.4.3_1", # iLembe
      str_detect(employer_upper, "UMZINTO|IFAFA|SCOTTBURGH|REYNOLDS BROTHERS|ESPERANZA") ~ "ZAF.4.5_1", # Ugu
      str_detect(employer_upper, "PIETERMARITZBURG|UMGUNGUNDLOVU") ~ "ZAF.4.6_1", # uMgungundlovu
      str_detect(employer_upper, "NEWCASTLE|AMAJUBA") ~ "ZAF.4.1_1", # Amajuba
      str_detect(employer_upper, "ZULULAND|EMPANGENI|RICHARDS BAY|UTHUNGULU|KING CETSHWAYO") ~ "ZAF.4.10_1", # King Cetshwayo
      TRUE ~ NA_character_
    ),
    district_name = case_when(
      gadm2_region == "ZAF.4.2_1" ~ "eThekwini",
      gadm2_region == "ZAF.4.3_1" ~ "iLembe",
      gadm2_region == "ZAF.4.5_1" ~ "Ugu",
      gadm2_region == "ZAF.4.6_1" ~ "uMgungundlovu",
      gadm2_region == "ZAF.4.1_1" ~ "Amajuba",
      gadm2_region == "ZAF.4.10_1" ~ "King Cetshwayo",
      TRUE ~ NA_character_
    ),
    origin_region = case_when(
      str_detect(zillah, "MADRAS|CHINGLEPUT|NORTH ARCOT|SOUTH ARCOT|CHITTOOR|TANJORE|SALEM|MADURA|COIMBATORE|NELLORE|TRICHINOPOLY|GODAVARI|VIZAGAPATAM|GANJAM") ~ "South India",
      str_detect(zillah, "PATNA|SHAHABAD|GYA|GAYA|BUSTEE|GONDA|GHAZEEPORE|AZIMGURH|GORAKHPORE|BENARES|JAUNPORE|ALLAHABAD|LUCKNOW|FAIZABAD") ~ "North India",
      TRUE ~ "Other/Unknown"
    )
  )

coverage_summary <- tibble(
  total_records = nrow(indentured_clean),
  records_with_district = sum(!is.na(indentured_clean$gadm2_region)),
  district_match_rate = mean(!is.na(indentured_clean$gadm2_region)),
  unique_zillah = n_distinct(indentured_clean$zillah[!is.na(indentured_clean$zillah)]),
  unique_employers = n_distinct(indentured_clean$employer[!is.na(indentured_clean$employer)])
)

indentured_by_district_origin <- indentured_clean |>
  filter(!is.na(gadm2_region), !is.na(zillah), zillah != "") |>
  count(gadm2_region, district_name, zillah, origin_region, name = "n_indentured") |>
  group_by(gadm2_region) |>
  mutate(
    total_indentured = sum(n_indentured),
    pct_origin = n_indentured / total_indentured,
    partner_iso2 = "IN"
  ) |>
  ungroup() |>
  arrange(gadm2_region, desc(n_indentured), zillah)

indentured_district_totals <- indentured_clean |>
  filter(!is.na(gadm2_region)) |>
  count(gadm2_region, district_name, origin_region, name = "n_indentured") |>
  group_by(gadm2_region) |>
  mutate(
    total_indentured = sum(n_indentured),
    pct_origin = n_indentured / total_indentured
  ) |>
  ungroup() |>
  arrange(gadm2_region, desc(n_indentured), origin_region)

write.csv(coverage_summary, file.path(derived_dir, "indentured_coverage_summary.csv"), row.names = FALSE, na = "")
write.csv(indentured_by_district_origin, file.path(derived_dir, "indentured_origins_by_district.csv"), row.names = FALSE, na = "")
write.csv(indentured_district_totals, file.path(derived_dir, "indentured_regions_by_district.csv"), row.names = FALSE, na = "")

cat("Indentured files:", length(indentured_files), "\n")
cat("Indentured rows:", nrow(indentured_clean), "\n")
cat("Matched to districts:", sum(!is.na(indentured_clean$gadm2_region)), "\n")
cat("District match rate:", round(mean(!is.na(indentured_clean$gadm2_region)) * 100, 1), "%\n")
cat("District-origin rows written:", nrow(indentured_by_district_origin), "\n")

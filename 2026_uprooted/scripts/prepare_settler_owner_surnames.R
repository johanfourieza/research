library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(script_dir, "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_root, "Data")
generated_dir <- file.path(script_dir, "generated")
if (!dir.exists(generated_dir)) dir.create(generated_dir, recursive = TRUE)

clean_token <- function(x) {
  x |>
    as.character() |>
    str_squish()
}

clean_key <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish()
}

slave_path <- file.path(data_dir, "Slave Emancipation Dataset.xlsx")
huguenot_dict_path <- file.path(generated_dir, "huguenot_surname_dictionary.csv")
huguenot_vars_path <- file.path(generated_dir, "huguenot_surname_variants.csv")
settlers_1820_html_path <- file.path(generated_dir, "_tmp_1820_surnames.html")

slaves <- read_excel(slave_path) |>
  transmute(
    district = clean_token(District_name),
    owner_name = clean_token(Owner_name),
    owner_surname = clean_token(Owner_surname),
    owner_key = clean_key(Owner_surname),
    owner_huguenot_builtin = as.integer(Owner_hugenoot),
    owner_brit = as.integer(Owner_brit),
    n_slaves = suppressWarnings(as.numeric(Num_slaves)),
    compensation = suppressWarnings(as.numeric(Compensation))
  ) |>
  filter(!is.na(district), district != "", !is.na(owner_key), owner_key != "")

huguenot_exact <- read_csv(huguenot_dict_path, show_col_types = FALSE) |>
  transmute(
    surname_key = clean_key(surname_key),
    matched_surname = clean_token(surname),
    match_type = "exact_dictionary"
  )

huguenot_variants <- read_csv(huguenot_vars_path, show_col_types = FALSE) |>
  transmute(
    surname_key = clean_key(alt_key),
    matched_surname = clean_token(alt_variant),
    match_type = "variant_dictionary"
  )

huguenot_keys <- bind_rows(huguenot_exact, huguenot_variants) |>
  filter(!is.na(surname_key), surname_key != "") |>
  arrange(match_type, matched_surname) |>
  distinct(surname_key, .keep_all = TRUE)

owners_matched <- slaves |>
  left_join(huguenot_keys, by = c("owner_key" = "surname_key")) |>
  mutate(
    owner_huguenot_dictionary = as.integer(!is.na(matched_surname)),
    owner_huguenot_any = pmax(owner_huguenot_builtin, owner_huguenot_dictionary, na.rm = TRUE)
  )

if (!file.exists(settlers_1820_html_path)) {
  stop("Missing cached 1820 settlers surname page: ", settlers_1820_html_path)
}

settlers_1820_html <- paste(readLines(settlers_1820_html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
settlers_1820_matches <- str_match_all(
  settlers_1820_html,
  "(?i)<a\\s+href=\"settlersearch\\.php\\?mylastname=([^\"]+)\">([^<]+)</a>\\s*\\((\\d+)\\)"
)[[1]]

settlers_1820_dict <- tibble(
  surname_site = settlers_1820_matches[, 3],
  surname_key = clean_key(settlers_1820_matches[, 3]),
  n_site_records = as.integer(settlers_1820_matches[, 4]),
  source = "1820settlers.com",
  source_url = "https://www.1820settlers.com/genealogy/settlernames-all.php",
  origin_partner_iso2 = "GB",
  origin_partner_label = "United Kingdom",
  fallback_origin_place = "England"
) |>
  filter(!is.na(surname_key), surname_key != "") |>
  distinct(surname_key, .keep_all = TRUE) |>
  arrange(surname_site)

owners_matched <- owners_matched |>
  left_join(
    settlers_1820_dict |>
      select(
        surname_key,
        surname_site_1820 = surname_site,
        n_site_records_1820 = n_site_records,
        origin_partner_iso2_1820 = origin_partner_iso2,
        origin_partner_label_1820 = origin_partner_label,
        fallback_origin_place_1820 = fallback_origin_place
      ),
    by = c("owner_key" = "surname_key")
  ) |>
  mutate(
    owner_1820_dictionary = as.integer(!is.na(surname_site_1820)),
    owner_brit_any = pmax(owner_brit, owner_1820_dictionary, na.rm = TRUE)
  )

owner_key_summary <- owners_matched |>
  group_by(owner_key, owner_surname) |>
  summarise(
    n_rows = n(),
    districts = paste(sort(unique(district)), collapse = "; "),
    n_districts = n_distinct(district),
    owner_brit_builtin = as.integer(any(owner_brit == 1, na.rm = TRUE)),
    owner_1820_dictionary = as.integer(any(owner_1820_dictionary == 1, na.rm = TRUE)),
    owner_brit_any = as.integer(any(owner_brit_any == 1, na.rm = TRUE)),
    owner_huguenot_builtin = as.integer(any(owner_huguenot_builtin == 1, na.rm = TRUE)),
    owner_huguenot_dictionary = as.integer(any(owner_huguenot_dictionary == 1, na.rm = TRUE)),
    owner_huguenot_any = as.integer(any(owner_huguenot_any == 1, na.rm = TRUE)),
    surname_site_1820 = first(na.omit(surname_site_1820)),
    matched_surname = first(na.omit(matched_surname)),
    match_type = first(na.omit(match_type)),
    .groups = "drop"
  ) |>
  arrange(desc(owner_brit_any), desc(owner_huguenot_any), desc(n_rows), owner_surname)

district_summary <- owners_matched |>
  group_by(district) |>
  summarise(
    n_owner_rows = n(),
    n_owner_surnames = n_distinct(owner_key),
    n_builtin_brit_rows = sum(owner_brit == 1, na.rm = TRUE),
    n_dictionary_1820_rows = sum(owner_1820_dictionary == 1, na.rm = TRUE),
    n_any_brit_rows = sum(owner_brit_any == 1, na.rm = TRUE),
    share_builtin_brit_rows = n_builtin_brit_rows / n_owner_rows,
    share_dictionary_1820_rows = n_dictionary_1820_rows / n_owner_rows,
    share_any_brit_rows = n_any_brit_rows / n_owner_rows,
    n_builtin_huguenot_rows = sum(owner_huguenot_builtin == 1, na.rm = TRUE),
    n_dictionary_huguenot_rows = sum(owner_huguenot_dictionary == 1, na.rm = TRUE),
    n_any_huguenot_rows = sum(owner_huguenot_any == 1, na.rm = TRUE),
    share_builtin_huguenot_rows = n_builtin_huguenot_rows / n_owner_rows,
    share_dictionary_huguenot_rows = n_dictionary_huguenot_rows / n_owner_rows,
    share_any_huguenot_rows = n_any_huguenot_rows / n_owner_rows,
    n_unique_builtin_huguenot_surnames = n_distinct(owner_key[owner_huguenot_builtin == 1]),
    n_unique_dictionary_huguenot_surnames = n_distinct(owner_key[owner_huguenot_dictionary == 1]),
    n_unique_any_huguenot_surnames = n_distinct(owner_key[owner_huguenot_any == 1]),
    slave_count_any_huguenot = sum(n_slaves[owner_huguenot_any == 1], na.rm = TRUE),
    slave_count_total = sum(n_slaves, na.rm = TRUE),
    slave_share_any_huguenot = ifelse(slave_count_total > 0, slave_count_any_huguenot / slave_count_total, NA_real_),
    compensation_any_huguenot = sum(compensation[owner_huguenot_any == 1], na.rm = TRUE),
    compensation_total = sum(compensation, na.rm = TRUE),
    compensation_share_any_huguenot = ifelse(compensation_total > 0, compensation_any_huguenot / compensation_total, NA_real_),
    .groups = "drop"
  ) |>
  arrange(desc(share_any_brit_rows), desc(share_any_huguenot_rows), district)

validation_summary <- owners_matched |>
  mutate(
    builtin_label = ifelse(owner_huguenot_builtin == 1, "builtin_huguenot", "builtin_non_huguenot"),
    dict_label = ifelse(owner_huguenot_dictionary == 1, "dict_huguenot", "dict_non_huguenot")
  ) |>
  count(builtin_label, dict_label, name = "n_rows") |>
  arrange(desc(n_rows))

validation_1820_summary <- owners_matched |>
  mutate(
    builtin_label = ifelse(owner_brit == 1, "builtin_brit", "builtin_non_brit"),
    dict_label = ifelse(owner_1820_dictionary == 1, "dict_1820", "dict_non_1820")
  ) |>
  count(builtin_label, dict_label, name = "n_rows") |>
  arrange(desc(n_rows))

dictionary_only_surnames <- owner_key_summary |>
  filter(owner_huguenot_builtin == 0, owner_huguenot_dictionary == 1) |>
  arrange(desc(n_rows), owner_surname)

builtin_only_surnames <- owner_key_summary |>
  filter(owner_huguenot_builtin == 1, owner_huguenot_dictionary == 0) |>
  arrange(desc(n_rows), owner_surname)

dictionary_only_1820 <- owner_key_summary |>
  filter(owner_brit_builtin == 0, owner_1820_dictionary == 1) |>
  arrange(desc(n_rows), owner_surname)

builtin_only_brit <- owner_key_summary |>
  filter(owner_brit_builtin == 1, owner_1820_dictionary == 0) |>
  arrange(desc(n_rows), owner_surname)

write_csv(owners_matched, file.path(generated_dir, "settler_owner_rows.csv"), na = "")
write_csv(owner_key_summary, file.path(generated_dir, "settler_owner_surname_summary.csv"), na = "")
write_csv(district_summary, file.path(generated_dir, "settler_huguenot_by_district.csv"), na = "")
write_csv(validation_summary, file.path(generated_dir, "settler_huguenot_validation.csv"), na = "")
write_csv(dictionary_only_surnames, file.path(generated_dir, "settler_huguenot_dictionary_only.csv"), na = "")
write_csv(builtin_only_surnames, file.path(generated_dir, "settler_huguenot_builtin_only.csv"), na = "")
write_csv(settlers_1820_dict, file.path(generated_dir, "settler_1820_surname_dictionary.csv"), na = "")
write_csv(
  district_summary |>
    select(
      district,
      n_owner_rows,
      n_owner_surnames,
      n_builtin_brit_rows,
      n_dictionary_1820_rows,
      n_any_brit_rows,
      share_builtin_brit_rows,
      share_dictionary_1820_rows,
      share_any_brit_rows
    ),
  file.path(generated_dir, "settler_1820_by_district.csv"),
  na = ""
)
write_csv(validation_1820_summary, file.path(generated_dir, "settler_1820_validation.csv"), na = "")
write_csv(dictionary_only_1820, file.path(generated_dir, "settler_1820_dictionary_only.csv"), na = "")
write_csv(builtin_only_brit, file.path(generated_dir, "settler_1820_builtin_only.csv"), na = "")

cat("Owner rows:", nrow(owners_matched), "\n")
cat("Unique owner surnames:", n_distinct(owners_matched$owner_key), "\n")
cat("Builtin British rows:", sum(owners_matched$owner_brit == 1, na.rm = TRUE), "\n")
cat("1820-settler dictionary rows:", sum(owners_matched$owner_1820_dictionary == 1, na.rm = TRUE), "\n")
cat("Any British rows:", sum(owners_matched$owner_brit_any == 1, na.rm = TRUE), "\n")
cat("Builtin Huguenot rows:", sum(owners_matched$owner_huguenot_builtin == 1, na.rm = TRUE), "\n")
cat("Dictionary Huguenot rows:", sum(owners_matched$owner_huguenot_dictionary == 1, na.rm = TRUE), "\n")
cat("Any Huguenot rows:", sum(owners_matched$owner_huguenot_any == 1, na.rm = TRUE), "\n")
cat("Districts:", nrow(district_summary), "\n")

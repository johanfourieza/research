library(readxl)
library(dplyr)
library(stringr)

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

hug_raw <- read_excel(file.path(data_dir, "List of Huguenots.xlsx"), sheet = "Huguenots")

hug_surnames <- hug_raw |>
  transmute(
    surname = clean_token(Surname),
    alt_spelling = clean_token(`Alternative spelling`),
    area = clean_token(Area),
    residence = clean_token(Residence),
    town_origin = clean_token(`Town origin`),
    province_origin = clean_token(`Province origin`)
  ) |>
  filter(!is.na(surname), surname != "") |>
  mutate(
    surname_key = clean_key(surname)
  )

alt_variants <- hug_surnames |>
  filter(!is.na(alt_spelling), alt_spelling != "") |>
  mutate(alt_split = str_split(alt_spelling, ",")) |>
  tidyr::unnest(alt_split) |>
  mutate(
    alt_split = clean_token(alt_split),
    alt_key = clean_key(alt_split)
  ) |>
  filter(alt_split != "") |>
  select(
    surname,
    surname_key,
    alt_variant = alt_split,
    alt_key,
    area,
    residence,
    town_origin,
    province_origin
  ) |>
  distinct()

surname_summary <- hug_surnames |>
  group_by(surname, surname_key) |>
  summarise(
    n_people = n(),
    n_areas = n_distinct(area[!is.na(area) & area != ""]),
    areas = paste(sort(unique(area[!is.na(area) & area != ""])), collapse = "; "),
    residences = paste(sort(unique(residence[!is.na(residence) & residence != ""])), collapse = "; "),
    towns_origin = paste(sort(unique(town_origin[!is.na(town_origin) & town_origin != ""])), collapse = "; "),
    provinces_origin = paste(sort(unique(province_origin[!is.na(province_origin) & province_origin != ""])), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(n_people), surname)

area_summary <- hug_surnames |>
  filter(!is.na(area), area != "") |>
  count(area, sort = TRUE, name = "n_people") |>
  arrange(desc(n_people), area)

write.csv(hug_surnames, file.path(generated_dir, "huguenot_people_raw.csv"), row.names = FALSE, na = "")
write.csv(alt_variants, file.path(generated_dir, "huguenot_surname_variants.csv"), row.names = FALSE, na = "")
write.csv(surname_summary, file.path(generated_dir, "huguenot_surname_dictionary.csv"), row.names = FALSE, na = "")
write.csv(area_summary, file.path(generated_dir, "huguenot_area_summary.csv"), row.names = FALSE, na = "")

cat("Huguenot individuals:", nrow(hug_surnames), "\n")
cat("Unique surnames:", n_distinct(hug_surnames$surname_key), "\n")
cat("Surname variants:", nrow(alt_variants), "\n")
cat("Distinct Cape areas:", nrow(area_summary), "\n")

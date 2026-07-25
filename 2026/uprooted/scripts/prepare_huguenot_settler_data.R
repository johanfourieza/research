library(readxl)
library(data.table)
library(dplyr)
library(stringr)

script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(script_dir, "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_root, "Data")
generated_dir <- file.path(script_dir, "generated")
if (!dir.exists(generated_dir)) dir.create(generated_dir, recursive = TRUE)

clean_name <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish()
}

residence_aliases <- tribble(
  ~raw,                 ~alias,
  "BO LA MOTTE",        "LA MOTTE",
  "L ARC D ORLEANS",    "ORLEANS",
  "DE ZOETE INVAL",     "ZOETE INVAL",
  "DE GOEDE HOOP",      "GOEDE HOOP",
  "VREDE EN LUST",      "VREDE LUST",
  "LUSTIGAAN",          "LUSTIG AAN",
  "LA TERRA DE LUC",    "TERRA DE LUC",
  "LA TERRE DE LUC",    "TERRE DE LUC"
)

hug <- read_excel(file.path(data_dir, "List of Huguenots.xlsx"), sheet = "Huguenots") |>
  transmute(
    surname = str_squish(as.character(Surname)),
    alt_spelling = str_squish(as.character(`Alternative spelling`)),
    residence = str_squish(as.character(Residence)),
    area = str_squish(as.character(Area)),
    town_origin = str_squish(as.character(`Town origin`)),
    province_origin = str_squish(as.character(`Province origin`)),
    residence_clean = clean_name(Residence)
  ) |>
  left_join(residence_aliases, by = c("residence_clean" = "raw")) |>
  mutate(
    residence_match_key = coalesce(alias, residence_clean)
  )

farms <- fread(file.path(data_dir, "farm_classifications.csv")) |>
  transmute(
    GID_2,
    farm_name,
    language,
    farm_name_clean = clean_name(farm_name)
  ) |>
  filter(str_detect(GID_2, "^ZAF\\.9\\."))

contains_match_vec <- function(farm, key) {
  out <- rep(FALSE, length(farm))
  keep <- !is.na(farm) & !is.na(key) & key != ""
  out[keep] <- str_detect(farm[keep], fixed(key[keep]))
  out
}

hug_keys <- hug |>
  filter(!is.na(residence_match_key), residence_match_key != "") |>
  distinct(surname, alt_spelling, residence, area, town_origin, province_origin, residence_match_key)

candidate_matches <- tidyr::crossing(
  farms |> mutate(row_id = row_number()),
  hug_keys |> mutate(key_id = row_number())
) |>
  filter(
    nchar(residence_match_key) >= 5,
    contains_match_vec(farm_name_clean, residence_match_key) |
      contains_match_vec(residence_match_key, farm_name_clean)
  ) |>
  select(GID_2, farm_name, language, residence, residence_match_key, surname,
         alt_spelling, area, town_origin, province_origin)

distinctive_patterns <- c(
  "LA MOTTE", "CABRIERE", "CHAMPAGNE", "LA COTTE", "LA DAUPHINE",
  "LA PROVENCE", "TERRA DE LUC", "TERRE DE LUC", "LANGUEDOC",
  "ORLEANS", "L ARC", "BLAAUWKLIP", "IDASVALLEI", "LE RHONE",
  "RHONEN", "LA CONCORDE", "LA BRIE", "CALAIS", "NANTES",
  "BERGEN HENEGOUWEN", "PICARDI"
)

candidate_matches <- candidate_matches |>
  mutate(
    distinctive_huguenot = str_detect(
      clean_name(farm_name),
      str_c(distinctive_patterns, collapse = "|")
    ) |
      str_detect(
        clean_name(residence_match_key),
        str_c(distinctive_patterns, collapse = "|")
      )
  )

district_summary <- candidate_matches |>
  distinct(GID_2, farm_name, residence_match_key, .keep_all = TRUE) |>
  count(GID_2, name = "n_huguenot_farms") |>
  right_join(
    farms |>
      distinct(GID_2) |>
      count(GID_2, name = "n_total_dummy") |>
      select(-n_total_dummy),
    by = "GID_2"
  ) |>
  mutate(
    n_huguenot_farms = if_else(is.na(n_huguenot_farms), 0L, n_huguenot_farms)
  ) |>
  left_join(
    farms |> count(GID_2, name = "n_total_farms"),
    by = "GID_2"
  ) |>
  mutate(
    pct_huguenot_farms = if_else(n_total_farms > 0, n_huguenot_farms / n_total_farms, 0)
  ) |>
  arrange(desc(n_huguenot_farms), GID_2)

district_summary_strict <- candidate_matches |>
  filter(distinctive_huguenot) |>
  distinct(GID_2, farm_name, residence_match_key, .keep_all = TRUE) |>
  count(GID_2, name = "n_huguenot_farms_strict") |>
  right_join(
    farms |>
      distinct(GID_2) |>
      count(GID_2, name = "n_total_dummy") |>
      select(-n_total_dummy),
    by = "GID_2"
  ) |>
  mutate(
    n_huguenot_farms_strict = if_else(is.na(n_huguenot_farms_strict), 0L, n_huguenot_farms_strict)
  ) |>
  left_join(
    farms |> count(GID_2, name = "n_total_farms"),
    by = "GID_2"
  ) |>
  mutate(
    pct_huguenot_farms_strict = if_else(n_total_farms > 0, n_huguenot_farms_strict / n_total_farms, 0)
  ) |>
  arrange(desc(n_huguenot_farms_strict), GID_2)

write.csv(candidate_matches, file.path(generated_dir, "huguenot_farm_matches.csv"), row.names = FALSE, na = "")
write.csv(district_summary, file.path(generated_dir, "huguenot_farms_by_district.csv"), row.names = FALSE, na = "")
write.csv(district_summary_strict, file.path(generated_dir, "huguenot_farms_by_district_strict.csv"), row.names = FALSE, na = "")

cat("Huguenot keys:", nrow(hug_keys), "\n")
cat("Candidate farm matches:", nrow(candidate_matches), "\n")
cat("Districts with any Huguenot match:", sum(district_summary$n_huguenot_farms > 0), "\n")
cat("Districts with strict Huguenot match:", sum(district_summary_strict$n_huguenot_farms_strict > 0), "\n")

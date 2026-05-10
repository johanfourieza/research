###############################################################################
# 01_clean_data.R
#
# The Efficiency Costs of Ethnic Discrimination in State Employment:
# Evidence from the South African Constabulary
#
# Reads SAC final.xlsx (Augmented sheet) and BoerSurnames.xlsx,
# cleans and constructs analysis variables, saves sac_clean.rds
###############################################################################

# ── 1.1 Setup and Package Loading ──────────────────────────────────────────

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(janitor)

# Paths — LabourEconomicsSubmission self-contained build.
# The script must be run from the LabourEconomicsSubmission folder.
# Raw data inputs (SAC final.xlsx, BoerSurnames.xlsx) are read from
# the project root (Fourie_SAC); the cleaned data and all outputs
# are written inside LabourEconomicsSubmission.
cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!basename(cwd) %in% c("LabourEconomicsSubmission", "Submission_LE")) {
  stop("Run this script from the LabourEconomicsSubmission folder ",
       "(the directory that contains 01_clean_data.R).")
}
work_dir  <- cwd
proj_root <- if (basename(cwd) == "LabourEconomicsSubmission") {
  dirname(cwd)            # .../Fourie_SAC
} else {
  dirname(dirname(cwd))   # .../Fourie_SAC (legacy Submission_LE layout)
}

raw_file        <- file.path(proj_root, "SAC final.xlsx")
boer_file       <- file.path(proj_root, "BoerSurnames.xlsx")
out_file        <- file.path(work_dir, "sac_clean.rds")
afr_review_file <- file.path(work_dir, "tables", "afrikaner_manual_review.csv")

# Output directories (all inside Submission_LE/)
dir.create(file.path(work_dir, "tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "models"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "output"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "output", "tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "output", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(work_dir, "output", "models"),  showWarnings = FALSE, recursive = TRUE)

# ── 1.2 Read Raw Data ─────────────────────────────────────────────────────

cat("Reading Augmented sheet...\n")
aug <- read_excel(raw_file, sheet = "Augmented")
cat("  Augmented:", nrow(aug), "rows x", ncol(aug), "cols\n")

cat("Reading Original sheet for validation...\n")
orig <- read_excel(raw_file, sheet = "Original")
cat("  Original:", nrow(orig), "rows x", ncol(orig), "cols\n")

cat("Reading BoerSurnames...\n")
boer_raw <- read_excel(boer_file)
boer_surnames <- unique(tolower(trimws(boer_raw$Surname)))
boer_surnames <- boer_surnames[!is.na(boer_surnames) & boer_surnames != ""]
cat("  Unique Boer surnames:", length(boer_surnames), "\n")

cat("Reading Calling sheet...\n")
calling <- read_excel(raw_file, sheet = "Calling")

# ── 1.3 Basic Cleaning ────────────────────────────────────────────────────

df <- aug

# Recode "." and "" to NA across all character columns
df <- df %>%
  mutate(across(where(is.character), ~ if_else(.x %in% c(".", "", "None"), NA_character_, .x)))

# Coerce key numeric columns (some read as character due to "." entries)
numeric_cols <- c("Wage", "Lwage", "Age", "Height", "Weight", "BMI",
                  "Length", "lLength", "Chestdiff", "Chestave", "Chestmin", "Chestmax",
                  "EDay", "EMonth", "EYear", "Dday", "Dmonth", "Dyear",
                  "Afrikaans", "Repeatboth", "Repeatlast",
                  "MaritalStatusDum", "Farmer", "Class",
                  "Heightcal", "Weightcal", "Lengthcal",
                  "Feet", "Inches1", "Inches2",
                  "Pounds1", "Stone", "Pounds2",
                  "Birthday", "Birthmonth", "Birthyear",
                  "Blue", "Ruddy", "Tattoo",
                  "RegionDum", "Jewish", "Catholic",
                  "Black", "French", "Dieddum",
                  "NamesNumber", "Letterdum", "Row", "Number")

for (col in numeric_cols) {
  if (col %in% names(df)) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
}

# Standardize Enlisted years (contract length)
df <- df %>%
  mutate(
    contract_years = case_when(
      `Enlisted years` == "3"          ~ 3,
      `Enlisted years` == "2"          ~ 2,
      `Enlisted years` == "1"          ~ 1,
      `Enlisted years` == "6 months"   ~ 0.5,
      `Enlisted years` == "12 months"  ~ 1,
      str_detect(`Enlisted years`, "yr") ~ {
        # Parse "1 yr 233 days" type entries
        yrs  <- as.numeric(str_extract(`Enlisted years`, "^\\d+"))
        days <- as.numeric(str_extract(`Enlisted years`, "\\d+(?=\\s*days?)"))
        yrs + days / 365.25
      },
      `Enlisted years` == "Special"    ~ NA_real_,
      TRUE                             ~ NA_real_
    )
  )

# Standardize MaritalStatus → binary married indicator
df <- df %>%
  mutate(
    marital_status_clean = case_when(
      str_to_lower(trimws(MaritalStatus)) == "married" ~ "Married",
      str_to_lower(trimws(MaritalStatus)) == "single"  ~ "Single",
      str_to_lower(trimws(MaritalStatus)) == "widower" ~ "Widower",
      TRUE ~ NA_character_
    ),
    married = as.integer(marital_status_clean == "Married")
  )

# ── 1.4 Construct Date Variables ──────────────────────────────────────────

# Enlistment date from (EDay, EMonth, EYear)
df <- df %>%
  mutate(
    date_enlist = make_date(EYear, EMonth, EDay)
  )

# Discharge date from (Dyear, Dmonth, Dday)
df <- df %>%
  mutate(
    date_discharge = make_date(Dyear, Dmonth, Dday)
  )

# Treaty of Vereeniging
treaty_date <- as.Date("1902-05-31")

df <- df %>%
  mutate(
    # Post-Vereeniging indicator
    post_vereeniging = as.integer(!is.na(date_enlist) & date_enlist > treaty_date),

    # Tenure in days (computed; cross-validate against existing Length)
    tenure_days_calc = as.numeric(difftime(date_discharge, date_enlist, units = "days")),

    # Use existing Length when available, computed as fallback
    # Set negative values to NA (data errors)
    tenure_days = {
      td <- coalesce(Length, tenure_days_calc)
      if_else(!is.na(td) & td < 0, NA_real_, td)
    },

    # Months and quarters from treaty (for event-study)
    days_from_treaty = as.numeric(difftime(date_enlist, treaty_date, units = "days")),
    months_from_treaty = floor(days_from_treaty / 30.44),
    quarters_from_treaty = floor(days_from_treaty / 91.31),

    # Year-month for FE
    enlist_ym = if_else(!is.na(date_enlist),
                        format(date_enlist, "%Y-%m"),
                        NA_character_),
    enlist_year = EYear
  )

# Validate tenure_days against existing Length
tenure_check <- df %>%
  filter(!is.na(Length) & !is.na(tenure_days_calc)) %>%
  summarise(
    n = n(),
    correlation = cor(Length, tenure_days_calc),
    mean_diff = mean(Length - tenure_days_calc),
    exact_match = mean(abs(Length - tenure_days_calc) < 2)
  )
cat("\nTenure validation (Length vs computed):\n")
print(tenure_check)

# ── 1.5 Construct Afrikaans Indicator (3 variants) ────────────────────────

df <- df %>%
  mutate(
    surname_lower = tolower(trimws(Surname)),

    # Strict: surname match only
    afrikaans_strict = as.integer(surname_lower %in% boer_surnames),

    # Region: surname match AND born in Boer Republics
    afrikaans_region = as.integer(
      surname_lower %in% boer_surnames &
        !is.na(Region) & Region == "Boer Republics"
    ),

    # Broad: surname match OR born in Boer Republics
    afrikaans_broad = as.integer(
      surname_lower %in% boer_surnames |
        (!is.na(Region) & Region == "Boer Republics")
    )
  )

# Validate against existing Afrikaans column
afr_validation <- df %>%
  filter(!is.na(Afrikaans)) %>%
  summarise(
    n = n(),
    existing_afr = sum(Afrikaans == 1, na.rm = TRUE),
    strict_afr   = sum(afrikaans_strict == 1, na.rm = TRUE),
    region_afr   = sum(afrikaans_region == 1, na.rm = TRUE),
    broad_afr    = sum(afrikaans_broad == 1, na.rm = TRUE),
    agree_strict  = mean(Afrikaans == afrikaans_strict, na.rm = TRUE),
    agree_region  = mean(Afrikaans == afrikaans_region, na.rm = TRUE),
    agree_broad   = mean(Afrikaans == afrikaans_broad, na.rm = TRUE)
  )
cat("\nAfrikaans classification validation:\n")
print(afr_validation)

# Use existing column as baseline (it was carefully constructed for the 2017 paper)
# but keep new variants for robustness
df <- df %>%
  mutate(afrikaans = coalesce(as.integer(Afrikaans), afrikaans_strict))

# Manual review workflow for disputed Afrikaans cases.
# The exported CSV can be edited by hand using afrikaner_review = 1 or 0.
afr_review_candidates <- df %>%
  transmute(
    id = ID,
    row = Row,
    surname = Surname,
    names_orig = Names,
    region = Region,
    country = Country,
    place_origin = `Place origin`,
    place = Place,
    where_recruited = `Where Recruited`,
    signed_at = `Signed at`,
    baseline_afrikaans = afrikaans,
    afrikaans_strict,
    afrikaans_region,
    afrikaans_broad,
    disagree_strict = as.integer(afrikaans != afrikaans_strict),
    disagree_region = as.integer(afrikaans != afrikaans_region),
    disagree_broad = as.integer(afrikaans != afrikaans_broad),
    afrikaner_review = NA_integer_
  ) %>%
  filter(disagree_strict == 1 | disagree_region == 1 | disagree_broad == 1) %>%
  arrange(desc(baseline_afrikaans), desc(disagree_region), desc(disagree_strict), surname, names_orig, id)

if (!file.exists(afr_review_file)) {
  write.csv(afr_review_candidates, afr_review_file, row.names = FALSE, na = "")
  cat("\nCreated Afrikaner manual review file:\n")
} else {
  cat("\nUsing existing Afrikaner manual review file:\n")
}
cat("  ", afr_review_file, "\n")
cat("  Review rows:", nrow(afr_review_candidates), "\n")

afr_review_input <- suppressWarnings(read.csv(
  afr_review_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
))

if (!"afrikaner_review" %in% names(afr_review_input)) {
  afr_review_input$afrikaner_review <- NA_integer_
}

afr_review_input <- afr_review_input %>%
  transmute(
    row = suppressWarnings(as.integer(row)),
    id = as.character(id),
    afrikaner_review = suppressWarnings(as.integer(afrikaner_review))
  ) %>%
  filter(!is.na(row), !is.na(afrikaner_review), afrikaner_review %in% c(0L, 1L)) %>%
  distinct(row, id, .keep_all = TRUE)

df <- df %>%
  mutate(
    id_chr = as.character(ID),
    row_int = suppressWarnings(as.integer(Row))
  ) %>%
  left_join(afr_review_input, by = c("row_int" = "row", "id_chr" = "id")) %>%
  mutate(
    afrikaans_reviewed = if_else(!is.na(afrikaner_review), afrikaner_review, afrikaans),
    afrikaner_manual_override = as.integer(!is.na(afrikaner_review) & afrikaner_review != afrikaans)
  ) %>%
  select(-id_chr, -row_int)

cat("\nAfrikaner manual review summary:\n")
cat("  Baseline Afrikaners:", sum(df$afrikaans == 1, na.rm = TRUE), "\n")
cat("  Reviewed rows used:", nrow(afr_review_input), "\n")
cat("  Reviewed Afrikaners:", sum(df$afrikaans_reviewed == 1, na.rm = TRUE), "\n")
cat("  Manual overrides:", sum(df$afrikaner_manual_override == 1, na.rm = TRUE), "\n")

# ── 1.6 Construct Discharge Outcome Variables ─────────────────────────────

# Standardize Cause of discharge
df <- df %>%
  mutate(
    discharge_cause_clean = case_when(
      str_detect(`Cause of discharge`, regex("dismiss", ignore_case = TRUE))   ~ "Dismissed",
      str_detect(`Cause of discharge`, regex("desert", ignore_case = TRUE))    ~ "Deserted",
      str_detect(`Cause of discharge`, regex("misconduct", ignore_case = TRUE))~ "Misconduct",
      str_detect(`Cause of discharge`, regex("purchase", ignore_case = TRUE))  ~ "Purchase",
      str_detect(`Cause of discharge`, regex("free discharge", ignore_case = TRUE)) ~ "Free discharge",
      str_detect(`Cause of discharge`, regex("resign", ignore_case = TRUE))    ~ "Resigned",
      str_detect(`Cause of discharge`, regex("time exp", ignore_case = TRUE))  ~ "Time expired",
      str_detect(`Cause of discharge`, regex("medic", ignore_case = TRUE))     ~ "Medically unfit",
      str_detect(`Cause of discharge`, regex("retrench", ignore_case = TRUE))  ~ "Retrenched",
      str_detect(`Cause of discharge`, regex("deceas", ignore_case = TRUE))    ~ "Deceased",
      str_detect(`Cause of discharge`, regex("died", ignore_case = TRUE))      ~ "Deceased",
      str_detect(`Cause of discharge`, regex("transfer", ignore_case = TRUE))  ~ "Transfer",
      str_detect(`Cause of discharge`, regex("new condition", ignore_case = TRUE)) ~ "New conditions",
      str_detect(`Cause of discharge`, regex("GO98|para 8", ignore_case = TRUE)) ~ "Under GO98",
      str_detect(`Cause of discharge`, regex("promot", ignore_case = TRUE))    ~ "Promoted",
      str_detect(`Cause of discharge`, regex("own wish", ignore_case = TRUE))  ~ "Resigned",
      str_detect(`Cause of discharge`, regex("never attested", ignore_case = TRUE)) ~ NA_character_,
      str_detect(`Cause of discharge`, regex("unknown", ignore_case = TRUE))   ~ NA_character_,
      TRUE ~ NA_character_
    ),

    # Analytical groupings
    dismissed_deserted = as.integer(discharge_cause_clean %in% c("Dismissed", "Deserted", "Misconduct")),
    voluntary_exit     = as.integer(discharge_cause_clean %in% c("Purchase", "Free discharge", "Resigned")),
    time_expired       = as.integer(discharge_cause_clean == "Time expired"),
    medically_unfit    = as.integer(discharge_cause_clean == "Medically unfit"),
    retrenched         = as.integer(discharge_cause_clean == "Retrenched"),
    deceased           = as.integer(discharge_cause_clean == "Deceased"),
    transfer           = as.integer(discharge_cause_clean == "Transfer"),

    # Competing risks event type (0=censored/other, 1=dismissed, 2=voluntary, 3=time expired)
    event_type = case_when(
      dismissed_deserted == 1 ~ 1L,
      voluntary_exit == 1     ~ 2L,
      time_expired == 1       ~ 3L,
      TRUE                    ~ 0L
    ),

    # Any observed discharge
    discharged = as.integer(!is.na(discharge_cause_clean))
  )

# Character score (ordered 1-6 from worst to best)
df <- df %>%
  mutate(
    character_clean = case_when(
      str_detect(CharacterOrigin, regex("^exemplary", ignore_case = TRUE))    ~ "Exemplary",
      str_detect(CharacterOrigin, regex("^excellent", ignore_case = TRUE))    ~ "Exemplary",
      str_detect(CharacterOrigin, regex("^very good", ignore_case = TRUE))    ~ "Very Good",
      str_detect(CharacterOrigin, regex("^very fair", ignore_case = TRUE))    ~ "Fair",
      str_detect(CharacterOrigin, regex("^very bad", ignore_case = TRUE))     ~ "Very Bad",
      str_detect(CharacterOrigin, regex("^very ind", ignore_case = TRUE))     ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^very uns", ignore_case = TRUE))     ~ "Bad",
      str_detect(CharacterOrigin, regex("^good", ignore_case = TRUE))         ~ "Good",
      str_detect(CharacterOrigin, regex("^fair", ignore_case = TRUE))         ~ "Fair",
      str_detect(CharacterOrigin, regex("^indifferent", ignore_case = TRUE))  ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^bad", ignore_case = TRUE))          ~ "Bad",
      str_detect(CharacterOrigin, regex("^latterly good", ignore_case = TRUE))    ~ "Good",
      str_detect(CharacterOrigin, regex("^latterly very good", ignore_case = TRUE)) ~ "Very Good",
      str_detect(CharacterOrigin, regex("^latterly bad", ignore_case = TRUE))     ~ "Bad",
      str_detect(CharacterOrigin, regex("^latterly ind", ignore_case = TRUE))     ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^latterly very fair", ignore_case = TRUE)) ~ "Fair",
      str_detect(CharacterOrigin, regex("^formerly good", ignore_case = TRUE))    ~ "Good",
      str_detect(CharacterOrigin, regex("^formerly very good", ignore_case = TRUE)) ~ "Very Good",
      str_detect(CharacterOrigin, regex("^formerly fair", ignore_case = TRUE))    ~ "Fair",
      str_detect(CharacterOrigin, regex("^unsuit", ignore_case = TRUE))       ~ "Bad",
      str_detect(CharacterOrigin, regex("^unsat", ignore_case = TRUE))        ~ "Bad",
      str_detect(CharacterOrigin, regex("^undes", ignore_case = TRUE))        ~ "Bad",
      str_detect(CharacterOrigin, regex("^unrel", ignore_case = TRUE))        ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^unstable", ignore_case = TRUE))     ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^useless", ignore_case = TRUE))      ~ "Bad",
      str_detect(CharacterOrigin, regex("^poor", ignore_case = TRUE))         ~ "Bad",
      str_detect(CharacterOrigin, regex("^medium", ignore_case = TRUE))       ~ "Fair",
      str_detect(CharacterOrigin, regex("^insuf", ignore_case = TRUE))        ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^inef", ignore_case = TRUE))         ~ "Indifferent",
      str_detect(CharacterOrigin, regex("^sober", ignore_case = TRUE))        ~ "Good",
      str_detect(CharacterOrigin, regex("^\\\"decline", ignore_case = TRUE))  ~ NA_character_,
      str_detect(CharacterOrigin, regex("^\\\"otherwise", ignore_case = TRUE))~ "Good",
      str_detect(CharacterOrigin, regex("^no entr", ignore_case = TRUE))      ~ NA_character_,
      str_detect(CharacterOrigin, regex("^\\(no further", ignore_case = TRUE))~ NA_character_,
      str_detect(CharacterOrigin, regex("^a reliable", ignore_case = TRUE))   ~ "Very Good",
      str_detect(CharacterOrigin, regex("^nice", ignore_case = TRUE))         ~ "Good",
      str_detect(CharacterOrigin, regex("^previously", ignore_case = TRUE))   ~ "Good",
      str_detect(CharacterOrigin, regex("^time exp", ignore_case = TRUE))     ~ NA_character_,
      str_detect(CharacterOrigin, regex("^until", ignore_case = TRUE))        ~ "Good",
      str_detect(CharacterOrigin, regex("^up to", ignore_case = TRUE))        ~ "Good",
      str_detect(CharacterOrigin, regex("^IMG", ignore_case = TRUE))          ~ "Bad",
      str_detect(CharacterOrigin, regex("^according", ignore_case = TRUE))    ~ "Bad",
      str_detect(CharacterOrigin, regex("^last 6", ignore_case = TRUE))       ~ NA_character_,
      str_detect(CharacterOrigin, regex("^died", ignore_case = TRUE))         ~ NA_character_,
      str_detect(CharacterOrigin, regex("^latterly", ignore_case = TRUE))     ~ "Fair",
      str_detect(CharacterOrigin, regex("^v\\.", ignore_case = TRUE))         ~ "Very Good",
      TRUE ~ NA_character_
    ),

    character_score = case_when(
      character_clean == "Very Bad"     ~ 1L,
      character_clean == "Bad"          ~ 2L,
      character_clean == "Indifferent"  ~ 3L,
      character_clean == "Fair"         ~ 4L,
      character_clean == "Good"         ~ 5L,
      character_clean == "Very Good"    ~ 6L,
      character_clean == "Exemplary"    ~ 7L,
      TRUE                              ~ NA_integer_
    ),

    good_character = as.integer(character_clean %in% c("Very Good", "Exemplary"))
  )

cat("\nCharacter classification:\n")
print(table(df$character_clean, useNA = "always"))

cat("\nDischarge cause classification:\n")
print(table(df$discharge_cause_clean, useNA = "always"))

# ── 1.7 Construct Control Variables ──────────────────────────────────────

# Rank categories
df <- df %>%
  mutate(
    title_clean = case_when(
      str_detect(Title, regex("3/c\\s*Trooper", ignore_case = TRUE))   ~ "3/c Trooper",
      str_detect(Title, regex("2/c\\s*Trooper", ignore_case = TRUE))   ~ "2/c Trooper",
      str_detect(Title, regex("1/c\\s*Trooper", ignore_case = TRUE))   ~ "1/c Trooper",
      str_detect(Title, regex("Trooper", ignore_case = TRUE))          ~ "Trooper",
      str_detect(Title, regex("3/c\\s*Constable", ignore_case = TRUE)) ~ "3/c Constable",
      str_detect(Title, regex("Head\\s*Constable", ignore_case = TRUE))~ "Head Constable",
      str_detect(Title, regex("Constable", ignore_case = TRUE))        ~ "Constable",
      str_detect(Title, regex("Sgt.*Major|Sergeant.*Major", ignore_case = TRUE)) ~ "Sgt Major",
      str_detect(Title, regex("Staff.*Serg|Staff.*Sgt", ignore_case = TRUE))     ~ "Staff Sergeant",
      str_detect(Title, regex("1/c.*Serg|1/c.*Sgt", ignore_case = TRUE))        ~ "1/c Sergeant",
      str_detect(Title, regex("2/c.*Serg|2/c.*Sgt", ignore_case = TRUE))        ~ "2/c Sergeant",
      str_detect(Title, regex("Serg|Sgt", ignore_case = TRUE))                   ~ "Sergeant",
      str_detect(Title, regex("Lance.*Corp", ignore_case = TRUE))      ~ "Lance Corporal",
      str_detect(Title, regex("Medical.*Corp", ignore_case = TRUE))    ~ "Medical Corporal",
      str_detect(Title, regex("Corporal", ignore_case = TRUE))         ~ "Corporal",
      str_detect(Title, regex("Pioneer", ignore_case = TRUE))          ~ "Pioneer",
      str_detect(Title, regex("Warrant", ignore_case = TRUE))          ~ "Warrant Officer",
      str_detect(Title, regex("Hosp.*Orderly", ignore_case = TRUE))    ~ "Hospital Orderly",
      str_detect(Title, regex("Farrier", ignore_case = TRUE))          ~ "Farrier",
      str_detect(Title, regex("Bugler", ignore_case = TRUE))           ~ "Bugler",
      !is.na(Title) ~ Title,
      TRUE ~ NA_character_
    ),

    rank_cat = case_when(
      title_clean %in% c("3/c Trooper", "2/c Trooper", "1/c Trooper", "Trooper") ~ "Trooper",
      title_clean %in% c("3/c Constable", "Constable", "Head Constable")          ~ "Constable",
      title_clean %in% c("Corporal", "Lance Corporal", "Medical Corporal",
                          "Sergeant", "1/c Sergeant", "2/c Sergeant",
                          "Staff Sergeant", "Sgt Major", "Warrant Officer")        ~ "NCO",
      TRUE ~ "Specialist"
    )
  )

# Physical variables
df <- df %>%
  mutate(
    height_cm = Height,  # Already in cm in Augmented
    weight_kg = Weight,  # Already in kg in Augmented
    bmi       = BMI,     # Already computed
    chest_diff = Chestdiff
  )

# Skills (clean binary indicators; "." already recoded to NA)
df <- df %>%
  mutate(
    can_ride  = as.integer(Ride == "1" | Ride == 1),
    can_shoot = as.integer(Shoot == "1" | Shoot == 1),
    can_swim  = as.integer(Swim == "1" | Swim == 1),
    prior_army = as.integer(Army1 == "1" | Army1 == 1),
    served_army = as.integer(Army2 == "1" | Army2 == 1),
    speaks_african_lang = as.integer(Black == 1),
    speaks_french = as.integer(French == 1),
    farmer = as.integer(Farmer == 1),

    # Construct speaks_dutch from raw text columns Languages and Other languages?
    # Match Dutch, Hollands, Taal, Flemish (case-insensitive) in either column
    speaks_dutch = {
      lang_raw   <- tolower(coalesce(as.character(Languages), ""))
      other_raw  <- tolower(coalesce(as.character(`Other languages?`), ""))
      combined   <- paste(lang_raw, other_raw)
      as.integer(
        str_detect(combined, "dutch|hollands|\\btaal\\b|flemish")
      )
    }
  )

cat("\nSpeaks Dutch (from raw language text):\n")
print(table(df$speaks_dutch, useNA = "always"))
cat("  Cross-tab with Afrikaans:\n")
print(table(Afrikaans = df$Afrikaans, speaks_dutch = df$speaks_dutch, useNA = "always"))

# Location variables
df <- df %>%
  mutate(
    where_recruited = `Where Recruited`,
    signed_at = `Signed at`,
    region = Region,
    country = Country
  )

# Class (occupation) — already numeric 1/2/3 in data
df <- df %>%
  mutate(class = as.integer(Class))

# Log wage
df <- df %>%
  mutate(
    wage = Wage,
    lwage = coalesce(Lwage, log(Wage))
  )

# ── 1.8 Handle Repeat Enlisters ──────────────────────────────────────────

df <- df %>%
  mutate(
    repeat_enlister = as.integer(Repeatboth == 1),
    first_enlistment = as.integer(Repeatboth == 0 | is.na(Repeatboth))
  )

# Create spell number for repeat enlisters
df <- df %>%
  group_by(Surname, Names) %>%
  mutate(
    spell_number = row_number(date_enlist)
  ) %>%
  ungroup()

# ── 1.9 Construct IV-Related Variables ────────────────────────────────────

# Leave-one-out mean wage by (title_clean × enlist_year)
df <- df %>%
  group_by(title_clean, enlist_year) %>%
  mutate(
    cell_n = sum(!is.na(wage)),
    cell_sum = sum(wage, na.rm = TRUE),
    # Leave-one-out: (cell sum - own wage) / (cell N - 1)
    wage_schedule = if_else(
      cell_n > 1 & !is.na(wage),
      (cell_sum - wage) / (cell_n - 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-cell_n, -cell_sum)

# Interaction term for IV
df <- df %>%
  mutate(
    afr_post = afrikaans * post_vereeniging
  )

# Contract-length pay component: leave-one-out mean by (contract_years × enlist_year)
df <- df %>%
  group_by(contract_years, enlist_year) %>%
  mutate(
    cell_n_ct = sum(!is.na(wage)),
    cell_sum_ct = sum(wage, na.rm = TRUE),
    contract_wage_schedule = if_else(
      cell_n_ct > 1 & !is.na(wage),
      (cell_sum_ct - wage) / (cell_n_ct - 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-cell_n_ct, -cell_sum_ct)

# ── 1.10 Select, Order, Save ─────────────────────────────────────────────

# Additional useful variables
df <- df %>%
  mutate(
    age_sq = Age^2,
    height_sq = height_cm^2,
    is_trooper_3c = as.integer(title_clean == "3/c Trooper")
  )

# Select analysis variables
sac <- df %>%
  select(
    # Identifiers
    id = ID, row = Row, surname = Surname, names_orig = Names,
    img = IMG,

    # Afrikaans indicators
    afrikaans, afrikaans_reviewed, afrikaner_manual_override,
    afrikaans_strict, afrikaans_region, afrikaans_broad,

    # Dates
    date_enlist, date_discharge, enlist_year, enlist_ym,
    e_day = EDay, e_month = EMonth, e_year = EYear,
    d_day = Dday, d_month = Dmonth, d_year = Dyear,
    post_vereeniging, days_from_treaty, months_from_treaty, quarters_from_treaty,

    # Wage
    wage, lwage, wage_schedule, contract_wage_schedule,

    # Contract
    contract_years, enlisted_years_orig = `Enlisted years`,

    # Rank
    title_orig = Title, title_clean, rank_cat,
    is_trooper_3c,

    # Demographics
    age = Age, age_sq,
    height_cm, height_sq, weight_kg, bmi, chest_diff,
    married, marital_status_clean,

    # Skills
    can_ride, can_shoot, can_swim,
    prior_army, served_army,
    speaks_african_lang, speaks_french, speaks_dutch,

    # Occupation
    class, hisco = HISCO, hisco2 = HISCO2,
    calling = Calling, farmer,

    # Location
    region, country, where_recruited, signed_at,
    place_origin = `Place origin`, place = Place,

    # Outcomes
    tenure_days, tenure_days_calc,
    discharge_cause_orig = `Cause of discharge`,
    discharge_cause_clean,
    dismissed_deserted, voluntary_exit, time_expired,
    medically_unfit, retrenched, deceased, transfer,
    event_type, discharged,
    character_orig = CharacterOrigin,
    character_clean, character_score, good_character,

    # IV
    afr_post,

    # Repeat enlisters
    repeat_enlister, first_enlistment, spell_number,

    # Other
    religion = Religion, jewish = Jewish, catholic = Catholic,
    region_dum = RegionDum, died = Dieddum
  )

# ── Validation ────────────────────────────────────────────────────────────

cat("\n══════════════════════════════════════════════════════════════\n")
cat("VALIDATION SUMMARY\n")
cat("══════════════════════════════════════════════════════════════\n\n")

cat("Dataset dimensions:", nrow(sac), "x", ncol(sac), "\n\n")

# Key pattern: wage gap
wage_gap <- sac %>%
  group_by(afrikaans) %>%
  summarise(
    n = n(),
    mean_wage = mean(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE),
    mean_lwage = mean(lwage, na.rm = TRUE),
    .groups = "drop"
  )
cat("Wage by Afrikaans status:\n")
print(as.data.frame(wage_gap))
cat("  Expected: Afrikaans ~4.46-4.50 vs Non-Afrikaans ~5.01-5.06\n\n")

# Key pattern: tenure gap
tenure_gap <- sac %>%
  group_by(afrikaans) %>%
  summarise(
    n = n(),
    mean_tenure = mean(tenure_days, na.rm = TRUE),
    sd_tenure = sd(tenure_days, na.rm = TRUE),
    .groups = "drop"
  )
cat("Tenure (days) by Afrikaans status:\n")
print(as.data.frame(tenure_gap))
cat("  Expected: Afrikaans ~416 vs Non-Afrikaans ~631\n\n")

# Pre/post treaty counts
treaty_counts <- sac %>%
  group_by(post_vereeniging, afrikaans) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = afrikaans, values_from = n, names_prefix = "afr_")
cat("Pre/Post-Treaty counts:\n")
print(as.data.frame(treaty_counts))
cat("  Expected: ~10 Afrikaans pre-treaty\n\n")

# Discharge causes
cat("Discharge cause distribution:\n")
print(table(sac$discharge_cause_clean, useNA = "always"))
cat("\n")

# Summary statistics for key variables
cat("Key variable summaries:\n")
summary_vars <- sac %>%
  select(wage, lwage, age, height_cm, weight_kg, bmi, tenure_days,
         contract_years, character_score) %>%
  summary()
print(summary_vars)

# Save
cat("\nSaving sac_clean.rds...\n")
saveRDS(sac, out_file)
cat("Done! Saved", nrow(sac), "observations with", ncol(sac), "variables.\n")

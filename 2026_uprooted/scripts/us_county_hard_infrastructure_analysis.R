# US county hard-infrastructure robustness for the ancestry-SCI relationship
#
# Purpose:
#   Test whether the US county ancestry -> homeland SCI estimates survive
#   controls for historical communication and transport infrastructure.
#
# Inputs:
#   ../Data/all_region_to_country.zip
#   ../Data/acs_ancestry_county.rds
#   newdata/198825-V1.zip
#   newdata/RR_NetworkDatabase_DH_Oct2015.rar
#
# Outputs:
#   scripts/generated/us_hard_infrastructure/
#     - us_county_hard_infrastructure_controls.csv
#     - us_county_hard_infrastructure_coverage.csv
#     - us_county_hard_infrastructure_results.csv
#     - us_county_hard_infrastructure_diagnostics.txt
#
# Notes:
#   This script is intentionally separate from meta_history_v4.R. It does not
#   modify manuscript files and does not require network access.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(fixest)
  library(haven)
  library(readxl)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

find_project_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(cur, "Fourie_Meta.Rproj"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Could not find Fourie_Meta.Rproj walking upward from: ", start)
    }
    cur <- parent
  }
}

root_dir <- find_project_root()
submission_dir <- file.path(root_dir, "Submission")
data_dir <- file.path(root_dir, "Data")
newdata_dir <- file.path(submission_dir, "newdata")
out_dir <- file.path(submission_dir, "scripts", "generated", "us_hard_infrastructure")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

diagnostics <- character()
note <- function(...) {
  msg <- paste0(...)
  diagnostics <<- c(diagnostics, msg)
  message(msg)
}

extract_zip_file <- function(zipfile, member, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zipfile, files = member, exdir = exdir, overwrite = TRUE)
  file.path(exdir, member)
}

extract_rar_files_with_tar <- function(rarfile, members, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  args <- c("-xf", normalizePath(rarfile, winslash = "/", mustWork = TRUE),
            "-C", normalizePath(exdir, winslash = "/", mustWork = TRUE),
            members)
  status <- system2("tar", args = args)
  if (!identical(status, 0L)) {
    stop("tar failed while extracting railroad archive. Exit status: ", status)
  }
  file.path(exdir, members)
}

make_hist_fips <- function(state_fip, county_icp) {
  sprintf("%05.0f", state_fip * 1000 + county_icp / 10)
}

safe_log <- function(x) log(pmax(as.numeric(x), 1e-12))

note("Project root: ", root_dir)

# -------------------------------------------------------------------------
# 1. Load county-to-country SCI and cached ACS ancestry shares
# -------------------------------------------------------------------------

county_sci_zip <- file.path(data_dir, "all_region_to_country.zip")
acs_cache <- file.path(data_dir, "acs_ancestry_county.rds")

if (!file.exists(county_sci_zip)) stop("Missing SCI zip: ", county_sci_zip)
if (!file.exists(acs_cache)) stop("Missing ACS cache: ", acs_cache)

tmp_base <- file.path(out_dir, "_tmp_extract")
if (dir.exists(tmp_base)) unlink(tmp_base, recursive = TRUE, force = TRUE)
dir.create(tmp_base, recursive = TRUE)
on.exit(unlink(tmp_base, recursive = TRUE, force = TRUE), add = TRUE)

sci_csv <- extract_zip_file(county_sci_zip, "us_counties_to_country.csv",
                            file.path(tmp_base, "sci"))
sci_raw <- fread(sci_csv, na.strings = "")
sci_value_col <- if ("scaled_sci_2021" %in% names(sci_raw)) {
  "scaled_sci_2021"
} else if ("scaled_sci" %in% names(sci_raw)) {
  "scaled_sci"
} else {
  stop("Could not find SCI value column. Available columns: ",
       paste(names(sci_raw), collapse = ", "))
}
sci_county <- sci_raw |>
  rename(fips = user_region,
         partner_iso2 = friend_country,
         sci = all_of(sci_value_col)) |>
  transmute(
    fips = sprintf("%05d", as.integer(fips)),
    partner_iso2,
    log_sci = log(sci)
  )

acs_anc <- readRDS(acs_cache) |>
  mutate(fips = sprintf("%05d", as.integer(fips)))

note("SCI rows: ", nrow(sci_county),
     "; counties: ", n_distinct(sci_county$fips),
     "; partners: ", n_distinct(sci_county$partner_iso2))
note("ACS ancestry counties: ", nrow(acs_anc))

origin_map <- tribble(
  ~partner_iso2, ~anc_var,       ~label,
  "GB",          "pct_british",  "UK",
  "DE",          "pct_german",   "Germany",
  "IE",          "pct_irish",    "Ireland",
  "IT",          "pct_italian",  "Italy",
  "MX",          "pct_mexican",  "Mexico",
  "PL",          "pct_polish",   "Poland",
  "FR",          "pct_french",   "France",
  "CN",          "pct_chinese",  "China",
  "IN",          "pct_indian",   "India",
  "PH",          "pct_filipino", "Philippines"
)

# -------------------------------------------------------------------------
# 2. Build county hard-infrastructure controls from Aneja-Xu replication data
# -------------------------------------------------------------------------

postal_zip <- file.path(newdata_dir, "198825-V1.zip")
if (!file.exists(postal_zip)) stop("Missing postal archive: ", postal_zip)

postal_members <- c(
  "data/raw/post_office.dta",
  "data/raw/western_union.dta",
  "data/raw/census_county_1880.dta",
  "data/raw/census_county_1900.dta"
)
postal_paths <- extract_zip_file(postal_zip, postal_members,
                                 file.path(tmp_base, "postal"))
names(postal_paths) <- basename(postal_members)

post_office <- read_dta(postal_paths[["post_office.dta"]]) |>
  mutate(
    fips = sprintf("%05.0f", as.numeric(fips)),
    established = suppressWarnings(as.numeric(established))
  ) |>
  filter(!is.na(fips), fips != "   NA")

post_by_county <- post_office |>
  mutate(established_by_1900 = is.na(established) | established <= 1900) |>
  group_by(fips) |>
  summarise(
    n_post_offices = n(),
    n_post_offices_by_1900 = sum(established_by_1900, na.rm = TRUE),
    any_customs_port = as.integer(any(customs_port == 1, na.rm = TRUE)),
    .groups = "drop"
  )

western_union <- read_dta(postal_paths[["western_union.dta"]]) |>
  mutate(
    id = as.character(id),
    offices = suppressWarnings(as.numeric(offices))
  )

wu_by_county <- western_union |>
  left_join(post_office |>
              mutate(id = as.character(id)) |>
              select(id, fips) |>
              distinct(),
            by = "id") |>
  filter(!is.na(fips)) |>
  group_by(fips) |>
  summarise(
    n_western_union_cities = n_distinct(id),
    n_western_union_offices = sum(ifelse(is.na(offices), 1, offices), na.rm = TRUE),
    western_union_any = as.integer(n_western_union_cities > 0),
    .groups = "drop"
  )

census_1880 <- read_dta(postal_paths[["census_county_1880.dta"]]) |>
  transmute(
    fips = make_hist_fips(STATEFIP, COUNTYICP),
    share_literate_1880 = as.numeric(share_literate),
    emp_postal_1880 = as.numeric(employees_postal),
    emp_telegraph_1880 = as.numeric(employees_telegraph),
    emp_railway_1880 = as.numeric(employees_railway),
    mean_occscore_1880 = as.numeric(mean_occscore)
  )

census_1900 <- read_dta(postal_paths[["census_county_1900.dta"]]) |>
  transmute(
    fips = make_hist_fips(STATEFIP, COUNTYICP),
    share_literate_1900 = as.numeric(share_literate),
    emp_postal_1900 = as.numeric(employees_postal),
    emp_telegraph_1900 = as.numeric(employees_telegraph),
    emp_railway_1900 = as.numeric(employees_railway),
    mean_occscore_1900 = as.numeric(mean_occscore)
  )

postal_controls <- full_join(post_by_county, wu_by_county, by = "fips") |>
  full_join(census_1880, by = "fips") |>
  full_join(census_1900, by = "fips") |>
  mutate(
    across(c(n_post_offices, n_post_offices_by_1900, n_western_union_cities,
             n_western_union_offices, western_union_any, any_customs_port),
           ~ifelse(is.na(.x), 0, .x)),
    log_post_offices = log1p(n_post_offices_by_1900),
    log_western_union = log1p(n_western_union_offices),
    delta_literate = share_literate_1900 - share_literate_1880,
    delta_emp_postal = emp_postal_1900 - emp_postal_1880,
    delta_emp_telegraph = emp_telegraph_1900 - emp_telegraph_1880,
    delta_emp_railway = emp_railway_1900 - emp_railway_1880
  )

note("Postal controls counties: ", nrow(postal_controls),
     "; post-office counties: ", sum(postal_controls$n_post_offices_by_1900 > 0, na.rm = TRUE),
     "; Western Union counties: ", sum(postal_controls$western_union_any > 0, na.rm = TRUE))

# -------------------------------------------------------------------------
# 3. Build county rail/transport controls from Donaldson-Hornbeck archive
# -------------------------------------------------------------------------

rail_rar <- file.path(newdata_dir, "RR_NetworkDatabase_DH_Oct2015.rar")
if (!file.exists(rail_rar)) stop("Missing railroad archive: ", rail_rar)

rail_members <- c(
  "RR_NetworkDatabase_DH_Oct2015/Data/Transportation_Costs_AllDecades/NSFtranspCost.dta",
  "RR_NetworkDatabase_DH_Oct2015/Data/Transportation_Costs_1870_1890_allScenarios/Cost_ID_county.xlsx"
)
rail_paths <- extract_rar_files_with_tar(rail_rar, rail_members,
                                         file.path(tmp_base, "rail"))
names(rail_paths) <- basename(rail_members)

county_crosswalk <- read_excel(rail_paths[["Cost_ID_county.xlsx"]]) |>
  transmute(
    gis_id = as.numeric(`gis id`),
    fips = sprintf("%05.0f", as.numeric(ICPSRFIP))
  ) |>
  filter(!is.na(gis_id), !is.na(fips), fips != "   NA") |>
  distinct(gis_id, fips)

rail_costs <- read_dta(rail_paths[["NSFtranspCost.dta"]]) |>
  select(gisid_origin, gisid_destination, cost1890, cost_noRR) |>
  mutate(
    gisid_origin = as.numeric(gisid_origin),
    gisid_destination = as.numeric(gisid_destination),
    cost1890 = as.numeric(cost1890),
    cost_noRR = as.numeric(cost_noRR)
  )

origin_map_rail <- county_crosswalk |> rename(gisid_origin = gis_id, fips_origin = fips)
dest_map_rail <- county_crosswalk |> rename(gisid_destination = gis_id, fips_destination = fips)

rail_controls <- rail_costs |>
  inner_join(origin_map_rail, by = "gisid_origin") |>
  inner_join(dest_map_rail, by = "gisid_destination") |>
  filter(fips_origin != fips_destination,
         is.finite(cost1890), is.finite(cost_noRR)) |>
  group_by(fips = fips_origin) |>
  summarise(
    mean_cost1890 = mean(cost1890, na.rm = TRUE),
    mean_cost_no_rail = mean(cost_noRR, na.rm = TRUE),
    rail_access_1890 = mean(1 / (1 + cost1890), na.rm = TRUE),
    rail_access_no_rail = mean(1 / (1 + cost_noRR), na.rm = TRUE),
    rail_access_gain = rail_access_1890 - rail_access_no_rail,
    log_mean_cost1890 = log1p(mean_cost1890),
    log_mean_cost_no_rail = log1p(mean_cost_no_rail),
    log_cost_reduction_from_rail = log1p(mean_cost_no_rail) - log1p(mean_cost1890),
    .groups = "drop"
  )

note("Rail transport controls counties: ", nrow(rail_controls))

hard_controls <- full_join(postal_controls, rail_controls, by = "fips") |>
  mutate(
    fips = sprintf("%05d", as.integer(fips)),
    across(c(log_post_offices, log_western_union, n_post_offices_by_1900,
             n_western_union_offices, western_union_any, any_customs_port),
           ~ifelse(is.na(.x), 0, .x))
  )

fwrite(hard_controls,
       file.path(out_dir, "us_county_hard_infrastructure_controls.csv"))

note("Combined hard-infrastructure counties: ", nrow(hard_controls))

# -------------------------------------------------------------------------
# 4. Estimate county regressions with and without infrastructure controls
# -------------------------------------------------------------------------

summarise_model <- function(model, term, label, spec, n_counties_all) {
  ct <- summary(model)$coeftable
  tibble(
    label = label,
    spec = spec,
    term = term,
    beta = unname(ct[term, "Estimate"]),
    se = unname(ct[term, "Std. Error"]),
    p_value = unname(ct[term, "Pr(>|t|)"]),
    n_obs = nobs(model),
    n_counties_before_filter = n_counties_all,
    r2_within = tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA_real_)
  )
}

results <- list()
coverage <- list()

infra_vars <- c(
  "log_post_offices",
  "log_western_union",
  "share_literate_1880",
  "emp_postal_1880",
  "emp_telegraph_1880",
  "emp_railway_1880"
)

rail_vars <- c("rail_access_1890", "log_cost_reduction_from_rail")

for (i in seq_len(nrow(origin_map))) {
  partner <- origin_map$partner_iso2[i]
  anc_col <- origin_map$anc_var[i]
  lbl <- origin_map$label[i]

  sci_partner <- sci_county |>
    filter(partner_iso2 == partner) |>
    select(fips, log_sci)

  county_df <- acs_anc |>
    inner_join(sci_partner, by = "fips") |>
    left_join(hard_controls, by = "fips") |>
    filter(!is.na(.data[[anc_col]]), .data[[anc_col]] > 0) |>
    mutate(
      log_anc_pct = safe_log(.data[[anc_col]]),
      state_fips = substr(fips, 1, 2)
    )

  coverage[[lbl]] <- tibble(
    label = lbl,
    partner_iso2 = partner,
    n_base = nrow(county_df),
    n_postal_complete = sum(complete.cases(county_df[, infra_vars])),
    n_rail_complete = sum(complete.cases(county_df[, rail_vars])),
    n_full_complete = sum(complete.cases(county_df[, c(infra_vars, rail_vars)]))
  )

  if (nrow(county_df) <= 50) next

  base_model <- feols(log_sci ~ log_anc_pct + log_pop | state_fips,
                      data = county_df, vcov = ~state_fips)

  postal_model <- feols(
    log_sci ~ log_anc_pct + log_pop +
      log_post_offices + log_western_union + share_literate_1880 +
      emp_postal_1880 + emp_telegraph_1880 + emp_railway_1880 |
      state_fips,
    data = county_df, vcov = ~state_fips
  )

  rail_model <- feols(
    log_sci ~ log_anc_pct + log_pop +
      rail_access_1890 + log_cost_reduction_from_rail |
      state_fips,
    data = county_df, vcov = ~state_fips
  )

  full_model <- feols(
    log_sci ~ log_anc_pct + log_pop +
      log_post_offices + log_western_union + share_literate_1880 +
      emp_postal_1880 + emp_telegraph_1880 + emp_railway_1880 +
      rail_access_1890 + log_cost_reduction_from_rail |
      state_fips,
    data = county_df, vcov = ~state_fips
  )

  results[[paste(lbl, "base")]] <- summarise_model(base_model, "log_anc_pct", lbl, "base", nrow(county_df))
  results[[paste(lbl, "postal")]] <- summarise_model(postal_model, "log_anc_pct", lbl, "postal_controls", nrow(county_df))
  results[[paste(lbl, "rail")]] <- summarise_model(rail_model, "log_anc_pct", lbl, "rail_controls", nrow(county_df))
  results[[paste(lbl, "full")]] <- summarise_model(full_model, "log_anc_pct", lbl, "postal_plus_rail", nrow(county_df))
}

results_df <- bind_rows(results) |>
  left_join(bind_rows(coverage), by = "label") |>
  group_by(label) |>
  mutate(
    beta_base = beta[spec == "base"][1],
    retention_vs_base = beta / beta_base
  ) |>
  ungroup() |>
  select(label, partner_iso2, spec, beta, se, p_value, retention_vs_base,
         n_obs, n_base, n_postal_complete, n_rail_complete, n_full_complete,
         r2_within)

fwrite(results_df,
       file.path(out_dir, "us_county_hard_infrastructure_results.csv"))

coverage_df <- bind_rows(coverage) |>
  mutate(estimated = label %in% results_df$label)

fwrite(coverage_df,
       file.path(out_dir, "us_county_hard_infrastructure_coverage.csv"))

# A compact human-readable diagnostic report.
sig_summary <- results_df |>
  group_by(spec) |>
  summarise(
    n_origins = n(),
    n_positive = sum(beta > 0, na.rm = TRUE),
    n_p_lt_05 = sum(p_value < 0.05, na.rm = TRUE),
    mean_beta = mean(beta, na.rm = TRUE),
    median_retention = median(retention_vs_base, na.rm = TRUE),
    .groups = "drop"
  )

diag_lines <- c(
  diagnostics,
  "",
  "Specification summary:",
  capture.output(print(sig_summary)),
  "",
  "Coverage by origin:",
  capture.output(print(coverage_df, n = Inf)),
  "",
  "Origin-level ancestry coefficient results:",
  capture.output(print(results_df |> arrange(label, spec), n = Inf))
)

writeLines(diag_lines,
           file.path(out_dir, "us_county_hard_infrastructure_diagnostics.txt"))

note("Wrote results to: ", out_dir)

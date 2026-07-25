library(data.table)
library(dplyr)
library(readxl)
library(tidyr)
library(fixest)

root_dir <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta"
data_dir <- file.path(root_dir, "Data")
wp_dir <- file.path(root_dir, "WorkingPaperNew")
script_dir <- file.path(wp_dir, "scripts")
gen_dir <- file.path(script_dir, "generated")

dir.create(gen_dir, recursive = TRUE, showWarnings = FALSE)

read_main_analysis <- function() {
  sci_tmp <- tempfile(fileext = ".csv")
  unzip(file.path(data_dir, "country.zip"), files = "country.csv", exdir = dirname(sci_tmp))
  file.rename(file.path(dirname(sci_tmp), "country.csv"), sci_tmp)
  sci_raw <- fread(sci_tmp, na.strings = "")
  file.remove(sci_tmp)

  sci <- sci_raw |>
    filter(user_country < friend_country) |>
    select(iso2_i = user_country, iso2_j = friend_country, scaled_sci) |>
    mutate(log_sci = log(scaled_sci))

  putt_raw <- read_excel(file.path(data_dir, "matrix version 1.1.xls"))
  origin_codes <- names(putt_raw)[3:(ncol(putt_raw) - 1)]
  putt_long <- putt_raw |>
    select(-update) |>
    tidyr::pivot_longer(
      cols = all_of(origin_codes),
      names_to = "origin_wb",
      values_to = "ancestry_share"
    ) |>
    rename(dest_wb = wbcode) |>
    mutate(
      origin_wb = toupper(origin_wb),
      ancestry_share = as.numeric(ancestry_share)
    ) |>
    filter(!is.na(ancestry_share))

  geo <- read_excel(file.path(data_dir, "geo_cepii.xls")) |>
    select(iso2, iso3) |>
    filter(!is.na(iso2), !is.na(iso3), iso2 != ".", iso3 != ".") |>
    distinct(iso2, .keep_all = TRUE)

  geo <- bind_rows(
    geo,
    tibble::tribble(
      ~iso2, ~iso3,
      "ME",  "MNE",
      "RS",  "SRB",
      "SS",  "SSD",
      "TL",  "TLS",
      "XK",  "XKX",
      "CD",  "COD"
    )
  ) |>
    distinct(iso2, .keep_all = TRUE)

  sci_iso3 <- sci |>
    left_join(geo, by = c("iso2_i" = "iso2")) |>
    rename(iso3_i = iso3) |>
    left_join(geo, by = c("iso2_j" = "iso2")) |>
    rename(iso3_j = iso3) |>
    filter(!is.na(iso3_i), !is.na(iso3_j))

  ancestry_pairs <- putt_long |>
    select(origin = origin_wb, dest = dest_wb, share = ancestry_share)

  analysis <- sci_iso3 |>
    left_join(ancestry_pairs, by = c("iso3_i" = "origin", "iso3_j" = "dest")) |>
    rename(anc_ij = share) |>
    left_join(ancestry_pairs, by = c("iso3_j" = "origin", "iso3_i" = "dest")) |>
    rename(anc_ji = share) |>
    mutate(
      anc_ij = dplyr::coalesce(anc_ij, 0),
      anc_ji = dplyr::coalesce(anc_ji, 0),
      anc_max = pmax(anc_ij, anc_ji),
      anc_log = log(1 + 1000 * anc_max)
    )

  grav_tmp <- tempfile(fileext = ".csv")
  unzip(file.path(data_dir, "Gravity_csv_V202211.zip"),
        files = "Gravity_V202211.csv", exdir = dirname(grav_tmp))
  file.rename(file.path(dirname(grav_tmp), "Gravity_V202211.csv"), grav_tmp)
  gravity_all <- fread(
    grav_tmp,
    select = c("year", "iso3_o", "iso3_d", "dist", "contig",
               "comlang_off", "col_dep_ever", "pop_o", "pop_d",
               "gdp_o", "gdp_d"),
    showProgress = FALSE
  )
  file.remove(grav_tmp)

  gravity <- gravity_all |>
    filter(year == 2021) |>
    distinct(iso3_o, iso3_d, .keep_all = TRUE) |>
    select(-year)

  analysis_fwd <- analysis |>
    left_join(
      gravity,
      by = c("iso3_i" = "iso3_o", "iso3_j" = "iso3_d")
    )

  unmatched <- analysis_fwd |>
    filter(is.na(dist)) |>
    select(iso2_i, iso2_j, iso3_i, iso3_j, scaled_sci, log_sci, anc_ij, anc_ji, anc_max, anc_log)

  matched_rev <- unmatched |>
    left_join(
      gravity,
      by = c("iso3_j" = "iso3_o", "iso3_i" = "iso3_d")
    )

  analysis_full <- bind_rows(
    analysis_fwd |>
      filter(!is.na(dist)),
    matched_rev
  ) |>
    mutate(
      log_dist = log(dist),
      pair_id = paste0(pmin(iso3_i, iso3_j), "_", pmax(iso3_i, iso3_j))
    ) |>
    filter(!is.na(log_sci), !is.na(log_dist), is.finite(log_sci), is.finite(log_dist))

  as.data.table(analysis_full)
}

prepare_sym_corridors <- function(path, prefix) {
  dt <- fread(path)
  setDT(dt)
  dt_sym <- rbindlist(list(
    dt[, .(iso3_a = origin_iso3, iso3_b = dest_iso3, slave_flow = slave_flow_disembarked)],
    dt[, .(iso3_a = dest_iso3, iso3_b = origin_iso3, slave_flow = slave_flow_disembarked)]
  ))

  out <- dt_sym[, .(slave_flow = sum(slave_flow, na.rm = TRUE)), by = .(iso3_a, iso3_b)][
    iso3_a < iso3_b
  ]
  setnames(out, c("iso3_a", "iso3_b", "slave_flow"), c("iso3_i", "iso3_j", paste0(prefix, "_flow")))
  out[, (paste0(prefix, "_log")) := log1p(get(paste0(prefix, "_flow")))]
  list(
    sym = out,
    raw = dt
  )
}

estimate_models <- function(df, slave_var, label) {
  rhs_base <- paste0(slave_var, " + log_dist + contig + comlang_off + col_dep_ever | iso3_i + iso3_j")
  rhs_hr <- paste0("anc_log + ", slave_var, " + log_dist + contig + comlang_off + col_dep_ever | iso3_i + iso3_j")
  rhs_anc <- "anc_log + log_dist + contig + comlang_off + col_dep_ever | iso3_i + iso3_j"
  rhs_hr_asinh <- paste0("asinh_sci ~ anc_log + ", slave_var, " + log_dist + contig + comlang_off + col_dep_ever | iso3_i + iso3_j")

  m_slave <- feols(as.formula(paste("log_sci ~", rhs_base)), data = df, vcov = ~iso3_i + iso3_j)
  m_hr <- feols(as.formula(paste("log_sci ~", rhs_hr)), data = df, vcov = ~iso3_i + iso3_j)
  m_anc <- feols(as.formula(paste("log_sci ~", rhs_anc)), data = df, vcov = ~iso3_i + iso3_j)
  m_hr_asinh <- feols(as.formula(rhs_hr_asinh), data = df, vcov = ~iso3_i + iso3_j)

  pos_df <- df[get(slave_var) > 0]
  m_anc_pos <- if (nrow(pos_df) > 50) {
    feols(as.formula(paste("log_sci ~", rhs_anc)), data = pos_df, vcov = ~iso3_i + iso3_j)
  } else NULL
  m_slave_pos <- if (nrow(pos_df) > 50) {
    feols(as.formula(paste("log_sci ~", rhs_base)), data = pos_df, vcov = ~iso3_i + iso3_j)
  } else NULL
  m_hr_pos <- if (nrow(pos_df) > 50) {
    feols(as.formula(paste("log_sci ~", rhs_hr)), data = pos_df, vcov = ~iso3_i + iso3_j)
  } else NULL
  m_hr_pos_asinh <- if (nrow(pos_df) > 50) {
    feols(as.formula(rhs_hr_asinh), data = pos_df, vcov = ~iso3_i + iso3_j)
  } else NULL

  list(label = label, data = df, slave_var = slave_var, m_anc = m_anc, m_slave = m_slave, m_hr = m_hr,
       m_hr_asinh = m_hr_asinh, m_anc_pos = m_anc_pos, m_slave_pos = m_slave_pos,
       m_hr_pos = m_hr_pos, m_hr_pos_asinh = m_hr_pos_asinh)
}

coef_row <- function(model, term, spec, sample_label, n_pairs, n_positive) {
  if (is.null(model) || !(term %in% names(coef(model)))) return(NULL)
  data.table(
    sample = sample_label,
    spec = spec,
    term = term,
    beta = unname(coef(model)[term]),
    se = unname(se(model)[term]),
    p_value = unname(pvalue(model)[term]),
    n = nobs(model),
    wr2 = tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA_real_),
    total_pairs = n_pairs,
    positive_slave_pairs = n_positive
  )
}

analysis_clean <- read_main_analysis()
analysis_clean[, asinh_sci := asinh(scaled_sci)]

ta_corr <- prepare_sym_corridors(
  file.path(gen_dir, "slave_corridors_transatlantic_country_crossborder.csv"),
  "slave_ta"
)
ia_corr <- prepare_sym_corridors(
  file.path(gen_dir, "slave_corridors_intraamerican_country_crossborder.csv"),
  "slave_ia"
)

analysis_slave <- merge(analysis_clean, ta_corr$sym, by = c("iso3_i", "iso3_j"), all.x = TRUE)
analysis_slave <- merge(analysis_slave, ia_corr$sym, by = c("iso3_i", "iso3_j"), all.x = TRUE)
analysis_slave[, slave_ta_flow := fifelse(is.na(slave_ta_flow), 0, slave_ta_flow)]
analysis_slave[, slave_ta_log := fifelse(is.na(slave_ta_log), 0, slave_ta_log)]
analysis_slave[, slave_ia_flow := fifelse(is.na(slave_ia_flow), 0, slave_ia_flow)]
analysis_slave[, slave_ia_log := fifelse(is.na(slave_ia_log), 0, slave_ia_log)]

ta_origin_set <- unique(ta_corr$raw$origin_iso3)
ta_dest_set <- unique(ta_corr$raw$dest_iso3)
ia_nodes <- unique(c(ia_corr$raw$origin_iso3, ia_corr$raw$dest_iso3))

ta_sample <- analysis_slave[
  (iso3_i %in% ta_origin_set & iso3_j %in% ta_dest_set) |
    (iso3_j %in% ta_origin_set & iso3_i %in% ta_dest_set)
]

ia_sample <- analysis_slave[
  iso3_i %in% ia_nodes & iso3_j %in% ia_nodes
]

top_destinations <- ta_corr$raw[
  (origin_iso3 %in% ta_origin_set & dest_iso3 %in% ta_dest_set) |
    (dest_iso3 %in% ta_origin_set & origin_iso3 %in% ta_dest_set)
][, .(slave_flow = sum(slave_flow_disembarked, na.rm = TRUE)), by = dest_iso3][order(-slave_flow)][1:4]
ta_leave_one_out <- rbindlist(lapply(top_destinations$dest_iso3, function(drop_dest) {
  df <- ta_sample[iso3_i != drop_dest & iso3_j != drop_dest]
  if (nrow(df[slave_ta_flow > 0]) < 50) return(NULL)
  m <- feols(log_sci ~ anc_log + slave_ta_log + log_dist + contig + comlang_off + col_dep_ever | iso3_i + iso3_j,
             data = df, vcov = ~iso3_i + iso3_j)
  data.table(
    sample = "trans_atlantic",
    spec = paste0("horse_race_drop_", drop_dest),
    term = "slave_ta_log",
    beta = unname(coef(m)["slave_ta_log"]),
    se = unname(se(m)["slave_ta_log"]),
    p_value = unname(pvalue(m)["slave_ta_log"]),
    n = nobs(m),
    wr2 = tryCatch(fitstat(m, "wr2")[[1]], error = function(e) NA_real_),
    total_pairs = nrow(df),
    positive_slave_pairs = sum(df$slave_ta_flow > 0)
  )
}), use.names = TRUE, fill = TRUE)
ta_leave_one_out <- ta_leave_one_out[total_pairs < nrow(ta_sample)]

ta_models <- estimate_models(ta_sample, "slave_ta_log", "trans_atlantic")
ia_models <- estimate_models(ia_sample, "slave_ia_log", "intra_american")

results_tbl <- rbindlist(list(
  coef_row(ta_models$m_anc, "anc_log", "ancestry_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_slave, "slave_ta_log", "slave_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr, "anc_log", "horse_race", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr, "slave_ta_log", "horse_race", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr_asinh, "slave_ta_log", "horse_race_asinh", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_slave_pos, "slave_ta_log", "slave_only_positive_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_anc_pos, "anc_log", "ancestry_only_positive_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr_pos, "anc_log", "horse_race_positive_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr_pos, "slave_ta_log", "horse_race_positive_only", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ta_models$m_hr_pos_asinh, "slave_ta_log", "horse_race_positive_only_asinh", "trans_atlantic", nrow(ta_sample), sum(ta_sample$slave_ta_flow > 0)),
  coef_row(ia_models$m_anc, "anc_log", "ancestry_only", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_slave, "slave_ia_log", "slave_only", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr, "anc_log", "horse_race", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr, "slave_ia_log", "horse_race", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr_asinh, "slave_ia_log", "horse_race_asinh", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_slave_pos, "slave_ia_log", "slave_only_positive_only", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr_pos, "anc_log", "horse_race_positive_only", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr_pos, "slave_ia_log", "horse_race_positive_only", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  coef_row(ia_models$m_hr_pos_asinh, "slave_ia_log", "horse_race_positive_only_asinh", "intra_american", nrow(ia_sample), sum(ia_sample$slave_ia_flow > 0)),
  ta_leave_one_out
), use.names = TRUE, fill = TRUE)

fwrite(analysis_slave, file.path(gen_dir, "analysis_country_pairs_with_slave_corridors.csv"))
fwrite(results_tbl, file.path(gen_dir, "slavevoyages_country_model_results.csv"))
saveRDS(
  list(
    ta_models = ta_models,
    ia_models = ia_models,
    ta_sample_n = nrow(ta_sample),
    ia_sample_n = nrow(ia_sample),
    ta_positive_n = sum(ta_sample$slave_ta_flow > 0),
    ia_positive_n = sum(ia_sample$slave_ia_flow > 0)
  ),
  file.path(gen_dir, "slavevoyages_country_models.rds")
)

md <- c(
  "# Slave Voyages Country-Level Models",
  "",
  "This file was generated by `run_slavevoyages_country_models.R`.",
  "",
  "## Samples",
  "",
  paste0("- Trans-Atlantic sample: ", nrow(ta_sample), " dyads; ", sum(ta_sample$slave_ta_flow > 0), " with positive slave-corridor exposure."),
  paste0("- Intra-American sample: ", nrow(ia_sample), " dyads; ", sum(ia_sample$slave_ia_flow > 0), " with positive slave-corridor exposure."),
  paste0("- Top trans-Atlantic destinations used for leave-one-out checks: ", paste(top_destinations$dest_iso3, collapse = ", "), "."),
  "",
  "## Key Coefficients",
  "",
  "| sample | spec | term | beta | se | p-value | n |",
  "|---|---|---|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(results_tbl))) {
  row <- results_tbl[i]
  md <- c(md, paste0(
    "| ", row$sample, " | ", row$spec, " | ", row$term, " | ",
    sprintf("%.4f", row$beta), " | ",
    sprintf("%.4f", row$se), " | ",
    sprintf("%.4f", row$p_value), " | ",
    row$n, " |"
  ))
}

md <- c(
  md,
  "",
  "## Model Notes",
  "",
  "- All reported models are `feols` specifications with `iso3_i + iso3_j` fixed effects and two-way clustering on the same identifiers.",
  "- `slave_only` uses only the slave-corridor variable plus the gravity controls.",
  "- `horse_race` adds `anc_log` to ask whether the slave-corridor measure has explanatory power over and above ancestry.",
  "- `positive_only` restricts to dyads with strictly positive mapped slave-corridor exposure.",
  "- `horse_race_asinh` repeats the main horse race with `asinh(SCI)` to mirror the paper's alternative-dependent-variable logic.",
  "- `horse_race_drop_*` drops the largest trans-Atlantic destinations one at a time to check whether the corridor result is driven by a single destination."
)

writeLines(md, file.path(gen_dir, "slavevoyages_country_model_summary.md"))

cat("Wrote:\n")
cat(file.path(gen_dir, "analysis_country_pairs_with_slave_corridors.csv"), "\n")
cat(file.path(gen_dir, "slavevoyages_country_model_results.csv"), "\n")
cat(file.path(gen_dir, "slavevoyages_country_model_summary.md"), "\n")
cat(file.path(gen_dir, "slavevoyages_country_models.rds"), "\n")

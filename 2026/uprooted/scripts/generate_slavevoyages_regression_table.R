library(fixest)
library(data.table)

wp_dir <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta/WorkingPaperNew"
gen_dir <- file.path(wp_dir, "scripts", "generated")
models_path <- file.path(gen_dir, "slavevoyages_country_models.rds")

obj <- readRDS(models_path)

table_tex <- file.path(gen_dir, "table_slavevoyages_country_models.tex")
table_md <- file.path(gen_dir, "table_slavevoyages_country_models.md")

etable(
  obj$ta_models$m_slave,
  obj$ta_models$m_hr,
  obj$ta_models$m_hr_pos,
  obj$ia_models$m_slave,
  obj$ia_models$m_hr,
  tex = TRUE,
  file = table_tex,
  replace = TRUE,
  digits = 3,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  headers = list(
    " " = c("Trans-Atlantic", "Trans-Atlantic", "Trans-Atlantic", "Intra-American", "Intra-American")
  ),
  fixef.group = list("Country FE" = c("iso3_i", "iso3_j")),
  dict = c(
    log_sci = "log(SCI)",
    slave_ta_log = "log(1 + slave corridor)",
    slave_ia_log = "log(1 + slave corridor)",
    anc_log = "log shared ancestry",
    log_dist = "log(distance)",
    contig = "Contiguous",
    comlang_off = "Common language",
    col_dep_ever = "Colonial tie"
  ),
  keep = c("%slave_ta_log", "%slave_ia_log", "%anc_log"),
  fitstat = ~ n + wr2,
  notes = c(
    "Dependent variable: log(SCI).",
    "Columns 1-3 use the Africa-Americas trans-Atlantic sample; columns 4-5 use the Americas-only intra-American sample.",
    "Column 3 restricts the trans-Atlantic sample to dyads with strictly positive mapped slave-corridor exposure.",
    "All specifications include gravity controls and country fixed effects, with standard errors clustered on the two country identifiers."
  ),
  title = "Slave-trade corridors and modern social connectedness"
)

lines <- c(
  "# Slave-Trade Corridor Regressions",
  "",
  "This file is a companion summary to `table_slavevoyages_country_models.tex`.",
  "",
  "| Column | Sample | Specification | Key coefficient | Beta | SE | p-value |",
  "|---|---|---|---|---:|---:|---:|",
  paste0("| (1) | Trans-Atlantic | Slave corridor only | log(1 + slave corridor) | ",
         sprintf("%.3f", coef(obj$ta_models$m_slave)["slave_ta_log"]), " | ",
         sprintf("%.3f", se(obj$ta_models$m_slave)["slave_ta_log"]), " | ",
         sprintf("%.3f", pvalue(obj$ta_models$m_slave)["slave_ta_log"]), " |"),
  paste0("| (2) | Trans-Atlantic | Horse race | log(1 + slave corridor) | ",
         sprintf("%.3f", coef(obj$ta_models$m_hr)["slave_ta_log"]), " | ",
         sprintf("%.3f", se(obj$ta_models$m_hr)["slave_ta_log"]), " | ",
         sprintf("%.3f", pvalue(obj$ta_models$m_hr)["slave_ta_log"]), " |"),
  paste0("| (3) | Trans-Atlantic positive only | Horse race | log(1 + slave corridor) | ",
         sprintf("%.3f", coef(obj$ta_models$m_hr_pos)["slave_ta_log"]), " | ",
         sprintf("%.3f", se(obj$ta_models$m_hr_pos)["slave_ta_log"]), " | ",
         sprintf("%.3f", pvalue(obj$ta_models$m_hr_pos)["slave_ta_log"]), " |"),
  paste0("| (4) | Intra-American | Slave corridor only | log(1 + slave corridor) | ",
         sprintf("%.3f", coef(obj$ia_models$m_slave)["slave_ia_log"]), " | ",
         sprintf("%.3f", se(obj$ia_models$m_slave)["slave_ia_log"]), " | ",
         sprintf("%.3f", pvalue(obj$ia_models$m_slave)["slave_ia_log"]), " |"),
  paste0("| (5) | Intra-American | Horse race | log(1 + slave corridor) | ",
         sprintf("%.3f", coef(obj$ia_models$m_hr)["slave_ia_log"]), " | ",
         sprintf("%.3f", se(obj$ia_models$m_hr)["slave_ia_log"]), " | ",
         sprintf("%.3f", pvalue(obj$ia_models$m_hr)["slave_ia_log"]), " |"),
  "",
  paste0("- Trans-Atlantic sample size: ", obj$ta_sample_n, " dyads; ", obj$ta_positive_n, " with positive mapped exposure."),
  paste0("- Intra-American sample size: ", obj$ia_sample_n, " dyads; ", obj$ia_positive_n, " with positive mapped exposure.")
)

writeLines(lines, table_md)

cat("Wrote:\n")
cat(table_tex, "\n")
cat(table_md, "\n")

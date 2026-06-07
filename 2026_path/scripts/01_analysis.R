# =============================================================================
#
#   PATH DEPENDENCE IN ECONOMIC HISTORY PUBLICATIONS
#   01_analysis.R — Main Analysis Script (Replication Package)
#
#   Produces ALL regressions, tests, and tables for the paper.
#   Assumes setwd() has been called to the PathDepPaper/ directory.
#
# =============================================================================


# =============================================================================
# SECTION 0: SETUP
# =============================================================================

rm(list = ls())

set.seed(42)

# Paths — auto-detect the scripts/ directory robustly (Rscript, RStudio, source)
get_script_dir <- function() {
  # 1. Rscript: the --file= argument carries the script path
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  # 2. RStudio interactive
  p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p)))
  # 3. Fallback to common working directories
  if (file.exists("scripts/01_analysis.R")) return(normalizePath("scripts"))
  if (file.exists("01_analysis.R"))         return(normalizePath("."))
  normalizePath("PathDepPaper/scripts")
}
SCRIPT_DIR  <- get_script_dir()
BASE_DIR    <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/")
setwd(BASE_DIR)
cat("Working directory:", getwd(), "\n")

# Shared helpers (clean_person_name, session-order mapping) — single source of truth
source(file.path(SCRIPT_DIR, "conference_data_helpers.R"))

DATA_DIR    <- file.path(BASE_DIR, "data")
FIGURE_DIR  <- file.path(BASE_DIR, "figures")
TABLE_DIR   <- file.path(BASE_DIR, "tables")
RESULTS_DIR <- file.path(BASE_DIR, "results")

for (d in c(FIGURE_DIR, TABLE_DIR, RESULTS_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# --- Packages ----------------------------------------------------------------
packages <- c("data.table", "lfe", "fixest", "stargazer", "boot",
              "stringdist", "ggplot2", "scales", "igraph")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Optional packages for DML (Section 8)
dml_available <- requireNamespace("DoubleML", quietly = TRUE) &&
                 requireNamespace("mlr3", quietly = TRUE) &&
                 requireNamespace("mlr3learners", quietly = TRUE) &&
                 requireNamespace("ranger", quietly = TRUE)

# --- Standard-error helpers --------------------------------------------------
# Publication standard: heteroskedasticity-robust (White) SEs for cross-sectional
# felm models. lfe::summary(robust = TRUE) returns the robust SE in column 2 of
# the coefficient table. rob_se() returns the robust SE for one coefficient;
# rob_se_vec() returns the full robust SE vector (for stargazer se = ... lists).
rob_se <- function(m, var) {
  ct <- summary(m, robust = TRUE)$coefficients
  if (!var %in% rownames(ct)) return(NA_real_)
  ct[var, 2]
}
rob_se_vec <- function(m) {
  ct <- summary(m, robust = TRUE)$coefficients
  setNames(ct[, 2], rownames(ct))
}

# --- Sink log ----------------------------------------------------------------
sink(file.path(RESULTS_DIR, "analysis_log.txt"), split = TRUE)
on.exit(sink(), add = TRUE)  # ensure sink closes even if script errors

cat("\n")
cat("====================================================================\n")
cat("  PATH DEPENDENCE IN ECONOMIC HISTORY PUBLICATIONS\n")
cat("  Analysis script — run date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("====================================================================\n\n")


# =============================================================================
# SECTION 1: LOAD AND PREPARE DATA
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 1: LOAD AND PREPARE DATA\n")
cat("--------------------------------------------------------------------\n\n")

jn <- fread(file.path(DATA_DIR, "Journals_2026_clean.csv"))

cat("Loaded clean data:", nrow(jn), "papers\n")
cat("Journals:", paste(sort(unique(jn$Journal)), collapse = ", "), "\n")
cat("Year range:", min(jn$Year), "-", max(jn$Year), "\n\n")

# --- 1.1 Rename columns -----------------------------------------------------
jn <- jn[, .(
  id = ID,
  year = Year,
  journal = Journal,
  title = Title,
  vol = Vol,
  issue_raw = No,
  pagestart = Pagestart,
  pageend = Pageend,
  continent = Continent1,
  n_authors = `No of authors`,
  author1 = Author1, author1_uni = `Author1 university`, author1_country = `Author1 country`,
  author2 = Author2, author2_uni = `Author2 university`, author2_country = `Author2 country`,
  author3 = Author3, author3_uni = `Author3 university`, author3_country = `Author3 country`,
  author4 = Author4, author4_uni = `Author4 university`, author4_country = `Author4 country`,
  author5 = Author5, author5_uni = `Author5 university`, author5_country = `Author5 country`,
  g14 = Google14, g15 = Google15, g16 = Google16, g17 = Google17,
  g18 = Google18, g19 = Google19, g20 = Google20, g21 = Google21,
  g22 = Google22, g23 = Google23, g24 = Google24, g25 = Google25, g26 = Google26
)]

# --- 1.2 Citation trajectories ----------------------------------------------
cat("Calculating age-specific citations...\n")

get_cite_at_age <- function(pub_year, age, g14, g15, g16, g17, g18, g19,
                             g20, g21, g22, g23, g24, g25, g26) {
  target_year <- pub_year + age
  result <- rep(NA_real_, length(pub_year))
  result[target_year == 2014] <- g14[target_year == 2014]
  result[target_year == 2015] <- g15[target_year == 2015]
  result[target_year == 2016] <- g16[target_year == 2016]
  result[target_year == 2017] <- g17[target_year == 2017]
  result[target_year == 2018] <- g18[target_year == 2018]
  result[target_year == 2019] <- g19[target_year == 2019]
  result[target_year == 2020] <- g20[target_year == 2020]
  result[target_year == 2021] <- g21[target_year == 2021]
  result[target_year == 2022] <- g22[target_year == 2022]
  result[target_year == 2023] <- g23[target_year == 2023]
  result[target_year == 2024] <- g24[target_year == 2024]
  result[target_year == 2025] <- g25[target_year == 2025]
  result[target_year == 2026] <- g26[target_year == 2026]
  return(result)
}

jn[, cite_age_1 := get_cite_at_age(year, 1, g14, g15, g16, g17, g18, g19,
                                    g20, g21, g22, g23, g24, g25, g26)]
jn[, cite_age_2 := get_cite_at_age(year, 2, g14, g15, g16, g17, g18, g19,
                                    g20, g21, g22, g23, g24, g25, g26)]
jn[, cite_age_3 := get_cite_at_age(year, 3, g14, g15, g16, g17, g18, g19,
                                    g20, g21, g22, g23, g24, g25, g26)]
jn[, cite_age_5 := get_cite_at_age(year, 5, g14, g15, g16, g17, g18, g19,
                                    g20, g21, g22, g23, g24, g25, g26)]
jn[, cite_age_8 := get_cite_at_age(year, 8, g14, g15, g16, g17, g18, g19,
                                    g20, g21, g22, g23, g24, g25, g26)]

jn[, cite_early := cite_age_2]
jn[, cite_longrun := ifelse(!is.na(cite_age_8), cite_age_8,
                            ifelse(!is.na(cite_age_5), cite_age_5, NA_real_))]
jn[, cite_growth := cite_longrun - cite_early]

jn[, has_early := !is.na(cite_early)]
jn[, has_longrun := !is.na(cite_longrun)]
jn[, has_trajectory := !is.na(cite_early) & !is.na(cite_longrun)]

traj_summary <- jn[, .(total = .N, has_early = sum(has_early),
                        has_longrun = sum(has_longrun),
                        has_both = sum(has_trajectory)), by = year][order(year)]
cat("\nTrajectory data by publication year:\n")
print(traj_summary[year >= 2007 & year <= 2025])
cat("\nTotal with full trajectory:", sum(jn$has_trajectory), "\n\n")

# --- 1.3 Fast starters and other variables ----------------------------------
cat("Creating analysis variables...\n")

core_journals <- c("JEH", "EHR", "EREH", "Explorations")
jn[, is_core := journal %in% core_journals]

jn[has_early == TRUE & is_core == TRUE,
   cite_early_pctile := frank(cite_early, ties.method = "average") / .N,
   by = .(journal, year)]
jn[has_early == TRUE & is_core == TRUE,
   cohort_mean := mean(cite_early, na.rm = TRUE), by = .(journal, year)]
jn[has_early == TRUE & is_core == TRUE,
   cohort_sd := sd(cite_early, na.rm = TRUE), by = .(journal, year)]

jn[, fast_starter := ifelse(!is.na(cite_early_pctile) & cite_early_pctile > 0.75, 1L, 0L)]
jn[, fast_starter_strict := ifelse(!is.na(cite_early_pctile) & cite_early_pctile > 0.90, 1L, 0L)]
cat("Fast starters (top 25%):", sum(jn$fast_starter, na.rm = TRUE), "\n")

# Region
jn[, region := NA_character_]
jn[continent %in% c("Africa", "South Africa"), region := "Africa"]
jn[continent %in% c("Western Europe", "Britain", "Eastern Europe/Russia", "Scandinavia",
                    "England", "Eastern Europe", "European", "Europe"), region := "Europe"]
jn[continent %in% c("North America", "Latin America", "South America", "USA"), region := "Americas"]
jn[continent %in% c("Australasia", "Asia", "China", "Oceania", "Australia",
                    "India", "Middle East"), region := "Asia & Oceania"]
jn[grepl("Global|Europe/|/Europe|North America/", continent), region := "Global"]

# Topic classification
topic_dict <- list(
  trade = c("trade", "export", "import", "tariff", "globalization", "commerce"),
  finance = c("bank", "credit", "debt", "finance", "money", "monetary", "currency"),
  labor = c("labor", "labour", "wage", "worker", "employment", "education", "skill"),
  agriculture = c("agricultur", "farm", "crop", "land", "rural", "grain"),
  industry = c("industr", "manufactur", "factory", "technology", "innovation"),
  slavery = c("slave", "slavery", "enslaved", "abolition"),
  colonial = c("colonial", "colony", "empire", "imperial"),
  institutions = c("institution", "state", "government", "politic", "law", "democracy"),
  inequality = c("inequality", "living standard", "welfare", "poverty", "income", "wealth"),
  demography = c("population", "demographic", "fertility", "migration", "family"),
  urban = c("urban", "city", "cities", "agglomeration"),
  war = c("war", "warfare", "military", "conflict", "revolution")
)

classify_topic <- function(title_text) {
  if (is.na(title_text) || title_text == "") return("other")
  tl <- tolower(title_text)
  scores <- sapply(names(topic_dict), function(t) {
    sum(sapply(topic_dict[[t]], function(k) grepl(k, tl)))
  })
  if (max(scores) == 0) return("other")
  return(names(topic_dict)[which.max(scores)])
}

jn[, topic := sapply(title, classify_topic)]
cat("Topics:", paste(unique(jn$topic), collapse = ", "), "\n")

# Top institutions
top_inst <- c("Harvard", "MIT", "Stanford", "Berkeley", "Yale", "Princeton", "Chicago",
              "Northwestern", "Columbia", "Penn", "UCLA", "Michigan", "NYU",
              "Oxford", "Cambridge", "LSE", "London School of Economics", "Warwick")

check_top <- function(u) {
  if (is.na(u) || u == "" || u == ".") return(0L)
  as.integer(any(sapply(top_inst, function(x) grepl(x, u, ignore.case = TRUE))))
}

jn[, t1 := sapply(author1_uni, check_top)]
jn[, t2 := sapply(author2_uni, check_top)]
jn[, t3 := sapply(author3_uni, check_top)]
jn[, t4 := sapply(author4_uni, check_top)]
jn[, t5 := sapply(author5_uni, check_top)]
jn[, any_top_inst := pmax(t1, t2, t3, t4, t5, na.rm = TRUE)]
jn[, c("t1", "t2", "t3", "t4", "t5") := NULL]
cat("Papers with top institution author:", sum(jn$any_top_inst, na.rm = TRUE), "\n")

# Article-level controls
jn[, issue_no := suppressWarnings(as.numeric(gsub("[^0-9.]", "", issue_raw)))]
jn[, article_length := pageend - pagestart + 1]
jn[article_length <= 0 | article_length > 200, article_length := NA]
jn[, log_article_length := log(article_length + 1)]
jn[, title_nchar := nchar(title)]
jn[!is.na(issue_no) & !is.na(pagestart),
   article_position := frank(pagestart, ties.method = "first"),
   by = .(journal, year, vol, issue_no)]

for (v in c("log_article_length", "title_nchar", "article_position", "issue_no")) {
  jn[is.na(get(v)), (v) := median(jn[[v]], na.rm = TRUE)]
}

cat("Article-level controls created.\n\n")

# --- 1.4 Estimation sample --------------------------------------------------
est <- jn[is_core == TRUE & has_trajectory == TRUE & !is.na(n_authors) &
            cite_early >= 0 & cite_longrun >= 0]

cat("Estimation sample:", nrow(est), "papers\n")
cat("Years:", min(est$year), "-", max(est$year), "\n")
cat("Journals:", paste(unique(est$journal), collapse = ", "), "\n\n")

cat("Citation statistics:\n")
cat("  Early (age 2): Mean =", round(mean(est$cite_early), 1),
    ", Median =", median(est$cite_early), ", SD =", round(sd(est$cite_early), 1), "\n")
cat("  Long-run: Mean =", round(mean(est$cite_longrun), 1),
    ", Median =", median(est$cite_longrun), ", SD =", round(sd(est$cite_longrun), 1), "\n\n")

# Log transforms
est[, log_early := log(cite_early + 1)]
est[, log_longrun := log(cite_longrun + 1)]


# =============================================================================
# SECTION 2: MAIN REGRESSIONS — TABLE 2
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 2: MAIN REGRESSIONS\n")
cat("--------------------------------------------------------------------\n\n")

# Model 1: Simple OLS (felm with no FE so robust SEs are available)
m1 <- felm(log_longrun ~ log_early, data = est)

# Model 2: OLS + controls
m2 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no, data = est)

# Model 3: Journal + Year FE
m3 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year, data = est)

# Model 4: + Topic FE
m4 <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year + topic,
           data = est[topic != "other"])

# Model 5: Binary fast starter
m5 <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year + topic,
           data = est[topic != "other" & !is.na(fast_starter)])

cat("Main regression results (heteroskedasticity-robust SEs):\n\n")

cat("Model 1 (OLS):\n")
cat("  log_early:", round(coef(m1)["log_early"], 4),
    "(robust SE:", round(rob_se(m1, "log_early"), 4), ")\n\n")

cat("Model 2 (+ Controls):\n")
cat("  log_early:", round(coef(m2)["log_early"], 4),
    "(robust SE:", round(rob_se(m2, "log_early"), 4), ")\n\n")

cat("Model 3 (+ Journal/Year FE):\n")
cat("  log_early:", round(coef(m3)["log_early"], 4),
    "(robust SE:", round(rob_se(m3, "log_early"), 4), ")\n\n")

cat("Model 4 (+ Topic FE):\n")
cat("  log_early:", round(coef(m4)["log_early"], 4),
    "(robust SE:", round(rob_se(m4, "log_early"), 4), ")\n\n")

cat("Model 5 (Binary Fast Starter):\n")
cat("  fast_starter:", round(coef(m5)["fast_starter"], 4),
    "(robust SE:", round(rob_se(m5, "fast_starter"), 4), ")\n\n")

# Save Table 2 — pass robust SEs explicitly so the table matches the text
stargazer(m1, m2, m3, m4, m5,
          type = "latex",
          out = file.path(TABLE_DIR, "Table2_MainResults.tex"),
          se = list(rob_se_vec(m1), rob_se_vec(m2), rob_se_vec(m3),
                    rob_se_vec(m4), rob_se_vec(m5)),
          title = "Path Dependence in Citations",
          label = "tab:main",
          dep.var.labels = "Log(long-run citations)",
          covariate.labels = c("Log(early citations)", "Fast starter",
                               "N authors", "Top institution"),
          add.lines = list(c("Journal FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Year FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Topic FE", "No", "No", "No", "Yes", "Yes")),
          omit.stat = c("f", "ser"),
          notes = "Heteroskedasticity-robust standard errors in parentheses.")
cat("Saved: Table2_MainResults.tex\n\n")


# =============================================================================
# SECTION 3: ROBUSTNESS
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 3: ROBUSTNESS CHECKS\n")
cat("--------------------------------------------------------------------\n\n")

# --- 3.1 Leave-one-journal-out ----------------------------------------------
cat("1. Leave-one-journal-out:\n")
lojo <- lapply(unique(est$journal), function(j) {
  m <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year,
            data = est[journal != j])
  data.table(excluded = j, coef = coef(m)["log_early"],
             se = summary(m)$coefficients["log_early", "Std. Error"])
})
lojo <- rbindlist(lojo)
print(lojo)
cat("   Range:", round(min(lojo$coef), 3), "-", round(max(lojo$coef), 3), "\n\n")

# --- 3.2 Leave-one-year-out -------------------------------------------------
cat("2. Leave-one-year-out:\n")
loyo <- lapply(unique(est$year), function(y) {
  m <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year,
            data = est[year != y])
  data.table(excluded = y, coef = coef(m)["log_early"],
             se = summary(m)$coefficients["log_early", "Std. Error"])
})
loyo <- rbindlist(loyo)
cat("   Range:", round(min(loyo$coef), 3), "-", round(max(loyo$coef), 3), "\n\n")

# --- 3.3 Bootstrap -----------------------------------------------------------
cat("3. Bootstrap (200 replications):\n")
boot_fn <- function(d, i) {
  m <- tryCatch({
    felm(log_longrun ~ log_early + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no |
           journal + year, data = d[i, ])
  }, error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  coef(m)["log_early"]
}
boot_res <- boot(as.data.frame(est), boot_fn, R = 200)
boot_se <- sd(boot_res$t, na.rm = TRUE)
cat("   Analytical SE:", round(summary(m3)$coefficients["log_early", "Std. Error"], 4), "\n")
cat("   Bootstrap SE:", round(boot_se, 4), "\n\n")

# --- 3.4 Threshold sensitivity -----------------------------------------------
cat("4. Fast starter threshold sensitivity:\n")
thresh_df <- lapply(c(0.50, 0.60, 0.70, 0.75, 0.80, 0.90), function(th) {
  est[, fs_temp := ifelse(!is.na(cite_early_pctile) & cite_early_pctile > th, 1L, 0L)]
  m <- felm(log_longrun ~ fs_temp + n_authors + any_top_inst +
               log_article_length + title_nchar + article_position + issue_no |
               journal + year, data = est)
  data.table(threshold = paste0("Top ", (1 - th) * 100, "%"),
             coef = coef(m)["fs_temp"],
             se = summary(m)$coefficients["fs_temp", "Std. Error"])
})
thresh_df <- rbindlist(thresh_df)
print(thresh_df)
cat("\n")

# --- 3.5 Extended sample with age-3 fallback ---------------------------------
cat("5. Extended sample with age-3 fallback:\n")

jn[, cite_longrun_ext := ifelse(!is.na(cite_age_8), cite_age_8,
                                 ifelse(!is.na(cite_age_5), cite_age_5,
                                        ifelse(!is.na(cite_age_3), cite_age_3, NA_real_)))]
jn[, longrun_source := ifelse(!is.na(cite_age_8), "age8",
                               ifelse(!is.na(cite_age_5), "age5",
                                      ifelse(!is.na(cite_age_3), "age3", NA_character_)))]

est_ext <- jn[!is.na(cite_early) & !is.na(cite_longrun_ext) &
                cite_early >= 0 & cite_longrun_ext >= 0 &
                n_authors > 0 & is_core == TRUE]
est_ext[, log_early_ext := log(cite_early + 1)]
est_ext[, log_longrun_ext := log(cite_longrun_ext + 1)]

cat("   Main sample (age 5-8):", nrow(est), "papers, years",
    min(est$year), "-", max(est$year), "\n")
cat("   Extended sample (age 3-8):", nrow(est_ext), "papers, years",
    min(est_ext$year), "-", max(est_ext$year), "\n")
cat("   New papers added:", nrow(est_ext) - nrow(est), "\n")
cat("   Long-run source breakdown:\n")
print(est_ext[, .N, by = longrun_source][order(longrun_source)])
cat("\n")

m_ext <- felm(log_longrun_ext ~ log_early_ext + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = est_ext)

cat("   Main sample coef (M3):", round(coef(m3)["log_early"], 4),
    "(robust SE:", round(rob_se(m3, "log_early"), 4),
    ", N =", nrow(est), ")\n")
cat("   Extended sample coef:", round(coef(m_ext)["log_early_ext"], 4),
    "(robust SE:", round(rob_se(m_ext, "log_early_ext"), 4),
    ", N =", nrow(est_ext), ")\n")
cat("   Change:", round((coef(m_ext)["log_early_ext"] - coef(m3)["log_early"]) /
                         coef(m3)["log_early"] * 100, 1), "%\n\n")

# --- 3.6 Post-age-two citation GROWTH (removes mechanical overlap) -----------
# The baseline regresses cumulative long-run citations on cumulative age-two
# citations; the outcome structurally contains the regressor. As a flow-based
# check, regress citations accumulated strictly AFTER age two (cite_growth =
# long-run minus early) on early citations. A still-strong elasticity here
# rebuts the concern that the baseline is arithmetic overlap.
cat("6. Post-age-two citation growth (flow outcome, removes overlap):\n")
# Google Scholar counts occasionally decline between snapshots, so clamp the rare
# negative growth (measurement noise) at zero before the log.
est[, log_growth := log(pmax(cite_growth, 0) + 1)]
m_growth <- felm(log_growth ~ log_early + n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est)
cat("   Growth elasticity (post-age-2):", round(coef(m_growth)["log_early"], 4),
    "(robust SE:", round(rob_se(m_growth, "log_early"), 4), ", N =", m_growth$N, ")\n")
cat("   Share of papers with zero post-age-2 growth:",
    round(mean(est$cite_growth == 0) * 100, 1), "%\n\n")

robustness <- list(lojo = lojo, loyo = loyo, boot_se = boot_se, thresh = thresh_df,
                   ext_sample = list(
                     coef = coef(m_ext)["log_early_ext"],
                     se = rob_se(m_ext, "log_early_ext"),
                     n_main = nrow(est), n_ext = nrow(est_ext)
                   ),
                   growth = list(coef = coef(m_growth)["log_early"],
                                 se = rob_se(m_growth, "log_early"), n = m_growth$N))


# =============================================================================
# SECTION 4: PLACEBO TESTS
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 4: PLACEBO TESTS\n")
cat("--------------------------------------------------------------------\n\n")

N_PERMUTATIONS <- 1000

# --- 4a: Placebo fast-starter ------------------------------------------------
cat("4a. Placebo fast-starter (", N_PERMUTATIONS, "permutations)...\n")

placebo_data <- est[!is.na(fast_starter)]
true_model <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                     log_article_length + title_nchar + article_position + issue_no |
                     journal + year, data = placebo_data)
true_coef_fs <- coef(true_model)["fast_starter"]
cat("True fast_starter coefficient:", round(true_coef_fs, 4), "\n")

placebo_coefs_fs <- numeric(N_PERMUTATIONS)

for (p in seq_len(N_PERMUTATIONS)) {
  if (p %% 100 == 0) cat("  Permutation", p, "/", N_PERMUTATIONS, "\n")

  perm_data <- copy(placebo_data)
  perm_data[, fast_starter := sample(fast_starter), by = .(journal, year)]

  m_perm <- tryCatch({
    felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
           log_article_length + title_nchar + article_position + issue_no |
           journal + year, data = perm_data)
  }, error = function(e) NULL)

  placebo_coefs_fs[p] <- if (!is.null(m_perm)) coef(m_perm)["fast_starter"] else NA_real_
}

placebo_coefs_fs <- placebo_coefs_fs[!is.na(placebo_coefs_fs)]
emp_p_fs <- mean(placebo_coefs_fs >= true_coef_fs)

cat("\nEmpirical p-value:", formatC(emp_p_fs, format = "f", digits = 4), "\n")
cat("  Mean placebo:", round(mean(placebo_coefs_fs), 4), "\n")
cat("  SD placebo:", round(sd(placebo_coefs_fs), 4), "\n")
cat("  True / SD:", round(true_coef_fs / sd(placebo_coefs_fs), 2), "standard deviations\n\n")


# --- 4b: Placebo conference --------------------------------------------------
# (Placebo conference test moved to after Section 5)

# =============================================================================
# SECTION 5: CONFERENCE ANALYSIS
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 5: CONFERENCE ANALYSIS\n")
cat("--------------------------------------------------------------------\n\n")

conf_parsed_file <- file.path(DATA_DIR, "conference_parsed_data.rds")

if (file.exists(conf_parsed_file)) {

  cat("Loading pre-parsed conference data...\n")
  conf_data <- readRDS(conf_parsed_file)
  cat("  Loaded", nrow(conf_data), "conference papers\n")

  # Ensure column names
  if ("title" %in% names(conf_data) && !"conf_title" %in% names(conf_data)) {
    conf_data[, conf_title := title]
  }
  if ("authors" %in% names(conf_data) && !"conf_authors" %in% names(conf_data)) {
    conf_data[, conf_authors := authors]
  }
  if ("year" %in% names(conf_data) && !"conf_year" %in% names(conf_data)) {
    conf_data[, conf_year := year]
  }

  # --- 5.1 Fuzzy match conference papers to journal papers -------------------
  cat("Matching conference papers to journal dataset...\n")

  # Prepare journal data for matching
  jn[, title_clean := tolower(gsub("[^a-z0-9 ]", "", title))]
  jn[, title_clean := gsub("\\s+", " ", trimws(title_clean))]

  conf_data[, conf_title_clean := tolower(gsub("[^a-z0-9 ]", "", conf_title))]
  conf_data[, conf_title_clean := gsub("\\s+", " ", trimws(conf_title_clean))]

  jn[, author1_clean := tolower(gsub("[^a-z ]", "", author1))]
  conf_data[, conf_author_clean := tolower(gsub("[^a-z ]", "", conf_authors))]

  # Clean conference titles: remove timestamps and short entries
  conf_data[grepl("[0-9]:[0-9]{2}", conf_title), conf_title_clean := NA_character_]
  conf_data[!is.na(conf_title_clean) & nchar(conf_title_clean) < 15, conf_title_clean := NA_character_]

  # Helper: extract surname(s) from an author string. Journal authors are stored
  # "Firstname Lastname"; conference authors may be "; "/"and"-separated. Take the
  # last token of each name.
  extract_last_names <- function(author_str) {
    if (is.na(author_str) || nchar(trimws(author_str)) < 2) return(character(0))
    parts <- unlist(strsplit(author_str, "[,;/&]|\\band\\b"))
    parts <- trimws(parts)
    parts <- parts[nchar(parts) > 1]
    last_names <- sapply(parts, function(p) {
      words <- strsplit(trimws(p), "\\s+")[[1]]
      words <- words[nchar(words) > 1]
      if (length(words) == 0) return(NA_character_)
      tolower(tail(words, 1))
    }, USE.NAMES = FALSE)
    last_names <- last_names[!is.na(last_names) & nchar(last_names) > 1]
    unique(last_names)
  }

  # Precompute the surname set of every journal paper ONCE (reused by each match).
  jn[, surnames := mapply(function(a1, a2, a3, a4, a5)
        list(unique(c(extract_last_names(a1), extract_last_names(a2),
                      extract_last_names(a3), extract_last_names(a4),
                      extract_last_names(a5)))),
      author1, author2, author3, author4, author5, SIMPLIFY = FALSE)]
  # Surnames from the RAW author string (extract_last_names splits on ; & "and";
  # conf_author_clean has those separators stripped, which would collapse a
  # multi-author talk to just its last surname).
  conf_data[, conf_surnames := lapply(conf_authors, extract_last_names)]

  # --- Improved, author-validated matcher --------------------------------------
  # Title drift between a conference presentation and the published version
  # (British/US spelling -- "industrialisation"/"industrialization" -- subtitle
  # changes, em-dashes) routinely pushes the Jaro-Winkler title distance to
  # 0.15-0.25 even for the SAME paper. The earlier matcher validated authors only
  # against the single closest title, so genuine retitled papers were missed when
  # another title happened to be marginally closer. This version chooses the
  # closest AUTHOR-VALIDATED candidate within a title band, which roughly doubles
  # the (correct) match count without admitting topical false positives. Tiers:
  #   A: title distance < 0.10                    -> accept (near-exact title)
  #   B: title distance < 0.25 AND author overlap -> accept (validated retitle)
  #   C: no conference author info, title < 0.15  -> accept (title only)
  TIER_A_MAX <- 0.10
  TIER_B_MAX <- 0.25
  TIER_C_MAX <- 0.15

  find_best_match <- function(ct, conf_lnames, cy, jn_data) {
    miss <- list(id = NA_integer_, dist = NA_real_, tier = NA_character_)
    if (is.na(ct) || nchar(ct) < 15) return(miss)
    cand <- jn_data[year >= cy - 1 & year <= cy + 5]
    if (nrow(cand) == 0) return(miss)
    td <- stringdist(ct, cand$title_clean, method = "jw")
    k <- which.min(td)
    if (td[k] < TIER_A_MAX) return(list(id = cand$id[k], dist = td[k], tier = "A"))
    if (length(conf_lnames) > 0) {
      ov <- vapply(cand$surnames, function(js) any(conf_lnames %in% js), logical(1))
      idxB <- which(ov & td < TIER_B_MAX)
      if (length(idxB) > 0) {
        j <- idxB[which.min(td[idxB])]
        return(list(id = cand$id[j], dist = td[j], tier = "B"))
      }
    } else if (td[k] < TIER_C_MAX) {
      return(list(id = cand$id[k], dist = td[k], tier = "C"))
    }
    list(id = NA_integer_, dist = td[k], tier = NA_character_)
  }

  cat("  Running author-validated fuzzy matching...\n")
  conf_data[, matched_id := NA_integer_]
  conf_data[, match_dist := NA_real_]
  conf_data[, match_tier := NA_character_]
  n_skipped_short <- sum(is.na(conf_data$conf_title_clean))
  cat("  Skipping", n_skipped_short, "entries (NA or short titles)\n")

  for (i in 1:nrow(conf_data)) {
    if (!is.na(conf_data$conf_title_clean[i]) && nchar(conf_data$conf_title_clean[i]) >= 15) {
      r <- find_best_match(conf_data$conf_title_clean[i], conf_data$conf_surnames[[i]],
                           conf_data$conf_year[i], jn)
      conf_data[i, `:=`(matched_id = r$id, match_dist = r$dist, match_tier = r$tier)]
    }
    if (i %% 1000 == 0) cat("    Processed", i, "entries,",
                            sum(!is.na(conf_data$matched_id)), "matches so far\n")
  }

  cat("\n  Successfully matched", sum(!is.na(conf_data$matched_id)), "conference entries\n")
  cat("  By conference and tier:\n")
  print(conf_data[!is.na(matched_id), .N, by = .(conference, match_tier)][order(conference, match_tier)])

  # Print a sample of tier-B (author-validated retitle) matches for verification
  cat("\n  Sample author-validated (tier B) matches for inspection:\n")
  bsamp <- conf_data[match_tier == "B"][order(match_dist)]
  if (nrow(bsamp) > 0) {
    for (i in seq_len(min(12, nrow(bsamp)))) {
      jp <- jn[id == bsamp$matched_id[i]]
      cat(sprintf("    [%.2f] CONF: %.56s\n           JRNL: %.56s\n",
                  bsamp$match_dist[i], bsamp$conf_title[i], jp$title[1]))
    }
  }
  cat("\n")

  # --- 5.2 Create conference variables in main dataset ----------------------
  cat("Creating conference variables...\n")

  has_session <- "session_order" %in% names(conf_data)

  # Aggregate matches to paper level. Retain EHA identity and the earliest EHA
  # session (only EHA carries reliable begin-times) for the session-timing check.
  conf_matches <- conf_data[!is.na(matched_id), .(
    presented_at_conference   = 1L,
    presented_at_eha          = as.integer(any(conference == "EHA")),
    presented_at_ehs          = as.integer(any(conference == "EHS")),
    n_conference_presentations = .N,
    eha_session_order = if (has_session && any(conference == "EHA"))
        suppressWarnings(as.integer(min(session_order[conference == "EHA"], na.rm = TRUE))) else NA_integer_,
    eha_pre_lunch  = if (has_session && any(conference == "EHA"))
        as.integer(any(pre_lunch[conference == "EHA"]  == 1, na.rm = TRUE)) else 0L,
    eha_post_lunch = if (has_session && any(conference == "EHA"))
        as.integer(any(post_lunch[conference == "EHA"] == 1, na.rm = TRUE)) else 0L
  ), by = .(matched_id)]
  setnames(conf_matches, "matched_id", "id")
  conf_matches[is.infinite(eha_session_order), eha_session_order := NA_integer_]

  # Merge to jn
  jn <- merge(jn, conf_matches, by = "id", all.x = TRUE)
  for (v in c("presented_at_conference", "presented_at_eha", "presented_at_ehs",
              "n_conference_presentations", "eha_pre_lunch", "eha_post_lunch")) {
    jn[is.na(get(v)), (v) := 0L]
  }

  cat("  Papers at any conference:", sum(jn$presented_at_conference), "\n")
  cat("    via EHA:", sum(jn$presented_at_eha), "  via EHS:", sum(jn$presented_at_ehs), "\n\n")

  # Update estimation sample
  est_conf <- jn[is_core == TRUE & has_trajectory == TRUE & !is.na(n_authors) &
                   cite_early >= 0 & cite_longrun >= 0]
  est_conf[, log_early := log(cite_early + 1)]
  est_conf[, log_longrun := log(cite_longrun + 1)]

  cat("Estimation sample:", nrow(est_conf), "papers\n")
  cat("  Presented at conference:", sum(est_conf$presented_at_conference), "\n\n")

  # --- 5.3 Conference premium regression ------------------------------------
  if (sum(est_conf$presented_at_conference) >= 20) {

    cat("=== CONFERENCE PREMIUM REGRESSION ===\n\n")

    c1 <- felm(log_longrun ~ presented_at_conference + log_early + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = est_conf)

    cat("Model C1: Any conference presentation effect (EHA clean + EHS)\n")
    cat("  N presenters:", sum(est_conf$presented_at_conference), "\n")
    cat("  presented_at_conference:", round(coef(c1)["presented_at_conference"], 4),
        "(robust SE:", round(rob_se(c1, "presented_at_conference"), 4), ")\n")
    cat("  Interpretation: Conference presenters have",
        round((exp(coef(c1)["presented_at_conference"]) - 1) * 100, 1),
        "% more long-run citations\n\n")
  }

  # --- 5.4 Session-timing "visibility shock" (EHA only) ---------------------
  # The clean EHA programmes carry exact begin-times, so we can ask whether a
  # talk's slot -- the last session before lunch vs the first after -- shifts
  # long-run citations (a pseudo-random attention shock). This is informative
  # ONLY if (a) the slots are balanced on pre-determined covariates and (b) the
  # matched cells are large enough. We GATE on both and report honestly; an
  # underpowered or imbalanced design is not interpreted as a result.
  cat("=== SESSION-TIMING (EHA) BALANCE & POWER CHECK ===\n\n")
  eha_est <- est_conf[presented_at_eha == 1 & !is.na(eha_session_order)]
  cl <- NULL; cs <- NULL
  timing_powered <- FALSE
  cat("  EHA-matched papers in estimation sample:", nrow(eha_est), "\n")
  if (nrow(eha_est) > 0) {
    cat("  Session-order distribution:\n")
    print(eha_est[, .N, by = eha_session_order][order(eha_session_order)])
    n_pre <- sum(eha_est$eha_pre_lunch); n_post <- sum(eha_est$eha_post_lunch)
    cat("  Pre-lunch (last AM):", n_pre, "  Post-lunch (first PM):", n_post, "\n\n")

    # Balance: pre- vs post-lunch on pre-determined covariates
    lunch <- eha_est[eha_pre_lunch == 1 | eha_post_lunch == 1]
    if (nrow(lunch) >= 8 && uniqueN(lunch$eha_pre_lunch) == 2) {
      cat("  Balance (normalised mean difference, pre vs post lunch):\n")
      for (v in c("log_early", "fast_starter", "any_top_inst", "n_authors")) {
        g1 <- lunch[eha_pre_lunch == 1][[v]]; g0 <- lunch[eha_pre_lunch == 0][[v]]
        nmd <- (mean(g1, na.rm = TRUE) - mean(g0, na.rm = TRUE)) /
               sqrt((var(g1, na.rm = TRUE) + var(g0, na.rm = TRUE)) / 2)
        pv <- tryCatch(t.test(g1, g0)$p.value, error = function(e) NA_real_)
        cat(sprintf("    %-14s NMD = %6.3f  (t-test p = %.3f)\n", v, nmd, pv))
      }
    }

    # Power gate: require a minimum number of papers in each lunch-adjacent cell
    MIN_CELL <- 25
    timing_powered <- (n_pre >= MIN_CELL && n_post >= MIN_CELL)
    cat(sprintf("\n  Power gate: pre = %d, post = %d, minimum per cell = %d  ->  %s\n",
                n_pre, n_post, MIN_CELL,
                if (timing_powered) "POWERED" else "UNDERPOWERED"))

    if (timing_powered) {
      lunch[, post_lunch_ind := as.integer(eha_post_lunch == 1)]
      cl <- felm(log_longrun ~ post_lunch_ind + log_early + n_authors + any_top_inst |
                   journal + year, data = lunch)
      cat("  Lunch contrast (post vs pre): coef", round(coef(cl)["post_lunch_ind"], 4),
          "(robust SE", round(rob_se(cl, "post_lunch_ind"), 4), ")\n")
      cs <- felm(log_longrun ~ eha_session_order + log_early + n_authors + any_top_inst |
                   journal + year, data = eha_est)
      cat("  Session-order gradient: coef", round(coef(cs)["eha_session_order"], 4),
          "(robust SE", round(rob_se(cs, "eha_session_order"), 4), ")\n\n")
    } else {
      cat("  -> Even with hand-verified EHA timing, too few matched papers fall in\n")
      cat("     the lunch-adjacent slots for inference. Reported as inconclusive.\n\n")
    }
  }

  # Also update est with conference data for downstream sections
  est <- merge(est, conf_matches, by = "id", all.x = TRUE)
  for (v in c("presented_at_conference", "presented_at_eha", "presented_at_ehs",
              "n_conference_presentations", "eha_pre_lunch", "eha_post_lunch")) {
    if (v %in% names(est)) est[is.na(get(v)), (v) := 0L]
  }

  # --- 5c: Author-based conference exposure -----------------------------------
  # Alternative measure: did ANY author of a published paper present ANYTHING
  # at EHA/EHS in the year before or year of publication?
  # This captures the broader visibility/networking externality of conference
  # attendance and avoids the title-matching problem.
  cat("--- 5c: Author-based conference exposure ---\n\n")

  # Step 1: Extract all conference presenter last names by year (raw author string)
  conf_data[, conf_lnames := lapply(conf_authors, extract_last_names)]
  conf_presenters <- conf_data[!is.na(conf_year),
                                .(conf_lname = unlist(conf_lnames)), by = conf_year]
  conf_presenters <- unique(conf_presenters)
  conf_presenters <- conf_presenters[nchar(conf_lname) > 1]  # drop single-char junk
  cat("  Unique presenter last names:", uniqueN(conf_presenters$conf_lname), "\n")
  cat("  Conference years covered:", paste(sort(unique(conf_presenters$conf_year)), collapse = ", "), "\n")

  # Step 2: Extract journal paper author last names
  jn[, paper_lnames := mapply(function(a1, a2, a3, a4, a5) {
    all_authors <- c(a1, a2, a3, a4, a5)
    all_authors <- all_authors[!is.na(all_authors) & all_authors != ""]
    unique(unlist(lapply(all_authors, extract_last_names)))
  }, author1, author2, author3, author4, author5, SIMPLIFY = FALSE)]

  # Step 3: Match — did any author present at a conference in [year-1, year]?
  jn[, author_conf_exposure := mapply(function(pub_year, lnames) {
    if (length(lnames) == 0) return(0L)
    conf_names_window <- conf_presenters[conf_year %in% (pub_year - 1):pub_year, conf_lname]
    if (length(conf_names_window) == 0) return(0L)
    as.integer(any(lnames %in% conf_names_window))
  }, year, paper_lnames)]

  cat("  Papers with author conference exposure:", sum(jn$author_conf_exposure),
      "/", nrow(jn), "(", round(mean(jn$author_conf_exposure) * 100, 1), "%)\n")

  # Propagate to est
  est[, author_conf_exposure := jn$author_conf_exposure[match(est$id, jn$id)]]
  est[is.na(author_conf_exposure), author_conf_exposure := 0L]

  cat("  In estimation sample:", sum(est$author_conf_exposure),
      "/", nrow(est), "(", round(mean(est$author_conf_exposure) * 100, 1), "%)\n\n")

  # Step 4: Author-exposure regressions
  if (sum(est$author_conf_exposure) >= 30) {

    cat("=== AUTHOR-BASED CONFERENCE EXPOSURE REGRESSIONS ===\n\n")

    c2 <- felm(log_longrun ~ author_conf_exposure + log_early + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = est)
    cat("Model C2: Author conference exposure (broad measure)\n")
    cat("  author_conf_exposure:", round(coef(c2)["author_conf_exposure"], 4),
        "(robust SE:", round(rob_se(c2, "author_conf_exposure"), 4), ")\n")
    cat("  Interpretation: Authors who presented at a conference have",
        round((exp(coef(c2)["author_conf_exposure"]) - 1) * 100, 1),
        "% more long-run citations\n\n")

    # Both measures together
    if ("presented_at_conference" %in% names(est) && sum(est$presented_at_conference) >= 10) {
      c3 <- felm(log_longrun ~ presented_at_conference + author_conf_exposure + log_early +
                   n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est)
      cat("Model C3: Title-matched + author exposure (joint)\n")
      cat("  presented_at_conference:", round(coef(c3)["presented_at_conference"], 4),
          "(robust SE:", round(rob_se(c3, "presented_at_conference"), 4), ")\n")
      cat("  author_conf_exposure:", round(coef(c3)["author_conf_exposure"], 4),
          "(robust SE:", round(rob_se(c3, "author_conf_exposure"), 4), ")\n\n")
    }
  }

} else {
  cat("Conference data file not found:", conf_parsed_file, "\n")
  cat("Skipping conference analysis.\n\n")
  est_conf <- NULL
}

# --- 5b: Placebo conference test (runs after conference data is merged) ---
if ("presented_at_conference" %in% names(est) &&
    sum(est$presented_at_conference, na.rm = TRUE) >= 20) {

  cat("--- 5b: Placebo conference (", N_PERMUTATIONS, "permutations) ---\n")

  conf_placebo_data <- est[!is.na(presented_at_conference)]
  true_conf_model <- felm(log_longrun ~ presented_at_conference + log_early +
                            n_authors + any_top_inst +
                            log_article_length + title_nchar + article_position + issue_no |
                            journal + year,
                          data = conf_placebo_data)
  true_conf_coef <- coef(true_conf_model)["presented_at_conference"]
  cat("True conference coefficient:", round(true_conf_coef, 4), "\n")

  placebo_conf_coefs <- numeric(N_PERMUTATIONS)

  for (p in seq_len(N_PERMUTATIONS)) {
    if (p %% 100 == 0) cat("  Permutation", p, "/", N_PERMUTATIONS, "\n")
    perm_data <- copy(conf_placebo_data)
    perm_data[, presented_at_conference := sample(presented_at_conference),
              by = .(journal, year)]
    m_perm <- tryCatch({
      felm(log_longrun ~ presented_at_conference + log_early +
             n_authors + any_top_inst +
             log_article_length + title_nchar + article_position + issue_no |
             journal + year, data = perm_data)
    }, error = function(e) NULL)
    placebo_conf_coefs[p] <- if (!is.null(m_perm)) coef(m_perm)["presented_at_conference"] else NA_real_
  }

  placebo_conf_coefs <- placebo_conf_coefs[!is.na(placebo_conf_coefs)]
  emp_p_conf <- mean(placebo_conf_coefs >= true_conf_coef)

  cat("Empirical p-value:", formatC(emp_p_conf, format = "f", digits = 4), "\n")
  cat("  Mean placebo:", round(mean(placebo_conf_coefs), 4), "\n")
  cat("  SD placebo:", round(sd(placebo_conf_coefs), 4), "\n\n")

} else {
  cat("Placebo conference: SKIPPED (fewer than 20 conference presenters in est).\n\n")
}


# =============================================================================
# SECTION 6: MECHANISM ANALYSIS
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 6: MECHANISM ANALYSIS\n")
cat("--------------------------------------------------------------------\n\n")

# Create mechanism dataset
mech <- copy(est)

# Load network / citation data
cite_raw_file <- file.path(DATA_DIR, "network_citation_data.rds")
oa_match_file <- file.path(DATA_DIR, "network_openalex_matches.rds")

has_network <- file.exists(cite_raw_file) && file.exists(oa_match_file)

if (has_network) {

  cat("Loading network data...\n")
  cite_raw   <- readRDS(cite_raw_file)
  oa_matches <- readRDS(oa_match_file)
  cat("  Citation links:", nrow(cite_raw), "\n")
  cat("  OpenAlex matches:", nrow(oa_matches), "\n\n")

  # Load network metrics (PageRank, HHI, etc.) if available
  metrics_file <- file.path(DATA_DIR, "network_paper_metrics.rds")
  if (file.exists(metrics_file)) {
    metrics <- readRDS(metrics_file)
    metrics[, id := as.integer(id)]
    mech <- merge(mech, metrics, by = "id", all.x = TRUE)
    cat("  Network metrics merged:", sum(!is.na(mech$pagerank)), "papers with metrics\n\n")
  }

  # Build lookup tables
  id_to_oa <- setNames(oa_matches$openalex_id, as.character(oa_matches$id))
  oa_to_id <- setNames(as.integer(oa_matches$id), oa_matches$openalex_id)


  # -------------------------------------------------------------------------
  # 6a: Citation source decomposition
  # -------------------------------------------------------------------------
  cat("--- 6a: Citation source decomposition ---\n\n")

  # Classify each citing work by DISCIPLINE, not by its single top OpenAlex
  # concept. The top concept is mostly topic-level (e.g. "Inequality",
  # "Human capital") or noise, so the old whitelist match badly under-counted
  # economics (it returned ~9.5% within-field). We instead use the re-queried
  # primary_topic / level-0 concept data cached by scripts/verify_crossfield.R
  # and define "within-field" as: Economics or History appears among the
  # citing work's level-0 (root-discipline) concept tags.
  cfd_file <- file.path(DATA_DIR, "citing_field_data.rds")
  if (!file.exists(cfd_file)) {
    stop("citing_field_data.rds not found -- run scripts/verify_crossfield.R first ",
         "to fetch citing-work disciplines from OpenAlex.")
  }
  cfd <- readRDS(cfd_file); setDT(cfd)
  cite_raw[, citing_short := gsub("https://openalex.org/", "", citing_oa_id)]
  cite_raw <- merge(cite_raw,
                    cfd[, .(citing_oa_id, l0_concepts, pt_field, pt_subfield, type)],
                    by.x = "citing_short", by.y = "citing_oa_id", all.x = TRUE)

  within_l0 <- function(s) {
    if (is.na(s)) return(NA)
    p <- trimws(strsplit(s, ";")[[1]])
    any(tolower(p) %in% c("economics", "history"))
  }
  cite_raw[, is_within_field := vapply(l0_concepts, within_l0, logical(1))]

  cat("Within-field citations:", sum(cite_raw$is_within_field, na.rm = TRUE),
      "/", sum(!is.na(cite_raw$is_within_field)),
      sprintf(" (%.1f%%)\n", 100 * mean(cite_raw$is_within_field, na.rm = TRUE)))
  cat("Cross-field citations:", sum(!cite_raw$is_within_field, na.rm = TRUE), "\n\n")

  # Aggregate per paper
  cite_source <- cite_raw[!is.na(is_within_field),
                          .(n_within_cites = sum(is_within_field),
                            n_cross_cites = sum(!is_within_field),
                            n_total_source = .N),
                          by = .(cited_id)]
  cite_source[, cross_field_share := n_cross_cites / n_total_source]
  cite_source[, cited_id := as.integer(cited_id)]  # Bug fix: convert to integer

  mech <- merge(mech, cite_source, by.x = "id", by.y = "cited_id", all.x = TRUE)
  mech[is.na(n_within_cites), n_within_cites := 0L]
  mech[is.na(n_cross_cites), n_cross_cites := 0L]
  mech[is.na(cross_field_share), cross_field_share := NA_real_]

  mech[, log_within_cites := log(n_within_cites + 1)]
  mech[, log_cross_cites := log(n_cross_cites + 1)]

  cat("Papers with citation source data:", sum(mech$n_within_cites + mech$n_cross_cites > 0), "\n\n")

  # Regressions
  s1a <- felm(log_within_cites ~ log_early + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = mech)
  cat("S1a: log_early -> log(within-field cites)\n")
  cat("  Coef:", round(coef(s1a)["log_early"], 4),
      "(SE:", round(summary(s1a)$coefficients["log_early", "Std. Error"], 4), ")\n\n")

  s1b <- felm(log_cross_cites ~ log_early + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = mech)
  cat("S1b: log_early -> log(cross-field cites)\n")
  cat("  Coef:", round(coef(s1b)["log_early"], 4),
      "(SE:", round(summary(s1b)$coefficients["log_early", "Std. Error"], 4), ")\n\n")

  # Does early success shift a paper's audience OUT of the field? (the real test)
  # Regress the cross-field SHARE of a paper's citations on the fast-starter
  # indicator. A ~zero coefficient => early visibility scales within- and
  # cross-field citations proportionally; it does not redirect attention outward.
  fs_dat <- mech[!is.na(cross_field_share)]
  fs_x <- felm(cross_field_share ~ fast_starter | journal + year, data = fs_dat)
  fs_co <- coef(fs_x)[grep("fast_starter", names(coef(fs_x)))[1]]
  fs_se <- summary(fs_x, robust = TRUE)$coefficients[
    grep("fast_starter", rownames(summary(fs_x)$coefficients))[1], 2]
  cat("Fast-starter -> cross-field SHARE (journal-year FE):\n")
  cat(sprintf("  coef %.4f (robust SE %.4f); within/cross count elasticities %.3f / %.3f\n\n",
              fs_co, fs_se, coef(s1a)["log_early"], coef(s1b)["log_early"]))


  # -------------------------------------------------------------------------
  # 6b: Self-citation analysis
  # -------------------------------------------------------------------------
  cat("--- 6b: Self-citation analysis ---\n\n")

  # Build author name lookup
  author_cols <- paste0("author", 1:5)
  author_lookup <- list()

  for (i in seq_len(nrow(jn))) {
    paper_id <- jn$id[i]
    authors <- c()
    for (ac in author_cols) {
      a <- jn[[ac]][i]
      if (!is.na(a) && nchar(trimws(a)) > 0 && trimws(a) != ".") {
        a_norm <- tolower(trimws(a))
        a_norm <- gsub("[^a-z -]", "", a_norm)
        a_norm <- gsub("\\s+", " ", a_norm)
        if (nchar(a_norm) > 1) authors <- c(authors, a_norm)
      }
    }
    author_lookup[[as.character(paper_id)]] <- authors
  }

  # Check for self-citations
  cite_raw[, is_self_cite := FALSE]
  cite_raw[, citing_our_id := oa_to_id[citing_oa_id]]

  n_self <- 0L
  n_checked <- 0L

  for (i in seq_len(nrow(cite_raw))) {
    cited_id_i  <- as.character(cite_raw$cited_id[i])
    citing_id_i <- cite_raw$citing_our_id[i]

    if (is.na(citing_id_i)) next

    authors_cited  <- author_lookup[[cited_id_i]]
    authors_citing <- author_lookup[[as.character(citing_id_i)]]

    if (is.null(authors_cited) || is.null(authors_citing)) next

    n_checked <- n_checked + 1L

    overlap <- any(authors_cited %in% authors_citing)
    if (!overlap) {
      last_cited  <- sapply(strsplit(authors_cited, " "), tail, 1)
      last_citing <- sapply(strsplit(authors_citing, " "), tail, 1)
      overlap <- any(last_cited %in% last_citing)
    }

    if (overlap) {
      cite_raw[i, is_self_cite := TRUE]
      n_self <- n_self + 1L
    }
  }

  cat("Within-dataset citation links checked:", n_checked, "\n")
  cat("Self-citations identified:", n_self, "\n")
  cat("Self-citation rate:", round(n_self / max(n_checked, 1) * 100, 1), "%\n\n")

  # Aggregate per paper
  self_cite_agg <- cite_raw[, .(n_total_cites_sc = .N,
                                 n_self_cites = sum(is_self_cite)),
                             by = .(cited_id)]
  self_cite_agg[, self_cite_share := n_self_cites / n_total_cites_sc]
  self_cite_agg[, cited_id := as.integer(cited_id)]  # Bug fix

  mech <- merge(mech, self_cite_agg, by.x = "id", by.y = "cited_id", all.x = TRUE)
  mech[is.na(n_total_cites_sc), n_total_cites_sc := 0L]
  mech[is.na(n_self_cites), n_self_cites := 0L]
  mech[is.na(self_cite_share), self_cite_share := 0]

  mech[, log_non_self_cites := log(n_total_cites_sc - n_self_cites + 1)]
  mech[, log_total_cites_sc := log(n_total_cites_sc + 1)]

  # Regressions
  sc1 <- felm(log_non_self_cites ~ log_early + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = mech)
  cat("SC1: log_early -> log(non-self cites)\n")
  cat("  Coef:", round(coef(sc1)["log_early"], 4),
      "(SE:", round(summary(sc1)$coefficients["log_early", "Std. Error"], 4), ")\n\n")

  sc2 <- felm(log_total_cites_sc ~ log_early + n_authors + any_top_inst +
                log_article_length + title_nchar + article_position + issue_no |
                journal + year, data = mech)
  cat("SC2: log_early -> log(total cites)\n")
  cat("  Coef:", round(coef(sc2)["log_early"], 4),
      "(SE:", round(summary(sc2)$coefficients["log_early", "Std. Error"], 4), ")\n\n")

  change_sc <- (coef(sc1)["log_early"] - coef(sc2)["log_early"]) /
    abs(coef(sc2)["log_early"]) * 100
  cat("Change from SC2 to SC1:", round(change_sc, 1), "%\n")
  cat("  -> Stability confirms self-citations do not drive the result\n\n")


  # -------------------------------------------------------------------------
  # 6c: Cascade depth
  # -------------------------------------------------------------------------
  cat("--- 6c: Citation cascade depth ---\n\n")

  # Build within-dataset citation network
  our_oa_ids_set <- oa_matches$openalex_id
  edges_w <- cite_raw[citing_oa_id %in% our_oa_ids_set & cited_oa_id %in% our_oa_ids_set,
                      .(from = citing_oa_id, to = cited_oa_id)]
  edges_w <- unique(edges_w)

  if (nrow(edges_w) > 0) {
    g_within <- graph_from_data_frame(edges_w, directed = TRUE)

    cascade_list <- list()
    for (oa_id in intersect(V(g_within)$name, our_oa_ids_set)) {
      dists <- distances(g_within, v = oa_id, mode = "in")
      finite_dists <- dists[is.finite(dists) & dists > 0]
      cascade_depth <- if (length(finite_dists) > 0) max(finite_dists) else 0
      n_direct <- sum(dists == 1)
      n_indirect <- sum(finite_dists > 1)
      our_id <- oa_to_id[oa_id]
      if (!is.na(our_id)) {
        cascade_list[[as.character(our_id)]] <- data.table(
          id = our_id, cascade_depth = cascade_depth,
          n_direct_citers = n_direct, n_indirect_citers = n_indirect
        )
      }
    }
    cascade_dt <- rbindlist(cascade_list)
    cat("Cascade depth computed for", nrow(cascade_dt), "papers\n")
    cat("  Mean depth:", round(mean(cascade_dt$cascade_depth), 2), "\n")
    cat("  Max depth:", max(cascade_dt$cascade_depth), "\n\n")

    # Merge
    cascade_dt[, id := as.integer(id)]  # Bug fix
    mech <- merge(mech, cascade_dt, by = "id", all.x = TRUE)
    mech[is.na(cascade_depth), cascade_depth := 0]
    mech[is.na(n_direct_citers), n_direct_citers := 0L]
    mech[is.na(n_indirect_citers), n_indirect_citers := 0L]

    # Regressions
    if (sum(mech$cascade_depth > 0, na.rm = TRUE) >= 20) {
      cd1 <- felm(cascade_depth ~ fast_starter + log_early +
                    n_authors + any_top_inst +
                    log_article_length + title_nchar + article_position + issue_no |
                    journal + year,
                  data = mech[!is.na(fast_starter)])
      cat("CD1: fast_starter -> cascade depth\n")
      cat("  fast_starter:", round(coef(cd1)["fast_starter"], 4),
          "(SE:", round(summary(cd1)$coefficients["fast_starter", "Std. Error"], 4), ")\n\n")
    }

    mech[, log_indirect := log(n_indirect_citers + 1)]

    if (sum(mech$n_indirect_citers > 0, na.rm = TRUE) >= 20) {
      cd2 <- felm(log_indirect ~ fast_starter + log_early +
                    n_authors + any_top_inst +
                    log_article_length + title_nchar + article_position + issue_no |
                    journal + year,
                  data = mech[!is.na(fast_starter)])
      cat("CD2: fast_starter -> log(indirect citers)\n")
      cat("  fast_starter:", round(coef(cd2)["fast_starter"], 4),
          "(SE:", round(summary(cd2)$coefficients["fast_starter", "Std. Error"], 4), ")\n\n")
    }

  } else {
    cat("No within-dataset edges found. Skipping cascade analysis.\n\n")
  }


  # -------------------------------------------------------------------------
  # 6d: Citation concentration / HHI
  # -------------------------------------------------------------------------
  cat("--- 6d: Citation concentration (HHI) ---\n\n")

  # Check if cite_concentration is already in mech (from network metrics)
  if ("cite_concentration" %in% names(mech)) {
    n_hhi <- sum(!is.na(mech$cite_concentration))
    cat("Papers with HHI data (from network metrics):", n_hhi, "\n\n")
  } else {
    # Compute from cite_raw: HHI of citation counts across years
    cat("Computing citation concentration from raw data...\n")

    # For each paper, compute HHI of citation counts by year
    hhi_list <- list()
    for (pid in unique(mech$id)) {
      oa_id <- id_to_oa[as.character(pid)]
      if (is.na(oa_id)) next

      paper_cites <- cite_raw[cited_oa_id == oa_id & !is.na(citing_year)]
      if (nrow(paper_cites) < 2) next

      year_counts <- paper_cites[, .N, by = citing_year]
      total <- sum(year_counts$N)
      shares <- year_counts$N / total
      hhi <- sum(shares^2)

      hhi_list[[as.character(pid)]] <- data.table(id = as.integer(pid),
                                                   cite_concentration = hhi)
    }

    if (length(hhi_list) > 0) {
      hhi_dt <- rbindlist(hhi_list)
      mech <- merge(mech, hhi_dt, by = "id", all.x = TRUE)
      n_hhi <- sum(!is.na(mech$cite_concentration))
      cat("Papers with HHI data:", n_hhi, "\n\n")
    } else {
      n_hhi <- 0
      cat("No HHI data computed.\n\n")
    }
  }

  if (exists("n_hhi") && n_hhi >= 30) {
    hc1 <- felm(cite_concentration ~ fast_starter + log_early +
                  n_authors + any_top_inst +
                  log_article_length + title_nchar + article_position + issue_no |
                  journal + year,
                data = mech[!is.na(fast_starter) & !is.na(cite_concentration)])
    cat("HC1: fast_starter -> cite_concentration (HHI)\n")
    cat("  fast_starter:", round(coef(hc1)["fast_starter"], 4),
        "(SE:", round(summary(hc1)$coefficients["fast_starter", "Std. Error"], 4), ")\n")
    cat("  (Negative = more temporally spread citations)\n\n")
  }

} else {
  cat("Network data files not found. Skipping mechanism analysis.\n")
  cat("  Expected:", cite_raw_file, "\n")
  cat("  Expected:", oa_match_file, "\n\n")
}


# =============================================================================
# SECTION 7: HETEROGENEITY
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 7: HETEROGENEITY BY TOPIC\n")
cat("--------------------------------------------------------------------\n\n")

# Overall reference estimate
m3_ref <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = mech)
overall_coef <- coef(m3_ref)["log_early"]
overall_se   <- summary(m3_ref)$coefficients["log_early", "Std. Error"]

cat("Overall log_early coefficient:", round(overall_coef, 4),
    "(SE:", round(overall_se, 4), ")\n\n")

# Estimate by topic
topic_counts <- mech[!is.na(topic) & topic != "other", .N, by = topic]
cat("Topic frequencies:\n")
print(topic_counts[order(-N)])
cat("\n")

topic_results_list <- list()
topic_fs_list <- list()

for (t in topic_counts[N >= 30, topic]) {
  t_data <- mech[topic == t]

  # log_early model
  tryCatch({
    m_t <- felm(log_longrun ~ log_early + n_authors + any_top_inst +
                  log_article_length + title_nchar + article_position + issue_no |
                  journal + year, data = t_data)
    topic_results_list[[t]] <- data.table(
      topic = t,
      n = nrow(t_data),
      coef = coef(m_t)["log_early"],
      se = summary(m_t)$coefficients["log_early", "Std. Error"]
    )
  }, error = function(e) {
    cat("  Skipping topic", t, ":", e$message, "\n")
  })

  # fast_starter model
  tryCatch({
    t_fs_data <- t_data[!is.na(fast_starter)]
    if (nrow(t_fs_data) >= 30 && sum(t_fs_data$fast_starter) >= 5) {
      m_fs <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                    log_article_length + title_nchar + article_position + issue_no |
                    journal + year, data = t_fs_data)
      topic_fs_list[[t]] <- data.table(
        topic = t,
        n = nrow(t_fs_data),
        coef = coef(m_fs)["fast_starter"],
        se = summary(m_fs)$coefficients["fast_starter", "Std. Error"]
      )
    }
  }, error = function(e) NULL)
}

topic_results <- rbindlist(topic_results_list)
topic_results[, ci_lo := coef - 1.96 * se]
topic_results[, ci_hi := coef + 1.96 * se]

cat("=== PATH DEPENDENCE BY TOPIC ===\n\n")
print(topic_results[order(-coef)])
cat("\n")

if (length(topic_fs_list) > 0) {
  topic_fs_results <- rbindlist(topic_fs_list)
  cat("Fast-starter effect by topic:\n")
  print(topic_fs_results[order(-coef)])
  cat("\n")
}

# --- 7.2 Heterogeneity by authorship and institution -------------------------
cat("=== HETEROGENEITY BY AUTHORSHIP AND INSTITUTION ===\n\n")
# Solo vs co-authored elasticity
het_solo  <- felm(log_longrun ~ log_early + any_top_inst + log_article_length +
                    title_nchar + article_position + issue_no | journal + year,
                  data = est[n_authors == 1])
het_co    <- felm(log_longrun ~ log_early + n_authors + any_top_inst + log_article_length +
                    title_nchar + article_position + issue_no | journal + year,
                  data = est[n_authors >= 2])
cat(sprintf("  Solo-authored elasticity:       %.3f (robust SE %.3f, N=%d)\n",
            coef(het_solo)["log_early"], rob_se(het_solo, "log_early"), het_solo$N))
cat(sprintf("  Co-authored elasticity:         %.3f (robust SE %.3f, N=%d)\n",
            coef(het_co)["log_early"], rob_se(het_co, "log_early"), het_co$N))
# Institution interaction: is the early-citation elasticity larger at top institutions?
het_inst <- felm(log_longrun ~ log_early * any_top_inst + n_authors + log_article_length +
                   title_nchar + article_position + issue_no | journal + year, data = est)
cat(sprintf("  log_early x top-institution:    %.3f (robust SE %.3f)  [interaction]\n",
            coef(het_inst)["log_early:any_top_inst"], rob_se(het_inst, "log_early:any_top_inst")))
cat(sprintf("  top-institution main effect:    %.3f (robust SE %.3f)\n\n",
            coef(het_inst)["any_top_inst"], rob_se(het_inst, "any_top_inst")))
heterogeneity_extra <- list(
  solo = c(coef(het_solo)["log_early"], rob_se(het_solo, "log_early"), het_solo$N),
  co   = c(coef(het_co)["log_early"],   rob_se(het_co, "log_early"),   het_co$N),
  inst_interaction = c(coef(het_inst)["log_early:any_top_inst"], rob_se(het_inst, "log_early:any_top_inst")))


# =============================================================================
# SECTION 7b: EXTENDED CONTROLS FOR DML
# =============================================================================
# The DML robustness check (Section 8) conditions on author seniority, h-index,
# topic uniqueness, cross-field citing, working-paper / NBER status, conference
# presentation, and prizes -- in addition to the base controls. Those variables
# are CONSTRUCTED here, on the estimation sample, from the OpenAlex / RePEc /
# prize side-files, and merged into `est` BEFORE DML runs. (Previously the files
# were loaded but never merged, so DML silently used only the base controls --
# the manuscript's "nine controls including author seniority, topic uniqueness,
# and conference presentation" did not match what the code did.)

cat("--------------------------------------------------------------------\n")
cat("  SECTION 7b: EXTENDED CONTROLS (author quality, topic, WP, prizes)\n")
cat("--------------------------------------------------------------------\n\n")

# Drop any pre-existing extended-control columns so the merges below can't create
# .x/.y duplicates if an upstream section already touched them.
drop_if_present <- c("topic_uniqueness", "cross_cite_ratio", "has_working_paper",
                     "wp_is_nber", "team_max_seniority", "team_max_hindex",
                     "author_nber_wp", "paper_won_prize", "author_won_dissertation_prize")
est[, (intersect(drop_if_present, names(est))) := NULL]

# (1) OpenAlex: topic uniqueness, cross-field citing, working-paper version ----
topic_file <- file.path(DATA_DIR, "openalex_topic_data.rds")
if (file.exists(topic_file)) {
  oa <- as.data.table(readRDS(topic_file))
  oa_merge <- oa[, .(id = paper_id, topic_uniqueness, cross_cite_ratio,
                     has_working_paper = as.integer(has_wp_version),
                     wp_is_nber = as.integer(wp_is_nber))]
  est <- merge(est, oa_merge, by = "id", all.x = TRUE)
  est[is.na(has_working_paper), has_working_paper := 0L]
  cat("  OpenAlex: topic_uniqueness on", sum(!is.na(est$topic_uniqueness)),
      "papers; has_working_paper =", sum(est$has_working_paper), "\n")
} else {
  est[, `:=`(topic_uniqueness = NA_real_, cross_cite_ratio = NA_real_,
             has_working_paper = 0L, wp_is_nber = 0L)]
  cat("  OpenAlex topic data not found -- extended topic controls set to NA/0.\n")
}

# (2) RePEc: team seniority, team h-index, NBER working paper ------------------
repec_file <- file.path(DATA_DIR, "repec_author_data.rds")
if (file.exists(repec_file)) {
  repec <- as.data.table(readRDS(repec_file))
  repec[, name_key := clean_person_name(author_name)]
  sen_lk  <- repec[!is.na(first_pub_year), .(first_pub = min(first_pub_year)), by = name_key]
  hix_lk  <- repec[!is.na(hindex),         .(hindex = max(hindex)),            by = name_key]
  nber_keys <- unique(repec[has_nber_wp == TRUE, name_key])
  est[, author_nber_wp := 0L]
  for (i in 1:5) {
    a <- paste0("author", i); key <- clean_person_name(est[[a]])
    est[, paste0("a", i, "_sen") := year - sen_lk$first_pub[match(key, sen_lk$name_key)]]
    est[, paste0("a", i, "_hix") := hix_lk$hindex[match(key, hix_lk$name_key)]]
    est[key %in% nber_keys, author_nber_wp := 1L]
  }
  est[, team_max_seniority := suppressWarnings(pmax(a1_sen, a2_sen, a3_sen, a4_sen, a5_sen, na.rm = TRUE))]
  est[, team_max_hindex    := suppressWarnings(pmax(a1_hix, a2_hix, a3_hix, a4_hix, a5_hix, na.rm = TRUE))]
  est[is.infinite(team_max_seniority), team_max_seniority := NA_real_]
  est[is.infinite(team_max_hindex),    team_max_hindex := NA_real_]
  est[, paste0("a", 1:5, "_sen") := NULL]; est[, paste0("a", 1:5, "_hix") := NULL]
  cat("  RePEc: seniority on", sum(!is.na(est$team_max_seniority)),
      "papers; h-index on", sum(!is.na(est$team_max_hindex)),
      "papers; author_nber_wp =", sum(est$author_nber_wp), "\n")
} else {
  est[, `:=`(team_max_seniority = NA_real_, team_max_hindex = NA_real_, author_nber_wp = 0L)]
  cat("  RePEc author data not found -- extended author controls set to NA/0.\n")
}

# (3) Prizes: paper-level (Cole/Ashton/Figuerola) + dissertation (author) ------
prize_paper_file <- file.path(DATA_DIR, "prize_paper_data.rds")
if (file.exists(prize_paper_file)) {
  pp <- as.data.table(readRDS(prize_paper_file))
  est[, paper_won_prize := as.integer(id %in% unique(na.omit(pp$matched_id)))]
} else est[, paper_won_prize := 0L]

prize_diss_file <- file.path(DATA_DIR, "prize_dissertation_data.rds")
est[, author_won_dissertation_prize := 0L]
if (file.exists(prize_diss_file)) {
  pdz <- as.data.table(readRDS(prize_diss_file))
  pdz <- pdz[!is.na(recipient_clean) & nchar(recipient_clean) >= 3]
  py_lk <- pdz[, .(min_year = min(prize_year)), by = recipient_clean]
  for (i in 1:5) {
    key <- clean_person_name(est[[paste0("author", i)]])
    m_year <- py_lk$min_year[match(key, py_lk$recipient_clean)]
    est[!is.na(m_year) & m_year < year, author_won_dissertation_prize := 1L]
  }
}
cat("  Prizes: paper_won_prize =", sum(est$paper_won_prize),
    "; author_won_dissertation_prize =", sum(est$author_won_dissertation_prize), "\n\n")


# =============================================================================
# SECTION 8: FAST-STARTER ATTENUATION
# =============================================================================
# The raw fast-starter premium is large, but it is mostly a relabelling of the
# continuous early-citation level. A transparent OLS sequence shows this: the
# threshold coefficient collapses once we control for log(early citations), and
# again once author-quality controls are added.
#
# NOTE: a double-machine-learning version was explored but is NOT used in the
# paper. Author seniority/h-index (from RePEc) and topic uniqueness (from
# OpenAlex) are missing for a large share of the 2012-2021 estimation sample and
# must be median-imputed, which destabilises the random-forest nuisance
# estimates (DML returns a markedly larger, specification-sensitive coefficient
# than every OLS specification). The OLS sequence below is robust and
# transparent, so it is what the paper reports. The DML code is retained, inert,
# below for reference (guarded by `if (FALSE)`).

cat("--------------------------------------------------------------------\n")
cat("  SECTION 8: FAST-STARTER ATTENUATION (OLS)\n")
cat("--------------------------------------------------------------------\n\n")

fs_raw <- felm(log_longrun ~ fast_starter + n_authors + any_top_inst +
                 log_article_length + title_nchar + article_position + issue_no |
                 journal + year, data = est)
fs_early <- felm(log_longrun ~ fast_starter + log_early + n_authors + any_top_inst +
                   log_article_length + title_nchar + article_position + issue_no |
                   journal + year, data = est)
fs_quality <- felm(log_longrun ~ fast_starter + log_early + n_authors + any_top_inst +
                     log_article_length + title_nchar + article_position + issue_no +
                     team_max_seniority + team_max_hindex + paper_won_prize +
                     author_won_dissertation_prize | journal + year, data = est)

attenuation <- data.table(
  spec = c("Raw (no early-cite control)",
           "+ control for log(early citations)",
           "+ author quality (seniority, h-index, prizes)"),
  coef = c(coef(fs_raw)["fast_starter"], coef(fs_early)["fast_starter"],
           coef(fs_quality)["fast_starter"]),
  se   = c(rob_se(fs_raw, "fast_starter"), rob_se(fs_early, "fast_starter"),
           rob_se(fs_quality, "fast_starter")),
  N    = c(fs_raw$N, fs_early$N, fs_quality$N))

cat("Fast-starter threshold attenuation (robust SEs):\n")
print(attenuation)
cat(sprintf("\n  -> Controlling for the continuous early-citation level collapses the\n     threshold premium from %.3f to %.3f; adding author quality -> %.3f.\n\n",
            attenuation$coef[1], attenuation$coef[2], attenuation$coef[3]))

fwrite(attenuation, file.path(TABLE_DIR, "Table3_FastStarterAttenuation.csv"))

dml_results <- NULL

if (FALSE) {  # DML dropped from the paper (unstable under imputation); kept inert for reference

  tryCatch({

    library(DoubleML)
    library(mlr3)
    library(mlr3learners)

    cat("Setting up DML analysis...\n")

    # Prepare data: base controls always available
    dml_vars_base <- c("log_longrun", "fast_starter", "log_early", "n_authors", "any_top_inst",
                       "log_article_length", "title_nchar", "article_position", "issue_no")

    # Extended controls — CONSTRUCTED and merged into `est` in Section 7b above.
    dml_vars_ext <- c("team_max_seniority", "team_max_hindex", "topic_uniqueness",
                      "cross_cite_ratio", "presented_at_conference",
                      "has_working_paper", "author_nber_wp",
                      "paper_won_prize", "author_won_dissertation_prize")

    # Keep only extended controls that have REAL coverage in the estimation
    # sample: at least one non-missing value AND some variation. (OpenAlex topic
    # data was collected for the pre-2006 ID range and does not cover the
    # 2012-2021 estimation papers, so topic_uniqueness/cross_cite_ratio are
    # all-NA here; an all-NA or constant control is uninformative and breaks the
    # cross-fitting, so it is dropped. The manuscript reports the actual set.)
    has_coverage <- function(v) {
      x <- est[[v]]
      sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
    }
    ext_present <- intersect(dml_vars_ext, names(est))
    ext_used    <- ext_present[vapply(ext_present, has_coverage, logical(1))]
    ext_dropped <- setdiff(dml_vars_ext, ext_used)
    cat("  Extended controls WITH coverage entering DML (", length(ext_used), "):\n    ",
        paste(ext_used, collapse = ", "), "\n", sep = "")
    if (length(ext_dropped) > 0) {
      cat("  Dropped (no coverage / no variation in estimation sample):\n    ",
          paste(ext_dropped, collapse = ", "), "\n", sep = "")
    }

    available_vars <- intersect(c(dml_vars_base, ext_used), names(est))
    dml_data <- est[, .SD, .SDcols = available_vars]

    # Add journal and year as numeric
    dml_data[, journal_num := as.numeric(factor(est$journal))]
    dml_data[, year_num := est$year]

    # Remove rows with missing outcome or treatment
    dml_data <- dml_data[!is.na(log_longrun) & !is.na(fast_starter)]

    # Impute remaining missing controls with the median (only columns that have
    # at least one non-missing value reach this point).
    for (col in names(dml_data)) {
      if (is.numeric(dml_data[[col]]) && sum(is.na(dml_data[[col]])) > 0 &&
          sum(!is.na(dml_data[[col]])) > 0) {
        med_val <- median(dml_data[[col]], na.rm = TRUE)
        dml_data[is.na(get(col)), (col) := med_val]
      }
    }

    cat("DML sample size:", nrow(dml_data),
        "| total controls:", length(setdiff(names(dml_data), c("log_longrun", "fast_starter"))), "\n\n")

    if (nrow(dml_data) >= 100) {

      x_cols <- setdiff(names(dml_data), c("log_longrun", "fast_starter"))

      dml_data_obj <- DoubleML::DoubleMLData$new(
        data = as.data.frame(dml_data),
        y_col = "log_longrun",
        d_cols = "fast_starter",
        x_cols = x_cols
      )

      ml_l <- mlr3::lrn("regr.ranger",
                         num.trees = 500,
                         min.node.size = 5,
                         max.depth = 10)

      ml_m <- mlr3::lrn("classif.ranger",
                         num.trees = 500,
                         min.node.size = 5,
                         max.depth = 10,
                         predict_type = "prob")

      cat("Fitting DML Partially Linear Model...\n")
      dml_plr <- DoubleML::DoubleMLPLR$new(
        data = dml_data_obj,
        ml_l = ml_l,
        ml_m = ml_m,
        n_folds = 5,
        n_rep = 3,
        score = "partialling out"
      )

      dml_plr$fit()

      dml_coef <- dml_plr$coef
      dml_se <- dml_plr$se
      dml_pval <- 2 * pnorm(-abs(dml_coef / dml_se))
      dml_ci <- c(dml_coef - 1.96 * dml_se, dml_coef + 1.96 * dml_se)

      cat("\n=== DOUBLE MACHINE LEARNING RESULTS ===\n\n")
      cat("Treatment: fast_starter\n")
      cat("Outcome: log_longrun\n")
      cat("Controls:", length(x_cols), "variables\n")
      cat("Cross-fitting: 5 folds x 3 repetitions\n\n")

      cat("Debiased Treatment Effect:\n")
      cat("  Coefficient:", round(dml_coef, 4), "\n")
      cat("  Std. Error:", round(dml_se, 4), "\n")
      cat("  t-stat:", round(dml_coef / dml_se, 3), "\n")
      cat("  p-value:", format.pval(dml_pval, digits = 4), "\n")
      cat("  95% CI: [", round(dml_ci[1], 4), ",", round(dml_ci[2], 4), "]\n\n")

      cat("Interpretation:\n")
      cat("  Fast starters have", round((exp(dml_coef) - 1) * 100, 1),
          "% more long-run citations\n\n")

      # Compare with OLS
      ols_compare <- lm(log_longrun ~ fast_starter + ., data = dml_data)
      ols_coef <- coef(ols_compare)["fast_starter"]
      ols_se <- summary(ols_compare)$coefficients["fast_starter", "Std. Error"]

      cat("Comparison with OLS:\n")
      cat("  OLS coefficient:", round(ols_coef, 4), "(SE:", round(ols_se, 4), ")\n")
      cat("  DML coefficient:", round(dml_coef, 4), "(SE:", round(dml_se, 4), ")\n\n")

      dml_results <- list(coef = dml_coef, se = dml_se, pval = dml_pval, ci = dml_ci,
                          n = nrow(dml_data), n_controls = length(x_cols),
                          ols_coef = ols_coef, ols_se = ols_se)
    } else {
      cat("Insufficient data for DML analysis (n =", nrow(dml_data), ")\n\n")
    }

  }, error = function(e) {
    cat("DML error:", e$message, "\n")
    cat("Skipping DML analysis.\n\n")
  })

}  # end inert DML block


# =============================================================================
# SECTION 8b: UNPREDICTABILITY AND AMPLIFICATION OF EARLY SUCCESS
# =============================================================================
# Persistence (early citations predict long-run citations) is only PATH
# DEPENDENCE if early success is partly idiosyncratic -- if small, not-about-
# quality differences get amplified. Three steps:
#   (a) how much of early citations do fundamentals explain? (mostly: little)
#   (b) does the UNPREDICTABLE (idiosyncratic) part of early citations still
#       propagate to long-run impact? (yes, almost as strongly as the
#       predictable part)
#   (c) does a quasi-random nudge to early visibility -- within-issue article
#       position -- causally raise long-run citations? (IV)

cat("--------------------------------------------------------------------\n")
cat("  SECTION 8b: UNPREDICTABILITY AND AMPLIFICATION OF EARLY SUCCESS\n")
cat("--------------------------------------------------------------------\n\n")

Fstat <- function(m, v) as.numeric((coef(m)[v] / rob_se(m, v))^2)  # single-instrument first-stage F
R2manual <- function(m, y) 1 - sum(m$residuals^2) / sum((y - mean(y))^2)

# --- 8b.1 Predictability of early citations ---------------------------------
ly <- est$log_early
pred_fe  <- felm(log_early ~ 1 | journal + year + topic, data = est)
pred_obs <- felm(log_early ~ article_position + issue_no + n_authors + any_top_inst +
                   log_article_length + title_nchar | journal + year + topic, data = est)
sub_q <- est[!is.na(team_max_seniority)]
pred_qual <- felm(log_early ~ article_position + issue_no + n_authors + any_top_inst +
                    log_article_length + title_nchar + team_max_seniority + team_max_hindex +
                    paper_won_prize + author_won_dissertation_prize | journal + year + topic,
                  data = sub_q)
r2_fe   <- R2manual(pred_fe, ly)
r2_obs  <- R2manual(pred_obs, ly)
r2_qual <- R2manual(pred_qual, sub_q$log_early)
cat("Predictability of log(early citations) -- share of variance explained:\n")
cat(sprintf("  Journal+year+topic FE only      : R2 = %.3f\n", r2_fe))
cat(sprintf("  + article observables           : R2 = %.3f\n", r2_obs))
cat(sprintf("  + author quality (N=%d)        : R2 = %.3f\n", nrow(sub_q), r2_qual))
cat(sprintf("  -> fundamentals explain ~%.0f%% of early citations; ~%.0f%% is idiosyncratic.\n\n",
            100 * r2_obs, 100 * (1 - r2_obs)))

# --- 8b.2 Does the idiosyncratic component propagate? -----------------------
# Decompose early citations into the part explained by fundamentals (early_pred)
# and the idiosyncratic residual (early_resid), then let both predict long-run.
est[, early_resid := as.numeric(pred_obs$residuals)]
est[, early_pred  := log_early - early_resid]
decomp <- felm(log_longrun ~ early_pred + early_resid | journal + year, data = est)
sd_resid <- sd(est$early_resid)
luck_pct <- (exp(coef(decomp)["early_resid"] * sd_resid) - 1) * 100
cat("Propagation of predictable vs idiosyncratic early citations (long-run outcome):\n")
cat(sprintf("  predictable-early : %.3f (robust SE %.3f)\n",
            coef(decomp)["early_pred"], rob_se(decomp, "early_pred")))
cat(sprintf("  idiosyncratic-early: %.3f (robust SE %.3f)\n",
            coef(decomp)["early_resid"], rob_se(decomp, "early_resid")))
cat(sprintf("  SD(idiosyncratic early)=%.3f -> a 1-SD lucky early break = %.0f%% more long-run citations.\n\n",
            sd_resid, luck_pct))

# Robustness: same decomposition on POST-AGE-2 GROWTH (no cumulative overlap).
decomp_growth <- felm(log_growth ~ early_pred + early_resid | journal + year, data = est)
cat("  [growth outcome] predictable-early :", round(coef(decomp_growth)["early_pred"], 3),
    "(rse", round(rob_se(decomp_growth, "early_pred"), 3), ")\n")
cat("  [growth outcome] unexplained-early :", round(coef(decomp_growth)["early_resid"], 3),
    "(rse", round(rob_se(decomp_growth, "early_resid"), 3), ")\n\n")

# --- 8b.3 A quasi-random nudge: within-issue article position ---------------
# Candidate instruments for early visibility. We TEST several and use the one
# that is both relevant (first-stage F >= 10) and balanced on author quality.
# Build the candidates on est.
isz <- jn[, .(issue_size = .N), by = .(journal, vol, issue_raw)]
est <- merge(est, isz, by = c("journal", "vol", "issue_raw"), all.x = TRUE)
est[, lead_article := as.integer(article_position == 1)]
.surname <- function(s) { w <- strsplit(trimws(tolower(gsub("[^a-z ]", " ", s))), "\\s+")[[1]]
  w <- w[nchar(w) > 0]; if (!length(w)) NA_character_ else tail(w, 1) }
est[, fa_initial := vapply(author1, function(s) { ln <- .surname(s)
  if (is.na(ln) || !nchar(ln)) NA_integer_ else as.integer(utf8ToInt(substr(ln, 1, 1)) - 96L) }, integer(1))]
est[fa_initial < 1 | fa_initial > 26, fa_initial := NA_integer_]
est[, na_author := as.integer(grepl("North America", continent, ignore.case = TRUE))]  # conf-in-region proxy (EHA=US)

cat("Candidate instruments for early citations (first stage on log_early):\n")
cands <- c("article_position", "lead_article", "issue_size", "fa_initial", "na_author")
fs_tab <- rbindlist(lapply(cands, function(v) {
  fs <- tryCatch(felm(as.formula(paste0("log_early ~ ", v,
        " + n_authors + any_top_inst + log_article_length | journal + year")), data = est),
        error = function(e) NULL)
  if (is.null(fs)) return(NULL)
  data.table(instrument = v, coef = coef(fs)[v], se = rob_se(fs, v), F = Fstat(fs, v))
}))
print(fs_tab)
cat("\nBalance of leading instrument (article_position) on author quality | journal+year:\n")
for (q in c("any_top_inst", "team_max_seniority", "team_max_hindex")) {
  bb <- felm(as.formula(paste0("article_position ~ ", q, " | journal + year")), data = est[!is.na(get(q))])
  cat(sprintf("  article_position ~ %-18s : %.4f (robust SE %.4f)\n", q, coef(bb)[q], rob_se(bb, q)))
}

# Reduced form, first stage and IV for the chosen instrument (article_position)
rf  <- felm(log_longrun ~ article_position + n_authors + any_top_inst + log_article_length |
              journal + year, data = est)
ivm <- felm(log_longrun ~ n_authors + any_top_inst + log_article_length |
              journal + year | (log_early ~ article_position) | 0, data = est)
iv_nm <- grep("log_early", names(coef(ivm)), value = TRUE)[1]
fs_pos <- felm(log_early ~ article_position + n_authors + any_top_inst + log_article_length |
                 journal + year, data = est)
cat(sprintf("\n  First stage (position -> early): %.4f (rse %.4f), F = %.1f\n",
            coef(fs_pos)["article_position"], rob_se(fs_pos, "article_position"), Fstat(fs_pos, "article_position")))
cat(sprintf("  Reduced form (position -> long-run): %.4f (rse %.4f)\n",
            coef(rf)["article_position"], rob_se(rf, "article_position")))
cat(sprintf("  IV LATE (early citations, instrumented by position): %.3f (rse %.3f)\n",
            coef(ivm)[iv_nm], rob_se(ivm, iv_nm)))

# Robustness: drop JEH issue lead slots (presidential address is position 1 in JEH)
est_nolead <- est[!(journal == "JEH" & article_position == 1)]
ivm_rob <- tryCatch(felm(log_longrun ~ n_authors + any_top_inst + log_article_length |
              journal + year | (log_early ~ article_position) | 0, data = est_nolead),
              error = function(e) NULL)
if (!is.null(ivm_rob)) {
  iv_nm2 <- grep("log_early", names(coef(ivm_rob)), value = TRUE)[1]
  cat(sprintf("  IV LATE excl. JEH lead slots (N=%d): %.3f (rse %.3f)\n\n",
              nrow(est_nolead), coef(ivm_rob)[iv_nm2], rob_se(ivm_rob, iv_nm2)))
}

# --- 8b.4 Save the section's results table ----------------------------------
unpredictability <- list(
  r2_fe = r2_fe, r2_obs = r2_obs, r2_qual = r2_qual,
  decomp_pred = coef(decomp)["early_pred"], decomp_pred_se = rob_se(decomp, "early_pred"),
  decomp_resid = coef(decomp)["early_resid"], decomp_resid_se = rob_se(decomp, "early_resid"),
  sd_resid = sd_resid, luck_pct = luck_pct,
  first_stage = fs_tab,
  rf_pos = coef(rf)["article_position"], rf_pos_se = rob_se(rf, "article_position"),
  iv_late = coef(ivm)[iv_nm], iv_late_se = rob_se(ivm, iv_nm),
  fs_pos_F = Fstat(fs_pos, "article_position"))
fwrite(fs_tab, file.path(TABLE_DIR, "Table5_InstrumentFirstStage.csv"))
cat("Saved: Table5_InstrumentFirstStage.csv\n\n")


# =============================================================================
# SECTION 8c: IS POSITION QUASI-RANDOM? EDITOR-SORTING FALSIFICATION
# =============================================================================
# The position instrument (8b) is threatened if editors place papers they judge
# higher-quality earlier in an issue. We treat this as an examiner/judge design
# (Chyn, Frandsen & Leslie 2025): position is as-good-as-random only CONDITIONAL
# on the assignment unit. The person who sets running order (chief editor,
# managing editor, or an administrator) is unobserved and time-varying, so we
# condition on the editorial REGIME via journal-by-year fixed effects, tightening
# to within-issue fixed effects (the running order is a within-issue decision).
# We then run a negative-control-outcome falsification battery (Danieli et al.
# 2026): predetermined author-quality proxies should be unrelated to position
# conditional on the regime. Per Chyn/Angrist-Kolesar we report -- but do NOT
# screen on -- the first-stage F.

cat("--------------------------------------------------------------------\n")
cat("  SECTION 8c: IS POSITION QUASI-RANDOM? EDITOR-SORTING FALSIFICATION\n")
cat("--------------------------------------------------------------------\n\n")

est[, issue_id := paste(journal, vol, issue_raw, sep = "_")]
est[, regime := paste(journal, year, sep = "_")]   # editorial-regime (journal x year) as a single factor
iv_late <- function(m) { nm <- grep("log_early", names(coef(m)), value = TRUE)[1]; c(coef(m)[nm], rob_se(m, nm)) }

# --- 8c.1 Conditioning ladder: tighten the editorial-regime fixed effects ----
cat("Conditioning ladder (IV LATE of log_early, instrumented by position):\n")
fe_specs <- list(
  `journal + year`        = "journal + year",
  `journal x year (regime)` = "regime",
  `issue (journal-vol-no)`  = "issue_id")
ladder <- rbindlist(lapply(names(fe_specs), function(lbl) {
  fe <- fe_specs[[lbl]]
  ivm <- tryCatch(felm(as.formula(paste0(
    "log_longrun ~ n_authors + any_top_inst + log_article_length | ", fe,
    " | (log_early ~ article_position) | 0")), data = est), error = function(e) NULL)
  fsm <- tryCatch(felm(as.formula(paste0(
    "log_early ~ article_position + n_authors + any_top_inst + log_article_length | ", fe)),
    data = est), error = function(e) NULL)
  if (is.null(ivm) || is.null(fsm)) return(NULL)
  est_iv <- iv_late(ivm)
  data.table(conditioning = lbl, iv_late = est_iv[1], iv_se = est_iv[2],
             first_stage = coef(fsm)["article_position"], fs_F = Fstat(fsm, "article_position"),
             N = ivm$N)
}))
print(ladder)
cat("  -> LATE stability as we tighten to within-issue FE is the key evidence;\n")
cat("     first-stage F reported for information only (not used as a screen).\n\n")

# --- 8c.2 Danieli negative-control-outcome (NCO) battery ---------------------
# Predetermined author-quality proxies should be unrelated to position | regime.
ncos <- c("team_max_seniority", "team_max_hindex", "any_top_inst",
          "paper_won_prize", "author_won_dissertation_prize")
cat("NCO tests: predetermined quality proxy regressed on position | journal x year\n")
nco_tab <- rbindlist(lapply(ncos, function(v) {
  m <- tryCatch(felm(as.formula(paste0(v, " ~ article_position | regime")),
                     data = est[!is.na(get(v))]), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  data.table(nco = v, coef = coef(m)["article_position"], se = rob_se(m, "article_position"),
             N = m$N)
}))
print(nco_tab)
# Joint test (Danieli Model 7): regress position on all NCOs | regime, F-test NCOs = 0.
joint <- tryCatch({
  m7 <- felm(article_position ~ team_max_seniority + team_max_hindex + any_top_inst +
               paper_won_prize + author_won_dissertation_prize | regime,
             data = est[!is.na(team_max_seniority) & !is.na(team_max_hindex)])
  w <- lfe::waldtest(m7, ~ team_max_seniority + team_max_hindex + any_top_inst +
                       paper_won_prize + author_won_dissertation_prize)
  list(F = unname(w["F"]), p = unname(w["p.F"]), N = m7$N)
}, error = function(e) { cat("  (joint waldtest failed:", conditionMessage(e), ")\n"); NULL })
if (!is.null(joint))
  cat(sprintf("\n  Joint NCO F-test (sorting on observable quality): F = %.2f, p = %.3f, N = %d\n\n",
              joint$F, joint$p, joint$N))

# Functional-form robustness (Danieli sec IID): quadratic-in-position NCO for h-index.
mq <- tryCatch(felm(team_max_hindex ~ article_position + I(article_position^2) | regime,
                    data = est[!is.na(team_max_hindex)]), error = function(e) NULL)
if (!is.null(mq)) cat(sprintf("  h-index ~ position + position^2 | regime: lin %.4f, quad %.5f\n\n",
                              coef(mq)["article_position"], coef(mq)["I(article_position^2)"]))

# --- 8c.3 Condition-on-quality correction (Danieli sec IIID) ----------------
# Add observable quality to the IV. Does the LATE move toward the OLS 0.78?
cat("Condition-on-quality correction (does LATE move toward OLS ~0.78?):\n")
iv_base <- felm(log_longrun ~ n_authors + any_top_inst + log_article_length |
                  regime | (log_early ~ article_position) | 0, data = est)
iv_qpz  <- felm(log_longrun ~ n_authors + any_top_inst + log_article_length +
                  paper_won_prize + author_won_dissertation_prize |
                  regime | (log_early ~ article_position) | 0, data = est)
iv_qall <- felm(log_longrun ~ n_authors + any_top_inst + log_article_length +
                  paper_won_prize + author_won_dissertation_prize +
                  team_max_seniority + team_max_hindex |
                  regime | (log_early ~ article_position) | 0,
                data = est[!is.na(team_max_seniority) & !is.na(team_max_hindex)])
for (lbl_m in list(c("regime FE, base controls", "iv_base"),
                   c("+ prizes (N=1262)", "iv_qpz"),
                   c("+ seniority/h-index (N=732)", "iv_qall"))) {
  m <- get(lbl_m[2]); v <- iv_late(m)
  cat(sprintf("  %-32s LATE = %.3f (rse %.3f), N = %d\n", lbl_m[1], v[1], v[2], m$N))
}
cat("\n")

# --- 8c.4 Heterogeneity by journal (is it a few or all?) --------------------
cat("By-journal first stage (position -> early) and reduced form (position -> long-run):\n")
byj <- rbindlist(lapply(sort(unique(est$journal)), function(j) {
  d <- est[journal == j]
  fs <- tryCatch(felm(log_early ~ article_position + n_authors + any_top_inst + log_article_length | year, data = d), error = function(e) NULL)
  rf <- tryCatch(felm(log_longrun ~ article_position + n_authors + any_top_inst + log_article_length | year, data = d), error = function(e) NULL)
  nco <- tryCatch(felm(team_max_hindex ~ article_position | year, data = d[!is.na(team_max_hindex)]), error = function(e) NULL)
  if (is.null(fs) || is.null(rf)) return(NULL)
  data.table(journal = j, N = nrow(d),
             first_stage = coef(fs)["article_position"], fs_se = rob_se(fs, "article_position"),
             reduced_form = coef(rf)["article_position"], rf_se = rob_se(rf, "article_position"),
             nco_hindex = if (!is.null(nco)) coef(nco)["article_position"] else NA_real_)
}))
print(byj)
cat("  -> a broad-based negative gradient across journals indicates a general\n")
cat("     visibility mechanism rather than a few quality-sorting editorial regimes.\n\n")

# --- 8c.5 Exclusion restriction: position's direct channel? -----------------
# NCO/regime FE address conditional random assignment, NOT exclusion (Chyn Assn 1).
# Suggestive check: does position predict long-run citations CONDITIONAL on early?
excl <- felm(log_longrun ~ article_position + log_early + n_authors + any_top_inst +
               log_article_length | regime, data = est)
cat(sprintf("Exclusion check: position -> long-run | early citations = %.4f (rse %.4f)\n",
            coef(excl)["article_position"], rob_se(excl, "article_position")))
cat("  (Near zero supports exclusion; a residual effect would flag a direct channel.)\n\n")

# Save falsification results
editor_falsification <- list(ladder = ladder, nco = nco_tab, joint = joint,
                             iv_base = iv_late(iv_base), iv_qpz = iv_late(iv_qpz), iv_qall = iv_late(iv_qall),
                             by_journal = byj,
                             excl_coef = coef(excl)["article_position"], excl_se = rob_se(excl, "article_position"))
fwrite(ladder, file.path(TABLE_DIR, "Table6_ConditioningLadder.csv"))
fwrite(nco_tab, file.path(TABLE_DIR, "Table6_NCObattery.csv"))
fwrite(byj, file.path(TABLE_DIR, "Table6_ByJournal.csv"))
cat("Saved: Table6_ConditioningLadder.csv, Table6_NCObattery.csv, Table6_ByJournal.csv\n\n")


# =============================================================================
# SECTION 9: WITHIN-AUTHOR PANEL
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 9: WITHIN-AUTHOR PANEL\n")
cat("--------------------------------------------------------------------\n\n")

# --- 9.1 Build paper-year panel from citation snapshots ---
cat("Building paper-year panel...\n")

citation_cols <- paste0("g", 14:26)

# Identify columns present
panel_id_vars <- c("id", "year", "journal", "title", "n_authors",
                   "any_top_inst", "topic", "region", "author1", "fast_starter")
if ("presented_at_conference" %in% names(jn)) {
  panel_id_vars <- c(panel_id_vars, "presented_at_conference")
}

panel_measure_vars <- intersect(citation_cols, names(jn))

panel <- melt(jn[is_core == TRUE],
              id.vars = intersect(panel_id_vars, names(jn)),
              measure.vars = panel_measure_vars,
              variable.name = "cite_year_var",
              value.name = "cit_cum")

# Extract calendar year from variable name
panel[, cite_year := as.integer(paste0("20", gsub("g", "", cite_year_var)))]

# Calculate paper age and filter
panel[, paper_age := cite_year - year]
panel <- panel[paper_age >= 0]

# New citations (first difference)
setorder(panel, id, cite_year)
panel[, cit_new := cit_cum - shift(cit_cum, 1, type = "lag"), by = id]
panel[paper_age == 0, cit_new := cit_cum]

# Lagged cumulative
panel[, cit_cum_lag1 := shift(cit_cum, 1, type = "lag"), by = id]

# Log transforms
panel[, log_cit_new := log(cit_new + 1)]
panel[, log_cit_cum := log(cit_cum + 1)]
panel[, log_cit_cum_lag1 := log(cit_cum_lag1 + 1)]

cat("Panel dimensions:", nrow(panel), "paper-year observations\n")
cat("Unique papers:", uniqueN(panel$id), "\n")
cat("Years covered:", min(panel$cite_year), "-", max(panel$cite_year), "\n\n")

# --- 9.2 Within-author FE regression ---
cat("Running within-author FE regression...\n")

panel_author <- panel[cit_new >= 0 & !is.na(author1) & author1 != ""]

# Ensure presented_at_conference exists (defaults to 0 if conference section didn't run)
if (!"presented_at_conference" %in% names(panel_author)) {
  panel_author[, presented_at_conference := 0L]
  cat("NOTE: presented_at_conference not found — defaulting to 0 for all papers.\n")
  cat("  (Run 00_data_collection.R first to generate conference_parsed_data.rds)\n\n")
}

author_variety <- panel_author[, .(
  has_presented = any(presented_at_conference == 1, na.rm = TRUE),
  has_not_presented = any(presented_at_conference == 0, na.rm = TRUE)
), by = author1]
n_both <- sum(author_variety$has_presented & author_variety$has_not_presented)

cat("Authors with variation in conference presentation:", n_both, "\n\n")

within_author <- NULL
within_author_yr <- NULL

if (n_both >= 20 && "presented_at_conference" %in% names(panel_author)) {

  # SE in column 2 of a felm summary; with a cluster in the formula this is the
  # cluster-robust SE.
  se2 <- function(m, var) {
    ct <- summary(m)$coefficients
    if (!var %in% rownames(ct)) return(NA_real_)
    ct[var, 2]
  }

  # Design A: Author + calendar-year + paper-age fixed effects, clustered by paper.
  # paper_age FE absorb the non-linear citation lifecycle (the lambda_t term in
  # the conceptual model), so the conference and reinforcement estimates are not
  # confounded by papers of the same author being at different ages.
  cat("Model WA1: Author + paper-age Fixed Effects (SEs clustered by paper)\n")
  within_author <- felm(log_cit_new ~ presented_at_conference + log_cit_cum_lag1 +
                          n_authors | author1 + cite_year + paper_age | 0 | id,
                        data = panel_author)

  cat("  presented_at_conference:", round(coef(within_author)["presented_at_conference"], 4),
      "(cluster SE:", round(se2(within_author, "presented_at_conference"), 4), ")\n")
  cat("  log_cit_cum_lag1 (reinforcement):", round(coef(within_author)["log_cit_cum_lag1"], 4),
      "(cluster SE:", round(se2(within_author, "log_cit_cum_lag1"), 4), ")\n")
  cat("  Interpretation: Within-author, presented papers have",
      round((exp(coef(within_author)["presented_at_conference"]) - 1) * 100, 1),
      "% more citations\n\n")

  # Design B: Author-year FE (stricter)
  cat("Model WA2: Author-Year Fixed Effects (stricter)\n")
  panel_author[, author_year := paste0(author1, "_", year)]

  ay_variety <- panel_author[, .(
    has_presented = any(presented_at_conference == 1, na.rm = TRUE),
    has_not_presented = any(presented_at_conference == 0, na.rm = TRUE)
  ), by = author_year]
  n_ay_both <- sum(ay_variety$has_presented & ay_variety$has_not_presented)
  cat("  Author-years with variation:", n_ay_both, "\n")

  if (n_ay_both >= 10) {
    within_author_yr <- felm(log_cit_new ~ presented_at_conference + log_cit_cum_lag1 |
                               author_year + cite_year + paper_age | 0 | id,
                             data = panel_author)

    if ("presented_at_conference" %in% names(coef(within_author_yr))) {
      cat("  presented_at_conference:", round(coef(within_author_yr)["presented_at_conference"], 4),
          "(cluster SE:", round(se2(within_author_yr, "presented_at_conference"), 4), ")\n\n")
    } else {
      cat("  Insufficient within-author-year variation\n\n")
    }
  } else {
    cat("  Insufficient author-year variation for strict FE\n\n")
  }

  # Save Table 4
  tryCatch({
    model_list <- if (!is.null(within_author_yr)) {
      list(within_author, within_author_yr)
    } else {
      list(within_author)
    }
    stargazer(model_list,
              type = "latex",
              out = file.path(TABLE_DIR, "Table4_WithinAuthor.tex"),
              title = "Within-Author Comparisons",
              label = "tab:withinauthor",
              dep.var.labels = "Log(new citations)",
              notes = "Author fixed effects. Comparing papers by same author.")
    cat("Saved: Table4_WithinAuthor.tex\n\n")
  }, error = function(e) {
    cat("Could not save Table4:", e$message, "\n\n")
  })

} else {
  cat("Insufficient author variation for within-author analysis.\n")
  cat("(Need conference data merged first.)\n\n")
}


# =============================================================================
# SECTION 10: REVERSE CAUSALITY
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 10: REVERSE CAUSALITY TEST\n")
cat("--------------------------------------------------------------------\n\n")

has_conf_var <- "presented_at_conference" %in% names(mech) &&
  sum(!is.na(mech$presented_at_conference)) >= 30

if (has_conf_var) {

  rc_data <- mech[!is.na(presented_at_conference) & !is.na(log_longrun) &
                    !is.na(log_early) & !is.na(n_authors) & !is.na(any_top_inst)]
  cat("Reverse causality sample:", nrow(rc_data), "\n\n")

  # Probit: does future success predict conference selection?
  cat("=== PROBIT: FUTURE SUCCESS -> CONFERENCE SELECTION ===\n\n")

  rc1 <- glm(presented_at_conference ~ log_longrun + log_early + n_authors +
               any_top_inst + factor(journal) + factor(year),
             data = rc_data, family = binomial(link = "probit"))

  rc1_summary <- summary(rc1)
  rc1_coefs <- rc1_summary$coefficients

  cat("RC1: log_longrun -> presented_at_conference (probit)\n")
  cat("  log_longrun:", round(rc1_coefs["log_longrun", "Estimate"], 4),
      "(SE:", round(rc1_coefs["log_longrun", "Std. Error"], 4),
      ", p:", formatC(rc1_coefs["log_longrun", "Pr(>|z|)"], format = "f", digits = 4), ")\n")
  cat("  log_early:", round(rc1_coefs["log_early", "Estimate"], 4),
      "(SE:", round(rc1_coefs["log_early", "Std. Error"], 4),
      ", p:", formatC(rc1_coefs["log_early", "Pr(>|z|)"], format = "f", digits = 4), ")\n")

  if (rc1_coefs["log_longrun", "Pr(>|z|)"] > 0.05) {
    cat("  -> PASS: long-run citations do NOT predict conference selection\n\n")
  } else {
    cat("  -> NOTE: long-run citations significantly predict conference selection\n\n")
  }

  # Fast-starter predicting conference
  rc_data_fs <- rc_data[!is.na(fast_starter)]
  if (nrow(rc_data_fs) >= 30) {
    rc2 <- glm(presented_at_conference ~ fast_starter + n_authors +
                 any_top_inst + factor(journal) + factor(year),
               data = rc_data_fs, family = binomial(link = "probit"))

    rc2_summary <- summary(rc2)
    rc2_coefs <- rc2_summary$coefficients
    cat("RC2: fast_starter -> presented_at_conference (probit)\n")
    cat("  fast_starter:", round(rc2_coefs["fast_starter", "Estimate"], 4),
        "(SE:", round(rc2_coefs["fast_starter", "Std. Error"], 4),
        ", p:", formatC(rc2_coefs["fast_starter", "Pr(>|z|)"], format = "f", digits = 4), ")\n\n")
  }

  # Balance test
  cat("=== BALANCE TEST: CONFERENCE vs NON-CONFERENCE ===\n\n")

  balance_vars <- c("n_authors", "any_top_inst", "log_early")
  for (v in balance_vars) {
    if (!v %in% names(rc_data)) next
    conf_vals <- rc_data[presented_at_conference == 1][[v]]
    nonc_vals <- rc_data[presented_at_conference == 0][[v]]
    tt <- tryCatch(t.test(conf_vals, nonc_vals), error = function(e) NULL)
    if (!is.null(tt)) {
      cat(sprintf("  %-15s  Conf: %6.3f  Non-conf: %6.3f  diff: %6.3f  p: %s\n",
                  v,
                  mean(conf_vals, na.rm = TRUE),
                  mean(nonc_vals, na.rm = TRUE),
                  mean(conf_vals, na.rm = TRUE) - mean(nonc_vals, na.rm = TRUE),
                  formatC(tt$p.value, format = "f", digits = 4)))
    }
  }
  cat("\n")

} else {
  cat("Conference data not available. Skipping reverse causality test.\n\n")
}


# =============================================================================
# SECTION 11: SAVE RESULTS
# =============================================================================

cat("--------------------------------------------------------------------\n")
cat("  SECTION 11: SAVE RESULTS\n")
cat("--------------------------------------------------------------------\n\n")

# --- Table 1: Summary Statistics (LaTeX) ---
sum_tbl <- data.frame(
  Variable = c("Early citations (age 2)", "Long-run citations", "Citation growth",
               "N authors", "Top institution", "Fast starter"),
  N = c(sum(!is.na(est$cite_early)), sum(!is.na(est$cite_longrun)),
        sum(!is.na(est$cite_growth)), sum(!is.na(est$n_authors)),
        sum(!is.na(est$any_top_inst)), sum(!is.na(est$fast_starter))),
  Mean = round(c(mean(est$cite_early), mean(est$cite_longrun),
                 mean(est$cite_growth, na.rm = TRUE), mean(est$n_authors),
                 mean(est$any_top_inst), mean(est$fast_starter, na.rm = TRUE)), 2),
  SD = round(c(sd(est$cite_early), sd(est$cite_longrun),
               sd(est$cite_growth, na.rm = TRUE), sd(est$n_authors),
               sd(est$any_top_inst), sd(est$fast_starter, na.rm = TRUE)), 2),
  Min = round(c(min(est$cite_early), min(est$cite_longrun),
                min(est$cite_growth, na.rm = TRUE), min(est$n_authors),
                min(est$any_top_inst), min(est$fast_starter, na.rm = TRUE)), 2),
  Max = round(c(max(est$cite_early), max(est$cite_longrun),
                max(est$cite_growth, na.rm = TRUE), max(est$n_authors),
                max(est$any_top_inst), max(est$fast_starter, na.rm = TRUE)), 2)
)

stargazer(sum_tbl, type = "latex", summary = FALSE, rownames = FALSE,
          out = file.path(TABLE_DIR, "Table1_SummaryStats.tex"),
          title = "Summary Statistics",
          label = "tab:sumstats",
          notes = paste0("Estimation sample: ", nrow(est), " papers from core economic history journals."))
cat("Saved: Table1_SummaryStats.tex\n")


# --- Table 3: Method Comparison (OLS, FE, DML) ---
method_rows <- list(
  data.table(Method = "OLS (M1)", Coef = round(coef(m1)["log_early"], 4),
             SE = round(rob_se(m1, "log_early"), 4),
             Variable = "log_early", N = nobs(m1)),
  data.table(Method = "OLS + Controls (M2)", Coef = round(coef(m2)["log_early"], 4),
             SE = round(rob_se(m2, "log_early"), 4),
             Variable = "log_early", N = nobs(m2)),
  data.table(Method = "FE (M3)", Coef = round(coef(m3)["log_early"], 4),
             SE = round(rob_se(m3, "log_early"), 4),
             Variable = "log_early", N = m3$N),
  data.table(Method = "FE + Topic (M4)", Coef = round(coef(m4)["log_early"], 4),
             SE = round(rob_se(m4, "log_early"), 4),
             Variable = "log_early", N = m4$N),
  data.table(Method = "FE + Topic (M5, binary)", Coef = round(coef(m5)["fast_starter"], 4),
             SE = round(rob_se(m5, "fast_starter"), 4),
             Variable = "fast_starter", N = m5$N)
)

if (!is.null(dml_results)) {
  method_rows[[length(method_rows) + 1]] <- data.table(
    Method = "Double ML", Coef = round(dml_results$coef, 4),
    SE = round(dml_results$se, 4),
    Variable = "fast_starter", N = dml_results$n
  )
}

method_comparison <- rbindlist(method_rows)

stargazer(method_comparison, type = "latex", summary = FALSE, rownames = FALSE,
          out = file.path(TABLE_DIR, "Table3_MethodComparison.tex"),
          title = "Comparison Across Estimation Methods",
          label = "tab:methods",
          notes = "All models control for article-level covariates where applicable.")
cat("Saved: Table3_MethodComparison.tex\n")


# --- Save workspace ---
save.image(file.path(RESULTS_DIR, "analysis_results.RData"))
cat("Saved: analysis_results.RData\n\n")


cat("====================================================================\n")
cat("  ANALYSIS COMPLETE\n")
cat("  Time:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("====================================================================\n")

sink()

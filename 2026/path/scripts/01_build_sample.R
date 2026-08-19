# =============================================================================
# 01_build_sample.R -- data preparation and estimation sample
# -----------------------------------------------------------------------------
# Reads data/raw/Journals_2026_clean.csv, constructs all derived variables,
# merges the author-quality and prize side-files, and saves the analysis
# dataset used by every later script (results/analysis_data.rds).
#
# Also produces (new in this revision, in response to the referees):
#   - an explicit sample-attrition table (core journals 3,250 -> estimation 1,262)
#       -> output/tables/TableA1_Attrition.tex
#   - the topic-classification keyword dictionary
#       -> output/tables/TableA2_TopicDictionary.tex
#   - OpenAlex linkage diagnostics (match rate; correlation of OpenAlex and
#     Google Scholar citation counts)
#
# Citation data: the columns Google14..Google26 are cumulative Google Scholar
# citation counts collected by hand in February-March of each year 2014-2026.
# The count at paper age k is the count in the snapshot of calendar year
# (publication year + k).
# =============================================================================

# Locate and source the shared setup (works under Rscript and interactively).
local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

open_log("01_build_sample")

# =============================================================================
# 1. Load raw data
# =============================================================================

jn <- fread(file.path(DATA_RAW, "Journals_2026_clean.csv"))

cat("Loaded clean data:", nrow(jn), "papers\n")
cat("Journals:", paste(sort(unique(jn$Journal)), collapse = ", "), "\n")
cat("Year range:", min(jn$Year), "-", max(jn$Year), "\n\n")

n_corpus <- nrow(jn)

# --- 1.1 Rename columns -------------------------------------------------------
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

# --- 1.2 Citation trajectories --------------------------------------------------
# cite_age_k = cumulative citations in the snapshot of year (publication + k).
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

for (a in c(1, 2, 3, 5, 8)) {
  jn[, paste0("cite_age_", a) := get_cite_at_age(year, a, g14, g15, g16, g17,
       g18, g19, g20, g21, g22, g23, g24, g25, g26)]
}

# Early = age 2. Long-run = age 8 where observable, otherwise age 5.
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

# --- 1.3 Fast starters and other variables ---------------------------------------
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

# Fast starter = top quartile of age-2 citations within the journal-year cohort.
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

# Topic classification: keyword matching in article titles. A title is assigned
# to the topic whose keyword list it matches most often; titles matching no
# keyword are classified "other". The full dictionary is exported below
# (TableA2_TopicDictionary.tex) and reproduced in the paper's appendix.
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
  demography = c("population", "demographic", "fertility", "migration", "family",
                 "gender", "women", "woman", "marriage", "marital"),
  urban = c("urban", "city", "cities", "agglomeration"),
  war = c("war", "warfare", "military", "conflict", "revolution"),
  `financial markets` = c("stock", "equity", "bond", "securit", "share", "investor",
                        "insurance", "financial market", "capital market", "exchange"),
  growth = c("growth", "gdp", "development", "productivity", "divergence",
             "convergence", "national income", "national product", "per capita"),
  crises = c("depression", "crisis", "crises", "recession", "boom", "recovery",
             "business cycle", "panic", "bubble", "default", "deflation", "inflation"),
  health = c("health", "mortality", "disease", "epidemic", "pandemic", "nutrition",
             "height", "stature", "anthropometr", "numeracy", "heaping", "biological",
             "morbidity", "medical", "hospital", "sanitat")
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

# Top institutions: an indicator for whether any of the first five authors is
# affiliated with one of the following universities (substring match on the
# affiliation field). The list is reproduced in the paper's appendix.
# NOTE: "LSE" and "London School of Economics" are the SAME institution; both
# spellings are kept because some affiliation strings use only the abbreviation
# (e.g. "LSE - Economic History") and others only the full name. The list is
# therefore 18 match-strings for 17 distinct universities. Because any_top_inst
# is a 0/1 OR indicator, the two spellings cannot double-count a paper.
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

# Missing control values are imputed with the full-sample median.
for (v in c("log_article_length", "title_nchar", "article_position", "issue_no")) {
  jn[is.na(get(v)), (v) := median(jn[[v]], na.rm = TRUE)]
}

cat("Article-level controls created.\n\n")

# =============================================================================
# 2. Estimation sample, with explicit attrition accounting (Table A1)
# =============================================================================
# The filters below reproduce, one step at a time, the estimation-sample
# definition: core journals, observable age-2 AND age-5/8 citations (which
# restricts publication years to 2012-2021 given the 2014-2026 snapshots),
# non-missing author count, and non-negative citation counts.

step_rows <- list()
add_step <- function(label, d) {
  step_rows[[length(step_rows) + 1]] <<- data.table(
    step = label, papers = nrow(d),
    years = if (nrow(d)) paste0(min(d$year), "-", max(d$year)) else "")
}

s1 <- jn[is_core == TRUE]
add_step("Four core journals (JEH, EHR, EEH, EREH), 1997-2025", s1)
s2 <- s1[has_early == TRUE]
add_step("Age-2 citations observable (published 2012 or later)", s2)
s3 <- s2[has_longrun == TRUE]
add_step("Age-5 or age-8 citations observable (published 2021 or earlier)", s3)
s4 <- s3[!is.na(n_authors)]
add_step("Author count non-missing", s4)
s5 <- s4[cite_early >= 0 & cite_longrun >= 0]
add_step("Citation counts non-negative", s5)

attrition <- rbindlist(step_rows)
attrition[, dropped := shift(papers) - papers]
attrition[1, dropped := NA_integer_]
cat("Sample attrition:\n"); print(attrition); cat("\n")

# LaTeX export (booktabs)
att_tex <- c(
  "\\begin{tabular}{lrrl}",
  "\\toprule",
  "Step & Papers & Dropped & Publication years \\\\",
  "\\midrule",
  attrition[, sprintf("%s & %s & %s & %s \\\\",
    step, format(papers, big.mark = ","),
    ifelse(is.na(dropped), "--", format(dropped, big.mark = ",")), years)],
  "\\bottomrule",
  "\\end{tabular}")
writeLines(att_tex, file.path(TAB_DIR, "TableA1_Attrition.tex"))
cat("Saved: TableA1_Attrition.tex\n\n")

est <- s5
cat("Estimation sample:", nrow(est), "papers\n")
cat("Years:", min(est$year), "-", max(est$year), "\n")
cat("Journals:", paste(unique(est$journal), collapse = ", "), "\n\n")

cat("Citation statistics:\n")
cat("  Early (age 2): Mean =", round(mean(est$cite_early), 1),
    ", Median =", median(est$cite_early), ", SD =", round(sd(est$cite_early), 1), "\n")
cat("  Long-run: Mean =", round(mean(est$cite_longrun), 1),
    ", Median =", median(est$cite_longrun), ", SD =", round(sd(est$cite_longrun), 1), "\n\n")

# Log transforms. Google Scholar counts occasionally decline between snapshots
# (measurement noise), so post-age-2 growth is clamped at zero before the log.
est[, log_early := log(cite_early + 1)]
est[, log_longrun := log(cite_longrun + 1)]
est[, log_growth := log(pmax(cite_growth, 0) + 1)]

# =============================================================================
# 3. Extended controls: author quality (RePEc), prizes, working papers
# =============================================================================
# Used by the attenuation sequence and the position-design checks (script 08).
# RePEc-derived seniority and h-index cover roughly 58 percent of the
# estimation sample; coverage is reported in the log and in the paper.

drop_if_present <- c("topic_uniqueness", "cross_cite_ratio", "has_working_paper",
                     "wp_is_nber", "team_max_seniority", "team_max_hindex",
                     "author_nber_wp", "paper_won_prize", "author_won_dissertation_prize")
est[, (intersect(drop_if_present, names(est))) := NULL]

# (1) OpenAlex topic side-file. Not shipped with this package: it was collected
# for an earlier ID range and has no coverage on the 2012-2021 estimation
# sample, so these controls are set to NA/0 and are not used in the paper.
topic_file <- file.path(DATA_CACHE, "openalex_topic_data.rds")
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
  cat("  OpenAlex topic side-file not present -- extended topic controls set to NA/0",
      "(no coverage on the estimation sample; not used in the paper).\n")
}

# (2) RePEc: team seniority, team h-index, NBER working paper -------------------
repec_file <- file.path(DATA_CACHE, "repec_author_data.rds")
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

# (3) Prizes: paper-level (Cole/Ashton/Figuerola) + dissertation (author) --------
prize_paper_file <- file.path(DATA_CACHE, "prize_paper_data.rds")
if (file.exists(prize_paper_file)) {
  pp <- as.data.table(readRDS(prize_paper_file))
  est[, paper_won_prize := as.integer(id %in% unique(na.omit(pp$matched_id)))]
} else est[, paper_won_prize := 0L]

prize_diss_file <- file.path(DATA_CACHE, "prize_dissertation_data.rds")
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
# 4. OpenAlex linkage diagnostics (new in this revision)
# =============================================================================
# (a) Share of corpus papers matched to an OpenAlex record.
# (b) Agreement between OpenAlex and Google Scholar citation counts for
#     matched papers -- Pearson correlation on log(1+x) and Spearman rank
#     correlation. OpenAlex counts are as of the 2026 download; they are
#     compared with the 2025 Google Scholar snapshot (the last snapshot with
#     full corpus coverage).

oa_match_file <- file.path(DATA_CACHE, "openalex_paper_matches.rds")
oa_validation <- NULL
if (file.exists(oa_match_file)) {
  oam <- as.data.table(readRDS(oa_match_file))
  oam[, id := as.integer(id)]
  # Restrict cached matches to the corpus (the cache was built on a wider ID
  # range; the corpus is now the four core journals only).
  oam <- oam[id %in% jn$id]
  match_rate_all <- nrow(oam) / n_corpus
  match_rate_est <- mean(est$id %in% oam$id)

  val <- merge(jn[, .(id, g25)], oam[, .(id, oa_cited_by_count)], by = "id")
  val <- val[!is.na(g25) & !is.na(oa_cited_by_count)]
  pearson_log <- cor(log1p(val$g25), log1p(val$oa_cited_by_count))
  spearman    <- cor(val$g25, val$oa_cited_by_count, method = "spearman")

  cat("OpenAlex linkage diagnostics:\n")
  cat(sprintf("  Corpus papers matched to OpenAlex: %d / %d (%.1f%%)\n",
              nrow(oam), n_corpus, 100 * match_rate_all))
  cat(sprintf("  Estimation-sample papers matched:  %.1f%%\n", 100 * match_rate_est))
  cat(sprintf("  Google Scholar (2025) vs OpenAlex counts, N = %d:\n", nrow(val)))
  cat(sprintf("    Pearson correlation of log(1+counts): %.3f\n", pearson_log))
  cat(sprintf("    Spearman rank correlation:            %.3f\n\n", spearman))

  oa_validation <- list(n_matched = nrow(oam), n_corpus = n_corpus,
                        match_rate_all = match_rate_all,
                        match_rate_est = match_rate_est,
                        n_compared = nrow(val),
                        pearson_log = pearson_log, spearman = spearman)
} else {
  cat("OpenAlex match file not found -- linkage diagnostics skipped.\n\n")
}

# =============================================================================
# 5. Topic dictionary export (new in this revision)
# =============================================================================

topic_counts_est <- est[, .N, by = topic][order(-N)]
cat("Topic distribution in the estimation sample (including 'other'):\n")
print(topic_counts_est); cat("\n")

dict_dt <- data.table(topic = names(topic_dict),
                      keywords = vapply(topic_dict, paste, "", collapse = ", "))
dict_dt <- merge(dict_dt, topic_counts_est, by = "topic", all.x = TRUE)
setorder(dict_dt, -N)
dict_tex <- c(
  "\\begin{tabular}{lp{9cm}r}",
  "\\toprule",
  "Topic & Keywords (matched anywhere in the title) & Papers \\\\",
  "\\midrule",
  dict_dt[, sprintf("%s & %s & %d \\\\", topic, keywords, N)],
  sprintf("other & no keyword matched & %d \\\\", topic_counts_est[topic == "other", N]),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(dict_tex, file.path(TAB_DIR, "TableA2_TopicDictionary.tex"))
cat("Saved: TableA2_TopicDictionary.tex\n\n")

# =============================================================================
# 6. Save
# =============================================================================

analysis_data <- list(jn = jn, est = est,
                      topic_dict = topic_dict, top_inst = top_inst,
                      core_journals = core_journals,
                      attrition = attrition, traj_summary = traj_summary,
                      oa_validation = oa_validation)
saveRDS(analysis_data, file.path(RESULTS_DIR, "analysis_data.rds"))
cat("Saved: results/analysis_data.rds\n")

close_log()

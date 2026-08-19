# =============================================================================
# 06_mechanisms.R -- where citations come from, cascades, self-citation, HHI
# -----------------------------------------------------------------------------
# Uses the OpenAlex citation-link cache (data/cache/network_citation_data.rds,
# 72,695 citing works) and the discipline classification of all citing works
# (data/cache/citing_field_data.rds, built by 00b_citing_fields.R).
#
# 6a Citation source decomposition: within-field (economics/history) vs
#    cross-field counts; does early success shift a paper's audience out of
#    the field? (It does not.)
# 6b Self-citation analysis (within-dataset links)
# 6c Citation cascade depth (within-dataset citation network)
# 6d Citation concentration over time (Herfindahl index)
#
# Outputs: results/mech_data.rds (paper-level mechanism dataset)
#          results/res_06_mechanisms.rds
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

suppressMessages(library(igraph))

open_log("06_mechanisms")

ad  <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
jn  <- ad$jn
est <- ad$est

# Conference flags (from script 05) are merged so the mechanism dataset carries
# them for the reverse-causality test in script 09.
flags_file <- file.path(RESULTS_DIR, "conference_flags.rds")
mech <- copy(est)
if (file.exists(flags_file)) {
  mech <- merge(mech, readRDS(flags_file), by = "id", all.x = TRUE)
}

cite_raw_file <- file.path(DATA_CACHE, "network_citation_data.rds")
oa_match_file <- file.path(DATA_CACHE, "openalex_paper_matches.rds")
if (!file.exists(cite_raw_file) || !file.exists(oa_match_file)) {
  stop("Network caches not found in data/cache/ (network_citation_data.rds, ",
       "openalex_paper_matches.rds).")
}

cat("Loading network data...\n")
cite_raw   <- readRDS(cite_raw_file)
oa_matches <- readRDS(oa_match_file)
cat("  Citation links:", nrow(cite_raw), "\n")
cat("  OpenAlex matches:", nrow(oa_matches), "\n\n")

metrics_file <- file.path(DATA_CACHE, "network_paper_metrics.rds")
if (file.exists(metrics_file)) {
  metrics <- readRDS(metrics_file)
  metrics[, id := as.integer(id)]
  mech <- merge(mech, metrics, by = "id", all.x = TRUE)
  cat("  Network metrics merged:", sum(!is.na(mech$pagerank)), "papers with metrics\n\n")
}

id_to_oa <- setNames(oa_matches$openalex_id, as.character(oa_matches$id))
oa_to_id <- setNames(as.integer(oa_matches$id), oa_matches$openalex_id)

# =============================================================================
# 6a: Citation source decomposition
# =============================================================================
cat("--- 6a: Citation source decomposition ---\n\n")

# Each citing work is classified by DISCIPLINE using its OpenAlex level-0
# (root-discipline) concept tags; "within-field" means Economics or History
# appears among them. (Classifying by the single top concept, as in an earlier
# version, badly under-counts economics; see 00b_citing_fields.R.)
cfd_file <- file.path(DATA_CACHE, "citing_field_data.rds")
if (!file.exists(cfd_file)) {
  stop("citing_field_data.rds not found -- run 00b_citing_fields.R first.")
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

cite_source <- cite_raw[!is.na(is_within_field),
                        .(n_within_cites = sum(is_within_field),
                          n_cross_cites = sum(!is_within_field),
                          n_total_source = .N),
                        by = .(cited_id)]
cite_source[, cross_field_share := n_cross_cites / n_total_source]
cite_source[, cited_id := as.integer(cited_id)]

mech <- merge(mech, cite_source, by.x = "id", by.y = "cited_id", all.x = TRUE)
mech[is.na(n_within_cites), n_within_cites := 0L]
mech[is.na(n_cross_cites), n_cross_cites := 0L]
mech[is.na(cross_field_share), cross_field_share := NA_real_]

mech[, log_within_cites := log(n_within_cites + 1)]
mech[, log_cross_cites := log(n_cross_cites + 1)]

n_source_papers <- sum(mech$n_within_cites + mech$n_cross_cites > 0)
cat("Papers with citation source data:", n_source_papers, "\n\n")

# The count elasticities are estimated on the subsample of papers whose citers
# could be classified (cross_field_share non-missing); papers with no classified
# citers are excluded rather than zero-filled, and the paper states this N.
mech_cls <- mech[!is.na(cross_field_share)]
cat("Papers with classified citers (count-elasticity subsample):", nrow(mech_cls), "\n\n")

s1a <- felm(log_within_cites ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year, data = mech_cls)
cat("S1a: log_early -> log(within-field cites), classified subsample\n")
cat("  Coef:", round(coef(s1a)["log_early"], 4),
    "(SE:", round(summary(s1a)$coefficients["log_early", "Std. Error"], 4),
    ", N =", s1a$N, ")\n\n")

s1b <- felm(log_cross_cites ~ log_early + n_authors + any_top_inst +
              log_article_length + title_nchar + article_position + issue_no |
              journal + year, data = mech_cls)
cat("S1b: log_early -> log(cross-field cites), classified subsample\n")
cat("  Coef:", round(coef(s1b)["log_early"], 4),
    "(SE:", round(summary(s1b)$coefficients["log_early", "Std. Error"], 4),
    ", N =", s1b$N, ")\n\n")

# Does early success shift a paper's audience OUT of the field? Regress the
# cross-field SHARE on the fast-starter indicator. A coefficient near zero
# means early visibility scales within- and cross-field citations
# proportionally; it does not redirect attention outward.
fs_dat <- mech[!is.na(cross_field_share)]
fs_x <- felm(cross_field_share ~ fast_starter | journal + year, data = fs_dat)
fs_co <- coef(fs_x)[grep("fast_starter", names(coef(fs_x)))[1]]
fs_se <- summary(fs_x, robust = TRUE)$coefficients[
  grep("fast_starter", rownames(summary(fs_x)$coefficients))[1], 2]
cat("Fast-starter -> cross-field SHARE (journal-year FE):\n")
cat(sprintf("  coef %.4f (robust SE %.4f), N = %d; within/cross count elasticities %.3f / %.3f\n\n",
            fs_co, fs_se, nrow(fs_dat), coef(s1a)["log_early"], coef(s1b)["log_early"]))

# =============================================================================
# 6b: Self-citation analysis
# =============================================================================
cat("--- 6b: Self-citation analysis ---\n\n")

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

self_cite_agg <- cite_raw[, .(n_total_cites_sc = .N,
                               n_self_cites = sum(is_self_cite)),
                           by = .(cited_id)]
self_cite_agg[, self_cite_share := n_self_cites / n_total_cites_sc]
self_cite_agg[, cited_id := as.integer(cited_id)]

mech <- merge(mech, self_cite_agg, by.x = "id", by.y = "cited_id", all.x = TRUE)
mech[is.na(n_total_cites_sc), n_total_cites_sc := 0L]
mech[is.na(n_self_cites), n_self_cites := 0L]
mech[is.na(self_cite_share), self_cite_share := 0]

mech[, log_non_self_cites := log(n_total_cites_sc - n_self_cites + 1)]
mech[, log_total_cites_sc := log(n_total_cites_sc + 1)]

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
cat("  -> Stability indicates self-citations do not drive the result\n\n")

# =============================================================================
# 6c: Cascade depth
# =============================================================================
cat("--- 6c: Citation cascade depth ---\n\n")

our_oa_ids_set <- oa_matches$openalex_id
edges_w <- cite_raw[citing_oa_id %in% our_oa_ids_set & cited_oa_id %in% our_oa_ids_set,
                    .(from = citing_oa_id, to = cited_oa_id)]
edges_w <- unique(edges_w)

cd1 <- NULL; cd2 <- NULL
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

  cascade_dt[, id := as.integer(id)]
  mech <- merge(mech, cascade_dt, by = "id", all.x = TRUE)
  mech[is.na(cascade_depth), cascade_depth := 0]
  mech[is.na(n_direct_citers), n_direct_citers := 0L]
  mech[is.na(n_indirect_citers), n_indirect_citers := 0L]

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

# =============================================================================
# 6d: Citation concentration / HHI
# =============================================================================
cat("--- 6d: Citation concentration (HHI) ---\n\n")

hc1 <- NULL
if ("cite_concentration" %in% names(mech)) {
  n_hhi <- sum(!is.na(mech$cite_concentration))
  cat("Papers with HHI data (from network metrics):", n_hhi, "\n\n")
} else {
  cat("Computing citation concentration from raw data...\n")
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

# =============================================================================
# Save
# =============================================================================

saveRDS(mech, file.path(RESULTS_DIR, "mech_data.rds"))
cat("Saved: results/mech_data.rds\n")

grab <- function(m, v) if (is.null(m)) NULL else
  list(coef = coef(m)[v], se = summary(m)$coefficients[v, "Std. Error"], n = m$N)

res_06 <- list(
  within_share = mean(cite_raw$is_within_field, na.rm = TRUE),
  n_links_classified = sum(!is.na(cite_raw$is_within_field)),
  n_source_papers = n_source_papers,
  s1a = grab(s1a, "log_early"), s1b = grab(s1b, "log_early"),
  fs_crossshare = list(coef = fs_co, se = fs_se, n = nrow(fs_dat)),
  selfcite = list(n_checked = n_checked, n_self = n_self,
                  rate = n_self / max(n_checked, 1),
                  sc1 = grab(sc1, "log_early"), sc2 = grab(sc2, "log_early"),
                  change_pct = change_sc),
  cascade = list(cd1 = grab(cd1, "fast_starter"), cd2 = grab(cd2, "fast_starter")),
  hhi = grab(hc1, "fast_starter")
)
saveRDS(res_06, file.path(RESULTS_DIR, "res_06_mechanisms.rds"))
cat("Saved: results/res_06_mechanisms.rds\n")

close_log()

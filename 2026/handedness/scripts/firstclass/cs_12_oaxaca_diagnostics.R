# =============================================================================
# cs_12_oaxaca_diagnostics.R
# Why does the Oaxaca "explained share" differ so much across competitions
# (Tests 130% vs County 66% vs Sheffield 35%)?
# D1: left-hander quality premium + dispersion (the raw-gap driver)
# D2: pairwise decompositions (LR-RR, LR-LL, LL-RR), raw vs quality-adjusted gap
# D3: re-run the main decomposition with the P1 controls (max+min pre-match avg)
# D4: reconcile the Oaxaca "unexplained" with the P1 regression coefficient
# Run with:  Rscript cs_12_oaxaca_diagnostics.R   (NOT inline Rscript -e)
# =============================================================================

suppressMessages({
  library(tidyverse); library(fixest)
})
has_oaxaca <- requireNamespace("oaxaca", quietly = TRUE)

base_dir   <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), "..", ".."))
ana_dir    <- file.path(base_dir, "data", "analysis")
out_tables <- file.path(base_dir, "CS", "output", "tables")
out_note   <- file.path(base_dir, "CS", "output", "oaxaca_diagnostics.md")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)

samples <- c(tests = "Tests", county = "County", sheffield = "Sheffield")

load_analysis <- function(fmt) {
  read_csv(file.path(ana_dir, paste0("analysis_", fmt, ".csv")), show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(hand_combination = factor(hand_combination, levels = c("RR", "LR", "LL")),
           partnership_number_c = pmin(partnership_number, 10),
           is_mixed = as.integer(is_mixed_hand))
}
dl <- map(names(samples), load_analysis) %>% set_names(names(samples))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ---------------------------------------------------------------------------
# D1 — left-hander quality premium and dispersion
# ---------------------------------------------------------------------------
d1 <- imap_dfr(dl, function(df, fmt) {
  long <- bind_rows(
    transmute(df, hand = batter_1_hand, avg = pre_match_avg_1),
    transmute(df, hand = batter_2_hand, avg = pre_match_avg_2))
  lh <- mean(long$avg[long$hand == "left"],  na.rm = TRUE)
  rh <- mean(long$avg[long$hand == "right"], na.rm = TRUE)
  m  <- df %>% group_by(hand_combination) %>% summarise(r = mean(runs_scored), .groups = "drop")
  getm <- function(h) m$r[m$hand_combination == h] %||% NA_real_
  tibble(sample = samples[[fmt]],
         lh_pre_avg = lh, rh_pre_avg = rh, lh_minus_rh = lh - rh,
         sd_pre_avg = sd(long$avg, na.rm = TRUE),
         mean_RR = getm("RR"), mean_LR = getm("LR"), mean_LL = getm("LL"),
         LL_minus_RR = getm("LL") - getm("RR"),
         LR_minus_RR = getm("LR") - getm("RR"))
})

# ---------------------------------------------------------------------------
# Helper: regression-based twofold gap for a two-group contrast.
#   raw gap         = mean(runs | g=1) - mean(runs | g=0)
#   adjusted gap    = coef on g in OLS with quality controls (the "unexplained")
#   explained       = raw - adjusted
# controls_set: "avg" (avg_partnership_quality) or "maxmin" (max+min pre avg)
# ---------------------------------------------------------------------------
contrast_gap <- function(df, hi, lo, controls_set = "avg") {
  d <- df %>% filter(hand_combination %in% c(hi, lo)) %>%
    mutate(g = as.integer(hand_combination == hi))
  raw <- mean(d$runs_scored[d$g == 1]) - mean(d$runs_scored[d$g == 0])
  ctrl <- if (controls_set == "avg")
    "avg_partnership_quality" else "max_pre_match_avg + min_pre_match_avg"
  f <- as.formula(paste0("runs_scored ~ g + ", ctrl,
                         " + combined_experience + factor(partnership_number_c) + factor(innings) + wickets_at_start + runs_at_start"))
  adj <- coef(lm(f, data = d))[["g"]]
  tibble(raw_gap = raw, adjusted_gap = adj, explained = raw - adj,
         pct_explained = (raw - adj) / raw * 100, n = nrow(d))
}

# ---------------------------------------------------------------------------
# D2 — pairwise contrasts (raw vs quality-adjusted), avg control
# ---------------------------------------------------------------------------
d2 <- imap_dfr(dl, function(df, fmt) {
  bind_rows(
    contrast_gap(df, "LR", "RR") %>% mutate(contrast = "LR vs RR"),
    contrast_gap(df, "LR", "LL") %>% mutate(contrast = "LR vs LL"),
    contrast_gap(df, "LL", "RR") %>% mutate(contrast = "LL vs RR")
  ) %>% mutate(sample = samples[[fmt]], .before = 1)
})

# ---------------------------------------------------------------------------
# oaxaca() main decomposition (LR vs same-hand pooled), two control sets.
# Returns raw gap, endowment (explained), pct explained.
# ---------------------------------------------------------------------------
run_oax <- function(df, controls_set) {
  if (!has_oaxaca) return(tibble(raw_gap = NA, endow = NA, pct = NA))
  rhs <- if (controls_set == "avg")
    "avg_partnership_quality" else "max_pre_match_avg + min_pre_match_avg"
  f <- as.formula(paste0("runs_scored ~ ", rhs,
        " + combined_experience + partnership_number_c + innings + wickets_at_start + runs_at_start | is_mixed"))
  ob <- tryCatch(oaxaca::oaxaca(f, data = as.data.frame(df), R = 50),
                 error = function(e) NULL)
  if (is.null(ob)) return(tibble(raw_gap = NA, endow = NA, pct = NA))
  raw <- tryCatch(ob$y$y.diff, error = function(e) NA_real_)
  if (is.na(raw)) raw <- tryCatch(ob$y[["y.A"]] - ob$y[["y.B"]], error = function(e) NA_real_)
  tf <- ob$threefold$overall
  endow <- if (is.matrix(tf)) tf[1, 1] else tf[1]
  tibble(raw_gap = raw, endow = endow, pct = endow / raw * 100)
}

# ---------------------------------------------------------------------------
# D3 + D4 — P1 coefficient and reconciliation, both control sets
# ---------------------------------------------------------------------------
recon <- imap_dfr(dl, function(df, fmt) {
  # P1 preferred FE coefficient
  m5 <- feols(runs_scored ~ is_mixed_hand + avg_partnership_quality +
                max_pre_match_avg + min_pre_match_avg + combined_experience +
                factor(partnership_number_c) + wickets_at_start + runs_at_start |
                paste0(match_id, "_", innings), data = df, cluster = ~match_id)
  p1 <- coef(m5)[["is_mixed_hand"]]
  oax_avg    <- run_oax(df, "avg")
  oax_maxmin <- run_oax(df, "maxmin")
  tibble(sample = samples[[fmt]],
         raw_gap = oax_avg$raw_gap,
         oax_unexplained_avg    = oax_avg$raw_gap - oax_avg$endow,
         pct_explained_avg      = oax_avg$pct,
         oax_unexplained_maxmin = oax_maxmin$raw_gap - oax_maxmin$endow,
         pct_explained_maxmin   = oax_maxmin$pct,
         p1_mixed_coef = p1)
})

# ---------------------------------------------------------------------------
# Write CSVs + a readable note
# ---------------------------------------------------------------------------
write_csv(d1,    file.path(out_tables, "diag_d1_lh_premium.csv"))
write_csv(d2,    file.path(out_tables, "diag_d2_pairwise.csv"))
write_csv(recon, file.path(out_tables, "diag_d4_reconciliation.csv"))

fmtn <- function(x) formatC(x, format = "f", digits = 2)
note <- c(
  "# Oaxaca diagnostics: why the explained share differs across competitions",
  "",
  "## D1. Left-hander quality premium and dispersion",
  "Mean pre-match batting average of left- vs right-handers; SD of pre-match average;",
  "and the LL-RR raw run gap (the pure left-hander premium, since LL and RR differ only in hand).",
  "",
  paste(capture.output(print(as.data.frame(d1), digits = 4)), collapse = "\n"),
  "",
  "## D2. Pairwise contrasts: raw gap vs quality-adjusted gap (avg control)",
  "`explained` = raw - adjusted; `adjusted_gap` is the residual after quality controls.",
  "",
  paste(capture.output(print(as.data.frame(d2), digits = 3)), collapse = "\n"),
  "",
  "## D3/D4. Reconciliation: Oaxaca unexplained vs the P1 regression coefficient",
  "`oax_unexplained_*` should approximate `p1_mixed_coef`. The max+min control set",
  "(matching P1) should leave less unexplained than the single-average control set.",
  "",
  paste(capture.output(print(as.data.frame(recon), digits = 3)), collapse = "\n"),
  ""
)
writeLines(note, out_note)
cat("Wrote:", out_note, "\n")
cat("\n--- D1 ---\n"); print(as.data.frame(d1), digits = 4)
cat("\n--- D2 ---\n"); print(as.data.frame(d2), digits = 3)
cat("\n--- D4 reconciliation ---\n"); print(as.data.frame(recon), digits = 3)

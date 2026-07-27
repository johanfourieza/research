# =============================================================================
# 17_referee_revisions.R  —  JSE revise & resubmit (Referee Round 1)
#
# New analyses requested by the referee:
#   A. Openers vs non-openers subsamples (endogeneity: openers chosen in
#      advance; later hand combinations hard to predict ex ante)
#   B. Position splits with opening pair separated (1-2 / 3-6 / 7-11):
#      B1 T20I quantile regressions by position (replaces appendix B6)
#      B2 mean-effect (match x innings FE) position splits, all formats
#   C. Bowler style, ball level:
#      C1 LR x strike_changed x bowler_type (pace vs spin) + subsample splits
#      C2 LR x strike_changed x bowler_hand (left vs right arm) + splits
#      C3 four-class splits: pace-R, pace-L, spin-R, spin-L
#   D. Wides and no-balls: does a mixed-hand pair (or a strike change within
#      one) raise the probability of a wide/no-ball, especially for pace?
#   E. First-class corroboration: preferred P1 + P2 specs on County
#      Championship and Sheffield Shield data
#
# Headless: run as  Rscript scripts/17_referee_revisions.R
# (Do NOT use Rscript -e; it segfaults in this environment.)
# Output: scripts/output/referee_revisions/tables/*.csv
# =============================================================================

library(tidyverse)
library(data.table)
library(fixest)
library(quantreg)

# --- Headless path detection: scripts/<this>.R -> project root one level up ---
base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_dir      <- file.path(base_dir, "scripts", "output", "referee_revisions", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260721)

formats       <- c("tests", "odis", "t20is")
format_labels <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is",
                   county = "County Championship", sheffield = "Sheffield Shield")

# =============================================================================
# Loaders (mirror scripts/07 and scripts/10 sample filters exactly)
# =============================================================================

load_partnerships <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) { cat(sprintf("MISSING: %s\n", path)); return(NULL) }
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      partnership_number_f = factor(pmin(partnership_number, 10)),
      innings_f = factor(innings),
      match_innings_id = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings),
      # Referee split: opening pair (positions 1-2) / 3-6 / 7-11
      position_group2 = case_when(
        max_bat_pos <= 2  ~ "opening",
        max_bat_pos <= 6  ~ "middle",
        max_bat_pos <= 12 ~ "lower",
        TRUE              ~ NA_character_
      )
    )
  cat(sprintf("Loaded %s: %d partnerships\n", fmt, nrow(df)))
  df
}

load_balls <- function(fmt) {
  path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(path)) { cat(sprintf("MISSING: %s\n", path)); return(NULL) }
  dt <- fread(path)
  dt <- dt[!is.na(LR_at_crease)]
  # Mirrors script 10: drop deliveries with missing batter quality (debutants)
  dt <- dt[!is.na(striker_pre_avg) & !is.na(ns_pre_avg)]
  dt[, is_illegal := pmax(is_wide, is_noball)]
  cat(sprintf("Loaded ball-level %s: %d deliveries\n", fmt, nrow(dt)))
  dt
}

# Helper: one-row summary of a coefficient from a fixest model
grab <- function(model, coef_name, label, extra = list()) {
  if (is.null(model)) return(NULL)
  est <- coef(model)[coef_name]
  if (is.na(est)) return(NULL)
  bind_cols(
    tibble(label = label, term = coef_name,
           estimate = unname(est),
           se = unname(se(model)[coef_name]),
           p_value = unname(pvalue(model)[coef_name]),
           n = model$nobs),
    as_tibble(extra)
  )
}

# =============================================================================
# A. OPENERS VS NON-OPENERS
# =============================================================================
cat("\n========== A. OPENERS VS NON-OPENERS ==========\n")

res_a <- map_dfr(formats, function(fmt) {
  df <- load_partnerships(fmt)
  if (is.null(df)) return(NULL)

  # (i) Openers: partnership_number == 1. One opening partnership per innings,
  # so match x innings FE is infeasible; use match FE (replicates appendix B3).
  df_open <- df %>% filter(partnership_number == 1)
  m_open <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + combined_experience + innings_f |
      match_id,
    data = df_open, cluster = ~match_id
  )

  # (ii) Non-openers: full preferred specification (Table 3, FE columns).
  df_rest <- df %>% filter(partnership_number > 1)
  m_rest <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df_rest, cluster = ~match_id
  )

  bind_rows(
    grab(m_open, "is_mixed_hand", format_labels[fmt],
         list(subsample = "Openers (partnership 1)", fe = "match")),
    grab(m_rest, "is_mixed_hand", format_labels[fmt],
         list(subsample = "Non-openers (partnerships 2-10)", fe = "match x innings"))
  )
})
write_csv(res_a, file.path(out_dir, "tableA_openers_vs_rest.csv"))
cat("Saved: tableA_openers_vs_rest.csv\n")

# =============================================================================
# B. POSITION SPLITS WITH OPENING PAIR SEPARATED (1-2 / 3-6 / 7-11)
# =============================================================================
cat("\n========== B. POSITION SPLITS (1-2 / 3-6 / 7-11) ==========\n")

# --- B1. T20I quantile regressions by position (replaces appendix B6) ---
df_t20 <- load_partnerships("t20is")
pos_levels <- c("opening", "middle", "lower")

res_b1 <- map_dfr(pos_levels, function(pos) {
  df_sub <- df_t20 %>% filter(position_group2 == pos)
  if (nrow(df_sub) < 500) return(NULL)
  cat(sprintf("  T20I %s: %d partnerships\n", pos, nrow(df_sub)))
  map_dfr(c(0.25, 0.50, 0.75), function(tau) {
    m <- tryCatch(
      rq(runs_scored ~ is_mixed_hand +
           avg_partnership_quality + combined_experience +
           as.numeric(partnership_number_f) + as.numeric(innings_f) +
           wickets_at_start + runs_at_start,
         data = df_sub, tau = tau),
      error = function(e) NULL)
    if (is.null(m)) return(NULL)
    # Cluster bootstrap at the match level (500 reps); fall back to plain boot
    s <- tryCatch(
      summary(m, se = "boot", R = 500, cluster = df_sub$match_id),
      error = function(e) tryCatch(
        summary(m, se = "boot", R = 500), error = function(e2) summary(m)))
    ct <- s$coefficients
    idx <- which(rownames(ct) == "is_mixed_hand")
    if (length(idx) == 0) return(NULL)
    tibble(position = pos, tau = tau, n = nrow(df_sub),
           estimate = ct[idx, 1], se = ct[idx, 2],
           p_value = if (ncol(ct) >= 4) ct[idx, 4] else
             2 * pt(-abs(ct[idx, 1] / ct[idx, 2]), df = nrow(df_sub) - nrow(ct)))
  })
})
write_csv(res_b1, file.path(out_dir, "tableB1_t20i_qte_by_position12.csv"))
cat("Saved: tableB1_t20i_qte_by_position12.csv\n")

# --- B2. Mean-effect position splits, all formats (match x innings FE) ---
res_b2 <- map_dfr(formats, function(fmt) {
  df <- if (fmt == "t20is") df_t20 else load_partnerships(fmt)
  if (is.null(df)) return(NULL)
  map_dfr(pos_levels, function(pos) {
    df_sub <- df %>% filter(position_group2 == pos)
    if (nrow(df_sub) < 500) return(NULL)
    m <- tryCatch(feols(
      runs_scored ~ is_mixed_hand +
        avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
        combined_experience + partnership_number_f +
        wickets_at_start + runs_at_start |
        match_innings_id,
      data = df_sub, cluster = ~match_id), error = function(e) NULL)
    grab(m, "is_mixed_hand", format_labels[fmt], list(position = pos))
  })
})
write_csv(res_b2, file.path(out_dir, "tableB2_mean_by_position12.csv"))
cat("Saved: tableB2_mean_by_position12.csv\n")

rm(df_t20); gc()

# =============================================================================
# C + D. BALL-LEVEL: BOWLER STYLE AND WIDES/NO-BALLS
# =============================================================================
cat("\n========== C/D. BALL-LEVEL BOWLER STYLE & EXTRAS ==========\n")

ball_controls <- ~ striker_pre_avg + striker_pre_sr + ns_pre_avg +
  over + partnership_ball_number + cum_innings_wickets

res_c_triple <- list(); res_c_split <- list(); res_d <- list()

for (fmt in formats) {
  dt <- load_balls(fmt)
  if (is.null(dt)) next
  flab <- format_labels[fmt]

  # ---- C1. Pace vs spin ----
  dt_bt <- dt[!is.na(bowler_type) & bowler_type != ""]
  m_c1 <- feols(
    runs_batter ~ LR_at_crease * strike_changed * bowler_type +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt_bt, cluster = ~match_id + bowler)
  res_c_triple[[paste0(fmt, "_type")]] <- grab(
    m_c1, "LR_at_crease:strike_changed:bowler_typespin",
    flab, list(moderator = "bowler type (spin vs pace)"))

  # Subsample splits: LR x strike_changed within pace and within spin
  for (bt in c("pace", "spin")) {
    m_s <- feols(
      runs_batter ~ LR_at_crease * strike_changed +
        striker_pre_avg + striker_pre_sr + ns_pre_avg +
        over + partnership_ball_number + cum_innings_wickets |
        match_innings_id + bowler,
      data = dt_bt[bowler_type == bt], cluster = ~match_id + bowler)
    res_c_split[[paste0(fmt, "_", bt)]] <- grab(
      m_s, "LR_at_crease:strike_changed", flab, list(bowler_class = bt))
  }

  # ---- C2. Bowler hand (left vs right arm) ----
  dt_bh <- dt[!is.na(bowler_hand) & bowler_hand %in% c("left", "right")]
  m_c2 <- feols(
    runs_batter ~ LR_at_crease * strike_changed * bowler_hand +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt_bh, cluster = ~match_id + bowler)
  res_c_triple[[paste0(fmt, "_hand")]] <- grab(
    m_c2, "LR_at_crease:strike_changed:bowler_handright",
    flab, list(moderator = "bowler hand (right vs left arm)"))

  # ---- C3. Four-class splits: pace-R, pace-L, spin-R, spin-L ----
  dt_4 <- dt[!is.na(bowler_type) & bowler_type != "" &
             bowler_hand %in% c("left", "right")]
  for (bt in c("pace", "spin")) for (bh in c("right", "left")) {
    sub <- dt_4[bowler_type == bt & bowler_hand == bh]
    if (nrow(sub) < 5000) next
    m_s <- tryCatch(feols(
      runs_batter ~ LR_at_crease * strike_changed +
        striker_pre_avg + striker_pre_sr + ns_pre_avg +
        over + partnership_ball_number + cum_innings_wickets |
        match_innings_id + bowler,
      data = sub, cluster = ~match_id + bowler), error = function(e) NULL)
    res_c_split[[paste0(fmt, "_", bt, "_", bh)]] <- grab(
      m_s, "LR_at_crease:strike_changed", flab,
      list(bowler_class = paste0(bt, "-", bh, "-arm")))
  }

  # ---- D. Wides and no-balls ----
  # Sample includes illegal deliveries (never filtered out above).
  for (dv in c("is_wide", "is_noball", "is_illegal")) {
    for (bt in c("pace", "spin")) {
      sub <- dt_bt[bowler_type == bt]
      f <- as.formula(paste0(
        dv, " ~ LR_at_crease * strike_changed + striker_pre_avg + ",
        "striker_pre_sr + ns_pre_avg + over + partnership_ball_number + ",
        "cum_innings_wickets | match_innings_id + bowler"))
      m_d <- tryCatch(feols(f, data = sub, cluster = ~match_id + bowler),
                      error = function(e) NULL)
      base_rate <- mean(sub[[dv]], na.rm = TRUE)
      res_d[[paste0(fmt, "_", dv, "_", bt)]] <- bind_rows(
        grab(m_d, "LR_at_crease", flab,
             list(dv = dv, bowler_class = bt, coef_of = "LR at crease",
                  base_rate = base_rate)),
        grab(m_d, "LR_at_crease:strike_changed", flab,
             list(dv = dv, bowler_class = bt, coef_of = "LR x strike changed",
                  base_rate = base_rate)))
    }
  }

  rm(dt, dt_bt, dt_bh, dt_4); gc()
}

write_csv(bind_rows(res_c_triple), file.path(out_dir, "tableC1_bowler_triples.csv"))
write_csv(bind_rows(res_c_split),  file.path(out_dir, "tableC2_bowler_class_splits.csv"))
write_csv(bind_rows(res_d),        file.path(out_dir, "tableD_wides_noballs.csv"))
cat("Saved: tableC1_bowler_triples.csv, tableC2_bowler_class_splits.csv, tableD_wides_noballs.csv\n")

# =============================================================================
# E. FIRST-CLASS CORROBORATION (County Championship + Sheffield Shield)
# =============================================================================
cat("\n========== E. FIRST-CLASS CORROBORATION ==========\n")

res_e <- map_dfr(c("county", "sheffield"), function(fmt) {
  df <- load_partnerships(fmt)
  if (is.null(df)) return(NULL)

  # P1: preferred partnership-level specification
  m_p1 <- feols(
    runs_scored ~ is_mixed_hand +
      avg_partnership_quality + max_pre_match_avg + min_pre_match_avg +
      combined_experience + partnership_number_f +
      wickets_at_start + runs_at_start |
      match_innings_id,
    data = df, cluster = ~match_id)

  # P2: ball-level mechanism specification
  dt <- load_balls(fmt)
  m_p2 <- feols(
    runs_batter ~ LR_at_crease * strike_changed +
      striker_pre_avg + striker_pre_sr + ns_pre_avg +
      over + partnership_ball_number + cum_innings_wickets |
      match_innings_id + bowler,
    data = dt, cluster = ~match_id + bowler)

  out <- bind_rows(
    grab(m_p1, "is_mixed_hand", format_labels[fmt], list(test = "P1 partnership")),
    grab(m_p2, "LR_at_crease:strike_changed", format_labels[fmt],
         list(test = "P2 ball-level interaction")),
    grab(m_p2, "strike_changed", format_labels[fmt],
         list(test = "P2 strike-changed main effect")))
  rm(dt); gc()
  out
})
write_csv(res_e, file.path(out_dir, "tableE_firstclass.csv"))
cat("Saved: tableE_firstclass.csv\n")

cat("\nAll referee-revision analyses complete.\n")

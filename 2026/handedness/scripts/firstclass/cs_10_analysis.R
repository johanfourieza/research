# =============================================================================
# cs_10_analysis.R  —  Cricket Statistician paper
# Longer-form only: Tests (international) + County Championship + Sheffield Shield.
# Replicates the two core tests from the JSE paper:
#   P1  partnership-level: runs ~ mixed-hand + quality + ... | match x innings FE
#   P2  ball-level switching cost: runs ~ LR + strike_changed + LR:strike_changed
# Plus descriptives and an Oaxaca-Blinder quality decomposition.
# Writes tables (CSV) to CS/output/tables and figures (PNG 300dpi) to CS/output/figures.
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)

# --- Headless path detection: CS/scripts/<this>.R -> project root two levels up ---
base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), "..", ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_tables   <- file.path(base_dir, "CS", "output", "tables")
out_figs     <- file.path(base_dir, "CS", "output", "figures")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figs,   recursive = TRUE, showWarnings = FALSE)

# Three longer-form samples
samples <- c(tests = "Tests (international)",
             county = "County Championship",
             sheffield = "Sheffield Shield")

# Clean theme for figures
theme_cs <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")
hand_cols <- c(RR = "#4C72B0", LR = "#DD8452", LL = "#55A868")

# =============================================================================
# LOAD (mirrors scripts/07_regressions.R load filter)
# =============================================================================
load_analysis <- function(fmt) {
  path <- file.path(analysis_dir, paste0("analysis_", fmt, ".csv"))
  if (!file.exists(path)) { cat(sprintf("MISSING: %s\n", path)); return(NULL) }
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(
      hand_combination     = factor(hand_combination, levels = c("RR", "LR", "LL")),
      partnership_number_f = factor(pmin(partnership_number, 10)),
      match_innings_id     = if ("match_innings_id" %in% names(.))
        match_innings_id else paste0(match_id, "_", innings)
    )
  cat(sprintf("Loaded %-10s: %6d partnerships\n", fmt, nrow(df)))
  df
}
data_list <- map(names(samples), load_analysis) %>% set_names(names(samples))
data_list <- compact(data_list)

# =============================================================================
# 1. DESCRIPTIVES
# =============================================================================
describe_one <- function(df, fmt) {
  # left-hander prevalence: distinct batsmen across both seats
  bats <- bind_rows(
    df %>% transmute(batter = batter_1, hand = batter_1_hand),
    df %>% transmute(batter = batter_2, hand = batter_2_hand)
  ) %>% distinct(batter, .keep_all = TRUE)
  comp <- df %>% count(hand_combination) %>% mutate(p = n / sum(n))
  getp <- function(h) { v <- comp$p[comp$hand_combination == h]; if (length(v)) v else 0 }
  means <- df %>% group_by(hand_combination) %>% summarise(m = mean(runs_scored), .groups = "drop")
  getm <- function(h) { v <- means$m[means$hand_combination == h]; if (length(v)) v else NA_real_ }
  tibble(
    sample        = samples[[fmt]],
    n_matches     = n_distinct(df$match_id),
    n_partnerships = nrow(df),
    pct_left_batsmen = mean(bats$hand == "left", na.rm = TRUE) * 100,
    pct_RR = getp("RR") * 100, pct_LR = getp("LR") * 100, pct_LL = getp("LL") * 100,
    mean_runs_RR = getm("RR"), mean_runs_LR = getm("LR"), mean_runs_LL = getm("LL"),
    mean_balls   = mean(df$balls_faced, na.rm = TRUE)
  )
}
desc <- imap_dfr(data_list, ~describe_one(.x, .y))
write_csv(desc, file.path(out_tables, "table1_descriptives.csv"))
cat("\n=== Table 1: Descriptives ===\n"); print(as.data.frame(desc))

# Figure 1: mean runs by hand combination, faceted by sample
fig1_df <- imap_dfr(data_list, function(df, fmt) {
  df %>% group_by(hand_combination) %>%
    summarise(mean_runs = mean(runs_scored),
              se = sd(runs_scored) / sqrt(n()), .groups = "drop") %>%
    mutate(sample = factor(samples[[fmt]], levels = unname(samples)))
})
p1 <- ggplot(fig1_df, aes(hand_combination, mean_runs, fill = hand_combination)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = mean_runs - 1.96*se, ymax = mean_runs + 1.96*se), width = 0.2) +
  facet_wrap(~sample, scales = "free_y") +
  scale_fill_manual(values = hand_cols, guide = "none") +
  labs(x = "Partnership hand combination", y = "Mean partnership runs",
       title = "Raw mean partnership runs by hand combination") +
  theme_cs
ggsave(file.path(out_figs, "fig1_mean_runs.png"), p1, width = 9, height = 4, dpi = 300)

# =============================================================================
# 2. P1 — partnership-level regressions (preferred match x innings FE)
# =============================================================================
run_p1 <- function(df, fmt) {
  # m5: preferred FE spec (mixed-hand indicator)
  m5 <- feols(runs_scored ~ is_mixed_hand + avg_partnership_quality +
                max_pre_match_avg + min_pre_match_avg + combined_experience +
                partnership_number_f + wickets_at_start + runs_at_start |
                match_innings_id, data = df, cluster = ~match_id)
  # m6: decomposed (RR base; LR and LL dummies)
  m6 <- feols(runs_scored ~ hand_combination + avg_partnership_quality +
                max_pre_match_avg + min_pre_match_avg + combined_experience +
                partnership_number_f + wickets_at_start + runs_at_start |
                match_innings_id, data = df, cluster = ~match_id)
  cf <- coef(m5); se <- se(m5); pv <- pvalue(m5)
  cf6 <- coef(m6); se6 <- se(m6); pv6 <- pvalue(m6)
  tibble(
    sample = samples[[fmt]], n = nobs(m5), mean_runs = mean(df$runs_scored),
    mixed_b = cf["is_mixed_hand"], mixed_se = se["is_mixed_hand"], mixed_p = pv["is_mixed_hand"],
    LR_b = cf6["hand_combinationLR"], LR_se = se6["hand_combinationLR"], LR_p = pv6["hand_combinationLR"],
    LL_b = cf6["hand_combinationLL"], LL_se = se6["hand_combinationLL"], LL_p = pv6["hand_combinationLL"]
  )
}
p1_tab <- imap_dfr(data_list, ~run_p1(.x, .y))
write_csv(p1_tab, file.path(out_tables, "table2_p1_regressions.csv"))
cat("\n=== Table 2: P1 (match x innings FE) ===\n"); print(as.data.frame(p1_tab))

# Figure 2: coefficient plot of mixed-hand effect across samples
fig2_df <- p1_tab %>%
  mutate(sample = factor(sample, levels = unname(samples)),
         lo = mixed_b - 1.96*mixed_se, hi = mixed_b + 1.96*mixed_se)
p2 <- ggplot(fig2_df, aes(mixed_b, fct_rev(sample))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#DD8452", linewidth = 0.9, size = 0.7) +
  labs(x = "Effect of mixed-hand (LR) composition on partnership runs",
       y = NULL, title = "P1: the mixed-hand premium is a precise zero in every longer-form sample") +
  theme_cs
ggsave(file.path(out_figs, "fig2_p1_coef.png"), p2, width = 9, height = 3.5, dpi = 300)

# =============================================================================
# 3. OAXACA-BLINDER decomposition (LR vs same-hand)
# =============================================================================
run_oaxaca <- function(df, fmt) {
  if (!requireNamespace("oaxaca", quietly = TRUE)) {
    cat("  oaxaca package missing — skipping\n"); return(NULL) }
  df_ob <- df %>%
    filter(!is.na(is_mixed_hand), !is.na(avg_partnership_quality)) %>%
    mutate(partnership_number_c = pmin(partnership_number, 10),
           is_mixed = as.integer(is_mixed_hand)) %>% as.data.frame()
  ob <- tryCatch(oaxaca::oaxaca(
    runs_scored ~ avg_partnership_quality + combined_experience +
      partnership_number_c + innings + wickets_at_start + runs_at_start | is_mixed,
    data = df_ob, R = 100), error = function(e) { cat(sprintf("  oaxaca failed: %s\n", e$message)); NULL })
  if (is.null(ob)) return(NULL)
  raw_gap <- tryCatch(ob$y$y.diff, error = function(e) NA_real_)
  if (is.na(raw_gap)) raw_gap <- tryCatch(ob$y[["y.A"]] - ob$y[["y.B"]], error = function(e) NA_real_)
  tf <- ob$threefold$overall
  endow <- if (is.matrix(tf)) tf[1,1] else tf[1]
  tibble(sample = samples[[fmt]], raw_gap = raw_gap,
         endowments = endow, pct_explained = endow / raw_gap * 100)
}
oax_tab <- imap_dfr(data_list, ~run_oaxaca(.x, .y))
if (nrow(oax_tab)) {
  write_csv(oax_tab, file.path(out_tables, "table3_oaxaca.csv"))
  cat("\n=== Table 3: Oaxaca-Blinder ===\n"); print(as.data.frame(oax_tab))
}

# =============================================================================
# 4. P2 — ball-level switching-cost mechanism (preferred: M x I + bowler FE)
# =============================================================================
run_p2 <- function(fmt) {
  path <- file.path(analysis_dir, paste0("ball_level_", fmt, ".csv"))
  if (!file.exists(path)) { cat(sprintf("  ball-level missing: %s\n", path)); return(NULL) }
  cat(sprintf("  P2 loading %s ...\n", fmt))
  dt <- fread(path)
  dt <- dt[!is.na(LR_at_crease) & !is.na(striker_pre_avg) & !is.na(ns_pre_avg)]
  m1c <- feols(runs_batter ~ LR_at_crease + strike_changed +
                 LR_at_crease:strike_changed +
                 striker_pre_avg + striker_pre_sr + ns_pre_avg +
                 over + partnership_ball_number + cum_innings_wickets |
                 match_innings_id + bowler,
               data = dt, cluster = ~match_id + bowler)
  cf <- coef(m1c); se <- se(m1c); pv <- pvalue(m1c)
  ix <- "LR_at_crease:strike_changed"
  tibble(
    sample = samples[[fmt]], n_deliveries = nobs(m1c),
    LR_b = cf["LR_at_crease"], LR_se = se["LR_at_crease"], LR_p = pv["LR_at_crease"],
    strike_b = cf["strike_changed"], strike_se = se["strike_changed"], strike_p = pv["strike_changed"],
    interact_b = cf[ix], interact_se = se[ix], interact_p = pv[ix]
  )
}
p2_tab <- map_dfr(names(data_list), run_p2)
write_csv(p2_tab, file.path(out_tables, "table4_p2_mechanism.csv"))
cat("\n=== Table 4: P2 ball-level mechanism ===\n"); print(as.data.frame(p2_tab))

# Figure 3: the LR x strike-changed interaction across samples
fig3_df <- p2_tab %>% mutate(sample = factor(sample, levels = unname(samples)),
                             lo = interact_b - 1.96*interact_se, hi = interact_b + 1.96*interact_se)
p3 <- ggplot(fig3_df, aes(interact_b, fct_rev(sample))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), colour = "#4C72B0", linewidth = 0.9, size = 0.7) +
  labs(x = "LR × strike-changed interaction (runs per delivery)",
       y = NULL, title = "P2: no hand-specific switching cost in any longer-form sample") +
  theme_cs
ggsave(file.path(out_figs, "fig3_p2_mechanism.png"), p3, width = 9, height = 3.5, dpi = 300)

cat("\nDONE. Tables in CS/output/tables, figures in CS/output/figures.\n")

# =============================================================================
# 17b_opening_quantiles.R  —  JSE revise & resubmit (Referee Round 1)
#
# Companion to 17_referee_revisions.R: T20I quantile regressions for the
# OPENING pair (positions 1-2). The generic specification in script 17 fails
# for this group because partnership number, wickets at start and runs at
# start are all constant for openers (singular design). Here those constants
# are dropped; controls are the pair's quality and experience plus innings.
#
# Run as: Rscript scripts/17b_opening_quantiles.R
# Output: appends rows to tableB1 logic -> tableB1_opening.csv
# =============================================================================

library(tidyverse)
library(quantreg)

base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_dir      <- file.path(base_dir, "scripts", "output", "referee_revisions", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260721)

df <- read_csv(file.path(analysis_dir, "analysis_t20is.csv"),
               show_col_types = FALSE) %>%
  filter(hand_known == 1, both_avg_known == 1, either_debutant == 0,
         max_bat_pos <= 2) %>%
  mutate(innings_num = as.numeric(innings))

cat(sprintf("T20I opening partnerships (positions 1-2): %d\n", nrow(df)))

res <- map_dfr(c(0.25, 0.50, 0.75), function(tau) {
  m <- rq(runs_scored ~ is_mixed_hand +
            avg_partnership_quality + combined_experience + innings_num,
          data = df, tau = tau)
  s <- tryCatch(
    summary(m, se = "boot", R = 500, cluster = df$match_id),
    error = function(e) tryCatch(
      summary(m, se = "boot", R = 500), error = function(e2) summary(m)))
  ct <- s$coefficients
  idx <- which(rownames(ct) == "is_mixed_hand")
  tibble(position = "opening", tau = tau, n = nrow(df),
         estimate = ct[idx, 1], se = ct[idx, 2],
         p_value = if (ncol(ct) >= 4) ct[idx, 4] else
           2 * pt(-abs(ct[idx, 1] / ct[idx, 2]), df = nrow(df) - nrow(ct)))
})

print(as.data.frame(res))
write_csv(res, file.path(out_dir, "tableB1_opening.csv"))
cat("Saved: tableB1_opening.csv\n")

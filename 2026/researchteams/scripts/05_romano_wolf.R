# Romano-Wolf step-down p-values for the 20 registered tests
# (two indicators x ten outcomes). Optional; not run by run_all.R.
#
# Reads   data/papers.csv, data/researcher_links.csv, data/registered_tests.csv
# Writes  results/bootstrap_t/<outcome>.csv   bootstrap t-statistics
#         results/romano_wolf.csv             adjusted p-values
#
# Run     Rscript scripts/05_romano_wolf.R          1,999 draws, as in the letter
#         Rscript scripts/05_romano_wolf.R 199      a shorter trial
#
# With 1,999 draws the script compares its p-values with those stored in
# data/registered_tests.csv, which are the values reported in the letter, and
# stops if any differs.
#
# The bootstrap for each outcome runs in its own R process
# (05a_bootstrap_outcome.R). The draws are fixed by a seed, so a process that
# fails can be run again and gives the same numbers.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args  <- commandArgs(trailingOnly = TRUE)
draws <- if (length(args)) as.integer(args[1]) else 1999L
stopifnot(draws >= 1, draws <= 1999)

stored   <- read_csv("data/registered_tests.csv", show_col_types = FALSE)
outcomes <- unique(stored$outcome)        # registered order of the ten outcomes
rscript  <- file.path(R.home("bin"), "Rscript")

# ---- Bootstrap t-statistics, one process per outcome ---------------------------
for (y in outcomes) {
  file <- file.path("results/bootstrap_t", paste0(y, ".csv"))
  done <- function() file.exists(file) &&
    nrow(read_csv(file, show_col_types = FALSE)) == draws + 1
  attempt <- 0
  while (!done() && attempt < 3) {
    attempt <- attempt + 1
    system2(rscript, c("scripts/05a_bootstrap_outcome.R", y, draws))
  }
  if (!done()) stop("Bootstrap failed for outcome ", y)
  cat(sprintf("%-20s done\n", y))
}

# ---- Collect: one column per test, in registered order --------------------------
tests <- expand.grid(indicator = c("marker", "disclosed"), outcome = outcomes,
                     stringsAsFactors = FALSE)[, c("outcome", "indicator")]
observed <- numeric(nrow(tests))
t_star   <- matrix(NA_real_, draws, nrow(tests))
for (i in seq_len(nrow(tests))) {
  d <- read_csv(file.path("results/bootstrap_t", paste0(tests$outcome[i], ".csv")),
                show_col_types = FALSE)
  observed[i] <- d[[tests$indicator[i]]][d$draw == 0]
  t_star[, i] <- d[[tests$indicator[i]]][d$draw > 0]
}

# ---- Step-down adjustment --------------------------------------------------------
# Order the hypotheses from the largest observed |t| to the smallest. Hypothesis
# k is compared with the bootstrap distribution of the maximum |t| over
# hypotheses k, k+1, ..., 20, that is, over those not yet rejected.
ord   <- order(-abs(observed), seq_along(observed))
max_t <- abs(t_star[, ord, drop = FALSE])
for (k in (ncol(max_t) - 1):1) max_t[, k] <- pmax(max_t[, k], max_t[, k + 1])
exceed <- vapply(seq_along(ord), \(k) sum(max_t[, k] >= abs(observed[ord[k]])), numeric(1))
p_step <- (1 + exceed) / (draws + 1)
rw_p <- numeric(length(ord))
rw_p[ord] <- cummax(p_step)               # adjusted p-values cannot decrease

result <- tests |>
  mutate(t_observed = observed, rw_p = rw_p, bootstrap_draws = draws) |>
  left_join(select(stored, outcome, indicator, rw_p_stored = rw_p),
            by = c("outcome", "indicator"))
write_csv(result, "results/romano_wolf.csv")
print(as.data.frame(result))

if (draws == 1999L) {
  stopifnot(max(abs(result$rw_p - result$rw_p_stored)) < 1e-12)
  cat("All 20 Romano-Wolf p-values equal the stored values.\n")
} else {
  cat("Trial run: the comparison with the stored values needs 1,999 draws.\n")
}

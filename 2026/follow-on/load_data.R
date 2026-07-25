# =============================================================================
#  load_data.R  —  Load the data and reproduce the headline estimates.
#  Run this first. It is written to be read line by line.
# =============================================================================

library(readr)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  script_dir <- if (is.null(source_file)) getwd() else dirname(normalizePath(source_file))
}
d <- read_csv(file.path(script_dir, "followon_matches.csv"), show_col_types = FALSE)
cat("matches:", nrow(d), "| eligible:", sum(d$eligible),
    "| enforced:", sum(d$enforced), "\n")

# -----------------------------------------------------------------------------
#  STEP 1. Look at the rule working.
#  Enforcement should be near zero below the statutory margin and jump above it.
# -----------------------------------------------------------------------------
below <- d[d$dist >= -10 & d$dist < 0, ]
above <- d[d$dist >= 0 & d$dist <= 10, ]
cat("\nenforcement rate 10 runs below the line:", round(mean(below$enforced), 3), "\n")
cat(  "enforcement rate 10 runs above the line:", round(mean(above$enforced), 3), "\n")

# -----------------------------------------------------------------------------
#  STEP 2. The naive comparison, which is WRONG.
#  Captains choose whether to enforce, so this compares different situations.
# -----------------------------------------------------------------------------
el <- d[d$eligible == 1, ]
cat("\nNaive comparison among eligible matches:\n")
print(round(tapply(el$first_side_won, el$enforced, mean), 3))

# -----------------------------------------------------------------------------
#  STEP 3. The regression discontinuity estimate, which is the right one.
#  Eligibility is used as an instrument for enforcement.
# -----------------------------------------------------------------------------
library(rdrobust)

cat("\nFIRST STAGE: does crossing the line change behaviour?\n")
summary(rdrobust(y = d$enforced, x = d$dist, c = 0, masspoints = "adjust"))

cat("\nFUZZY RD: the effect of enforcing on winning\n")
summary(rdrobust(y = d$first_side_won, x = d$dist, c = 0,
                 fuzzy = d$enforced, masspoints = "adjust"))

cat("\nFUZZY RD: the effect of enforcing on LOSING\n")
summary(rdrobust(y = d$first_side_lost, x = d$dist, c = 0,
                 fuzzy = d$enforced, masspoints = "adjust"))

# -----------------------------------------------------------------------------
#  STEP 4. A check you should always run: is the running variable manipulated?
#  The side batting second bats to stay below the margin, so there is excess
#  mass just below zero. Compare the counts.
# -----------------------------------------------------------------------------
cat("\nmatches within 3 runs below the line:", sum(d$dist >= -3 & d$dist < 0), "\n")
cat(  "matches within 3 runs above the line:", sum(d$dist >= 0 & d$dist < 3), "\n")
bt <- binom.test(sum(d$dist >= 0 & d$dist < 3),
                 sum(d$dist >= -3 & d$dist < 3), 0.5)
cat("binomial test p-value:", format.pval(bt$p.value, digits = 3,
                                        eps = 0.0001), "\n")
cat("  Excess mass BELOW the line. The side batting second is batting to stay\n")
cat("  under the margin and avoid the follow-on. It is the side that does NOT\n")
cat("  make the decision we are studying.\n")

# -----------------------------------------------------------------------------
#  STEP 5. How much does that manipulation matter? Drop the affected matches.
# -----------------------------------------------------------------------------
dd <- d[abs(d$dist) > 2, ]
cat("\nExcluding matches within 2 runs of the cutoff:\n")
summary(rdrobust(y = dd$first_side_won, x = dd$dist, c = 0,
                 fuzzy = dd$enforced, masspoints = "adjust"))

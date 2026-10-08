# =============================================================================
#  run_all.R  -  Reproduce every table, figure and headline number in the
#  paper from the aggregate files in data/, then rebuild the person-level
#  results from data/microdata/ and check them against both.
#
#  Usage, from the package root:
#     Rscript scripts/run_all.R
#
#  Requires R 4.1 or later with readr, dplyr, tidyr, ggplot2 and scales.
#  Runs in a few seconds. Each step stops if a number differs from the paper.
# =============================================================================

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else file.path(getwd(), "scripts")

for (step in c("01_tables.R", "02_figures.R", "03_continuation.R", "04_microdata_checks.R")) {
  cat("\n==========", step, "==========\n")
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(file.path(SCRIPTS, step))))
  if (!identical(status, 0L)) stop("Step failed: ", step)
}
cat("\nDone. Tables are in output/tables and figures in output/figures.\n")

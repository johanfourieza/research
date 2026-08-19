# =============================================================================
# run_all.R -- run the full replication pipeline
# -----------------------------------------------------------------------------
# Usage (from the replication/ directory):
#   Rscript run_all.R
#
# Runs scripts/01_build_sample.R through scripts/10_figures.R, each in a fresh
# R session (so no state leaks between scripts), and writes sessionInfo to
# output/logs/sessionInfo.txt. Total runtime is roughly 30-60 minutes; the
# permutation tests (04, 05) and the decomposition bootstrap (08) dominate.
#
# Script 00 (external data collection) is NOT run: every analysis runs offline
# from the shipped caches in data/cache/. To re-collect from the APIs, set the
# environment variable RUN_COLLECTION=TRUE and provide the API credentials
# described in README.md.
# =============================================================================

args <- commandArgs(trailingOnly = FALSE)
m <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
setwd(ROOT)

rscript <- file.path(R.home("bin"), "Rscript")

scripts <- c(
  if (toupper(Sys.getenv("RUN_COLLECTION", "FALSE")) == "TRUE")
    c("00_data_collection.R", "00b_citing_fields.R"),
  "01_build_sample.R",
  "02_main_results.R",
  "03_robustness.R",
  "04_placebo.R",
  "05_conference.R",
  "06_mechanisms.R",
  "07_heterogeneity.R",
  "08_attenuation_luck.R",
  "09_within_author.R",
  "10_figures.R"
)

t0 <- Sys.time()
for (s in scripts) {
  cat("\n============================================================\n")
  cat("RUNNING:", s, "\n")
  cat("============================================================\n")
  status <- system2(rscript, args = shQuote(file.path("scripts", s)))
  if (status != 0) stop("Script failed: ", s, " (exit code ", status, ")")
}

# Record the environment of the successful run
dir.create(file.path(ROOT, "output", "logs"), recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()),
           file.path(ROOT, "output", "logs", "sessionInfo.txt"))

cat("\n============================================================\n")
cat("PIPELINE COMPLETE in",
    round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
cat("Tables in output/tables/, figures in output/figures/, logs in output/logs/\n")
cat("============================================================\n")

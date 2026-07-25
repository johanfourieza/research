# =============================================================================
#  run_all.R  -  Run the whole analysis from the raw data to the exhibits.
#
#  Usage, from the project root:
#     Rscript scripts/run_all.R
#
#  The analysis is Windows-based because the licensed source database is stored
#  at a Windows path.  PowerShell launches each stage as a direct, clean R
#  process.  A nested Rscript process crashes in the permutation-heavy
#  mechanisms stage on this system; delegating process control to PowerShell
#  avoids that native Windows failure.  The PowerShell runner writes exit status
#  and elapsed time for every stage to output/logs/run_manifest.csv and stops
#  immediately if a stage fails.
# =============================================================================

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
runner <- normalizePath(file.path(SCRIPTS, "run_all.ps1"), winslash = "/",
                        mustWork = TRUE)

if (.Platform$OS.type != "windows") {
  stop("This project runner currently requires Windows PowerShell.")
}

status <- system2(
  "powershell.exe",
  c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(runner))
)
if (!identical(status, 0L)) quit(status = as.integer(status))

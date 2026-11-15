# Reproduce Table 1, Figures 1 and 2, and the numbers in the text of
# "AI and entry-level knowledge work: Evidence from economics research assistants".
#
# Run from this folder:   Rscript run_all.R
# Runtime: under one minute.
#
# The Romano-Wolf bootstrap is separate because it is slow:
#   Rscript scripts/05_romano_wolf.R

# Each script runs in its own R process, so none depends on objects left in
# memory by another.
rscript <- file.path(R.home("bin"), "Rscript")
for (script in c("01_table1.R", "02_page_split.R", "03_figures.R",
                 "04_check_numbers.R")) {
  cat("\n==== ", script, " ====\n", sep = "")
  status <- system2(rscript, file.path("scripts", script))
  if (status != 0) stop(script, " failed")
}

# Record the R and package versions used.
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fixest); library(ggplot2)
})
writeLines(capture.output(sessionInfo()), "results/session_info.txt")
cat("\nAll scripts ran. Outputs are in results/.\n")

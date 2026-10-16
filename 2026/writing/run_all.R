# run_all.R -- reproduce Section 8 and Figure 1 of Fourie (2026), "Writing Is Not (Necessarily) Thinking".
# Usage, from this folder:  Rscript run_all.R
# Runs scripts/01-03, each in a fresh R session, then scripts/04_check_numbers.R, which stops with an
# error if any number reported in the paper is not reproduced. Writes results/session_info.txt.
# Requires R >= 4.3 with dplyr, tidyr, fixest, ggplot2, patchwork and svglite. Runtime: under a minute.

args <- commandArgs(trailingOnly = FALSE)
m <- grep('^--file=', args, value = TRUE)
ROOT <- if (length(m)) dirname(normalizePath(sub('^--file=', '', m[1]))) else getwd()
setwd(ROOT)
dir.create('results', showWarnings = FALSE)

rscript <- file.path(R.home('bin'), 'Rscript')
for (s in c('01_trends.R', '02_groups.R', '03_figure.R', '04_check_numbers.R')) {
  cat('\n== ', s, '\n')
  if (system2(rscript, file.path('scripts', s)) != 0) stop('Failed: ', s)
}

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(fixest); library(ggplot2); library(patchwork)})
writeLines(capture.output(sessionInfo()), 'results/session_info.txt')
cat('\nDone.\n')

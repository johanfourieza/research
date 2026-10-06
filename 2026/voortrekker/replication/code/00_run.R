# Runs the analysis pipeline once, with the linkage selected by VT_LINKAGE
# ("spouse", the default, or "blind"). To replicate the paper, run code/run_all.R
# from the replication folder instead; it runs both linkages and the couples analysis.
#
# Usage, from the replication folder:  Rscript code/00_run.R

script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  sp <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(sp) > 0) dirname(normalizePath(sp[1], winslash = "/")) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) getwd())
}, error = function(e) getwd())

source(file.path(script_dir, "pipeline.R"))

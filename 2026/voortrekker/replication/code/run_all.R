# Run the full replication from the replication root:  Rscript code/run_all.R
# 1. pipeline with the wife-blind linkage (VT_LINKAGE=blind), outputs moved to output_wife_blind/
# 2. pipeline with the final (spouse-assisted) linkage, outputs in output/
# 3. married-household analyses on both, outputs in output/couples_analysis/
# Each step runs in a fresh R process. Takes about 15 minutes.
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(linkage) {
  Sys.setenv(VT_LINKAGE = linkage)
  status <- system2(rscript, c("--encoding=UTF-8", "code/pipeline.R"))
  if (status != 0) stop("pipeline failed for VT_LINKAGE=", linkage)
}
stopifnot(file.exists("code/pipeline.R"))
run("blind")
unlink("output_wife_blind", recursive = TRUE)
stopifnot(file.rename("output", "output_wife_blind"))
dir.create("output")
run("spouse")
Sys.unsetenv("VT_LINKAGE")
stopifnot(system2(rscript, "code/couples_analysis.R") == 0)
cat("Replication complete: output/ (final linkage), output_wife_blind/, output/couples_analysis/\n")

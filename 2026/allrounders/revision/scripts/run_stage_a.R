# Reconstruct the local Stage A checkpoint. Licensed inputs must already be available.
# This runner never opens the data gate or runs substantive estimation.
# County register pages are downloaded only when their local cache is absent.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
steps<-list(c("03_stage_and_audit.R"),c("05_cricsheet_pilot.R"),c("06_county_register.R"),
 c("08_make_pilot_crosswalk.R"),c("09_apply_verified_corrections.R"),c("05_cricsheet_pilot.R","--corrected"),
 c("13_source_adjudication_diagnostics.R"),c("15_pilot_adjudications.R"),c("11_registry_identity_audit.R"),
 c("10_rebuild_components.R"),c("12_implementation_pilots.R"),c("14_field_dictionary.R"),
 c("16_identity_timing_queries.R"),c("17_execution_checkpoint.R"))
rscript<-file.path(R.home("bin"),"Rscript.exe")
if(!file.exists(rscript))rscript<-file.path(R.home("bin"),"Rscript")
for(i in seq_along(steps)) {
 step<-steps[[i]];script<-file.path(revision_dir,"scripts",step[1])
 log<-file.path(revision_dir,"audit",sprintf("stage_a_%02d_%s.log",i,sub("\\.R$","",step[1])))
 cat("Running",paste(step,collapse=" "),"\n")
 status<-system2(rscript,c(shQuote(script),step[-1]),stdout=log,stderr=log)
 if(status!=0L)stop("Stage A stopped at ",step[1],"; inspect ",log)
}
cat("Validation checkpoint reconstructed. Review execution_report.md and the issue register; the data gate remains CLOSED.\n")

# Explicit reconstruction order. No paid API or labelling calls.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'revision_common.R'))
full<-'--full-source-audit'%in%commandArgs(TRUE)
source_steps<-list(c('03_stage_and_audit.R'),c('05_cricsheet_pilot.R'),c('06_county_register.R'),c('08_make_pilot_crosswalk.R'),c('09_apply_verified_corrections.R'),c('05_cricsheet_pilot.R','--corrected'),c('13_source_adjudication_diagnostics.R'),c('15_pilot_adjudications.R'),c('11_registry_identity_audit.R'),c('10_rebuild_components.R'),c('14_field_dictionary.R'),c('16_identity_timing_queries.R'))
if(!full) {
 required<-c('data/staging/source_tests.rds','data/staging/source_county.rds','audit/execution/registry_collision_queries.csv','audit/execution/registry_split_queries.csv','audit/execution/cricsheet_pilot_adjudications.csv','audit/execution/same_day_national_identity_queries.csv')
 if(!all(file.exists(file.path(revision_dir,required))))stop('Source checkpoints missing. Run with --full-source-audit and provide the documented archives.')
 source_steps<-list(c('09_apply_verified_corrections.R'))
}
analysis_steps<-lapply(c('20_resolve_identities','10_rebuild_components','12_implementation_pilots','21_open_scoped_gate','22_construct_analysis','23_dual_output','24_promotion_audit','25_prospective_comparisons','26_mortality_overlay','27_promotion_analysis','28_additional_checks','30_whiteball_reference_checks','39_feedback_checks','40_share_by_role','31_make_exhibits','41_feedback_exhibits','36_test_output_map','33_transition_linkage_checks','35_supplement_exhibits','38_fable_requested_checks','32_final_verification'),function(s)paste0(s,'.R'))
rscript<-file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe'else'Rscript')
for(step in c(source_steps,analysis_steps)) {
 if(step[1]=='32_final_verification.R' && '--licensed-replication'%in%commandArgs(TRUE))step<-c(step,'--licensed-replication')
 cat('Running',step[1],'\n')
 log<-file.path(revision_dir,'audit',paste0('reproduction_',sub('[.]R$','',step[1]),'.log'))
 status<-system2(rscript,c(shQuote(file.path(revision_dir,'scripts',step[1])),step[-1]),stdout=log,stderr=log)
 if(status!=0)stop('Reconstruction failed: ',step[1],'; see ',log)
}
cat('Revision analyses and exhibits reproduced. Compile the LaTeX files in revision/paper next.\n')

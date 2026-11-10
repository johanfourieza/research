# Launch from the package root. All outputs remain inside this package.
stopifnot(file.exists('.here'), file.exists('replication/run_all.R'))
required <- c('tidyverse','here','KFAS','igraph','patchwork','scales','ragg','pdftools','digest','fixest')
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop('Install required R packages: ',paste(missing,collapse=', '))
here::i_am('replication/run_all.R')
Sys.setenv(OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',READR_NUM_THREADS='1',CAPE_VALIDATION_RUN='preferred1795')
args <- commandArgs(trailingOnly=TRUE)
valid <- c('--stage1-only','--stage2-only','--variants','--bridges','--validation','--simulation','--all-expensive')
if(any(!args %in% valid)) stop('Unknown flag. Available: ',paste(valid,collapse=' '))
if(all(c('--stage1-only','--stage2-only') %in% args)) stop('Choose one stage restriction.')
all_expensive <- '--all-expensive' %in% args
enabled <- function(x) all_expensive || x %in% args
work <- 'replication/work'
dir.create(file.path(work,'analysis','variants'),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(work,'generated_tables'),recursive=TRUE,showWarnings=FALSE)
# Frozen summaries seed missing outputs only; completed expensive runs take precedence.
for(f in list.files('replication/results',recursive=TRUE,full.names=TRUE)) {
 rel <- substring(f,nchar('replication/results/')+1)
 target <- file.path(work,'analysis',rel)
 dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(target)) stopifnot(file.copy(f,target))
}
for(name in c('inherited_monetary_assumptions_NOT_VERIFIED_PARITIES.csv',
 'inherited_basket_monetary_scenarios_NOT_HEADLINE.csv'))
 stopifnot(file.copy(file.path('data/inputs',name),file.path(work,'analysis',name),overwrite=TRUE))
timings <- list()
run <- function(script,arguments=character(),stage='2') {
 cat('\nRUN ',script,'\n',sep='');flush.console()
 start <- proc.time()[3]
 status <- system2(file.path(R.home('bin'),'Rscript'),c('replication/launch.R',script,shQuote(arguments)))
 timings[[length(timings)+1L]] <<- data.frame(stage,script,seconds=unname(proc.time()[3]-start),status)
 write.csv(do.call(rbind,timings),file.path(work,'run_times.csv'),row.names=FALSE,na='')
 if(status!=0L) stop(script,' failed (exit ',status,').')
}
if(!'--stage2-only' %in% args) {
 for(s in c('build_inputs','fit_primary','paper_annual_index','paper_source_connectivity')) run(s,stage='1')
 run('export_primary',stage='1')
}
if('--stage1-only' %in% args) quit(status=0)
if(!file.exists(file.path(work,'analysis','annual_state_model.rds'))) stop('Run stage 1 first.')
run('restore_joint')
if(enabled('--variants')) {
 run('channel_loading_map',stage='3 variants')
 run('run_variants',stage='3 variants');run('merge_variants',stage='3 variants')
 run('fit_constant',stage='3 variants');run('peak_diagnostics',stage='3 variants');run('peak_all_periods',stage='3 variants')
 run('variant_numerics',stage='3 variants');run('paper_model_numerics',stage='3 variants')
}
if(enabled('--bridges')) {
 run('channel_loading_map',stage='3 bridges');run('bridge_diagnostics',stage='3 bridges');run('offset_pass_through',stage='3 bridges')
}
if(enabled('--validation')) {
 run('prepare_validation',stage='3 validation')
 for(s in c('validation_run','validation_summarise','validation_verify','validation_design_diagnostics','validation_reporting_sidecar','validation_author_results_final')) run(s,stage='3 validation')
}
if(enabled('--simulation')) {
 run('channel_loading_map',stage='3 simulation')
 sim <- file.path(work,'analysis','simulation','revised_primary1795')
 addon <- paste0(sim,'_addon')
 if(!file.exists(file.path(sim,'design.rds'))) run('simulation_revised_setup',c(file.path(work,'analysis','annual_state_model.rds'),file.path(work,'analysis','channel_family_loading_map.csv')),stage='3 simulation')
 run('simulation_revised_validate',stage='3 simulation')
 batch <- format(Sys.time(),'%Y%m%d%H%M%S')
 run('simulation_revised_run',c('worker1',batch),stage='3 simulation')
 run('simulation_revised_summarize',stage='3 simulation')
 if(!file.exists(file.path(addon,'frozen_schedule.csv'))) run('simulation_addon_setup',stage='3 simulation')
 if(!file.exists(file.path(addon,'frozen_addon_code_manifest.csv'))) run('simulation_addon_freeze',stage='3 simulation')
 run('simulation_addon_run',c('worker1',batch),stage='3 simulation')
 for(s in c('simulation_addon_summarize','simulation_pair_mcse_sidecar','simulation_refine_flagged','simulation_refine_fourth_generated','simulation_final_disposition','simulation_anchor_diagnosis')) run(s,stage='3 simulation')
}
for(s in c('household_companion','enslaved_companion','paper_companion_indices','paper_asset_companions','paper_published_comparison','results_registry',
 'paper_tables','extended_tables','validation_tables','simulation_tables','export_data','conclusion_rates','describe_variants','text_numbers','appendix_tables','figures_v2')) run(s)
cat('\nComplete. Tables, figures, numerical claims and run times are in replication/work/.\n')

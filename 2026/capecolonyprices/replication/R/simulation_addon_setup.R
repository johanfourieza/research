source(here::here("replication","R","simulation_revised_common.R"))
suppressPackageStartupMessages(library(digest))
out<-here::here("replication","work","analysis","simulation","revised_primary1795_addon")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,'frozen_schedule.csv')))stop('Add-on already frozen; do not overwrite.')
schedule<-tibble(scenario='S4_fixed_reference',replicate=1:200,fit_variant='primary',job_id=1:200) |>
 mutate(key=sprintf('S4_fixed_reference_rep%03d_primary',replicate),generator_seed=sr_d$seed_base+replicate,posterior_seed=sr_d$posterior_seed_base+replicate)
paircheck<-map_dfr(1:200,function(rep) {
 clean<-sr_generate(rep,'S4_fixed_reference');wedge<-sr_generate(rep,'S1_fixed_source_wedge')
 jj<-match(clean$panel$model_source_id,sr_d$channels$model_source_id)
 tibble(replicate=rep,theta_max_difference=max(abs(clean$theta-wedge$theta)),
  quote_pair_error=max(abs(wedge$panel$log_price-sr_d$wedge*sr_d$g_table$g_all[jj]-clean$panel$log_price)),
  same_truth_index=identical(clean$index,wedge$index))
})
stopifnot(max(paircheck$theta_max_difference)==0,max(paircheck$quote_pair_error)<1e-12,all(paircheck$same_truth_index))
write_csv(schedule,file.path(out,'frozen_schedule.csv'));write_csv(paircheck,file.path(out,'paired_generator_checks.csv'))
write_lines(c('FROZEN ADD-ON: CLEAN FIXED-PATH CONTROL',as.character(Sys.time()),
 '200primary re-estimates using sr_generate(replicate,S4_fixed_reference), the frozen69-channel model and the same fixed posterior commodity-price path as S1-S3. Fresh source offsets and observation errors are exactly paired by replicate with those stresses. S1 minus its fixed g_all wedge equals the clean generated quote vector to floating-point precision for all200replicates.',
 'Same optimizer, seven fitted scales,1000conditional posterior draws, quantities, normalizations and metrics as the frozen experiment. Atomic separate checkpoint directory and separate named-batch claims. No completed original fit rerun.',
 'Report paired stress-minus-clean errors, RMSE and coverage within replicate. Their Monte Carlo standard errors use the paired differences, not independent-group formulas. Reference coverage remains conditional on the chosen fixed latent path, not prior calibration.',
 'The fixed path is wheat-heavy and peaks in1826. Its within-run movement is already conditioned on empirical flat official quotes. S3may therefore understate carry-forward damage on this path; this is not a formal lower bound.',
 'S0calibration conditions on the offset prior SD tau=.5 being true. Tau is not estimated. Peak calibration sidecar uses the post1795window only and does not validate the full1700-1834peak.',
 'The S0peak sidecar uses stored probabilities: event peak<1825 Brier score, reliability bins, and the log probability assigned to the true peak year. Zero empirical mass among1000draws is recorded explicitly, with empirical log score minus infinity. No pseudocount or artificial finite log score is supplied.'),file.path(out,'FROZEN_ADDON_PROTOCOL.txt'))
write_csv(tibble(input=c('base_design','base_code_manifest'),sha256=c(digest(file=file.path(sr_out,'design.rds'),algo='sha256'),digest(file=file.path(sr_out,'frozen_code_manifest.csv'),algo='sha256'))),file.path(out,'base_input_hashes.csv'))
cat('Paired clean-reference DGP verified for all200replicates; add-on schedule frozen.\n')

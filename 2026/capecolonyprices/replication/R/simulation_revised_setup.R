Sys.setenv(OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1',BLIS_NUM_THREADS='1')
suppressPackageStartupMessages({library(tidyverse);library(here);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2)
fit_file<-args[1];g_file<-args[2]
out<-here::here("replication","work","analysis","simulation","revised_primary1795")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,'design.rds')))stop('Frozen design exists: do not overwrite.')
source(here::here("replication","R","helpers.R"))
f<-readRDS(fit_file);g<-read_csv(g_file,show_col_types=FALSE)
stopifnot(nrow(f$channels)==69,nrow(f$panel)==632,f$optim$convergence==0,all(c('model_source_id','g_all','g_new')%in%names(g)),!anyDuplicated(g$model_source_id))
g<-f$channels |> select(model_source_id) |> left_join(g,by='model_source_id')
stopifnot(!anyNA(g$g_all),!anyNA(g$g_new),all(g$g_all%in%c(0,1)),all(g$g_new%in%c(0,1)))
theta_seed<-2026110917L
fixed_theta<-draw_price_fit(f,nsim=1L,seed=theta_seed)[,,1]
panel<-f$panel |> arrange(model_source_id,year) |> group_by(model_source_id) |>
 mutate(run_start=cumsum(replace_na(year-lag(year)!=1|abs(log_price-lag(log_price))>1e-10,TRUE))) |>
 group_by(model_source_id,run_start) |> mutate(run_n=n(),run_first_year=min(year)) |> ungroup()
runs<-panel |> filter(noise_role=='official',run_n>=2) |>
 select(model_source_id,commodity,year,run_first_year,run_n,actual_log_quote=log_price) |>
 mutate(run_key=paste(model_source_id,run_first_year,sep=' :: run='))
design<-list(years=f$years,commodities=f$commodities,channels=f$channels,panel=f$panel,parameters=f$optim$par,sigma=exp(f$optim$par),
 initial_rw=as.numeric(f$smooth$alphahat[1,seq_len(f$K)]),q=quantity_table$quantity[match(f$commodities,quantity_table$commodity)],
 offset_sd=.5,ar_sd=.30,ar_rho=.8,wedge=log(1.25),g_table=g,fixed_theta=fixed_theta,fixed_theta_seed=theta_seed,
 flat_runs=runs,nrep=200L,ndraws=1000L,seed_base=202611100L,posterior_seed_base=202711100L,
 scenarios=c('S0_prior_calibration','S1_fixed_source_wedge','S2_official_channel_ar','S3_administered_runs'),
 neutral_replicates=c(1L,41L,81L,121L,161L),primary_sha256=digest(file=fit_file,algo='sha256'),g_sha256=digest(file=g_file,algo='sha256'),
 primary_file=fit_file,g_file=g_file,created=as.character(Sys.time()))
saveRDS(design,file.path(out,'design.rds'))
file.copy(here::here("replication","R","helpers.R"),file.path(out,'helpers_frozen.txt'),overwrite=FALSE)
file.copy(here::here("replication","R","state_model.R"),file.path(out,'state_model_frozen.txt'),overwrite=FALSE)
write_csv(g,file.path(out,'explicit_loading_table.csv'));write_csv(runs,file.path(out,'actual_official_flat_runs.csv'))
write_csv(f$panel |> select(commodity,model_source_id,year,precision_units,noise_role),file.path(out,'empirical_observation_mask.csv'))
write_csv(tibble(parameter=c('common_rw_sd','relative_rw_sd','wheat_extra_rw_sd','auction_noise_sd','official_noise_sd','other_noise_sd','wheat_transitory_sd'),value=design$sigma),file.path(out,'generator_parameters.csv'))
fixed_cost<-as.vector(exp(fixed_theta)%*%design$q);w<-match('Wheat',design$commodities)
write_csv(tibble(year=design$years,true_index=100*fixed_cost/mean(fixed_cost[between(design$years,1770,1779)]),wheat_share=exp(fixed_theta[,w])*design$q[w]/fixed_cost),file.path(out,'fixed_stress_truth.csv'))
grid<-crossing(scenario=design$scenarios,replicate=seq_len(design$nrep))
schedule<-bind_rows(grid |> mutate(fit_variant='primary'),grid |> filter(scenario=='S0_prior_calibration') |> mutate(fit_variant='true_parameter_plugin'),
 grid |> filter(replicate%in%design$neutral_replicates) |> mutate(fit_variant='neutral_start_check')) |>
 arrange(replicate,scenario,fit_variant) |> mutate(job_id=row_number(),key=sprintf('%s_rep%03d_%s',scenario,replicate,fit_variant),generator_seed=design$seed_base+replicate,posterior_seed=design$posterior_seed_base+replicate)
write_csv(schedule,file.path(out,'frozen_schedule.csv'))
write_lines(c('FROZEN REVISED PRIMARY SIMULATION DESIGN',paste('Created',design$created),
 'Primary:1795-only institutional-offset design; audited632channel-year cells and69channels. Old65-channel simulations are obsolete-design pilots and will never be pooled with these results.',
 'Four prespecified DGPs with200replicates each. All use the exact empirical observation mask, channel identities, noise roles and capped precision. All800primary analyses re-estimate the seven variance parameters. S0also has200cheap fits at true parameters;20additional neutral-start optimization checks are prespecified (replicates1,41,81,121,161 in each DGP).',
 'S0 prior calibration:fix1700persistent good levels at primary smoothed values; independently draw common, relative and extra wheat RW innovations, wheat transitory shocks, and independent N(0,.5^2) channel offsets. Use fitted primary scales as frozen generating values. No observed trend is added.',
 'S1-S3 fixed realistic truth:one joint posterior commodity-price path, selected using the prespecified seed2026110917, held fixed in every replicate and across all three stresses. Fresh independent N(0,.5^2) channel offsets and independent role-noise draws in every replicate. Coverage here is conditional on this chosen latent path and averaged over source offsets/measurement errors; it is not prior-predictive calibration.',
 'S1:add fixed+log(1.25) to quote channels flagged g_all in the separately reviewed explicit loading table. g_new is documented but its alternative-loading effect is evaluated in the separate exact pass-through analysis, not selected from simulation scores.',
 'S2:add independent stationary AR(1) measurement-error paths to official channels, rho.8,stationary SD.30; no common official factor. States evolve at annual frequency through unobserved years and do not enter true price.',
 'S3:within every actual consecutive identical official-quote run of length>=2, copy the entire generated quote from its first year into all later run years. This repeats the run-start observation noise as well as its price. Latent true prices keep moving. All empirical precision values remain unchanged.',
 'Observation noise:independent Gaussian with role variance divided by min(precision,5). Fresh components generated in the same order for all scenarios so their observation noise and offsets are paired by replicate. S0truth differs from the fixed stress truth.',
 'Inference:1000joint conditional posterior state draws per primary/plugin fit; latent baskets include the transitory wheat state and exclude channel offsets/measurement errors. Normalize every true and posterior basket separately to its own1770smean using fixed quantities. Parameter uncertainty remains excluded from reported conditional bands.',
 'Metrics:log basket index RMSE/bias/coverage; good-year latent log-level errors; pre/post1795 and year-support classes0,1-2,>=3goods. Historical relatives and five period-average relatives:1700-09,1800-04,1820-24,1830-34 versus1770-79,and1825-34versus1820-24. Posterior post1795peak distribution, discrete5th-95th percentile interval, and truth inclusion. Report wheat shares for every S0replicate and for the fixed stress truth.',
 'Monte Carlo sampling unit:replicate. Summary RMSE/bias/pointwise coverage use replicate-level means and MCSE; pooled coverage also has a2000-draw replicate-cluster bootstrap. Functional/peak coverage has Wilson95percent intervals and binomial MCSE. Years within a replicate are not independent samples.',
 'Optimization:true seven scales only initialize warm starts; no test observation fixes estimated parameters. fit_checked internal retries are recorded; on error retry with neutral starts. Failures stay in diagnostics and denominators, not silently discarded. Bound hits recorded separately for every parameter. Neutral-start check results are diagnostic and do not select scenarios.',
 'Execution:atomic job claims in a named batch folder, checkpoint per fit, no overwriting successful fits. Start a fresh batch name only after old workers have stopped; existing completed checkpoints are skipped. BLAS/OpenMP thread counts fixed at1. Freeze code hashes before launch.',
 'Scope:recovery under stated assumptions and selected violations; no claim that this proves the true historical monetary system, source wedges or administrative interpretation.'),file.path(out,'FROZEN_PROTOCOL.txt'))
cat('Frozen',nrow(schedule),'jobs; primary800, plugin200, neutral checks20.\n')

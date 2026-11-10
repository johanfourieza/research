suppressPackageStartupMessages({library(tidyverse);library(here);library(digest)})
out<-here::here("replication","work","analysis","simulation","revised_primary1795")
files<-list.files(file.path(out,'checkpoints'),pattern='[.]rds$',full.names=TRUE)
stopifnot(length(files)>0)
r<-map(files,readRDS);diagnostics<-bind_rows(map(r,'diagnostics'))
write_csv(diagnostics,file.path(out,'fit_diagnostics.csv'))
idcols<-c('scenario','replicate','fit_variant')
attach_id<-function(x,field)bind_cols(x$diagnostics |> select(all_of(idcols)) |> slice(rep(1,nrow(x[[field]]))),x[[field]])
write_csv(map_dfr(r,~attach_id(.x,'attempts')),file.path(out,'optimizer_attempts.csv'))
write_csv(bind_rows(map(r,'generator_check')) |> distinct(),file.path(out,'generator_checks.csv'))
ok<-keep(r,~isTRUE(.x$diagnostics$success))
pars<-map_dfr(ok,~attach_id(.x,'parameters'))
write_csv(pars,file.path(out,'replicate_parameters_and_bounds.csv'))
write_csv(pars |> group_by(scenario,fit_variant,parameter) |> summarise(n=n(),mean_estimate=mean(estimate),sd_estimate=sd(estimate),n_lower=sum(lower_bound_hit),n_upper=sum(upper_bound_hit),.groups='drop'),file.path(out,'parameter_bound_summary.csv'))
evaluated<-keep(ok,~!is.null(.x$evaluation))
get_eval<-function(field)bind_rows(map(evaluated,~.x$evaluation[[field]]))
walk(c('annual','metrics','good_years','good_metrics','ratios','peak','peak_distribution','checks'),function(field)write_csv(get_eval(field),file.path(out,paste0('replicate_',field,'.csv'))))
wheat<-map_dfr(evaluated,~attach_id(.x,'wheat_shares')) |> filter(fit_variant=='primary')
write_csv(wheat,file.path(out,'replicate_wheat_shares.csv'))
write_csv(wheat |> group_by(scenario,replicate) |> summarise(mean_share=mean(wheat_share),min_share=min(wheat_share),max_share=max(wheat_share),mean_1770s_share=mean(wheat_share[between(year,1770,1779)]),mean_post1795_share=mean(wheat_share[year>=1795]),.groups='drop'),file.path(out,'replicate_wheat_share_summary.csv'))
metrics<-get_eval('metrics')
means<-metrics |> pivot_longer(c(log_rmse,log_bias,pointwise_coverage,mean_interval_log_width),names_to='metric',values_to='value') |>
 group_by(scenario,fit_variant,group,metric) |> summarise(n=n(),mean=mean(value),replicate_sd=sd(value),mcse=replicate_sd/sqrt(n),.groups='drop')
write_csv(means,file.path(out,'basket_recovery_summary.csv'))
gm<-get_eval('good_metrics') |> pivot_longer(c(log_rmse,log_bias,pointwise_coverage),names_to='metric',values_to='value') |>
 group_by(scenario,fit_variant,commodity,coverage_class,metric) |> summarise(n=n(),mean=mean(value),replicate_sd=sd(value),mcse=replicate_sd/sqrt(n),.groups='drop')
write_csv(gm,file.path(out,'good_level_recovery_summary.csv'))
set.seed(2026110919L)
boot<-metrics |> group_by(scenario,fit_variant,group) |> group_modify(function(z,key) {
  n<-nrow(z);b<-replicate(2000L,mean(sample(z$pointwise_coverage,n,replace=TRUE)))
  tibble(n=n,coverage=mean(z$pointwise_coverage),bootstrap95_low=quantile(b,.025),bootstrap95_high=quantile(b,.975),bootstrap_mcse=sd(b))
 }) |> ungroup()
write_csv(boot,file.path(out,'pointwise_coverage_cluster_bootstrap.csv'))
wilson<-function(k,n) {
 z<-qnorm(.975);p<-k/n;center<-(p+z^2/(2*n))/(1+z^2/n);half<-z*sqrt(p*(1-p)/n+z^2/(4*n^2))/(1+z^2/n)
 tibble(coverage=p,coverage_mcse=sqrt(p*(1-p)/n),wilson95_low=center-half,wilson95_high=center+half)
}
ratios<-get_eval('ratios')
rs<-ratios |> group_by(scenario,fit_variant,functional) |> summarise(n=n(),covered=sum(covered),mean_log_error=mean(log_error),log_error_mcse=sd(log_error)/sqrt(n()),log_rmse=sqrt(mean(log_error^2)),mean_interval_log_width=mean(log(q95/q05)),.groups='drop')
rs<-bind_cols(rs,map2_dfr(rs$covered,rs$n,wilson)) |> mutate(planned_replicates=200L,missing_or_failed=200L-n)
write_csv(rs,file.path(out,'functional_recovery_summary.csv'))
ps<-get_eval('peak') |> group_by(scenario,fit_variant) |> summarise(n=n(),covered=sum(covered),mean_error_years=mean(error_years),error_mcse=sd(error_years)/sqrt(n()),mean_absolute_error=mean(abs(error_years)),mean_interval_years=mean(interval_years),.groups='drop')
write_csv(bind_cols(ps,map2_dfr(ps$covered,ps$n,wilson)),file.path(out,'peak_recovery_summary.csv'))
plugin<-metrics |> filter(scenario=='S0_prior_calibration') |> pivot_longer(c(log_rmse,log_bias,pointwise_coverage,mean_interval_log_width),names_to='metric',values_to='value') |>
 select(replicate,fit_variant,group,metric,value) |> pivot_wider(names_from=fit_variant,values_from=value)
if(all(c('primary','true_parameter_plugin')%in%names(plugin))) {
 plugin<-plugin |> filter(is.finite(primary),is.finite(true_parameter_plugin)) |> mutate(difference_primary_minus_plugin=primary-true_parameter_plugin)
 write_csv(plugin,file.path(out,'paired_S0_plugin_differences.csv'))
 write_csv(plugin |> group_by(group,metric) |> summarise(n=n(),mean_difference=mean(difference_primary_minus_plugin),mcse=sd(difference_primary_minus_plugin)/sqrt(n()),.groups='drop'),file.path(out,'S0_plugin_difference_summary.csv'))
}
neutral<-keep(ok,~.x$diagnostics$fit_variant=='neutral_start_check')
neutral_comparison<-map_dfr(neutral,function(x) {
 path<-file.path(out,'checkpoints',paste0(str_replace(x$diagnostics$key,'neutral_start_check$','primary'),'.rds'))
 if(!file.exists(path))return(tibble())
 p<-readRDS(path);if(!isTRUE(p$diagnostics$success))return(tibble())
 tibble(scenario=x$diagnostics$scenario,replicate=x$diagnostics$replicate,primary_nll=p$nll,neutral_nll=x$nll,
  nll_primary_minus_neutral=p$nll-x$nll,max_log_price_difference=max(abs(p$log_price_point-x$log_price_point)),
  optimizer_flag=abs(p$nll-x$nll)>1e-4)
})
write_csv(neutral_comparison,file.path(out,'neutral_start_comparison.csv'))
progress<-diagnostics |> group_by(scenario,fit_variant) |> summarise(completed=n(),successful=sum(success),failed=sum(!success),neutral_retries=sum(neutral_retry),fit_seconds=sum(fit_seconds),total_seconds=sum(total_seconds),.groups='drop')
write_csv(progress,file.path(out,'progress.csv'))
schedule<-read_csv(file.path(out,'frozen_schedule.csv'),show_col_types=FALSE)
integrity<-tibble(check=c('expected1020keys','no_duplicate_keys','all_completed_successful','all_evaluated_draws_normalized','all_truth_decompositions_exact'),
 passed=c(setequal(diagnostics$key,schedule$key),!anyDuplicated(diagnostics$key),all(diagnostics$success),all(get_eval('checks')$normalization_error<1e-8),all(bind_rows(map(r,'generator_check'))$decomposition_error<1e-10)))
write_csv(integrity,file.path(out,'integrity_checks.csv'))
manifest<-read_csv(file.path(out,'frozen_code_manifest.csv'),show_col_types=FALSE)
manifest<-manifest |> mutate(current_sha256=map_chr(file,~digest(file=here::here("replication","R",.x),algo='sha256')),unchanged=sha256==current_sha256)
write_csv(manifest,file.path(out,'code_integrity.csv'));stopifnot(all(manifest$unchanged))
write_csv(tibble(key=str_remove(basename(files),'[.]rds$'),sha256=map_chr(files,~digest(file=.x,algo='sha256'))),file.path(out,'checkpoint_hashes.csv'))
write_lines(c('REVISED PRIMARY KNOWN-TRUTH EXPERIMENT: TECHNICAL REPORT',
 paste('Current completed jobs:',nrow(diagnostics),'of1020; successful:',sum(diagnostics$success),'; failed:',sum(!diagnostics$success),'. Read progress.csv before using any summary.'),
 'Design and seeds were frozen before fitting in FROZEN_PROTOCOL.txt, design.rds and frozen_schedule.csv. Four DGPs each have200primary fits; S0adds200true-parameter plug-in fits;20prespecified neutral-start checks assess numerical optimization. No scenarios were selected using scores.',
 'S0is prior-generated calibration on the empirical network. Its random walks can shift basket composition substantially; replicate_wheat_share_summary.csv reports that change. S1-S3use the same preseeded fixed posterior commodity-price path. Their coverage is frequentist coverage conditional on that chosen latent path, with fresh random source offsets and observation errors, and is not prior-predictive calibration.',
 'S1uses the explicit broader source-era g_all loading, which includes the new English-labelled soap auction channel as well as compiled and official sources. It is not an official-only shock. The alternative g_new loading is handled in the separate exact pass-through analysis.',
 'S2uses independent-per-channel stationary official AR errors, not a common official price cycle. S3holds generated official quotes literally constant over the empirical consecutive identical-price runs, repeating the first-year measurement noise. Actual observation masks and capped precisions remain unchanged.',
 'The evaluated truth is the latent basket, including transitory wheat price but excluding all measurement components. Every true and posterior path is normalized to its own1770smean with fixed quantities. Conditional intervals omit variance-parameter uncertainty. S0_plugin_difference_summary.csv isolates some effects of estimating those scales from the generated sample.',
 'basket_recovery_summary.csv and good_level_recovery_summary.csv report replicate-level RMSE,bias and pointwise coverage by period or actual year-support class. pointwise_coverage_cluster_bootstrap.csv resamples whole replicates2000times. No calculation treats the135correlated years of one history as independent Monte Carlo observations.',
 'functional_recovery_summary.csv evaluates the historical relatives and the five prespecified period ratios. Wilson95percent intervals and binomial MCSE use replicate-level inclusion events. peak_recovery_summary.csv tests the discrete5th-95th percentile peak-year interval, with posterior peak probabilities preserved in replicate_peak_distribution.csv.',
 'All optimizer attempts, retries and failures remain in diagnostics. parameter_bound_summary.csv reports bound hits separately for each scale; the generating extra wheat innovation itself is at its lower bound. neutral_start_comparison.csv reports the prespecified20optimizer checks. Successful-fit coverage must be read alongside failures and completed denominators.',
 'The checkpoint and code hashes protect this experiment from changes to production inputs or helper functions during execution. Older constant-offset pilot results are preserved separately and are not pooled here.',
 'Interpretation:these results quantify recovery under the specified DGPs. They cannot establish that the colonial price process, monetary conversions, source-level assumptions or historical interpretation of flat official schedules are correct.'),file.path(out,'TECHNICAL_REPORT.txt'))
print(progress);print(integrity)

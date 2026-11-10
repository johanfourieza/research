suppressPackageStartupMessages({library(tidyverse);library(here);library(digest)})
base<-here::here("replication","work","analysis","simulation","revised_primary1795")
out<-here::here("replication","work","analysis","simulation","revised_primary1795_addon")
files<-list.files(file.path(out,'checkpoints'),pattern='[.]rds$',full.names=TRUE);stopifnot(length(files)>0)
r<-map(files,readRDS);diag<-bind_rows(map(r,'diagnostics'));write_csv(diag,file.path(out,'fit_diagnostics.csv'))
ok<-keep(r,~isTRUE(.x$diagnostics$success));get<-function(field)bind_rows(map(ok,~.x$evaluation[[field]]))
walk(c('annual','metrics','good_years','good_metrics','ratios','peak','peak_distribution','checks'),function(field)write_csv(get(field),file.path(out,paste0('replicate_',field,'.csv'))))
identity<-function(x,field)bind_cols(x$diagnostics |> select(scenario,replicate,fit_variant) |> slice(rep(1,nrow(x[[field]]))),x[[field]])
write_csv(map_dfr(r,~identity(.x,'attempts')),file.path(out,'optimizer_attempts.csv'))
write_csv(map_dfr(ok,~identity(.x,'parameters')),file.path(out,'parameter_bounds.csv'))
write_csv(get('metrics') |> pivot_longer(c(log_rmse,log_bias,pointwise_coverage,mean_interval_log_width),names_to='metric',values_to='value') |> group_by(group,metric) |> summarise(n=n(),mean=mean(value),mcse=sd(value)/sqrt(n()),.groups='drop'),file.path(out,'clean_basket_recovery_summary.csv'))
stress_files<-list.files(file.path(base,'checkpoints'),pattern='^S[123].*_primary[.]rds$',full.names=TRUE)
s<-map(stress_files,readRDS) |> keep(~isTRUE(.x$diagnostics$success))
sm<-bind_rows(map(s,~.x$evaluation$metrics));cm<-get('metrics')
paired<-sm |> select(-fit_variant) |> inner_join(cm |> select(-scenario,-fit_variant),by=c('replicate','group'),suffix=c('_stress','_clean')) |>
 mutate(log_rmse_difference=log_rmse_stress-log_rmse_clean,log_bias_difference=log_bias_stress-log_bias_clean,coverage_difference=pointwise_coverage_stress-pointwise_coverage_clean,interval_log_width_difference=mean_interval_log_width_stress-mean_interval_log_width_clean)
write_csv(paired,file.path(out,'paired_replicate_basket_differences.csv'))
write_csv(paired |> pivot_longer(ends_with('_difference'),names_to='metric',values_to='value') |> group_by(scenario,group,metric) |> summarise(n=n(),mean_difference=mean(value),mcse=sd(value)/sqrt(n()),.groups='drop'),file.path(out,'paired_stress_basket_summary.csv'))
sr<-bind_rows(map(s,~.x$evaluation$ratios));cr<-get('ratios')
pr<-sr |> select(scenario,replicate,functional,point,truth,log_error,covered) |>
 inner_join(cr |> select(replicate,functional,point,truth,log_error,covered),by=c('replicate','functional'),suffix=c('_stress','_clean')) |>
 mutate(log_error_difference=log_error_stress-log_error_clean,squared_error_difference=log_error_stress^2-log_error_clean^2,coverage_difference=as.numeric(covered_stress)-as.numeric(covered_clean))
stopifnot(max(abs(pr$truth_stress-pr$truth_clean))<1e-10)
write_csv(pr,file.path(out,'paired_replicate_functional_differences.csv'))
write_csv(pr |> group_by(scenario,functional) |> summarise(n=n(),mean_paired_log_error=mean(log_error_difference),log_error_mcse=sd(log_error_difference)/sqrt(n()),rmse_stress=sqrt(mean(log_error_stress^2)),rmse_clean=sqrt(mean(log_error_clean^2)),mean_squared_error_difference=mean(squared_error_difference),squared_error_mcse=sd(squared_error_difference)/sqrt(n()),coverage_difference=mean(coverage_difference),coverage_difference_mcse=sd(coverage_difference)/sqrt(n()),.groups='drop'),file.path(out,'paired_stress_functional_summary.csv'))
# Proper scores use only prior-generated S0 truths, never the chosen fixed stress peak.
s0files<-list.files(file.path(base,'checkpoints'),pattern='^S0.*(primary|true_parameter_plugin)[.]rds$',full.names=TRUE)
s0<-map(s0files,readRDS) |> keep(~isTRUE(.x$diagnostics$success))
scores<-map_dfr(s0,function(x) {
 p<-x$evaluation$peak_distribution;true<-x$evaluation$peak$true_peak_year
 mass<-p$probability[match(true,p$peak_year)];before<-sum(p$probability[p$peak_year<1825]);event<-as.numeric(true<1825)
 x$diagnostics |> select(replicate,fit_variant) |> mutate(true_peak_year=true,prob_true_peak=mass,zero_empirical_mass=mass==0,
  empirical_log_score=if_else(mass==0,-Inf,log(mass)),prob_peak_before1825=before,true_peak_before1825=event,brier=(before-event)^2,
  probability_bin=cut(before,breaks=seq(0,1,.1),include.lowest=TRUE))
})
write_csv(scores,file.path(out,'S0_peak_proper_scores.csv'))
write_csv(scores |> group_by(fit_variant) |> summarise(n=n(),mean_brier=mean(brier),brier_mcse=sd(brier)/sqrt(n()),n_zero_empirical_mass=sum(zero_empirical_mass),
  empirical_mean_log_score=mean(empirical_log_score),finite_only_mean_log_score=if(any(!zero_empirical_mass))mean(empirical_log_score[!zero_empirical_mass])else NA_real_,.groups='drop'),file.path(out,'S0_peak_score_summary.csv'))
wilson<-function(k,n) {z<-qnorm(.975);p<-k/n;center<-(p+z*z/(2*n))/(1+z*z/n);half<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/(1+z*z/n);tibble(wilson95_low=center-half,wilson95_high=center+half)}
reliability<-scores |> group_by(fit_variant,probability_bin) |> summarise(n=n(),mean_forecast=mean(prob_peak_before1825),events=sum(true_peak_before1825),event_frequency=mean(true_peak_before1825),.groups='drop')
write_csv(bind_cols(reliability,map2_dfr(reliability$events,reliability$n,wilson)),file.path(out,'S0_peak_reliability.csv'))
wide<-scores |> select(replicate,fit_variant,brier) |> pivot_wider(names_from=fit_variant,values_from=brier)
if(all(c('primary','true_parameter_plugin')%in%names(wide)))write_csv(wide |> filter(is.finite(primary),is.finite(true_parameter_plugin)) |> mutate(brier_primary_minus_plugin=primary-true_parameter_plugin),file.path(out,'S0_paired_peak_Brier.csv'))
write_csv(tibble(completed=nrow(diag),successful=sum(diag$success),failed=sum(!diag$success),planned=200L),file.path(out,'progress.csv'))
manifest<-read_csv(file.path(out,'frozen_addon_code_manifest.csv'),show_col_types=FALSE) |> mutate(current_sha256=map_chr(file,~digest(file=here::here("replication","R",.x),algo='sha256')),unchanged=sha256==current_sha256)
write_csv(manifest,file.path(out,'code_integrity.csv'));stopifnot(all(manifest$unchanged))
write_csv(tibble(key=str_remove(basename(files),'[.]rds$'),sha256=map_chr(files,~digest(file=.x,algo='sha256'))),file.path(out,'checkpoint_hashes.csv'))
write_lines(c('CLEAN FIXED-PATH CONTROL AND PEAK-SCORING SIDECAR',paste('Completed clean reference fits:',nrow(diag),'of200; failures:',sum(!diag$success)),
 'This add-on leaves all original frozen results unchanged. Paired stress-minus-clean estimates isolate each added distortion from the shrinkage estimator\'s behaviour on the chosen fixed path. Monte Carlo errors use within-replicate differences.',
 'The control and stresses share one wheat-heavy price path peaking in1826. These results cannot be generalized across every plausible price history. Conditioning that path on flat official data may understate carry-forward damage on this path; it does not establish a formal lower bound.',
 'S0calibration assumes tau=.5is the true channel-offset prior scale; tau is not re-estimated. Its peak scores cover1795-1834only. This does not validate an empirical peak distribution over1700-1834.',
 'Peak-before1825Brier scores and reliability frequencies are calculated from stored joint-draw probabilities. The log score at the true discrete peak is minus infinity whenever none of1000draws assigns that year. Zero empirical mass can reflect simulation resolution. No pseudocount is inserted. A finite-only log-score mean is conditional on nonzero mass and is not an overall proper-score estimate.',
 'Use progress.csv and the number of paired replicates before quoting any result. The separate base experiment report describes generators, refits, posterior normalization and conditional uncertainty.'),file.path(out,'TECHNICAL_ADDON_REPORT.txt'))
cat('Clean checkpoints',nrow(diag),'of200; paired stress rows',nrow(pr),'\n')

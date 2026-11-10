source(here::here("replication","R","simulation_revised_common.R"))
out<-file.path(sr_out,'flagged_optimizer_refinement');dir.create(out,showWarnings=FALSE)
flags<-read_csv(file.path(sr_out,'neutral_start_comparison.csv'),show_col_types=FALSE) |> filter(optimizer_flag)
# Separate local function environment; production and frozen functions/files unchanged.
refenv<-new.env(parent=environment(fit_price_panel))
refenv$fitSSM<-function(...,control=list()) {
 control<-modifyList(control,list(factr=1e4,pgtol=1e-9))
 KFAS::fitSSM(...,control=control)
}
refined_fit<-fit_price_panel;environment(refined_fit)<-refenv
write_csv(tibble(setting=c('factr','pgtol','maxit','conditional_draws'),value=c(1e4,1e-9,1000,1000)),file.path(out,'refinement_settings.csv'))
for(i in seq_len(nrow(flags))) {
 z<-flags[i,];key<-sprintf('%s_rep%03d',z$scenario,z$replicate);file<-file.path(out,paste0(key,'.rds'));if(file.exists(file))next
 cat('REFINE START',key,format(Sys.time()),'\n');flush.console()
 p<-readRDS(file.path(sr_out,'checkpoints',paste0(key,'_primary.rds')))
 n<-readRDS(file.path(sr_out,'checkpoints',paste0(key,'_neutral_start_check.rds')))
 truth<-sr_generate(z$replicate,z$scenario)
 wp<-log(p$parameters$estimate);np<-log(n$parameters$estimate)
 # Fixed-parameter rebuild is only for evaluating the already estimated neutral solution.
 old_neutral<-fit_price_panel(truth$panel,fixed_parameters=np)
 en<-sr_evaluate(old_neutral,truth,z$replicate,z$scenario,'old_neutral')
 w<-refined_fit(truth$panel,inits=wp,maxit=1000L)
 cat('REFINED WARM',key,w$optim$convergence,w$optim$value,'\n');flush.console()
 nn<-refined_fit(truth$panel,inits=np,maxit=1000L)
 cat('REFINED NEUTRAL',key,nn$optim$convergence,nn$optim$value,'\n');flush.console()
 ew<-sr_evaluate(w,truth,z$replicate,z$scenario,'refined_warm');enn<-sr_evaluate(nn,truth,z$replicate,z$scenario,'refined_neutral')
 pars<-bind_rows(p$parameters |> mutate(solution='old_warm'),n$parameters |> mutate(solution='old_neutral'),
  tibble(parameter=p$parameters$parameter,estimate=exp(w$optim$par),solution='refined_warm'),tibble(parameter=p$parameters$parameter,estimate=exp(nn$optim$par),solution='refined_neutral'))
 opt<-tibble(solution=c('old_warm','old_neutral','refined_warm','refined_neutral'),nll=c(p$nll,n$nll,w$optim$value,nn$optim$value),
  convergence=c(0L,0L,w$optim$convergence,nn$optim$convergence),message=c('original fit','original neutral check',w$optim$message %||% '',nn$optim$message %||% ''))
 models<-list(old_warm=fit_price_panel(truth$panel,fixed_parameters=wp),old_neutral=old_neutral,refined_warm=w,refined_neutral=nn)
 prof<-imap_dfr(models,function(f,label) {
  wi<-match('Wheat',f$commodities);oldsd<-exp(f$optim$par[3]);grid<-sort(unique(c(.005,.01,.02,.035,.05,.07,.1,.15,oldsd)))
  tibble(solution=label,wheat_extra_sd=grid,nll_holding_other_parameters_fixed=map_dbl(grid,function(v) {m<-f$model;m$Q[wi,wi,1]<-m$Q[wi,wi,1]+v^2-oldsd^2;-as.numeric(logLik(m))}))
 })
 result<-list(scenario=z$scenario,replicate=z$replicate,parameters=pars,optimization=opt,profile=prof,
  evaluations=list(old_warm=p$evaluation,old_neutral=en,refined_warm=ew,refined_neutral=enn))
 saveRDS(result,file);cat('REFINE DONE',key,format(Sys.time()),'\n');flush.console()
}
rr<-map(list.files(out,pattern='[.]rds$',full.names=TRUE),readRDS)
add<-function(x,field)x[[field]] |> mutate(scenario=x$scenario,replicate=x$replicate)
write_csv(map_dfr(rr,~add(.x,'optimization')),file.path(out,'optimization_comparison.csv'))
write_csv(map_dfr(rr,~add(.x,'parameters')),file.path(out,'parameter_comparison.csv'))
write_csv(map_dfr(rr,~add(.x,'profile')),file.path(out,'wheat_variance_conditional_profile.csv'))
evals<-function(field)map_dfr(rr,function(x)imap_dfr(x$evaluations,function(e,label)e[[field]] |> mutate(solution=label,scenario=x$scenario,replicate=x$replicate)))
for(field in c('annual','metrics','ratios','peak','peak_distribution'))write_csv(evals(field),file.path(out,paste0(field,'_by_solution.csv')))
rat<-evals('ratios');old<-rat |> filter(solution=='old_warm') |> select(scenario,replicate,functional,old_point=point,old_q05=q05,old_q95=q95,old_covered=covered)
write_csv(rat |> filter(solution!='old_warm') |> left_join(old,by=c('scenario','replicate','functional')) |> mutate(point_log_change=log(point/old_point),q05_log_change=log(q05/old_q05),q95_log_change=log(q95/old_q95),coverage_changed=covered!=old_covered),file.path(out,'functional_changes_from_original.csv'))
peaks<-evals('peak_distribution') |> group_by(scenario,replicate,solution) |> summarise(prob_peak_before1825=sum(probability[peak_year<1825]),prob_peak_before1826=sum(probability[peak_year<1826]),.groups='drop')
write_csv(peaks,file.path(out,'peak_probability_comparison.csv'))
write_lines(c('Separate refinement of prespecified optimizer-check discrepancies; frozen fits unchanged.',
 'Both previously obtained warm and neutral parameter solutions were refined with factr1e4,pgtol1e-9,maxit1000. The private function environment overrides only the local fitSSM control; no production or frozen source was changed.',
 'Original warm, original neutral and both refined solutions use identical1000-draw seeds for their historical functionals, annual coverage and peak probabilities. The likelihood grid changes only extra-wheat innovation SD while holding each solution\'s other parameters fixed; it is not a profiled likelihood.',
 'Inspect optimization_comparison.csv and functional_changes_from_original.csv before deciding whether further numerical reconciliation is needed. No scenario or replicate result has been silently replaced.'),file.path(out,'REFINEMENT_NOTE.txt'))

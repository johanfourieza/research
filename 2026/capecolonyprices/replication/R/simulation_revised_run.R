source(here::here("replication","R","simulation_revised_common.R"))
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)>=2)
worker<-args[1];batch<-args[2];maxrep<-if(length(args)>=3)as.integer(args[3])else 200L
stopifnot(str_detect(worker,'^[A-Za-z0-9_-]+$'),str_detect(batch,'^[A-Za-z0-9_-]+$'))
schedule<-read_csv(file.path(sr_out,'frozen_schedule.csv'),show_col_types=FALSE) |> filter(replicate<=maxrep)
checkpoint_dir<-file.path(sr_out,'checkpoints');claim_dir<-file.path(sr_out,'claims',batch)
dir.create(checkpoint_dir,recursive=TRUE,showWarnings=FALSE);dir.create(claim_dir,recursive=TRUE,showWarnings=FALSE)
raw_fit<-fit_price_panel;attempt_log<-list()
fit_price_panel<-function(...) {
 aa<-list(...);start<-proc.time()[3];z<-tryCatch(raw_fit(...),error=function(e)e)
 attempt_log[[length(attempt_log)+1L]]<<-tibble(attempt=length(attempt_log)+1L,maxit=aa$maxit %||% NA_integer_,
  fixed_parameters=!is.null(aa$fixed_parameters),elapsed_seconds=unname(proc.time()[3]-start),
  convergence=if(inherits(z,'error'))NA_integer_ else z$optim$convergence,nll=if(inherits(z,'error'))NA_real_ else z$optim$value,
  message=if(inherits(z,'error'))conditionMessage(z)else z$optim$message %||% '')
 if(inherits(z,'error'))stop(z)
 z
}
lower<-log(c(.001,.005,.005,.01,.01,.01,.005));upper<-log(c(.8,.8,1.5,1.5,1.5,1.5,1.5))
for(i in seq_len(nrow(schedule))) {
 job<-schedule[i,];checkpoint<-file.path(checkpoint_dir,paste0(job$key,'.rds'))
 if(file.exists(checkpoint))next
 claim<-file.path(claim_dir,job$key)
 if(!dir.create(claim,showWarnings=FALSE))next
 write_csv(tibble(worker=worker,pid=Sys.getpid(),claimed_at=as.character(Sys.time()),job_id=job$job_id),file.path(claim,'owner.csv'))
 cat('START',job$key,format(Sys.time()),'\n');flush.console();start<-proc.time()[3];attempt_log<-list()
 truth<-sr_generate(job$replicate,job$scenario)
 settings<-list(offset_sd=.5,inits=sr_d$parameters)
 if(job$fit_variant=='true_parameter_plugin')settings$fixed_parameters<-sr_d$parameters
 if(job$fit_variant=='neutral_start_check')settings$inits<-log(c(.05,.08,.2,.45,.15,.3,.2))
 f<-tryCatch(do.call(fit_checked,c(list(panel=truth$panel),settings)),error=function(e)e)
 retry<-inherits(f,'error');first_error<-if(retry)conditionMessage(f)else NA_character_
 if(retry&&job$fit_variant!='true_parameter_plugin') {
  settings$inits<-log(c(.05,.08,.2,.45,.15,.3,.2));f<-tryCatch(do.call(fit_checked,c(list(panel=truth$panel),settings)),error=function(e)e)
 }
 ok<-!inherits(f,'error')
 diag<-job |> mutate(worker=worker,pid=Sys.getpid(),success=ok,neutral_retry=retry,
  first_error=first_error,final_error=if(ok)NA_character_ else conditionMessage(f),fit_seconds=unname(proc.time()[3]-start),n_attempts=length(attempt_log))
 result<-list(diagnostics=diag,attempts=bind_rows(attempt_log),generator_check=truth$check,
  wheat_shares=tibble(year=sr_d$years,wheat_share=truth$wheat_share))
 if(ok) {
  result$parameters<-tibble(parameter=c('common_rw_sd','relative_rw_sd','wheat_extra_rw_sd','auction_noise_sd','official_noise_sd','other_noise_sd','wheat_transitory_sd'),estimate=exp(f$optim$par),truth=sr_d$sigma,
   lower_bound_hit=abs(f$optim$par-lower)<1e-4,upper_bound_hit=abs(f$optim$par-upper)<1e-4)
  result$nll<-f$optim$value;result$log_price_point<-price_fit_locations(f)
  if(job$fit_variant!='neutral_start_check') {
   ev<-tryCatch(sr_evaluate(f,truth,job$replicate,job$scenario,job$fit_variant),error=function(e)e)
   if(inherits(ev,'error')) {result$diagnostics$success<-FALSE;result$diagnostics$final_error<-paste('Posterior/evaluation error:',conditionMessage(ev))} else result$evaluation<-ev
  }
 }
 result$diagnostics$total_seconds<-unname(proc.time()[3]-start)
 # Write atomically so live summaries cannot read a partially serialized checkpoint.
 temp<-paste0(checkpoint,'.tmp_',Sys.getpid());saveRDS(result,temp);stopifnot(file.rename(temp,checkpoint))
 cat('DONE',job$key,'success',result$diagnostics$success,'seconds',round(result$diagnostics$total_seconds,1),'\n');flush.console()
}
cat('WORKER FINISHED',worker,batch,format(Sys.time()),'\n')

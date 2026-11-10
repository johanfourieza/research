source(here::here("replication","R","simulation_revised_common.R"))
out<-here::here("replication","work","analysis","simulation","revised_primary1795_addon")
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)>=2)
worker<-args[1];batch<-args[2];stopifnot(str_detect(worker,'^[A-Za-z0-9_-]+$'),str_detect(batch,'^[A-Za-z0-9_-]+$'))
schedule<-read_csv(file.path(out,'frozen_schedule.csv'),show_col_types=FALSE)
cp<-file.path(out,'checkpoints');cl<-file.path(out,'claims',batch)
dir.create(cp,recursive=TRUE,showWarnings=FALSE);dir.create(cl,recursive=TRUE,showWarnings=FALSE)
raw_fit<-fit_price_panel;attempt_log<-list()
fit_price_panel<-function(...) {
 aa<-list(...);start<-proc.time()[3];z<-tryCatch(raw_fit(...),error=function(e)e)
 attempt_log[[length(attempt_log)+1L]]<<-tibble(attempt=length(attempt_log)+1L,maxit=aa$maxit %||% NA_integer_,elapsed_seconds=unname(proc.time()[3]-start),
  convergence=if(inherits(z,'error'))NA_integer_ else z$optim$convergence,nll=if(inherits(z,'error'))NA_real_ else z$optim$value,
  message=if(inherits(z,'error'))conditionMessage(z)else z$optim$message %||% '')
 if(inherits(z,'error'))stop(z)
 z
}
lower<-log(c(.001,.005,.005,.01,.01,.01,.005));upper<-log(c(.8,.8,1.5,1.5,1.5,1.5,1.5))
for(i in seq_len(nrow(schedule))) {
 j<-schedule[i,];file<-file.path(cp,paste0(j$key,'.rds'));if(file.exists(file))next
 claim<-file.path(cl,j$key);if(!dir.create(claim,showWarnings=FALSE))next
 write_csv(tibble(worker=worker,pid=Sys.getpid(),claimed_at=as.character(Sys.time())),file.path(claim,'owner.csv'))
 cat('START',j$key,format(Sys.time()),'\n');flush.console();start<-proc.time()[3];attempt_log<-list()
 truth<-sr_generate(j$replicate,j$scenario)
 f<-tryCatch(fit_checked(truth$panel,inits=sr_d$parameters),error=function(e)e)
 retry<-inherits(f,'error');first_error<-if(retry)conditionMessage(f)else NA_character_
 if(retry)f<-tryCatch(fit_checked(truth$panel,inits=log(c(.05,.08,.2,.45,.15,.3,.2))),error=function(e)e)
 ok<-!inherits(f,'error')
 result<-list(diagnostics=j |> mutate(worker=worker,pid=Sys.getpid(),success=ok,neutral_retry=retry,first_error=first_error,final_error=if(ok)NA_character_ else conditionMessage(f),fit_seconds=unname(proc.time()[3]-start),n_attempts=length(attempt_log)),
  attempts=bind_rows(attempt_log),generator_check=truth$check,wheat_shares=tibble(year=sr_d$years,wheat_share=truth$wheat_share))
 if(ok) {
  result$parameters<-tibble(parameter=c('common_rw_sd','relative_rw_sd','wheat_extra_rw_sd','auction_noise_sd','official_noise_sd','other_noise_sd','wheat_transitory_sd'),estimate=exp(f$optim$par),truth=sr_d$sigma,lower_bound_hit=abs(f$optim$par-lower)<1e-4,upper_bound_hit=abs(f$optim$par-upper)<1e-4)
  result$nll<-f$optim$value;result$log_price_point<-price_fit_locations(f)
  ev<-tryCatch(sr_evaluate(f,truth,j$replicate,j$scenario,'primary'),error=function(e)e)
  if(inherits(ev,'error')) {result$diagnostics$success<-FALSE;result$diagnostics$final_error<-paste('Posterior/evaluation error:',conditionMessage(ev))}else result$evaluation<-ev
 }
 result$diagnostics$total_seconds<-unname(proc.time()[3]-start)
 temp<-paste0(file,'.tmp_',Sys.getpid());saveRDS(result,temp);stopifnot(file.rename(temp,file))
 cat('DONE',j$key,'success',result$diagnostics$success,'seconds',round(result$diagnostics$total_seconds,1),'\n');flush.console()
}

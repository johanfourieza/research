source(here::here("replication","R","simulation_revised_common.R"))
suppressPackageStartupMessages(library(digest))
d<-sr_d;K<-length(d$commodities);J<-nrow(d$channels);w<-match('Wheat',d$commodities)
set.seed(2026110918L);N<-100000L
inc<-sweep(matrix(rnorm(N*K,0,d$sigma[2]),N,K),1,rnorm(N,0,d$sigma[1]),'+');inc[,w]<-inc[,w]+rnorm(N,0,d$sigma[3])
target<-matrix(d$sigma[1]^2,K,K)+diag(d$sigma[2]^2,K);target[w,w]<-target[w,w]+d$sigma[3]^2
empirical<-cov(inc);mcse<-sqrt((target^2+outer(diag(target),diag(target)))/N)
covcheck<-expand_grid(i=seq_len(K),j=seq_len(K)) |> mutate(target=target[cbind(i,j)],empirical=empirical[cbind(i,j)],mcse=mcse[cbind(i,j)],z_error=(empirical-target)/mcse)
stopifnot(max(abs(covcheck$z_error))<5)
write_csv(covcheck,file.path(sr_out,'generator_covariance_check.csv'))
z0<-rnorm(N,0,.30);z1<-.8*z0+rnorm(N,0,.30*sqrt(1-.8^2))
stopifnot(abs(var(z1)/.30^2-1)<.025,abs(cov(z0,z1)/(.8*.30^2)-1)<.025)
checks<-map_dfr(d$scenarios,function(sc) {
 gg<-map(1:2,~sr_generate(.x,sc))
 f<-fit_price_panel(gg[[1]]$panel,fixed_parameters=d$parameters)
 ii<-cbind(match(d$panel$year,d$years),match(d$panel$model_source_id,d$channels$model_source_id))
 hh<-map2_dbl(ii[,1],ii[,2],~f$model$H[.y,.y,.x])
 stopifnot(max(abs(f$model$Q[seq_len(K),seq_len(K),1]-target))<1e-10,
  max(abs(hh-gg[[1]]$noise_sd^2))<1e-10,max(abs(f$model$P1[f$n_dynamic+seq_len(J),f$n_dynamic+seq_len(J)]-diag(.25,J)))<1e-10)
 if(sc!='S0_prior_calibration')stopifnot(identical(gg[[1]]$theta,d$fixed_theta),identical(gg[[2]]$theta,d$fixed_theta))
 if(sc=='S3_administered_runs') {
  v<-d$flat_runs |> left_join(gg[[1]]$panel |> select(model_source_id,year,log_price),by=c('model_source_id','year')) |> group_by(run_key) |> summarise(range=max(log_price)-min(log_price),.groups='drop')
  stopifnot(all(v$range==0))
 }
 bind_rows(map(gg,'check'))
})
write_csv(checks,file.path(sr_out,'generator_pilot_checks.csv'))
code<-list.files(here::here("replication","R"),pattern='^simulation_revised_.*[.]R$',full.names=TRUE)
walk(code,~invisible(parse(file=.x)))
snapshot<-file.path(sr_out,'code_snapshot');dir.create(snapshot,showWarnings=FALSE)
manifest_file<-file.path(sr_out,'frozen_code_manifest.csv')
if(!file.exists(manifest_file)) {
 walk(code,~file.copy(.x,file.path(snapshot,basename(.x)),overwrite=FALSE))
 write_csv(tibble(file=basename(code),sha256=map_chr(code,~digest(file=.x,algo='sha256')),frozen_at=as.character(Sys.time())),manifest_file)
} else {
 old<-read_csv(manifest_file,show_col_types=FALSE);now<-tibble(file=basename(code),sha256=map_chr(code,~digest(file=.x,algo='sha256')))
 stopifnot(setequal(paste(old$file,old$sha256),paste(now$file,now$sha256)))
}
cat('Generator moments, model matrices, fixed stress truths, identical administered runs, exact decomposition and code syntax passed.\n')

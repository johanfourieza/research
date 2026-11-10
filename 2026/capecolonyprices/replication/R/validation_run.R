source(here::here("replication","R","helpers.R"))
run_name <- Sys.getenv("CAPE_VALIDATION_RUN","constant_offsets")
out <- if(run_name=="constant_offsets") file.path(v2_out,"validation") else file.path(v2_out,"validation",run_name)
design <- readRDS(file.path(out,"design.rds"))
stopifnot(identical(design$primary_md5,unname(tools::md5sum(file.path(out,"primary_snapshot.rds")))))
if(file.exists(file.path(out,"state_model_snapshot.R"))) source(file.path(out,"state_model_snapshot.R"))
panel <- design$panel
dir.create(file.path(out,"checkpoints"),showWarnings=FALSE)
interp <- function(year,value,target) {
  ok <- is.finite(value);year <- year[ok];value <- value[ok]
  if(!length(value)) return(rep(NA_real_,length(target)))
  if(length(value)==1L) return(rep(value,length(target)))
  approx(year,value,xout=target,rule=2,ties=mean)$y
}
baseline <- function(train,test,within=TRUE) {
  pred <- rep(NA_real_,nrow(test));keys <- if(within) "model_source_id" else "commodity"
  for(id in unique(test[[keys]])) {
    z <- train[train[[keys]]==id,] |> group_by(year) |> summarise(log_price=mean(log_price),.groups="drop")
    ix <- which(test[[keys]]==id);pred[ix] <- interp(z$year,z$log_price,test$year[ix])
  };pred
}
args <- commandArgs(trailingOnly=TRUE)
fold_ids <- names(design$folds)
if(length(args)) fold_ids <- fold_ids[seq_along(fold_ids)%%as.integer(args[2])==as.integer(args[1])]
for(fid in fold_ids) {
 test <- panel |> filter(row_id%in%design$folds[[fid]]) |> arrange(row_id)
 train <- panel |> filter(!row_id%in%test$row_id)
 for(method in c("full_common_drift","independent_drift")) {
  path <- file.path(out,"checkpoints",paste0(fid,"__",method,".rds"))
  if(file.exists(path)) {old <- readRDS(path);if(identical(old$primary_md5,design$primary_md5)&&old$status=="success") next}
  started <- Sys.time();cat(format(started),fid,method,"\n");flush.console()
  err <- character();f <- NULL
  starts <- list(design$neutral_inits,log(c(.1,.15,.3,.6,.25,.4,.3)))
  for(attempt in seq_along(starts)) {
    f <- tryCatch(fit_checked(panel,holdout=test,inits=starts[[attempt]],independent=method=="independent_drift",shared_offset_sd=0),error=function(e) {err <<- c(err,conditionMessage(e));NULL})
    if(!is.null(f)) break
  }
  result <- list(fold_id=fid,method=method,primary_md5=design$primary_md5,status=if(is.null(f)) "failed" else "success",errors=err,attempts=attempt)
  if(!is.null(f)) {
    ti <- match(test$year,f$years);cj <- match(test$model_source_id,f$channels$model_source_id)
    Z <- f$model$Z[,,1];mu <- f$smooth$alphahat
    signal <- vapply(seq_len(nrow(test)),function(i) sum(mu[ti[i],]*Z[cj[i],]),numeric(1))
    h <- vapply(seq_len(nrow(test)),function(i) f$model$H[cj[i],cj[i],ti[i]],numeric(1))
    v <- vapply(seq_len(nrow(test)),function(i) as.numeric(Z[cj[i],]%*%f$smooth$V[,,ti[i]]%*%Z[cj[i],])+h[i],numeric(1))
    pred <- test |> mutate(method=method,prediction=signal,predictive_sd=sqrt(pmax(0,v)),q05=signal-qnorm(.95)*sqrt(pmax(0,v)),q95=signal+qnorm(.95)*sqrt(pmax(0,v)),
      within_channel=baseline(train,test,TRUE),cross_channel_splice=baseline(train,test,FALSE))
    pred$overlap_splice <- NA_real_
    if(!is.null(design$splice_donors)&&fid%in%design$splice_donors$fold_id) {
      splice <- design$splice_donors |> filter(fold_id==fid)
      target_train <- train |> filter(model_source_id==splice$target) |> select(year,target_price=log_price)
      donor_train <- train |> filter(model_source_id==splice$donor) |> select(year,donor_price=log_price)
      overlap <- inner_join(target_train,donor_train,by="year")
      stopifnot(nrow(overlap)>=3L,all(test$year%in%donor_train$year),all(test$model_source_id==splice$target))
      adjustment <- median(overlap$target_price-overlap$donor_price)
      pred$overlap_splice <- donor_train$donor_price[match(test$year,donor_train$year)]+adjustment
    }
    seed <- design$seed+match(fid,names(design$folds))*10L+match(method,c("full_common_drift","independent_drift"))
    set.seed(seed);states <- simulateSSM(f$model,type="states",nsim=design$nsim,conditional=TRUE)
    joint <- matrix(NA_real_,nrow(test),design$nsim)
    for(i in seq_len(nrow(test))) joint[i,] <- as.numeric(Z[cj[i],]%*%states[ti[i],,])
    noisy <- joint+matrix(rnorm(length(joint)),nrow(joint))*sqrt(h)
    result$predictions <- pred;result$joint_signal <- joint;result$joint_quote <- noisy
    result$fit_diagnostics <- tibble(fold_id=fid,method=method,n_test=nrow(test),convergence=f$optim$convergence,negative_log_likelihood=f$optim$value,
      attempts=attempt,seconds=as.numeric(difftime(Sys.time(),started,units="secs")),seed=seed,nsim=design$nsim,
      common_sd=exp(f$optim$par[1]),relative_sd=exp(f$optim$par[2]),wheat_sd=exp(f$optim$par[3]),auction_noise_sd=exp(f$optim$par[4]),official_noise_sd=exp(f$optim$par[5]),other_noise_sd=exp(f$optim$par[6]),wheat_transitory_sd=exp(f$optim$par[7]),
      offset_sd=f$offset_sd,shared_offset_sd=f$shared_offset_sd,official_ar_sd=f$official_ar_sd)
  }
  temp_path <- paste0(path,".tmp")
  saveRDS(result,temp_path)
  if(file.exists(path)) file.remove(path)
  stopifnot(file.rename(temp_path,path))
 }
}

suppressPackageStartupMessages({library(tidyverse);library(here);library(KFAS)})
source(here::here("replication","R","state_model.R"))
v2_root <- here::here("replication","work")
v2_out <- file.path(v2_root,"analysis")
v2_read <- function(name) read_csv(file.path(v2_out,name),show_col_types=FALSE,guess_max=Inf)
v2_write <- function(x,name) write_csv(x,file.path(v2_out,name))
quantity_table <- tribble(~commodity,~quantity,~standard_unit,
 "Wheat",179,"kg","Beans",20,"kg","Meat",5,"kg","Butter",3,"kg",
 "Soap",1.3,"kg","Cotton",3,"m2","Candles",1.3,"kg","Oil",1.3,"litre","Fuel",3,"MBtu")

fit_checked <- function(panel,...) {
 args <- list(...)
 if(is.null(args$maxit)) args$maxit <- 250L
 f <- do.call(fit_price_panel,c(list(panel=panel),args))
 if(f$optim$convergence!=0L) {
   args$inits <- f$optim$par;args$maxit <- 600L
   f <- do.call(fit_price_panel,c(list(panel=panel),args))
 }
 stopifnot(f$optim$convergence==0L,is.finite(f$optim$value),all(is.finite(price_fit_locations(f))))
 f
}

index_from_fit <- function(fit,nsim=1000L,seed=20261109L) {
 q <- quantity_table$quantity[match(fit$commodities,quantity_table$commodity)]
 stopifnot(!anyNA(q))
 base <- between(fit$years,1770,1779)
 prices <- exp(price_fit_locations(fit));cost <- as.vector(prices%*%q)
 index <- 100*cost/mean(cost[base])
 lp <- draw_price_fit(fit,nsim,seed)
 draw_cost <- apply(sweep(exp(lp),2L,q,"*"),c(1L,3L),sum)
 joint <- 100*sweep(draw_cost,2L,colMeans(draw_cost[base,,drop=FALSE]),"/")
 inflation <- rbind(NA_real_,100*(joint[-1,,drop=FALSE]/joint[-nrow(joint),,drop=FALSE]-1))
 quant <- function(x) t(apply(x,1L,function(a) if(all(is.na(a))) rep(NA_real_,3L) else quantile(a,c(.05,.5,.95),names=FALSE)))
 qi <- quant(joint);qf <- quant(inflation)
 annual <- tibble(year=fit$years,index_point=index,index_q05=qi[,1],index_draw_median=qi[,2],index_q95=qi[,3],
   inflation_pct=c(NA_real_,100*(tail(index,-1)/head(index,-1)-1)),inflation_q05=qf[,1],inflation_draw_median=qf[,2],inflation_q95=qf[,3],annual_cost_rd=cost)
 weights <- tibble(commodity=fit$commodities,base_weight=colMeans(prices[base,,drop=FALSE])*q/mean(cost[base]))
 stopifnot(max(abs(colMeans(joint[base,,drop=FALSE])-100))<1e-8,abs(sum(weights$base_weight)-1)<1e-10)
 list(annual=annual,joint=joint,inflation=inflation,weights=weights,log_price=lp,seed=seed)
}

historical_functionals <- function(annual,joint,variant_id) {
 yy <- annual$year;at <- function(y) match(y,yy)
 base <- between(yy,1770,1779);early_high <- between(yy,1818,1824)
 point <- annual$index_point
 defs <- list(
  ratio_1770s_to1700=list(point=mean(point[base])/point[at(1700)],draw=colMeans(joint[base,,drop=FALSE])/joint[at(1700),]),
  ratio_1822_to1795=list(point=point[at(1822)]/point[at(1795)],draw=joint[at(1822),]/joint[at(1795),]),
  ratio_1818_1824_to1795=list(point=mean(point[early_high])/point[at(1795)],draw=colMeans(joint[early_high,,drop=FALSE])/joint[at(1795),]),
  ratio_1834_to1795=list(point=point[at(1834)]/point[at(1795)],draw=joint[at(1834),]/joint[at(1795),]),
  ratio_1824_to1822=list(point=point[at(1824)]/point[at(1822)],draw=joint[at(1824),]/joint[at(1822),]),
  ratio_1787_to1785=list(point=point[at(1787)]/point[at(1785)],draw=joint[at(1787),]/joint[at(1785),]))
 relatives <- imap_dfr(defs,function(x,id) tibble(variant_id=variant_id,functional=id,point=x$point,
   q05=quantile(x$draw,.05),median=median(x$draw),q95=quantile(x$draw,.95),prob_gt1=mean(x$draw>1),prob_gt3=mean(x$draw>3),draws=length(x$draw)))
 win <- which(yy>=1795);peaks <- yy[win[max.col(t(joint[win,,drop=FALSE]),ties.method="first")]]
 peak <- tibble(variant_id=variant_id,draw_id=seq_along(peaks),peak_year=peaks)
 peak_summary <- tibble(variant_id=variant_id,point_peak_year=yy[win[which.max(point[win])]],
   peak_q05=quantile(peaks,.05,type=1),peak_median=median(peaks),peak_q95=quantile(peaks,.95,type=1),
   prob_peak_before1825=mean(peaks<1825),prob_peak_before1826=mean(peaks<1826),prob_peak1818_1824=mean(between(peaks,1818,1824)),draws=length(peaks))
 list(relatives=relatives,peak_draws=peak,peak_summary=peak_summary)
}

export_joint <- function(joint,years,name,seed) {
 dir.create(file.path(here::here("data","series")),showWarnings=FALSE)
 z <- tibble(year=rep(years,ncol(joint)),draw_id=rep(seq_len(ncol(joint)),each=length(years)),index=as.vector(joint))
 path <- file.path(here::here("data","series"),paste0(name,".csv.gz"))
 write_csv(z,path)
 check <- read_csv(path,show_col_types=FALSE)
 stopifnot(nrow(check)==length(joint),max(abs(check$index-z$index))<1e-10,identical(check$year,as.double(z$year)))
 saveRDS(list(year=years,draw_id=seq_len(ncol(joint)),index=joint,seed=seed,scope="conditional_on_fitted_variance_parameters_units_source_prior_and_model"),file.path(here::here("data","series"),paste0(name,".rds")))
 invisible(z)
}

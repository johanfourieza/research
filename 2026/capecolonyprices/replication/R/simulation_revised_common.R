Sys.setenv(OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1',BLIS_NUM_THREADS='1')
suppressPackageStartupMessages({library(tidyverse);library(here);library(KFAS)})
sr_out<-here::here("replication","work","analysis","simulation","revised_primary1795")
source(file.path(sr_out,'helpers_frozen.txt'));source(file.path(sr_out,'state_model_frozen.txt'))
sr_d<-readRDS(file.path(sr_out,'design.rds'))

sr_generate<-function(rep,scenario) {
 d<-sr_d;set.seed(d$seed_base+rep);T<-length(d$years);K<-length(d$commodities);J<-nrow(d$channels);w<-match('Wheat',d$commodities)
 inc<-sweep(matrix(rnorm((T-1)*K,0,d$sigma[2]),T-1,K),1,rnorm(T-1,0,d$sigma[1]),'+')
 inc[,w]<-inc[,w]+rnorm(T-1,0,d$sigma[3])
 rw<-rbind(d$initial_rw,sweep(apply(inc,2,cumsum),2,d$initial_rw,'+'))
 psi<-rnorm(T,0,d$sigma[7]);theta<-rw;theta[,w]<-theta[,w]+psi
 if(scenario!='S0_prior_calibration')theta<-d$fixed_theta
 offsets<-rnorm(J,0,.5);official<-which(d$channels$noise_role=='official');ar<-matrix(0,T,J)
 ar[1,official]<-rnorm(length(official),0,d$ar_sd)
 for(t in 2:T)ar[t,official]<-d$ar_rho*ar[t-1,official]+rnorm(length(official),0,d$ar_sd*sqrt(1-d$ar_rho^2))
 panel<-d$panel;tt<-match(panel$year,d$years);jj<-match(panel$model_source_id,d$channels$model_source_id);kk<-match(panel$commodity,d$commodities)
 role<-match(panel$noise_role,c('auction','official','other'))+3L
 noise_sd<-d$sigma[role]/sqrt(pmin(5,panel$precision_units));noise<-rnorm(nrow(panel),0,noise_sd)
 source_wedge<-if(scenario=='S1_fixed_source_wedge')d$wedge*d$g_table$g_all[jj]else rep(0,nrow(panel))
 persistent_error<-if(scenario=='S2_official_channel_ar')ar[cbind(tt,jj)]else rep(0,nrow(panel))
 true_price<-theta[cbind(tt,kk)];y<-true_price+offsets[jj]+source_wedge+persistent_error+noise;carry<-rep(0,length(y))
 if(scenario=='S3_administered_runs') {
  keys<-paste(panel$model_source_id,panel$year)
  for(run in unique(d$flat_runs$run_key)) {
   r<-d$flat_runs |> filter(run_key==run);dest<-match(paste(r$model_source_id,r$year),keys);start<-match(paste(r$model_source_id[1],r$run_first_year[1]),keys)
   carry[dest]<-y[start]-y[dest];y[dest]<-y[start]
  }
 }
 panel$log_price<-y;cost<-as.vector(exp(theta)%*%d$q);base<-between(d$years,1770,1779);index<-100*cost/mean(cost[base]);wshare<-exp(theta[,w])*d$q[w]/cost
 err<-max(abs(y-true_price-offsets[jj]-source_wedge-persistent_error-noise-carry))
 stopifnot(err<1e-10,abs(mean(index[base])-100)<1e-8,identical(panel$year,d$panel$year),all(is.finite(y)),all(is.finite(index)))
 list(panel=panel,theta=theta,index=index,wheat_share=wshare,cost=cost,noise_sd=noise_sd,
  check=tibble(scenario=scenario,replicate=rep,decomposition_error=err,truth_normalization=mean(index[base]),n_quotes=nrow(panel),
   wheat_share_mean=mean(wshare),wheat_share_q05=quantile(wshare,.05),wheat_share_q95=quantile(wshare,.95),wheat_base_cost_share=mean(exp(theta[base,w]))*d$q[w]/mean(cost[base])))
}

sr_periods<-function(year)list(all=rep(TRUE,length(year)),pre1795=year<1795,post1795=year>=1795)

sr_evaluate<-function(f,truth,rep,scenario,variant) {
 d<-sr_d;idx<-index_from_fit(f,nsim=d$ndraws,seed=d$posterior_seed_base+rep)
 support<-d$panel |> distinct(year,commodity) |> count(year,name='n_goods')
 a<-idx$annual |> left_join(support,by='year') |> mutate(n_goods=replace_na(n_goods,0L),coverage_class=case_when(n_goods==0~'0',n_goods<=2~'1-2',TRUE~'>=3'),
  true_index=truth$index,log_error=log(index_point/true_index),covered=true_index>=index_q05&true_index<=index_q95,interval_log_width=log(index_q95/index_q05))
 masks<-c(sr_periods(a$year),set_names(map(c('0','1-2','>=3'),~a$coverage_class==.x),paste0('goods_',c('0','1-2','>=3'))),list(post1795_zero_quotes=a$year>=1795&a$n_goods==0))
 metrics<-imap_dfr(masks,function(m,label)tibble(group=label,n_years=sum(m),log_rmse=sqrt(mean(a$log_error[m]^2)),log_bias=mean(a$log_error[m]),pointwise_coverage=mean(a$covered[m]),mean_interval_log_width=mean(a$interval_log_width[m])))
 lp<-price_fit_locations(f);lo<-apply(idx$log_price,c(1,2),quantile,.05);hi<-apply(idx$log_price,c(1,2),quantile,.95)
 goods<-expand_grid(commodity=d$commodities,year=d$years) |> mutate(true_log_price=as.vector(truth$theta),fitted_log_price=as.vector(lp),q05=as.vector(lo),q95=as.vector(hi),log_error=fitted_log_price-true_log_price,covered=true_log_price>=q05&true_log_price<=q95) |>
 left_join(a |> select(year,n_goods,coverage_class),by='year')
 goods_metrics<-goods |> group_by(commodity,coverage_class) |> summarise(n_years=n(),log_rmse=sqrt(mean(log_error^2)),log_bias=mean(log_error),pointwise_coverage=mean(covered),.groups='drop')
 hf<-historical_functionals(idx$annual,idx$joint,variant);ht<-historical_functionals(tibble(year=d$years,index_point=truth$index),matrix(truth$index,ncol=1),'truth')
 ratios<-hf$relatives |> left_join(ht$relatives |> select(functional,truth=point),by='functional')
 periods<-list(ratio_1700_1709_to1770s=c(1700,1709,1770,1779),ratio_1800_1804_to1770s=c(1800,1804,1770,1779),ratio_1820_1824_to1770s=c(1820,1824,1770,1779),ratio_1830_1834_to1770s=c(1830,1834,1770,1779),ratio_1825_1834_to1820_1824=c(1825,1834,1820,1824))
 extra<-imap_dfr(periods,function(r,id) {
  n<-between(d$years,r[1],r[2]);b<-between(d$years,r[3],r[4]);draw<-colMeans(idx$joint[n,,drop=FALSE])/colMeans(idx$joint[b,,drop=FALSE])
  tibble(variant_id=variant,functional=id,point=mean(a$index_point[n])/mean(a$index_point[b]),q05=quantile(draw,.05),median=median(draw),q95=quantile(draw,.95),prob_gt1=mean(draw>1),prob_gt3=mean(draw>3),draws=length(draw),truth=mean(truth$index[n])/mean(truth$index[b]))
 })
 ratios<-bind_rows(ratios,extra) |> mutate(covered=truth>=q05&truth<=q95,log_error=log(point/truth))
 peak<-hf$peak_summary |> mutate(true_peak_year=ht$peak_summary$point_peak_year,covered=true_peak_year>=peak_q05&true_peak_year<=peak_q95,error_years=point_peak_year-true_peak_year,interval_years=peak_q95-peak_q05)
 peaks<-hf$peak_draws |> count(peak_year,name='n_draws') |> complete(peak_year=1795:1834,fill=list(n_draws=0L)) |> mutate(probability=n_draws/d$ndraws)
 add<-function(x)x |> mutate(replicate=rep,scenario=scenario,fit_variant=variant)
 list(annual=add(a),metrics=add(metrics),good_years=add(goods),good_metrics=add(goods_metrics),ratios=add(ratios),peak=add(peak),peak_distribution=add(peaks),
  checks=add(tibble(normalization_error=max(abs(colMeans(idx$joint[between(d$years,1770,1779),,drop=FALSE])-100)),finite=all(is.finite(idx$joint)))))
}

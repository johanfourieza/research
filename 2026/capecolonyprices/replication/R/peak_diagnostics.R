source(here::here("replication","R","helpers.R"))
# This decomposition removes the realised transitory wheat state from the SAME
# fitted model and SAME joint draws. It is distinct from re-estimating sigma_psi=0.
peak_result <- function(joint,point,years,id,target,window) {
 w <- if(window=="1700_1834") seq_along(years) else which(years>=1795)
 py <- years[w[max.col(t(joint[w,,drop=FALSE]),ties.method="first")]]
 list(draws=tibble(variant_id=id,target,window,draw_id=seq_along(py),peak_year=py),
  summary=tibble(variant_id=id,target,window,point_peak_year=years[w[which.max(point[w])]],
   peak_q05=quantile(py,.05,type=1),peak_median=median(py),peak_q95=quantile(py,.95,type=1),
   prob_peak_before1825=mean(py<1825),prob_peak_before1826=mean(py<1826),
   prob_peak1818_1824=mean(between(py,1818,1824)),draws=length(py)))
}
ans <- list();annual <- list()
for(id in c("primary","offsets_constant")) {
 path <- if(id=="primary") "annual_state_model.rds" else "audited_constant_fit.rds"
 fit <- readRDS(file.path(v2_out,path))
 seed <- if(id=="primary") 20261109L else 20261112L
 set.seed(seed)
 z <- simulateSSM(fit$model,type="states",nsim=1000L,conditional=TRUE)
 q <- quantity_table$quantity[match(fit$commodities,quantity_table$commodity)]
 base <- between(fit$years,1770,1779)
 for(target in c("basket_including_transitory_wheat","persistent_price_component_only")) {
  mapping <- fit$price_map
  if(target=="persistent_price_component_only") mapping[,fit$K+1L] <- 0
  mu <- fit$smooth$alphahat%*%t(mapping);cost <- as.vector(exp(mu)%*%q)
  point <- 100*cost/mean(cost[base]);joint <- vapply(seq_len(dim(z)[3]),function(i) {
   cc <- as.vector(exp(z[,,i]%*%t(mapping))%*%q);100*cc/mean(cc[base])
  },numeric(length(fit$years)))
  annual[[paste(id,target)]] <- tibble(variant_id=id,target,year=fit$years,index_point=point,
   index_q05=apply(joint,1,quantile,.05),index_q95=apply(joint,1,quantile,.95))
  for(window in c("1700_1834","1795_1834")) ans[[paste(id,target,window)]] <- peak_result(joint,point,fit$years,id,target,window)
  if(id=="primary"&&target=="basket_including_transitory_wheat") {
   exported <- readRDS(file.path(here::here("data","series"),"annual_index_joint_draws.rds"))
   stopifnot(max(abs(joint-exported$index))<1e-10)
  }
 }
 rm(z);gc()
}
v2_write(map_dfr(ans,"draws"),"peak_decomposition_draws.csv")
v2_write(map_dfr(ans,"summary"),"peak_decomposition_summary.csv")
v2_write(bind_rows(annual),"peak_decomposition_annual.csv")
print(map_dfr(ans,"summary"),width=180)

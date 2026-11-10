source(here::here("replication","R","helpers.R"))
settings <- v2_read("v2_variant_settings.csv")
ids <- c("primary",settings$variant_id);out <- list();draws <- list()
for(id in ids) {
 if(id=="primary") {
  z <- readRDS(file.path(here::here("data","series"),"annual_index_joint_draws.rds"))
  a <- v2_read("annual_inflation_1700_1834.csv")
 } else {
  z <- readRDS(file.path(v2_out,"variants",paste0(id,"_joint.rds")))
  a <- readRDS(file.path(v2_out,"variants",paste0(id,"_summary.rds")))$annual
 }
 for(window in c("1700_1834","1795_1834")) {
  w <- if(window=="1700_1834") seq_along(z$year) else which(z$year>=1795)
  py <- z$year[w[max.col(t(z$index[w,,drop=FALSE]),ties.method="first")]]
  out[[paste(id,window)]] <- tibble(variant_id=id,window,point_peak_year=a$year[w[which.max(a$index_point[w])]],
   peak_q05=quantile(py,.05,type=1),peak_median=median(py),peak_q95=quantile(py,.95,type=1),
   prob_peak_before1825=mean(py<1825),prob_peak_before1826=mean(py<1826),
   prob_peak1818_1824=mean(between(py,1818,1824)),draws=length(py))
  draws[[paste(id,window)]] <- tibble(variant_id=id,window,draw_id=seq_along(py),peak_year=py)
 }
}
v2_write(bind_rows(out),"peak_timing_by_window.csv")
v2_write(bind_rows(draws),"peak_year_draws_by_window.csv")
check <- bind_rows(out) |> filter(window=="1795_1834") |> select(-window)
reference <- v2_read("peak_timing_all.csv")
stopifnot(isTRUE(all.equal(check,reference,tolerance=1e-12,check.attributes=FALSE)))
cat("All44 variants have full-period and post1795 peak distributions.\n")

source(here::here("replication","R","helpers.R"))
f <- readRDS(file.path(v2_out,"annual_state_model.rds"));p <- f$panel
channels <- p |> group_by(commodity,model_source_id) |> summarise(first=min(year),last=max(year),n=n(),.groups="drop") |>
 filter(first<1795,last>=1795) |> arrange(commodity,model_source_id)
bridges <- map_dfr(seq_len(nrow(channels)),function(i) {
 z <- p |> filter(model_source_id==channels$model_source_id[i]) |> arrange(year)
 pre <- z |> filter(year<1795) |> slice_tail(n=1);post <- z |> filter(year>=1795) |> slice_head(n=1)
 tibble(bridge_id=i,commodity=z$commodity[1],model_source_id=z$model_source_id[1],source_role=z$source_role[1],
  n_before=sum(z$year<1795),n_after=sum(z$year>=1795),last_pre_year=pre$year,first_post_year=post$year,
  last_pre_price=exp(pre$log_price),first_post_price=exp(post$log_price),
  log_long_difference=post$log_price-pre$log_price,price_relative=exp(post$log_price-pre$log_price),
  elapsed_years=post$year-pre$year,annualised_pct=100*expm1((post$log_price-pre$log_price)/(post$year-pre$year)),
  first_year=first(z$year),last_year=last(z$year),full_span_price_relative=exp(last(z$log_price)-first(z$log_price)))
})
v2_write(bridges,"direct_1795_bridges.csv")
res <- list();fun <- list()
dir.create(file.path(v2_out,"bridges"),showWarnings=FALSE)
for(i in seq_len(nrow(channels))) {
 path <- file.path(v2_out,"bridges",paste0("omit_",i,".rds"))
 if(file.exists(path)) {r <- readRDS(path)} else {
  panel <- p |> filter(model_source_id!=channels$model_source_id[i])
  fit <- fit_checked(panel,inits=f$optim$par)
  d <- index_from_fit(fit,nsim=1000,seed=20261200+i)
  h <- historical_functionals(d$annual,d$joint,paste0("omit_bridge_",i))
  r <- list(annual=d$annual |> mutate(bridge_id=i,omitted_channel=channels$model_source_id[i]),functionals=h$relatives,peak=h$peak_summary,convergence=fit$optim$convergence)
  saveRDS(r,path)
 }
 res[[i]] <- r$annual;fun[[i]] <- r$functionals
 cat("Bridge omission",i,"/",nrow(channels),"complete.\n")
}
v2_write(bind_rows(res),"bridge_omission_annual.csv");v2_write(bind_rows(fun),"bridge_omission_functionals.csv")
cat(nrow(channels),"direct channels bridge1795. Their differences cancel static channel offsets, not currency errors or within-channel quality change.\n")

source(here::here("replication","R","helpers.R"))
primary <- readRDS(file.path(v2_out,"annual_state_model.rds"))
map <- v2_read("channel_family_loading_map.csv");p <- primary$panel
out <- file.path(v2_out,"pass_through");dir.create(out,showWarnings=FALSE)
q <- quantity_table$quantity[match(primary$commodities,quantity_table$commodity)]
base <- between(primary$years,1770,1779)
cost_index <- function(mu) {z <- as.vector(exp(mu)%*%q);100*z/mean(z[base])}
grid <- crossing(loading=c("g_all","g_new"),gamma=c(0,.25,.5,.75))
for(i in seq_len(nrow(grid))) {
 a <- grid[i,];path <- file.path(out,paste0(a$loading,"_",a$gamma,".rds"));if(file.exists(path)) next
 g <- setNames(map[[a$loading]],map$model_source_id)
 basefit <- fit_checked(p,fixed_parameters=primary$optim$par,shared_offset_sd=a$gamma,shared_loading_override=g)
 mu0 <- price_fit_locations(basefit);idx0 <- cost_index(mu0)
 results <- list()
 for(scenario in c("source_wedge","true_price_step")) {
  x <- p;delta <- log(1.25)
  loading <- if(scenario=="source_wedge") unname(g[x$model_source_id]) else as.numeric(x$year>=1795)
  x$log_price <- x$log_price+delta*loading
  z <- fit_checked(x,fixed_parameters=primary$optim$par,shared_offset_sd=a$gamma,shared_loading_override=g)
  mu1 <- price_fit_locations(z);idx1 <- cost_index(mu1)
  # Check linearity only for Gaussian log-price states, not nonlinear basket costs.
  xx <- p;xx$log_price <- xx$log_price+2*delta*loading
  zz <- fit_checked(xx,fixed_parameters=primary$optim$par,shared_offset_sd=a$gamma,shared_loading_override=g)
  err <- max(abs((price_fit_locations(zz)-mu0)-2*(mu1-mu0)))
  stopifnot(err<1e-7)
  results[[scenario]] <- list(annual=tibble(loading=a$loading,gamma=a$gamma,scenario,year=primary$years,
    baseline_index=idx0,perturbed_index=idx1,basket_log_response=log(idx1/idx0),finite_basket_pass_through=log(idx1/idx0)/delta),
   good=expand_grid(commodity=primary$commodities,year=primary$years) |>
    mutate(loading=a$loading,gamma=a$gamma,scenario,log_price_response=as.vector(mu1-mu0),log_price_pass_through=log_price_response/delta),
   check=tibble(loading=a$loading,gamma=a$gamma,scenario,max_linearity_error=err))
 }
 factor <- tibble(loading=a$loading,gamma=a$gamma,mean=0,sd=0)
 if(a$gamma>0) {
  j <- basefit$n_dynamic+seq_len(nrow(basefit$channels));gg <- unname(g[basefit$channels$model_source_id])
  covprior <- basefit$model$P1[j,j];l <- as.vector(a$gamma^2*solve(covprior,gg))
  a_mean <- as.vector(basefit$smooth$alphahat[1,j]);a_cov <- basefit$smooth$V[j,j,1]
  factor$mean <- sum(l*a_mean)
  factor$sd <- sqrt(max(0,a$gamma^2-a$gamma^2*sum(gg*l)+as.numeric(t(l)%*%a_cov%*%l)))
 }
 saveRDS(list(results=results,factor=factor),path)
 cat("Pass-through",a$loading,a$gamma,"complete.\n")
}
r <- map(seq_len(nrow(grid)),~readRDS(file.path(out,paste0(grid$loading[.x],"_",grid$gamma[.x],".rds"))))
annual <- map_dfr(r,~map_dfr(.x$results,"annual"))
v2_write(annual,"fixed_parameter_pass_through_annual.csv")
v2_write(map_dfr(r,~map_dfr(.x$results,"good")),"fixed_parameter_pass_through_goods.csv")
v2_write(map_dfr(r,~map_dfr(.x$results,"check")),"pass_through_linearity_checks.csv")
v2_write(map_dfr(r,"factor"),"assumption_conditioned_shared_factor_diagnostic.csv")
v2_write(annual |> filter(between(year,1820,1824)) |> group_by(loading,gamma,scenario) |>
 summarise(mean_log_response=mean(basket_log_response),finite_basket_pass_through=mean(finite_basket_pass_through),.groups="drop"),"pass_through_1820s_summary.csv")

# Every direct same-good/year overlap supporting the loading contrast is explicit.
p2 <- p |> left_join(map |> select(model_source_id,g_all,g_new,family),by="model_source_id")
overlaps <- map_dfr(c("g_all","g_new"),function(gname) p2 |> group_split(commodity,year) |> map_dfr(function(z) {
 left <- filter(z,.data[[gname]]==1);right <- filter(z,.data[[gname]]==0)
 if(!nrow(left)||!nrow(right)) return(tibble())
 crossing(i=seq_len(nrow(left)),j=seq_len(nrow(right))) |>
 transmute(loading=gname,commodity=z$commodity[1],year=z$year[1],loaded_channel=left$model_source_id[i],
  unloaded_channel=right$model_source_id[j],loaded_role=left$noise_role[i],unloaded_role=right$noise_role[j],
  observed_log_wedge=left$log_price[i]-right$log_price[j])
}))
v2_write(overlaps,"direct_loading_overlap_pairs.csv")

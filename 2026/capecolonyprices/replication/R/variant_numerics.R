source(here::here("replication","R","helpers.R"))
ledger <- v2_read("nominal_observation_ledger.csv")
checks <- list()
for(id in c("offsets_constant","source_prior_025","official_ar015","official_ar030")) {
 args <- switch(id,offsets_constant=list(),source_prior_025=list(offset_sd=.25),
 official_ar015=list(official_ar_sd=.15),official_ar030=list(official_ar_sd=.30))
 p <- prepare_price_panel(ledger,fixed_offsets=id=="offsets_constant")
 cold <- do.call(fit_checked,c(list(panel=p),args))
 previous <- readRDS(file.path(v2_out,"variants",paste0(id,"_summary.rds")))
 q <- quantity_table$quantity[match(cold$commodities,quantity_table$commodity)]
 cost <- as.vector(exp(price_fit_locations(cold))%*%q);index <- 100*cost/mean(cost[between(cold$years,1770,1779)])
 z <- tibble(variant_id=id,neutral_convergence=cold$optim$convergence,neutral_nll=cold$optim$value,
 warm_nll=previous$parameters$negative_log_likelihood,nll_difference=neutral_nll-warm_nll,
 max_relative_index_difference=max(abs(index/previous$annual$index_point-1)))
 checks[[id]] <- z
 v2_write(bind_rows(checks),"variant_neutral_start_checks.csv")
 stopifnot(z$neutral_convergence==0,abs(z$nll_difference)<.01,z$max_relative_index_difference<.001)
 cat(id,"neutral start checked.\n")
}

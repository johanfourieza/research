source(here::here("replication","R","helpers.R"))
s <- v2_read("v2_variant_settings.csv")
r <- map(s$variant_id,~readRDS(file.path(v2_out,"variants",paste0(.x,"_summary.rds"))))
annual <- map_dfr(r,"annual");parameters <- map_dfr(r,"parameters")
stopifnot(nrow(annual)==135*nrow(s),all(parameters$convergence==0L))
v2_write(annual |> transmute(variant_id,year,index=index_point,inflation_pct),"annual_structural_variants.csv")
v2_write(annual,"annual_variant_conditional_summaries.csv")
v2_write(parameters,"structural_variant_parameters.csv")
v2_write(bind_rows(v2_read("historical_functionals_primary.csv"),map_dfr(r,"functionals")),"historical_functionals_all.csv")
v2_write(bind_rows(v2_read("peak_timing_primary.csv"),map_dfr(r,"peak")),"peak_timing_all.csv")
v2_write(bind_rows(v2_read("peak_year_draws_primary.csv"),map_dfr(r,"peak_draws")),"peak_year_draws_all.csv")
primary <- readRDS(file.path(v2_out,"annual_state_model.rds"))
ll <- bind_rows(tibble(variant_id="primary",offset_sd=.5,shared_offset_sd=0,shared_offset_mean=0,official_ar_sd=0,
 negative_log_likelihood=primary$optim$value),parameters |> filter(likelihood_same_observed_values) |>
 select(variant_id,offset_sd,shared_offset_sd,shared_offset_mean,official_ar_sd,negative_log_likelihood)) |>
 mutate(delta_nll_vs_primary=negative_log_likelihood-primary$optim$value)
v2_write(ll,"comparable_likelihoods.csv")
cat("Merged",nrow(s),"variants.\n")

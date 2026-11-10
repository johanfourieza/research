source(here::here("replication","R","helpers.R"))
data_dir <- file.path(here::here("data","series"))
a <- v2_read("annual_inflation_1700_1834.csv")
write_csv(a,file.path(data_dir,"annual_inflation_1700_1834.csv"))
d <- readRDS(file.path(data_dir,"annual_index_joint_draws.rds"))
joint <- tibble(year=rep(d$year,ncol(d$index)),draw_id=rep(d$draw_id,each=length(d$year)),index=as.vector(d$index))
# Keep the minimal primary schema stable for ordinary pivot/deflation code.
write_csv(joint,file.path(data_dir,"annual_index_joint_draws.csv.gz"))
write_csv(v2_read("elementary_annual_state.csv"),file.path(data_dir,"commodity_price_summaries.csv"))
write_csv(v2_read("basket_quantities_weights.csv"),file.path(data_dir,"basket_quantities_weights.csv"))
write_csv(v2_read("historical_functionals_primary.csv"),file.path(data_dir,"historical_relatives.csv"))
write_csv(v2_read("peak_decomposition_summary.csv"),file.path(data_dir,"peak_timing_diagnostics.csv"))
dec <- joint |> mutate(decade=year%/%10L*10L) |> group_by(draw_id,decade) |>
 summarise(mean_index=mean(index),.groups="drop") |> group_by(decade) |>
 summarise(q05=quantile(mean_index,.05),median=median(mean_index),q95=quantile(mean_index,.95),.groups="drop")
write_csv(dec,file.path(data_dir,"decadal_index_joint_intervals.csv"))
write_csv(tibble(variant_id="v2_nominal_basic9_1795_offsets",base_period="arithmetic_mean_1770_1779_equals100",
 years=135L,draws=1000L,seed=d$seed,draw_order="year_varies_fastest_within_draw",
 scope=d$scope,point_estimator="sum_quantities_times_exp_smoothed_log_price_then_normalise",
 quantile_estimator="within_draw_basket_normalisation_then_quantiles",
 peak_tie_rule="first_chronological_year",primary_offset_sd=.5,official_split_year=1795),file.path(data_dir,"joint_draw_metadata.csv"))
readback <- read_csv(file.path(data_dir,"annual_index_joint_draws.csv.gz"),show_col_types=FALSE)
stopifnot(max(abs(readback$index-joint$index))<1e-10,nrow(readback)==135000,
 max(abs(colMeans(d$index[between(d$year,1770,1779),])-100))<1e-8)
stats <- apply(d$index,1,quantile,c(.05,.5,.95))
stopifnot(max(abs(stats[1,]-a$index_q05))<1e-8,max(abs(stats[3,]-a$index_q95))<1e-8)
if(file.exists(file.path(v2_out,"annual_variant_conditional_summaries.csv"))) {
 write_csv(v2_read("annual_variant_conditional_summaries.csv"),file.path(data_dir,"alternative_annual_series.csv"))
 write_csv(v2_read("v2_variant_settings.csv"),file.path(data_dir,"alternative_model_settings.csv"))

}
v2_write(tibble(check=c("portable_draw_roundtrip","135000_primary_draw_rows","all_draw_bases100","summary_quantiles_reconcile"),passed=TRUE),"data_export_checks.csv")
cat("Portable primary and available alternative data exported; joint covariance retained.\n")

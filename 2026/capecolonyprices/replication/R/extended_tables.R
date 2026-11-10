source(here::here("replication","R","helpers.R"))
tab <- file.path(v2_root,"generated_tables")
fmt <- function(x,digits=2) ifelse(is.finite(x),formatC(x,format="f",digits=digits),"--")
rows <- function(x,name) writeLines(apply(as.data.frame(x),1,function(z) paste0(paste(z,collapse=" & ")," \\\\")),file.path(tab,name))
label <- function(id) paste0("\\variantlabel{",id,"}")
a <- v2_read("annual_inflation_1700_1834.csv")
rows(a |> transmute(year,index=fmt(index_point),interval=paste0("[",fmt(index_q05),", ",fmt(index_q95),"]"),
 inflation=fmt(inflation_pct),cost=fmt(annual_cost_rd),dated=fmt(observed_level_weight_share,3),
 market=fmt(market_change_weight_share,3),flat=fmt(flat_official_change_weight_share,3),
 support=paste0("\\path{",support_class,"}")),"annual_full_rows.tex")
h <- v2_read("historical_functionals_all.csv")
rows(filter(h,variant_id=="primary") |> transmute(functional=paste0("\\functionallabel{",functional,"}"),
 point=fmt(point),interval=paste0("[",fmt(q05),", ",fmt(q95),"]"),prob_gt1=fmt(prob_gt1,3),prob_gt3=fmt(prob_gt3,3)),"historical_relative_rows.tex")
selected <- c("primary","offsets_constant","source_prior_025","shared_post_offset050","shared_new_columns050",
 "post_source_mean_plus50","post_source_mean_minus50","independent_commodity_paths","official_ar030",
 "sterling_quotes_excluded","sterling_fixed0200","wheat1787_excluded")
peak <- v2_read("peak_timing_all.csv")
joint <- filter(h,functional=="ratio_1822_to1795",variant_id%in%selected) |>
 left_join(peak,by="variant_id",suffix=c("","_peak")) |> arrange(match(variant_id,selected))
rows(joint |> transmute(variant=label(variant_id),relative=fmt(point),interval=paste0("[",fmt(q05),", ",fmt(q95),"]"),
 prob_gt3=fmt(prob_gt3,3),peak_interval=paste0(peak_q05,"--",peak_q95),
 prob_peak_before1825=fmt(prob_peak_before1825,3)),"joint_inference_variant_rows.tex")
v2_write(joint,"paper_table_joint_inference.csv")
pars <- v2_read("structural_variant_parameters.csv")
primary <- readRDS(file.path(v2_out,"annual_state_model.rds"))
likelihood <- bind_rows(tibble(variant_id="primary",offset_sd=.5,shared_offset_sd=0,shared_offset_mean=0,official_ar_sd=0,
 negative_log_likelihood=primary$optim$value),
 pars |> filter(likelihood_same_observed_values) |> select(variant_id,offset_sd,shared_offset_sd,shared_offset_mean,official_ar_sd,negative_log_likelihood))
v2_write(likelihood,"paper_table_comparable_likelihoods.csv")
rows(likelihood |> transmute(variant=label(variant_id),tau=fmt(offset_sd),gamma=fmt(shared_offset_sd),
 delta=fmt(shared_offset_mean,3),ar_sd=fmt(official_ar_sd),nll=fmt(negative_log_likelihood)),"likelihood_rows.tex")
bridges <- v2_read("direct_1795_bridges.csv")
omission <- v2_read("bridge_omission_functionals.csv") |> filter(functional=="ratio_1822_to1795") |>
 mutate(bridge_id=as.integer(str_remove(variant_id,"omit_bridge_")))
br <- left_join(bridges,omission,by="bridge_id")
v2_write(br,"paper_table_bridges.csv")
rows(br |> transmute(bridge_id,commodity,last_pre_year,first_post_year,relative=fmt(price_relative),
 fitted_relative_omitting=fmt(point),interval=paste0("[",fmt(q05),", ",fmt(q95),"]")),"bridge_rows.tex")
main_ids <- c("offsets_constant","source_prior_025","shared_post_offset050","shared_new_columns050",
 "post_source_mean_plus50","post_source_mean_minus50","independent_commodity_paths","official_ar030",
 "sterling_quotes_excluded","wheat1787_excluded")
vt <- v2_read("paper_table_structural_variants.csv") |> filter(variant_id%in%main_ids) |> arrange(match(variant_id,main_ids))
rows(vt |> mutate(variant_id=label(variant_id),across(where(is.numeric),~fmt(.x,1))),"structural_variant_main_rows.tex")
write_csv(tibble(file=list.files(tab,pattern="\\.tex$"),generation_scope="v2_data_tables_not_prose"),file.path(tab,"table_manifest.csv"))
cat("Extended numeric tables generated.\n")

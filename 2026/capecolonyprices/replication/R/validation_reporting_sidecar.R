source(here::here("replication","R","helpers.R"))
base <- file.path(v2_out,"validation","preferred1795");out <- file.path(base,"reporting")
dir.create(out,showWarnings=FALSE)
read <- function(name) read_csv(file.path(base,name),show_col_types=FALSE)
write <- function(x,name) write_csv(x,file.path(out,name))
stopifnot(all(read("fit_status.csv")$status=="success"))
d <- readRDS(file.path(base,"design.rds"));metadata <- read("fold_metadata.csv")
metadata <- metadata |> mutate(block_length=if_else(fold_type=="portfolio_block",as.integer(str_extract(fold_id,"[0-9]+$")),NA_integer_),
 reporting_family=case_when(fold_type=="portfolio_block"~paste0("portfolio_block_L",block_length),
   fold_type=="source_overlap_component"&size_rank==1L~"component_rank1_prior_diagnostic",
   fold_type=="source_overlap_component"&size_rank>=2L~"component_rank_ge2",
   fold_type=="transported_component_union"~"transported_union_prior_diagnostic",
   fold_type=="portfolio_actual_pattern"~"transferred_sparse_pattern",
   fold_type=="single_channel_partial"~"partial_channel_splice",TRUE~fold_type))
write(metadata,"reporting_fold_families.csv")
# Flat-run strata use actual source data only as evaluation labels. They never
# enter a fit, predictor, donor selection or holdout selection. Consecutive means
# consecutive calendar years, with equal log quotes to numerical tolerance1e-10.
flat <- d$panel |> group_by(model_source_id) |> arrange(year,.by_group=TRUE) |>
 mutate(same_previous=replace_na(year-lag(year)==1L&abs(log_price-lag(log_price))<1e-10,FALSE),
   same_next=replace_na(lead(year)-year==1L&abs(lead(log_price)-log_price)<1e-10,FALSE),
   flat_run_position=case_when(same_previous&same_next~"run_interior",same_previous|same_next~"run_edge",TRUE~"not_in_consecutive_flat_run")) |> ungroup() |>
 select(row_id,flat_run_position,same_previous,same_next)
write(flat,"actual_flat_run_evaluation_labels.csv")
position <- read("baseline_support_positions.csv") |> filter(method=="within_channel") |> select(fold_id,row_id,within_support=support)
p <- read("quote_predictions.csv") |> left_join(metadata |> select(fold_id,reporting_family,block_length,size_rank),by="fold_id") |>
 left_join(flat,by="row_id") |> left_join(position,by=c("fold_id","row_id"))
unique_cases <- p |> filter(method=="full_common_drift") |> distinct(fold_id,row_id,reporting_family)
frequency <- unique_cases |> count(row_id,name="n_fold_appearances")
write(frequency,"cell_multiplicity.csv")
write(tibble(n_fold_cases=nrow(unique_cases),n_unique_cells=nrow(frequency),maximum_multiplicity=max(frequency$n_fold_appearances),n_at_maximum=sum(frequency$n_fold_appearances==max(frequency$n_fold_appearances))),"case_count_audit.csv")
write(unique_cases |> count(reporting_family,row_id,name="within_family_multiplicity"),"within_family_cell_multiplicity.csv")
comparisons <- tribble(~model,~baseline,
 "full_common_drift","independent_drift","full_common_drift","pooled_loglinear","full_common_drift","within_channel","full_common_drift","overlap_splice",
 "independent_drift","pooled_loglinear","independent_drift","within_channel","independent_drift","overlap_splice")
paired <- map_dfr(seq_len(nrow(comparisons)),function(i) {
 m <- comparisons$model[i];b <- comparisons$baseline[i]
 a <- p |> filter(method==m,is.finite(error)) |> select(fold_id,row_id,reporting_family,block_length,size_rank,noise_role,source_role,flat_run_position,within_support,model_error=error,model_covered90=covered90)
 bb <- p |> filter(method==b,is.finite(error)) |> select(fold_id,row_id,baseline_error=error)
 inner_join(a,bb,by=c("fold_id","row_id")) |> mutate(model=m,baseline=b)
})
cases <- bind_rows(paired |> mutate(support_scope="all_pairwise_eligible"),paired |> filter(within_support=="interpolation") |> mutate(support_scope="within_channel_interpolation_only"))
write(cases,"matched_pairwise_quote_cases.csv")
fold_scores <- function(z,extra=character()) {
 groups <- c("support_scope","reporting_family","model","baseline",extra,"fold_id")
 z |> group_by(across(all_of(groups))) |> summarise(n_cases=n(),rmse_model=sqrt(mean(model_error^2)),rmse_baseline=sqrt(mean(baseline_error^2)),
   difference=rmse_model-rmse_baseline,model_predictive_coverage=mean(model_covered90),.groups="drop")
}
fold_equal <- function(z,extra=character()) z |> group_by(across(all_of(c("support_scope","reporting_family","model","baseline",extra)))) |>
 summarise(n_folds=n(),n_prediction_cases=sum(n_cases),mean_fold_rmse_difference=mean(difference),median_fold_rmse_difference=median(difference),
  share_folds_model_better=mean(difference< -1e-12),share_folds_tied=mean(abs(difference)<=1e-12),fold_equal_model_rmse=mean(rmse_model),fold_equal_baseline_rmse=mean(rmse_baseline),
  fold_equal_predictive_coverage=mean(model_predictive_coverage),.groups="drop")
weighted <- function(z,extra=character()) {
 groups <- c("support_scope","reporting_family","model","baseline",extra)
 # Compute multiplicity on the SAME pairwise eligible support. A cell's weights
 # sum to one inside each reporting family/stratum/method comparison.
 z |> group_by(across(all_of(c(groups,"row_id")))) |> mutate(cell_weight=1/n()) |> ungroup() |>
  group_by(across(all_of(groups))) |> summarise(n_folds=n_distinct(fold_id),n_prediction_cases=n(),n_unique_cells=n_distinct(row_id),sum_weights=sum(cell_weight),
    rmse_model=sqrt(weighted.mean(model_error^2,cell_weight)),rmse_baseline=sqrt(weighted.mean(baseline_error^2,cell_weight)),
    rmse_difference=rmse_model-rmse_baseline,predictive_90_coverage=weighted.mean(model_covered90,cell_weight),.groups="drop")
}
ff <- fold_scores(cases);write(ff,"quote_fold_rmse_differences.csv")
write(fold_equal(ff),"quote_fold_equal_comparisons.csv")
ww <- weighted(cases);write(ww,"quote_inverse_multiplicity_scores.csv")
for(strata in c("noise_role","flat_run_position","source_role")) {
 s <- fold_scores(cases,strata);write(fold_equal(s,strata),paste0("quote_fold_equal_by_",strata,".csv"))
 write(weighted(cases,strata),paste0("quote_inverse_multiplicity_by_",strata,".csv"))
}
write(weighted(cases,c("noise_role","flat_run_position")),"quote_inverse_multiplicity_by_role_and_flat_position.csv")
write(p |> filter(method=="full_common_drift") |> count(reporting_family,noise_role,flat_run_position),"native_role_flat_case_counts.csv")
splices <- read("overlap_splice_selected_design.csv") |> rowwise() |> mutate(unordered_pair=paste(sort(c(target,donor)),collapse=" <-> ")) |> ungroup()
write(splices,"splice_folds_with_unordered_pairs.csv")
write(splices |> group_by(commodity,unordered_pair) |> summarise(n_directional_folds=n(),n_quote_cases=5L*n(),folds=paste(fold_id,collapse=";"),.groups="drop"),"splice_pair_summary.csv")
baskets <- read("subset_basket_eligibility.csv") |> mutate(includes_wheat=str_detect(paste0("; ",coalesce(goods,""),";"),"; Wheat;"),
 meets_composition_rule=(n_goods>=3L&includes_wheat)|reference_cost_coverage>=.3,
 reporting_eligible=status=="eligible"&meets_composition_rule,
 reporting_reason=case_when(status!="eligible"~status,!meets_composition_rule~"fails_three_goods_with_wheat_or_30pct_cost_rule",TRUE~"eligible"))
write(baskets,"subset_basket_reporting_eligibility.csv")
write(read("noisy_subset_basket_log_relatives.csv") |> semi_join(baskets |> filter(reporting_eligible),by="fold_id"),"reported_noisy_subset_basket_relatives.csv")
template <- read("actual_late_missingness_template.csv")
present <- template |> filter(!missing)
realised <- map_dfr(c(-80L,-50L,-30L),function(shift) {
 target <- present |> transmute(commodity,year=year+shift)
 hit <- target |> semi_join(d$panel |> distinct(commodity,year),by=c("commodity","year"))
 tibble(fold_id=paste0("actual_missingness_shift",shift),n_original_pattern_present_good_years=nrow(present),n_realised_retained_good_years=nrow(hit),retained_present_share=nrow(hit)/nrow(present))
}) |> left_join(read("transferred_missingness_realisability.csv"),by="fold_id")
write(realised,"transferred_mask_retained_and_absent_shares.csv")
checks <- tibble(check=c("1745_cases_615_unique_cells","weights_sum_to_unique_cells","seven_splice_folds_four_unordered_pairs","all_eight_previous_baskets_pass_stricter_reporting","45_original_present_pattern_cells","matched_pair_keys_unique"),
 pass=c(nrow(unique_cases)==1745L&&nrow(frequency)==615L,all(abs(ww$sum_weights-ww$n_unique_cells)<1e-10),nrow(splices)==7L&&n_distinct(splices$unordered_pair)==4L,
 sum(baskets$reporting_eligible)==8L&&all(baskets$meets_composition_rule[baskets$status=="eligible"]),all(realised$n_original_pattern_present_good_years==45L),!anyDuplicated(cases[c("support_scope","fold_id","row_id","model","baseline")])) )
write(checks,"reporting_checks.csv");stopifnot(all(checks$pass))
writeLines(c("REPORTING SIDECAR: NO NEW FITS OR HOLDOUT RESELECTION",
 "Primary reporting: quote_fold_equal_comparisons.csv and quote_inverse_multiplicity_scores.csv, separated by reporting_family and support_scope.",
 "Negative RMSE difference favours the named model. Share_folds_model_better treats folds equally. No independence, confidence interval or significance claim is attached.",
 "1745 prediction cases reuse615 cells, some up to6 times. Original pooled scores are diagnostics only and must not be a headline.",
 "Portfolio block lengths are separate. component_rank_ge2 is the smaller-component comparison; rank1 and transported unions are prior-sensitive diagnostics.",
 "within_channel_interpolation_only excludes constant endpoint carries and single-year constants for every method comparison on that support.",
 "Inverse multiplicity weights are1/(eligible appearances of that cell within that reporting family and comparison), so every unique cell contributes total weight1.",
 "Official/other/auction and flat-run strata describe evaluation cases. Flat interiors require equal actual quotes in immediately preceding and following calendar years; they are not predictor inputs.",
 "Seven splice folds and35 cases comprise four unordered source pairs. Reciprocal directions and shared years do not create independent evidence.",
 "The stricter basket composition rule removes none of the eight already calculated subsets; no newly eligible or invented basket has been added.",
 "Only21/45,30/45,29/45 pattern-present good-years exist in the shifted earlier windows. Sparse-coverage tests are not replicas of1795 source transitions."),file.path(out,"REPORTING_README.txt"))
print(checks)

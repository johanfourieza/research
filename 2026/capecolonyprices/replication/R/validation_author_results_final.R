source(here::here("replication","R","helpers.R"))
base <- file.path(v2_out,"validation","preferred1795");out <- file.path(base,"reporting")
read <- function(name,dir=out) read_csv(file.path(dir,name),show_col_types=FALSE)
checks <- read("validation_checks.csv",base);rc <- read("reporting_checks.csv")
stopifnot(all(checks$pass),all(rc$pass))
ff <- read("quote_fold_equal_comparisons.csv");ww <- read("quote_inverse_multiplicity_scores.csv")
tables <- ff |> filter(model=="full_common_drift") |>
 select(support_scope,reporting_family,model,baseline,n_folds,n_prediction_cases,mean_fold_rmse_difference,share_folds_model_better) |>
 left_join(ww |> select(support_scope,reporting_family,model,baseline,n_unique_cells,weighted_rmse_model=rmse_model,weighted_rmse_baseline=rmse_baseline,predictive_90_coverage),
   by=c("support_scope","reporting_family","model","baseline"))
main <- tables |> filter(support_scope=="all_pairwise_eligible",!str_detect(reporting_family,"prior_diagnostic")) |> select(-support_scope,-model)
diagnostic <- tables |> filter(support_scope=="all_pairwise_eligible",str_detect(reporting_family,"prior_diagnostic")) |> select(-support_scope,-model)
interpolation <- tables |> filter(support_scope=="within_channel_interpolation_only",baseline=="within_channel") |> select(-support_scope,-model)
write_csv(main,file.path(out,"AUTHOR_table_main_validation.csv"))
write_csv(diagnostic,file.path(out,"AUTHOR_table_prior_diagnostics.csv"))
write_csv(interpolation,file.path(out,"AUTHOR_table_interpolation_only.csv"))
roles <- read("quote_inverse_multiplicity_by_role_and_flat_position.csv") |> filter(support_scope=="all_pairwise_eligible",model=="full_common_drift",baseline=="within_channel",reporting_family=="transferred_sparse_pattern") |>
 select(noise_role,flat_run_position,n_folds,n_prediction_cases,n_unique_cells,rmse_model,rmse_baseline,predictive_90_coverage)
write_csv(roles,file.path(out,"AUTHOR_table_transferred_role_flat_strata.csv"))

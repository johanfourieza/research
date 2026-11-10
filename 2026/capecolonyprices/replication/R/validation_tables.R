source(here::here("replication","R","helpers.R"))
vd <- file.path(v2_out,"validation","preferred1795");rd <- file.path(vd,"reporting")
tab <- file.path(v2_root,"generated_tables")
read <- function(x) read_csv(file.path(rd,x),show_col_types=FALSE)
fmt <- function(x,digits=3) ifelse(is.finite(x),formatC(x,format="f",digits=digits),"--")
rows <- function(x,name) writeLines(apply(as.data.frame(x),1,function(z) paste0(paste(z,collapse=" & ")," \\\\")),file.path(tab,name))
lab <- function(x) paste0("\\validationlabel{",x,"}")
all <- read("AUTHOR_table_main_validation.csv")
selected <- all |> filter((str_starts(reporting_family,"portfolio_block_")&baseline=="independent_drift")|
 (reporting_family=="transferred_sparse_pattern"&baseline%in%c("independent_drift","pooled_loglinear"))|
 (reporting_family=="component_rank_ge2"&baseline=="independent_drift")|
 (reporting_family=="partial_channel_splice"&baseline=="overlap_splice"))
interpolation <- read("AUTHOR_table_interpolation_only.csv") |>
 filter(reporting_family=="transferred_sparse_pattern") |>
 mutate(reporting_family="transferred_sparse_pattern_interpolation")
selected <- bind_rows(selected,interpolation) |> arrange(match(reporting_family,c("portfolio_block_L1","portfolio_block_L5",
 "portfolio_block_L10","transferred_sparse_pattern","transferred_sparse_pattern_interpolation","component_rank_ge2","partial_channel_splice")),baseline)
format_table <- function(x) x |> transmute(family=lab(reporting_family),baseline=lab(baseline),n_folds,
 cases_unique=paste(n_prediction_cases,n_unique_cells,sep=" / "),
 mean_fold_difference=fmt(mean_fold_rmse_difference),share_better=fmt(share_folds_model_better,2),
 weighted_model=fmt(weighted_rmse_model),weighted_baseline=fmt(weighted_rmse_baseline),coverage=fmt(predictive_90_coverage,2))
rows(format_table(selected),"validation_main_v2_rows.tex")
rows(selected |> transmute(family=lab(reporting_family),baseline=lab(baseline),n_folds,n_unique_cells,
 mean_fold_difference=fmt(mean_fold_rmse_difference),share_better=fmt(share_folds_model_better,2)),"validation_main_compact_rows.tex")
rows(format_table(all),"validation_complete_v2_rows.tex")
rows(format_table(read("AUTHOR_table_prior_diagnostics.csv")),"validation_prior_diagnostic_rows.tex")
rows(format_table(read("AUTHOR_table_interpolation_only.csv")),"validation_interpolation_rows.tex")
strata <- read("AUTHOR_table_transferred_role_flat_strata.csv")
rows(strata |> transmute(role=lab(noise_role),position=lab(flat_run_position),n_folds,
 cases_unique=paste(n_prediction_cases,n_unique_cells,sep=" / "),
 model=fmt(rmse_model),baseline=fmt(rmse_baseline),coverage=fmt(predictive_90_coverage,2)),"validation_role_flat_rows.tex")
v2_write(selected,"paper_table_validation_v2_main.csv")
checks <- read_csv(file.path(vd,"validation_checks.csv"),show_col_types=FALSE)
report_checks <- read("reporting_checks.csv")
stopifnot(nrow(selected)==8L,nrow(checks)==15L,nrow(report_checks)==6L,
 all(checks$pass),all(report_checks$pass))
cat("Preferred validation table bodies generated from frozen sidecars.\n")

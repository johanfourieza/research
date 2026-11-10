source(here::here("replication","R","helpers.R"))
run_name <- Sys.getenv("CAPE_VALIDATION_RUN","constant_offsets")
out <- if(run_name=="constant_offsets") file.path(v2_out,"validation") else file.path(v2_out,"validation",run_name)
d <- readRDS(file.path(out,"design.rds"))
read <- function(name) read_csv(file.path(out,name),show_col_types=FALSE)
status <- read("fit_status.csv");fd <- read("fit_diagnostics.csv");p <- read("quote_predictions.csv")
checks <- list()
check <- function(id,pass,detail="") checks[[length(checks)+1L]] <<- tibble(check=id,pass=isTRUE(pass),detail=detail)
check("all_expected_fits_complete",nrow(status)==2L*length(d$folds)&&all(status$status=="success"))
check("prediction_keys_unique",!anyDuplicated(p[c("fold_id","row_id","method")]))
check("all_optimizers_converged",nrow(fd)==2L*length(d$folds)&&all(fd$convergence==0))
check("shared_noise_assumptions",all(fd$offset_sd==.5&fd$shared_offset_sd==0&fd$official_ar_sd==0))
check("all_masks_unique",length(unique(vapply(d$folds,paste,collapse=",",FUN.VALUE=character(1))))==length(d$folds))
check("all_training_portfolios_retain_all_goods",all(vapply(d$folds,function(ids) n_distinct(d$panel$commodity[!d$panel$row_id%in%ids])==9L,logical(1))))
state_ids <- p |> filter(method%in%c("full_common_drift","independent_drift")) |> group_by(fold_id,row_id) |> summarise(n_methods=n_distinct(method),.groups="drop")
check("common_independent_have_identical_test_ids",nrow(state_ids)==sum(lengths(d$folds))&&all(state_ids$n_methods==2L))
check("all_computed_quotes_finite",all(is.finite(p$prediction[p$method%in%c("full_common_drift","independent_drift","pooled_loglinear")])) )
check("state_predictive_intervals_positive",all(p$q95[p$method%in%c("full_common_drift","independent_drift")]>p$q05[p$method%in%c("full_common_drift","independent_drift")]))
component <- p |> filter(fold_type=="source_overlap_component",method=="within_channel")
check("held_out_component_has_no_within_channel_baseline",all(is.na(component$prediction)))
scores <- read("quote_scores_matched.csv")
check("matched_support_equal_within_comparison",all((scores |> group_by(comparison_set,fold_type) |> summarise(n_n=n_distinct(n),.groups="drop"))$n_n==1L))
membership <- read("subset_basket_membership.csv")
check("subset_basket_source_is_same_at_endpoints",all(vapply(seq_len(nrow(membership)),function(i) {
 z <- membership[i,];test <- d$panel |> filter(row_id%in%d$folds[[z$fold_id]],model_source_id==z$model_source_id,year%in%c(z$year_from,z$year_to));nrow(test)==2L
},logical(1))))
# Reconstruct every baseline independently from training rows only.
baseline_errors <- numeric()
for(fid in names(d$folds)) {
 tr <- d$panel |> filter(!row_id%in%d$folds[[fid]])
 for(method in c("within_channel","pooled_loglinear")) {
  z <- p |> filter(fold_id==fid,.data$method==.env$method);key <- if(method=="within_channel") "model_source_id" else "commodity"
  for(i in seq_len(nrow(z))) {
   a <- tr[tr[[key]]==z[[key]][i],] |> group_by(year) |> summarise(lp=mean(log_price),.groups="drop")
   expected <- if(!nrow(a)) NA_real_ else if(nrow(a)==1L) a$lp else approx(a$year,a$lp,xout=z$year[i],rule=2)$y
   baseline_errors <- c(baseline_errors,if(is.na(expected)&&is.na(z$prediction[i])) 0 else abs(expected-z$prediction[i]))
  }
 }
}
check("all_baselines_reconstruct_from_training_only",all(is.finite(baseline_errors))&&max(baseline_errors)<1e-10)
if(!is.null(d$splice_donors)&&nrow(d$splice_donors)) {
 errors <- numeric()
 for(i in seq_len(nrow(d$splice_donors))) {
  s <- d$splice_donors[i,];tr <- d$panel |> filter(!row_id%in%d$folds[[s$fold_id]])
  a <- tr |> filter(model_source_id==s$target) |> select(year,a=log_price)
  b <- tr |> filter(model_source_id==s$donor) |> select(year,b=log_price)
  ov <- inner_join(a,b,by="year");z <- p |> filter(fold_id==s$fold_id,method=="overlap_splice")
  expected <- b$b[match(z$year,b$year)]+median(ov$a-ov$b)
  errors <- c(errors,abs(expected-z$prediction))
 }
 check("actual_splice_reconstructs_from_training_overlap",length(errors)==5L*nrow(d$splice_donors)&&all(is.finite(errors))&&max(errors)<1e-10)
 paired <- read("paired_mask_integrity.csv")
 check("constant_and_preferred_masks_identical",all(paired$identical_observation_ids))
}
record <- bind_rows(checks);write_csv(record,file.path(out,"validation_checks.csv"));print(record)
code_paths <- c(file.path(here::here("replication","R"),c("helpers.R","state_model.R","validation_design.R","validation_preferred_design.R","validation_splice_design.R","validation_run.R","validation_summarise.R","validation_design_diagnostics.R","validation_compare_designs.R","validation_verify.R")),file.path(out,c("primary_snapshot.rds","state_model_snapshot.R","helpers_snapshot.R")))
write_csv(tibble(path=code_paths,md5=unname(tools::md5sum(code_paths))),file.path(out,"validation_input_code_manifest.csv"))
capture.output(sessionInfo(),file=file.path(out,"validation_R_session.txt"))
stopifnot(all(record$pass))

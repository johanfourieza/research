source(here::here("replication","R","helpers.R"))
run_name <- Sys.getenv("CAPE_VALIDATION_RUN","constant_offsets")
out <- if(run_name=="constant_offsets") file.path(v2_out,"validation") else file.path(v2_out,"validation",run_name)
design <- readRDS(file.path(out,"design.rds"))
component_sizes <- read_csv(file.path(out,"source_overlap_component_sizes.csv"),show_col_types=FALSE)
fold_metadata <- if("component_id"%in%names(design$ledger)) design$ledger else design$ledger |>
  mutate(component_id=if_else(fold_type=="source_overlap_component",str_remove(fold_id,"^component_"),NA_character_)) |>
  left_join(component_sizes |> select(component_id,size_rank,n_rows),by="component_id")
fold_metadata <- fold_metadata |> mutate(is_largest_component=if_else(fold_type=="source_overlap_component",size_rank==1L,NA))
write_csv(fold_metadata,file.path(out,"fold_metadata.csv"))
files <- list.files(file.path(out,"checkpoints"),pattern="[.]rds$",full.names=TRUE)
results <- lapply(files,readRDS)
stopifnot(all(vapply(results,function(x) identical(x$primary_md5,design$primary_md5),logical(1))))
success <- Filter(function(x) x$status=="success",results)
success <- lapply(success,function(x) {
  if("cross_channel_splice"%in%names(x$predictions)) x$predictions <- x$predictions |> rename(pooled_loglinear=cross_channel_splice)
  if(!"overlap_splice"%in%names(x$predictions)) x$predictions$overlap_splice <- NA_real_
  x
})
status <- map_dfr(results,function(x) tibble(fold_id=x$fold_id,method=x$method,status=x$status,attempts=x$attempts,error=paste(x$errors,collapse=" | ")))
if(!nrow(status)) status <- tibble(fold_id=character(),method=character(),status=character(),attempts=integer(),error=character())
expected <- expand_grid(fold_id=names(design$folds),method=c("full_common_drift","independent_drift"))
status <- expected |> left_join(status,by=c("fold_id","method")) |> mutate(status=replace_na(status,"pending"))
write_csv(status,file.path(out,"fit_status.csv"))
diag <- map_dfr(success,"fit_diagnostics");write_csv(diag,file.path(out,"fit_diagnostics.csv"))
if(!length(success)) quit(status=0)
bounds <- tibble(parameter=c("common_sd","relative_sd","wheat_sd","auction_noise_sd","official_noise_sd","other_noise_sd","wheat_transitory_sd"),
  lower=c(.001,.005,.005,.01,.01,.01,.005),upper=c(.8,.8,1.5,1.5,1.5,1.5,1.5))
bound_report <- diag |> select(fold_id,method,all_of(bounds$parameter)) |> pivot_longer(all_of(bounds$parameter),names_to="parameter",values_to="estimate") |>
  left_join(bounds,by="parameter") |> mutate(unused_parameter=method=="independent_drift"&parameter=="common_sd",
    at_lower=!unused_parameter&abs(log(estimate/lower))<1e-5,at_upper=!unused_parameter&abs(log(estimate/upper))<1e-5)
write_csv(bound_report,file.path(out,"fit_parameter_bounds.csv"))
allpred <- map_dfr(success,function(x) x$predictions |> mutate(fold_id=x$fold_id))
baseline_columns <- c("within_channel","pooled_loglinear",if(!is.null(design$splice_donors)&&nrow(design$splice_donors)) "overlap_splice")
baselines <- allpred |> distinct(fold_id,row_id,.keep_all=TRUE) |>
  select(-method,-prediction,-predictive_sd,-q05,-q95) |>
  select(-any_of(setdiff(c("within_channel","pooled_loglinear","overlap_splice"),baseline_columns))) |>
  pivot_longer(all_of(baseline_columns),names_to="method",values_to="prediction") |>
  mutate(predictive_sd=NA_real_,q05=NA_real_,q95=NA_real_)
pred <- bind_rows(allpred |> select(-within_channel,-pooled_loglinear,-overlap_splice),baselines) |>
  left_join(design$ledger |> select(fold_id,fold_type),by="fold_id") |>
  mutate(error=prediction-log_price,covered90=if_else(is.finite(q05),log_price>=q05&log_price<=q95,NA))
write_csv(pred,file.path(out,"quote_predictions.csv"))
method_sets <- list(state_models=c("full_common_drift","independent_drift"),states_and_pooled=c("full_common_drift","independent_drift","pooled_loglinear"),all_four=c("full_common_drift","independent_drift","pooled_loglinear","within_channel"))
if("overlap_splice"%in%baseline_columns) method_sets <- c(method_sets,list(states_and_overlap=c("full_common_drift","independent_drift","overlap_splice"),all_five=c("full_common_drift","independent_drift","pooled_loglinear","within_channel","overlap_splice")))
matched <- function(z,keys) {
  imap_dfr(method_sets,function(methods,label) {
    selected <- z |> filter(method%in%methods,is.finite(error))
    ids <- selected |> group_by(across(all_of(keys))) |> summarise(n=n_distinct(method),.groups="drop") |> filter(n==length(methods)) |> select(-n)
    selected |> semi_join(ids,by=keys) |> mutate(comparison_set=label)
  })
}
support <- pred |> group_by(fold_type,method) |> summarise(n_scheduled=n(),n_eligible=sum(is.finite(prediction)),n_unmatched=sum(!is.finite(prediction)),.groups="drop")
write_csv(support,file.path(out,"quote_method_support.csv"))
score <- function(z,group) z |> group_by(across(all_of(group))) |>
  summarise(n=n(),n_folds=n_distinct(fold_id),rmse=sqrt(mean(error^2)),mae=mean(abs(error)),bias=mean(error),
    predictive_90_coverage=if(all(is.na(covered90))) NA_real_ else mean(covered90,na.rm=TRUE),
    mean_predictive_interval_width=if(all(is.na(q05))) NA_real_ else mean(q95-q05,na.rm=TRUE),.groups="drop")
quote_matched <- matched(pred,c("fold_id","row_id"))
write_csv(score(quote_matched,c("comparison_set","fold_type","method")),file.path(out,"quote_scores_matched.csv"))
write_csv(score(quote_matched,c("comparison_set","method")),file.path(out,"quote_scores_pooled_matched.csv"))
write_csv(score(quote_matched,c("comparison_set","fold_id","fold_type","method")),file.path(out,"quote_scores_by_fold.csv"))
write_csv(score(quote_matched |> left_join(fold_metadata |> select(fold_id,size_rank),by="fold_id") |> filter(fold_type=="source_overlap_component"),c("comparison_set","size_rank","method")),file.path(out,"quote_component_scores_by_size_rank.csv"))
write_csv(score(quote_matched,c("comparison_set","commodity","method")),file.path(out,"quote_scores_by_commodity.csv"))
write_csv(score(pred |> filter(is.finite(error)),c("fold_type","method")),file.path(out,"quote_scores_native_support.csv"))
# Paired source-channel differences cancel its constant offset algebraically.
# Predictive intervals retain two independent observation errors, plus the
# joint smoothed-state covariance; they do not assess latent-price calibration.
diffs <- map_dfr(success,function(x) {
  p <- x$predictions
  map_dfr(sort(unique(p$model_source_id)),function(ch) {
    ix <- which(p$model_source_id==ch);a <- ix[which.min(p$year[ix])];b <- ix[which.max(p$year[ix])]
    if(p$year[b]-p$year[a]<3) return(tibble())
    d <- x$joint_quote[b,]-x$joint_quote[a,];q <- quantile(d,c(.05,.95),names=FALSE)
    tibble(fold_id=x$fold_id,method=x$method,commodity=p$commodity[a],model_source_id=ch,year_from=p$year[a],year_to=p$year[b],
      observed=p$log_price[b]-p$log_price[a],prediction=p$prediction[b]-p$prediction[a],q05=q[1],q95=q[2],
      within_channel=p$within_channel[b]-p$within_channel[a],pooled_loglinear=p$pooled_loglinear[b]-p$pooled_loglinear[a],overlap_splice=p$overlap_splice[b]-p$overlap_splice[a])
  })
})
if(nrow(diffs)) {
 db <- diffs |> distinct(fold_id,model_source_id,.keep_all=TRUE) |> select(-method,-prediction,-q05,-q95) |>
   select(-any_of(setdiff(c("within_channel","pooled_loglinear","overlap_splice"),baseline_columns))) |>
   pivot_longer(all_of(baseline_columns),names_to="method",values_to="prediction") |> mutate(q05=NA_real_,q95=NA_real_)
 diffs <- bind_rows(diffs |> select(-within_channel,-pooled_loglinear,-overlap_splice),db) |>
   left_join(design$ledger |> select(fold_id,fold_type),by="fold_id") |> mutate(error=prediction-observed,covered90=if_else(is.finite(q05),observed>=q05&observed<=q95,NA))
 write_csv(diffs,file.path(out,"within_channel_long_differences.csv"))
 write_csv(diffs |> group_by(fold_type,method) |> summarise(n_scheduled=n(),n_eligible=sum(is.finite(prediction)),n_unmatched=sum(!is.finite(prediction)),.groups="drop"),file.path(out,"long_difference_method_support.csv"))
 dm <- matched(diffs,c("fold_id","model_source_id"))
 write_csv(score(dm,c("comparison_set","fold_type","method")),file.path(out,"long_difference_scores_matched.csv"))
 write_csv(score(dm,c("comparison_set","method")),file.path(out,"long_difference_scores_pooled_matched.csv"))
}
# Fixed quantities; identical chosen source channel for each good at both dates.
# Endpoint dates are the first/last test years, chosen without using prices.
# Select among eligible channels by TRAINING count then identifier, never price.
primary <- readRDS(file.path(out,"primary_snapshot.rds"))
reference <- tibble(commodity=primary$commodities,reference_price=colMeans(exp(price_fit_locations(primary))[between(primary$years,1770,1779),,drop=FALSE])) |>
  left_join(quantity_table,by="commodity") |> mutate(base_cost=quantity*reference_price,base_cost_weight=base_cost/sum(base_cost))
write_csv(reference,file.path(out,"basket_coverage_reference_1770s_primary.csv"))
basket_ledger <- list();basket_members <- list();baskets <- list()
for(fid in names(design$folds)) {
 type <- design$ledger$fold_type[match(fid,design$ledger$fold_id)]
 if(!startsWith(type,"portfolio")) next
 test <- design$panel |> filter(row_id%in%design$folds[[fid]])
 train <- design$panel |> filter(!row_id%in%test$row_id)
 yr <- range(test$year)
 eligible <- test |> filter(year%in%yr) |> group_by(commodity,model_source_id) |> summarise(n_endpoints=n_distinct(year),.groups="drop") |> filter(n_endpoints==2) |>
   left_join(train |> count(model_source_id,name="training_n"),by="model_source_id") |> mutate(training_n=replace_na(training_n,0L)) |>
   arrange(commodity,desc(training_n),model_source_id) |> group_by(commodity) |> slice_head(n=1) |> ungroup() |> left_join(reference,by="commodity")
 reason <- if(diff(yr)<3) "endpoints_less_than_three_years_apart" else if(nrow(eligible)<2) "fewer_than_two_goods_with_same_channel_at_both_endpoints" else "eligible"
 basket_ledger[[fid]] <- tibble(fold_id=fid,fold_type=type,year_from=yr[1],year_to=yr[2],n_goods=nrow(eligible),goods=paste(eligible$commodity,collapse="; "),reference_cost_coverage=sum(eligible$base_cost_weight),status=reason)
 if(reason!="eligible") next
 basket_members[[fid]] <- eligible |> mutate(fold_id=fid,year_from=yr[1],year_to=yr[2])
 for(x in Filter(function(x) x$fold_id==fid,success)) {
   p <- x$predictions;a <- match(paste(eligible$model_source_id,yr[1]),paste(p$model_source_id,p$year));b <- match(paste(eligible$model_source_id,yr[2]),paste(p$model_source_id,p$year))
   stopifnot(!anyNA(c(a,b)))
   q <- eligible$quantity
   relative <- function(vals) log(sum(q*exp(vals[b]))/sum(q*exp(vals[a])))
   observed <- relative(p$log_price);predicted <- relative(p$prediction)
   draw_relative <- log(colSums(q*exp(x$joint_quote[b,,drop=FALSE]))/colSums(q*exp(x$joint_quote[a,,drop=FALSE])))
   bounds <- quantile(draw_relative,c(.05,.95),names=FALSE)
   baskets[[paste(fid,x$method)]] <- tibble(fold_id=fid,method=x$method,observed=observed,prediction=predicted,q05=bounds[1],q95=bounds[2],within_channel=relative(p$within_channel),pooled_loglinear=relative(p$pooled_loglinear))
 }
}
write_csv(bind_rows(basket_ledger),file.path(out,"subset_basket_eligibility.csv"))
write_csv(bind_rows(basket_members),file.path(out,"subset_basket_membership.csv"))
bs <- bind_rows(baskets)
if(nrow(bs)) {
 bb <- bs |> distinct(fold_id,.keep_all=TRUE) |> select(-method,-prediction,-q05,-q95) |> pivot_longer(c(within_channel,pooled_loglinear),names_to="method",values_to="prediction") |> mutate(q05=NA_real_,q95=NA_real_)
 bs <- bind_rows(bs |> select(-within_channel,-pooled_loglinear),bb) |> left_join(bind_rows(basket_ledger),by="fold_id") |>
   mutate(error=prediction-observed,covered90=if_else(is.finite(q05),observed>=q05&observed<=q95,NA))
 write_csv(bs,file.path(out,"noisy_subset_basket_log_relatives.csv"))
 write_csv(bs |> group_by(method) |> summarise(n_scheduled=n(),n_eligible=sum(is.finite(prediction)),n_unmatched=sum(!is.finite(prediction)),.groups="drop"),file.path(out,"subset_basket_method_support.csv"))
 write_csv(score(matched(bs,"fold_id"),c("comparison_set","method")),file.path(out,"subset_basket_scores_matched.csv"))
}
print(status |> count(status));print(score(quote_matched,c("comparison_set","method")))

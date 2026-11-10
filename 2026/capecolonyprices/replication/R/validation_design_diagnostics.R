source(here::here("replication","R","helpers.R"))
run_name <- Sys.getenv("CAPE_VALIDATION_RUN","constant_offsets")
out <- if(run_name=="constant_offsets") file.path(v2_out,"validation") else file.path(v2_out,"validation",run_name)
d <- readRDS(file.path(out,"design.rds"));p <- d$panel
positions <- imap_dfr(d$folds,function(ids,fid) {
 tr <- p |> filter(!row_id%in%ids);te <- p |> filter(row_id%in%ids)
 ch <- tr |> group_by(model_source_id) |> summarise(training_n=n(),first_training_year=min(year),last_training_year=max(year),.groups="drop")
 good <- tr |> group_by(commodity) |> summarise(training_n=n_distinct(year),first_training_year=min(year),last_training_year=max(year),.groups="drop")
 a <- te |> select(row_id,commodity,model_source_id,year) |> left_join(ch,by="model_source_id") |> mutate(method="within_channel")
 b <- te |> select(row_id,commodity,model_source_id,year) |> left_join(good,by="commodity") |> mutate(method="pooled_loglinear")
 bind_rows(a,b) |> mutate(fold_id=fid,training_n=replace_na(training_n,0L),
  support=case_when(training_n==0L~"unavailable",training_n==1L~"single_training_year_constant",year<first_training_year~"constant_left_extrapolation",year>last_training_year~"constant_right_extrapolation",TRUE~"interpolation"),
  extrapolation=if_else(training_n==0L,NA,year<first_training_year|year>last_training_year))
})
write_csv(positions,file.path(out,"baseline_support_positions.csv"))
write_csv(positions |> left_join(d$ledger |> select(fold_id,fold_type),by="fold_id") |> count(fold_type,method,support),file.path(out,"baseline_support_position_counts.csv"))
template <- read_csv(file.path(out,"actual_late_missingness_template.csv"),show_col_types=FALSE)
absence <- template |> group_by(year) |> summarise(n_goods_observed=sum(!missing),.groups="drop")
write_csv(absence,file.path(out,"late_pattern_goods_observed_by_year.csv"))
mask <- imap_dfr(d$folds,function(ids,fid) {
 if(!startsWith(fid,"actual_missingness")) return(tibble())
 te <- p |> filter(row_id%in%ids)
 tibble(fold_id=fid,n_template_commodity_years=nrow(template),n_absent_template_commodity_years=sum(template$missing),
   n_template_years_without_any_good=sum(absence$n_goods_observed==0L),n_realisable_masked_commodity_years=nrow(distinct(te,commodity,year)),
   share_absence_cells_with_observed_target=nrow(distinct(te,commodity,year))/sum(template$missing),n_test_quotes=nrow(te),n_test_official=sum(te$noise_role=="official"),share_test_official=mean(te$noise_role=="official"))
})
write_csv(mask,file.path(out,"transferred_missingness_realisability.csv"))
if(run_name!="constant_offsets") {
 old <- readRDS(file.path(v2_out,"validation","design.rds"))
 paired <- imap_dfr(old$folds,function(ids,fid) tibble(fold_id=fid,constant_n=length(ids),preferred_n=length(d$folds[[fid]]),identical_observation_ids=identical(sort(ids),sort(d$folds[[fid]]))))
 write_csv(paired,file.path(out,"paired_mask_integrity.csv"));stopifnot(all(paired$identical_observation_ids))
 if(nrow(d$splice_donors)) {
  detail <- map_dfr(seq_len(nrow(d$splice_donors)),function(i) {
   z <- d$splice_donors[i,];tr <- p |> filter(!row_id%in%d$folds[[z$fold_id]])
   a <- tr |> filter(model_source_id==z$target) |> select(year,target_log_price=log_price)
   b <- tr |> filter(model_source_id==z$donor) |> select(year,donor_log_price=log_price)
   ov <- inner_join(a,b,by="year")
   z |> mutate(overlap_years=paste(ov$year,collapse=";"),median_training_adjustment=median(ov$target_log_price-ov$donor_log_price))
  })
  write_csv(detail,file.path(out,"overlap_splice_training_calibration.csv"))
 }
}

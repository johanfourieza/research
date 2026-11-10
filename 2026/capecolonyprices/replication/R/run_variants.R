source(here::here("replication","R","helpers.R"))
ledger <- v2_read("nominal_observation_ledger.csv")
primary <- readRDS(file.path(v2_out,"annual_state_model.rds"))
variants_dir <- file.path(v2_out,"variants");dir.create(variants_dir,showWarnings=FALSE)
settings <- tribble(~variant_id,~kind,~value,~group,
 "legacy_1770_1795_splits","split",1770,"identification",
 "split_1770_only","split",1770,"identification",
 "offsets_constant","constant",0,"identification",
 "source_prior_010","tau",.10,"identification",
 "source_prior_015","tau",.15,"identification",
 "source_prior_025","tau",.25,"identification",
 "source_prior_035","tau",.35,"identification",
 "source_prior_075","tau",.75,"identification",
 "source_prior_100","tau",1,"identification",
 "shared_post_offset025","shared_sd",.25,"identification",
 "shared_post_offset050","shared_sd",.5,"identification",
 "shared_post_offset075","shared_sd",.75,"identification",
 "shared_new_columns025","shared_new_sd",.25,"identification",
 "shared_new_columns050","shared_new_sd",.5,"identification",
 "shared_new_columns075","shared_new_sd",.75,"identification",
 "post_source_mean_plus25","shared_mean",log(1.25),"identification",
 "post_source_mean_minus25","shared_mean",-log(1.25),"identification",
 "post_source_mean_plus50","shared_mean",log(1.5),"identification",
 "post_source_mean_minus50","shared_mean",-log(1.5),"identification",
 "new_column_mean_plus25","shared_new_mean",log(1.25),"identification",
 "new_column_mean_minus25","shared_new_mean",-log(1.25),"identification",
 "new_column_mean_plus50","shared_new_mean",log(1.5),"identification",
 "new_column_mean_minus50","shared_new_mean",-log(1.5),"identification",
 "independent_commodity_paths","independent",0,"identification",
 "official_ar015","ar",.15,"measurement",
 "official_ar030","ar",.30,"measurement",
 "official_flat_runs_discounted","flat",0,"stress",
 "official_runs_first_only","run_first",0,"stress",
 "constant_without_meat_fuel_bridges","joint_bridge",0,"identification",
 "unresolved_guilder_divide3","guilder",3,"currency",
 "sterling_quotes_excluded","sterling_exclude",0,"currency",
 "sterling_fixed0075","sterling_fixed",.075,"currency",
 "sterling_fixed0200","sterling_fixed",.2,"currency",
 "wheat1787_excluded","wheat1787",0,"source",
 "soap_additions_excluded","soap_audit",0,"source",
 "preaudit_inputs","preaudit",0,"source",
 "wheat_including_koorn","broad",0,"source",
 "inventory_valuations_excluded","inventory",0,"source",
 "official_sources_excluded","no_official",0,"stress",
 "no_transitory_wheat","transitory",0,"stress",
 "robust_auction_variance","robust",0,"measurement",
 "no_wheat_specific_random_walk","wheat_extra",0,"measurement",
 "common_observation_variance","noise",0,"measurement")
args <- commandArgs(trailingOnly=TRUE)
batch <- if(length(args)) as.integer(args[1]) else 1L
nbatch <- if(length(args)>1) as.integer(args[2]) else 1L
if(batch==1L) write_csv(settings,file.path(v2_out,"v2_variant_settings.csv"))
loading_map <- v2_read("channel_family_loading_map.csv")
for(i in seq_len(nrow(settings))) {
 if((i-1L)%%nbatch+1L!=batch) next
 a <- settings[i,];done <- file.path(variants_dir,paste0(a$variant_id,"_summary.rds"))
 if(file.exists(done)) next
 x <- ledger;pa <- list(fixed_offsets=FALSE);fa <- list(inits=primary$optim$par)
 if(a$kind=="constant") pa$fixed_offsets <- TRUE
 if(a$kind=="split") {
  pa$fixed_offsets <- FALSE
  if(a$variant_id=="legacy_1770_1795_splits") x$model_source_id <- x$legacy_model_source_id
  else x <- x |> mutate(model_source_id=paste(if_else(str_detect(source_role,"VOC|government"),paste(source_id,if_else(year<a$value,"before","after"),sep=" :: "),source_id),source_role,sep=" :: role="))
 }
 if(a$kind=="tau") fa$offset_sd <- a$value
 if(a$kind=="shared_sd") fa$shared_offset_sd <- a$value
 if(a$kind=="shared_mean") fa$shared_offset_mean <- a$value
 if(a$kind=="shared_new_sd") fa$shared_offset_sd <- a$value
 if(a$kind=="shared_new_mean") fa$shared_offset_mean <- a$value
 if(str_starts(a$kind,"shared")) {
  g <- if(str_detect(a$kind,"new")) loading_map$g_new else loading_map$g_all
  fa$shared_loading_override <- setNames(g,loading_map$model_source_id)
 }
 if(a$kind=="ar") fa$official_ar_sd <- a$value
 if(a$kind=="independent") fa$independent <- TRUE
 if(a$kind=="flat") pa$flat_discount <- TRUE
 if(a$kind=="guilder") x <- x |> mutate(price_rd_standard=if_else(conversion_rule=="account_guilder_divide_2.4_assumption",price_rd_standard*2.4/3,price_rd_standard))
 if(a$kind=="sterling_exclude") x <- x |> filter(!coalesce(v2_sterling_origin_unverified,FALSE))
 if(a$kind=="sterling_fixed") x <- x |> mutate(price_rd_standard=if_else(coalesce(v2_sterling_origin_unverified,FALSE),native_amount/a$value/physical_factor,price_rd_standard))
 if(a$kind=="wheat1787") x <- x |> filter(observation_id!="Wheat!E140")
 if(a$kind=="soap_audit") x <- x |> filter(!(commodity=="Soap"&coalesce(v2_action,"")=="independent_source_audit_addition"))
 if(a$kind=="preaudit") {
  x <- read_csv(here::here("data","inputs","accepted_price_ledger.csv"),show_col_types=FALSE,guess_max=Inf) |>
   mutate(model_source_id=paste(if_else(str_detect(source_role,"VOC|government"),paste(source_id,if_else(year<1795,"pre1795","post1795"),sep=" :: "),source_id),source_role,sep=" :: role="))
 }
 if(a$kind=="joint_bridge") {
  pa$fixed_offsets <- TRUE
  x <- x |> filter(!str_detect(source_id,"^Meat \\| F \\||^Firewood \\| E \\|"))
 }
 if(a$kind=="broad") pa$broad_grain <- TRUE
 if(a$kind=="inventory") x <- x |> filter(source_role!="inventory_valuation")
 if(a$kind=="no_official") pa$official <- FALSE
 if(a$kind=="transitory") fa$transitory_wheat <- FALSE
 if(a$kind=="robust") fa$robust_auction <- TRUE
 if(a$kind=="wheat_extra") fa$wheat_specific <- FALSE
 if(a$kind=="noise") fa$role_noise <- FALSE
 panel <- do.call(prepare_price_panel,c(list(ledger=x),pa))
 if(a$kind=="run_first") panel <- panel |> group_by(model_source_id) |> arrange(year,.by_group=TRUE) |>
  mutate(run_first=replace_na(year!=lag(year)+1|abs(log_price-lag(log_price))>1e-10,TRUE)) |>
  filter(noise_role!="official"|run_first) |> ungroup() |> select(-run_first) |> arrange(commodity,model_source_id,year)
 f <- do.call(fit_checked,c(list(panel=panel),fa))
 d <- index_from_fit(f,nsim=1000,seed=20261109L+i)
 h <- historical_functionals(d$annual,d$joint,a$variant_id)
 same_y <- function(z) z |> select(commodity,year,log_price) |> arrange(commodity,year,log_price)
 comparable <- isTRUE(all.equal(same_y(panel),same_y(primary$panel),check.attributes=FALSE,tolerance=1e-12))&&a$kind!="robust"
 pars <- tibble(variant_id=a$variant_id,group=a$group,channels=nrow(f$channels),channel_years=nrow(panel),
  common_sd=if(isTRUE(f$independent)) 0 else exp(f$optim$par[1]),relative_sd=exp(f$optim$par[2]),
  wheat_extra_sd=if(isTRUE(f$wheat_specific)) exp(f$optim$par[3]) else 0,
  auction_observation_sd=exp(f$optim$par[if(f$role_noise) 4 else 6]),official_observation_sd=exp(f$optim$par[if(f$role_noise) 5 else 6]),other_observation_sd=exp(f$optim$par[6]),
  wheat_transitory_sd=if(isTRUE(f$transitory_wheat)) exp(f$optim$par[7]) else 0,
  offset_sd=f$offset_sd,shared_offset_sd=f$shared_offset_sd,shared_offset_mean=f$shared_offset_mean,official_ar_sd=f$official_ar_sd,
  convergence=f$optim$convergence,negative_log_likelihood=f$optim$value,likelihood_same_observed_values=comparable)
 saveRDS(list(annual=d$annual |> mutate(variant_id=a$variant_id),parameters=pars,functionals=h$relatives,peak=h$peak_summary,peak_draws=h$peak_draws),done)
 saveRDS(list(year=f$years,draw_id=seq_len(ncol(d$joint)),index=d$joint,seed=d$seed,variant_id=a$variant_id),file.path(variants_dir,paste0(a$variant_id,"_joint.rds")))
 # The complete primary fit is retained. Variant parameters plus declared input
 # rules suffice for refitting; avoid duplicating large smoother covariance arrays.
 cat("Completed",i,"/",nrow(settings),a$variant_id,"NLL",round(f$optim$value,3),"peak",h$peak_summary$point_peak_year,"\n")
 rm(f,d);gc(verbose=FALSE)
}

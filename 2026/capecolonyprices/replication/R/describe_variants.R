source(here::here("replication","R","helpers.R"))
s <- v2_read("v2_variant_settings.csv") |>
 mutate(shared_loading=case_when(str_detect(kind,"^shared_new")~"g_new",str_detect(kind,"^shared")~"g_all",TRUE~NA_character_),
  source_prior_mean_log=if_else(kind%in%c("shared_mean","shared_new_mean"),value,NA_real_),
  source_prior_level_factor=exp(source_prior_mean_log),
  source_prior_percent_deviation=100*expm1(source_prior_mean_log),
  interpretation=case_when(!is.na(source_prior_mean_log)~"prior source quote level relative to latent price; negative log shifts use reciprocal factors",
   kind%in%c("shared_sd","shared_new_sd")~"shared offset standard deviation in log units, not a percentage change",
   kind=="tau"~"independent source offset standard deviation in log units",
   kind=="ar"~"stationary per-official-channel measurement-error SD in log units; rho0.8",
   TRUE~"see declared input/model transformation in run_variants.R"))
write_csv(s,file.path(here::here("data","series"),"alternative_assumption_interpretation.csv"))
v2_write(s,"variant_assumption_interpretation.csv")
stopifnot(abs(s$source_prior_percent_deviation[s$variant_id=="post_source_mean_minus25"]+20)<1e-10,
 abs(s$source_prior_percent_deviation[s$variant_id=="post_source_mean_minus50"]+100/3)<1e-10)

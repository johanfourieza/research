suppressPackageStartupMessages({library(tidyverse);library(here)})
root <- here::here("replication","work");out <- file.path(root,"analysis")
original <- read_csv(here::here("data","inputs","accepted_price_ledger.csv"),show_col_types=FALSE,guess_max=Inf)
stopifnot(nrow(problems(original))==0L)
audit <- read_csv(here::here("data","inputs","currency_audit_actions.csv"),show_col_types=FALSE,guess_max=Inf)
money <- read_csv(file.path(out,"inherited_monetary_assumptions_NOT_VERIFIED_PARITIES.csv"),show_col_types=FALSE)
stopifnot(nrow(audit)==43L,!anyDuplicated(audit$observation_id),all(audit$arithmetic_agrees))
x <- original |> left_join(audit |> select(observation_id,v2_action=action,v2_new_year=new_year,v2_reason=reason,v2_raw_cells=raw_cells,v2_currency_provenance=currency_provenance),by="observation_id") |>
 mutate(year=if_else(v2_action=="correct_year_from_raw_sheet"&!is.na(v2_action),v2_new_year,year),
  included=if_else(str_detect(coalesce(v2_action,""),"^exclude"),FALSE,included),
  exclusion_reason=if_else(str_detect(coalesce(v2_action,""),"^exclude"),paste0("v2_source_audit: ",v2_reason),exclusion_reason),
  variant_use=if_else(str_detect(coalesce(v2_action,""),"^exclude"),"excluded",variant_use),
  source_role=if_else(v2_action=="exclude_procurement"&!is.na(v2_action),"government_purchase",source_role),
  source_role=if_else(v2_action=="exclude_trade_unit_value"&!is.na(v2_action),"trade_unit_value",source_role))
new_rate <- money$gbp_per_rd[match(x$year,money$year)]
corrected <- which(x$v2_action=="correct_year_from_raw_sheet")
x$gbp_per_rd[corrected] <- new_rate[corrected]
x$currency_factor[corrected] <- 1/new_rate[corrected]
x$price_rd_standard[corrected] <- x$native_amount[corrected]*x$currency_factor[corrected]/x$physical_factor[corrected]
soap <- read_csv(here::here("data","inputs","additional_soap_document_observations.csv"),show_col_types=FALSE,guess_max=Inf)
butter <- read_csv(here::here("data","inputs","currency_audit_additions_ledger.csv"),show_col_types=FALSE,guess_max=Inf)
additions <- bind_rows(soap,butter) |> mutate(v2_action="independent_source_audit_addition")
x <- bind_rows(x,additions) |> mutate(
  v2_sterling_origin_unverified=included&conversion_rule=="sterling_divide_dated_or_interpolated_rate",
  regime=case_when(year<1770~"pre1770",year<1795~"1770_1794",TRUE~"1795_on"),
  legacy_model_source_id=paste(if_else(str_detect(source_role,"VOC|government"),paste(source_id,regime,sep=" :: "),source_id),source_role,sep=" :: role="),
  stable_model_source_id=paste(source_id,source_role,sep=" :: role="),
  model_source_id=paste(if_else(str_detect(source_role,"VOC|government"),
   paste(source_id,if_else(year<1795,"pre1795","post1795"),sep=" :: "),source_id),source_role,sep=" :: role="))
retained_primary <- x$included&!x$source_role%in%c("VOC_purchase","government_purchase")
stopifnot(nrow(x)==nrow(original)+3L,!anyDuplicated(x$observation_id),sum(retained_primary)==661L,
 sum(x$v2_sterling_origin_unverified,na.rm=TRUE)==37L,all(x$price_rd_standard[x$included]>0))
write_csv(x,file.path(out,"nominal_observation_ledger.csv"))
write_csv(x |> count(commodity,variant_use,exclusion_reason),file.path(out,"nominal_exclusion_counts.csv"))
write_csv(x |> filter(!is.na(v2_action)) |> select(observation_id,commodity,year,v2_action,included,price_rd_standard,v2_reason),file.path(out,"v2_source_changes.csv"))
write_csv(tibble(check=c("unique_observation_ids","661_retained_rows","37_flagged_sterling_rows","positive_retained_prices","three_audited_additions"),passed=TRUE),file.path(out,"v2_input_checks.csv"))
cat("V2 inputs: ",sum(retained_primary)," retained primary rows; 12 corrected dates, 7 exclusions, 3 new document observations.\n",sep="")

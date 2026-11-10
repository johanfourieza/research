# Separate analytical outcomes. These prices never enter consumption weights.
suppressPackageStartupMessages({library(tidyverse);library(here)})
out <- here::here("replication","work","analysis")
cpi <- read_csv(file.path(out,"annual_inflation_1700_1834.csv"),show_col_types=FALSE)
p <- read_csv(file.path(out,"enslaved_annual_completed.csv"),show_col_types=FALSE) |>
 filter(series=="all_individuals",between(year,1700,1834)) |>
 mutate(nominal_sale_price_index=100*price_completed_rd/mean(price_completed_rd[between(year,1770,1779)],na.rm=TRUE)) |>
 left_join(select(cpi,year,consumption_index=index_point),by="year") |>
 mutate(sale_price_relative_to_consumption=100*nominal_sale_price_index/consumption_index,
   concept="historical_price_of_coercive_claim_separate_from_consumption_no_personal_welfare_interpretation",
   composition_status="annual_unadjusted_composition_see_separate_decadal_diagnostic")
stopifnot(nrow(p)==135,all(is.finite(p$nominal_sale_price_index)))
write_csv(p,file.path(out,"enslaved_sale_price_companion.csv"))
dc <- cpi |> mutate(decade=year%/%10*10) |> group_by(decade) |>
 summarise(consumption_index=mean(index_point),years=n(),.groups="drop")
adjusted <- read_csv(file.path(out,"composition_adjusted_decadal_index.csv"),show_col_types=FALSE) |>
 filter(between(decade,1700,1830)) |> left_join(dc,by="decade") |>
 mutate(recorded_characteristics_adjusted_relative_index=100*adjusted_index/consumption_index,
   scope="recorded_sex_origin_age_skill_health_controls_missing_age_not_recovered",
   uncertainty_note="existing_nominal_cluster_intervals_only_no_joint_consumption_uncertainty_interval_constructed")
write_csv(adjusted,file.path(out,"enslaved_recorded_characteristics_decadal_companion.csv"))
h <- read_csv(file.path(out,"household_annual_acquisition_indices_NOT_CPI.csv"),show_col_types=FALSE) |>
 filter(basket=="household8") |> group_by(method) |>
 mutate(equipment_index=100*index_equal_reference_share/mean(index_equal_reference_share[between(year,1770,1779)])) |>
 ungroup() |> left_join(select(cpi,year,consumption_index=index_point),by="year") |>
 mutate(equipment_relative_to_consumption=100*equipment_index/consumption_index,
  concept="equal_reference_cost_equipment_acquisition_companion_not_CPI_expenditure_weights") |>
 group_by(method) |> arrange(year,.by_group=TRUE) |>
 mutate(equipment_inflation_pct=100*(equipment_index/lag(equipment_index)-1)) |> ungroup()
write_csv(h,file.path(out,"household_equipment_companion.csv"))
write_csv(tibble(check=c("135_sale_price_years","no_person_sale_price_in_consumption_basket","two_equipment_methods_complete"),
 passed=c(nrow(p)==135,!any(str_detect(read_csv(file.path(out,"basket_quantities_weights.csv"),show_col_types=FALSE)$commodity,"enslav|slave")),nrow(h)==270)),
 file.path(out,"asset_companion_checks.csv"))

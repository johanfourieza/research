suppressPackageStartupMessages({library(tidyverse);library(here)})
out <- here::here("replication","work","analysis")
a <- read_csv(file.path(out,"annual_inflation_1700_1834.csv"),show_col_types=FALSE)
e <- read_csv(file.path(out,"elementary_annual_state.csv"),show_col_types=FALSE)
q <- read_csv(file.path(out,"basket_quantities_weights.csv"),show_col_types=FALSE)
money <- read_csv(file.path(out,"inherited_monetary_assumptions_NOT_VERIFIED_PARITIES.csv"),show_col_types=FALSE)
# Physical-quantity variant, not relabelled as De Zwart's original179kg basket.
q_variant <- left_join(e,select(q,commodity,quantity),by="commodity") |>
 mutate(quantity=if_else(commodity=="Wheat",193.5,quantity)) |> group_by(year) |>
 summarise(cost=sum(quantity*price_median),.groups="drop") |>
 mutate(index=100*cost/mean(cost[between(year,1770,1779)]),
   inflation_pct=100*(index/lag(index)-1),variant_id="wheat_quantity193.5")
write_csv(q_variant,file.path(out,"quantity_variant.csv"))

# Constant reference budget shares give a geometric comparator. This imposes
# substitution elasticity one; the fixed-quantity basket imposes zero substitution.
# Neither preference restriction is estimated from estate sales quantities.
geometric <- left_join(e,select(q,commodity,base_weight),by="commodity") |>
 group_by(commodity) |> mutate(reference_price=mean(price_median[between(year,1770,1779)]),
   term=base_weight*log(price_median/reference_price)) |> ungroup() |>
 group_by(year) |> summarise(raw_index=exp(sum(term)),.groups="drop") |>
 mutate(index=100*raw_index/mean(raw_index[between(year,1770,1779)]),
   inflation_pct=100*(index/lag(index)-1),variant_id="constant_reference_shares_geometric")
write_csv(geometric,file.path(out,"geometric_consumption_comparator.csv"))

# A partial market-reference level calibration tests the basket-weight convention.
# Only market channels actually present in the1770s qualify; absent markets do
# not acquire invented retail margins. Quantities remain fixed, not expenditures.
panel <- read_csv(file.path(out,"annual_model_panel.csv"),show_col_types=FALSE)
offsets <- read_csv(file.path(out,"source_offset_estimates.csv"),show_col_types=FALSE)
market_adjustment <- panel |> filter(between(year,1770,1779),source_role%in%c("auction","auction_transcribed","newspaper")) |>
 left_join(select(offsets,model_source_id,offset),by="model_source_id") |> group_by(commodity) |>
 summarise(log_market_adjustment=weighted.mean(offset,pmin(precision_units,5)),
   base_market_years=n_distinct(year),base_market_channels=n_distinct(model_source_id),.groups="drop")
market_weights <- q |> left_join(market_adjustment,by="commodity") |>
 mutate(log_market_adjustment=replace_na(log_market_adjustment,0),
   calibrated_to_market=!is.na(base_market_years),unscaled_weight=base_weight*exp(log_market_adjustment),
   market_reference_weight=unscaled_weight/sum(unscaled_weight))
write_csv(market_weights,file.path(out,"partial_market_reference_weights.csv"))
market_index <- e |> left_join(select(market_weights,commodity,quantity,log_market_adjustment),by="commodity") |>
 group_by(year) |> summarise(cost=sum(price_median*quantity*exp(log_market_adjustment)),.groups="drop") |>
 mutate(index=100*cost/mean(cost[between(year,1770,1779)]),inflation_pct=100*(index/lag(index)-1),
   variant_id="partial_1770s_market_reference_levels")
write_csv(market_index,file.path(out,"partial_market_reference_index.csv"))

h <- read_csv(file.path(out,"household_annual_acquisition_indices_NOT_CPI.csv"),show_col_types=FALSE) |>
 filter(basket=="household8") |> group_by(method) |>
 mutate(equipment_index=100*index_equal_reference_share/mean(index_equal_reference_share[between(year,1770,1779)])) |>
 ungroup() |> select(year,method,equipment_index)
# These are explicit normative allocations to equipment replacement, not estimated
# expenditure shares and not gross auction turnover. No claimed user-cost estimate.
extended <- crossing(equipment_share=c(.05,.10,.20),method=unique(h$method),year=a$year) |>
 left_join(h,by=c("year","method")) |> left_join(select(a,year,index_point),by="year") |>
 mutate(index=(1-equipment_share)*index_point+equipment_share*equipment_index,
   interpretation="hypothetical_equipment_replacement_allocation_NOT_observed_household_budget") |>
 group_by(method,equipment_share) |> arrange(year,.by_group=TRUE) |>
 mutate(inflation_pct=100*(index/lag(index)-1)) |> ungroup()
write_csv(extended,file.path(out,"equipment_allocation_variants.csv"))

silver <- select(a,year,index_nominal=index_point,annual_cost_rd) |> left_join(money,by="year") |>
 pivot_longer(c(silver_per_rd_2.4,silver_per_rd_3),names_to="monetary_scenario",values_to="silver_per_rd") |>
 mutate(cost_silver=annual_cost_rd*silver_per_rd) |> group_by(monetary_scenario) |> arrange(year,.by_group=TRUE) |>
 mutate(index_silver=100*cost_silver/mean(cost_silver[between(year,1770,1779)]),
   inflation_silver_pct=100*(index_silver/lag(index_silver)-1),
   status="accounting_comparator_inherited_silver_content_schedule_not_causal_monetary_estimate") |> ungroup()
write_csv(silver,file.path(out,"nominal_silver_comparator.csv"))

v <- read_csv(file.path(out,"annual_structural_variants.csv"),show_col_types=FALSE)
vs <- left_join(v,select(a,year,index_primary=index_point,inflation_primary=inflation_pct),by="year") |>
 group_by(variant_id) |> summarise(years=n(),max_index_difference_pct=max(abs(index/index_primary-1))*100,
   max_inflation_difference_pp=max(abs(inflation_pct-inflation_primary),na.rm=TRUE),
   index1700=index[year==1700],index1800=index[year==1800],index1818=index[year==1818],index1834=index[year==1834],
   largest_inflation_difference_year=year[which.max(abs(inflation_pct-inflation_primary))],
   mean1820s=mean(index[between(year,1820,1829)]),.groups="drop")
write_csv(vs,file.path(out,"structural_variant_comparison.csv"))
write_csv(filter(v,year%in%c(1769,1770,1794,1795,1800,1818,1834)),file.path(out,"structural_variant_key_years.csv"))
expected_variants <- nrow(read_csv(file.path(out,"v2_variant_settings.csv"),show_col_types=FALSE))
checks <- tibble(check=c("all_structural_variants_complete","all_variant_bases_100","all_changes_match_levels","all_variant_fits_converged"),
 passed=c(nrow(v)==expected_variants*135,all(abs(v |> filter(between(year,1770,1779)) |> group_by(variant_id) |> summarise(m=mean(index)) |> pull(m)-100)<1e-8),
   all(abs(v |> group_by(variant_id) |> arrange(year,.by_group=TRUE) |> mutate(error=inflation_pct-100*(index/lag(index)-1)) |> pull(error))<1e-8,na.rm=TRUE),
   all(read_csv(file.path(out,"structural_variant_parameters.csv"),show_col_types=FALSE)$convergence==0)))
write_csv(checks,file.path(out,"structural_variant_checks.csv"));stopifnot(all(checks$passed))
print(vs,width=160)

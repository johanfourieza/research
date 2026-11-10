# Accounting comparisons with the inherited completed basket. These share data
# and are not independent validation. Keep nominal and silver units separate.
suppressPackageStartupMessages({library(tidyverse);library(here)})
out <- here::here("replication","work","analysis")
a <- read_csv(file.path(out,"annual_inflation_1700_1834.csv"),show_col_types=FALSE)
old <- read_csv(file.path(out,"inherited_basket_monetary_scenarios_NOT_HEADLINE.csv"),show_col_types=FALSE) |>
 filter(basket=="cost_formula") |>
 select(year,monetary_scenario,inherited_silver_index=index,
        inherited_silver_inflation_pct=inflation_pct,
        inherited_nominal_scenario_index=index_nominal_conditional,
        inherited_nominal_scenario_inflation_pct=inflation_nominal_conditional)
new <- read_csv(file.path(out,"nominal_silver_comparator.csv"),show_col_types=FALSE) |>
 select(year,monetary_scenario,new_silver_accounting_index=index_silver,
        new_silver_accounting_inflation_pct=inflation_silver_pct)
comparison <- old |> left_join(new,by=c("year","monetary_scenario")) |>
 left_join(select(a,year,new_nominal_index=index_point,new_nominal_inflation_pct=inflation_pct),by="year") |>
 mutate(nominal_index_ratio=new_nominal_index/inherited_nominal_scenario_index,
        silver_index_ratio=new_silver_accounting_index/inherited_silver_index,
        interpretation="shared_sources_and_assumed_monetary_conversions_NOT_independent_validation")
write_csv(comparison,file.path(out,"published_basket_accounting_comparison.csv"))
key <- comparison |> filter(year%in%c(1700,1743,1769,1770,1794,1795,1800,1818,1820,1821,1834))
write_csv(key,file.path(out,"published_basket_comparison_key_years.csv"))
stopifnot(nrow(comparison)==270,all(is.finite(comparison$nominal_index_ratio)),
 all(is.finite(comparison$silver_index_ratio)),
 all(comparison$new_nominal_index>0),all(comparison$inherited_silver_index>0))
print(filter(key,year%in%c(1700,1795,1800,1818,1834)),width=180)

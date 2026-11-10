source(here::here('replication','R','helpers.R'))
for(pair in list(c('annual_inflation_1700_1834.csv','annual_inflation_1700_1834.csv'),
 c('elementary_annual_state.csv','commodity_price_summaries.csv'),c('basket_quantities_weights.csv','basket_quantities_weights.csv'),
 c('historical_functionals_primary.csv','historical_relatives.csv'))) {
 write_csv(v2_read(pair[1]),here::here('data','series',pair[2]))
}

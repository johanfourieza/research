suppressPackageStartupMessages({library(tidyverse);library(here)})
source(here::here("replication","R","state_model.R"))
out <- here::here("replication","work","analysis")
fit <- readRDS(file.path(out,"annual_state_model.rds"))
d <- readRDS(file.path(out,"elementary_annual_draws.rds"))
quantities <- tribble(~commodity,~quantity,~standard_unit,
 "Wheat",179,"kg","Beans",20,"kg","Meat",5,"kg","Butter",3,"kg",
 "Soap",1.3,"kg","Cotton",3,"m2","Candles",1.3,"kg","Oil",1.3,"litre","Fuel",3,"MBtu")
# The primary basket matches De Zwart (2013), Table 2. The 193.5kg wheat label
# in the later GPIH workbook is a separate quantity sensitivity.
q <- quantities$quantity[match(d$commodity,quantities$commodity)]
mu <- price_fit_locations(fit)
p <- exp(mu); prices <- exp(d$log_price)
cost <- as.vector(p%*%q)
draw_cost <- apply(sweep(prices,2,q,"*"),c(1,3),sum)
base <- d$year>=1770&d$year<=1779
idx <- cost/mean(cost[base])*100
idx_draw <- sweep(draw_cost,2,colMeans(draw_cost[base,,drop=FALSE]),"/")*100
infl <- c(NA_real_,100*(tail(idx,-1)/head(idx,-1)-1))
infl_draw <- rbind(NA_real_,100*(idx_draw[-1,,drop=FALSE]/idx_draw[-nrow(idx_draw),,drop=FALSE]-1))
qs <- function(z) t(apply(z,1,quantile,probs=c(.05,.5,.95),na.rm=TRUE))
qi <- qs(idx_draw);qf <- qs(infl_draw)
weights <- colMeans(p[base,,drop=FALSE])*q/mean(cost[base])
write_csv(quantities |> left_join(tibble(commodity=d$commodity,base_weight=weights),by="commodity"),
 file.path(out,"basket_quantities_weights.csv"))

# Evidence for a change requires the SAME source/specification in both years.
panel <- fit$panel
support <- panel |> arrange(commodity,model_source_id,year) |> group_by(commodity,model_source_id) |>
 mutate(same_source_previous_year=year-lag(year)==1,
   flat_official_change=same_source_previous_year&str_detect(source_role,"VOC|government|official_statistics")&abs(log_price-lag(log_price))<1e-10,
   market=source_role%in%c("auction","auction_transcribed","newspaper")) |> ungroup() |>
 group_by(commodity,year) |> summarise(observed_level=TRUE,
   observed_change=any(same_source_previous_year,na.rm=TRUE),market_level=any(market),
   market_change=any(market&same_source_previous_year,na.rm=TRUE),
   flat_official_change=any(flat_official_change,na.rm=TRUE),
   source_rows=sum(n_input_rows),documents=sum(n_documents),conversion_dependent=any(conversion_assumption),.groups="drop")
support <- expand_grid(commodity=d$commodity,year=d$year) |> left_join(support,by=c("commodity","year")) |>
 mutate(across(c(observed_level,observed_change,market_level,market_change,conversion_dependent,flat_official_change),~replace_na(.x,FALSE)),
   across(c(source_rows,documents),~replace_na(.x,0))) |>
 group_by(commodity) |> mutate(nearest_anchor_years=map_dbl(year,~min(abs(.x-year[observed_level]))),
   outside_anchor_range=year<min(year[observed_level])|year>max(year[observed_level])) |> ungroup() |>
 left_join(tibble(commodity=d$commodity,base_weight=weights),by="commodity")
aggregate_support <- support |> group_by(year) |> summarise(
 observed_level_weight_share=sum(base_weight*observed_level),
 observed_change_weight_share=if_else(first(year)==1700,NA_real_,sum(base_weight*observed_change)),
 market_level_weight_share=sum(base_weight*market_level),
 market_change_weight_share=if_else(first(year)==1700,NA_real_,sum(base_weight*market_change)),
 flat_official_change_weight_share=if_else(first(year)==1700,NA_real_,sum(base_weight*flat_official_change)),
 weighted_anchor_distance=sum(base_weight*nearest_anchor_years),
 beyond_anchor_weight=sum(base_weight*outside_anchor_range),.groups="drop")
annual <- tibble(variant_id="v2_nominal_basic9_1795_offsets",year=d$year,index_point=idx,
 index_q05=qi[,1],index_draw_median=qi[,2],index_q95=qi[,3],inflation_pct=infl,
 inflation_q05=qf[,1],inflation_draw_median=qf[,2],inflation_q95=qf[,3],
 annual_cost_rd=cost,interval_scope="conditional_on_fitted_parameters_unit_conversions_and_source_prior") |>
 left_join(aggregate_support,by="year") |>
 mutate(dated_quote_change_weight_share=observed_change_weight_share,
   support_class=case_when(year==1700~"level_only",market_change_weight_share>=.75~"high_market_quote_support",
   market_change_weight_share>=.4~"mixed_market_quotes_and_model",market_change_weight_share>0~"thin_market_quote_support",TRUE~"no_consecutive_market_quotes"),
   release_status="v2_conditional_reconstruction")
write_csv(annual,file.path(out,"annual_inflation_1700_1834.csv"))
write_csv(support,file.path(out,"annual_evidence_support.csv"))
saveRDS(list(year=d$year,index=idx_draw,inflation=infl_draw,conditional=TRUE),file.path(out,"annual_index_draws.rds"))
contrib <- rbind(NA_real_,100*sweep(p[-1,,drop=FALSE]-p[-nrow(p),,drop=FALSE],2,q,"*")/head(cost,-1))
contributions <- expand_grid(commodity=d$commodity,year=d$year) |> mutate(contribution_pp=as.vector(contrib))
write_csv(contributions,file.path(out,"annual_inflation_contributions.csv"))
decs <- annual |> mutate(decade=10*(year%/%10),previous_index=lag(index_point)) |> group_by(decade) |>
 summarise(years=n(),mean_index=mean(index_point),start_year=min(year),end_year=max(year),
 within_decade_compound_inflation_pct=100*((last(index_point)/first(index_point))^(1/(n()-1))-1),
 all_changes_in_decade_compound_pct=100*(exp(mean(log1p(inflation_pct/100),na.rm=TRUE))-1),
 n_annual_changes=sum(is.finite(inflation_pct)),
 mean_observed_change_share=mean(observed_change_weight_share,na.rm=TRUE),.groups="drop")
write_csv(decs,file.path(out,"decadal_inflation_summary.csv"))
tests <- tibble(check=c("135_annual_levels","134_annual_changes","base_mean_100","weights_sum_one","contributions_reconcile","ordered_intervals"),
 passed=c(nrow(annual)==135,sum(is.finite(infl))==134,abs(mean(idx[base])-100)<1e-10,
 abs(sum(weights)-1)<1e-10,max(abs(rowSums(contrib)[-1]-infl[-1]))<1e-10,all(qi[,1]<=qi[,3])))
write_csv(tests,file.path(out,"annual_index_checks.csv"));stopifnot(all(tests$passed))
print(decs,n=20)

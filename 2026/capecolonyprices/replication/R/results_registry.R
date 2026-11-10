source(here::here("replication","R","helpers.R"))
a <- v2_read("annual_inflation_1700_1834.csv")
p <- v2_read("annual_model_panel.csv")
l <- v2_read("nominal_observation_ledger.csv")
q <- v2_read("basket_quantities_weights.csv")
kept <- l |> filter(included,!source_role%in%c("VOC_purchase","government_purchase"))
stopifnot(setequal(p$model_source_id,kept$model_source_id),nrow(kept)==661,nrow(p)==632)
v2_write(kept |> count(native_currency,conversion_rule,name="input_rows"),"paper_primary_currency_rules.csv")
v2_write(kept |> group_by(source_label,source_role) |> summarise(input_rows=n(),commodities=n_distinct(commodity),
 first_year=min(year),last_year=max(year),.groups="drop"),"paper_primary_source_labels.csv")
coverage <- p |> group_by(commodity) |>
 summarise(channel_years=n(),channels=n_distinct(model_source_id),observed_years=n_distinct(year),
 earliest_year=min(year),latest_year=max(year),
 years_with_market_quotes=n_distinct(year[source_role%in%c("auction","auction_transcribed","newspaper")]),.groups="drop") |>
 left_join(kept |> group_by(commodity) |> summarise(ledger_rows=n(),
 project_document_specification_rows=sum(source_role=="auction"),
 underlying_project_lots=sum(n_lots[source_role=="auction"],na.rm=TRUE),.groups="drop"),by="commodity") |>
 left_join(q,by="commodity") |>
 left_join(select(v2_read("source_connectivity_summary.csv"),-channels),by="commodity")
v2_write(coverage,"paper_table_coverage.csv")
v2_write(kept |> filter(source_role=="auction") |> group_by(commodity) |>
 summarise(lots=sum(n_lots),document_specification_rows=n(),observed_years=n_distinct(year),
 unique_document_ids=n_distinct(document),first_year=min(year),last_year=max(year),.groups="drop"),"paper_auction_primary_coverage.csv")
claims <- list()
add <- function(id,value,unit,source,definition) {
 claims[[length(claims)+1L]] <<- tibble(claim_id=id,value=as.numeric(value),unit,source,definition)
}
add("annual_levels",nrow(a),"years","annual_inflation_1700_1834.csv","1700 through1834 inclusive")
add("annual_changes",sum(is.finite(a$inflation_pct)),"changes","annual_inflation_1700_1834.csv","1701 through1834")
add("primary_channels",n_distinct(p$model_source_id),"channels","annual_model_panel.csv","compiler source/specification/role; official offsets split1795 only")
add("primary_channel_years",nrow(p),"channel-years","annual_model_panel.csv","aggregated quoted cells, not individual transactions")
add("primary_input_rows",nrow(kept),"input rows","nominal_observation_ledger.csv","purchases excluded")
add("retained_sterling_quotes",sum(kept$v2_sterling_origin_unverified,na.rm=TRUE),"input rows","nominal_observation_ledger.csv","conditional convention; original sterling provenance unresolved")
add("wheat_base_share",q$base_weight[q$commodity=="Wheat"],"share","basket_quantities_weights.csv","fixed quantity times mean1770s fitted price, divided by mean cost")
add("no_quote_years",sum(a$observed_level_weight_share==0),"years","annual_inflation_1700_1834.csv","no retained quote for any of nine goods")
add("no_quote_years1795_1824",sum(a$observed_level_weight_share==0&between(a$year,1795,1824)),"years","annual_inflation_1700_1834.csv","no retained quote for any of nine goods in1795-1824")
add("mean_market_change_share",mean(a$market_change_weight_share,na.rm=TRUE),"share","annual_inflation_1700_1834.csv","average across134 annual changes, fixed base-weight shares")
for(i in seq_len(nrow(a))) for(col in c("index_point","inflation_pct","index_q05","index_q95")) {
 if(is.finite(a[[col]][i])) add(paste(col,a$year[i],sep="_"),a[[col]][i],if(col=="inflation_pct")"percent" else "1770s=100","annual_inflation_1700_1834.csv","conditional primary model")
}
hf <- v2_read("historical_functionals_primary.csv")
for(i in seq_len(nrow(hf))) for(col in c("point","q05","median","q95","prob_gt1","prob_gt3")) add(paste(hf$functional[i],col,sep="_"),hf[[col]][i],"ratio or probability","historical_functionals_primary.csv","joint conditional draws, not ratios of marginal quantiles")
peaks <- v2_read("peak_decomposition_summary.csv") |> filter(variant_id=="primary",target=="basket_including_transitory_wheat",window=="1700_1834")
for(col in c("point_peak_year","peak_q05","peak_median","peak_q95","prob_peak_before1825","prob_peak_before1826")) add(col,peaks[[col]],"year or probability","peak_decomposition_summary.csv","full1700-1834 window,1000 conditional joint draws")
v2_write(bind_rows(claims),"paper_claims_registry.csv")
stopifnot(!anyDuplicated(bind_rows(claims)$claim_id))

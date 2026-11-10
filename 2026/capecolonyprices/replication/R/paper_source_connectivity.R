suppressPackageStartupMessages({library(tidyverse);library(here);library(igraph)})
out <- here::here("replication","work","analysis")
fit <- readRDS(file.path(out,"annual_state_model.rds"))
panel <- fit$panel
nodes <- map_dfr(unique(panel$commodity),function(c) {
 p <- filter(panel,commodity==c)
 edges <- p |> group_split(year) |> map_dfr(function(z) {
   s <- unique(z$model_source_id)
   if(length(s)<2) return(tibble(from=character(),to=character(),year=integer()))
   a <- t(combn(s,2));tibble(from=a[,1],to=a[,2],year=z$year[1])
 })
 v <- p |> group_by(model_source_id) |> summarise(n_years=n_distinct(year),first=min(year),last=max(year),.groups="drop")
 g <- graph_from_data_frame(edges,directed=FALSE,vertices=rename(v,name=model_source_id))
 cc <- components(g)
 v |> mutate(commodity=c,overlap_component=unname(cc$membership[model_source_id]),
   n_overlap_components=cc$no,component_source_count=cc$csize[overlap_component],
   overlap_years=map_int(model_source_id,~n_distinct(edges$year[edges$from==.x|edges$to==.x])),
   identification_note=if_else(component_source_count==1,"no_direct_source_overlap_offset_prior_required",
     "within_component_relative_offsets_supported_between_components_prior_bridge_required"))
})
write_csv(nodes,file.path(out,"source_overlap_connectivity.csv"))
write_csv(nodes |> group_by(commodity) |> summarise(channels=n(),overlap_components=first(n_overlap_components),
 isolated_channels=sum(component_source_count==1),one_year_channels=sum(n_years==1),.groups="drop"),
 file.path(out,"source_connectivity_summary.csv"))

# Within-year source wedges are descriptive. They do not establish a causal
# treatment effect of Company regulation or independently validate duplicate data.
w <- panel |> mutate(role_group=case_when(str_detect(source_role,"VOC|government")~"official",
    source_role%in%c("auction","auction_transcribed","newspaper")~"market",TRUE~"other")) |>
 filter(role_group!="other") |> group_by(commodity,year,role_group) |>
 summarise(log_price=mean(log_price),source_channels=n(),.groups="drop") |>
 pivot_wider(names_from=role_group,values_from=c(log_price,source_channels)) |>
 filter(is.finite(log_price_market),is.finite(log_price_official)) |>
 mutate(log_official_market_wedge=log_price_official-log_price_market,
   regime=case_when(year<1770~"pre1770",year<1795~"1770_1794",TRUE~"1795_on"))
write_csv(w,file.path(out,"official_market_wedge.csv"))
print(nodes |> count(commodity,n_overlap_components),n=12)

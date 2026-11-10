suppressPackageStartupMessages({library(tidyverse);library(here)})
out<-here("replication","work","analysis");dir.create(out,recursive=TRUE,showWarnings=FALSE)
a<-read_csv(here("data","inputs","household_observed_annual.csv"),show_col_types=FALSE)
core<-c("chairs","tables","bedsteads","buckets","iron_pots","copper_kettles")
predict_path<-function(x,method,grid=1700:1834){
 z<-x |> filter(is.finite(price_observed_rd),price_observed_rd>0) |> arrange(year)
 if(method=="log_linear")return(exp(approx(z$year,log(z$price_observed_rd),xout=grid,rule=2)$y))
 # Fixed smoothing choices are evaluated in blocked holdouts, not tuned to fit a historical story.
 if(method=="spline_df12")return(exp(predict(smooth.spline(z$year,log(z$price_observed_rd),w=pmin(z$auctions,5),df=min(12,nrow(z)-2)),x=pmin(max(z$year),pmax(min(z$year),grid)))$y))
 stop("Unknown method")
}
methods<-c("log_linear","spline_df12")
cv<-map_dfr(unique(a$category),function(g){
 z<-filter(a,category==g)
 map_dfr(c(1L,3L,5L),function(gap)map_dfr(seq(1710,1820,10),function(start){
 test<-filter(z,between(year,start,start+gap-1),!is.na(price_observed_rd))
 if(nrow(test)==0)return(tibble())
 train<-z |> mutate(price_observed_rd=if_else(between(year,start,start+gap-1),NA_real_,price_observed_rd))
 map_dfr(methods,function(m)test |> transmute(category=g,method=m,gap=gap,start=start,year,actual=price_observed_rd,predicted=predict_path(train,m,year),log_error=log(predicted/actual)))
 }))
})
write_csv(cv,file.path(out,"household_blocked_holdouts.csv"))
scores<-cv |> group_by(method,gap) |> summarise(n=n(),rmse_log=sqrt(mean(log_error^2)),median_abs_log_error=median(abs(log_error)),.groups="drop")
write_csv(scores,file.path(out,"household_holdout_scores.csv"))
paths<-map_dfr(unique(a$category),function(g){
 z<-filter(a,category==g);obs<-filter(z,!is.na(price_observed_rd))
 map_dfr(methods,function(m)z |> mutate(method=m,price_estimated_rd=predict_path(z,m),nearest_observation_year=map_int(year,~obs$year[which.min(abs(obs$year-.x))]),
  distance_to_observation=abs(year-nearest_observation_year),
  status=case_when(year<min(obs$year)|year>max(obs$year)~"endpoint_carried_constant",m=="spline_df12"~"smoothed_estimate",is.na(price_observed_rd)~"log_interpolated",TRUE~"observed_auction_median")))
})
write_csv(paths,file.path(out,"household_annual_elementary_paths.csv"))
idx<-map_dfr(c("household8"),function(set){
 z<-if(set=="core6")filter(paths,category%in%core)else paths
 z<-z |> group_by(category,method) |> mutate(reference=exp(mean(log(price_estimated_rd[between(year,1770,1779)]))),relative=price_estimated_rd/reference) |> ungroup()
 z |> group_by(method,year) |> summarise(basket=set,goods=n(),index_geometric=100*exp(mean(log(relative))),index_equal_reference_share=100*mean(relative),observed_categories=sum(!is.na(price_observed_rd)),thin_categories=sum(auctions<3),filled_categories=sum(is.na(price_observed_rd)),max_anchor_distance=max(distance_to_observation),.groups="drop") |>
 group_by(method) |> arrange(year,.by_group=TRUE) |> mutate(inflation_pct=100*(index_geometric/lag(index_geometric)-1)) |> ungroup()
})
write_csv(idx,file.path(out,"household_annual_acquisition_indices_NOT_CPI.csv"))
checks<-tibble(check=c("135_years_per_basket_method","all_prices_positive","no_missing_index","first_year_change_unavailable","raw_source_rows_preserved"),
 passed=c(all(count(idx,basket,method)$n==135),all(paths$price_estimated_rd>0),all(is.finite(idx$index_geometric)),all(is.na(filter(idx,year==1700)$inflation_pct)),nrow(paths)==2*nrow(a)))
write_csv(checks,file.path(out,"household_annual_checks.csv"));stopifnot(all(checks$passed));print(scores);print(filter(idx,year%in%c(1700,1770,1800,1834)))

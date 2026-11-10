# captions, headers, explanatory prose and choice of rows shown in the paper.
suppressPackageStartupMessages({library(tidyverse);library(here)})
out <- here::here("replication","work","analysis")
tab <- here::here("replication","work","generated_tables")
dir.create(tab,showWarnings=FALSE)
read <- function(name) read_csv(file.path(out,name),show_col_types=FALSE)
fmt <- function(x,digits=1) ifelse(is.finite(x),formatC(x,format="f",digits=digits),"--")
rows <- function(x,name) writeLines(apply(as.data.frame(x),1,function(z) paste0(paste(z,collapse=" & ")," \\\\")),file.path(tab,name))
a <- read("annual_inflation_1700_1834.csv")
p <- read("annual_model_panel.csv")
c <- read("paper_table_coverage.csv") |> arrange(desc(base_weight))
coverage <- c |> transmute(commodity,quantity=fmt(quantity),unit=if_else(standard_unit=="m2","m$^2$",standard_unit),
 share=fmt(base_weight,3),observed_years,years_with_market_quotes,channels,channel_years,overlap_components)
total <- tibble(commodity="Total",quantity="--",unit="--",share="1.000",observed_years=n_distinct(p$year),
 years_with_market_quotes=n_distinct(p$year[p$source_role%in%c("auction","auction_transcribed","newspaper")]),
 channels=n_distinct(p$model_source_id),channel_years=nrow(p),overlap_components=sum(c$overlap_components))
rows(bind_rows(coverage,total),"coverage_rows.tex")
summary_rows <- function(x,groups) x |> group_by(across(all_of(groups))) |>
 summarise(years=n(),changes=sum(is.finite(inflation_pct)),mean_index=mean(index_point),
  mean_inflation_pct=mean(inflation_pct,na.rm=TRUE),
  compound_all_changes_pct=100*expm1(mean(log1p(inflation_pct/100),na.rm=TRUE)),
  zero_quote_years=sum(observed_level_weight_share==0),
  dated_level_share=mean(observed_level_weight_share),
  market_change_share=mean(market_change_weight_share,na.rm=TRUE),
  flat_official_share=mean(flat_official_change_weight_share,na.rm=TRUE),.groups="drop")
dec <- a |> mutate(period=as.character(year%/%10*10)) |> summary_rows("period")
reg <- a |> mutate(period=case_when(year<1770~"1700--1769",year<1795~"1770--1794",year<1825~"1795--1824",TRUE~"1825--1834")) |> summary_rows("period")
full <- mutate(a,period="1700--1834") |> summary_rows("period")
write_csv(dec,file.path(out,"paper_table_decades.csv"));write_csv(bind_rows(reg,full),file.path(out,"paper_table_regimes.csv"))
summary_tex <- function(z) z |> transmute(period,years_changes=paste(years,changes,sep=" / "),
 mean_index=fmt(mean_index),mean_inflation=fmt(mean_inflation_pct,2),compound=fmt(compound_all_changes_pct,2),
 zero_quote_years,dated_level_share=fmt(dated_level_share,3),market_change_share=fmt(market_change_share,3))
rows(summary_tex(dec),"decade_rows.tex");rows(summary_tex(bind_rows(reg,full)),"regime_rows.tex")
key_years <- c(1700,1787,1795,1800,1818,1822,1834)
variant_table <- function(z) {
 comp <- left_join(z,select(a,year,primary=index_point),by="year") |> group_by(variant_id) |>
  summarise(max_index_difference_pct=100*max(abs(index/primary-1)),.groups="drop")
 z |> filter(year%in%key_years) |> select(variant_id,year,index) |>
  pivot_wider(names_from=year,values_from=index) |> select(variant_id,all_of(as.character(key_years))) |>
  left_join(comp,by="variant_id")
}
v <- read("annual_structural_variants.csv")
vt <- variant_table(v)
write_csv(vt,file.path(out,"paper_table_structural_variants.csv"))
variant_tex <- function(z) z |> mutate(variant_id=paste0("\\variantlabel{",variant_id,"}"),across(where(is.numeric),fmt))
rows(variant_tex(vt),"structural_variant_rows.tex")
b <- map_dfr(c("quantity_variant.csv","geometric_consumption_comparator.csv","partial_market_reference_index.csv"),
 ~read(.x) |> select(variant_id,year,index,inflation_pct))
bt <- variant_table(b);write_csv(bt,file.path(out,"paper_table_basket_variants.csv"));rows(variant_tex(bt),"basket_variant_rows.tex")
h <- read("household_equipment_companion.csv") |> transmute(variant_id=paste0("equipment_",method),year,index=equipment_index)
s <- read("enslaved_sale_price_companion.csv") |> transmute(variant_id="individual_sale_price",year,index=nominal_sale_price_index)
eq <- read("equipment_allocation_variants.csv") |> filter(equipment_share==.1,method=="log_linear") |>
 transmute(variant_id="equipment_allocation_10pct_log_linear",year,index)
ct <- bind_rows(h,s,eq) |> filter(year%in%key_years) |> pivot_wider(names_from=year,values_from=index) |>
 select(variant_id,all_of(as.character(key_years)))
write_csv(ct,file.path(out,"paper_table_companions.csv"));rows(variant_tex(ct),"companion_rows.tex")
mon <- read("nominal_silver_comparator.csv") |>
 transmute(variant_id=monetary_scenario,year,index=index_silver) |> filter(year%in%key_years) |>
 pivot_wider(names_from=year,values_from=index) |> select(variant_id,all_of(as.character(key_years)))
write_csv(mon,file.path(out,"paper_table_silver_comparators.csv"));rows(variant_tex(mon),"silver_comparator_rows.tex")
# Preferred validation tables are generated separately from the paired v2 tests.
support_years <- a |> filter(observed_level_weight_share==0) |> select(year)
write_csv(support_years,file.path(out,"paper_zero_quote_years.csv"))
stopifnot(nrow(coverage)==9,nrow(vt)==43,nrow(dec)==14,nrow(reg)==4)
write_csv(tibble(file=list.files(tab,pattern="\\.tex$"),source="paper_tables.R from checked analysis CSVs"),file.path(tab,"table_manifest.csv"))

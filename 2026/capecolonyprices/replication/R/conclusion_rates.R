# Read-only summaries for the 10 November conclusion edit; no re-estimation.
suppressPackageStartupMessages({library(tidyverse);library(here)})
root <- here::here("replication","work")
a <- read_csv(file.path(here::here("data","series"),"annual_inflation_1700_1834.csv"),show_col_types=FALSE)
stopifnot(identical(a$year,as.numeric(1700:1834)))
stopifnot(max(abs(a$inflation_pct[-1]-100*(a$index_point[-1]/head(a$index_point,-1)-1)))<1e-10)
z <- a |> mutate(period=case_when(year<1770~"1700--1769",year<1795~"1770--1794",year<1825~"1795--1824",TRUE~"1825--1834"))
summarise_rates <- function(x) x |> group_by(period) |> summarise(changes=sum(is.finite(inflation_pct)),
  arithmetic_mean_pct=mean(inflation_pct,na.rm=TRUE),compound_pct=100*expm1(mean(log1p(inflation_pct/100),na.rm=TRUE)),.groups="drop")
s <- bind_rows(summarise_rates(z),summarise_rates(mutate(a,period="1700--1834")))
old <- read_csv(file.path(root,"analysis","paper_table_regimes.csv"),show_col_types=FALSE)
stopifnot(identical(s$period,old$period),max(abs(s$arithmetic_mean_pct-old$mean_inflation_pct))<1e-10)
dest <- file.path(root,"analysis")
write_csv(s,file.path(dest,"verified_period_rates.csv"))
write_csv(a |> filter(is.finite(inflation_pct)) |> arrange(desc(inflation_pct)) |> slice_head(n=5) |>
 select(year,inflation_pct,inflation_q05,inflation_q95),file.path(dest,"largest_annual_increases.csv"))
print(s,width=150)
print(a |> slice_max(inflation_pct,n=1) |> select(year,inflation_pct,inflation_q05,inflation_q95))

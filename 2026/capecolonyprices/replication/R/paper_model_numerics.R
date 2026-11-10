suppressPackageStartupMessages({library(tidyverse);library(here)})
source(here("replication","R","state_model.R"))
out <- here("replication","work","analysis")
fit <- readRDS(file.path(out,"annual_state_model.rds"))
p <- fit$panel
starts <- list(log(c(.02,.15,.1,.3,.3,.4,.1)),log(c(.15,.04,.3,.6,.1,.2,.3)))
r <- map_dfr(seq_along(starts),function(i) {
 z <- fit_price_panel(p,inits=starts[[i]],maxit=250)
 tibble(test="full_sample_alternative_start",start_id=i,convergence=z$optim$convergence,
   negative_log_likelihood=z$optim$value,nll_difference=z$optim$value-fit$optim$value,
   max_log_price_difference=max(abs(price_fit_locations(z)-price_fit_locations(fit))))
})
for(start_year in c(1723L,1811L)) {
 test <- p |> filter(commodity=="Wheat",between(year,start_year,start_year+9L))
 warm <- fit_price_panel(p,holdout=test,inits=fit$optim$par,maxit=250)
 cold <- fit_price_panel(p,holdout=test,maxit=250)
 r <- bind_rows(r,tibble(test="holdout_default_vs_warm_start",start_id=start_year,
   convergence=max(warm$optim$convergence,cold$optim$convergence),negative_log_likelihood=cold$optim$value,
   nll_difference=cold$optim$value-warm$optim$value,
   max_log_price_difference=max(abs(price_fit_locations(cold)-price_fit_locations(warm)))))
 write_csv(r,file.path(out,"model_numerical_checks.csv"))
}
print(r,width=150)
stopifnot(all(r$convergence==0),all(abs(r$nll_difference)<.01),all(r$max_log_price_difference<.01))

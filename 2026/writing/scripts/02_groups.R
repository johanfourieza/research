# Indicators by affiliation group for papers issued since January 2023: shares by group and year,
# and linear probability models within series and half-year (standard errors clustered by series).
# Input: data/papers.csv. Output: results/shares_by_group.csv, results/shares_by_group_year.csv,
# results/within_series_estimates.csv.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(fixest)})

p <- read.csv('data/papers.csv') %>% filter(group != '') %>%
  mutate(group = factor(group, levels = c('English', 'Mixed', 'Non-English')), year = period %/% 2)
stopifnot(nrow(p) == 6939, all(p$period >= 4046 & p$period <= 4053))

ind <- c('disclosed', 'marker', 'colon_flag')
long <- p %>% pivot_longer(all_of(ind), names_to = 'indicator', values_to = 'value') %>% filter(!is.na(value))
write.csv(long %>% group_by(indicator, group) %>% summarise(share = 100 * mean(value), n = n(), .groups = 'drop'),
          'results/shares_by_group.csv', row.names = FALSE)
write.csv(long %>% group_by(indicator, group, year) %>% summarise(share = 100 * mean(value), n = n(), .groups = 'drop'),
          'results/shares_by_group_year.csv', row.names = FALSE)

est <- bind_rows(lapply(ind, function(v) {
  z <- p %>% filter(!is.na(.data[[v]]))
  fit <- function(zz) feols(as.formula(paste(v, '~ i(group, ref = "English") | series^period')), zz, vcov = ~series)
  tidy <- function(f, spec, zz) { ct <- coeftable(f); ci <- confint(f)
    data.frame(indicator = v, spec = spec, term = rownames(ct), estimate = 100 * ct[, 1], se = 100 * ct[, 2],
               ci_low = 100 * ci[, 1], ci_high = 100 * ci[, 2], p = ct[, 4], n = nobs(f),
               base_mean = 100 * mean(zz[[v]][zz$group == 'English'])) }
  z3 <- filter(z, series != 'arxiv/economics'); z4 <- filter(z, year == 2023)
  bind_rows(tidy(fit(z), 'groups', z), tidy(fit(z3), 'groups_excl_arxiv', z3), tidy(fit(z4), 'groups_2023', z4))
}))
write.csv(est, 'results/within_series_estimates.csv', row.names = FALSE)

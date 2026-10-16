# Half-year shares of the two prose indicators and disclosed AI use (Figure 1, panel A),
# and paper-level correlations among papers on which all three indicators can be measured.
# Input: data/papers.csv. Output: results/trends.csv, results/correlations.csv.
suppressPackageStartupMessages({library(dplyr); library(tidyr)})

p <- read.csv('data/papers.csv')
stopifnot(nrow(p) == 42704)
vars <- c('disclosed', 'marker', 'colon_flag')

# Each indicator on its own denominator: the papers on which it can be measured
tr <- p %>% select(paper_id, period, all_of(vars)) %>%
  pivot_longer(all_of(vars), names_to = 'indicator', values_to = 'value') %>%
  filter(!is.na(value)) %>% group_by(period, indicator) %>%
  summarise(share = 100 * mean(value), n = n(), .groups = 'drop') %>%
  mutate(time = period %/% 2 + (period %% 2) / 2) %>%
  select(time, indicator, share, n)
write.csv(tr, 'results/trends.csv', row.names = FALSE)

cors <- function(z, lab) {
  z <- z %>% filter(!is.na(disclosed), !is.na(marker), !is.na(colon_flag))
  data.frame(sample = lab, n = nrow(z),
             r_disclosed_marker = cor(z$disclosed, z$marker),
             r_disclosed_colon  = cor(z$disclosed, z$colon_flag),
             r_marker_colon     = cor(z$marker, z$colon_flag),
             share_disclosed = mean(z$disclosed), share_marker = mean(z$marker), share_colon = mean(z$colon_flag),
             disclosing_flagged = mean(z$marker[z$disclosed == 1]),
             flagged_disclosing = mean(z$disclosed[z$marker == 1]))
}
cr <- bind_rows(cors(filter(p, period >= 4046), 'Issued January 2023 onward'),
                cors(filter(p, period >= 4050), 'Issued January 2025 onward'))
write.csv(cr, 'results/correlations.csv', row.names = FALSE)

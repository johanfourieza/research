# Stops with an error if any number reported in Section 8 or Figure 1 of the paper is not reproduced.
# Inputs: data/*.csv and the outputs of scripts 01-02.
suppressPackageStartupMessages({library(dplyr)})
chk <- function(label, value, target, digits = 1) {
  ok <- isTRUE(all.equal(round(value, digits), target))
  cat(sprintf('%-58s %12s  target %-8s %s\n', label, format(round(value, digits), nsmall = digits), target,
              if (ok) 'OK' else 'MISMATCH'))
  if (!ok) stop('Not reproduced: ', label)
}

p  <- read.csv('data/papers.csv')
tr <- read.csv('results/trends.csv')
cr <- read.csv('results/correlations.csv')
sg <- read.csv('results/shares_by_group.csv')
es <- read.csv('results/within_series_estimates.csv')
ac <- read.csv('data/affiliation_classification.csv')
share <- function(ind, per) tr$share[tr$indicator == ind & tr$period_code == per]
tr$period_code <- tr$time * 2

chk('Working papers', nrow(p), 42704, 0)
chk('Series', n_distinct(p$series), 32, 0)
pre <- p %>% filter(period < 4046, !is.na(marker))
chk('Detector share before 2023 (%; text: fewer than 1 in 200)', 100 * mean(pre$marker), 0.2)
stopifnot(mean(pre$marker) < 1 / 200)
chk('Detector share, January-June 2026 (%)', share('marker', 4052), 15.6)
chk('High colon rate, July-September 2026 (%)', share('colon_flag', 4053), 15.2)
chk('Detector share, July-September 2026 (%)', share('marker', 4053), 9.0)
c23 <- cr[cr$sample == 'Issued January 2023 onward', ]
chk('Papers with all three indicators since 2023', c23$n, 13103, 0)
chk('Correlation, detector flag and disclosed use', c23$r_disclosed_marker, 0.07, 2)

g <- table(p$group[p$group != ''])
chk('Papers with classified affiliations since 2023', sum(g), 6939, 0)
chk('  English-speaking countries only', g[['English']], 3703, 0)
chk('  Elsewhere only', g[['Non-English']], 1427, 0)
chk('  Both', g[['Mixed']], 1809, 0)
chk('Detector share, affiliations elsewhere (%)', sg$share[sg$indicator == 'marker' & sg$group == 'Non-English'], 8.3)
chk('Detector share, English-speaking countries (%)', sg$share[sg$indicator == 'marker' & sg$group == 'English'], 5.0)
e <- function(spec) es[es$indicator == 'marker' & es$spec == spec & es$term == 'group::Non-English', ]
chk('Within series x half-year difference (pp)', e('groups')$estimate, 2.8)
chk('  95% CI lower', e('groups')$ci_low, 1.5)
chk('  95% CI upper', e('groups')$ci_high, 4.1)
chk('Excluding arXiv (pp)', e('groups_excl_arxiv')$estimate, 2.0)
chk('  95% CI lower', e('groups_excl_arxiv')$ci_low, 0.5)
chk('  95% CI upper', e('groups_excl_arxiv')$ci_high, 3.6)

chk('Distinct affiliations classified', nrow(ac), 1838, 0)
chk('Classifier agreement (%)', 100 * mean(ac$codex == ac$claude), 98.5)
chk('Disagreements adjudicated', sum(ac$codex != ac$claude), 27, 0)
cat('All Section 8 numbers reproduced.\n')

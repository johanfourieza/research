# Figure 1: (A) detector flag and high colon rate by half-year, all papers;
# (B) detector flag by affiliation group, papers issued since 2023.
# Inputs: results/trends.csv, results/shares_by_group_year.csv. Output: results/figure1.{pdf,svg,png}.
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(patchwork)})
source('scripts/leap_style.R')
plum <- LEAP_COLORS[['plum']]; sage <- LEAP_COLORS[['sage']]; blue <- LEAP_COLORS[['blue']]

a <- read.csv('results/trends.csv') %>% filter(indicator %in% c('marker', 'colon_flag')) %>%
  mutate(label = factor(recode(indicator, marker = 'Detector flag', colon_flag = 'High colon rate'),
                        levels = c('Detector flag', 'High colon rate')))
gA <- ggplot(a, aes(time, share, colour = label, shape = label, linetype = label)) +
  geom_vline(xintercept = 2022.92, linetype = '22', colour = LEAP_NONSIG_COLOR) +
  geom_line(linewidth = .6) + geom_point(size = 1.5) +
  scale_colour_manual(values = c('Detector flag' = plum, 'High colon rate' = sage)) +
  scale_shape_manual(values = c(17, 15)) + scale_linetype_manual(values = c('solid', '44')) +
  scale_x_continuous(breaks = c(2015, 2018, 2021, 2024, 2026.5), labels = c('2015', '2018', '2021', '2024', 'Jul-Sep\n2026')) +
  scale_y_continuous(limits = c(0, 17)) +
  labs(x = 'Half-year of issue', y = 'Working papers (%)') +
  theme_leap(10) + theme(text = element_text(face = 'plain'), legend.title = element_blank(), legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

b <- read.csv('results/shares_by_group_year.csv') %>%
  filter(indicator == 'marker', group %in% c('English', 'Non-English')) %>%
  mutate(grp = factor(recode(group, English = 'Affiliations in English-speaking countries', `Non-English` = 'Affiliations elsewhere'),
                      levels = c('Affiliations in English-speaking countries', 'Affiliations elsewhere')))
stopifnot(nrow(b) == 8)
gB <- ggplot(b, aes(year, share, colour = grp, shape = grp, linetype = grp)) +
  geom_line(linewidth = .6) + geom_point(size = 1.8) +
  scale_colour_manual(values = c(blue, plum)) + scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c('solid', '22')) +
  scale_x_continuous(breaks = 2023:2026, labels = c('2023', '2024', '2025', '2026\n(Jan-Sep)')) +
  scale_y_continuous(limits = c(0, 17)) +
  labs(x = 'Year of issue', y = 'Flagged by the detector (%)') +
  theme_leap(10) + theme(text = element_text(face = 'plain'), legend.title = element_blank(), legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

g <- (gA | gB) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 10, colour = '#2D2D2D'),
  legend.key.height = grid::unit(9, 'pt'), legend.spacing.y = grid::unit(0, 'pt'), legend.margin = margin(0, 0, 0, 0),
  legend.text = element_text(size = 9))
save_leap_fig('results/figure1.pdf', g, 6.5, 3.7)

# Run from any directory: Rscript path/to/plot_review_lexis.R
# A Lexis diagram of manuscripts: article histories from Explorations in Economic
# History set against model releases. Calendar time runs across and months since
# submission run up, so each article climbs a diagonal from the date it was received.
# Inputs: ../data/article_histories.csv and ../data/model_releases.csv.
# Outputs: ../figures/review_lexis.{png,pdf} and ../figures/review_by_year.csv.
suppressPackageStartupMessages({library(tidyverse); library(ggrepel)})
args <- commandArgs(trailingOnly = FALSE)
script <- sub('^--file=', '', args[grepl('^--file=', args)][1])
root <- dirname(dirname(normalizePath(script)))
data_dir <- file.path(root, 'data')
out <- file.path(root, 'figures')

# ============================================================================
# LEAP VISUAL IDENTITY - Publication-Ready Graph Style
# ============================================================================

# LEAP colour palette
LEAP_COLORS <- c(
  plum  = "#5C2346",
  blue  = "#3D8EB9",
  sage  = "#6B8E5E",
  gold  = "#D4A03E",
  rose  = "#A34466",
  teal  = "#45808B",
  earth = "#8B6B3D",
  mint  = "#97C5B0"
)
LEAP_CYCLE <- unname(LEAP_COLORS)

# Utility colours
LEAP_NONSIG_COLOR <- "#AAAAAA"

# LEAP ggplot2 theme
theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      # Text
      text = element_text(family = "sans"),
      plot.title = element_text(
        size = 11, face = "bold", color = "#2D2D2D",
        margin = ggplot2::margin(b = 12), hjust = 0
      ),
      axis.title = element_text(size = 10, color = "#4A4A4A"),
      axis.text = element_text(size = 9, color = "#5A5A5A"),
      legend.text = element_text(size = 9),

      # Spines: only bottom and left
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),

      # Grid: horizontal only, light
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      # Ticks
      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),

      # Legend: no frame, no title by default
      legend.background = element_blank(),
      legend.key = element_blank(),

      # Background: pure white
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),

      # Margins
      plot.margin = ggplot2::margin(10, 10, 10, 10),

      # Strip text for facets
      strip.text = element_text(size = 10, face = "bold", color = "#2D2D2D")
    )
}

# Helper: save LEAP figure in both PNG and PDF at high resolution
save_leap_fig <- function(fig_path, plot, width, height, dpi = 600) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height, device = grDevices::cairo_pdf)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

# ============================================================================
# Data
# ============================================================================

releases <- read_csv(file.path(data_dir, 'model_releases.csv'), show_col_types = FALSE) |>
  mutate(series = paste(provider, 'release'))

months_since <- function(date, received) as.numeric(date - received) / 30.44
papers <- read_csv(file.path(data_dir, 'article_histories.csv'), show_col_types = FALSE) |>
  rename(online = available_online) |>
  mutate(n_releases = map2_int(received, accepted, ~ sum(releases$date > .x & releases$date <= .y)))
stopifnot(!anyNA(papers$received), !anyNA(papers$accepted), !anyNA(papers$online),
          all(papers$accepted >= papers$received), all(papers$online >= papers$accepted))

# ============================================================================
# Figure
# ============================================================================

# The axis starts in January 2022; articles received earlier enter from the left edge.
window <- c(max(min(papers$received) - 30, as.Date('2022-01-01')),
            max(c(papers$online, releases$date)) + 25)
shown <- filter(releases, date >= window[1])
tallest <- max(months_since(papers$online, papers$received))
line_top <- tallest + 1.5          # release lines stop here; their names sit above
panel_top <- line_top * 1.36

fig <- ggplot(papers) +
  geom_segment(data = shown, aes(x = date, xend = date, y = 0, yend = line_top, colour = series),
               linewidth = .35, alpha = .8) +
  geom_text_repel(data = shown, aes(date, line_top, label = label), colour = '#4A4A4A',
                  angle = 90, hjust = 0, direction = 'x', nudge_y = .35, size = 2.3,
                  box.padding = .02, point.padding = 0, segment.size = .2,
                  segment.colour = LEAP_NONSIG_COLOR, min.segment.length = 0, seed = 42,
                  max.overlaps = Inf, ylim = c(line_top + .3, NA)) +
  geom_segment(aes(x = received, xend = online, y = 0, yend = months_since(online, received)),
               colour = '#5C2346', linewidth = .3, alpha = .35) +
  geom_point(aes(revised, months_since(revised, received), colour = 'Revised'),
             shape = 21, fill = 'white', size = 2.7, stroke = .6) +
  geom_point(aes(accepted, months_since(accepted, received), colour = 'Accepted'),
             shape = 16, size = 1.5) +
  geom_point(aes(online, months_since(online, received), colour = 'Available online'),
             shape = 18, size = 2.3) +
  scale_colour_manual(values = c('OpenAI release' = '#3D8EB9', 'Anthropic release' = '#D4A03E',
                                 'Revised' = '#5C2346', 'Accepted' = '#5C2346',
                                 'Available online' = '#45808B'),
                      breaks = c('Revised', 'Accepted', 'Available online',
                                 'OpenAI release', 'Anthropic release')) +
  scale_x_date(date_breaks = '6 months', date_labels = '%b\n%Y', expand = expansion(0)) +
  scale_y_continuous(breaks = seq(0, floor(line_top / 3) * 3, 3), limits = c(0, panel_top),
                     expand = expansion(0)) +
  coord_cartesian(xlim = window) +  # clips, rather than drops, the lines that start earlier
  labs(x = NULL, y = 'Months since submission', colour = NULL) +
  theme_leap() +
  theme(legend.position = 'bottom')

# Narrower than the LEAP default of 10 x 6 so the labels stay legible at page width.
save_leap_fig(file.path(out, 'review_lexis.png'), fig, width = 8, height = 5)
# These numbers are quoted in Section 3 of the comment and in the figure notes.
cat(nrow(papers), 'articles; median months to acceptance:',
    round(median(months_since(papers$accepted, papers$received)), 1),
    '| releases between submission and acceptance: median', median(papers$n_releases),
    ', range', min(papers$n_releases), 'to', max(papers$n_releases), '\n')
# By year of acceptance: median months from submission to acceptance and median number of
# releases over that interval. Quoted in Section 3; also written to review_by_year.csv.
by_year <- papers |>
  mutate(year = year(accepted), months = months_since(accepted, received)) |>
  group_by(year) |>
  summarise(articles = n(), median_months = round(median(months), 1),
            median_releases = median(n_releases))
write_csv(by_year, file.path(out, 'review_by_year.csv'))
print(as.data.frame(by_year))

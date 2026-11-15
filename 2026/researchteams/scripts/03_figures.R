# Figures 1 and 2 of the letter.
#
# Reads   results/page_split_estimates.csv   (written by 02_page_split.R)
#         data/half_year_shares.csv
# Writes  results/Figure_1.{pdf,png}, results/Figure_2.{pdf,png}
#
# The two series in each figure differ by colour, by point shape and, in
# Figure 2, by line type, so the figures can be read without colour.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

plum <- "#5C2346"
blue <- "#3D8EB9"
grey <- "#AAAAAA"

theme_letter <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      axis.title = element_text(size = base_size, colour = "#4A4A4A"),
      axis.text = element_text(size = base_size - 1, colour = "#5A5A5A"),
      legend.text = element_text(size = base_size - 1),
      axis.line.x.bottom = element_line(colour = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(colour = "#4A4A4A", linewidth = 0.8),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 14, 6, 10)
    )
}

save_figure <- function(stem, plot) {
  ggsave(file.path("results", paste0(stem, ".pdf")), plot, width = 7,
         height = 4.4, device = cairo_pdf, bg = "white")
  ggsave(file.path("results", paste0(stem, ".png")), plot, width = 7,
         height = 4.4, dpi = 300, bg = "white")
}

# ---- Figure 1: additional main-text and appendix pages -----------------------
split <- read_csv("results/page_split_estimates.csv", show_col_types = FALSE) |>
  filter(outcome %in% c("main_pages", "appendix_pages")) |>
  mutate(
    group = factor(term,
                   levels = c("disclosed", "disclosed_oper", "disclosed_writing_only"),
                   labels = c("Any disclosure", "Operational\ndisclosure",
                              "Writing-only\ndisclosure")),
    part = factor(outcome, levels = c("main_pages", "appendix_pages"),
                  labels = c("Main-text pages", "Appendix pages")),
    # 95% confidence interval from the normal approximation
    lo = coefficient - qnorm(0.975) * se,
    hi = coefficient + qnorm(0.975) * se
  )
stopifnot(nrow(split) == 6)

dodge <- position_dodge(width = 0.45)
figure_1 <- ggplot(split, aes(group, coefficient, colour = part, shape = part)) +
  geom_hline(yintercept = 0, colour = grey, linewidth = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.8,
                position = dodge) +
  geom_point(size = 3, position = dodge) +
  scale_colour_manual(values = c(blue, plum)) +
  scale_shape_manual(values = c(17, 16)) +
  scale_y_continuous(breaks = seq(-8, 8, 2)) +
  labs(x = NULL, y = "Additional pages", colour = NULL, shape = NULL) +
  theme_letter()
save_figure("Figure_1", figure_1)

# ---- Figure 2: credited assistance and disclosure by half-year ----------------
# `scope == "institutional"` is the 23 series other than arXiv. `incomplete`
# marks the second half of 2026, which covers July to October only.
trend <- read_csv("data/half_year_shares.csv", show_col_types = FALSE) |>
  filter(scope == "institutional", outcome %in% c("any_ra", "disclosed")) |>
  mutate(series = factor(outcome, levels = c("any_ra", "disclosed"),
                         labels = c("Papers crediting any research assistant",
                                    "Papers disclosing AI use")))
stopifnot(nrow(trend) == 20)

figure_2 <- ggplot(trend, aes(time, share_pct, colour = series,
                              linetype = series, shape = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = filter(trend, !incomplete), size = 2.6) +
  # hollow points for the incomplete half-year
  geom_point(data = filter(trend, incomplete), size = 2.8, stroke = 1,
             show.legend = FALSE) +
  geom_point(data = filter(trend, incomplete), size = 1.3, colour = "white",
             show.legend = FALSE) +
  scale_colour_manual(values = c(plum, blue)) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(
    breaks = seq(2022, 2026.5, 0.5),
    labels = c("2022\nH1", "H2", "2023\nH1", "H2", "2024\nH1", "H2",
               "2025\nH1", "H2", "2026\nH1", "H2*"),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10),
                     labels = \(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Issue half-year", y = "Share of papers", colour = NULL,
       linetype = NULL, shape = NULL) +
  theme_letter() +
  theme(legend.key.width = unit(1.6, "cm"))
save_figure("Figure_2", figure_2)

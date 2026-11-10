# leap_theme.R — LEAP visual identity (from /leapstyle), shared by all figures

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
LEAP_NONSIG_COLOR <- "#AAAAAA"

scale_fill_leap  <- function(...) scale_fill_manual(values = LEAP_CYCLE, ...)
scale_color_leap <- function(...) scale_color_manual(values = LEAP_CYCLE, ...)

theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = 11, face = "bold", color = "#2D2D2D",
                                margin = ggplot2::margin(b = 12), hjust = 0),
      axis.title = element_text(size = 10, color = "#4A4A4A"),
      axis.text = element_text(size = 9, color = "#5A5A5A"),
      legend.text = element_text(size = 9),
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      strip.text = element_text(size = 10, face = "bold", color = "#2D2D2D")
    )
}

save_leap_fig <- function(fig_path, plot, width = 10, height = 6, dpi = 600) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

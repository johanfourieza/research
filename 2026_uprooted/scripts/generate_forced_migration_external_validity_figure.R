library(ggplot2)
library(data.table)

wp_dir <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta/WorkingPaperNew"
fig_dir <- file.path(wp_dir, "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

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
LEAP_NONSIG_COLOR <- "#AAAAAA"

theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(
        size = 11, face = "bold", color = "#2D2D2D",
        margin = ggplot2::margin(b = 12), hjust = 0
      ),
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

save_leap_fig <- function(fig_path, plot, width, height, dpi = 600) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

plot_df <- data.table(
  label = c(
    "South Africa: White -> Europe",
    "South Africa: Indian -> India",
    "Cape Colony: slave origins -> origin countries",
    "Atlantic world: trans-Atlantic corridors",
    "Atlantic world: trans-Atlantic corridors\n(positive corridors only)",
    "Atlantic world: intra-American corridors"
  ),
  family = c(
    "Free migration",
    "Semi-free migration",
    "Forced migration",
    "Forced migration",
    "Forced migration",
    "Forced migration"
  ),
  beta = c(0.578, 0.078, 0.010, 0.0255, 0.0000, -0.0408),
  se = c(0.014, 0.025, 0.008, 0.0151, 0.0437, 0.0805)
)

plot_df[, lower := beta - 1.96 * se]
plot_df[, upper := beta + 1.96 * se]
plot_df[, label := factor(label, levels = rev(label))]

pal <- c(
  "Free migration"      = "#5C2346",
  "Semi-free migration" = "#D4A03E",
  "Forced migration"    = "#AAAAAA"
)

p <- ggplot(plot_df, aes(x = beta, y = label, color = family)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0.18, linewidth = 0.8) +
  geom_point(size = 2.8) +
  scale_color_manual(values = pal) +
  labs(
    x = "Coefficient on historical migration measure",
    y = NULL,
    color = NULL,
    title = "Forced migration does not generate the same durable ties",
    subtitle = "South African and Atlantic-world estimates cluster around zero, unlike free and semi-free migration"
  ) +
  theme_leap() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(hjust = 1)
  )

save_leap_fig(file.path(fig_dir, "Fig18_forced_migration_external_validity.png"),
              p, width = 8.5, height = 4.8)

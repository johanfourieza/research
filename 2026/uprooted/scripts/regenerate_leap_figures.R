# Standalone script to regenerate the two figures that needed LEAP colour fixes
# Run from project root

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(forcats)
library(sf)

data_dir <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta/Data"
out_dir  <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta/WorkingPaperNew/Figures"

# LEAP visual identity
LEAP_COLORS <- c(
  plum  = "#5C2346", blue  = "#3D8EB9", sage  = "#6B8E5E", gold  = "#D4A03E",
  rose  = "#A34466", teal  = "#45808B", earth = "#8B6B3D", mint  = "#97C5B0"
)
LEAP_NONSIG_COLOR <- "#AAAAAA"

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

save_leap_fig <- function(fig_path, plot, width, height, dpi = 600) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

# ---- Figure 1: NZ birthplace bar chart ----

cat("Regenerating NZ birthplace bar chart...\n")
nz_birthplace <- fread(file.path(data_dir, "nz_region_birthplace.csv"))

nz_bar_data <- nz_birthplace |>
  select(gadm1_name, pct_english, pct_scottish, pct_irish, pct_nz_born) |>
  mutate(pct_other = 1 - pct_english - pct_scottish - pct_irish - pct_nz_born) |>
  pivot_longer(-gadm1_name, names_to = "origin", values_to = "share") |>
  mutate(
    origin = factor(origin,
                    levels = c("pct_nz_born", "pct_english", "pct_scottish",
                               "pct_irish", "pct_other"),
                    labels = c("NZ-born", "English", "Scottish", "Irish", "Other")),
    gadm1_name = fct_reorder(gadm1_name, share, .fun = function(x) x[3])
  )

fig_nz_bar <- ggplot(nz_bar_data,
                      aes(x = gadm1_name, y = share, fill = origin)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("NZ-born" = "#AAAAAA",
                                "English" = "#3D8EB9",
                                "Scottish" = "#5C2346",
                                "Irish" = "#6B8E5E",
                                "Other" = "#97C5B0")) +
  labs(
    x = NULL, y = "Share of population",
    title = "1881 Census: birthplace composition of NZ borough populations",
    fill = "Birthplace"
  ) +
  theme_leap() +
  theme(legend.position = "bottom")

save_leap_fig(file.path(out_dir, "Fig_nz_birthplace_bar.png"),
              fig_nz_bar, width = 10, height = 7)

# ---- Figure 2: Cape Colony farm name map ----

cat("Regenerating Cape Colony farm name map...\n")
farm_points <- fread(file.path(data_dir, "farm_classifications.csv"))

za_boundary <- st_read(file.path(data_dir, "za_gadm2.gpkg"), quiet = TRUE) |>
  filter(grepl("^ZAF\\.9\\.|^ZAF\\.1\\.|^ZAF\\.8\\.", GID_2))

farm_sf <- st_as_sf(farm_points |> filter(!is.na(lon), !is.na(lat)),
                     coords = c("lon", "lat"), crs = 4326)

fig_cape_map <- ggplot() +
  geom_sf(data = za_boundary, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = farm_sf, aes(color = language), size = 0.6, alpha = 0.7) +
  scale_color_manual(
    values = c("Dutch" = "#AAAAAA", "English" = "#5C2346",
               "French" = "#3D8EB9", "German" = "#D4A03E"),
    name = "Farm name origin"
  ) +
  coord_sf(xlim = c(17.5, 28), ylim = c(-35, -29.5)) +
  annotate("text", x = 18.5, y = -34.0, label = "Cape Town",
           size = 2.5, fontface = "italic") +
  annotate("text", x = 19.1, y = -33.85, label = "Franschhoek",
           size = 2, fontface = "italic", color = "#3D8EB9") +
  annotate("text", x = 26.5, y = -33.3, label = "Grahamstown",
           size = 2, fontface = "italic", color = "#5C2346") +
  annotate("text", x = 25.7, y = -33.95, label = "Port Elizabeth",
           size = 2, fontface = "italic") +
  labs(title = "Farm name origins in the Cape Colony, c.1850") +
  theme_leap() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_blank(),
        legend.position = "bottom")

save_leap_fig(file.path(out_dir, "Fig16_cape_colony_panel.png"),
              fig_cape_map, width = 10, height = 6)

cat("\nDone.\n")

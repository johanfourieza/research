# figures_v2.R -- core publication figures for the v2 Cape provisioning-cost paper.
#
# Usage, from the project root:
#   Rscript replication/R/figures_v2.R build     render, check and QA in figures_staging
#   Rscript replication/R/figures_v2.R promote   copy inspected staging outputs to figures
#
# Every plotted number comes from a file listed, with its MD5, in
# cannot read an unlisted file through inp(). Nothing is fitted, smoothed or re-estimated.
# Graphs carry no titles, subtitles, panel headings or explanatory notes; captions are
# written elsewhere.

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(patchwork); library(scales); library(grid)
})
v2 <- here::here("replication","work")
stage <- file.path(v2,"figures")
dir.create(stage,recursive=TRUE,showWarnings=FALSE)
manifest_in <- read_csv(here::here("docs","figure_inputs.csv"),show_col_types=FALSE)
root_paths <- file.path(here::here(),manifest_in$path)
stopifnot(all(file.exists(root_paths)))
manifest_in$md5 <- unname(tools::md5sum(root_paths))
inp <- function(name,fig=NA_character_) {
 hit <- which(basename(manifest_in$path)==name)
 stopifnot(length(hit)==1L)
 root_paths[hit]
}
rd <- function(name) read_csv(inp(name),show_col_types=FALSE,guess_max=Inf)
annual <- rd("annual_inflation_1700_1834.csv")
elementary <- rd("elementary_annual_state.csv")
basket <- rd("basket_quantities_weights.csv")
panel <- rd("annual_model_panel.csv")
offsets <- rd("source_offset_estimates.csv")
loading <- rd("channel_family_loading_map.csv")
bridges <- rd("direct_1795_bridges.csv")
variant_summaries <- rd("annual_variant_conditional_summaries.csv")
variants <- rd("annual_structural_variants.csv")
settings <- rd("v2_variant_settings.csv")
functionals <- rd("historical_functionals_all.csv")
peak_draws <- rd("peak_decomposition_draws.csv")
peak_summary <- rd("peak_decomposition_summary.csv")
pass_annual <- rd("fixed_parameter_pass_through_annual.csv")
pass_summary <- rd("pass_through_1820s_summary.csv")
equipment <- rd("household_equipment_companion.csv")
enslaved <- rd("enslaved_sale_price_companion.csv")
enslaved_decadal <- rd("enslaved_recorded_characteristics_decadal_companion.csv")
events <- rd("historical_events.csv")
primary_draws <- readRDS(inp("annual_index_joint_draws.rds"))
constant_draws <- readRDS(inp("offsets_constant_joint.rds"))

stopifnot(nrow(annual) == 135, !anyDuplicated(annual$year), nrow(elementary) == 1215,
          nrow(panel) == 632, n_distinct(panel$model_source_id) == 69, nrow(bridges) == 5,
          abs(mean(annual$index_point[between(annual$year, 1770, 1779)]) - 100) < 1e-8)

# Design system ---------------------------------------------------------------------
fs <- 9.6; txt <- fs / .pt
plum <- "#5C2346"; blue <- "#3D8EB9"; sage <- "#6B8E5E"; gold <- "#D4A03E"
rose <- "#A34466"; teal <- "#45808B"; earth <- "#8B6B3D"; mint <- "#97C5B0"; grey <- "#AAAAAA"
ink <- "#2D2D2D"; soft <- "#4A4A4A"; rule <- "#E4E4E4"
design_colour <- c(primary = plum, constant = blue)
year_limits <- c(1699.5, 1834.5)

theme_fig <- function() {
  theme_minimal(base_size = 10, base_family = "sans") +
    theme(text = element_text(family = "sans", face = "plain", colour = soft),
          plot.title = element_blank(), plot.subtitle = element_blank(), strip.text = element_blank(),
          axis.text = element_text(size = fs, colour = soft),
          axis.title = element_text(size = fs + 0.4, colour = soft),
          axis.line.x.bottom = element_line(colour = soft, linewidth = 0.55),
          axis.line.y.left = element_line(colour = soft, linewidth = 0.55),
          axis.ticks = element_line(colour = soft, linewidth = 0.35),
          axis.ticks.length = unit(2.4, "pt"),
          panel.grid.major.y = element_line(colour = rule, linewidth = 0.4),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none", legend.text = element_text(size = fs, colour = soft),
          legend.title = element_text(size = fs, colour = soft),
          plot.margin = ggplot2::margin(3, 8, 3, 6))
}
no_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.line.x.bottom = element_blank(), axis.title.x = element_blank())
no_y_axis <- theme(panel.grid.major.y = element_blank(), axis.line.y.left = element_blank(),
                   axis.ticks.y = element_blank())
x_years <- scale_x_continuous(breaks = seq(1700, 1825, 25), limits = year_limits, expand = c(0, 0))
x_none <- scale_x_continuous(limits = year_limits, expand = c(0, 0))
index_breaks <- c(100, 150, 200, 300, 400, 600)
index_limits <- c(min(annual$index_q05) * 0.97, max(annual$index_q95) * 1.03)

figure_log <- list(); check_log <- list()
save_fig <- function(id, stem, plot, height, width = 6.5, placement, inputs, encoding, metadata) {
  png <- file.path(stage, paste0(stem, ".png")); pdf <- file.path(stage, paste0(stem, ".pdf"))
  ggsave(png, plot, width = width, height = height, dpi = 600, device = ragg::agg_png, bg = "white")
  ggsave(pdf, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  stopifnot(all(inputs %in% basename(manifest_in$path)))
  figure_log[[id]] <<- tibble(figure_id = id, file_stem = stem, placement = placement,
    png_file = paste0(stem, ".png"), pdf_file = paste0(stem, ".pdf"),
    width_in = width, height_in = height, dpi_png = 600L,
    input_files = paste(inputs, collapse = "; "),
    input_md5 = paste(manifest_in$md5[match(inputs, basename(manifest_in$path))], collapse = "; "),
    encoding = encoding, caption_metadata = metadata, script = "replication/R/figures_v2.R")
  cat("Saved", stem, "\n")
}
chk <- function(fig, name, value, tol = 1e-8) {
  check_log[[length(check_log) + 1L]] <<- tibble(figure = fig, check = name, max_abs_difference = as.numeric(value),
                                                  tolerance = tol, pass = isTRUE(as.numeric(value) <= tol))
}
# Spread labels apart on a transformed scale, preserving order.
declump <- function(y, gap, trans = log, inv = exp) {
  o <- order(y); z <- trans(y[o])
  for (iter in 1:200) for (i in seq_along(z)[-1]) if (z[i] - z[i - 1] < gap) {
    m <- (z[i] + z[i - 1]) / 2; z[i - 1] <- m - gap / 2; z[i] <- m + gap / 2
  }
  out <- numeric(length(y)); out[o] <- inv(z); out
}

# Shared variant labels. Row text only; no numbers are read from these strings' values.
variant_labels <- tribble(
  ~variant_id, ~label,
  "offsets_constant", "Constant offsets",
  "legacy_1770_1795_splits", "1770 and 1795 offset splits",
  "split_1770_only", "1770 offset split only",
  "constant_without_meat_fuel_bridges", "Constant, meat and fuel bridges removed",
  "source_prior_010", "Offset prior sd .10",
  "source_prior_015", "Offset prior sd .15",
  "source_prior_025", "Offset prior sd .25",
  "source_prior_035", "Offset prior sd .35",
  "source_prior_075", "Offset prior sd .75",
  "source_prior_100", "Offset prior sd 1.00",
  "shared_post_offset025", "Shared post-1795 offset sd .25",
  "shared_post_offset050", "Shared post-1795 offset sd .50",
  "shared_post_offset075", "Shared post-1795 offset sd .75",
  "shared_new_columns025", "Shared new-column offset sd .25",
  "shared_new_columns050", "Shared new-column offset sd .50",
  "shared_new_columns075", "Shared new-column offset sd .75",
  "post_source_mean_plus25", "Post-1795 sources read 25% high",
  "post_source_mean_minus25", "Post-1795 sources read 20% low",
  "post_source_mean_plus50", "Post-1795 sources read 50% high",
  "post_source_mean_minus50", "Post-1795 sources read 33% low",
  "new_column_mean_plus25", "New columns read 25% high",
  "new_column_mean_minus25", "New columns read 20% low",
  "new_column_mean_plus50", "New columns read 50% high",
  "new_column_mean_minus50", "New columns read 33% low",
  "independent_commodity_paths", "No common drift across goods",
  "official_ar015", "Persistent official errors sd .15",
  "official_ar030", "Persistent official errors sd .30",
  "robust_auction_variance", "Robust auction variance",
  "no_wheat_specific_random_walk", "No wheat-specific random walk",
  "common_observation_variance", "One noise sd for all sources",
  "unresolved_guilder_divide3", "Guilders at 3 per rixdollar",
  "sterling_quotes_excluded", "Sterling quotes excluded",
  "sterling_fixed0075", "Sterling at fixed 0.075",
  "sterling_fixed0200", "Sterling at fixed 0.200",
  "wheat1787_excluded", "1787 wheat quote excluded",
  "soap_additions_excluded", "Audited soap additions excluded",
  "preaudit_inputs", "Pre-audit inputs",
  "wheat_including_koorn", "Wheat includes koorn",
  "inventory_valuations_excluded", "Inventory valuations excluded",
  "official_flat_runs_discounted", "Flat official runs discounted",
  "official_runs_first_only", "Flat runs, first year only",
  "official_sources_excluded", "Official sources removed",
  "no_transitory_wheat", "No transitory wheat state"
)
stopifnot(setequal(variant_labels$variant_id, settings$variant_id), nrow(variant_labels) == 43)
label_pct <- settings |> filter(kind %in% c("shared_mean", "shared_new_mean")) |> left_join(variant_labels, by = "variant_id") |>
  mutate(implied = round(100 * abs(exp(value) - 1)), shown = as.numeric(str_extract(label, "[0-9]+(?=%)")), dir_ok = (value > 0) == str_detect(label, "high"))
stopifnot(nrow(label_pct) == 8L)
chk("fig3_fig9", "scenario label percentages equal 100*|exp(mean)-1|", max(abs(label_pct$implied - label_pct$shown)), 0)
chk("fig3_fig9", "scenario labels have the correct direction", sum(!label_pct$dir_ok), 0)

# Figure 1: path, evidence gauges, annual change ------------------------------------
fig1_index <- ggplot(annual, aes(x = year)) +
  geom_ribbon(aes(ymin = index_q05, ymax = index_q95), fill = plum, alpha = 0.2) +
  geom_line(aes(y = index_point), colour = plum, linewidth = 0.75, lineend = "round") +
  scale_y_continuous(trans = "log", breaks = index_breaks, limits = index_limits, expand = c(0, 0)) +
  x_none + labs(x = NULL, y = "Provisioning cost (1770s mean = 100, log scale)") +
  theme_fig() + no_x

gauge_rows <- tribble(~column, ~label, ~y,
  "observed_level_weight_share", "Dated level", 2,
  "market_change_weight_share", "Market change", 1,
  "flat_official_change_weight_share", "Flat official", 0)
stopifnot(all(gauge_rows$column %in% names(annual)))
gauge_h <- 0.8
gauges <- annual |> select(year, all_of(gauge_rows$column)) |>
  pivot_longer(-year, names_to = "column", values_to = "share") |>
  left_join(gauge_rows, by = "column") |> filter(!is.na(share))
stopifnot(all(between(gauges$share, 0, 1 + 1e-9)))
fig1_gauges <- ggplot() +
  geom_rect(data = gauge_rows, aes(ymin = y, ymax = y + gauge_h), xmin = year_limits[1], xmax = year_limits[2], fill = "#F3EFF1") +
  geom_rect(data = filter(gauges, share > 0), aes(xmin = year - 0.5, xmax = year + 0.5, ymin = y, ymax = y + gauge_h * share),
            fill = plum, alpha = 0.72, colour = NA) +
  x_none + scale_y_continuous(breaks = gauge_rows$y + gauge_h / 2, labels = gauge_rows$label, expand = c(0, 0.04)) +
  labs(x = NULL, y = NULL) + theme_fig() + no_x + no_y_axis +
  theme(axis.text.y = element_text(size = fs - 0.6, colour = soft))

inflation_panel <- annual |> filter(!is.na(inflation_pct)) |> mutate(direction = if_else(inflation_pct >= 0, "up", "down"))
fig1_change <- ggplot(inflation_panel, aes(x = year)) +
  geom_hline(yintercept = 0, colour = soft, linewidth = 0.4) +
  geom_linerange(aes(ymin = inflation_q05, ymax = inflation_q95), colour = "#BDBDBD", linewidth = 0.3) +
  geom_col(aes(y = inflation_pct, fill = direction), width = 0.7) +
  scale_fill_manual(values = c(up = plum, down = blue)) +
  x_years + scale_y_continuous(breaks = seq(-40, 60, 20), expand = expansion(mult = 0.03)) +
  labs(x = NULL, y = "Annual change (%)") + theme_fig()
fig1 <- free(fig1_index, "label") / fig1_gauges / free(fig1_change, "label") + plot_layout(heights = c(3.4, 0.6, 1.5))

d1 <- layer_data(fig1_index, 2) |> arrange(x); d2 <- layer_data(fig1_index, 1) |> arrange(x)
chk("fig1", "line equals log index_point", max(abs(d1$y - log(annual$index_point))))
chk("fig1", "ribbon equals log q05 and q95", max(abs(d2$ymin - log(annual$index_q05)), abs(d2$ymax - log(annual$index_q95))))
chk("fig1", "bars equal inflation_pct", max(abs(layer_data(fig1_change, 3)$y[order(layer_data(fig1_change, 3)$x)] - inflation_panel$inflation_pct)))
save_fig("fig1", "fig1_provisioning_cost_index", fig1, height = 6.3, placement = "main",
  inputs = "annual_inflation_1700_1834.csv",
  encoding = "Top: log-scale point index and conditional 5th-95th percentile band from joint draws renormalised within draw. Middle: bar height in each year is the 1770s-weight share of goods with a dated level, a same-channel consecutive market change, or a flat official change; full height equals 100%. Bottom: annual change in the point path with draw-based whiskers.",
  metadata = "Annual path 1700-1834; 1770-79 arithmetic mean = 100. Band and whiskers conditional 90%, omitting variance-parameter, conversion, prior-scale and structural uncertainty. Reference reconstruction with the 1795 offset break.")

# Figure 2: identification map ------------------------------------------------------
family_group <- c(VOC = "VOC", Cape_Archives = "Cape Archives", Neumark = "Compilations", Theal = "Compilations",
                  project_auction = "Orphan Chamber", MOOC_transcribed = "Orphan Chamber",
                  newspaper = "Newspapers", Blue_Books = "Blue Books")
stopifnot(all(loading$family %in% names(family_group)))
family_levels <- c("VOC", "Cape Archives", "Orphan Chamber", "Compilations", "Newspapers", "Blue Books")
family_fill <- c("VOC" = blue, "Cape Archives" = teal, "Orphan Chamber" = plum, "Compilations" = gold,
                 "Newspapers" = earth, "Blue Books" = grey)
map_channels <- panel |> group_by(commodity, model_source_id) |>
  summarise(first = min(year), last = max(year), n = n(), .groups = "drop") |>
  left_join(select(loading, model_source_id, family), by = "model_source_id") |>
  left_join(select(basket, commodity, base_weight), by = "commodity") |>
  mutate(group = factor(unname(family_group[family]), levels = family_levels)) |>
  arrange(desc(base_weight), first, last, model_source_id)
stopifnot(!anyNA(map_channels$group), nrow(map_channels) == 69)
good_gap <- 1.1
map_channels <- map_channels |>
  mutate(new_good = commodity != lag(commodity, default = first(commodity)),
         row = -(row_number() - 1) - cumsum(new_good) * good_gap)
map_tiles <- panel |> select(commodity, model_source_id, year) |>
  left_join(select(map_channels, model_source_id, row, group), by = "model_source_id")
good_axis <- map_channels |> group_by(commodity) |> summarise(y = mean(range(row)), .groups = "drop")
bridge_short <- function(id) case_when(
  str_detect(id, "Neumark") ~ "Neumark",
  str_detect(id, "inventory") ~ "Inventory",
  str_detect(id, "project_auction") ~ "Auctions", TRUE ~ NA_character_)
map_bridges <- bridges |>
  left_join(select(map_channels, model_source_id, row), by = "model_source_id") |>
  mutate(label = paste0(bridge_short(model_source_id), ", ", elapsed_years, " yr"))
stopifnot(!anyNA(map_bridges$row), !anyNA(map_bridges$label), setequal(range(map_bridges$elapsed_years), c(8, 65)),
          all(map_bridges$first_post_year - map_bridges$last_pre_year == map_bridges$elapsed_years))
map_y <- c(min(map_channels$row) - 0.8, max(map_channels$row) + 0.8)
fig2 <- ggplot() +
  geom_rect(data = map_bridges, aes(ymin = row - 0.5, ymax = row + 0.5), xmin = year_limits[1], xmax = year_limits[2], fill = "#F7EFD8") +
  geom_vline(xintercept = 1794.5, colour = soft, linewidth = 0.35, linetype = "22") +
  geom_rect(data = map_tiles, aes(xmin = year - 0.5, xmax = year + 0.5, ymin = row - 0.38, ymax = row + 0.38, fill = group), colour = NA) +
  geom_segment(data = map_bridges, aes(x = last_pre_year + 0.5, xend = first_post_year - 0.5, y = row, yend = row),
               colour = ink, linewidth = 0.75, lineend = "butt") +
  geom_text(data = map_bridges, aes(x = 1836.5, y = row, label = label), hjust = 0, size = (fs - 1.1) / .pt, colour = ink) +
  scale_fill_manual(values = family_fill, breaks = family_levels, name = NULL, drop = FALSE,
                    guide = guide_legend(nrow = 1, override.aes = list(colour = NA), keywidth = unit(0.13, "in"), keyheight = unit(0.13, "in"))) +
  scale_x_continuous(breaks = c(1700, 1725, 1750, 1775, 1795, 1825), expand = c(0, 0)) +
  scale_y_continuous(breaks = good_axis$y, labels = good_axis$commodity, limits = map_y, expand = c(0, 0)) +
  coord_cartesian(xlim = year_limits, clip = "off") + labs(x = NULL, y = NULL) + theme_fig() + no_y_axis +
  theme(axis.text.y = element_text(colour = ink, size = fs), legend.position = "bottom",
        legend.margin = ggplot2::margin(4, 0, 0, 0), legend.spacing.x = unit(4, "pt"),
        plot.margin = ggplot2::margin(4, 76, 3, 6))
chk("fig2", "tile count equals 632 channel-years", abs(nrow(layer_data(fig2, 3)) - 632))
chk("fig2", "bridge gap years equal first_post minus last_pre", max(abs(map_bridges$first_post_year - map_bridges$last_pre_year - map_bridges$elapsed_years)))
chk("fig2", "all 69 channels mapped to a row", abs(n_distinct(map_tiles$row) - 69))
save_fig("fig2", "fig2_identification_map", fig2, height = 6.6, placement = "main",
  inputs = c("annual_model_panel.csv", "channel_family_loading_map.csv", "direct_1795_bridges.csv", "basket_quantities_weights.csv"),
  encoding = "One row per compiled model channel (69), grouped by good in descending 1770s weight and ordered by first year. A tile marks a retained channel-year, coloured by source family. Shaded rows are the five direct 1795 bridges; the dark connector spans the gap between each bridge's nearest pre- and post-1795 retained quotes, and the right label gives that gap in years. Dashed line separates 1794 and 1795.",
  metadata = "Channels are compiled model channels, not archival document counts. Black connectors join the nearest observed quotes either side of 1795; they are not continuous observations. Bridge rows do not have equal strength: nearest endpoints are 8 to 65 years apart, and the cotton connector spans 65 years.")

# Figure 3: assumption contour and declared-assumption forest -------------------------
contour_start <- 1770
contour <- bind_rows(
  annual |> transmute(variant_id = "primary", year, index_point, index_q05, index_q95),
  variant_summaries |> filter(variant_id == "offsets_constant") |> select(variant_id, year, index_point, index_q05, index_q95)
) |> filter(year >= contour_start)
thin_ids <- c("source_prior_025", "post_source_mean_plus25", "post_source_mean_minus25")
thin_label <- c(source_prior_025 = "Offset prior sd .25", post_source_mean_plus25 = "Post-1795 sources 25% high", post_source_mean_minus25 = "Post-1795 sources 20% low")
thin <- variant_summaries |> filter(variant_id %in% thin_ids, year >= contour_start) |> select(variant_id, year, index_point)
end_labels <- bind_rows(
  contour |> filter(year == 1834) |> transmute(variant_id, y = index_point,
    label = if_else(variant_id == "primary", "1795 offset break", "Constant offsets"),
    colour = if_else(variant_id == "primary", plum, blue)),
  thin |> filter(year == 1834) |> transmute(variant_id, y = index_point, label = unname(thin_label[variant_id]), colour = soft)
) |> mutate(y_lab = declump(y, 0.105))
contour_limits <- c(min(contour$index_q05) * 0.97, max(contour$index_q95) * 1.03)
fig3_paths <- ggplot() +
  geom_ribbon(data = filter(contour, variant_id == "offsets_constant"), aes(x = year, ymin = index_q05, ymax = index_q95), fill = blue, alpha = 0.16) +
  geom_ribbon(data = filter(contour, variant_id == "primary"), aes(x = year, ymin = index_q05, ymax = index_q95), fill = plum, alpha = 0.16) +
  geom_line(data = thin, aes(x = year, y = index_point, group = variant_id), colour = "#8C8C8C", linewidth = 0.4, linetype = "22") +
  geom_line(data = filter(contour, variant_id == "offsets_constant"), aes(x = year, y = index_point), colour = blue, linewidth = 0.8, lineend = "round") +
  geom_line(data = filter(contour, variant_id == "primary"), aes(x = year, y = index_point), colour = plum, linewidth = 0.8, lineend = "round") +
  geom_text(data = end_labels, aes(x = 1836, y = y_lab, label = label), colour = end_labels$colour, hjust = 0, size = (fs - 0.5) / .pt) +
  scale_y_continuous(trans = "log", breaks = index_breaks, limits = contour_limits, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1775, 1825, 25), expand = c(0, 0)) +
  coord_cartesian(xlim = c(contour_start - 0.5, 1834.5), clip = "off") + labs(x = NULL, y = "Provisioning cost (1770s = 100, log scale)") +
  theme_fig() + theme(plot.margin = ggplot2::margin(3, 142, 7, 6))

forest_ids <- c("primary", "offsets_constant", "source_prior_025", "source_prior_075", "shared_post_offset050", "shared_new_columns050",
                "post_source_mean_plus25", "post_source_mean_minus25", "new_column_mean_plus25", "new_column_mean_minus25",
                "official_ar015", "official_runs_first_only", "independent_commodity_paths", "unresolved_guilder_divide3")
forest <- functionals |> filter(functional == "ratio_1818_1824_to1795", variant_id %in% forest_ids) |>
  left_join(variant_labels, by = "variant_id") |>
  mutate(label = if_else(variant_id == "primary", "1795 offset break", label),
         tier = case_when(variant_id == "primary" ~ 1L, variant_id == "offsets_constant" ~ 2L, TRUE ~ 3L)) |>
  arrange(tier, desc(point)) |> mutate(y = -(row_number() - 1) - if_else(tier == 3L, 0.5, 0))
stopifnot(nrow(forest) == length(forest_ids), !anyNA(forest$label))
forest_colour <- case_when(forest$tier == 1L ~ plum, forest$tier == 2L ~ blue, TRUE ~ soft)
ratio_breaks <- c(2, 3, 4, 5, 6)
fig3_forest <- ggplot(forest, aes(y = y)) +
  geom_vline(xintercept = forest$point[forest$tier == 1L], colour = plum, linewidth = 0.3, linetype = "22") +
  geom_segment(aes(x = q05, xend = q95, yend = y), colour = forest_colour, linewidth = 0.5) +
  geom_point(aes(x = point), colour = forest_colour, size = 1.65) +
  scale_x_continuous(trans = "log", breaks = ratio_breaks, labels = paste0(ratio_breaks, "x"), expand = expansion(mult = 0.03)) +
  scale_y_continuous(breaks = forest$y, labels = forest$label, expand = expansion(add = 0.7)) +
  labs(x = "1818–24 mean relative to 1795 (log scale)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4),
        axis.line.y.left = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = ink, size = fs - 0.6), axis.title.x = element_text(size = fs + 0.4, colour = soft, margin = ggplot2::margin(t = 6)), plot.margin = ggplot2::margin(5, 12, 9, 6))
fig3 <- wrap_elements(full = fig3_paths) / fig3_forest + plot_layout(heights = c(3.25, 3.0))
chk("fig3", "reference line equals annual index_point", max(abs(filter(contour, variant_id == "primary")$index_point - annual$index_point[annual$year >= contour_start])))
chk("fig3", "forest points equal functionals file", max(abs(layer_data(fig3_forest, 3)$x - log(forest$point))))
thin_check <- tibble(variant_id = names(thin_label), label = unname(thin_label)) |> inner_join(settings, by = "variant_id") |> filter(kind == "shared_mean")
chk("fig3", "thin path label percentages equal 100*|exp(mean)-1|", max(abs(round(100 * abs(exp(thin_check$value) - 1)) - as.numeric(str_extract(thin_check$label, "[0-9]+(?=%)")))), 0)
chk("fig3", "forest has no variant mixed into a band: rows equal variants", abs(nrow(forest) - n_distinct(forest$variant_id)))
save_fig("fig3", "fig3_assumption_sensitivity", fig3, height = 6.5, placement = "main",
  inputs = c("annual_inflation_1700_1834.csv", "annual_variant_conditional_summaries.csv", "historical_functionals_all.csv"),
  encoding = "Top: reference and constant-offset point paths, 1770-1834, each with its own conditional 90% band, plus three thin dashed scenario paths. Bottom: 1818-24 mean relative to 1795 under the reference design, constant offsets and twelve selected declared assumptions, each with its own conditional 90% interval. Dashed vertical marks the reference point.",
  metadata = "Bands and intervals are per-variant; no variant is mixed into another's band. Legacy splits track the reference path and 1770-only tracks constant offsets. Full 43-variant departures: fig9_variant_heatmap. Scenario labels are source-level factors: +log(1.25) reads 25% high, -log(1.25) reads 20% low. Reference and constant offsets are co-equal substantive assumptions; the legend implies no likelihood ranking.")

# Figure 7: joint relatives and peak distribution -------------------------------------
stopifnot(identical(as.integer(primary_draws$year), 1700:1834), identical(as.integer(constant_draws$year), 1700:1834),
          ncol(primary_draws$index) == 1000, ncol(constant_draws$index) == 1000, constant_draws$seed == 20261112L)
yy <- primary_draws$year
draw_ratio <- function(m, num, den = 1795) {
  n <- colMeans(m[match(num, yy), , drop = FALSE]); n / m[match(den, yy), ]
}
ratio_defs <- list(ratio_1818_1824_to1795 = 1818:1824, ratio_1822_to1795 = 1822, ratio_1834_to1795 = 1834)
ratio_label <- c(ratio_1818_1824_to1795 = "1818–24 mean / 1795", ratio_1822_to1795 = "1822 / 1795", ratio_1834_to1795 = "1834 / 1795")
ratio_row <- c(ratio_1818_1824_to1795 = 3, ratio_1822_to1795 = 2, ratio_1834_to1795 = 1)
ratio_draws <- imap_dfr(ratio_defs, function(years, id) bind_rows(
  tibble(functional = id, design = "primary", draw_id = primary_draws$draw_id, ratio = draw_ratio(primary_draws$index, years)),
  tibble(functional = id, design = "constant", draw_id = constant_draws$draw_id, ratio = draw_ratio(constant_draws$index, years))))
ratio_stats <- ratio_draws |> group_by(functional, design) |>
  summarise(q05 = quantile(ratio, .05, names = FALSE), median = median(ratio), q95 = quantile(ratio, .95, names = FALSE), .groups = "drop")
ref <- functionals |> filter(variant_id %in% c("primary", "offsets_constant"), functional %in% names(ratio_defs)) |>
  mutate(design = if_else(variant_id == "primary", "primary", "constant"))
cmp <- inner_join(ratio_stats, ref, by = c("functional", "design"), suffix = c("", "_ref"))
stopifnot(nrow(cmp) == 6)
chk("fig7", "draw ratio quantiles equal functionals file", max(abs(cmp$q05 - cmp$q05_ref), abs(cmp$q95 - cmp$q95_ref), abs(cmp$median - cmp$median_ref)), 1e-8)
dens_grid <- seq(log(1.4), log(7.5), length.out = 600)
dens <- ratio_draws |> group_by(functional, design) |>
  group_modify(function(z, k) { d <- density(log(z$ratio), from = min(dens_grid), to = max(dens_grid), n = 600); tibble(x = d$x, density = d$y) }) |> ungroup()
dens_scale <- 0.44 / max(dens$density)
dens <- dens |> group_by(functional, design) |> filter(density > 0.015 * max(density)) |> ungroup() |> mutate(row = unname(ratio_row[functional]), sign = if_else(design == "primary", 1, -1),
                       y = row + sign * density * dens_scale, ratio = exp(x))
ratio_stats <- ratio_stats |> mutate(row = unname(ratio_row[functional]), sign = if_else(design == "primary", 1, -1))
rel_breaks <- c(2, 3, 4, 5, 6, 7)
fig7_rel <- ggplot() +
  geom_ribbon(data = dens, aes(x = ratio, ymin = pmin(y, row), ymax = pmax(y, row), group = interaction(functional, design), fill = design), alpha = 0.55, colour = NA) +
  geom_line(data = dens, aes(x = ratio, y = y, group = interaction(functional, design), colour = design), linewidth = 0.35) +
  geom_segment(data = ratio_stats, aes(x = q05, xend = q95, y = row + sign * 0.03, yend = row + sign * 0.03, colour = design), linewidth = 1.1, lineend = "butt") +
  geom_point(data = ratio_stats, aes(x = median, y = row + sign * 0.03), shape = 21, fill = "white", colour = ink, size = 1.5, stroke = 0.35) +
  scale_fill_manual(values = design_colour) + scale_colour_manual(values = design_colour) +
  scale_x_continuous(trans = "log", breaks = rel_breaks, labels = paste0(rel_breaks, "x"), limits = c(1.5, 7.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = unname(ratio_row), labels = unname(ratio_label[names(ratio_row)]), limits = c(0.5, 3.52), expand = c(0, 0)) +
  labs(x = "Joint price relative (log scale)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4),
        axis.line.y.left = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(colour = ink, size = fs),
        plot.margin = ggplot2::margin(3, 10, 8, 6))

# Peak distribution: same fits and draws, transitory state removed for the persistent target.
stopifnot(setequal(unique(peak_draws$target), c("basket_including_transitory_wheat", "persistent_price_component_only")))
windows_agree <- peak_draws |> pivot_wider(names_from = window, values_from = peak_year) |> summarise(n = sum(`1700_1834` != `1795_1834`)) |> pull(n)
chk("fig7", "full-period and post-1795 peak agree for every draw", windows_agree, 0)
peak_use <- peak_draws |> filter(window == "1700_1834")
peak_prob <- peak_use |> count(variant_id, target, peak_year) |> group_by(variant_id, target) |> mutate(p = n / sum(n)) |> ungroup() |>
  mutate(design = if_else(variant_id == "primary", "primary", "constant"), sign = if_else(design == "primary", 1, -1))
for (tg in unique(peak_use$target)) for (vr in unique(peak_use$variant_id)) {
  z <- filter(peak_use, target == tg, variant_id == vr)
  s <- filter(peak_summary, target == tg, variant_id == vr, window == "1700_1834")
  chk("fig7", paste("P(peak<1825) equals summary", vr, tg), abs(mean(z$peak_year < 1825) - s$prob_peak_before1825))
}
expected_p <- tribble(~variant_id, ~target, ~p,
  "primary", "basket_including_transitory_wheat", .807, "primary", "persistent_price_component_only", .813,
  "offsets_constant", "basket_including_transitory_wheat", .712, "offsets_constant", "persistent_price_component_only", .674)
for (i in seq_len(nrow(expected_p))) chk("fig7", paste("root-stated P(peak<1825)", expected_p$variant_id[i], expected_p$target[i]),
  abs(mean(filter(peak_use, variant_id == expected_p$variant_id[i], target == expected_p$target[i])$peak_year < 1825) - expected_p$p[i]), 5e-4)
recomputed <- function(m, ids) yy[max.col(t(m), ties.method = "first")]
chk("fig7", "reference draw peaks equal decomposition draws",
    sum(recomputed(primary_draws$index) != filter(peak_use, variant_id == "primary", target == "basket_including_transitory_wheat")$peak_year), 0)
chk("fig7", "constant draw peaks equal decomposition draws",
    sum(recomputed(constant_draws$index) != filter(peak_use, variant_id == "offsets_constant", target == "basket_including_transitory_wheat")$peak_year), 0)
peak_x <- c(min(peak_prob$peak_year) - 0.6, 1834.6)
stopifnot(min(peak_prob$peak_year) >= 1795)
basket_peak <- filter(peak_prob, target == "basket_including_transitory_wheat")
persist_peak <- filter(peak_prob, target == "persistent_price_component_only")
sterling <- filter(events, event_id == "sterling_reform")
peak_max <- max(peak_prob$p) * 1.12
fig7_peak <- ggplot() +
  geom_rect(data = sterling, aes(xmin = start_year - 0.5, xmax = end_year + 0.5), ymin = -peak_max, ymax = peak_max, fill = "#8A8A8A", alpha = 0.17) +
  geom_col(data = basket_peak, aes(x = peak_year, y = sign * p, fill = design), width = 0.74) +
  geom_point(data = persist_peak, aes(x = peak_year, y = sign * p), shape = 21, fill = "white", colour = ink, size = 1.25, stroke = 0.4) +
  geom_hline(yintercept = 0, colour = soft, linewidth = 0.4) +
  annotate("text", x = peak_x[1] + 0.6, y = peak_max * 0.62, label = "1795 offset break", hjust = 0, colour = plum, size = txt) +
  annotate("text", x = peak_x[1] + 0.6, y = -peak_max * 0.62, label = "Constant offsets", hjust = 0, colour = blue, size = txt) +
  annotate("point", x = peak_x[1] + 1, y = peak_max * 0.34, shape = 21, fill = "white", colour = ink, size = 1.25, stroke = 0.4) +
  annotate("text", x = peak_x[1] + 1.8, y = peak_max * 0.34, label = "Persistent component", hjust = 0, colour = soft, size = txt) +
  scale_fill_manual(values = design_colour) +
  scale_x_continuous(breaks = seq(1800, 1830, 5), limits = peak_x, expand = c(0, 0)) +
  scale_y_continuous(breaks = pretty(c(-peak_max, peak_max), 6), labels = function(v) percent(abs(v), accuracy = 1), limits = c(-peak_max, peak_max), expand = c(0, 0)) +
  labs(x = "Peak year", y = "Share of joint draws") + theme_fig()
fig7 <- fig7_rel / free(fig7_peak, "label") + plot_layout(heights = c(3.1, 2.3))
bk <- basket_peak |> arrange(design, peak_year)
save_fig("fig7", "fig7_joint_inference", fig7, height = 5.6, placement = "main",
  inputs = c("annual_index_joint_draws.rds", "offsets_constant_joint.rds", "historical_functionals_all.csv", "peak_decomposition_draws.csv", "peak_decomposition_summary.csv", "historical_events.csv"),
  encoding = "Top: mirrored densities of joint draw relatives, reference above and constant offsets below each baseline; bar is the 5th-95th percentile interval and open dot the median. Bottom: share of joint draws peaking in each year, reference upward and constant offsets downward; open dots are the same fits and draws with the existing realised transitory wheat state removed (not a refit). Grey column marks the 1825-26 sterling reform.",
  metadata = "Window for peak years: 1700-1834; identical to 1795-1834 for every draw. Constant draws use seed 20261112. P(peak before 1825): reference .807 (persistent component .813); constant offsets .712 (.674). Relatives use draw identifiers preserved across years. Conditional on fitted variance parameters. Mirrored bars are nonnegative frequencies: upward is the 1795 offset break, downward is constant offsets. Open dots remove the realised transitory wheat state within the same fit and draws.")

# Figure 6: historical context ------------------------------------------------------
expected_ids <- c("smallpox_1713", "smallpox_1755", "smallpox_1767", "french_garrison", "paper_rixdollars", "grain_relief",
                  "british_1795", "batavian_1803", "british_1806", "sterling_reform")
stopifnot(setequal(events$event_id, expected_ids), nrow(events) == 10, all(events$end_year >= events$start_year))
en_dash <- "–"
lane_levels <- unique(events$group)
lane_slots <- tibble(group = lane_levels, slot = if_else(lane_levels == "Government", 1.7, 1)) |>
  mutate(top = -cumsum(lag(slot, default = 0)), lane = top - slot / 2)
ev <- events |> left_join(select(lane_slots, group, lane), by = "group") |>
  mutate(tile_xmin = start_year - 0.5, tile_xmax = end_year + 0.5,
         year_text = if_else(start_year == end_year, as.character(start_year),
                             paste0(start_year, en_dash, str_pad(end_year %% 100L, 2, pad = "0"))),
         cluster = if_else(group %in% c("Disease", "Government"), group, event_id))
event_labels <- ev |> group_by(cluster, group, lane) |> arrange(start_year, .by_group = TRUE) |>
  summarise(label_text = if (n_distinct(label) == 1) paste(first(label), paste(year_text, collapse = ", ")) else {
    parts <- paste(label, year_text); paste0(parts[1], ",\n", paste(parts[-1], collapse = ", ")) },
    first_xmin = min(tile_xmin), last_xmax = max(tile_xmax), .groups = "drop") |>
  mutate(side = if_else(group == "Disease", "right", "left"),
         label_x = if_else(side == "right", last_xmax + 1, first_xmin - 1), label_hjust = if_else(side == "right", 0, 1))
guide_columns <- ev |> arrange(tile_xmin) |> mutate(column = cumsum(tile_xmin > lag(cummax(tile_xmax), default = -Inf))) |>
  group_by(column) |> summarise(xmin = min(tile_xmin), xmax = max(tile_xmax), .groups = "drop")
fig6_level <- ggplot(annual, aes(x = year)) +
  geom_rect(data = guide_columns, aes(xmin = xmin, xmax = xmax), ymin = -Inf, ymax = Inf, fill = "#8A8A8A", alpha = 0.17, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = index_q05, ymax = index_q95), fill = plum, alpha = 0.2) +
  geom_line(aes(y = index_point), colour = plum, linewidth = 0.8, lineend = "round") +
  scale_y_continuous(trans = "log", breaks = index_breaks, limits = index_limits, expand = c(0, 0)) +
  x_none + labs(x = NULL, y = "Provisioning cost (1770s mean = 100, log scale)") + theme_fig() + no_x
lane_h <- 0.66
fig6_lanes <- ggplot() +
  geom_rect(data = ev, aes(xmin = tile_xmin, xmax = tile_xmax, ymin = lane, ymax = 0), fill = "#EBEBEB", colour = NA) +
  geom_rect(data = ev, aes(xmin = tile_xmin, xmax = tile_xmax, ymin = lane - lane_h / 2, ymax = lane + lane_h / 2), fill = soft, colour = NA) +
  geom_text(data = event_labels, aes(x = label_x, y = lane, label = label_text, hjust = label_hjust), colour = ink, size = txt, lineheight = 0.95) +
  x_years + scale_y_continuous(breaks = lane_slots$lane, labels = lane_slots$group, limits = c(min(lane_slots$top - lane_slots$slot), 0), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_fig() + no_y_axis + theme(axis.text.y = element_text(colour = ink, size = fs))
fig6 <- free(fig6_level, "label") / fig6_lanes + plot_layout(heights = c(3.4, 1.55))
ld <- layer_data(fig6_level, 3) |> arrange(x)
chk("fig6", "line equals log index_point", max(abs(ld$y - log(annual$index_point))))
chk("fig6", "ten episode tiles", abs(nrow(layer_data(fig6_lanes, 2)) - 10))
chk("fig6", "tile bounds equal ledger", max(abs(sort(layer_data(fig6_lanes, 2)$xmin) - sort(ev$start_year - 0.5)), abs(sort(layer_data(fig6_lanes, 2)$xmax) - sort(ev$end_year + 0.5))))
save_fig("fig6", "fig6_historical_context", fig6, height = 5.0, placement = "main",
  inputs = c("annual_inflation_1700_1834.csv", "historical_events.csv"),
  encoding = "Top: reference point index and conditional 90% band with neutral alignment columns at dated episodes. Bottom: episode timeline from the event ledger, one lane per event group; each tile covers whole calendar years.",
  metadata = "Columns align dates only and imply no causal effect. Annual change panel deliberately omitted; see fig1.")

# Figure 4: companion series --------------------------------------------------------
stopifnot(all(c("log_linear", "spline_df12") %in% equipment$method))
equip_wide <- equipment |> select(year, method, equipment_index) |> pivot_wider(names_from = method, values_from = equipment_index)
sale <- enslaved |> select(year, sale = nominal_sale_price_index)
adj <- enslaved_decadal |> transmute(year = (decade + pmin(decade + 9, 1834)) / 2, adjusted_index, lower_95, upper_95)
consumer <- select(annual, year, consumer = index_point)
limits4 <- range(c(sale$sale, consumer$consumer, equipment$equipment_index, adj$lower_95, adj$upper_95), na.rm = TRUE)
fig4_end <- tribble(~label, ~y, ~colour,
  "Provisioning cost", consumer$consumer[consumer$year == 1834], plum,
  "Enslaved-person sale prices", sale$sale[sale$year == 1834], earth,
  "Household equipment", equip_wide$log_linear[equip_wide$year == 1834], teal) |>
  mutate(y_lab = declump(y, 0.13))
adj_key <- tibble(x = 1836.6, y = adj$adjusted_index[which.max(adj$year)])
fig4_end <- bind_rows(fig4_end, tibble(label = "Adjusted sale prices, decadal", y = adj$adjusted_index[which.max(adj$year)], colour = ink, y_lab = NA_real_)) |>
  mutate(y_lab = declump(y, 0.13))
fig4 <- ggplot() +
  geom_hline(yintercept = 100, colour = "#BDBDBD", linewidth = 0.4) +
  geom_line(data = equip_wide, aes(x = year, y = log_linear), colour = teal, linewidth = 0.55) +
  geom_line(data = equip_wide, aes(x = year, y = spline_df12), colour = teal, linewidth = 0.55, linetype = "22") +
  geom_line(data = sale, aes(x = year, y = sale), colour = earth, linewidth = 0.55) +
  geom_linerange(data = adj, aes(x = year, ymin = lower_95, ymax = upper_95), colour = ink, linewidth = 0.45) +
  geom_point(data = adj, aes(x = year, y = adjusted_index), shape = 23, fill = "white", colour = ink, size = 2.1, stroke = 0.45) +
  geom_line(data = consumer, aes(x = year, y = consumer), colour = plum, linewidth = 0.85, lineend = "round") +
  geom_text(data = fig4_end, aes(x = if_else(str_detect(label, "decadal"), 1839.3, 1836.5), y = y_lab, label = label), colour = fig4_end$colour, hjust = 0, size = (fs - 0.5) / .pt) +
  geom_point(data = filter(fig4_end, str_detect(label, "decadal")), aes(x = 1836.9, y = y_lab), shape = 23, fill = "white", colour = ink, size = 1.9, stroke = 0.45) +
  scale_y_continuous(trans = "log", breaks = c(25, 50, 100, 200, 400, 800), limits = c(limits4[1] * 0.9, limits4[2] * 1.1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1700, 1825, 25), expand = c(0, 0)) + coord_cartesian(xlim = year_limits, clip = "off") + labs(x = NULL, y = "Separate indices (each 1770s = 100, log scale)") +
  theme_fig() + theme(plot.margin = ggplot2::margin(3, 150, 3, 6))
chk("fig4", "provisioning line equals index_point", max(abs(arrange(layer_data(fig4, 7), x)$y - log(annual$index_point))))
save_fig("fig4", "fig4_companion_price_series", fig4, height = 4.2, placement = "appendix",
  inputs = c("annual_inflation_1700_1834.csv", "household_equipment_companion.csv", "enslaved_sale_price_companion.csv", "enslaved_recorded_characteristics_decadal_companion.csv"),
  encoding = "Log-scale separately normalised indices: provisioning cost; household equipment under two interpolation methods (solid log-linear, dashed spline); annual enslaved-person sale prices; decadal recorded-characteristics-adjusted sale-price index with existing 95% intervals at the midpoint of each decade's years inside the window.",
  metadata = "Enslaved-person series are prices of coercive claims, outside the consumption basket. Adjustment uses recorded covariates only. Equipment is an acquisition-cost companion, not a CPI component.")

# Figure 5: exact fixed-quantity composition ------------------------------------------
comp_map <- tribble(~commodity, ~group,
  "Wheat", "Wheat", "Beans", "Other food", "Butter", "Other food", "Meat", "Other food",
  "Soap", "Non-food", "Cotton", "Non-food", "Candles", "Non-food", "Oil", "Non-food", "Fuel", "Non-food")
base_cost <- mean(annual$annual_cost_rd[between(annual$year, 1770, 1779)])
composition <- elementary |> left_join(select(basket, commodity, quantity), by = "commodity") |> left_join(comp_map, by = "commodity") |>
  mutate(points = 100 * quantity * exp(log_price) / base_cost) |> group_by(year, group) |> summarise(points = sum(points), .groups = "drop") |>
  mutate(group = factor(group, levels = c("Wheat", "Other food", "Non-food")))
ct <- composition |> group_by(year) |> summarise(total = sum(points)) |> left_join(select(annual, year, index_point), by = "year")
chk("fig5", "stack total equals index_point", max(abs(ct$total - ct$index_point)))
group_fill <- c("Wheat" = plum, "Other food" = teal, "Non-food" = gold)
fig5 <- ggplot(composition, aes(x = year, y = points, fill = group)) +
  geom_area(colour = "white", linewidth = 0.2, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = group_fill) + x_years +
  scale_y_continuous(breaks = seq(0, 500, 100), expand = expansion(mult = c(0, 0.04))) +
  labs(x = NULL, y = "Index points (1770s basket total = 100)") + theme_fig()
drawn <- layer_data(fig5, 1) |> filter(x %in% annual$year) |> transmute(year = as.integer(x), fill = toupper(fill), ymin, ymax)
years_per_inch <- 135 / 5.75; pts_per_inch <- max(ct$total) * 1.04 / 3.8; text_h <- 0.135 * pts_per_inch
place_label <- function(g, tcol) {
  b <- filter(drawn, fill == toupper(group_fill[[g]])) |> arrange(year); win <- ceiling(nchar(g) * 0.075 * years_per_inch)
  st <- b$year[b$year <= max(b$year) - win]
  cl <- map_dbl(st, function(s) { w <- filter(b, between(year, s, s + win)); min(w$ymax) - max(w$ymin) })
  s0 <- st[which.max(cl)]; w <- filter(b, between(year, s0, s0 + win)); stopifnot(max(cl) > text_h)
  tibble(label = g, tcol, x = s0 + win / 2, y = (min(w$ymax) + max(w$ymin)) / 2)
}
fig5_labels <- bind_rows(place_label("Wheat", "white"), place_label("Other food", "white"), place_label("Non-food", ink))
fig5 <- fig5 + geom_text(data = fig5_labels, aes(x = x, y = y, label = label), inherit.aes = FALSE, colour = fig5_labels$tcol, size = txt)
save_fig("fig5", "fig5_basket_cost_composition", fig5, height = 4.0, placement = "appendix",
  inputs = c("elementary_annual_state.csv", "basket_quantities_weights.csv", "annual_inflation_1700_1834.csv"),
  encoding = "Stacked fixed-quantity cost components: 100 x quantity x exp(smoothed log price) / mean 1770s cost. Drawn stack equals the point index.",
  metadata = "Point estimates; no intervals. Band height follows fixed quantities and estimated prices and is not an observed expenditure share.")

# Figure 8: nine-good source-price tapestry -------------------------------------------
reference <- elementary |> filter(between(year, 1770, 1779)) |> group_by(commodity) |> summarise(reference_log = mean(log_price), .groups = "drop")
state <- elementary |> left_join(reference, by = "commodity") |>
  mutate(rel = log_price - reference_log, rel_low = log(price_q05) - reference_log, rel_high = log(price_q95) - reference_log)
class_of <- function(role) case_when(role %in% c("auction", "auction_transcribed", "newspaper") ~ "Market",
  str_detect(role, "VOC|government|official_statistics") ~ "Official", TRUE ~ "Other")
class_levels <- c("Market", "Official", "Other")
quotes <- panel |> left_join(select(offsets, model_source_id, offset), by = "model_source_id") |> left_join(reference, by = "commodity") |>
  mutate(rel = log_price - offset - reference_log, class = factor(class_of(source_role), levels = class_levels))
stopifnot(!anyNA(quotes$offset), !anyDuplicated(paste(quotes$model_source_id, quotes$year)))
row_gap <- 0.4; strip_gap <- 0.12; strip_h <- 0.2
rows <- bind_rows(transmute(state, commodity, low = rel_low, high = rel_high), transmute(quotes, commodity, low = rel, high = rel)) |>
  group_by(commodity) |> summarise(y_low = min(low), y_high = max(high), .groups = "drop") |>
  left_join(select(basket, commodity, base_weight), by = "commodity") |> arrange(desc(base_weight)) |>
  mutate(block = (y_high - y_low) + strip_gap + strip_h, top = -cumsum(lag(block + row_gap, default = 0)), baseline = top - y_high,
         strip_top = baseline + y_low - strip_gap, strip_bottom = strip_top - strip_h, row_label = paste0(commodity, " (", percent(base_weight, accuracy = 1), ")"))
state_rows <- left_join(state, select(rows, commodity, baseline), by = "commodity")
quote_rows <- left_join(quotes, select(rows, commodity, baseline), by = "commodity")
coverage <- quote_rows |> group_by(commodity, year) |> summarise(class = class_levels[min(as.integer(class))], .groups = "drop") |>
  mutate(class = factor(class, levels = class_levels)) |> left_join(select(rows, commodity, strip_top, strip_bottom), by = "commodity")
class_col <- c(Market = plum, Official = blue, Other = gold); class_shape <- c(Market = 21, Official = 22, Other = 24)
bar_x <- 1712; bar_y <- rows$baseline[nrow(rows)] + 0.25
fig8 <- ggplot() +
  geom_vline(xintercept = 1794.5, colour = "#9A9A9A", linewidth = 0.35, linetype = "22") +
  geom_rect(data = rows, aes(xmin = 1699.5, xmax = 1834.5, ymin = strip_bottom, ymax = strip_top), fill = "#F1F1F1", colour = NA) +
  geom_rect(data = coverage, aes(xmin = year - 0.5, xmax = year + 0.5, ymin = strip_bottom, ymax = strip_top, fill = class), colour = NA, show.legend = FALSE) +
  geom_segment(data = rows, aes(x = 1699.5, xend = 1834.5, y = baseline, yend = baseline), colour = rule, linewidth = 0.35) +
  geom_ribbon(data = state_rows, aes(x = year, ymin = baseline + rel_low, ymax = baseline + rel_high, group = commodity), fill = plum, alpha = 0.16) +
  geom_line(data = state_rows, aes(x = year, y = baseline + rel, group = commodity), colour = plum, linewidth = 0.55, lineend = "round") +
  geom_point(data = quote_rows, aes(x = year, y = baseline + rel, shape = class, fill = class, colour = class), size = 0.95, stroke = 0.15, alpha = 0.85) +
  annotate("segment", x = bar_x, xend = bar_x, y = bar_y, yend = bar_y + log(2), colour = ink, linewidth = 0.5) +
  annotate("text", x = bar_x + 1.5, y = bar_y + log(2) / 2, label = "x2", hjust = 0, colour = ink, size = txt) +
  scale_shape_manual(values = class_shape, name = NULL) + scale_fill_manual(values = class_col, name = NULL) + scale_colour_manual(values = class_col, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 2.2, alpha = 1)), fill = guide_legend(), shape = guide_legend()) +
  scale_x_continuous(breaks = seq(1700, 1825, 25), limits = year_limits, expand = c(0, 0)) +
  scale_y_continuous(breaks = rows$baseline, labels = rows$row_label, limits = c(min(rows$strip_bottom), max(rows$top)), expand = expansion(add = 0.08)) +
  labs(x = NULL, y = NULL) + theme_fig() + no_y_axis + theme(axis.text.y = element_text(colour = ink, hjust = 1, size = fs), legend.position = "bottom")
chk("fig8", "quote count equals panel", abs(nrow(layer_data(fig8, 7)) - 632))
save_fig("fig8", "fig8_source_price_tapestry", fig8, height = 8.2, placement = "appendix",
  inputs = c("elementary_annual_state.csv", "annual_model_panel.csv", "source_offset_estimates.csv", "basket_quantities_weights.csv"),
  encoding = "Nine rows on one common log scale; each baseline is that good's 1770s mean log state. Line and ribbon: fitted path and pointwise conditional 90% band. Points: channel-year quotes minus estimated channel offset, by source class. Strip: years with any quote, coloured by highest evidence class. Scale bar marks a doubling. Dashed line separates 1794 and 1795.",
  metadata = "Quotes are model-adjusted, not observed prices in a common unit. Row label is the 1770s base weight. Ribbon is relative to one fixed reference and is not an interval for change since the 1770s.")

# Figure 9: full variant departure heat map -------------------------------------------
stopifnot(setequal(variants$variant_id, settings$variant_id), nrow(variants) == 135 * 43)
group_order <- c("identification", "measurement", "currency", "source", "stress")
stopifnot(setequal(settings$group, group_order))
heat_rows <- settings |> left_join(variant_labels, by = "variant_id") |> mutate(g = match(group, group_order), i = row_number()) |> arrange(g, i) |>
  mutate(position = -(row_number() - 1) - (g - 1) * 0.7)
heat <- variants |> left_join(select(annual, year, primary = index_point), by = "year") |> mutate(departure_pct = 100 * (index / primary - 1)) |>
  left_join(select(heat_rows, variant_id, position), by = "variant_id")
sat <- 25
fig9 <- ggplot(heat, aes(x = year, y = position, fill = departure_pct)) +
  geom_tile(height = 0.92, colour = NA) +
  geom_text(data = heat_rows, aes(x = 1836.2, y = position, label = label), hjust = 0, colour = ink, size = (fs - 1.5) / .pt, inherit.aes = FALSE) +
  scale_fill_gradient2(low = blue, mid = "#F5F2F0", high = rose, midpoint = 0, limits = c(-sat, sat), oob = squish, breaks = c(-sat, 0, sat),
                       labels = function(x) paste0(if_else(x > 0, "+", ""), x, "%"), name = "Departure from reference reconstruction",
                       guide = guide_colourbar(barwidth = unit(1.7, "in"), barheight = unit(0.11, "in"), title.position = "left", title.vjust = 0.9)) +
  scale_x_continuous(breaks = seq(1700, 1825, 25), expand = c(0, 0)) + scale_y_continuous(breaks = NULL, expand = c(0, 0.3)) +
  coord_cartesian(xlim = year_limits, clip = "off") + labs(x = NULL, y = NULL) + theme_fig() + no_y_axis +
  theme(axis.text.y = element_blank(), legend.position = "bottom", plot.margin = ggplot2::margin(4, 162, 3, 14))
chk("fig9", "departure zero by construction at base primary mapping", abs(nrow(heat) - 135 * 43), 0)
save_fig("fig9", "fig9_variant_heatmap", fig9, height = 8.2, placement = "appendix",
  inputs = c("annual_structural_variants.csv", "annual_inflation_1700_1834.csv", "v2_variant_settings.csv"),
  encoding = "One row per declared variant (43), in five groups separated by small gaps (identification, measurement, currency, source, stress). Tile colour is the percentage departure of that variant's point path from the reference path.",
  metadata = paste0("Colour saturates at +/-", sat, "%; largest departure in the data is ", round(max(abs(heat$departure_pct)), 1), "%. These are point departures, not intervals. Scenario labels are source-level factors: +log(1.25) 25% high, -log(1.25) 20% low, +log(1.5) 50% high, -log(1.5) 33% low."))

# Figure 10: pass-through trade-off ---------------------------------------------------
pt_wide <- pass_summary |> select(loading, gamma, scenario, finite_basket_pass_through) |>
  pivot_wider(names_from = scenario, values_from = finite_basket_pass_through) |> arrange(loading, gamma)
loading_col <- c(g_all = plum, g_new = teal)
loading_text <- tibble(loading = c("g_all", "g_new"), label = c("All post-1795 channels", "New source columns"))
pt_lab <- pt_wide |> filter(gamma == 0) |> left_join(loading_text, by = "loading")
gamma_text <- function(g) if_else(g == 0, "γ = 0", sub("^0", "", format(g, trim = TRUE, drop0trailing = TRUE)))
pt_wide <- pt_wide |> mutate(lab = map_chr(gamma, gamma_text), nx = case_when(gamma == .75 ~ -0.016, gamma == .5 ~ 0.024, TRUE ~ 0),
                             ny = case_when(gamma == .5 ~ -0.026, TRUE ~ 0.026))
pt_series <- tibble(loading = c("g_new", "g_all"), source_wedge = c(0.0, 0.60), true_price_step = c(0.922, 0.655), label = c("New source columns", "All post-1795 channels"), hj = c(0, 1))
fig10_left <- ggplot(pt_wide, aes(x = source_wedge, y = true_price_step, colour = loading)) +
  geom_path(linewidth = 0.6) + geom_point(size = 1.9) +
  geom_text(aes(x = source_wedge + nx, y = true_price_step + ny, label = lab), size = (fs - 1.6) / .pt) +
  geom_text(data = pt_series, aes(label = label, hjust = hj), size = (fs - 0.6) / .pt) +
  scale_colour_manual(values = loading_col) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.2), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.5, 0.9, 0.1), expand = c(0, 0)) + coord_cartesian(xlim = c(-0.05, 0.62), ylim = c(0.5, 0.95)) +
  labs(x = "Source wedge retained, 1820–24 (share)", y = "True price step retained (share)") + theme_fig() +
  theme(panel.grid.major.x = element_line(colour = rule, linewidth = 0.4), plot.margin = ggplot2::margin(4, 12, 3, 6))
pa <- pass_annual |> filter(gamma == 0, year >= 1795) |>
  mutate(series = case_when(scenario == "true_price_step" ~ "True price step", loading == "g_all" ~ "All post-1795", TRUE ~ "New columns")) |>
  filter(!(scenario == "true_price_step" & loading == "g_new"))
series_col <- c("True price step" = ink, "All post-1795" = plum, "New columns" = teal)
pa_end <- pa |> filter(year == 1834) |> mutate(y_lab = declump(finite_basket_pass_through, 0.07, identity, identity))
fig10_right <- ggplot(pa, aes(x = year, y = finite_basket_pass_through, colour = series)) +
  geom_line(linewidth = 0.65, lineend = "round") +
  geom_text(data = pa_end, aes(x = 1835.3, y = y_lab, label = series), hjust = 0, size = (fs - 0.6) / .pt) +
  scale_colour_manual(values = series_col) +
  scale_x_continuous(breaks = seq(1800, 1830, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", ".25", ".50", ".75", "1"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(1795, 1834), ylim = c(0, 1), clip = "off") +
  labs(x = NULL, y = "Share of shift in basket") + theme_fig() + theme(plot.margin = ggplot2::margin(4, 86, 3, 8))
fig10 <- fig10_left + fig10_right + plot_layout(widths = c(1.1, 1))
chk("fig10", "left points equal summary", max(abs(sort(layer_data(fig10_left, 2)$x) - sort(pt_wide$source_wedge))))
save_fig("fig10", "fig10_pass_through_tradeoff", fig10, height = 3.5, placement = "appendix",
  inputs = c("pass_through_1820s_summary.csv", "fixed_parameter_pass_through_annual.csv"),
  encoding = "Left: each point is a shared-offset prior scale gamma (labelled); x is the share of a log(1.25) source wedge entering the 1820-24 basket, y is the share of a true post-1795 price step retained, for two loadings. Right: annual share of the shift in the basket at gamma = 0.",
  metadata = "Fixed-parameter assumption-conditioned response at estimated primary variance parameters. Not an empirical estimate of a historical wedge.")

# Figure 11: empirical validation diagnostic -----------------------------------------
val_checks <- rd("validation_checks.csv"); rep_checks <- rd("reporting_checks.csv")
stopifnot(all(val_checks$pass), all(rep_checks$pass), nrow(rep_checks) == 6L)
fold_diff <- rd("quote_fold_rmse_differences.csv") |> filter(model == "full_common_drift")
fold_eq <- rd("quote_fold_equal_comparisons.csv") |> filter(model == "full_common_drift")
author_main <- rd("AUTHOR_table_main_validation.csv"); author_interp <- rd("AUTHOR_table_interpolation_only.csv")
author_prior <- rd("AUTHOR_table_prior_diagnostics.csv"); role_strata <- rd("AUTHOR_table_transferred_role_flat_strata.csv")
role_full <- rd("quote_inverse_multiplicity_by_role_and_flat_position.csv"); weighted_scores <- rd("quote_inverse_multiplicity_scores.csv")
val_family <- tribble(~reporting_family, ~label, ~ord,
  "portfolio_block_L1", "1-year portfolio gaps", 1L, "portfolio_block_L5", "5-year portfolio gaps", 2L, "portfolio_block_L10", "10-year portfolio gaps", 3L,
  "transferred_sparse_pattern", "Transferred sparse pattern", 4L, "component_rank_ge2", "Smaller source components", 5L, "partial_channel_splice", "Partial-channel holdouts", 6L)
val_base <- tribble(~baseline, ~base_label, ~base_ord,
  "independent_drift", "Independent goods", 1L, "within_channel", "Within-channel", 2L, "pooled_loglinear", "Pooled log-linear", 3L, "overlap_splice", "Overlap splice", 4L)
base_col <- c("Independent goods" = teal, "Within-channel" = gold, "Pooled log-linear" = blue, "Overlap splice" = plum)
stopifnot(!any(str_detect(val_family$reporting_family, "prior")))
val_means <- fold_eq |> filter(support_scope == "all_pairwise_eligible") |> inner_join(val_family, by = "reporting_family") |> inner_join(val_base, by = "baseline") |>
  arrange(ord, base_ord) |> mutate(y = -(row_number() - 1) - (ord - 1) * 0.75, base_label = factor(base_label, levels = val_base$base_label))
val_folds <- fold_diff |> filter(support_scope == "all_pairwise_eligible") |> inner_join(select(val_means, reporting_family, baseline, y, base_label), by = c("reporting_family", "baseline"))
stopifnot(nrow(val_means) == 18L, nrow(val_folds) == sum(val_means$n_folds), n_distinct(val_folds$fold_id) == 38L)
val_axis <- val_means |> group_by(reporting_family, label, ord) |> summarise(y0 = mean(range(y)), top = max(y) + 0.62, bottom = min(y) - 0.62, k = first(n_folds), .groups = "drop") |>
  arrange(ord) |> mutate(row_text = if_else(reporting_family == "partial_channel_splice", paste0(label, "\n(", k, " folds, 4 source pairs)"), paste0(label, " (", k, ")")))
sym_log <- pseudo_log_trans(sigma = 0.05, base = exp(1))
val_breaks <- c(-1, -0.5, -0.2, -0.05, 0, 0.05, 0.2, 0.5)
fmt_diff <- function(v) if_else(v == 0, "0", sub("(^-?)0[.]", "\\1.", format(v, trim = TRUE, drop0trailing = TRUE)))
mean_key <- "Fold-equal mean"; fold_key <- "Individual fold"
fig11_main <- ggplot() +
  geom_rect(data = filter(val_axis, ord %% 2L == 1L), aes(ymin = bottom, ymax = top), xmin = -Inf, xmax = Inf, fill = "#F6F4F5") +
  geom_vline(xintercept = 0, colour = soft, linewidth = 0.45) +
  geom_point(data = val_folds, aes(x = difference, y = y, colour = base_label, shape = fold_key), size = 1.25, stroke = 0.42, alpha = 0.9) +
  geom_point(data = val_means, aes(x = mean_fold_rmse_difference, y = y, colour = base_label, shape = mean_key), size = 3.1) +
  scale_colour_manual(values = base_col, name = NULL, drop = FALSE) +
  scale_shape_manual(values = setNames(c(1, 18), c(fold_key, mean_key)), name = NULL, breaks = c(fold_key, mean_key)) +
  scale_x_continuous(trans = sym_log, breaks = val_breaks, labels = fmt_diff, limits = c(-1.05, 0.65), expand = c(0, 0)) +
  scale_y_continuous(breaks = val_axis$y0, labels = val_axis$row_text, expand = expansion(add = 0.45)) +
  guides(colour = guide_legend(order = 1, nrow = 1, override.aes = list(shape = 18, size = 3)), shape = guide_legend(order = 2, nrow = 1, override.aes = list(colour = soft))) +
  labs(x = "Common-model RMSE minus baseline RMSE, noisy quotes\n(negative favours the common model; symmetric log scale)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4),
        axis.line.y.left = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(colour = ink, size = fs - 0.3, lineheight = 0.92),
        legend.position = "bottom", legend.justification = "right", legend.box.just = "right", legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = ggplot2::margin(1, 0, 0, 0),
        plot.margin = ggplot2::margin(3, 12, 5, 6))
interp_family <- tribble(~reporting_family, ~short, ~ord,
  "portfolio_block_L1", "1-year gaps", 1L, "portfolio_block_L5", "5-year gaps", 2L, "portfolio_block_L10", "10-year gaps", 3L,
  "transferred_sparse_pattern", "Transferred", 4L, "partial_channel_splice", "Partial channel", 5L)
interp_means <- fold_eq |> filter(support_scope == "within_channel_interpolation_only", baseline == "within_channel") |> inner_join(interp_family, by = "reporting_family") |> mutate(y = -ord)
interp_folds <- fold_diff |> filter(support_scope == "within_channel_interpolation_only", baseline == "within_channel") |> inner_join(select(interp_means, reporting_family, y), by = "reporting_family")
stopifnot(nrow(interp_means) == 5L, nrow(interp_folds) == sum(interp_means$n_folds))
fig11_interp <- ggplot() +
  geom_vline(xintercept = 0, colour = soft, linewidth = 0.45) +
  geom_point(data = interp_folds, aes(x = difference, y = y), shape = 1, colour = gold, size = 1.25, stroke = 0.42) +
  geom_point(data = interp_means, aes(x = mean_fold_rmse_difference, y = y), shape = 18, colour = gold, size = 3.1) +
  scale_x_continuous(trans = sym_log, breaks = c(-0.2, -0.05, 0, 0.05, 0.2), labels = fmt_diff, limits = c(-0.45, 0.28), expand = c(0, 0)) +
  scale_y_continuous(breaks = interp_means$y, labels = interp_means$short, expand = expansion(add = 0.55)) +
  labs(x = "Common minus interpolation RMSE,\ninterpolation-only cells (fold-equal)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4), axis.line.y.left = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = ink, size = fs - 0.3), plot.margin = ggplot2::margin(6, 12, 3, 6))
role_rows <- tribble(~noise_role, ~flat_run_position, ~label, ~y,
  "official", "run_interior", "Official, flat-run interior", -1, "official", "run_edge", "Official, flat-run edge", -2,
  "official", "not_in_consecutive_flat_run", "Official, not flat", -3, "auction", "not_in_consecutive_flat_run", "Auction", -4, "other", "not_in_consecutive_flat_run", "Other", -5)
role_plot <- role_strata |> inner_join(role_rows, by = c("noise_role", "flat_run_position")) |> mutate(row_text = paste0(label, " (", n_unique_cells, ")"))
stopifnot(nrow(role_plot) == 5L)
fig11_cov <- ggplot(role_plot, aes(x = predictive_90_coverage, y = y)) +
  geom_vline(xintercept = 0.9, colour = soft, linewidth = 0.45, linetype = "22") +
  geom_segment(aes(x = 0.9, xend = predictive_90_coverage, yend = y), colour = "#BDBDBD", linewidth = 0.5) +
  geom_point(colour = plum, size = 2.2) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9, 1), labels = c(".7", ".8", ".9", "1"), limits = c(0.7, 1.005), expand = c(0, 0)) +
  scale_y_continuous(breaks = role_plot$y, labels = role_plot$row_text, expand = expansion(add = 0.55)) +
  labs(x = "90% quote-interval coverage, transferred\npatterns (unique-cell weighted)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4), axis.line.y.left = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = ink, size = fs - 0.3), axis.title.x = element_text(size = fs + 0.4, colour = soft, hjust = 1), plot.margin = ggplot2::margin(6, 12, 3, 4))
fig11 <- wrap_elements(full = fig11_main) / wrap_elements(full = fig11_interp + fig11_cov + plot_layout(widths = c(1, 1))) + plot_layout(heights = c(3.95, 1.75))
chk("fig11", "fold-equal means equal the mean of plotted folds", max(abs((val_folds |> group_by(reporting_family, baseline) |> summarise(m = mean(difference), .groups = "drop") |> inner_join(val_means, by = c("reporting_family", "baseline")) |> mutate(d = m - mean_fold_rmse_difference))$d)))
chk("fig11", "plotted means equal author main table", max(abs((val_means |> inner_join(author_main, by = c("reporting_family", "baseline"), suffix = c("", "_a")) |> mutate(d = mean_fold_rmse_difference - mean_fold_rmse_difference_a))$d)))
chk("fig11", "interpolation-only means equal author table", max(abs((interp_means |> inner_join(author_interp, by = c("reporting_family", "baseline"), suffix = c("", "_a")) |> mutate(d = mean_fold_rmse_difference - mean_fold_rmse_difference_a))$d)))
chk("fig11", "main mean markers drawn at file values", max(abs(sort(layer_data(fig11_main, 4)$x) - sort(sym_log$transform(val_means$mean_fold_rmse_difference)))))
chk("fig11", "every fold marker inside the plotted limits", sum(val_folds$difference < -1.05 | val_folds$difference > 0.65 | is.na(val_folds$difference)), 0)
chk("fig11", "role coverage equals author table", max(abs(layer_data(fig11_cov, 3)$x[order(layer_data(fig11_cov, 3)$y)] - role_plot$predictive_90_coverage[order(role_plot$y)])))
role_ref <- role_full |> filter(support_scope == "all_pairwise_eligible", reporting_family == "transferred_sparse_pattern", model == "full_common_drift", baseline == "within_channel") |>
  inner_join(role_strata, by = c("noise_role", "flat_run_position"), suffix = c("", "_a"))
chk("fig11", "role strata equal the unique-cell-weighted source file", max(abs(role_ref$predictive_90_coverage - role_ref$predictive_90_coverage_a)))
chk("fig11", "rank-1 and union prior diagnostics omitted", sum(str_detect(val_means$reporting_family, "rank1|union")), 0)
save_fig("fig11", "fig11_validation_diagnostic", fig11, height = 6.1, placement = "appendix (Appendix G)",
  inputs = c("validation_checks.csv", "reporting_checks.csv", "quote_fold_rmse_differences.csv", "quote_fold_equal_comparisons.csv", "quote_inverse_multiplicity_by_role_and_flat_position.csv", "AUTHOR_table_main_validation.csv", "AUTHOR_table_interpolation_only.csv", "AUTHOR_table_prior_diagnostics.csv", "AUTHOR_table_transferred_role_flat_strata.csv"),
  encoding = "Top: each row is one common-model versus baseline contrast on its pairwise matched support; open circles are individual folds and the diamond is the fold-equal mean RMSE difference (common minus baseline), with fold counts in the row label. Lower left: within-channel contrast restricted to interpolation-only cells. Lower right: common-model 90% predictive coverage of held-out quotes in transferred sparse-pattern masks, by source role and flat-run position, against the nominal .90; row labels give unique cells.",
  metadata = "All scores are held-out noisy quote predictions including observation noise H; they do not measure latent inflation or cross-regime level calibration. Parameters re-estimated per fold. Top and lower-left summaries are fold-equal; lower-right coverage is unique-cell (inverse-multiplicity) weighted. Folds are dependent, so no intervals or significance are implied, and the pooled 1745 fold-cases are never summarised. Seven partial-channel folds comprise four unordered source pairs. Rank-1 components and transported unions are prior-sensitive diagnostics and are omitted (AUTHOR_table_prior_diagnostics.csv). Each contrast has its own matched support. Symmetric log scale, sigma 0.05: gains and losses near zero are expanded. Transferred masks are sparse within-era recovery tests, 79-90% official, not recreations of a 1795 institutional or currency wedge.")

# Figure 12: simulation recovery ------------------------------------------------------
sim_truth <- rd("paper_table_simulation_truth_comparison.csv")
sim_cal <- rd("paper_table_simulation_calibration.csv") |> filter(fit_variant == "primary")
sim_clean <- rd("clean_basket_recovery_summary.csv") |> filter(metric == "pointwise_coverage")
sim_peak <- rd("paper_table_simulation_peak.csv")
sim_paired <- rd("AUTHOR_TABLE_paired_functionals.csv")
sim_paired_full <- rd("paired_stress_functional_summary.csv")
sim_fun <- rd("functional_recovery_summary.csv"); sim_peak_rec <- rd("peak_recovery_summary.csv"); sim_scores <- rd("S0_peak_score_summary.csv")
sim_s0_annual <- rd("AUTHOR_TABLE_S0_annual.csv"); sim_mcse <- rd("paired_coverage_mcse_verified.csv")
stopifnot(all(sim_truth$n == 200L), all(sim_cal$n_pointwise_coverage == 200L), all(sim_clean$n == 200L), all(sim_peak$n == 200L), all(sim_paired$n == 200L),
          all(is.finite(sim_paired_full$coverage_difference_mcse)))
annual_rows <- tribble(~group, ~label, ~ord, "all", "All years", 1L, "post1795", "Post-1795 years", 2L, "goods_0", "Zero-quote years", 3L)
cov_annual <- annual_rows |> left_join(transmute(sim_cal, group, s0 = mean_pointwise_coverage, s0_se = mcse_pointwise_coverage), by = "group") |>
  left_join(transmute(sim_clean, group, clean = mean, clean_se = mcse), by = "group") |> mutate(block = 1L)
peak_s0 <- filter(sim_peak, scenario == "S0_prior_calibration", fit_variant == "primary"); peak_clean <- filter(sim_peak, scenario == "S4_fixed_reference", fit_variant == "primary")
stopifnot(nrow(peak_s0) == 1L, nrow(peak_clean) == 1L)
cov_peak <- tibble(group = "peak", label = paste0("Peak year, 1795\u20131834\n(spans ", round(peak_s0$mean_interval_years), " and ", round(peak_clean$mean_interval_years), " yr)"), ord = 1L,
                   s0 = peak_s0$coverage, s0_se = peak_s0$coverage_mcse, clean = peak_clean$coverage, clean_se = peak_clean$coverage_mcse, block = 2L)
fun_label <- c(ratio_1820_1824_to1770s = "1820\u201324 / 1770s", ratio_1770s_to1700 = "1770s / 1700", ratio_1834_to1795 = "1834 / 1795", ratio_1824_to1822 = "1824 / 1822",
               ratio_1818_1824_to1795 = "1818\u201324 / 1795", ratio_1822_to1795 = "1822 / 1795", ratio_1787_to1785 = "1787 / 1785")
stopifnot(setequal(names(fun_label), sim_truth$functional))
cov_fun <- sim_truth |> transmute(group = functional, label = unname(fun_label[functional]), s0 = coverage_primary, s0_se = coverage_mcse_primary, clean = coverage, clean_se = coverage_mcse, block = 3L) |>
  arrange(desc(clean)) |> mutate(ord = row_number())
cov_rows <- bind_rows(cov_annual, cov_peak, cov_fun) |> arrange(block, ord) |> mutate(y = -(row_number() - 1) - (block - 1) * 0.75)
cov_long <- bind_rows(cov_rows |> transmute(y, series = "s0", value = s0, se = s0_se), cov_rows |> transmute(y, series = "clean", value = clean, se = clean_se)) |>
  mutate(lo = pmax(0, value - 2 * se), hi = pmin(1, value + 2 * se))
sim_colour <- c(s0 = teal, clean = gold); sim_shape <- c(s0 = 16, clean = 17)
key_y <- min(cov_rows$y) - 1.0
fig12_cov <- ggplot() +
  geom_vline(xintercept = 0.9, colour = soft, linewidth = 0.45, linetype = "22") +
  geom_segment(data = cov_rows, aes(x = s0, xend = clean, y = y, yend = y), colour = "#D2D2D2", linewidth = 0.5) +
  geom_segment(data = cov_long, aes(x = lo, xend = hi, y = y, yend = y, colour = series), linewidth = 0.75) +
  geom_point(data = cov_long, aes(x = value, y = y, colour = series, shape = series), size = 2.2) +
  annotate("point", x = 0.015, y = key_y, shape = 17, colour = gold, size = 2.2) +
  annotate("text", x = 0.04, y = key_y, label = "Clean fixed path, low 1795 anchor", hjust = 0, colour = earth, size = (fs - 0.5) / .pt) +
  annotate("point", x = 0.545, y = key_y, shape = 16, colour = teal, size = 2.2) +
  annotate("text", x = 0.57, y = key_y, label = "Prior-generated paths", hjust = 0, colour = teal, size = (fs - 0.5) / .pt) +
  scale_colour_manual(values = sim_colour) + scale_shape_manual(values = sim_shape) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 0.9, 1), labels = c("0", ".25", ".5", ".75", ".9", "1"), limits = c(0, 1.015), expand = c(0, 0)) +
  scale_y_continuous(breaks = cov_rows$y, labels = cov_rows$label, limits = c(key_y - 0.6, max(cov_rows$y) + 0.6), expand = c(0, 0)) +
  labs(x = "Coverage of nominal 90% intervals (whiskers: 2 Monte Carlo SE)", y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4), axis.line.y.left = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = ink, size = fs - 0.3, lineheight = 0.92), plot.margin = ggplot2::margin(3, 12, 6, 6))
stress_fun <- c(ratio_1820_1824_to1770s = "1820\u201324 / 1770s", ratio_1818_1824_to1795 = "1818\u201324 / 1795", ratio_1822_to1795 = "1822 / 1795", ratio_1834_to1795 = "1834 / 1795")
stress_lab <- c(S1_fixed_source_wedge = "Source wedge +log 1.25", S2_official_channel_ar = "Persistent official errors", S3_administered_runs = "Carry-forward runs")
stress_rows <- sim_paired |> filter(functional %in% names(stress_fun)) |>
  mutate(f_ord = match(functional, names(stress_fun)), s_ord = match(scenario, names(stress_lab)), stress = factor(unname(stress_lab[scenario]), levels = unname(stress_lab)),
         y = -((f_ord - 1) * 4 + s_ord), lab_y = -((f_ord - 1) * 4 + 2))
stopifnot(nrow(stress_rows) == 12L)
stress_axis <- distinct(stress_rows, functional, lab_y) |> mutate(label = unname(stress_fun[functional]))
stress_colour <- c("Source wedge +log 1.25" = plum, "Persistent official errors" = blue, "Carry-forward runs" = earth); stress_shape <- c("Source wedge +log 1.25" = 16, "Persistent official errors" = 15, "Carry-forward runs" = 17)
stress_panel <- function(value, se, xlab, breaks, show_y) {
  d <- stress_rows |> mutate(v = .data[[value]], m = .data[[se]], lo = v - 2 * m, hi = v + 2 * m)
  ggplot(d, aes(y = y, colour = stress, shape = stress)) +
    geom_vline(xintercept = 0, colour = soft, linewidth = 0.45) +
    geom_segment(aes(x = lo, xend = hi, yend = y), linewidth = 0.6, show.legend = FALSE) +
    geom_point(aes(x = v), size = 1.9) +
    scale_colour_manual(values = stress_colour, name = NULL) + scale_shape_manual(values = stress_shape, name = NULL) +
    scale_x_continuous(breaks = breaks, labels = function(b) if_else(b == 0, "0", sub("(^-?)0[.]", "\\1.", format(b, trim = TRUE, drop0trailing = TRUE)))) +
    scale_y_continuous(breaks = stress_axis$lab_y, labels = if (show_y) stress_axis$label else NULL, expand = expansion(add = 0.6)) +
    labs(x = xlab, y = NULL) + theme_fig() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = rule, linewidth = 0.4), axis.line.y.left = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour = ink, size = fs - 0.3), legend.position = "bottom", legend.margin = ggplot2::margin(0, 0, 0, 0))
}
fig12_bias <- stress_panel("mean_paired_log_error", "log_error_mcse", "Stress minus clean log error\n(replicate-paired mean, 2 MCSE)", c(-0.05, 0, 0.05, 0.1), TRUE)
fig12_sq <- stress_panel("mean_squared_error_difference", "squared_error_mcse", "Change in squared log error\n(replicate-paired mean, 2 MCSE)", c(-0.05, 0, 0.05), FALSE)
fig12 <- wrap_elements(full = fig12_cov) / wrap_elements(full = fig12_bias + fig12_sq + plot_layout(widths = c(1.15, 1), guides = "collect") & theme(legend.position = "bottom")) + plot_layout(heights = c(3.35, 2.15))
cl <- layer_data(fig12_cov, 4); cv <- arrange(cov_long, y, series)
chk("fig12", "coverage markers equal source tables", max(abs(arrange(cl, y, x)$x - arrange(cov_long, y, value)$value)))
chk("fig12", "S0 functional coverage equals functional_recovery_summary", max(abs((sim_truth |> inner_join(filter(sim_fun, scenario == "S0_prior_calibration", fit_variant == "primary"), by = "functional", suffix = c("", "_f")) |> mutate(d = coverage_primary - coverage_f))$d)))
chk("fig12", "S0 annual coverage equals AUTHOR table", max(abs((sim_cal |> inner_join(filter(sim_s0_annual, fit_variant == "primary", metric == "pointwise_coverage"), by = "group") |> mutate(d = mean_pointwise_coverage - mean))$d)))
chk("fig12", "S0 peak coverage equals peak_recovery_summary", abs(peak_s0$coverage - filter(sim_peak_rec, scenario == "S0_prior_calibration", fit_variant == "primary")$coverage))
chk("fig12", "paired effects equal the repaired summary", max(abs((sim_paired |> inner_join(sim_paired_full, by = c("scenario", "functional"), suffix = c("", "_s")) |> mutate(d = abs(mean_paired_log_error - mean_paired_log_error_s) + abs(log_error_mcse - log_error_mcse_s)))$d)))
chk("fig12", "repaired coverage MCSE equals verified file", max(abs((sim_paired_full |> inner_join(sim_mcse, by = c("scenario", "functional"), suffix = c("", "_v")) |> mutate(d = coverage_difference_mcse - coverage_difference_mcse_v))$d)))
stated <- tribble(~name, ~value, ~target, ~tol,
  "S0 mean annual coverage all years", sim_cal$mean_pointwise_coverage[sim_cal$group == "all"], .898, 5e-4,
  "S0 mean annual coverage post-1795", sim_cal$mean_pointwise_coverage[sim_cal$group == "post1795"], .886, 5e-4,
  "S0 mean annual coverage zero-quote years", sim_cal$mean_pointwise_coverage[sim_cal$group == "goods_0"], .871, 5e-4,
  "clean coverage 1822/1795", sim_truth$coverage[sim_truth$functional == "ratio_1822_to1795"], .24, 1e-9,
  "clean coverage 1818-24/1795", sim_truth$coverage[sim_truth$functional == "ratio_1818_1824_to1795"], .32, 1e-9,
  "clean coverage 1834/1795", sim_truth$coverage[sim_truth$functional == "ratio_1834_to1795"], .605, 1e-9,
  "clean coverage 1824/1822", sim_truth$coverage[sim_truth$functional == "ratio_1824_to1822"], .48, 1e-9,
  "clean coverage 1787/1785", sim_truth$coverage[sim_truth$functional == "ratio_1787_to1785"], .11, 1e-9,
  "clean coverage 1820-24/1770s", sim_truth$coverage[sim_truth$functional == "ratio_1820_1824_to1770s"], .865, 1e-9,
  "S1 paired log error 1820-24/1770s", sim_paired$mean_paired_log_error[sim_paired$scenario == "S1_fixed_source_wedge" & sim_paired$functional == "ratio_1820_1824_to1770s"], .12533, 5e-6,
  "S1 paired MCSE 1820-24/1770s", sim_paired$log_error_mcse[sim_paired$scenario == "S1_fixed_source_wedge" & sim_paired$functional == "ratio_1820_1824_to1770s"], .000487, 5e-7,
  "S0 peak interval coverage", peak_s0$coverage, .955, 1e-9,
  "S0 peak Brier score", sim_scores$mean_brier[sim_scores$fit_variant == "primary"], .1232, 5e-5)
for (i in seq_len(nrow(stated))) chk("fig12", paste("root-stated", stated$name[i]), abs(stated$value[i] - stated$target[i]), stated$tol[i])
chk("fig12", "one primary zero-mass replicate retained, unconditional log score not finite", abs(sim_scores$n_zero_empirical_mass[sim_scores$fit_variant == "primary"] - 1) + as.numeric(is.finite(sim_scores$empirical_mean_log_score[sim_scores$fit_variant == "primary"])), 0)
save_fig("fig12", "fig12_simulation_recovery", fig12, height = 6.3, placement = "simulation; short caption, full scope in appendix; for root at integration",
  inputs = c("paper_table_simulation_truth_comparison.csv", "paper_table_simulation_calibration.csv", "clean_basket_recovery_summary.csv", "paper_table_simulation_peak.csv", "AUTHOR_TABLE_paired_functionals.csv", "paired_stress_functional_summary.csv", "paired_coverage_mcse_verified.csv", "functional_recovery_summary.csv", "peak_recovery_summary.csv", "S0_peak_score_summary.csv", "AUTHOR_TABLE_S0_annual.csv"),
  encoding = "Top: coverage of nominal 90% intervals for latent targets (annual basket levels, peak year in 1795-1834, seven historical relatives) under prior-generated paths (S0, circles) and the clean fixed-path control (S4, triangles); dashed line marks .90. Rows of relatives are ordered by clean-control coverage. Bottom: replicate-paired stress-minus-clean effects for four relatives: mean change in log error and mean change in squared log error. Both designs re-estimate the seven variance parameters.",
  metadata = paste0("Whiskers are 2 Monte Carlo standard errors across 200 replicates; they are not price bands and treat replicates, not years, as independent. S0 averages over latent innovations and offsets given fixed initial levels and parameters; the clean control conditions on one preseeded posterior truth path and averages offsets and noise only. These are different estimands: prior calibration does not ensure coverage on every fixed path, and failure on this path does not identify historical bias. The fixed path was chosen before simulation and not reselected; its 1795 anchor is unusually low (index 95.6, below the empirical 5th percentile 102.7, median 125.0), and the clean mean fitted 1795 index is 118.4. Poor coverage of 1795-based relatives is a conditional result for this one path. Its 1795 anchor is locally unusual, but that does not make the whole path atypical, and this experiment alone cannot identify the historical direction of error. The relatives 1824/1822 and 1787/1785 do not use the 1795 anchor and also cover poorly on this path, so the low anchor is not the sole cause; smoothing of sharp local movements also contributes. Static fitted variance parameters do not generate latent volatility for the fixed path. A positive source wedge partly offsets the clean path's negative bias, so apparent improvement is not robustness; carry-forward effects depend on this wheat-heavy path and may understate damage. Peak window is 1795-1834 only, not the full period; mean peak-interval span ", round(peak_s0$mean_interval_years, 1), " years (S0), so high coverage does not establish sharp timing. S0 Brier score for a peak before 1825 is ", sprintf("%.3f", sim_scores$mean_brier[sim_scores$fit_variant == "primary"]), " (MCSE ", sprintf("%.3f", sim_scores$brier_mcse[sim_scores$fit_variant == "primary"]), "); one primary replicate assigns zero mass to the true year, so the unconditional mean log score is minus infinity and no pseudocount is used."))

# Checks, manifest, QA ---------------------------------------------------------------
checks <- bind_rows(check_log); write_csv(checks, file.path(stage, "figure_checks.csv"))
print(as.data.frame(checks |> select(figure, check, max_abs_difference, pass)), row.names = FALSE, digits = 3)
stopifnot(all(checks$pass))
fm <- bind_rows(figure_log) |>
  mutate(png_md5 = unname(tools::md5sum(file.path(stage, png_file))), pdf_md5 = unname(tools::md5sum(file.path(stage, pdf_file))),
         input_manifest = "docs/figure_inputs.csv",
         status = "core, validation and simulation figures")
write_csv(fm, file.path(stage, "figure_manifest.csv"))
png_size <- function(p) { b <- readBin(p, "raw", n = 24); c(sum(as.integer(b[17:20]) * 256^(3:0)), sum(as.integer(b[21:24]) * 256^(3:0))) }
qa <- map_dfr(seq_len(nrow(fm)), function(i) {
  f <- fm[i, ]; pdf <- file.path(stage, f$pdf_file); png <- file.path(stage, f$png_file)
  info <- pdftools::pdf_info(pdf); ps <- pdftools::pdf_pagesize(pdf); fo <- pdftools::pdf_fonts(pdf); px <- png_size(png)
  tibble(figure_id = f$figure_id, file_stem = f$file_stem, pdf_pages = info$pages, pdf_width_in = ps$width / 72, pdf_height_in = ps$height / 72,
         font_names = paste(unique(fo$name), collapse = "; "), fonts_all_embedded = all(fo$embedded), png_width_px = px[1], png_height_px = px[2])
})
write_csv(qa, file.path(stage, "figure_qa.csv"))
print(as.data.frame(qa), row.names = FALSE)
stopifnot(all(qa$fonts_all_embedded), all(qa$pdf_pages == 1), all(qa$png_width_px == 3900))
cat("Built", nrow(fm), "figures in", stage, "\n")

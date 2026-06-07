# =============================================================================
#
#   PATH DEPENDENCE IN ECONOMIC HISTORY PUBLICATIONS
#   02_figures.R — Figure Generation Script (Replication Package)
#
#   Loads results from 01_analysis.R and produces ALL 10 figures.
#   Assumes setwd() has been called to the PathDepPaper/ directory.
#
# =============================================================================

rm(list = ls())

# --- Packages ----------------------------------------------------------------
library(ggplot2)
library(data.table)
library(scales)
library(lfe)        # needed for coef/summary on felm model objects
library(patchwork)  # for multi-panel figures

# --- Paths (auto-detect the scripts/ directory robustly) --------------------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p)))
  if (file.exists("scripts/02_figures.R")) return(normalizePath("scripts"))
  if (file.exists("02_figures.R"))         return(normalizePath("."))
  normalizePath("PathDepPaper/scripts")
}
SCRIPT_DIR <- get_script_dir()
BASE_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/")
setwd(BASE_DIR)
cat("Working directory:", getwd(), "\n")

# --- Load results from 01_analysis.R ----------------------------------------
load(file.path(BASE_DIR, "results", "analysis_results.RData"))

# --- Paths -------------------------------------------------------------------
FIGURE_DIR <- file.path(BASE_DIR, "figures")
if (!dir.exists(FIGURE_DIR)) dir.create(FIGURE_DIR, recursive = TRUE)


# =============================================================================
# LEAP VISUAL IDENTITY
# =============================================================================

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
      plot.title = element_text(size = 11, face = "bold", color = "#2D2D2D",
                                margin = ggplot2::margin(b = 12), hjust = 0),
      plot.subtitle = element_text(size = 9, color = "#5A5A5A",
                                   margin = ggplot2::margin(b = 8), hjust = 0),
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


# =============================================================================
# FIGURE 1: Early vs Long-run Citations
# =============================================================================

cat("--- Figure 1: Early vs Long-run ---\n")

fig1 <- ggplot(est[cite_early > 0 & cite_longrun > 0],
               aes(x = cite_early, y = cite_longrun)) +
  geom_point(aes(colour = factor(fast_starter, levels = c(0, 1),
                                 labels = c("Other papers", "Fast starters"))),
             shape = 16, alpha = 0.4, size = 1.8) +
  geom_smooth(method = "lm", color = "#4A4A4A", linetype = "dashed",
              se = FALSE, linewidth = 0.7) +
  scale_colour_manual(values = c("Other papers" = unname(LEAP_COLORS["blue"]),
                                 "Fast starters" = unname(LEAP_COLORS["plum"]))) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "Early citations (age 2)",
       y = "Long-run citations (age 5\u20138)",
       colour = NULL,
       title = "Path Dependence in Citations") +
  theme_leap() +
  theme(legend.position = c(0.15, 0.85))

save_leap_fig(file.path(FIGURE_DIR, "Fig1_EarlyVsLongrun.png"),
              fig1, width = 6, height = 4.5)


# =============================================================================
# FIGURE 2: Citation Trajectories
# =============================================================================

cat("--- Figure 2: Trajectories ---\n")

traj <- est[!is.na(fast_starter),
            .(age1 = mean(cite_age_1, na.rm = TRUE),
              age2 = mean(cite_age_2, na.rm = TRUE),
              age3 = mean(cite_age_3, na.rm = TRUE),
              age5 = mean(cite_age_5, na.rm = TRUE),
              age8 = mean(cite_age_8, na.rm = TRUE)),
            by = fast_starter]

traj_long <- melt(traj, id.vars = "fast_starter",
                  variable.name = "age", value.name = "cites")
traj_long[, age := as.integer(gsub("age", "", age))]
traj_long[, group := factor(fast_starter,
                             levels = c(0, 1),
                             labels = c("Other", "Fast starters"))]
traj_long <- traj_long[!is.na(cites)]

fig2 <- ggplot(traj_long, aes(x = age, y = cites,
                               linetype = group, shape = group,
                               colour = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_linetype_manual(values = c("Other" = "dashed",
                                    "Fast starters" = "solid")) +
  scale_shape_manual(values = c("Other" = 1,
                                 "Fast starters" = 16)) +
  scale_colour_manual(values = c("Other" = unname(LEAP_COLORS["blue"]),
                                  "Fast starters" = unname(LEAP_COLORS["plum"]))) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 8)) +
  labs(x = "Years since publication",
       y = "Mean cumulative citations",
       linetype = NULL, shape = NULL, colour = NULL,
       title = "Citation Trajectories") +
  theme_leap() +
  theme(legend.position = c(0.2, 0.8))

save_leap_fig(file.path(FIGURE_DIR, "Fig2_Trajectories.png"),
              fig2, width = 6, height = 4.5)


# =============================================================================
# FIGURE 3: Coefficient Stability
# =============================================================================

cat("--- Figure 3: Coefficient Stability ---\n")

coef_df <- data.table(
  model = factor(c("OLS", "Controls", "J/Y FE", "Topic FE"),
                 levels = c("OLS", "Controls", "J/Y FE", "Topic FE")),
  coef = c(coef(m1)["log_early"], coef(m2)["log_early"],
           coef(m3)["log_early"], coef(m4)["log_early"]),
  se = c(summary(m1)$coefficients["log_early", "Std. Error"],
         summary(m2)$coefficients["log_early", "Std. Error"],
         summary(m3)$coefficients["log_early", "Std. Error"],
         summary(m4)$coefficients["log_early", "Std. Error"])
)

fig3 <- ggplot(coef_df, aes(x = model, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = LEAP_NONSIG_COLOR) +
  geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  colour = unname(LEAP_COLORS["plum"]), size = 0.6) +
  labs(x = NULL, y = "Coefficient on log(early citations)",
       title = "Coefficient Stability Across Specifications") +
  theme_leap()

save_leap_fig(file.path(FIGURE_DIR, "Fig3_Coefficients.png"),
              fig3, width = 6, height = 4.5)


# =============================================================================
# FIGURE 4: Leave-One-Journal-Out
# =============================================================================

cat("--- Figure 4: Leave-One-Journal-Out ---\n")

lojo[, excluded := factor(excluded, levels = excluded)]

fig4 <- ggplot(lojo, aes(x = excluded, y = coef)) +
  geom_hline(yintercept = coef(m3)["log_early"], linetype = "dashed",
             color = unname(LEAP_COLORS["rose"]), linewidth = 0.7) +
  geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  colour = unname(LEAP_COLORS["plum"]), size = 0.6) +
  annotate("text", x = 0.6, y = coef(m3)["log_early"],
           label = "Full-sample estimate", hjust = 0, vjust = -0.7,
           size = 3, colour = unname(LEAP_COLORS["rose"])) +
  labs(x = "Journal excluded", y = "Coefficient on log(early citations)",
       title = "Leave-One-Journal-Out Robustness") +
  theme_leap()

save_leap_fig(file.path(FIGURE_DIR, "Fig4_LOJO.png"),
              fig4, width = 6, height = 4.5)


# =============================================================================
# FIGURE 5: Placebo Fast-Starter
# =============================================================================

cat("--- Figure 5: Placebo Fast-Starter ---\n")

if (!exists("placebo_coefs_fs") || !exists("true_coef_fs")) {
  cat("  SKIPPED: placebo data not available.\n\n")
} else {

placebo_fs_dt <- data.table(coef = placebo_coefs_fs)

fig5 <- ggplot(placebo_fs_dt, aes(x = coef)) +
  geom_histogram(bins = 50, fill = unname(LEAP_COLORS["plum"]),
                 colour = NA, alpha = 0.85) +
  geom_vline(xintercept = true_coef_fs, colour = unname(LEAP_COLORS["rose"]),
             linewidth = 1, linetype = "solid") +
  annotate("text", x = true_coef_fs, y = Inf,
           label = paste0("True = ", round(true_coef_fs, 3),
                          "\np = ", round(emp_p_fs, 4)),
           hjust = -0.15, vjust = 1.5, colour = unname(LEAP_COLORS["rose"]),
           size = 4, fontface = "bold") +
  labs(x = "Placebo fast-starter coefficient",
       y = "Count",
       title = "Placebo Test: Fast-Starter Effect",
       subtitle = paste0(length(placebo_coefs_fs),
                         " permutations (shuffled within journal\u2013year cohorts)")) +
  theme_leap()

save_leap_fig(file.path(FIGURE_DIR, "Fig5_PlaceboFastStarter.png"),
              fig5, width = 6, height = 4.5)

} # end placebo_coefs_fs check


# =============================================================================
# FIGURE 6: Citation Source (composition by discipline + document type)
# =============================================================================
#
# The earlier within/cross-field coefficient plot rested on OpenAlex's single
# top-concept, which misclassifies discipline (see verify_crossfield.R). The
# corrected figure -- citation shares by citing DISCIPLINE and by document type
# -- is produced by the self-contained scripts/fig_citation_source.R, which
# reads the re-queried discipline cache (data/citing_field_data.rds).

cat("--- Figure 6: Citation Source (composition) ---\n")

fig6_script <- file.path(SCRIPT_DIR, "fig_citation_source.R")
if (file.exists(file.path(BASE_DIR, "data", "citing_field_data.rds")) &&
    file.exists(fig6_script)) {
  source(fig6_script)
} else {
  cat("  SKIPPED: run scripts/verify_crossfield.R (to build citing_field_data.rds)\n",
      "  and ensure scripts/fig_citation_source.R is present.\n\n")
}


# =============================================================================
# FIGURE 7: Citation Concentration (HHI)
# =============================================================================

cat("--- Figure 7: Citation Concentration ---\n")

if (!"cite_concentration" %in% names(mech)) {
  cat("  SKIPPED: cite_concentration not in mech (network metrics not loaded).\n\n")
} else {

plot_data_hhi <- mech[!is.na(cite_concentration) & !is.na(fast_starter)]
plot_data_hhi[, fs_label := factor(fast_starter,
                                    levels = c(0, 1),
                                    labels = c("Non-fast starter",
                                               "Fast starter"))]

medians_hhi <- plot_data_hhi[, .(med = median(cite_concentration)), by = fs_label]

fig7 <- ggplot(plot_data_hhi, aes(x = cite_concentration, fill = fs_label)) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(data = medians_hhi, aes(xintercept = med, colour = fs_label),
             linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("Non-fast starter" = unname(LEAP_COLORS["blue"]),
                                "Fast starter" = unname(LEAP_COLORS["plum"]))) +
  scale_colour_manual(values = c("Non-fast starter" = unname(LEAP_COLORS["blue"]),
                                  "Fast starter" = unname(LEAP_COLORS["plum"]))) +
  labs(x = "Citation Concentration (HHI)",
       y = "Density",
       fill = NULL,
       title = "Citation Temporal Concentration",
       subtitle = "Lower HHI = citations spread over more years") +
  theme_leap()

save_leap_fig(file.path(FIGURE_DIR, "Fig7_CitationConcentration.png"),
              fig7, width = 6, height = 4.5)

} # end cite_concentration check


# =============================================================================
# FIGURE 8: Topic Heterogeneity (Forest Plot)
# =============================================================================

cat("--- Figure 8: Topic Heterogeneity ---\n")

# topic_results and overall_coef come from 01_analysis.R Section 7
topic_results[, topic_label := paste0(topic, " (n=", n, ")")]
setorder(topic_results, coef)
topic_results[, topic_label := factor(topic_label, levels = topic_label)]

fig8 <- ggplot(topic_results, aes(x = coef, y = topic_label)) +
  geom_vline(xintercept = overall_coef, linetype = "dashed",
             colour = unname(LEAP_COLORS["rose"]), linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = LEAP_NONSIG_COLOR) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2,
                 colour = unname(LEAP_COLORS["plum"]), linewidth = 0.8) +
  geom_point(colour = unname(LEAP_COLORS["plum"]), size = 3) +
  annotate("text", x = overall_coef, y = 0.5,
           label = paste0("Overall: ", round(overall_coef, 3)),
           hjust = -0.1, size = 3, colour = unname(LEAP_COLORS["rose"])) +
  labs(x = "log(early citations) elasticity (with 95% CI)",
       y = NULL,
       title = "Path Dependence Elasticity by Topic",
       subtitle = "Dashed line = overall estimate from M3") +
  theme_leap() +
  theme(panel.grid.major.y = element_blank())

save_leap_fig(file.path(FIGURE_DIR, "Fig8_TopicHeterogeneity.png"),
              fig8, width = 6, height = 5)


# =============================================================================
# FIGURE 9: Placebo Conference
# =============================================================================

cat("--- Figure 9: Placebo Conference ---\n")

if (exists("placebo_conf_coefs") && exists("true_conf_coef") &&
    exists("emp_p_conf")) {

  placebo_conf_dt <- data.table(coef = placebo_conf_coefs)

  fig9 <- ggplot(placebo_conf_dt, aes(x = coef)) +
    geom_histogram(bins = 50, fill = unname(LEAP_COLORS["plum"]),
                   colour = NA, alpha = 0.85) +
    geom_vline(xintercept = true_conf_coef, colour = unname(LEAP_COLORS["rose"]),
               linewidth = 1) +
    annotate("text", x = true_conf_coef, y = Inf,
             label = paste0("True = ", round(true_conf_coef, 3),
                            "\np = ", round(emp_p_conf, 4)),
             hjust = -0.15, vjust = 1.5, colour = unname(LEAP_COLORS["rose"]),
             size = 4, fontface = "bold") +
    labs(x = "Placebo conference coefficient",
         y = "Count",
         title = "Placebo Test: Conference Effect",
         subtitle = paste0(length(placebo_conf_coefs),
                           " permutations (shuffled within journal\u2013year cohorts)")) +
    theme_leap()

  save_leap_fig(file.path(FIGURE_DIR, "Fig9_PlaceboConference.png"),
                fig9, width = 6, height = 4.5)

} else {
  cat("  Conference placebo data not available. Skipping Fig 9.\n")
}


# =============================================================================
# FIGURE 10: Cascade Depth (Two-panel)
# =============================================================================

cat("--- Figure 10: Cascade Depth ---\n")

if ("cascade_depth" %in% names(mech)) {

  # Requires patchwork for two-panel layout
  # patchwork loaded at script start
  library(patchwork)

  plot_data_cd <- mech[!is.na(fast_starter) & cascade_depth >= 0]
  plot_data_cd[, fs_label := factor(fast_starter,
                                     levels = c(0, 1),
                                     labels = c("Non-fast starter",
                                                "Fast starter"))]

  # Panel A: Violin + box plot of cascade depth
  panel_a <- ggplot(plot_data_cd,
                    aes(x = fs_label, y = cascade_depth, fill = fs_label)) +
    geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75),
                colour = NA) +
    geom_boxplot(width = 0.15, fill = "white", alpha = 0.7,
                 outlier.size = 0.5) +
    scale_fill_manual(values = c("Non-fast starter" = unname(LEAP_COLORS["blue"]),
                                  "Fast starter" = unname(LEAP_COLORS["plum"])),
                      guide = "none") +
    labs(x = NULL, y = "Cascade depth",
         title = "(a) Citation Cascade Depth") +
    theme_leap()

  # Panel B: Scatter of direct vs indirect citers
  panel_b <- ggplot(plot_data_cd[n_direct_citers > 0 & n_indirect_citers > 0],
                    aes(x = n_direct_citers, y = n_indirect_citers,
                        colour = fs_label)) +
    geom_point(alpha = 0.5, size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    scale_colour_manual(values = c("Non-fast starter" = unname(LEAP_COLORS["blue"]),
                                    "Fast starter" = unname(LEAP_COLORS["plum"])),
                        name = NULL) +
    scale_x_log10() + scale_y_log10() +
    labs(x = "Direct citers (log scale)",
         y = "Indirect citers (log scale)",
         title = "(b) Direct vs Indirect Citers") +
    theme_leap()

  fig10 <- panel_a + panel_b + plot_layout(ncol = 2)

  save_leap_fig(file.path(FIGURE_DIR, "Fig10_CascadeDepth.png"),
                fig10, width = 10, height = 4.5)

} else {
  cat("  Cascade depth data not available. Skipping Fig 10.\n")
}


# =============================================================================
# FIGURE 11: Predictable vs idiosyncratic early citations and long-run impact
# =============================================================================

cat("--- Figure 11: Unpredictability and amplification ---\n")

if (all(c("early_pred", "early_resid") %in% names(est))) {

  # Centre (NOT standardise) each early-citation component, bin into deciles, and
  # plot the mean long-run outcome. Centring keeps each series on the natural
  # log-citation scale, so the plotted slope equals the raw elasticity from
  # Table~tab:luck (predictable ~1.04, unexplained ~0.78). Standardising would
  # divide by each component's SD and make the higher-variance residual look
  # spuriously steeper.
  bin_df <- function(x, label) {
    xc <- x - mean(x, na.rm = TRUE)
    d <- data.table(xc = xc, y = est$log_longrun,
                    bin = cut(xc, breaks = quantile(xc, probs = seq(0, 1, 0.1), na.rm = TRUE),
                              include.lowest = TRUE, labels = FALSE))
    out <- d[!is.na(bin), .(xc = mean(xc), y = mean(y)), by = bin]
    out[, series := label]
    out
  }
  fig11_dat <- rbind(
    bin_df(est$early_pred,  "Predictable (fundamentals)"),
    bin_df(est$early_resid, "Unexplained (residual)"))

  fig11 <- ggplot(fig11_dat, aes(x = xc, y = y, colour = series, shape = series)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("Predictable (fundamentals)" = unname(LEAP_COLORS["blue"]),
                                   "Unexplained (residual)" = unname(LEAP_COLORS["plum"]))) +
    scale_shape_manual(values = c("Predictable (fundamentals)" = 16,
                                  "Unexplained (residual)" = 17)) +
    labs(x = "Early-citation component, centred (decile means, log scale)",
         y = "Mean log(1 + long-run citations)",
         colour = NULL, shape = NULL,
         title = "The unexplained component of early citations propagates almost as strongly as the predictable part") +
    theme_leap() +
    theme(legend.position = c(0.27, 0.9))

  save_leap_fig(file.path(FIGURE_DIR, "Fig11_Unpredictability.png"),
                fig11, width = 6.5, height = 4.5)

} else {
  cat("  Decomposition data (early_pred/early_resid) not available. Skipping Fig 11.\n")
}


# =============================================================================
cat("\n====================================================================\n")
cat("  ALL FIGURES COMPLETE\n")
cat("====================================================================\n")

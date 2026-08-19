# =============================================================================
# 10_figures.R -- all figures for the paper and appendices
# -----------------------------------------------------------------------------
# Reads only the raw/cache data and the compact results objects written by
# scripts 01-08 (results/res_*.rds); it does not re-estimate anything.
#
# Main text:
#   Fig_Persistence        two panels: (a) early vs long-run scatter,
#                          (b) mean citation trajectories by fast-starter status
#   Fig_Unpredictability   explained vs unexplained early-citation components
#                          against long-run citations (decile means)
#   Fig_CitationSource     citing works by (a) discipline and (b) document type
# Appendices:
#   FigB_LOJO                  leave-one-journal-out estimates
#   FigB_PlaceboFastStarter    permutation distribution vs true estimate
#   FigB_TopicHeterogeneity    elasticity by topic (forest plot)
#   FigB_CohortElasticity      elasticity by publication cohort (new)
#   FigE_PlaceboConference     conference permutation distribution
#
# Each figure is saved as PNG (600 dpi) and PDF in output/figures/.
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  sd <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else
        if (file.exists("scripts/_setup.R")) "scripts" else "."
  source(file.path(sd, "_setup.R"))
})

suppressMessages(library(patchwork))

open_log("10_figures")

ad     <- readRDS(file.path(RESULTS_DIR, "analysis_data.rds"))
est    <- ad$est
res03  <- readRDS(file.path(RESULTS_DIR, "res_03_robustness.rds"))
res04  <- readRDS(file.path(RESULTS_DIR, "res_04_placebo.rds"))
res05  <- readRDS(file.path(RESULTS_DIR, "res_05_conference.rds"))
res07  <- readRDS(file.path(RESULTS_DIR, "res_07_heterogeneity.rds"))
res08  <- readRDS(file.path(RESULTS_DIR, "res_08_luck.rds"))

# =============================================================================
# Fig_Persistence: (a) scatter + (b) trajectories
# =============================================================================
cat("--- Fig_Persistence ---\n")

pA <- ggplot(est[cite_early > 0 & cite_longrun > 0],
             aes(x = cite_early, y = cite_longrun)) +
  geom_point(aes(colour = factor(fast_starter, levels = c(0, 1),
                                 labels = c("Other papers", "Fast starters"))),
             shape = 16, alpha = 0.4, size = 1.6) +
  geom_smooth(method = "lm", color = "#4A4A4A", linetype = "dashed",
              se = FALSE, linewidth = 0.7) +
  scale_colour_manual(values = c("Other papers" = unname(LEAP_COLORS["blue"]),
                                 "Fast starters" = unname(LEAP_COLORS["plum"]))) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "Early citations (age 2)",
       y = "Long-run citations (age 5–8)",
       colour = NULL, title = "(a) Early vs long-run citations") +
  theme_leap() +
  theme(legend.position = c(0.22, 0.88))

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
traj_long[, group := factor(fast_starter, levels = c(0, 1),
                             labels = c("Other", "Fast starters"))]
traj_long <- traj_long[!is.na(cites)]

pB <- ggplot(traj_long, aes(x = age, y = cites,
                             linetype = group, shape = group, colour = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6) +
  scale_linetype_manual(values = c("Other" = "dashed", "Fast starters" = "solid")) +
  scale_shape_manual(values = c("Other" = 1, "Fast starters" = 16)) +
  scale_colour_manual(values = c("Other" = unname(LEAP_COLORS["blue"]),
                                  "Fast starters" = unname(LEAP_COLORS["plum"]))) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 8)) +
  labs(x = "Years since publication", y = "Mean cumulative citations",
       linetype = NULL, shape = NULL, colour = NULL,
       title = "(b) Citation trajectories") +
  theme_leap() +
  theme(legend.position = c(0.25, 0.85))

save_fig("Fig_Persistence", pA + pB + plot_layout(ncol = 2), width = 10, height = 5)

# =============================================================================
# Fig_Unpredictability: decomposition components vs long-run citations
# =============================================================================
cat("--- Fig_Unpredictability ---\n")

dec <- res08$est_decomp

# Centre (NOT standardise) each component, bin into deciles, plot mean long-run
# outcome. Centring keeps both series on the natural log-citation scale, so the
# plotted slopes equal the estimated elasticities. Standardising would divide
# by each component's SD and make the higher-variance residual look steeper.
bin_df <- function(x, y, label) {
  xc <- x - mean(x, na.rm = TRUE)
  d <- data.table(xc = xc, y = y,
                  bin = cut(xc, breaks = quantile(xc, probs = seq(0, 1, 0.1), na.rm = TRUE),
                            include.lowest = TRUE, labels = FALSE))
  out <- d[!is.na(bin), .(xc = mean(xc), y = mean(y)), by = bin]
  out[, series := label]
  out
}
fig_u_dat <- rbind(
  bin_df(dec$early_pred,  dec$log_longrun, "Explained (fundamentals)"),
  bin_df(dec$early_resid, dec$log_longrun, "Unexplained (residual)"))

fig_u <- ggplot(fig_u_dat, aes(x = xc, y = y, colour = series, shape = series)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Explained (fundamentals)" = unname(LEAP_COLORS["blue"]),
                                 "Unexplained (residual)" = unname(LEAP_COLORS["plum"]))) +
  scale_shape_manual(values = c("Explained (fundamentals)" = 16,
                                "Unexplained (residual)" = 17)) +
  labs(x = "Early-citation component, centred (decile means, log scale)",
       y = "Mean log(1 + long-run citations)",
       colour = NULL, shape = NULL) +
  theme_leap() +
  theme(legend.position = c(0.27, 0.9))

save_fig("Fig_Unpredictability", fig_u)

# =============================================================================
# Fig_CitationSource: discipline and document type of citing works
# =============================================================================
cat("--- Fig_CitationSource ---\n")

cr <- readRDS(file.path(DATA_CACHE, "network_citation_data.rds")); setDT(cr)
cr[, cs := gsub("https://openalex.org/", "", citing_oa_id)]
fd <- readRDS(file.path(DATA_CACHE, "citing_field_data.rds")); setDT(fd)
lk <- merge(cr, fd, by.x = "cs", by.y = "citing_oa_id", all.x = TRUE)

# Discipline (mutually exclusive, by primary_topic), grouped for colour
lk[, disc := fifelse(pt_field == "Economics, Econometrics and Finance", "Economics",
              fifelse(pt_subfield == "History", "History",
              fifelse(pt_field == "Social Sciences", "Other social sciences",
              fifelse(pt_field == "Business, Management and Accounting", "Business & management",
              fifelse(pt_field == "Arts and Humanities", "Arts & humanities",
              fifelse(pt_field %in% c("Medicine","Biochemistry, Genetics and Molecular Biology",
                        "Agricultural and Biological Sciences","Health Professions","Nursing",
                        "Immunology and Microbiology","Neuroscience","Psychology",
                        "Pharmacology, Toxicology and Pharmaceutics","Dentistry","Veterinary"),
                        "Health & life sciences",
              fifelse(is.na(pt_field), "Unclassified", "Other sciences & engineering")))))))]

disc_lab <- c("Economics","History","Other social sciences","Business & management",
              "Arts & humanities","Health & life sciences","Other sciences & engineering")
dd <- lk[disc != "Unclassified", .(N = .N), by = disc]
dd[, pct := 100 * N / sum(N)]
dd[, group := fifelse(disc %in% c("Economics","History"), "Economic history & economics",
              fifelse(disc %in% c("Other social sciences","Business & management"),
                      "Adjacent social science", "Other fields"))]
dd[, disc := factor(disc, levels = rev(disc_lab))]

grp_cols <- c("Economic history & economics" = unname(LEAP_COLORS["plum"]),
              "Adjacent social science"        = unname(LEAP_COLORS["blue"]),
              "Other fields"                   = unname(LEAP_COLORS["gold"]))

pA2 <- ggplot(dd, aes(x = pct, y = disc, fill = group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.5, colour = "grey20") +
  scale_fill_manual(values = grp_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Share of all citations (%)", y = NULL, title = "(a) By discipline") +
  theme_leap() + theme(legend.position = "none")

lk[, fmt := fifelse(type == "article", "Journal article",
             fifelse(type %in% c("book-chapter","book"), "Book or chapter",
             fifelse(type %in% c("preprint","report"), "Preprint / working paper",
             fifelse(type == "dissertation", "Dissertation", "Other"))))]
ff <- lk[!is.na(type), .(N = .N), by = fmt]
ff[, pct := 100 * N / sum(N)]
fmt_lev <- c("Journal article","Book or chapter","Preprint / working paper","Dissertation","Other")
ff[, fmt := factor(fmt, levels = rev(fmt_lev))]
ff[, grp := fifelse(fmt == "Journal article", "Journal article", "Other formats")]

pB2 <- ggplot(ff, aes(x = pct, y = fmt, fill = grp)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.5, colour = "grey20") +
  scale_fill_manual(values = c("Journal article" = unname(LEAP_COLORS["sage"]),
                               "Other formats"   = unname(LEAP_COLORS["earth"]))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Share of all citations (%)", y = NULL, title = "(b) By document type") +
  theme_leap() + theme(legend.position = "none")

save_fig("Fig_CitationSource", pA2 / pB2)

cat("Discipline shares:\n"); print(dd[order(-pct), .(disc, pct = round(pct, 1))])
cat("\nFormat shares:\n");   print(ff[order(-pct), .(fmt, pct = round(pct, 1))])
rm(cr, fd, lk); invisible(gc())

# =============================================================================
# FigB_LOJO: leave-one-journal-out
# =============================================================================
cat("--- FigB_LOJO ---\n")

lojo <- copy(res03$lojo)
lojo[, excluded := factor(excluded, levels = excluded)]
m3_coef <- res03$m3$coef

figb1 <- ggplot(lojo, aes(x = excluded, y = coef)) +
  geom_hline(yintercept = m3_coef, linetype = "dashed",
             color = unname(LEAP_COLORS["rose"]), linewidth = 0.7) +
  geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  colour = unname(LEAP_COLORS["plum"]), size = 0.6) +
  annotate("text", x = 0.6, y = m3_coef,
           label = "Full-sample estimate", hjust = 0, vjust = -0.7,
           size = 3.5, colour = unname(LEAP_COLORS["rose"])) +
  labs(x = "Journal excluded", y = "Coefficient on log(early citations)") +
  theme_leap()

save_fig("FigB_LOJO", figb1)

# =============================================================================
# FigB_PlaceboFastStarter
# =============================================================================
cat("--- FigB_PlaceboFastStarter ---\n")

placebo_fs_dt <- data.table(coef = res04$placebo_coefs_fs)
true_coef_fs <- res04$true_coef_fs

figb2 <- ggplot(placebo_fs_dt, aes(x = coef)) +
  geom_histogram(bins = 50, fill = unname(LEAP_COLORS["plum"]),
                 colour = NA, alpha = 0.85) +
  geom_vline(xintercept = true_coef_fs, colour = unname(LEAP_COLORS["rose"]),
             linewidth = 1, linetype = "solid") +
  annotate("text", x = true_coef_fs, y = Inf,
           label = paste0("True = ", round(true_coef_fs, 3),
                          "\np = ", round(res04$emp_p_fs, 4)),
           hjust = 1.1, vjust = 1.5, colour = unname(LEAP_COLORS["rose"]),
           size = 3.5, fontface = "bold") +
  labs(x = "Placebo fast-starter coefficient", y = "Count") +
  theme_leap()

save_fig("FigB_PlaceboFastStarter", figb2)

# =============================================================================
# FigB_TopicHeterogeneity
# =============================================================================
cat("--- FigB_TopicHeterogeneity ---\n")

topic_results <- copy(res07$topic_results)
overall_coef <- res07$overall_coef
topic_results[, topic_label := paste0(topic, " (n=", n, ")")]
setorder(topic_results, coef)
topic_results[, topic_label := factor(topic_label, levels = topic_label)]

figb3 <- ggplot(topic_results, aes(x = coef, y = topic_label)) +
  geom_vline(xintercept = overall_coef, linetype = "dashed",
             colour = unname(LEAP_COLORS["rose"]), linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = LEAP_NONSIG_COLOR) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2,
                 colour = unname(LEAP_COLORS["plum"]), linewidth = 0.8) +
  geom_point(colour = unname(LEAP_COLORS["plum"]), size = 3) +
  annotate("text", x = overall_coef, y = 0.5,
           label = paste0("Overall: ", round(overall_coef, 3)),
           hjust = -0.1, size = 3.5, colour = unname(LEAP_COLORS["rose"])) +
  labs(x = "log(early citations) elasticity (with 95% CI)", y = NULL) +
  theme_leap() +
  theme(panel.grid.major.y = element_blank())

save_fig("FigB_TopicHeterogeneity", figb3)

# =============================================================================
# FigB_CohortElasticity (new in this revision)
# =============================================================================
cat("--- FigB_CohortElasticity ---\n")

coh <- copy(res07$cohort$by_cohort)
pooled <- res07$cohort$trend

figb4 <- ggplot(coh, aes(x = cohort, y = coef)) +
  geom_hline(yintercept = weighted.mean(coh$coef, coh$n), linetype = "dashed",
             colour = unname(LEAP_COLORS["rose"]), linewidth = 0.6) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                  colour = unname(LEAP_COLORS["plum"]), size = 0.6) +
  scale_x_continuous(breaks = coh$cohort) +
  labs(x = "Publication cohort",
       y = "Elasticity of age-5 on age-2 citations") +
  theme_leap()

save_fig("FigB_CohortElasticity", figb4)

# =============================================================================
# FigE_PlaceboConference
# =============================================================================
cat("--- FigE_PlaceboConference ---\n")

if (!is.null(res05$placebo$coefs)) {
  placebo_conf_dt <- data.table(coef = res05$placebo$coefs)
  true_conf_coef <- res05$placebo$true_coef

  fige1 <- ggplot(placebo_conf_dt, aes(x = coef)) +
    geom_histogram(bins = 50, fill = unname(LEAP_COLORS["plum"]),
                   colour = NA, alpha = 0.85) +
    geom_vline(xintercept = true_conf_coef, colour = unname(LEAP_COLORS["rose"]),
               linewidth = 1) +
    annotate("text", x = true_conf_coef, y = Inf,
             label = paste0("True = ", round(true_conf_coef, 3),
                            "\np = ", round(res05$placebo$emp_p, 4)),
             hjust = -0.15, vjust = 1.5, colour = unname(LEAP_COLORS["rose"]),
             size = 3.5, fontface = "bold") +
    labs(x = "Placebo conference coefficient", y = "Count") +
    theme_leap()

  save_fig("FigE_PlaceboConference", fige1)
} else {
  cat("  Conference placebo results not available; skipped.\n")
}

cat("\nAll figures complete.\n")
close_log()

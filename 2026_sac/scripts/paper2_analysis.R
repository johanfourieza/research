###############################################################################
# paper2_analysis.R
#
# Integrating the Enemy: Sorting, Statistical Discrimination, and
# Organisational Costs in the South African Constabulary, 1900-1908
#
# Standalone analysis script. Requires sac_clean.rds from 01_clean_data.R.
#
# Outputs: Tables 1-5, Figures 1-11 (main paper)
#          Tables A1-A3, Figures A1-A2 (appendix)
#          Extension figures: ext2 (Oaxaca), ext5c (completion), ext1b (balance)
#          Extension tables: ext1, ext2, ext5 (CSV)
#          Console output for text-only results (placebos, permutation,
#          ordered probit, cost calculation, within-rank character,
#          employer learning, quality heterogeneity, character bias)
###############################################################################

# ── Packages ────────────────────────────────────────────────────────────────

library(MASS)          # polr(); load before tidyverse to avoid select() mask
library(tidyverse)
library(fixest)
library(modelsummary)
library(survival)
library(cmprsk)
library(patchwork)
library(ggsurvfit)
library(scales)

select <- dplyr::select

has_rddensity <- requireNamespace("rddensity", quietly = TRUE)
if (has_rddensity) library(rddensity)

has_oaxaca <- requireNamespace("oaxaca", quietly = TRUE)
if (has_oaxaca) library(oaxaca)

# ── Setup ───────────────────────────────────────────────────────────────────

# 2026_sac public release. Run from the 2026_sac/ folder. Reads the
# cleaned data from data/sac_clean.rds (also available as
# data/sac_clean.csv) and writes all tables, figures and models into
# tables/, figures/ and models/ subfolders that are created on demand.
cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (basename(cwd) != "2026_sac") {
  stop("Run this script from the 2026_sac/ folder ",
       "(the package root that contains data/ and scripts/).")
}
work_dir  <- cwd

sac       <- readRDS(file.path(work_dir, "data", "sac_clean.rds"))

# Use the manually reviewed Afrikaner indicator as the primary treatment
# definition when available, while preserving the original baseline for
# robustness and auditability.
if ("afrikaans_reviewed" %in% names(sac)) {
  sac <- sac %>%
    mutate(
      afrikaans_baseline = afrikaans,
      afrikaans = afrikaans_reviewed
    )
} else {
  sac <- sac %>%
    mutate(afrikaans_baseline = afrikaans)
}

if (!"afrikaner_manual_override" %in% names(sac)) {
  sac <- sac %>%
    mutate(afrikaner_manual_override = 0L)
}

cat("Primary Afrikaans definition:\n")
cat("  Reviewed afrikaans count:", sum(sac$afrikaans == 1, na.rm = TRUE), "\n")
cat("  Original baseline count:", sum(sac$afrikaans_baseline == 1, na.rm = TRUE), "\n")
cat("  Manual overrides:", sum(sac$afrikaner_manual_override == 1, na.rm = TRUE), "\n")

out_dir <- file.path(work_dir, "output")
tab_dir <- file.path(work_dir, "tables")
fig_dir <- file.path(work_dir, "figures")
mod_dir <- file.path(work_dir, "models")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(mod_dir, showWarnings = FALSE, recursive = TRUE)

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

# Scale functions for ggplot2
scale_fill_leap <- function(...) {
  scale_fill_manual(values = LEAP_CYCLE, ...)
}

scale_color_leap <- function(...) {
  scale_color_manual(values = LEAP_CYCLE, ...)
}

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
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

theme_set(theme_leap())

# Colours — LEAP palette
col_afr     <- "#5C2346"   # plum (primary)
col_non_afr <- "#3D8EB9"   # blue (secondary)
main_fixest_vcov <- ~ enlist_ym
alt_fixest_vcov  <- "hetero"
surv_cluster_var <- "id"

treaty_date <- as.Date("1902-05-31")

cat("Loaded", nrow(sac), "observations.\n\n")

extract_fixest_dual <- function(model, term = "afrikaans",
                                cluster_vcov = main_fixest_vcov,
                                robust_vcov = alt_fixest_vcov) {
  ct_cluster <- coeftable(summary(model, vcov = cluster_vcov))
  ct_robust  <- coeftable(summary(model, vcov = robust_vcov))
  rn_cluster <- rownames(ct_cluster)
  rn_robust  <- rownames(ct_robust)

  resolve_term <- function(requested, candidates) {
    if (requested %in% candidates) return(requested)

    if (grepl(":", requested, fixed = TRUE)) {
      parts <- strsplit(requested, ":", fixed = TRUE)[[1]]
      hit <- candidates[vapply(candidates, function(x) all(vapply(parts, grepl, logical(1), x, fixed = TRUE)), logical(1))]
      if (length(hit) >= 1) return(hit[1])
    }

    NA_character_
  }

  term_cluster <- resolve_term(term, rn_cluster)
  term_robust  <- resolve_term(term, rn_robust)

  if (is.na(term_cluster) || is.na(term_robust)) {
    return(c(
      est       = NA_real_,
      se_clust  = NA_real_,
      p_clust   = NA_real_,
      se_robust = NA_real_,
      p_robust  = NA_real_,
      n         = model$nobs,
      r2        = tryCatch(fitstat(model, "r2")[[1]], error = function(e) NA_real_),
      wr2       = tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA_real_)
    ))
  }

  c(
    est       = unname(ct_cluster[term_cluster, "Estimate"]),
    se_clust  = unname(ct_cluster[term_cluster, "Std. Error"]),
    p_clust   = unname(ct_cluster[term_cluster, "Pr(>|t|)"]),
    se_robust = unname(ct_robust[term_robust, "Std. Error"]),
    p_robust  = unname(ct_robust[term_robust, "Pr(>|t|)"]),
    n         = model$nobs,
    r2        = tryCatch(fitstat(model, "r2")[[1]], error = function(e) NA_real_),
    wr2       = tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA_real_)
  )
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

stars_from_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

norm_diff <- function(x_treat, x_ctrl) {
  m_t <- mean(x_treat, na.rm = TRUE)
  m_c <- mean(x_ctrl, na.rm = TRUE)
  s_t <- sd(x_treat, na.rm = TRUE)
  s_c <- sd(x_ctrl, na.rm = TRUE)
  denom <- sqrt((s_t^2 + s_c^2) / 2)
  if (is.na(denom) || denom == 0) return(NA_real_)
  (m_t - m_c) / denom
}

oster_delta_zero <- function(beta_tilde, beta_hat, r_tilde, r_hat, r_max = NULL) {
  if (is.null(r_max)) r_max <- min(1, 1.3 * r_hat)
  denom <- (beta_tilde - beta_hat) * (r_max - r_hat)
  numer <- beta_hat * (r_hat - r_tilde)
  if (is.na(denom) || abs(denom) < 1e-12) return(NA_real_)
  numer / denom
}


###########################################################################
#
#   SECTION 2 — HISTORICAL CONTEXT
#
###########################################################################

# ── Figure 1: Recruitment shift at the Treaty (2 panels) ────────────────

cat("=== Figure 1: Recruitment Shift ===\n")

monthly <- sac %>%
  filter(!is.na(date_enlist), !is.na(afrikaans)) %>%
  mutate(enlist_month = floor_date(date_enlist, "month"),
         group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans")) %>%
  group_by(enlist_month, group) %>%
  summarise(count = n(), .groups = "drop")

monthly_share <- sac %>%
  filter(!is.na(date_enlist), !is.na(afrikaans)) %>%
  mutate(enlist_month = floor_date(date_enlist, "month")) %>%
  group_by(enlist_month) %>%
  summarise(afr_share = mean(afrikaans, na.rm = TRUE), n = n(), .groups = "drop")

fig1a <- ggplot(monthly, aes(x = enlist_month, y = count, colour = group)) +
  geom_line() + geom_point(size = 1) +
  geom_vline(xintercept = treaty_date, linetype = "dashed", colour = "grey40") +
  annotate("text", x = treaty_date, y = Inf, label = "Treaty of\nVereeniging",
           vjust = 1.5, hjust = -0.1, size = 3, colour = "grey40") +
  labs(x = NULL, y = "Monthly enlistments", colour = NULL,
       title = "A. Enlistment counts by group") +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

fig1b <- ggplot(monthly_share, aes(x = enlist_month, y = afr_share)) +
  geom_line(colour = col_afr) + geom_point(size = 1, colour = col_afr) +
  geom_vline(xintercept = treaty_date, linetype = "dashed", colour = "grey40") +
  labs(x = "Enlistment month", y = "Afrikaans share",
       title = "B. Afrikaans share of recruits") +
  scale_y_continuous(labels = percent_format())

fig1 <- fig1a / fig1b
save_leap_fig(file.path(fig_dir, "fig1_recruitment_shift.png"), fig1, width = 10, height = 7)
cat("  Saved Figure 1.\n")


###########################################################################
#
#   SECTION 3 — DATA AND DESCRIPTIVE PATTERNS
#
###########################################################################

# ── Table 1: Summary statistics (3-panel) ───────────────────────────────

cat("\n=== Table 1: Summary Statistics ===\n")

make_panel <- function(vars, panel_label) {
  sac %>%
    filter(!is.na(afrikaans)) %>%
    select(afrikaans, all_of(vars)) %>%
    pivot_longer(-afrikaans, names_to = "Variable", values_to = "value") %>%
    group_by(Variable) %>%
    summarise(
      Mean_NonAfr = mean(value[afrikaans == 0], na.rm = TRUE),
      SD_NonAfr   = sd(value[afrikaans == 0], na.rm = TRUE),
      N_NonAfr    = sum(!is.na(value[afrikaans == 0])),
      Mean_Afr    = mean(value[afrikaans == 1], na.rm = TRUE),
      SD_Afr      = sd(value[afrikaans == 1], na.rm = TRUE),
      N_Afr       = sum(!is.na(value[afrikaans == 1])),
      Diff        = Mean_Afr - Mean_NonAfr,
      Norm_Diff   = norm_diff(value[afrikaans == 1], value[afrikaans == 0]),
      p_value     = tryCatch(
        t.test(value[afrikaans == 1], value[afrikaans == 0])$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(Panel = panel_label,
           Variable = factor(Variable, levels = vars)) %>%
    arrange(Variable)
}

panel_a <- make_panel(
  c("age", "height_cm", "weight_kg", "married", "farmer",
    "prior_army", "can_ride", "can_shoot", "can_swim", "speaks_dutch"),
  "A. Pre-determined characteristics"
)

panel_b <- make_panel(
  c("wage", "lwage", "contract_years", "class"),
  "B. Assignment variables"
)

panel_c <- make_panel(
  c("tenure_days", "dismissed_deserted", "voluntary_exit",
    "time_expired", "good_character", "character_score"),
  "C. Outcomes"
)

table1 <- bind_rows(panel_a, panel_b, panel_c)
print(as.data.frame(table1 %>% select(Panel, Variable, Mean_NonAfr, Mean_Afr, Diff, p_value, N_NonAfr, N_Afr)))

write.csv(table1, file.path(tab_dir, "table1_summary_stats.csv"), row.names = FALSE)

table1_labels <- c(
  age = "Age (years)",
  height_cm = "Height (cm)",
  weight_kg = "Weight (kg)",
  married = "Married",
  farmer = "Farmer",
  prior_army = "Prior army",
  can_ride = "Can ride",
  can_shoot = "Can shoot",
  can_swim = "Can swim",
  speaks_dutch = "Speaks Dutch",
  wage = "Wage (shillings/day)",
  lwage = "Log wage",
  contract_years = "Contract (years)",
  class = "Class (1--3)",
  tenure_days = "Tenure (days)",
  dismissed_deserted = "Dismissed/deserted",
  voluntary_exit = "Voluntary exit",
  time_expired = "Time expired",
  good_character = "Good character",
  character_score = "Character score (1--7)"
)

table1_panels <- split(table1, table1$Panel)
table1_tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{@{}lcccccccc@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Non-Afrikaans} & \\multicolumn{2}{c}{Afrikaans} & & & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Mean & SD & Mean & SD & Diff. & Norm. diff. & $p$ \\\\",
  "\\midrule"
)

for (panel_name in names(table1_panels)) {
  table1_tex <- c(table1_tex, paste0("\\multicolumn{8}{@{}l}{\\textit{", panel_name, "}} \\\\[3pt]"))
  panel_rows <- table1_panels[[panel_name]]
  for (i in seq_len(nrow(panel_rows))) {
    row <- panel_rows[i, ]
    table1_tex <- c(
      table1_tex,
      paste0(
        table1_labels[[as.character(row$Variable)]], " & ",
        fmt_num(row$Mean_NonAfr, ifelse(abs(row$Mean_NonAfr) >= 10, 1, 2)), " & ",
        ifelse(is.na(row$SD_NonAfr), "", fmt_num(row$SD_NonAfr, ifelse(abs(row$SD_NonAfr) >= 10, 1, 2))), " & ",
        fmt_num(row$Mean_Afr, ifelse(abs(row$Mean_Afr) >= 10, 1, 2)), " & ",
        ifelse(is.na(row$SD_Afr), "", fmt_num(row$SD_Afr, ifelse(abs(row$SD_Afr) >= 10, 1, 2))), " & ",
        fmt_num(row$Diff, ifelse(abs(row$Diff) >= 10, 1, 2)), " & ",
        fmt_num(row$Norm_Diff, 2), " & ",
        ifelse(is.na(row$p_value), "",
               ifelse(row$p_value < 0.001, "$<$0.001", fmt_num(row$p_value, 3))),
        " \\\\"
      )
    )
  }
  table1_tex <- c(table1_tex, "[3pt]")
}

table1_tex <- c(
  table1_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: Normalised differences use the Imbens--Rubin convention: the mean difference divided by the square root of the average group variances. $N = 10{,}045$ non-Afrikaans; $N = 354$ Afrikaans in the reviewed baseline classification. Panel~B variables are set at enlistment; Panel~C variables are recorded at separation.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

table1_tex <- gsub("^\\[3pt\\]$", "", table1_tex)
writeLines(table1_tex, file.path(tab_dir, "table1_summary_stats.tex"))
cat("  Saved summary-statistics LaTeX table.\n")

# ── Class missingness diagnostic ───────────────────────────────────────
# Class is recorded only for the Trooper grading ladder (3/c, 2/c, 1/c, etc.).
# Recruits in NCO, sergeant, and certain specialist ranks have no class
# designation. This block tabulates missingness by rank and by Afrikaans
# status so that the manuscript can substantiate the institutional reason
# for the ~15% gap between the contract and class regression samples.

class_miss_by_rank <- sac %>%
  mutate(rank_lab = coalesce(title_clean, rank_cat, "Unrecorded")) %>%
  group_by(rank_lab) %>%
  summarise(
    n            = n(),
    n_class_na   = sum(is.na(class)),
    pct_class_na = round(100 * mean(is.na(class)), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

class_miss_by_rankcat <- sac %>%
  group_by(rank_cat) %>%
  summarise(
    n            = n(),
    n_class_na   = sum(is.na(class)),
    pct_class_na = round(100 * mean(is.na(class)), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

class_miss_by_afr <- sac %>%
  filter(!is.na(afrikaans)) %>%
  group_by(afrikaans) %>%
  summarise(
    n           = n(),
    n_class_na  = sum(is.na(class)),
    pct_class_na = round(100 * mean(is.na(class)), 1),
    .groups = "drop"
  )

cat("\nClass missingness by detailed title (top 12):\n")
print(as.data.frame(head(class_miss_by_rank, 12)))
cat("\nClass missingness by rank category:\n")
print(as.data.frame(class_miss_by_rankcat))
cat("\nClass missingness by Afrikaans status:\n")
print(as.data.frame(class_miss_by_afr))

write.csv(class_miss_by_rank,
          file.path(tab_dir, "class_missingness_by_rank.csv"),
          row.names = FALSE)
write.csv(class_miss_by_rankcat,
          file.path(tab_dir, "class_missingness_by_rankcat.csv"),
          row.names = FALSE)
write.csv(class_miss_by_afr,
          file.path(tab_dir, "class_missingness_by_afrikaans.csv"),
          row.names = FALSE)

# ── Table 2: 2x2 cell means (Afrikaans x Post) ─────────────────────────

cat("\n=== Table 2: 2x2 Cell Means ===\n")

did_vars <- c("wage", "age", "height_cm", "class", "contract_years")

table2 <- sac %>%
  filter(!is.na(afrikaans), !is.na(post_vereeniging)) %>%
  mutate(period = if_else(post_vereeniging == 1, "Post", "Pre"),
         group  = if_else(afrikaans == 1, "Afr", "NonAfr")) %>%
  select(period, group, all_of(did_vars)) %>%
  pivot_longer(-c(period, group), names_to = "Variable", values_to = "value") %>%
  group_by(Variable, group, period) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            N    = sum(!is.na(value)), .groups = "drop") %>%
  pivot_wider(names_from = c(group, period), values_from = c(Mean, N))

print(as.data.frame(table2))
cat("Note: Pre-treaty Afrikaans N =",
    sum(sac$afrikaans == 1 & sac$post_vereeniging == 0, na.rm = TRUE), "\n")

write.csv(table2, file.path(tab_dir, "table2_did_means.csv"), row.names = FALSE)

# ── Figure 2: Wage distributions ────────────────────────────────────────

cat("\n=== Figure 2: Wage Distributions ===\n")

wage_plot_data <- sac %>%
  filter(!is.na(wage), !is.na(afrikaans)) %>%
  mutate(group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans"))

fig2a <- ggplot(wage_plot_data, aes(x = wage, fill = group)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.25,
                 alpha = 0.6, position = "identity") +
  geom_density(aes(colour = group), linewidth = 0.8, fill = NA) +
  labs(x = "Wage (shillings/day)", y = "Density", fill = NULL, colour = NULL,
       title = "A. Full sample") +
  scale_fill_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

fig2b <- wage_plot_data %>%
  filter(title_clean == "3/c Trooper") %>%
  ggplot(aes(x = wage, fill = group)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.25,
                 alpha = 0.6, position = "identity") +
  geom_density(aes(colour = group), linewidth = 0.8, fill = NA) +
  labs(x = "Wage (shillings/day)", y = "Density", fill = NULL, colour = NULL,
       title = "B. 3/c Troopers only") +
  scale_fill_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

fig2 <- fig2a / fig2b
save_leap_fig(file.path(fig_dir, "fig2_wage_distributions.png"), fig2, width = 10, height = 8)
cat("  Saved Figure 2.\n")

# ── Figure 3: Composition and wage break at treaty ──────────────────────

cat("\n=== Figure 3: Composition & Wage Break ===\n")

monthly_comp <- sac %>%
  filter(!is.na(date_enlist), !is.na(afrikaans)) %>%
  mutate(enlist_month = floor_date(date_enlist, "month"),
         running = as.numeric(difftime(date_enlist, treaty_date, units = "days"))) %>%
  group_by(enlist_month) %>%
  summarise(afr_share = mean(afrikaans, na.rm = TRUE),
            n = n(), running = mean(running), .groups = "drop") %>%
  mutate(post = as.integer(enlist_month > treaty_date))

fig3a <- ggplot(monthly_comp, aes(x = enlist_month, y = afr_share)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(data = filter(monthly_comp, post == 0),
              method = "lm", se = TRUE, colour = col_non_afr) +
  geom_smooth(data = filter(monthly_comp, post == 1),
              method = "lm", se = TRUE, colour = col_non_afr) +
  geom_vline(xintercept = treaty_date, linetype = "dashed", colour = "grey40") +
  labs(x = NULL, y = "Afrikaans share", size = "N",
       title = "A. Afrikaans share of recruits") +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position = "none")

monthly_wage <- sac %>%
  filter(!is.na(date_enlist), !is.na(afrikaans), !is.na(wage)) %>%
  mutate(enlist_month = floor_date(date_enlist, "month"),
         group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans")) %>%
  group_by(enlist_month, group) %>%
  summarise(mean_wage = mean(wage, na.rm = TRUE), n = n(), .groups = "drop") %>%
  mutate(post = as.integer(enlist_month > treaty_date))

fig3b <- ggplot(monthly_wage, aes(x = enlist_month, y = mean_wage, colour = group)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(data = filter(monthly_wage, post == 0), method = "lm", se = TRUE) +
  geom_smooth(data = filter(monthly_wage, post == 1), method = "lm", se = TRUE) +
  geom_vline(xintercept = treaty_date, linetype = "dashed", colour = "grey40") +
  labs(x = "Enlistment month", y = "Mean wage (shillings/day)",
       colour = NULL, size = "N",
       title = "B. Mean wage by group") +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

fig3 <- fig3a / fig3b
save_leap_fig(file.path(fig_dir, "fig3_composition_break.png"), fig3, width = 10, height = 8)
cat("  Saved Figure 3.\n")

# ── McCrary density test (text + appendix figure) ───────────────────────

cat("\n=== McCrary Density Test ===\n")

if (has_rddensity) {
  density_data <- sac %>%
    filter(!is.na(date_enlist)) %>%
    mutate(running = as.numeric(difftime(date_enlist, treaty_date, units = "days")))

  mccrary <- rddensity(X = density_data$running, c = 0)
  cat("McCrary density test at treaty cutoff:\n")
  print(summary(mccrary))

  pdf(file.path(fig_dir, "figA1_mccrary.pdf"), width = 10, height = 6)
  rdplotdensity(mccrary, density_data$running,
                title = "",
                xlabel = "Days from Treaty (May 31, 1902)",
                ylabel = "Density")
  dev.off()

  png(file.path(fig_dir, "figA1_mccrary.png"), width = 10, height = 6, units = "in", res = 600)
  rdplotdensity(mccrary, density_data$running,
                title = "",
                xlabel = "Days from Treaty (May 31, 1902)",
                ylabel = "Density")
  dev.off()
  cat("  Saved Figure A1.\n")
} else {
  cat("  rddensity not installed; skipping. Install with: install.packages('rddensity')\n")
}


###########################################################################
#
#   SECTION 4 — THE WAGE GAP: ASSIGNMENT VS. WITHIN-RANK
#
###########################################################################

# Common regression data
reg_data <- sac %>%
  filter(!is.na(lwage), !is.na(afrikaans)) %>%
  mutate(
    class_f       = factor(class),
    contract_f    = factor(contract_years),
    region_f      = factor(region),
    title_f       = factor(title_clean),
    enlist_ym_f   = factor(enlist_ym),
    enlist_year_f = factor(enlist_year)
  )

# ── Figure 4: Coefficient stability ─────────────────────────────────────

cat("\n=== Figure 4: Coefficient Stability ===\n")

# Progressive addition of controls — NO interaction term throughout.
# The point is to show the overall Afrikaans gap shrinking with controls.
ctrl_specs <- list(
  "No controls" =
    lwage ~ afrikaans,
  "+ Demographics" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married,
  "+ Skills" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army,
  "+ Class, contract" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f),
  "+ Region" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f),
  "+ Year FE" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) + i(enlist_year_f),
  "+ Rank FE" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f,
  "+ Rank + Year-month FE" =
    lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f
)

ctrl_results <- map_dfr(seq_along(ctrl_specs), function(i) {
  m <- tryCatch(
    suppressWarnings(feols(ctrl_specs[[i]], data = reg_data, vcov = main_fixest_vcov, notes = FALSE)),
    error = function(e) NULL
  )
  if (!is.null(m) && "afrikaans" %in% names(coef(m))) {
    cf <- coeftable(m)
    tibble(
      spec     = names(ctrl_specs)[i],
      spec_num = i,
      estimate = cf["afrikaans", "Estimate"],
      se       = cf["afrikaans", "Std. Error"],
      ci_lo    = estimate - 1.96 * se,
      ci_hi    = estimate + 1.96 * se,
      n        = m$nobs
    )
  } else {
    tibble(spec = names(ctrl_specs)[i], spec_num = i,
           estimate = NA_real_, se = NA_real_,
           ci_lo = NA_real_, ci_hi = NA_real_, n = NA_integer_)
  }
})

cat("\nCoefficient stability:\n")
print(as.data.frame(ctrl_results))

fig4 <- ctrl_results %>%
  filter(!is.na(estimate)) %>%
  mutate(spec = fct_reorder(spec, spec_num),
         label = paste0("N = ", comma(n))) %>%
  ggplot(aes(x = spec, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.4) +
  geom_text(aes(label = label, y = ci_hi), hjust = -0.15, size = 2.5, colour = "grey50") +
  coord_flip() +
  labs(x = NULL, y = "Coefficient on Afrikaans") +
  theme(axis.text.y = element_text(size = 9))

save_leap_fig(file.path(fig_dir, "fig4_coefficient_stability.png"), fig4, width = 10, height = 6)
cat("  Saved Figure 4.\n")

# ── Table 3: Wage regressions (3 specs) ─────────────────────────────────

cat("\n=== Table 3: Wage Regressions ===\n")

spec1 <- suppressWarnings(feols(
  lwage ~ afrikaans + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) + i(region_f) + i(enlist_year_f),
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

spec2 <- suppressWarnings(feols(
  lwage ~ afrikaans + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

spec3 <- suppressWarnings(feols(
  lwage ~ afrikaans * post_vereeniging + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

wage_models <- list("(1)" = spec1, "(2)" = spec2, "(3)" = spec3)

saveRDS(wage_models, file.path(mod_dir, "wage_models.rds"))

wage_rows <- list(
  spec1_afr  = extract_fixest_dual(spec1, "afrikaans"),
  spec2_afr  = extract_fixest_dual(spec2, "afrikaans"),
  spec3_afr  = extract_fixest_dual(spec3, "afrikaans"),
  spec3_post = extract_fixest_dual(spec3, "post_vereeniging"),
  spec3_int  = extract_fixest_dual(spec3, "afrikaans:post_vereeniging")
)

wage_tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{@{}lccc@{}}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  "\\midrule",
  paste0("Afrikaans & ",
         fmt_num(wage_rows$spec1_afr['est'], 4), stars_from_p(wage_rows$spec1_afr['p_clust']), " & ",
         fmt_num(wage_rows$spec2_afr['est'], 4), stars_from_p(wage_rows$spec2_afr['p_clust']), " & ",
         fmt_num(wage_rows$spec3_afr['est'], 4), stars_from_p(wage_rows$spec3_afr['p_clust']), " \\\\"),
  paste0(" & [", fmt_num(wage_rows$spec1_afr['se_clust'], 4), "] & [",
         fmt_num(wage_rows$spec2_afr['se_clust'], 4), "] & [",
         fmt_num(wage_rows$spec3_afr['se_clust'], 4), "] \\\\"),
  paste0(" & (", fmt_num(wage_rows$spec1_afr['se_robust'], 4), ") & (",
         fmt_num(wage_rows$spec2_afr['se_robust'], 4), ") & (",
         fmt_num(wage_rows$spec3_afr['se_robust'], 4), ") \\\\[4pt]"),
  paste0("Post-Vereeniging &  &  & ",
         fmt_num(wage_rows$spec3_post['est'], 4), stars_from_p(wage_rows$spec3_post['p_clust']), " \\\\"),
  paste0(" &  &  & [", fmt_num(wage_rows$spec3_post['se_clust'], 4), "] \\\\"),
  paste0(" &  &  & (", fmt_num(wage_rows$spec3_post['se_robust'], 4), ") \\\\[4pt]"),
  paste0("Afrikaans $\\times$ Post &  &  & ",
         fmt_num(wage_rows$spec3_int['est'], 4), stars_from_p(wage_rows$spec3_int['p_clust']), " \\\\"),
  paste0(" &  &  & [", fmt_num(wage_rows$spec3_int['se_clust'], 4), "] \\\\"),
  paste0(" &  &  & (", fmt_num(wage_rows$spec3_int['se_robust'], 4), ") \\\\[4pt]"),
  "Demographics & Yes & Yes & Yes \\\\",
  "Assignment controls & Yes & Yes & Yes \\\\",
  "Region controls & Yes & Yes & Yes \\\\",
  "Title FE & No & Yes & Yes \\\\",
  "Enlistment month FE & No & Yes & Yes \\\\",
  paste0("Observations & ", as.integer(wage_rows$spec1_afr['n']), " & ",
         as.integer(wage_rows$spec2_afr['n']), " & ",
         as.integer(wage_rows$spec3_afr['n']), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: OLS. Dependent variable is log daily wage in shillings. Brackets report standard errors clustered by enlistment year-month; parentheses report heteroskedasticity-robust standard errors. Stars are based on clustered inference. Columns (2) and (3) include title and enlistment-month fixed effects. The post-treaty interaction is retained as a historical consistency check rather than a central identification strategy.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

writeLines(wage_tex, file.path(tab_dir, "table3_wage_regs.tex"))
cat("  Saved wage regression table with clustered and robust SEs.\n")

# ── EXTENSION 2: Oaxaca-Blinder Decomposition ────────────────────────────
# Threefold decomposition of the log-wage gap into endowments, coefficients,
# and interaction. Two versions: (a) with assignment variables, (b) without.

cat("\n=== Extension 2: Oaxaca-Blinder Decomposition ===\n")

if (has_oaxaca) {

  # Prepare data: binary/continuous only (no factors — avoids conformability)
  ob_data <- sac %>%
    filter(!is.na(lwage), !is.na(afrikaans), !is.na(age), !is.na(height_cm),
           !is.na(weight_kg), !is.na(married), !is.na(class), !is.na(contract_years)) %>%
    mutate(
      class_num     = as.numeric(class),
      contract_2    = as.integer(contract_years == 2),
      contract_3    = as.integer(contract_years == 3),
      rank_trooper  = as.integer(rank_cat == "Trooper"),
      rank_nco      = as.integer(rank_cat == "NCO"),
      farmer_num    = as.integer(farmer),
      prior_army_num = as.integer(prior_army),
      can_ride_num  = as.integer(can_ride),
      can_shoot_num = as.integer(can_shoot),
      group_ob      = ifelse(afrikaans == 1, "A", "B")  # oaxaca expects character
    ) %>%
    filter(!is.na(farmer_num), !is.na(prior_army_num))

  cat("  Oaxaca sample N =", nrow(ob_data), "\n")

  # Version (a): WITH assignment variables
  ob_with <- tryCatch(
    oaxaca(lwage ~ age + height_cm + weight_kg + married + farmer_num +
             prior_army_num + class_num + contract_2 + contract_3 +
             rank_trooper + rank_nco | group_ob,
           data = ob_data, R = 100),
    error = function(e) { cat("  Oaxaca (with assignment) failed:", e$message, "\n"); NULL }
  )

  # Version (b): WITHOUT assignment variables (demographics only)
  ob_without <- tryCatch(
    oaxaca(lwage ~ age + height_cm + weight_kg + married + farmer_num +
             prior_army_num | group_ob,
           data = ob_data, R = 100),
    error = function(e) { cat("  Oaxaca (without assignment) failed:", e$message, "\n"); NULL }
  )

  # Extract threefold overall decomposition
  if (!is.null(ob_with) && !is.null(ob_without)) {
    # Threefold results: row 1 = endowments, row 2 = coefficients, row 3 = interaction
    threefold_with    <- ob_with$threefold$overall
    threefold_without <- ob_without$threefold$overall

    total_gap <- threefold_with[1, 1] + threefold_with[2, 1] + threefold_with[3, 1]

    ob_summary <- tibble(
      model = c(rep("With assignment", 3), rep("Without assignment", 3)),
      component = rep(c("Endowments", "Coefficients", "Interaction"), 2),
      estimate = c(threefold_with[, 1], threefold_without[, 1]),
      se       = c(threefold_with[, 2], threefold_without[, 2])
    ) %>%
      mutate(
        pct_of_gap = estimate / total_gap * 100,
        ci_lo = estimate - 1.96 * se,
        ci_hi = estimate + 1.96 * se
      )

    cat("\nOaxaca-Blinder threefold decomposition:\n")
    cat("  Total log-wage gap:", round(total_gap, 4), "\n\n")
    print(as.data.frame(ob_summary %>%
            mutate(across(c(estimate, se, pct_of_gap), ~ round(.x, 4)))))

    write.csv(ob_summary, file.path(tab_dir, "ext2_oaxaca_decomposition.csv"),
              row.names = FALSE)

    # Variable-level endowment contributions (from version with assignment vars)
    var_detail <- ob_with$threefold$variables
    if (!is.null(var_detail)) {
      # var_detail has columns: endowments (coef, se), coefficients (coef, se), interaction (coef, se)
      var_endow <- data.frame(
        variable  = rownames(var_detail),
        endowment = var_detail[, "coef(endowments)"],
        se        = var_detail[, "se(endowments)"]
      ) %>%
        filter(variable != "(Intercept)") %>%
        mutate(
          ci_lo = endowment - 1.96 * se,
          ci_hi = endowment + 1.96 * se,
          significant = sign(ci_lo) == sign(ci_hi),
          colour = ifelse(significant, "#5C2346", LEAP_NONSIG_COLOR),
          variable = recode(variable,
            class_num = "Class", contract_2 = "2-year contract",
            contract_3 = "3-year contract", rank_trooper = "Trooper rank",
            rank_nco = "NCO rank", farmer_num = "Farmer",
            prior_army_num = "Prior army", can_ride_num = "Can ride",
            can_shoot_num = "Can shoot", height_cm = "Height",
            weight_kg = "Weight", married = "Married", age = "Age")
        )

      fig_ext2 <- ggplot(var_endow, aes(x = endowment, y = reorder(variable, endowment))) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
        geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi, colour = colour), size = 0.4) +
        scale_colour_identity() +
        labs(x = "Endowment contribution to log-wage gap",
             y = NULL) +
        theme(axis.text.y = element_text(size = 9))

      save_leap_fig(file.path(fig_dir, "fig_ext2_oaxaca_detailed.png"),
                    fig_ext2, width = 10, height = 6)
      cat("  Saved Figure ext2 (Oaxaca variable-level).\n")
    }

    saveRDS(list(with_assignment = ob_with, without_assignment = ob_without),
            file.path(mod_dir, "oaxaca_models.rds"))
  }

} else {
  cat("  oaxaca package not installed; skipping. Install with: install.packages('oaxaca')\n")
}

# ── Placebo ethnic groups (text results) ─────────────────────────────────

cat("\n=== Placebo Ethnic Groups (for text) ===\n")

reg_data <- reg_data %>%
  mutate(
    scottish = as.integer(region == "Scotland"),
    irish    = as.integer(region == "Ireland"),
    canadian = as.integer(country == "Canada")
  )

placebo_scottish <- suppressWarnings(feols(
  lwage ~ scottish * post_vereeniging + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) | title_f + enlist_ym_f,
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

placebo_irish <- suppressWarnings(feols(
  lwage ~ irish * post_vereeniging + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) | title_f + enlist_ym_f,
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

placebo_canadian <- suppressWarnings(feols(
  lwage ~ canadian * post_vereeniging + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) | title_f + enlist_ym_f,
  data = reg_data, vcov = main_fixest_vcov, notes = FALSE
))

cat("\nPlacebo interactions (Group x Post-Vereeniging):\n")
cat("  Scottish x Post:",
    round(coef(placebo_scottish)["scottish:post_vereeniging"], 5),
    " (p =", round(coeftable(placebo_scottish)["scottish:post_vereeniging", "Pr(>|t|)"], 4), ")\n")
cat("  Irish x Post:",
    round(coef(placebo_irish)["irish:post_vereeniging"], 5),
    " (p =", round(coeftable(placebo_irish)["irish:post_vereeniging", "Pr(>|t|)"], 4), ")\n")
cat("  Canadian x Post:",
    round(coef(placebo_canadian)["canadian:post_vereeniging"], 5),
    " (p =", round(coeftable(placebo_canadian)["canadian:post_vereeniging", "Pr(>|t|)"], 4), ")\n")

# ── Permutation inference (text result) ──────────────────────────────────

cat("\n=== Permutation Inference ===\n")

perm_data <- reg_data %>%
  filter(!is.na(age), !is.na(height_cm), !is.na(weight_kg),
         !is.na(married), !is.na(post_vereeniging))

actual_model <- suppressWarnings(feols(
  lwage ~ afrikaans * post_vereeniging + age + height_cm + weight_kg + married +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = perm_data, vcov = main_fixest_vcov, notes = FALSE
))
actual_coef <- coef(actual_model)["afrikaans:post_vereeniging"]

set.seed(42)
n_perm     <- 1000
perm_coefs <- numeric(n_perm)

cat("Running", n_perm, "permutations...\n")
for (i in seq_len(n_perm)) {
  perm_data$afr_perm <- sample(perm_data$afrikaans)

  m <- tryCatch(
    suppressWarnings(feols(
      lwage ~ afr_perm * post_vereeniging + age + height_cm + weight_kg + married +
        i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
      data = perm_data, vcov = main_fixest_vcov, notes = FALSE
    )),
    error = function(e) NULL
  )

  perm_coefs[i] <- if (!is.null(m) && "afr_perm:post_vereeniging" %in% names(coef(m))) {
    coef(m)["afr_perm:post_vereeniging"]
  } else {
    NA_real_
  }

  if (i %% 200 == 0) cat("  Permutation", i, "of", n_perm, "\n")
}

perm_p <- mean(abs(perm_coefs) >= abs(actual_coef), na.rm = TRUE)
cat("  Actual interaction coefficient:", round(actual_coef, 4), "\n")
cat("  Permutation p-value:", round(perm_p, 4), "\n")


###########################################################################
#
#   SECTION 4B — THE SORTING MECHANISM: OVER-SORTING AND RANK ASSIGNMENT
#
###########################################################################

# ── Table 5: Rank assignment regressions (LPM) ──────────────────────────

cat("\n=== Table 5: Rank Assignment Regressions ===\n")

# Use only pre-determined characteristics (not assignment variables like
# contract_years or class, which are themselves outcomes of sorting)
rank_data <- sac %>%
  filter(!is.na(afrikaans), !is.na(is_trooper_3c)) %>%
  mutate(
    enlist_year_f = factor(enlist_year),
    region_f      = factor(region)
  )

# LPM: Pr(assigned to lowest rank = 3/c Trooper)
rank_spec1 <- suppressWarnings(feols(
  is_trooper_3c ~ afrikaans,
  data = rank_data, vcov = main_fixest_vcov, notes = FALSE
))

rank_spec2 <- suppressWarnings(feols(
  is_trooper_3c ~ afrikaans + age + height_cm + weight_kg + married,
  data = rank_data, vcov = main_fixest_vcov, notes = FALSE
))

rank_spec3 <- suppressWarnings(feols(
  is_trooper_3c ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army + farmer,
  data = rank_data, vcov = main_fixest_vcov, notes = FALSE
))

rank_spec4 <- suppressWarnings(feols(
  is_trooper_3c ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army + farmer | enlist_year_f,
  data = rank_data, vcov = main_fixest_vcov, notes = FALSE
))

cat("\nRank assignment (LPM: Pr(3/c Trooper)):\n")
etable(rank_spec1, rank_spec2, rank_spec3, rank_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2)

etable(rank_spec1, rank_spec2, rank_spec3, rank_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2,
       tex = TRUE, file = file.path(tab_dir, "table5_rank_assignment.tex"),
       replace = TRUE,
       title = "Rank assignment: Probability of lowest rank (3/c Trooper)")

rank_models <- list("(1)" = rank_spec1, "(2)" = rank_spec2,
                     "(3)" = rank_spec3, "(4)" = rank_spec4)
saveRDS(rank_models, file.path(mod_dir, "rank_assignment_models.rds"))

# ── Figure 7: Over-sorting — Predicted vs. actual rank assignment ────────

cat("\n=== Figure 7: Over-Sorting Measure ===\n")

# Prediction sample: individuals with complete pre-determined characteristics.
# This sample is required for the logit predictions but is differentially
# missing on Afrikaners (especially weight); we therefore compute the
# *actual* 3/c-Trooper rate on the full sample for transparency, while
# the *predicted* rate is necessarily computed on the complete-cases
# sample. The figure displays the full-sample actual rate alongside the
# complete-cases predicted rate; a diagnostic below prints both
# subsamples so any divergence is visible.

pred_data <- sac %>%
  filter(!is.na(is_trooper_3c), !is.na(age), !is.na(height_cm),
         !is.na(weight_kg), !is.na(married), !is.na(afrikaans),
         !is.na(can_ride), !is.na(can_shoot), !is.na(can_swim),
         !is.na(prior_army), !is.na(farmer))

non_afr_pred <- pred_data %>% filter(afrikaans == 0)
afr_pred     <- pred_data %>% filter(afrikaans == 1)

# Full-sample actual rates (for the figure and for the §6.3 cross-reference)
non_afr_full <- sac %>% filter(afrikaans == 0, !is.na(is_trooper_3c))
afr_full     <- sac %>% filter(afrikaans == 1, !is.na(is_trooper_3c))

actual_non_full <- mean(non_afr_full$is_trooper_3c)
actual_afr_full <- mean(afr_full$is_trooper_3c)
actual_non_cc   <- mean(non_afr_pred$is_trooper_3c)
actual_afr_cc   <- mean(afr_pred$is_trooper_3c)

# Train logit on non-Afrikaners: predict rank from pre-determined observables
rank_pred_model <- glm(
  is_trooper_3c ~ age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army + farmer,
  data = non_afr_pred,
  family = binomial
)

# Predict for both groups (necessarily on the complete-cases sample)
non_afr_pred$predicted_3c <- predict(rank_pred_model, newdata = non_afr_pred,
                                      type = "response")
afr_pred$predicted_3c <- predict(rank_pred_model, newdata = afr_pred,
                                  type = "response")

predicted_non_cc <- mean(non_afr_pred$predicted_3c)
predicted_afr_cc <- mean(afr_pred$predicted_3c)

cat("\nOver-sorting diagnostic (full vs. complete-cases sample):\n")
cat("  Non-Afr full-sample N =", nrow(non_afr_full),
    "; complete-cases N =", nrow(non_afr_pred), "\n")
cat("  Afr     full-sample N =", nrow(afr_full),
    "; complete-cases N =", nrow(afr_pred), "\n")
cat("  Non-Afr 3/c rate  full =", round(actual_non_full, 3),
    "; complete-cases =", round(actual_non_cc, 3), "\n")
cat("  Afr     3/c rate  full =", round(actual_afr_full, 3),
    "; complete-cases =", round(actual_afr_cc, 3), "\n")
cat("  Predicted 3/c (complete-cases) — Non-Afr =",
    round(predicted_non_cc, 3),
    "; Afr =", round(predicted_afr_cc, 3), "\n")
cat("  Over-sorting gap (full Afr actual - complete-cases predicted):",
    round(actual_afr_full - predicted_afr_cc, 3), "pp\n")

# Bootstrap confidence interval for the over-sorting gap
# (Afrikaner actual rate, full sample, minus model-implied predicted rate
# from the logit trained on non-Afrikaner complete cases.)
set.seed(123)
n_boot <- 500
boot_gaps <- numeric(n_boot)

for (b in seq_len(n_boot)) {
  # Resample non-Afrikaners (training set)
  boot_non <- non_afr_pred[sample(nrow(non_afr_pred), replace = TRUE), ]
  boot_mod <- tryCatch(
    glm(is_trooper_3c ~ age + height_cm + weight_kg + married +
          can_ride + can_shoot + can_swim + prior_army + farmer,
        data = boot_non, family = binomial),
    error = function(e) NULL
  )
  if (is.null(boot_mod)) { boot_gaps[b] <- NA; next }

  # Predict for Afrikaners (complete-cases) and resample full Afrikaner
  # actual rates with replacement to propagate full-sample uncertainty.
  boot_pred <- predict(boot_mod, newdata = afr_pred, type = "response")
  boot_actual <- mean(sample(afr_full$is_trooper_3c,
                             nrow(afr_full), replace = TRUE))
  boot_gaps[b] <- boot_actual - mean(boot_pred)
}

oversort_ci <- quantile(boot_gaps, c(0.025, 0.975), na.rm = TRUE)
cat("  95% bootstrap CI for over-sorting gap: [",
    round(oversort_ci[1], 3), ",", round(oversort_ci[2], 3), "]\n")

# Figure 7 — actual rates use the full sample; predicted rates use the
# complete-cases prediction sample, so the figure does not understate the
# unconditional Afrikaner 3/c-Trooper rate.
oversort_df <- tibble(
  Group = rep(c("Non-Afrikaans", "Afrikaans"), each = 2),
  Measure = rep(c("Predicted\n(based on observables, complete cases)",
                  "Actual\n(full sample)"), 2),
  Rate = c(
    predicted_non_cc,
    actual_non_full,
    predicted_afr_cc,
    actual_afr_full
  )
) %>%
  mutate(Group   = factor(Group, levels = c("Non-Afrikaans", "Afrikaans")),
         Measure = factor(Measure, levels = c(
           "Predicted\n(based on observables, complete cases)",
           "Actual\n(full sample)")))

fig7 <- ggplot(oversort_df, aes(x = Group, y = Rate, fill = Measure)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(Rate * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("#3D8EB9", "#5C2346")) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Probability of 3/c Trooper assignment",
       fill = NULL) +
  theme(legend.position = "bottom")

save_leap_fig(file.path(fig_dir, "fig7_oversorting.png"), fig7, width = 10, height = 6)
cat("  Saved Figure 7.\n")

# ── Table 6: Class assignment regressions ─────────────────────────────────
# Class (1=lowest, 3=highest) is a key sorting channel alongside contract

cat("\n=== Table 6: Class Assignment Regressions ===\n")

class_data <- sac %>%
  filter(!is.na(afrikaans), !is.na(class)) %>%
  mutate(enlist_year_f = factor(enlist_year))

# Note: skills (can_ride, can_shoot) excluded — only ~23 Afrikaner obs
class_spec1 <- suppressWarnings(feols(
  class ~ afrikaans,
  data = class_data, vcov = main_fixest_vcov, notes = FALSE
))

class_spec2 <- suppressWarnings(feols(
  class ~ afrikaans + age + height_cm + weight_kg + married,
  data = class_data, vcov = main_fixest_vcov, notes = FALSE
))

class_spec3 <- suppressWarnings(feols(
  class ~ afrikaans + age + height_cm + weight_kg + married + farmer + prior_army,
  data = class_data, vcov = main_fixest_vcov, notes = FALSE
))

class_spec4 <- suppressWarnings(feols(
  class ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = class_data, vcov = main_fixest_vcov, notes = FALSE
))

cat("\nClass assignment (OLS: Class 1-3, higher = better):\n")
etable(class_spec1, class_spec2, class_spec3, class_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2)

etable(class_spec1, class_spec2, class_spec3, class_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2,
       tex = TRUE, file = file.path(tab_dir, "table6_class_assignment.tex"),
       replace = TRUE,
       title = "Class assignment: OLS with class (1-3) as dependent variable")

# ── Table 7: Contract length regressions ─────────────────────────────────

cat("\n=== Table 7: Contract Length Regressions ===\n")

contract_data <- sac %>%
  filter(!is.na(afrikaans), !is.na(contract_years)) %>%
  mutate(enlist_year_f = factor(enlist_year))

contract_spec1 <- suppressWarnings(feols(
  contract_years ~ afrikaans,
  data = contract_data, vcov = main_fixest_vcov, notes = FALSE
))

contract_spec2 <- suppressWarnings(feols(
  contract_years ~ afrikaans + age + height_cm + weight_kg + married,
  data = contract_data, vcov = main_fixest_vcov, notes = FALSE
))

contract_spec3 <- suppressWarnings(feols(
  contract_years ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army,
  data = contract_data, vcov = main_fixest_vcov, notes = FALSE
))

contract_spec4 <- suppressWarnings(feols(
  contract_years ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = contract_data, vcov = main_fixest_vcov, notes = FALSE
))

cat("\nContract length (OLS: years):\n")
etable(contract_spec1, contract_spec2, contract_spec3, contract_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2)

etable(contract_spec1, contract_spec2, contract_spec3, contract_spec4,
       keep = c("%afrikaans"),
       se.below = TRUE, fitstat = ~ n + r2,
       tex = TRUE, file = file.path(tab_dir, "table7_contract_assignment.tex"),
       replace = TRUE,
       title = "Contract assignment: OLS with contract years as dependent variable")

assign_rows <- list(
  rank_raw       = extract_fixest_dual(rank_spec1),
  rank_pref      = extract_fixest_dual(rank_spec4),
  class_raw      = extract_fixest_dual(class_spec1),
  class_pref     = extract_fixest_dual(class_spec4),
  contract_raw   = extract_fixest_dual(contract_spec1),
  contract_pref  = extract_fixest_dual(contract_spec4)
)

assign_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Assignment regressions: Afrikaans coefficient}",
  "\\label{tab:assignment}",
  "\\small",
  "\\begin{threeparttable}",
  "\\begin{tabular}{@{}lcccccc@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Pr(3/c Trooper)} & \\multicolumn{2}{c}{Class (1--3)} & \\multicolumn{2}{c}{Contract (years)} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  paste0(
    "Afrikaans & ",
    fmt_num(assign_rows$rank_raw["est"]), stars_from_p(assign_rows$rank_raw["p_clust"]), " & ",
    fmt_num(assign_rows$rank_pref["est"]), stars_from_p(assign_rows$rank_pref["p_clust"]), " & ",
    fmt_num(assign_rows$class_raw["est"]), stars_from_p(assign_rows$class_raw["p_clust"]), " & ",
    fmt_num(assign_rows$class_pref["est"]), stars_from_p(assign_rows$class_pref["p_clust"]), " & ",
    fmt_num(assign_rows$contract_raw["est"]), stars_from_p(assign_rows$contract_raw["p_clust"]), " & ",
    fmt_num(assign_rows$contract_pref["est"]), stars_from_p(assign_rows$contract_pref["p_clust"]), " \\\\"
  ),
  paste0(
    " & [", fmt_num(assign_rows$rank_raw["se_clust"]), "] & [", fmt_num(assign_rows$rank_pref["se_clust"]),
    "] & [", fmt_num(assign_rows$class_raw["se_clust"]), "] & [", fmt_num(assign_rows$class_pref["se_clust"]),
    "] & [", fmt_num(assign_rows$contract_raw["se_clust"]), "] & [", fmt_num(assign_rows$contract_pref["se_clust"]), "] \\\\"
  ),
  paste0(
    " & (", fmt_num(assign_rows$rank_raw["se_robust"]), ") & (", fmt_num(assign_rows$rank_pref["se_robust"]),
    ") & (", fmt_num(assign_rows$class_raw["se_robust"]), ") & (", fmt_num(assign_rows$class_pref["se_robust"]),
    ") & (", fmt_num(assign_rows$contract_raw["se_robust"]), ") & (", fmt_num(assign_rows$contract_pref["se_robust"]), ") \\\\[6pt]"
  ),
  "Demographics & & Yes & & Yes & & Yes \\\\",
  "Background & & Yes & & Yes & & Yes \\\\",
  "Year FE & & Yes & & Yes & & Yes \\\\[3pt]",
  paste0(
    "Observations & ", as.integer(assign_rows$rank_raw["n"]), " & ", as.integer(assign_rows$rank_pref["n"]),
    " & ", as.integer(assign_rows$class_raw["n"]), " & ", as.integer(assign_rows$class_pref["n"]),
    " & ", as.integer(assign_rows$contract_raw["n"]), " & ", as.integer(assign_rows$contract_pref["n"]), " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: OLS. Columns (1), (3), and (5) report raw differences. Columns (2), (4), and (6) add age, height, weight, marital status, farmer, prior army, and enlistment-year fixed effects; the rank specification also adds riding, shooting, and swimming ability. Brackets report standard errors clustered by enlistment year-month; parentheses report heteroskedasticity-robust standard errors. Stars are based on clustered inference.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(assign_tex, file.path(tab_dir, "table_assignment_combined.tex"))
cat("  Saved combined assignment table.\n")

oster_assignment <- tibble(
  margin = c("Class", "Contract"),
  beta_tilde = c(unname(coef(class_spec1)["afrikaans"]), unname(coef(contract_spec1)["afrikaans"])),
  beta_hat   = c(unname(coef(class_spec4)["afrikaans"]), unname(coef(contract_spec4)["afrikaans"])),
  r_tilde    = c(fitstat(class_spec1, "r2")[[1]], fitstat(contract_spec1, "r2")[[1]]),
  r_hat      = c(fitstat(class_spec4, "r2")[[1]], fitstat(contract_spec4, "r2")[[1]])
) %>%
  mutate(
    r_max = pmin(1, 1.3 * r_hat),
    delta_zero = pmap_dbl(list(beta_tilde, beta_hat, r_tilde, r_hat, r_max), \(a, b, c, d, e) oster_delta_zero(a, b, c, d, e))
  )

write.csv(oster_assignment, file.path(tab_dir, "oster_assignment.csv"), row.names = FALSE)
cat("  Saved Oster sensitivity summary for assignment results.\n")

# ── Figure 9: Assignment channels — coefficient stability for class,
#    contract, and rank simultaneously ──────────────────────────────────────

cat("\n=== Figure 9: Assignment Channels ===\n")

# Extract Afrikaans coefficient from each set of regressions
assignment_results <- bind_rows(
  # Class specs
  map_dfr(list(class_spec1, class_spec2, class_spec3, class_spec4), function(m) {
    cf <- coeftable(m)
    tibble(outcome = "Class (1-3)",
           estimate = cf["afrikaans", "Estimate"],
           se = cf["afrikaans", "Std. Error"],
           n = m$nobs)
  }) %>% mutate(spec = c("No controls", "+ Demographics",
                          "+ Farmer, army", "+ Year FE")),
  # Contract specs
  map_dfr(list(contract_spec1, contract_spec2, contract_spec3, contract_spec4), function(m) {
    cf <- coeftable(m)
    tibble(outcome = "Contract (years)",
           estimate = cf["afrikaans", "Estimate"],
           se = cf["afrikaans", "Std. Error"],
           n = m$nobs)
  }) %>% mutate(spec = c("No controls", "+ Demographics",
                          "+ Farmer, army", "+ Year FE")),
  # Rank specs (from Table 5)
  map_dfr(list(rank_spec1, rank_spec2, rank_spec3, rank_spec4), function(m) {
    cf <- coeftable(m)
    tibble(outcome = "Pr(3/c Trooper)",
           estimate = cf["afrikaans", "Estimate"],
           se = cf["afrikaans", "Std. Error"],
           n = m$nobs)
  }) %>% mutate(spec = c("No controls", "+ Demographics",
                          "+ Skills, farmer", "+ Year FE"))
) %>%
  mutate(ci_lo = estimate - 1.96 * se,
         ci_hi = estimate + 1.96 * se,
         spec  = factor(spec, levels = rev(c("No controls", "+ Demographics",
                                              "+ Skills, farmer", "+ Farmer, army",
                                              "+ Year FE"))))

fig9 <- ggplot(assignment_results, aes(x = estimate, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.4) +
  facet_wrap(~ outcome, scales = "free_x") +
  labs(x = "Coefficient on Afrikaans", y = NULL) +
  theme(axis.text.y = element_text(size = 9),
        strip.text = element_text(face = "bold"))

save_leap_fig(file.path(fig_dir, "fig9_assignment_channels.png"), fig9, width = 12, height = 6)
cat("  Saved Figure 9.\n")


###########################################################################
#
#   SECTION 5 — ORGANISATIONAL COSTS
#
###########################################################################

# ── Common survival data ─────────────────────────────────────────────────

surv_data <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans)) %>%
  mutate(
    group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans"),
    group4 = interaction(
      if_else(afrikaans == 1, "Afr", "Non-Afr"),
      if_else(post_vereeniging == 1, "Post", "Pre"),
      sep = " / "
    ),
    event = as.integer(discharged == 1)
  )

# ── Figure 5a: KM Overall ───────────────────────────────────────────────

cat("\n=== Figure 5: Kaplan-Meier Survival ===\n")

km_fit1 <- survfit(Surv(tenure_days, event) ~ group, data = surv_data)

fig5a <- ggsurvfit(km_fit1) +
  labs(x = "Days since enlistment", y = "Survival probability",
       colour = NULL) +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  add_risktable()

save_leap_fig(file.path(fig_dir, "fig5a_km_overall.png"), fig5a, width = 10, height = 6)

# ── Figure 5b: KM 4-group ───────────────────────────────────────────────

km_fit2 <- survfit(Surv(tenure_days, event) ~ group4, data = surv_data)

fig5b <- ggsurvfit(km_fit2) +
  labs(x = "Days since enlistment", y = "Survival probability") +
  add_risktable()

save_leap_fig(file.path(fig_dir, "fig5b_km_4groups.png"), fig5b, width = 10, height = 7)

# Log-rank test
lr_test <- survdiff(Surv(tenure_days, event) ~ group, data = surv_data)
cat("\nLog-rank test:\n")
print(lr_test)
cat("  Saved Figure 5.\n")

# ── Figure 6: Cumulative incidence functions ─────────────────────────────

cat("\n=== Figure 6: Cumulative Incidence ===\n")

cif_data <- surv_data %>% filter(!is.na(event_type))
cif_fit  <- cuminc(cif_data$tenure_days, cif_data$event_type, group = cif_data$afrikaans)

# Extract CIF data for ggplot
event_labels <- c("1" = "Dismissed/Deserted", "2" = "Voluntary exit", "3" = "Time expired")
group_labels <- c("0" = "Non-Afrikaans", "1" = "Afrikaans")

cif_names <- names(cif_fit)[!names(cif_fit) %in% "Tests"]
cif_df <- map_dfr(cif_names, function(nm) {
  parts <- strsplit(nm, " ")[[1]]
  tibble(
    time  = cif_fit[[nm]]$time,
    est   = cif_fit[[nm]]$est,
    group = group_labels[parts[1]],
    event = event_labels[parts[2]]
  )
}) %>%
  filter(!is.na(event))

fig6 <- ggplot(cif_df, aes(x = time, y = est, colour = group, linetype = event)) +
  geom_step(linewidth = 0.7) +
  labs(x = "Days since enlistment", y = "Cumulative incidence",
       colour = NULL, linetype = "Separation type") +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  scale_linetype_manual(values = c("Dismissed/Deserted" = "solid",
                                    "Voluntary exit" = "dashed",
                                    "Time expired" = "dotted")) +
  theme(legend.box = "vertical")

save_leap_fig(file.path(fig_dir, "fig6_cif.png"), fig6, width = 10, height = 6)

# Gray's test
cat("\nGray's test for CIF differences:\n")
print(cif_fit$Tests)
cat("  Saved Figure 6.\n")

# ── Table 4: Competing-risks regression (Fine-Gray) ─────────────────────

cat("\n=== Table 4: Competing-Risks Regression ===\n")

cr_data <- surv_data %>%
  filter(!is.na(lwage), !is.na(age), !is.na(height_cm),
         !is.na(contract_years), !is.na(married))

# Covariate matrix — impute medians for occasional missing values
covs <- model.matrix(
  ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + contract_years,
  data = cr_data %>%
    mutate(across(c(weight_kg, can_ride, can_shoot),
                  ~ replace_na(.x, median(.x, na.rm = TRUE))))
)[, -1]

# Three event types
crr_dismiss <- crr(ftime = cr_data$tenure_days, fstatus = cr_data$event_type,
                   cov1 = covs, failcode = 1, cencode = 0)

crr_voluntary <- crr(ftime = cr_data$tenure_days, fstatus = cr_data$event_type,
                     cov1 = covs, failcode = 2, cencode = 0)

crr_time <- crr(ftime = cr_data$tenure_days, fstatus = cr_data$event_type,
                cov1 = covs, failcode = 3, cencode = 0)

# Combine results
table4 <- data.frame(
  Variable        = colnames(covs),
  Dismiss_coef    = round(crr_dismiss$coef, 4),
  Dismiss_se      = round(sqrt(diag(crr_dismiss$var)), 4),
  Dismiss_HR      = round(exp(crr_dismiss$coef), 3),
  Voluntary_coef  = round(crr_voluntary$coef, 4),
  Voluntary_se    = round(sqrt(diag(crr_voluntary$var)), 4),
  Voluntary_HR    = round(exp(crr_voluntary$coef), 3),
  TimeExp_coef    = round(crr_time$coef, 4),
  TimeExp_se      = round(sqrt(diag(crr_time$var)), 4),
  TimeExp_HR      = round(exp(crr_time$coef), 3)
)

cat("\nTable 4: Competing-Risks Regression (Fine-Gray)\n")
print(table4)

write.csv(table4, file.path(tab_dir, "table4_competing_risks.csv"), row.names = FALSE)
saveRDS(list(dismiss = crr_dismiss, voluntary = crr_voluntary, time = crr_time),
        file.path(mod_dir, "crr_models.rds"))

fg_keep <- c("lwage", "afrikaans", "contract_years")
fg_labels <- c(lwage = "Log wage", afrikaans = "Afrikaans", contract_years = "Contract yrs")
fg_extract <- function(var) {
  se_dismiss <- sqrt(diag(crr_dismiss$var))
  se_voluntary <- sqrt(diag(crr_voluntary$var))
  se_time <- sqrt(diag(crr_time$var))
  names(se_dismiss) <- names(crr_dismiss$coef)
  names(se_voluntary) <- names(crr_voluntary$coef)
  names(se_time) <- names(crr_time$coef)
  tibble(
    Variable = fg_labels[[var]],
    Dismiss_coef = crr_dismiss$coef[var],
    Dismiss_se   = se_dismiss[var],
    Dismiss_HR   = exp(crr_dismiss$coef[var]),
    Voluntary_coef = crr_voluntary$coef[var],
    Voluntary_se   = se_voluntary[var],
    Voluntary_HR   = exp(crr_voluntary$coef[var]),
    TimeExp_coef = crr_time$coef[var],
    TimeExp_se   = se_time[var],
    TimeExp_HR   = exp(crr_time$coef[var])
  )
}
fg_display <- bind_rows(lapply(fg_keep, fg_extract))

fg_tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{@{}lcccccc@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Dismissed} & \\multicolumn{2}{c}{Voluntary} & \\multicolumn{2}{c}{Time expired} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " & Coef. & SHR & Coef. & SHR & Coef. & SHR \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(fg_display))) {
  r <- fg_display[i, ]
  fg_tex <- c(
    fg_tex,
    paste0(r$Variable, " & ",
           fmt_num(r$Dismiss_coef, 3), " & ", fmt_num(r$Dismiss_HR, 2), " & ",
           fmt_num(r$Voluntary_coef, 3), " & ", fmt_num(r$Voluntary_HR, 2), " & ",
           fmt_num(r$TimeExp_coef, 3), " & ", fmt_num(r$TimeExp_HR, 2), " \\\\"),
    paste0(" & (", fmt_num(r$Dismiss_se, 3), ") & & (", fmt_num(r$Voluntary_se, 3),
           ") & & (", fmt_num(r$TimeExp_se, 3), ") & \\\\")
  )
}
fg_tex <- c(
  fg_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: Fine--Gray sub-distribution hazard models. SHR = sub-distribution hazard ratio. Standard errors in parentheses. Additional controls (age, height, weight, marital status, riding, shooting) included but not shown.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)
writeLines(fg_tex, file.path(tab_dir, "tableA2_competing_risks.tex"))
cat("  Saved Fine--Gray appendix table LaTeX.\n")

# ── Character ratings — ordered probit (text only) ───────────────────────

cat("\n=== Character Ratings: Ordered Probit (for text) ===\n")

op_data <- sac %>%
  filter(!is.na(character_score), !is.na(lwage), !is.na(afrikaans),
         !is.na(age), !is.na(height_cm), !is.na(married)) %>%
  mutate(character_ordered = factor(character_score, ordered = TRUE))

op_model <- polr(
  character_ordered ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + factor(contract_years) + factor(rank_cat),
  data = op_data, method = "probit", Hess = TRUE
)

op_summ <- coef(summary(op_model))
op_coef_names <- names(coef(op_model))
op_key <- op_summ[c("lwage", "afrikaans"), ]

cat("\nOrdered Probit — key coefficients:\n")
cat("  lwage:     coef =", round(op_key["lwage", "Value"], 3),
    " (t =", round(op_key["lwage", "t value"], 2), ")\n")
cat("  afrikaans: coef =", round(op_key["afrikaans", "Value"], 3),
    " (t =", round(op_key["afrikaans", "t value"], 2), ")\n")

# ── Back-of-the-envelope cost calculation (text only) ────────────────────

cat("\n=== Back-of-Envelope Cost Calculation ===\n")

cost_data <- sac %>%
  filter(!is.na(tenure_days), !is.na(afrikaans), !is.na(post_vereeniging))

post_afr <- cost_data %>% filter(afrikaans == 1, post_vereeniging == 1)
post_non <- cost_data %>% filter(afrikaans == 0, post_vereeniging == 1)
post_afr_trooper <- cost_data %>% filter(afrikaans == 1, post_vereeniging == 1, title_clean == "3/c Trooper")
post_non_trooper <- cost_data %>% filter(afrikaans == 0, post_vereeniging == 1, title_clean == "3/c Trooper")

n_post_afr      <- nrow(post_afr)
mean_tenure_afr <- mean(post_afr$tenure_days, na.rm = TRUE)
mean_tenure_non <- mean(post_non$tenure_days, na.rm = TRUE)
tenure_shortfall <- mean_tenure_non - mean_tenure_afr
mean_wage_afr   <- mean(post_afr$wage, na.rm = TRUE)
total_lost_days <- n_post_afr * tenure_shortfall
total_cost      <- total_lost_days * mean_wage_afr
trooper_gap     <- mean(post_non_trooper$tenure_days, na.rm = TRUE) - mean(post_afr_trooper$tenure_days, na.rm = TRUE)
conservative_lost_days <- n_post_afr * trooper_gap
conservative_cost      <- conservative_lost_days * mean_wage_afr

n_cost_boot <- 1000
cost_boot <- replicate(n_cost_boot, {
  boot_afr <- sample(post_afr_trooper$tenure_days, size = nrow(post_afr_trooper), replace = TRUE)
  boot_non <- sample(post_non_trooper$tenure_days, size = nrow(post_non_trooper), replace = TRUE)
  n_post_afr * (mean(boot_non, na.rm = TRUE) - mean(boot_afr, na.rm = TRUE))
})
cost_ci <- quantile(cost_boot, c(0.025, 0.975), na.rm = TRUE)

cat("  Post-treaty Afrikaans recruits (N):", n_post_afr, "\n")
cat("  Mean tenure — Afrikaans:", round(mean_tenure_afr, 0), "days\n")
cat("  Mean tenure — Non-Afrikaans:", round(mean_tenure_non, 0), "days\n")
cat("  Tenure shortfall:", round(tenure_shortfall, 0), "days per recruit\n")
cat("  3/c Trooper tenure shortfall:", round(trooper_gap, 0), "days per recruit\n")
cat("  Mean Afrikaans wage:", round(mean_wage_afr, 2), "shillings/day\n")
cat("  Upper-bound lost service-days:", comma(round(total_lost_days, 0)), "\n")
cat("  Conservative lost service-days:", comma(round(conservative_lost_days, 0)), "\n")
cat("  Conservative 95% CI:", "[", comma(round(cost_ci[1], 0)), ",", comma(round(cost_ci[2], 0)), "] service-days\n")
cat("  Upper-bound wage cost:", comma(round(total_cost, 0)), "shillings\n")
cat("  Conservative wage cost:", comma(round(conservative_cost, 0)), "shillings\n")

cost_summary <- tibble(
  metric = c("Post-treaty Afrikaner recruits", "Mean Afrikaner tenure", "Mean non-Afrikaner tenure",
             "Raw tenure gap", "Mean non-Afrikaner 3/c tenure", "Mean Afrikaner 3/c tenure",
             "Within-3/c tenure gap", "Upper-bound lost service-days", "Conservative lost service-days",
             "Conservative CI lower", "Conservative CI upper", "Mean Afrikaner wage",
             "Upper-bound wage cost", "Conservative wage cost"),
  value = c(n_post_afr, mean_tenure_afr, mean_tenure_non, tenure_shortfall,
            mean(post_non_trooper$tenure_days, na.rm = TRUE), mean(post_afr_trooper$tenure_days, na.rm = TRUE),
            trooper_gap, total_lost_days, conservative_lost_days, cost_ci[1], cost_ci[2],
            mean_wage_afr, total_cost, conservative_cost)
)

write.csv(cost_summary, file.path(tab_dir, "cost_calculation_summary.csv"), row.names = FALSE)

cost_tex <- c(
  "\\begin{threeparttable}",
  "\\begin{tabular}{@{}lc@{}}",
  "\\toprule",
  "Input or estimate & Value \\\\",
  "\\midrule",
  paste0("Post-treaty Afrikaner recruits & ", comma(n_post_afr), " \\\\"),
  paste0("Mean Afrikaner tenure & ", fmt_num(mean_tenure_afr, 0), " days \\\\"),
  paste0("Mean non-Afrikaner tenure & ", fmt_num(mean_tenure_non, 0), " days \\\\"),
  paste0("Raw tenure gap & ", fmt_num(tenure_shortfall, 0), " days \\\\"),
  paste0("Within-3/c Trooper tenure gap & ", fmt_num(trooper_gap, 0), " days \\\\"),
  paste0("Upper-bound lost service-days & ", comma(round(total_lost_days, 0)), " \\\\"),
  paste0("Conservative lost service-days & ", comma(round(conservative_lost_days, 0)), " \\\\"),
  paste0("Conservative 95\\% CI & [", comma(round(cost_ci[1], 0)), ", ", comma(round(cost_ci[2], 0)), "] \\\\"),
  paste0("Mean Afrikaner wage & ", fmt_num(mean_wage_afr, 2), " shillings/day \\\\"),
  paste0("Upper-bound wage cost & ", comma(round(total_cost, 0)), " shillings \\\\"),
  paste0("Conservative wage cost & ", comma(round(conservative_cost, 0)), " shillings \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: The upper bound scales the full post-treaty tenure gap by the number of post-treaty Afrikaner recruits. The conservative estimate instead uses the within-3/c-Trooper tenure gap, holding constant the main assignment channel. The confidence interval comes from a non-parametric bootstrap over Afrikaner and non-Afrikaner 3/c-Trooper tenures. Inputs are displayed rounded to whole days or two decimal places where appropriate; totals (lost service-days, wage costs, confidence intervals) are computed from the unrounded underlying values, so manual replication using the displayed inputs will produce small rounding discrepancies relative to the reported totals.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)
writeLines(cost_tex, file.path(tab_dir, "table_cost_calculation.tex"))


# ── EXTENSION 5: Separation Bunching at Contract Boundaries ───────────────
# Visualise how separations cluster relative to contract expiry and compute
# contract completion and re-enlistment rates by ethnicity.

cat("\n=== Extension 5: Contract Bunching ===\n")

bunch_data <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans),
         !is.na(contract_years), contract_years > 0, !is.na(event_type)) %>%
  mutate(
    contract_days  = contract_years * 365.25,
    days_to_end    = contract_days - tenure_days,
    group          = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans"),
    sep_type       = case_when(
      event_type == 1 ~ "Dismissed",
      event_type == 2 ~ "Voluntary exit",
      event_type == 3 ~ "Time expired",
      TRUE            ~ "Censored"
    )
  ) %>%
  filter(sep_type != "Censored")

cat("  Bunching sample N =", nrow(bunch_data), "\n")

# Panel A: Density histogram by ethnicity (pooled separation types)
fig12a <- bunch_data %>%
  filter(days_to_end >= -500, days_to_end <= 500) %>%
  ggplot(aes(x = days_to_end, fill = group)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 30,
                 alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 5, y = Inf, label = "Contract\nexpiry",
           vjust = 1.5, hjust = -0.1, size = 3, colour = "grey40") +
  labs(x = "Days to contract end", y = "Density", fill = NULL,
       title = "A. All separations") +
  scale_fill_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

# Panel B: Faceted by separation type
fig12b <- bunch_data %>%
  filter(days_to_end >= -500, days_to_end <= 500) %>%
  ggplot(aes(x = days_to_end, fill = group)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 30,
                 alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ sep_type, scales = "free_y") +
  labs(x = "Days to contract end", y = "Density", fill = NULL,
       title = "B. By separation type") +
  scale_fill_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr))

fig12 <- fig12a / fig12b + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_leap_fig(file.path(fig_dir, "fig12_contract_bunching.png"), fig12, width = 10, height = 9)
cat("  Saved Figure 12 (contract bunching).\n")

# Contract completion rates (survived to >= 95% of contract)
completion <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans),
         !is.na(contract_years), contract_years > 0) %>%
  mutate(
    contract_days = contract_years * 365.25,
    completed     = as.integer(tenure_days >= 0.95 * contract_days),
    group         = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans")
  )

completion_rates <- completion %>%
  group_by(group) %>%
  summarise(
    n             = n(),
    completion_rate = mean(completed, na.rm = TRUE),
    .groups       = "drop"
  )

# Re-enlistment rates
reenlist_rates <- sac %>%
  filter(!is.na(afrikaans)) %>%
  mutate(group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans")) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    reenlist_rate = mean(repeat_enlister, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nContract completion rates (survived to >= 95% of contract):\n")
print(as.data.frame(completion_rates))
cat("\nRe-enlistment rates:\n")
print(as.data.frame(reenlist_rates))

# Bar chart of completion and re-enlistment rates
rates_df <- bind_rows(
  completion_rates %>% select(group, rate = completion_rate) %>%
    mutate(measure = "Contract completion"),
  reenlist_rates %>% select(group, rate = reenlist_rate) %>%
    mutate(measure = "Re-enlistment")
)

fig_ext5c <- ggplot(rates_df, aes(x = group, y = rate, fill = measure)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(rate * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Contract completion" = "#3D8EB9",
                                "Re-enlistment"       = "#5C2346")) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Rate", fill = NULL) +
  theme(legend.position = "bottom")

save_leap_fig(file.path(fig_dir, "fig_ext5c_contract_completion.png"),
              fig_ext5c, width = 10, height = 6)
cat("  Saved Figure ext5c (contract completion/re-enlistment).\n")

# Summary CSV — write the four rates that anchor Figure ext5c's caption
# so the manuscript can be cross-checked against this file after rerun.
ext5_retention <- bind_rows(
  completion_rates %>% transmute(measure = "Completion",
                                  group, n, rate = completion_rate),
  reenlist_rates   %>% transmute(measure = "Re-enlistment",
                                  group, n, rate = reenlist_rate)
)
write.csv(ext5_retention,
          file.path(tab_dir, "ext5_retention_rates.csv"), row.names = FALSE)

ext5_summary <- ext5_retention

# Tidy up for CSV
ext5_out <- bunch_data %>%
  group_by(group, sep_type) %>%
  summarise(
    n          = n(),
    mean_days_to_end = mean(days_to_end, na.rm = TRUE),
    median_days_to_end = median(days_to_end, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(ext5_out, file.path(tab_dir, "ext5_contract_bunching.csv"), row.names = FALSE)


###########################################################################
#
#   SECTION 5B — EFFICIENCY TESTS: CHARACTER, LEARNING, QUALITY
#
###########################################################################

# ── Within-rank character comparison (key efficiency test) ────────────────

cat("\n=== Within-Rank Character Comparison ===\n")

# Full sample: character ~ afrikaans + controls + rank FE
char_full <- sac %>%
  filter(!is.na(character_score), !is.na(afrikaans), !is.na(age),
         !is.na(height_cm), !is.na(weight_kg), !is.na(married)) %>%
  mutate(enlist_year_f = factor(enlist_year),
         rank_cat_f    = factor(rank_cat))

char_reg_full <- suppressWarnings(feols(
  character_score ~ afrikaans + age + height_cm + weight_kg + married +
    factor(contract_years) + rank_cat_f + enlist_year_f,
  data = char_full, vcov = main_fixest_vcov, notes = FALSE
))

# Within 3/c Troopers only — the key test
char_trooper <- char_full %>% filter(title_clean == "3/c Trooper")

char_reg_trooper_raw <- suppressWarnings(feols(
  character_score ~ afrikaans,
  data = char_trooper, vcov = main_fixest_vcov, notes = FALSE
))

char_reg_trooper <- suppressWarnings(feols(
  character_score ~ afrikaans + age + height_cm + weight_kg + married +
    factor(contract_years) + enlist_year_f,
  data = char_trooper, vcov = main_fixest_vcov, notes = FALSE
))

char_full_stats <- extract_fixest_dual(char_reg_full, "afrikaans")
char_trooper_stats <- extract_fixest_dual(char_reg_trooper, "afrikaans")

cat("\nCharacter score regression — Full sample (with rank FE):\n")
cat("  Afrikaans coef =",
    round(char_full_stats["est"], 4),
    " (clustered se =", round(char_full_stats["se_clust"], 4),
    ", p =", round(char_full_stats["p_clust"], 4), ")\n")

cat("\nCharacter score regression — 3/c Troopers only:\n")
cat("  Afrikaans coef =",
    round(char_trooper_stats["est"], 4),
    " (clustered se =", round(char_trooper_stats["se_clust"], 4),
    ", p =", round(char_trooper_stats["p_clust"], 4), ")\n")
cat("  N =", nrow(char_trooper),
    " (Afr =", sum(char_trooper$afrikaans == 1),
    ", Non-Afr =", sum(char_trooper$afrikaans == 0), ")\n")

# T-test within 3/c Troopers
char_ttest <- t.test(character_score ~ afrikaans, data = char_trooper)
cat("\n  T-test (3/c Troopers): Afr mean =", round(char_ttest$estimate[2], 2),
    ", Non-Afr mean =", round(char_ttest$estimate[1], 2),
    ", diff =", round(diff(char_ttest$estimate), 2),
    ", p =", round(char_ttest$p.value, 4), "\n")

# Character distribution within 3/c Troopers
cat("\nCharacter distribution (3/c Troopers):\n")
cat("  Afrikaans:\n")
print(round(prop.table(table(char_trooper$character_clean[char_trooper$afrikaans == 1])), 3))
cat("  Non-Afrikaans:\n")
print(round(prop.table(table(char_trooper$character_clean[char_trooper$afrikaans == 0])), 3))

oster_character <- tibble(
  margin = "Within-rank character",
  beta_tilde = unname(coef(char_reg_trooper_raw)["afrikaans"]),
  beta_hat   = unname(coef(char_reg_trooper)["afrikaans"]),
  r_tilde    = fitstat(char_reg_trooper_raw, "r2")[[1]],
  r_hat      = fitstat(char_reg_trooper, "r2")[[1]]
) %>%
  mutate(
    r_max = pmin(1, 1.3 * r_hat),
    delta_zero = oster_delta_zero(beta_tilde, beta_hat, r_tilde, r_hat, r_max)
  )

write.csv(oster_character, file.path(tab_dir, "oster_character.csv"), row.names = FALSE)
cat("  Saved Oster sensitivity summary for within-rank character result.\n")

saveRDS(list(full = char_reg_full, trooper = char_reg_trooper,
             trooper_raw = char_reg_trooper_raw,
             full_stats = char_full_stats, trooper_stats = char_trooper_stats,
             oster = oster_character),
        file.path(mod_dir, "character_models.rds"))

# ── EXTENSION 3: Character Rating Bias Tests ─────────────────────────────
# Three tests of whether the rating system itself was biased against Afrikaners.

cat("\n=== Extension 3: Character Rating Bias Tests ===\n")

bias_data <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans),
         !is.na(character_score), !is.na(lwage), !is.na(age),
         !is.na(height_cm), !is.na(married), !is.na(event_type)) %>%
  mutate(
    dismiss_event = as.integer(event_type == 1),
    event_any     = as.integer(discharged == 1)
  )

# Test 1: Conditional outcomes
# Cox: Surv(tenure, dismiss) ~ character_score + afrikaans + controls
# If afrikaans HR < 1 conditional on rating → ratings biased against Afrikaners
cat("\n  Test 1: Conditional outcomes (dismissal ~ character + afrikaans)\n")

cox_bias1 <- tryCatch(
  coxph(Surv(tenure_days, dismiss_event) ~ character_score + afrikaans +
          lwage + age + height_cm + married + factor(contract_years) + cluster(id),
        data = bias_data),
  error = function(e) { cat("    Failed:", e$message, "\n"); NULL }
)

if (!is.null(cox_bias1)) {
  bias1_summ <- summary(cox_bias1)$coefficients
  cat("    character_score: HR =", round(exp(coef(cox_bias1)["character_score"]), 3),
      ", p =", round(bias1_summ["character_score", "Pr(>|z|)"], 4), "\n")
  cat("    afrikaans:       HR =", round(exp(coef(cox_bias1)["afrikaans"]), 3),
      ", p =", round(bias1_summ["afrikaans", "Pr(>|z|)"], 4), "\n")
  cat("    Interpretation: if afrikaans HR < 1 conditional on character → ratings biased against Afrikaners\n")
  cat("                    if afrikaans HR ~ 1 → ratings unbiased\n")
}

# Test 2: Re-enlistment interaction
# Logit: repeat_enlister ~ character_score * afrikaans + controls
cat("\n  Test 2: Re-enlistment interaction (do ratings predict re-enlistment equally?)\n")

reenlist_data <- bias_data %>% filter(!is.na(repeat_enlister))
n_afr_reenlist <- sum(reenlist_data$afrikaans == 1 & reenlist_data$repeat_enlister == 1)
cat("    Afrikaner re-enlisters:", n_afr_reenlist, "\n")

if (n_afr_reenlist >= 5) {
  logit_bias2 <- tryCatch(
    glm(repeat_enlister ~ character_score * afrikaans +
          lwage + age + height_cm + married + factor(contract_years),
        data = reenlist_data, family = binomial),
    error = function(e) { cat("    Failed:", e$message, "\n"); NULL }
  )

  if (!is.null(logit_bias2)) {
    bias2_summ <- summary(logit_bias2)$coefficients
    key_vars2 <- c("character_score", "afrikaans", "character_score:afrikaans")
    for (v in key_vars2) {
      if (v %in% rownames(bias2_summ)) {
        cat("    ", v, ": coef =", round(bias2_summ[v, "Estimate"], 4),
            ", p =", round(bias2_summ[v, "Pr(>|z|)"], 4), "\n")
      }
    }
    cat("    Interpretation: significant interaction → ratings predict re-enlistment differently by group\n")
  }
} else {
  cat("    Insufficient Afrikaner re-enlisters for interaction test.\n")
}

# Test 3: Calibration — among good-rated recruits (score >= 5)
# Cox dismissal model restricted to good-rated recruits
cat("\n  Test 3: Calibration among good-rated recruits (character >= 5)\n")

good_data <- bias_data %>% filter(character_score >= 5)
cat("    Good-rated N =", nrow(good_data),
    " (Afr =", sum(good_data$afrikaans == 1),
    ", Non-Afr =", sum(good_data$afrikaans == 0), ")\n")

# Dismissal rates by ethnicity among good-rated
good_dismiss_afr <- mean(good_data$dismiss_event[good_data$afrikaans == 1])
good_dismiss_non <- mean(good_data$dismiss_event[good_data$afrikaans == 0])
cat("    Dismissal rate (good-rated Afr):     ", round(good_dismiss_afr, 4), "\n")
cat("    Dismissal rate (good-rated Non-Afr): ", round(good_dismiss_non, 4), "\n")

cox_bias3 <- tryCatch(
  coxph(Surv(tenure_days, dismiss_event) ~ afrikaans +
          lwage + age + height_cm + married + factor(contract_years) + cluster(id),
        data = good_data),
  error = function(e) { cat("    Failed:", e$message, "\n"); NULL }
)

if (!is.null(cox_bias3)) {
  bias3_summ <- summary(cox_bias3)$coefficients
  cat("    afrikaans HR (among good-rated): ", round(exp(coef(cox_bias3)["afrikaans"]), 3),
      ", p =", round(bias3_summ["afrikaans", "Pr(>|z|)"], 4), "\n")
  cat("    Interpretation: HR < 1 → good-rated Afrikaners less likely dismissed\n")
  cat("                            (ratings understate their quality)\n")
}

# Save all bias test models
char_bias_models <- list(
  conditional_outcomes  = cox_bias1,
  reenlist_interaction  = if (exists("logit_bias2")) logit_bias2 else NULL,
  calibration           = cox_bias3
)
saveRDS(char_bias_models, file.path(mod_dir, "character_bias_models.rds"))
cat("  Saved character bias models.\n")

# ── Employer learning: time-varying Afrikaans hazard ──────────────────────

cat("\n=== Employer Learning Test ===\n")

# Piecewise Cox model: estimate Afrikaans hazard ratio by tenure period
# Using dismissal/deserted as the event (employer action), others censored
learn_data <- surv_data %>%
  filter(!is.na(lwage), !is.na(age), !is.na(height_cm),
         !is.na(weight_kg), !is.na(married)) %>%
  mutate(
    dismiss_event = as.integer(event_type == 1),
    contract_days = contract_years * 365.25
  )

learn_cc_flag <- surv_data %>%
  mutate(
    missing_weight = is.na(weight_kg),
    missing_ride   = is.na(can_ride),
    missing_shoot  = is.na(can_shoot),
    in_learning_sample = as.integer(!is.na(lwage) & !is.na(age) & !is.na(height_cm) &
                                      !is.na(weight_kg) & !is.na(married))
  )

learn_missing_summary <- tibble(
  metric = c("Total in survival sample", "Included in learning sample",
             "Excluded from learning sample", "Afrikaans share included",
             "Afrikaans share excluded", "Missing weight", "Missing can_ride",
             "Missing can_shoot", "Exclusion ~ Afrikaans p-value"),
  value = c(
    nrow(learn_cc_flag),
    sum(learn_cc_flag$in_learning_sample),
    sum(learn_cc_flag$in_learning_sample == 0),
    mean(learn_cc_flag$afrikaans[learn_cc_flag$in_learning_sample == 1], na.rm = TRUE),
    mean(learn_cc_flag$afrikaans[learn_cc_flag$in_learning_sample == 0], na.rm = TRUE),
    mean(learn_cc_flag$missing_weight, na.rm = TRUE),
    mean(learn_cc_flag$missing_ride, na.rm = TRUE),
    mean(learn_cc_flag$missing_shoot, na.rm = TRUE),
    summary(glm(I(in_learning_sample == 0) ~ afrikaans, data = learn_cc_flag, family = binomial))$coefficients["afrikaans", "Pr(>|z|)"]
  )
)

write.csv(learn_missing_summary, file.path(tab_dir, "learning_sample_missingness.csv"), row.names = FALSE)
cat("\nLearning-sample missingness:\n")
print(as.data.frame(learn_missing_summary))

periods      <- c(0, 180, 365, 730, Inf)
period_labels <- c("0-6 months", "6-12 months", "1-2 years", "2+ years")

# Dismissal hazard by period
learn_dismiss <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(dismiss_event == 1 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Dismissal",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Dismissal",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Dismissal",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

# Voluntary exit hazard by period (employee action — fairness response)
learn_vol <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(event_type == 2 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Voluntary exit",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_vol_contracttime <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start, !is.na(contract_days)) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(event_type == 2 & tenure_days <= t_end),
      remaining_contract_time = pmax(contract_days - t_start, 0)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit + remaining contract",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + remaining_contract_time + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit + remaining contract",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Voluntary exit + remaining contract",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_results <- bind_rows(learn_dismiss, learn_vol)
learn_vol_comparison <- bind_rows(learn_vol, learn_vol_contracttime)

# ── Calendar-year-of-event FE for voluntary exits ─────────────────────────
# A standing alternative to the fairness-response interpretation is that
# Afrikaner outside options recovered between 1903 and 1908 as the rural
# and mining economies thawed, so voluntary exits would rise on that
# schedule even without any misclassification mechanism. Enlistment-month
# fixed effects, which are at hire, do not absorb that. We therefore
# re-estimate the piecewise voluntary-exit Cox with calendar-year-of-event
# fixed effects (year of `date_discharge`), which absorb common
# year-specific shocks to outside options.

learn_vol_yearfe <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start, !is.na(date_discharge)) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(event_type == 2 & tenure_days <= t_end),
      event_year = lubridate::year(date_discharge)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i],
                  outcome = "Voluntary exit + event-year FE",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + factor(event_year) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i],
                  outcome = "Voluntary exit + event-year FE",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Voluntary exit + event-year FE",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

cat("\nVoluntary-exit Cox with calendar-year-of-event FE:\n")
print(as.data.frame(learn_vol_yearfe %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

write.csv(learn_vol_yearfe,
          file.path(tab_dir, "voluntary_exit_event_year_fe.csv"),
          row.names = FALSE)

# ── Total-effect specification (no contract-length control) ───────────────
# Contract length is itself an outcome of discriminatory sorting (Section 6.1),
# so conditioning on it estimates the *within-assignment* learning effect.
# The companion specification below drops contract length to estimate the
# *total* ethnic dismissal/exit disparity by tenure period.

learn_dismiss_total <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(dismiss_event == 1 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Dismissal (total)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Dismissal (total)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Dismissal (total)",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_vol_total <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(event_type == 2 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit (total)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit (total)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Voluntary exit (total)",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_results_total <- bind_rows(learn_dismiss_total, learn_vol_total)

cat("\nEmployer learning — TOTAL Afrikaans HR by tenure period (no contract control):\n")
print(as.data.frame(learn_results_total %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

write.csv(learn_results_total,
          file.path(tab_dir, "employer_learning_results_total.csv"), row.names = FALSE)

# ── Relaxed-sample Cox (drops the gratuitous missing-weight filter) ──────
# `weight_kg` is not a covariate in the Cox specifications, so requiring it
# to be non-missing in `learn_data` was tightening the sample without
# improving the model. The block below repeats the within-assignment Cox
# (Panel B of Table 3) on a relaxed sample that drops only the
# missing-weight requirement, restoring the Afrikaner-heavy excluded
# observations that the headline filter had removed. This is a robustness
# check, not a restatement of headline numbers; both samples are reported
# in the manuscript.

learn_data_relaxed <- surv_data %>%
  filter(!is.na(lwage), !is.na(age), !is.na(height_cm),
         !is.na(married)) %>%
  mutate(
    dismiss_event = as.integer(event_type == 1),
    contract_days = contract_years * 365.25
  )

cat("\nRelaxed-sample diagnostic:\n")
cat("  Headline learn_data N      :", nrow(learn_data),
    "; Afrikaner share =",
    round(mean(learn_data$afrikaans, na.rm = TRUE), 4), "\n")
cat("  Relaxed learn_data_relaxed N:", nrow(learn_data_relaxed),
    "; Afrikaner share =",
    round(mean(learn_data_relaxed$afrikaans, na.rm = TRUE), 4), "\n")

learn_dismiss_relaxed <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data_relaxed %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(dismiss_event == 1 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Dismissal (relaxed)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Dismissal (relaxed)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Dismissal (relaxed)",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_vol_relaxed <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_data_relaxed %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(event_type == 2 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit (relaxed)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], outcome = "Voluntary exit (relaxed)",
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n_events_afr = n_afr_events,
                  n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period       = period_labels[i],
    outcome      = "Voluntary exit (relaxed)",
    HR           = exp(coef(m)["afrikaans"]),
    ci_lo        = ci[1],
    ci_hi        = ci[2],
    n_events     = sum(d$evt),
    n_events_afr = n_afr_events,
    n            = nrow(d)
  )
})

learn_results_relaxed <- bind_rows(learn_dismiss_relaxed, learn_vol_relaxed)

cat("\nEmployer learning — RELAXED-sample Afrikaans HR by tenure period:\n")
print(as.data.frame(learn_results_relaxed %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

write.csv(learn_results_relaxed,
          file.path(tab_dir, "employer_learning_results_relaxed.csv"),
          row.names = FALSE)

# ── Permutation test on the dynamic dismissal pattern ─────────────────────
# The 0--6 month dismissal HR is estimated on 11 Afrikaner events. To test
# whether the early-vs-late attenuation pattern is more extreme than chance,
# we randomly reassign the Afrikaans label across recruits and recompute
# the early-period HR / late-period HR ratio under each draw. The observed
# ratio is then compared to the permutation distribution. This is a power
# diagnostic, not a substitute for the within-assignment Cox.

cat("\n=== Permutation test on dismissal-attenuation pattern ===\n")

cox_period_HRs <- function(data) {
  hrs <- numeric(length(period_labels))
  for (i in seq_along(period_labels)) {
    t_start <- periods[i]
    t_end   <- periods[i + 1]
    d <- data %>%
      filter(tenure_days > t_start) %>%
      mutate(
        t1  = t_start,
        t2  = pmin(tenure_days, t_end),
        evt = as.integer(dismiss_event == 1 & tenure_days <= t_end)
      )
    if (sum(d$evt) < 10 ||
        sum(d$evt[d$afrikaans_perm == 1], na.rm = TRUE) < 3) {
      hrs[i] <- NA_real_
      next
    }
    m <- tryCatch(
      coxph(Surv(t1, t2, evt) ~ afrikaans_perm + lwage + age + height_cm +
              married + factor(contract_years) + cluster(id),
            data = d),
      error = function(e) NULL
    )
    hrs[i] <- if (is.null(m) || !"afrikaans_perm" %in% names(coef(m)))
                NA_real_ else exp(coef(m)["afrikaans_perm"])
  }
  hrs
}

set.seed(20260430)
learn_perm_data <- learn_data
n_recruits  <- nrow(learn_perm_data)
afr_share   <- mean(learn_perm_data$afrikaans, na.rm = TRUE)

# Observed ratio
learn_perm_data$afrikaans_perm <- learn_perm_data$afrikaans
obs_hrs <- cox_period_HRs(learn_perm_data)
obs_ratio <- if (!is.na(obs_hrs[1]) && !is.na(obs_hrs[3]) && obs_hrs[3] > 0)
               obs_hrs[1] / obs_hrs[3] else NA_real_

cat("  Observed within-assignment HRs by period: ",
    paste(round(obs_hrs, 3), collapse = ", "), "\n")
cat("  Observed early/late HR ratio (0--6m / 1--2y):", round(obs_ratio, 3), "\n")

# Permutation draws
n_perm <- 500
perm_ratios <- numeric(n_perm)
for (b in seq_len(n_perm)) {
  learn_perm_data$afrikaans_perm <- as.integer(
    runif(n_recruits) < afr_share
  )
  hrs_b <- cox_period_HRs(learn_perm_data)
  perm_ratios[b] <- if (!is.na(hrs_b[1]) && !is.na(hrs_b[3]) && hrs_b[3] > 0)
                      hrs_b[1] / hrs_b[3] else NA_real_
}

perm_ratios <- perm_ratios[!is.na(perm_ratios)]
perm_p_one_sided <- mean(perm_ratios >= obs_ratio, na.rm = TRUE)

cat("  Permutation draws (valid):", length(perm_ratios), "of", n_perm, "\n")
cat("  Permutation distribution of early/late HR ratio:\n")
cat("    quantiles  5% / 50% / 95%:",
    paste(round(quantile(perm_ratios, c(0.05, 0.5, 0.95)), 3), collapse = " / "), "\n")
cat("  One-sided p-value (Pr[perm ratio >= observed]):", round(perm_p_one_sided, 4), "\n")

learn_permutation <- tibble(
  observed_early_HR  = obs_hrs[1],
  observed_late_HR   = obs_hrs[3],
  observed_ratio     = obs_ratio,
  permutation_p_one_sided = perm_p_one_sided,
  permutation_q05    = quantile(perm_ratios, 0.05),
  permutation_q50    = quantile(perm_ratios, 0.50),
  permutation_q95    = quantile(perm_ratios, 0.95),
  n_valid_draws      = length(perm_ratios)
)

write.csv(learn_permutation,
          file.path(tab_dir, "learning_permutation.csv"),
          row.names = FALSE)

cat("\nEmployer learning — Afrikaans HR by tenure period:\n")
print(as.data.frame(learn_results %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

cat("\nVoluntary-exit comparison with remaining contract time:\n")
print(as.data.frame(learn_vol_comparison %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

write.csv(learn_results,
          file.path(tab_dir, "employer_learning_results.csv"), row.names = FALSE)
write.csv(learn_vol_comparison,
          file.path(tab_dir, "voluntary_exit_remaining_contract.csv"), row.names = FALSE)

# Figure 8: Employer learning forest plot (two panels)
# Label shows total events in the at-risk full-sample window AND the
# Afrikaner-specific event count, since the latter governs precision of
# the plotted hazard ratio for a small minority group.
fig8_data <- learn_results %>%
  filter(!is.na(HR)) %>%
  mutate(period = factor(period, levels = rev(period_labels)),
         label = paste0("All: ", n_events, " | Afr: ", n_events_afr))

fig8 <- ggplot(fig8_data, aes(x = HR, y = period)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.5) +
  geom_text(aes(label = label, x = ci_hi), hjust = -0.1, size = 2.5,
            colour = "grey50") +
  facet_wrap(~ outcome, scales = "free_x") +
  labs(x = "Hazard ratio (Afrikaans vs. Non-Afrikaans)",
       y = NULL) +
  scale_x_log10() +
  theme(axis.text.y = element_text(size = 10))

save_leap_fig(file.path(fig_dir, "fig8_employer_learning.png"), fig8, width = 10, height = 6)
cat("  Saved Figure 8.\n")

# ── Quality heterogeneity (do high-quality Afrikaners fare differently?) ──

cat("\n=== Quality Heterogeneity Test ===\n")

# Construct a simple quality index from pre-determined observables
# High quality: above-median height + can ride + prior army (any 2 of 3)
surv_data <- surv_data %>%
  mutate(
    tall = as.integer(height_cm > median(height_cm, na.rm = TRUE)),
    high_quality = as.integer(
      (coalesce(tall, 0L) + coalesce(as.integer(can_ride), 0L) +
       coalesce(as.integer(prior_army), 0L)) >= 2
    ),
    afr_hq = afrikaans * high_quality
  )

# Competing risks: interact Afrikaans × high_quality
hq_cr_data <- surv_data %>%
  filter(!is.na(lwage), !is.na(age), !is.na(height_cm),
         !is.na(contract_years), !is.na(married), !is.na(high_quality),
         !is.na(event_type)) %>%
  mutate(across(c(weight_kg, can_ride, can_shoot),
                ~ replace_na(.x, median(.x, na.rm = TRUE))))

hq_covs <- model.matrix(
  ~ lwage + afrikaans + high_quality + afr_hq + age + height_cm +
    weight_kg + married + contract_years,
  data = hq_cr_data
)[, -1]

# Dismissal with quality interaction
hq_crr_dismiss <- tryCatch(
  crr(ftime = hq_cr_data$tenure_days, fstatus = hq_cr_data$event_type,
      cov1 = hq_covs, failcode = 1, cencode = 0),
  error = function(e) NULL
)

# Voluntary with quality interaction
hq_crr_vol <- tryCatch(
  crr(ftime = hq_cr_data$tenure_days, fstatus = hq_cr_data$event_type,
      cov1 = hq_covs, failcode = 2, cencode = 0),
  error = function(e) NULL
)

cat("\nQuality heterogeneity — Dismissal:\n")
if (!is.null(hq_crr_dismiss)) {
  hq_d_coefs <- data.frame(
    Variable = colnames(hq_covs),
    coef = round(hq_crr_dismiss$coef, 4),
    HR   = round(exp(hq_crr_dismiss$coef), 3),
    se   = round(sqrt(diag(hq_crr_dismiss$var)), 4)
  )
  print(hq_d_coefs[hq_d_coefs$Variable %in% c("afrikaans", "high_quality", "afr_hq"), ])
}

cat("\nQuality heterogeneity — Voluntary exit:\n")
if (!is.null(hq_crr_vol)) {
  hq_v_coefs <- data.frame(
    Variable = colnames(hq_covs),
    coef = round(hq_crr_vol$coef, 4),
    HR   = round(exp(hq_crr_vol$coef), 3),
    se   = round(sqrt(diag(hq_crr_vol$var)), 4)
  )
  print(hq_v_coefs[hq_v_coefs$Variable %in% c("afrikaans", "high_quality", "afr_hq"), ])
}

cat("\n  Interpretation: If afr_hq (Afrikaans x High Quality) is positive for\n")
cat("  voluntary exit, high-quality Afrikaners are MORE likely to quit —\n")
cat("  consistent with perceived unfairness of over-sorting.\n")

saveRDS(list(dismiss = hq_crr_dismiss, voluntary = hq_crr_vol),
        file.path(mod_dir, "quality_heterogeneity_models.rds"))


###########################################################################
#
#   SECTION 5C — EMPLOYER LEARNING ROBUSTNESS & FAIRNESS TESTS
#
###########################################################################

# ── Employer learning within 3/c Troopers ─────────────────────────────────

cat("\n=== Employer Learning: Within 3/c Troopers ===\n")

learn_trooper <- learn_data %>% filter(title_clean == "3/c Trooper")

learn_trooper_results <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_trooper %>%
    filter(tenure_days > t_start) %>%
    mutate(t1  = t_start,
           t2  = pmin(tenure_days, t_end),
           evt = as.integer(dismiss_event == 1 & tenure_days <= t_end))

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 2) {
    return(tibble(period = period_labels[i], HR = NA_real_,
                  ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + lwage + age + height_cm +
            married + factor(contract_years) + cluster(id), data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i], HR = NA_real_,
                  ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(period = period_labels[i],
         HR = exp(coef(m)["afrikaans"]),
         ci_lo = ci[1], ci_hi = ci[2],
         n_events = sum(d$evt), n = nrow(d))
})

cat("\nWithin 3/c Troopers — Dismissal HR by period:\n")
print(as.data.frame(learn_trooper_results %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

# ── Placebo employer learning (Scottish, Irish) ───────────────────────────

cat("\n=== Placebo Employer Learning ===\n")

learn_data <- learn_data %>%
  mutate(scottish = as.integer(region == "Scotland"),
         irish    = as.integer(region == "Ireland"))

placebo_learning <- function(data, group_var, group_label) {
  map_dfr(seq_along(period_labels), function(i) {
    t_start <- periods[i]
    t_end   <- periods[i + 1]

    d <- data %>%
      filter(tenure_days > t_start, !is.na(!!sym(group_var))) %>%
      mutate(t1  = t_start,
             t2  = pmin(tenure_days, t_end),
             evt = as.integer(dismiss_event == 1 & tenure_days <= t_end))

    n_grp_events <- sum(d$evt[d[[group_var]] == 1], na.rm = TRUE)
    if (sum(d$evt, na.rm = TRUE) < 10 || n_grp_events < 3) {
      return(tibble(group = group_label, period = period_labels[i],
                    HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                    n_events = sum(d$evt), n_events_grp = n_grp_events))
    }

    fml <- as.formula(paste0("Surv(t1, t2, evt) ~ ", group_var,
                             " + lwage + age + height_cm + married + factor(contract_years) + cluster(id)"))
    m <- tryCatch(coxph(fml, data = d), error = function(e) NULL)
    if (is.null(m) || !group_var %in% names(coef(m))) {
      return(tibble(group = group_label, period = period_labels[i],
                    HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                    n_events = sum(d$evt), n_events_grp = n_grp_events))
    }

    ci <- exp(confint(m)[group_var, ])
    tibble(group = group_label, period = period_labels[i],
           HR = exp(coef(m)[group_var]),
           ci_lo = ci[1], ci_hi = ci[2],
           n_events = sum(d$evt), n_events_grp = n_grp_events)
  })
}

placebo_scot  <- placebo_learning(learn_data, "scottish", "Scottish")
placebo_irish <- placebo_learning(learn_data, "irish", "Irish")

cat("\nPlacebo employer learning — Scottish (dismissal):\n")
print(as.data.frame(placebo_scot %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))
cat("\nPlacebo employer learning — Irish (dismissal):\n")
print(as.data.frame(placebo_irish %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

# ── Formal time-varying coefficient test ──────────────────────────────────

cat("\n=== Formal Time-Varying Coefficient Test ===\n")

cox_tv <- tryCatch(
  coxph(
    Surv(tenure_days, event) ~ afrikaans + tt(afrikaans) +
      lwage + age + height_cm + married + factor(contract_years) + cluster(id),
    data = learn_data %>% filter(tenure_days > 0),
    tt = function(x, t, ...) x * log(t)
  ),
  error = function(e) { cat("  tt() model failed:", e$message, "\n"); NULL }
)

if (!is.null(cox_tv)) {
  cat("\nCox with time-varying Afrikaans coefficient (Afrikaans x log(time)):\n")
  cat("  Afrikaans (baseline):  coef =",
      round(coef(cox_tv)["afrikaans"], 4),
      ", HR =", round(exp(coef(cox_tv)["afrikaans"]), 3),
      ", p =", round(summary(cox_tv)$coefficients["afrikaans", "Pr(>|z|)"], 4), "\n")
  cat("  Afrikaans x log(t):   coef =",
      round(coef(cox_tv)["tt(afrikaans)"], 4),
      ", p =", round(summary(cox_tv)$coefficients["tt(afrikaans)", "Pr(>|z|)"], 4), "\n")
  cat("  Interpretation: negative interaction = HR declines with tenure (learning)\n")
}

# ── Fairness response: high-performing Afrikaners' voluntary exit ─────────

cat("\n=== Fairness Response Test ===\n")
cat("  Do high-character Afrikaners who survive 6 months quit more?\n")

fair_data <- surv_data %>%
  filter(tenure_days > 180, !is.na(character_score), !is.na(lwage),
         !is.na(age), !is.na(height_cm), !is.na(married)) %>%
  mutate(good_char  = as.integer(character_score >= 5),
         vol_event  = as.integer(event_type == 2),
         t_from_6m  = tenure_days - 180)

n_afr_goodchar <- sum(fair_data$afrikaans == 1 & fair_data$good_char == 1)
cat("  Afrikaner x good character observations:", n_afr_goodchar, "\n")

if (n_afr_goodchar >= 10) {
  cox_fair <- tryCatch(
    coxph(Surv(t_from_6m, vol_event) ~ afrikaans * good_char +
            lwage + age + height_cm + married + factor(contract_years) + cluster(id),
          data = fair_data),
    error = function(e) NULL
  )

  if (!is.null(cox_fair)) {
    cat("\nCox: Voluntary exit among 6-month survivors\n")
    key_vars <- c("afrikaans", "good_char", "afrikaans:good_char")
    for (v in key_vars) {
      if (v %in% names(coef(cox_fair))) {
        cat("  ", v, ": HR =", round(exp(coef(cox_fair)[v]), 3),
            ", p =", round(summary(cox_fair)$coefficients[v, "Pr(>|z|)"], 4), "\n")
      }
    }
    cat("  Positive Afr x GoodChar interaction = high-performers quit MORE (fairness)\n")
    cat("  N =", cox_fair$n, ", Events =", cox_fair$nevent, "\n")
  }
} else {
  cat("  Insufficient observations for interaction test.\n")
}

# ── Cost of early screening ───────────────────────────────────────────────

cat("\n=== Cost of Early Screening (First 6 Months) ===\n")

post_afr_6m <- surv_data %>%
  filter(afrikaans == 1, post_vereeniging == 1, !is.na(event_type))
post_non_6m <- surv_data %>%
  filter(afrikaans == 0, post_vereeniging == 1, !is.na(event_type))

# Dismissal rate in first 6 months
afr_dismiss_6m <- mean(post_afr_6m$event_type == 1 & post_afr_6m$tenure_days <= 180,
                        na.rm = TRUE)
non_dismiss_6m <- mean(post_non_6m$event_type == 1 & post_non_6m$tenure_days <= 180,
                        na.rm = TRUE)
n_afr_post     <- nrow(post_afr_6m)

excess_dismiss_6m <- round(n_afr_post * (afr_dismiss_6m - non_dismiss_6m))
mean_wage_afr     <- mean(post_afr_6m$wage, na.rm = TRUE)

# Assume ~30 days training + recruitment cost ≈ 2 months' wages per recruit
recruit_cost_per_man <- mean_wage_afr * 60
total_screening_cost <- excess_dismiss_6m * recruit_cost_per_man

cat("  Dismissal rate (first 6 months):\n")
cat("    Afrikaans:", round(afr_dismiss_6m, 4), "\n")
cat("    Non-Afrikaans:", round(non_dismiss_6m, 4), "\n")
cat("  N post-treaty Afrikaners:", n_afr_post, "\n")
cat("  Excess early dismissals:", excess_dismiss_6m, "\n")
cat("  Estimated cost per excess dismissal:", round(recruit_cost_per_man, 0),
    "shillings (2 months' wages)\n")
cat("  Total screening waste:", comma(round(total_screening_cost, 0)), "shillings\n")

# ── Figure 10: Combined employer learning comparison ──────────────────────
# Afrikaner vs placebo groups over time — single compelling figure

cat("\n=== Figure 10: Employer Learning Comparison ===\n")

learn_comparison <- bind_rows(
  learn_dismiss %>% rename(n_events_grp = n_events_afr) %>% mutate(group = "Afrikaans"),
  placebo_scot %>% select(-group) %>% mutate(group = "Scottish"),
  placebo_irish %>% select(-group) %>% mutate(group = "Irish")
) %>%
  filter(!is.na(HR)) %>%
  mutate(period = factor(period, levels = period_labels),
         group  = factor(group, levels = c("Afrikaans", "Scottish", "Irish")))

fig10 <- ggplot(learn_comparison, aes(x = period, y = HR, colour = group)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                  position = position_dodge(width = 0.4), size = 0.5) +
  scale_y_log10() +
  labs(x = "Tenure period", y = "Dismissal hazard ratio (log scale)",
       colour = NULL) +
  scale_colour_manual(values = c("Afrikaans" = "#5C2346",
                                  "Scottish" = "#6B8E5E",
                                  "Irish"    = "#D4A03E")) +
  theme(axis.text.x = element_text(size = 9))

save_leap_fig(file.path(fig_dir, "fig10_learning_comparison.png"), fig10, width = 10, height = 6)
cat("  Saved Figure 10.\n")

# ── EXTENSION 1: Dynamic Selection vs. Genuine Learning ──────────────────
# Track mean observables of surviving Afrikaners at each tenure cutoff.
# If composite quality index is flat → learning; if rising → selection.

cat("\n=== Extension 1: Dynamic Selection vs. Genuine Learning ===\n")

obs_vars <- c("height_cm", "weight_kg", "can_ride", "can_shoot",
              "farmer", "age", "prior_army")

# Use the same periods as employer learning
cutoffs <- c(0, 180, 365, 730)
cutoff_labels <- c("Day 0", "180 days", "365 days", "730 days")

# Full-sample mean and sd for z-scoring
full_means <- sac %>%
  filter(!is.na(afrikaans)) %>%
  summarise(across(all_of(obs_vars), ~ mean(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "full_mean")

full_sds <- sac %>%
  filter(!is.na(afrikaans)) %>%
  summarise(across(all_of(obs_vars), ~ sd(as.numeric(.x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "full_sd")

full_stats <- left_join(full_means, full_sds, by = "variable")

# At each cutoff, compute mean observables for surviving Afrikaners and non-Afrikaners
survivor_quality <- map_dfr(seq_along(cutoffs), function(i) {
  cutoff <- cutoffs[i]

  survivors <- sac %>%
    filter(!is.na(afrikaans), !is.na(tenure_days), tenure_days > cutoff) %>%
    mutate(group = if_else(afrikaans == 1, "Afrikaans", "Non-Afrikaans"))

  if (nrow(survivors) < 20) return(NULL)

  survivors %>%
    group_by(group) %>%
    summarise(
      n = n(),
      across(all_of(obs_vars), ~ mean(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = all_of(obs_vars), names_to = "variable", values_to = "mean_val") %>%
    left_join(full_stats, by = "variable") %>%
    mutate(
      z_score = (mean_val - full_mean) / full_sd,
      cutoff  = cutoff,
      cutoff_label = cutoff_labels[i]
    )
})

# Composite quality index: average z-score across obs_vars
composite_quality <- survivor_quality %>%
  group_by(group, cutoff, cutoff_label, n) %>%
  summarise(
    composite_z  = mean(z_score, na.rm = TRUE),
    composite_se = sd(z_score, na.rm = TRUE) / sqrt(sum(!is.na(z_score))),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lo = composite_z - 1.96 * composite_se,
    ci_hi = composite_z + 1.96 * composite_se
  )

cat("\nComposite quality index by ethnicity and cutoff:\n")
print(as.data.frame(composite_quality %>%
        mutate(across(c(composite_z, ci_lo, ci_hi), ~ round(.x, 4)))))

# Figure 11: Composite quality trajectory by ethnicity
fig11 <- ggplot(composite_quality,
                aes(x = cutoff, y = composite_z, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = group), alpha = 0.15,
              colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  scale_fill_manual(values = c("Afrikaans" = col_afr, "Non-Afrikaans" = col_non_afr)) +
  scale_x_continuous(breaks = cutoffs, labels = cutoff_labels) +
  labs(x = "Tenure cutoff (survivors beyond this point)",
       y = "Composite quality index (mean z-score)",
       colour = NULL, fill = NULL) +
  theme(legend.position = "bottom")

save_leap_fig(file.path(fig_dir, "fig11_dynamic_selection.png"), fig11, width = 10, height = 6)
cat("  Saved Figure 11 (dynamic selection composite quality).\n")

# Within-Afrikaner balance test: regress each observable on survived_past_cutoff
cat("\n  Balance tests: do survivors differ on observables?\n")

balance_results <- map_dfr(seq_along(cutoffs[-1]), function(i) {
  cutoff <- cutoffs[i + 1]  # skip Day 0

  test_data <- sac %>%
    filter(afrikaans == 1, !is.na(tenure_days)) %>%
    mutate(survived = as.integer(tenure_days > cutoff))

  map_dfr(obs_vars, function(v) {
    d <- test_data %>% filter(!is.na(!!sym(v)))
    if (nrow(d) < 20 || sum(d$survived) < 5 || sum(d$survived == 0) < 5) {
      return(tibble(cutoff = cutoff, cutoff_label = cutoff_labels[i + 1],
                    variable = v, coef = NA_real_, se = NA_real_, p = NA_real_))
    }

    fml <- as.formula(paste0(v, " ~ survived"))
    m <- tryCatch(lm(fml, data = d), error = function(e) NULL)
    if (is.null(m) || !"survived" %in% names(coef(m))) {
      return(tibble(cutoff = cutoff, cutoff_label = cutoff_labels[i + 1],
                    variable = v, coef = NA_real_, se = NA_real_, p = NA_real_))
    }

    s <- summary(m)$coefficients
    tibble(cutoff = cutoff, cutoff_label = cutoff_labels[i + 1],
           variable = v,
           coef = s["survived", "Estimate"],
           se   = s["survived", "Std. Error"],
           p    = s["survived", "Pr(>|t|)"])
  })
})

balance_results <- balance_results %>%
  filter(!is.na(coef)) %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    significant = p < 0.05,
    variable = recode(variable,
      height_cm = "Height", weight_kg = "Weight", can_ride = "Can ride",
      can_shoot = "Can shoot", farmer = "Farmer", age = "Age",
      prior_army = "Prior army")
  )

cat("\n  Balance test coefficients (survived indicator within Afrikaners):\n")
print(as.data.frame(balance_results %>%
        mutate(across(c(coef, se, p), ~ round(.x, 4)))))

# Balance test forest plot faceted by cutoff
fig_ext1b <- ggplot(balance_results,
                     aes(x = coef, y = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi,
                      colour = ifelse(significant, "#A34466", LEAP_NONSIG_COLOR)),
                  size = 0.4) +
  scale_colour_identity() +
  facet_wrap(~ cutoff_label) +
  labs(x = "Coefficient on survived indicator",
       y = NULL) +
  theme(axis.text.y = element_text(size = 9))

save_leap_fig(file.path(fig_dir, "fig_ext1b_balance_tests.png"),
              fig_ext1b, width = 12, height = 6)
cat("  Saved Figure ext1b (balance tests).\n")

# Save CSV
write.csv(composite_quality, file.path(tab_dir, "ext1_survivor_selection.csv"),
          row.names = FALSE)


###########################################################################
#
#   SECTION 6 — ROBUSTNESS
#
###########################################################################

# ── Helper: Fit Fine-Gray for a given dataset and Afrikaans variable ─────

fit_crr_summary <- function(data, afr_var = "afrikaans") {
  # Prepare data
  d <- data %>%
    filter(!is.na(tenure_days), tenure_days > 0,
           !is.na(!!sym(afr_var)), !is.na(lwage), !is.na(age),
           !is.na(height_cm), !is.na(contract_years), !is.na(married),
           !is.na(event_type)) %>%
    mutate(across(c(weight_kg, can_ride, can_shoot),
                  ~ replace_na(.x, median(.x, na.rm = TRUE))))

  if (nrow(d) < 50) return(tibble(event = character(), coef = numeric(), se = numeric(), n = integer()))

  # Build covariate matrix
  fml <- as.formula(paste0("~ lwage + ", afr_var, " + age + height_cm + weight_kg + married + contract_years"))
  covs <- model.matrix(fml, data = d)[, -1]

  results <- map_dfr(1:3, function(fc) {
    m <- tryCatch(
      crr(ftime = d$tenure_days, fstatus = d$event_type,
          cov1 = covs, failcode = fc, cencode = 0),
      error = function(e) NULL
    )
    if (is.null(m)) return(tibble(event = NA_character_, coef = NA_real_, se = NA_real_, n = NA_integer_))

    event_lab <- c("Dismissed", "Voluntary", "TimeExpired")[fc]
    afr_idx <- which(colnames(covs) == afr_var)
    tibble(
      event = event_lab,
      coef  = m$coef[afr_idx],
      se    = sqrt(diag(m$var))[afr_idx],
      HR    = exp(m$coef[afr_idx]),
      n     = nrow(d)
    )
  })
  results
}

# ── Table A1: Alternative Afrikaans definitions (survival) ───────────────

cat("\n=== Table A1: Alternative Afrikaans Definitions (Survival) ===\n")

alt_defs <- list(
  "Reviewed baseline"      = "afrikaans",
  "Original baseline"      = "afrikaans_baseline",
  "Surname match"         = "afrikaans_strict",
  "Surname + Region"      = "afrikaans_region",
  "Surname OR Region"     = "afrikaans_broad"
)

tableA1 <- map_dfr(names(alt_defs), function(def_name) {
  fit_crr_summary(sac, afr_var = alt_defs[[def_name]]) %>%
    mutate(Definition = def_name)
})

cat("\nTable A1:\n")
print(as.data.frame(tableA1 %>% select(Definition, event, coef, se, HR, n) %>%
                      mutate(across(c(coef, se, HR), ~ round(.x, 4)))))

write.csv(tableA1, file.path(tab_dir, "tableA1_alt_definitions_survival.csv"), row.names = FALSE)

# ── Assignment regressions across alternative Afrikaans definitions ──────
# The reviewed baseline uses contextual information (spelling variants,
# manual disambiguation, region) that may exceed what an SAC recruiter
# could see at the moment of assignment. Discrimination operates on
# perceived identity, so the right empirical object is bracketed by
# definitions ranging from the most contextual (reviewed baseline) to
# the most surname-only (employer-observable). The block below
# re-estimates the headline class and contract-length regressions
# under each definition and writes the Afrikaans coefficient with its
# clustered standard error. Used in §7.2.

assignment_alt <- map_dfr(names(alt_defs), function(def_name) {
  vname <- alt_defs[[def_name]]
  d <- sac %>%
    filter(!is.na(.data[[vname]])) %>%
    mutate(afr_alt = .data[[vname]],
           enlist_year_f = factor(enlist_year))

  cls <- tryCatch(
    feols(class ~ afr_alt + age + height_cm + weight_kg + married +
            farmer + prior_army | enlist_year_f,
          data = d, vcov = main_fixest_vcov, notes = FALSE),
    error = function(e) NULL
  )
  con <- tryCatch(
    feols(contract_years ~ afr_alt + age + height_cm + weight_kg +
            married + farmer + prior_army | enlist_year_f,
          data = d, vcov = main_fixest_vcov, notes = FALSE),
    error = function(e) NULL
  )

  tibble(
    Definition       = def_name,
    n_treated        = sum(d$afr_alt == 1, na.rm = TRUE),
    class_coef       = if (!is.null(cls)) coef(cls)["afr_alt"] else NA_real_,
    class_se_clust   = if (!is.null(cls)) sqrt(vcov(cls)["afr_alt", "afr_alt"]) else NA_real_,
    class_n          = if (!is.null(cls)) cls$nobs else NA_integer_,
    contract_coef    = if (!is.null(con)) coef(con)["afr_alt"] else NA_real_,
    contract_se_clust = if (!is.null(con)) sqrt(vcov(con)["afr_alt", "afr_alt"]) else NA_real_,
    contract_n       = if (!is.null(con)) con$nobs else NA_integer_
  )
})

cat("\nAssignment-margin coefficients across Afrikaans definitions:\n")
print(as.data.frame(assignment_alt %>%
        mutate(across(c(class_coef, class_se_clust, contract_coef, contract_se_clust),
                      ~ round(.x, 4)))))

write.csv(assignment_alt,
          file.path(tab_dir, "assignment_alt_definitions.csv"),
          row.names = FALSE)

# ── Table A2: Sample restrictions (survival) ─────────────────────────────

cat("\n=== Table A2: Sample Restrictions (Survival) ===\n")

sample_defs <- list(
  "Full sample"       = sac,
  "First enlistment"  = sac %>% filter(first_enlistment == 1),
  "3/c Troopers"      = sac %>% filter(title_clean == "3/c Trooper"),
  "3-year contracts"  = sac %>% filter(contract_years == 3),
  "1901-1903 window"  = sac %>% filter(enlist_year >= 1901, enlist_year <= 1903),
  "Complete cases"    = sac %>% filter(!is.na(tenure_days), !is.na(discharge_cause_clean))
)

tableA2 <- map_dfr(names(sample_defs), function(s_name) {
  fit_crr_summary(sample_defs[[s_name]], afr_var = "afrikaans") %>%
    mutate(Sample = s_name)
})

cat("\nTable A2:\n")
print(as.data.frame(tableA2 %>% select(Sample, event, coef, se, HR, n) %>%
                      mutate(across(c(coef, se, HR), ~ round(.x, 4)))))

write.csv(tableA2, file.path(tab_dir, "tableA2_sample_restrictions_survival.csv"), row.names = FALSE)

# ── Table A3: Alternative survival specifications ────────────────────────

cat("\n=== Table A3: Alternative Survival Specifications ===\n")

surv_reg_data <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans),
         !is.na(lwage), !is.na(age), !is.na(height_cm), !is.na(married)) %>%
  mutate(event = as.integer(discharged == 1))

# Cox PH
cox_model <- coxph(
  Surv(tenure_days, event) ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + factor(contract_years) + factor(rank_cat) + cluster(id),
  data = surv_reg_data
)

ph_test <- cox.zph(cox_model)
cat("\nCox PH — key coefficients:\n")
cat("  lwage:     HR =", round(exp(coef(cox_model)["lwage"]), 3),
    " (p =", round(summary(cox_model)$coefficients["lwage", "Pr(>|z|)"], 4), ")\n")
cat("  afrikaans: HR =", round(exp(coef(cox_model)["afrikaans"]), 3),
    " (p =", round(summary(cox_model)$coefficients["afrikaans", "Pr(>|z|)"], 4), ")\n")
cat("  PH global test p =", round(ph_test$table["GLOBAL", "p"], 4), "\n")

# Parametric AFT
aft_weibull <- survreg(
  Surv(tenure_days, event) ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + factor(contract_years) + factor(rank_cat),
  data = surv_reg_data, dist = "weibull"
)

aft_lognormal <- survreg(
  Surv(tenure_days, event) ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + factor(contract_years) + factor(rank_cat),
  data = surv_reg_data, dist = "lognormal"
)

aft_loglogistic <- survreg(
  Surv(tenure_days, event) ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + factor(contract_years) + factor(rank_cat),
  data = surv_reg_data, dist = "loglogistic"
)

# Frailty model (shared frailty by enlistment location)
frailty_model <- coxph(
  Surv(tenure_days, event) ~ lwage + afrikaans + age + height_cm + weight_kg +
    married + can_ride + can_shoot + factor(contract_years) + factor(rank_cat) +
    frailty(signed_at),
  data = surv_reg_data %>% filter(!is.na(signed_at))
)

# Collect key coefficients
tableA3 <- data.frame(
  Variable = c("lwage", "afrikaans"),
  Cox_coef = round(coef(cox_model)[c("lwage", "afrikaans")], 4),
  Cox_se   = round(sqrt(diag(vcov(cox_model)))[c("lwage", "afrikaans")], 4),
  Weibull_coef    = round(coef(aft_weibull)[c("lwage", "afrikaans")], 4),
  Weibull_se      = round(summary(aft_weibull)$table[c("lwage", "afrikaans"), "Std. Error"], 4),
  LogNormal_coef  = round(coef(aft_lognormal)[c("lwage", "afrikaans")], 4),
  LogNormal_se    = round(summary(aft_lognormal)$table[c("lwage", "afrikaans"), "Std. Error"], 4),
  LogLogistic_coef = round(coef(aft_loglogistic)[c("lwage", "afrikaans")], 4),
  LogLogistic_se   = round(summary(aft_loglogistic)$table[c("lwage", "afrikaans"), "Std. Error"], 4)
)

cat("\nTable A3: Alternative Survival Specifications\n")
print(tableA3)

write.csv(tableA3, file.path(tab_dir, "tableA3_alt_survival_specs.csv"), row.names = FALSE)
saveRDS(list(cox = cox_model, weibull = aft_weibull, lognormal = aft_lognormal,
             loglogistic = aft_loglogistic, frailty = frailty_model, ph_test = ph_test),
        file.path(mod_dir, "robustness_survival_models.rds"))

# ── Figure A2: Subgroup forest plot ──────────────────────────────────────

cat("\n=== Figure A2: Subgroup Forest Plot ===\n")

subgroup_est <- function(data, subgroup_var, subgroup_val, label) {
  d <- data %>% filter(!!sym(subgroup_var) == subgroup_val)
  m <- tryCatch(
    suppressWarnings(feols(lwage ~ afrikaans + age + height_cm + weight_kg + married +
            i(class_f) + i(contract_f) | title_f + enlist_ym_f,
          data = d, vcov = "hetero", notes = FALSE)),
    error = function(e) NULL
  )
  if (!is.null(m) && "afrikaans" %in% names(coef(m))) {
    cf <- coeftable(m)
    tibble(subgroup = label,
           estimate = cf["afrikaans", "Estimate"],
           se       = cf["afrikaans", "Std. Error"],
           ci_lo    = estimate - 1.96 * se,
           ci_hi    = estimate + 1.96 * se,
           n        = m$nobs)
  } else {
    tibble(subgroup = label, estimate = NA_real_, se = NA_real_,
           ci_lo = NA_real_, ci_hi = NA_real_, n = NA_integer_)
  }
}

subgroups <- bind_rows(
  subgroup_est(reg_data, "rank_cat", "Trooper", "Troopers"),
  subgroup_est(reg_data, "rank_cat", "Constable", "Constables"),
  subgroup_est(reg_data, "rank_cat", "NCO", "NCOs"),
  subgroup_est(reg_data, "post_vereeniging", 1, "Post-Treaty"),
  subgroup_est(reg_data, "post_vereeniging", 0, "Pre-Treaty"),
  subgroup_est(reg_data, "contract_years", 3, "3-year contract"),
  subgroup_est(reg_data, "contract_years", 2, "2-year contract"),
  subgroup_est(reg_data, "region", "England", "English"),
  subgroup_est(reg_data, "region", "Scotland", "Scottish"),
  subgroup_est(reg_data, "region", "Ireland", "Irish"),
  subgroup_est(reg_data, "class", 1, "Class 1"),
  subgroup_est(reg_data, "class", 2, "Class 2"),
  subgroup_est(reg_data, "class", 3, "Class 3")
)

figA2 <- subgroups %>%
  filter(!is.na(estimate)) %>%
  mutate(subgroup = fct_rev(fct_inorder(subgroup))) %>%
  ggplot(aes(x = estimate, y = subgroup)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), size = 0.4) +
  geom_text(aes(label = paste0("N=", comma(n))),
            hjust = -0.3, size = 2.5, colour = "grey50") +
  labs(x = "Coefficient on Afrikaans",
       y = NULL) +
  theme(axis.text.y = element_text(size = 9))

save_leap_fig(file.path(fig_dir, "figA2_subgroup_forest.png"), figA2, width = 10, height = 6)
cat("  Saved Figure A2.\n")


###########################################################################
#
#   SECTION 7 — LANGUAGE ABILITY: IDENTITY VS. SKILL
#
#   Three analyses exploiting Dutch and African language ability:
#   (1) "Same skill, different identity" — assignment test
#   (2) Reclassification robustness — are Dutch-speaking non-Afrikaners
#       "hidden Afrikaners"?
#   (3) African language ability as a valued-local-knowledge control
#
###########################################################################

cat("\n")
cat("======================================================================\n")
cat("SECTION 7: LANGUAGE ABILITY ANALYSIS\n")
cat("======================================================================\n\n")

# ── 7.0 Descriptive statistics by language group ─────────────────────────

cat("=== 7.0 Descriptive Statistics by Language Group ===\n")

lang_desc <- sac %>%
  filter(!is.na(afrikaans)) %>%
  mutate(
    lang_group = case_when(
      afrikaans == 1                                   ~ "Afrikaans",
      speaks_dutch == 1 & speaks_african_lang == 1     ~ "Non-Afr: Dutch + African",
      speaks_dutch == 1                                ~ "Non-Afr: Dutch only",
      speaks_african_lang == 1                         ~ "Non-Afr: African lang only",
      TRUE                                             ~ "Non-Afr: neither"
    )
  )

lang_summary <- lang_desc %>%
  group_by(lang_group) %>%
  summarise(
    N = n(),
    mean_wage       = mean(wage, na.rm = TRUE),
    mean_class      = mean(class, na.rm = TRUE),
    mean_contract   = mean(contract_years, na.rm = TRUE),
    mean_tenure     = mean(tenure_days, na.rm = TRUE),
    mean_character  = mean(character_score, na.rm = TRUE),
    pct_dismissed   = mean(dismissed_deserted, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nLanguage group descriptives:\n")
print(as.data.frame(lang_summary %>%
        mutate(across(where(is.numeric) & !matches("^N$"), ~ round(.x, 3)))))

write.csv(lang_summary, file.path(tab_dir, "language_group_descriptives.csv"),
          row.names = FALSE)


# ══════════════════════════════════════════════════════════════════════════
# 7.1 PROPOSAL 1: "Same Skill, Different Identity" — Assignment Test
#
# Dutch ability should be valued for policing the Transvaal. If the SAC
# rewarded Dutch speakers but penalised Afrikaners (who all spoke Dutch),
# the discrimination targeted identity, not linguistic skill.
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 7.1 Same Skill, Different Identity ===\n")

# Prepare data with language variables
lang_reg <- sac %>%
  filter(!is.na(afrikaans), !is.na(speaks_dutch)) %>%
  mutate(
    afr_x_dutch    = afrikaans * speaks_dutch,
    class_f        = factor(class),
    contract_f     = factor(contract_years),
    region_f       = factor(region),
    title_f        = factor(title_clean),
    enlist_ym_f    = factor(enlist_ym),
    enlist_year_f  = factor(enlist_year)
  )

cat("  N with language data:", nrow(lang_reg), "\n")
cat("  speaks_dutch = 1:", sum(lang_reg$speaks_dutch == 1), "\n")
cat("  afrikaans = 1:",    sum(lang_reg$afrikaans == 1), "\n")
cat("  Both:",             sum(lang_reg$afrikaans == 1 & lang_reg$speaks_dutch == 1), "\n")

# --- 7.1a Class assignment: does Dutch ability predict higher class? ---

cat("\n  --- Class assignment ---\n")

class_lang_base <- suppressWarnings(feols(
  class ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(class)),
  vcov = main_fixest_vcov, notes = FALSE
))

class_lang_dutch <- suppressWarnings(feols(
  class ~ afrikaans + speaks_dutch + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(class)),
  vcov = main_fixest_vcov, notes = FALSE
))

class_lang_interact <- suppressWarnings(feols(
  class ~ afrikaans * speaks_dutch + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(class)),
  vcov = main_fixest_vcov, notes = FALSE
))

class_lang_african <- suppressWarnings(feols(
  class ~ afrikaans + speaks_dutch + speaks_african_lang +
    age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(class)),
  vcov = main_fixest_vcov, notes = FALSE
))

cat("\nClass assignment with language controls:\n")
etable(class_lang_base, class_lang_dutch, class_lang_interact, class_lang_african,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2,
       headers = c("Baseline", "+ Dutch", "Afr x Dutch", "+ African lang"))

etable(class_lang_base, class_lang_dutch, class_lang_interact, class_lang_african,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2,
       tex = TRUE, file = file.path(tab_dir, "table_lang_class_assignment.tex"),
       replace = TRUE,
       title = "Class assignment with language controls")

# --- 7.1b Contract length: same pattern? ---

cat("\n  --- Contract length ---\n")

contract_lang_base <- suppressWarnings(feols(
  contract_years ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(contract_years)),
  vcov = main_fixest_vcov, notes = FALSE
))

contract_lang_dutch <- suppressWarnings(feols(
  contract_years ~ afrikaans + speaks_dutch + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(contract_years)),
  vcov = main_fixest_vcov, notes = FALSE
))

contract_lang_interact <- suppressWarnings(feols(
  contract_years ~ afrikaans * speaks_dutch + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(contract_years)),
  vcov = main_fixest_vcov, notes = FALSE
))

contract_lang_african <- suppressWarnings(feols(
  contract_years ~ afrikaans + speaks_dutch + speaks_african_lang +
    age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(contract_years)),
  vcov = main_fixest_vcov, notes = FALSE
))

cat("\nContract length with language controls:\n")
etable(contract_lang_base, contract_lang_dutch, contract_lang_interact, contract_lang_african,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2,
       headers = c("Baseline", "+ Dutch", "Afr x Dutch", "+ African lang"))

etable(contract_lang_base, contract_lang_dutch, contract_lang_interact, contract_lang_african,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2,
       tex = TRUE, file = file.path(tab_dir, "table_lang_contract_assignment.tex"),
       replace = TRUE,
       title = "Contract assignment with language controls")

# --- 7.1c Wage regression: Dutch ability in the wage equation ---

cat("\n  --- Wage regressions ---\n")

wage_lang_data <- lang_reg %>% filter(!is.na(lwage))

wage_lang_base <- suppressWarnings(feols(
  lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = wage_lang_data, vcov = main_fixest_vcov, notes = FALSE
))

wage_lang_dutch <- suppressWarnings(feols(
  lwage ~ afrikaans + speaks_dutch + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = wage_lang_data, vcov = main_fixest_vcov, notes = FALSE
))

wage_lang_interact <- suppressWarnings(feols(
  lwage ~ afrikaans * speaks_dutch + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = wage_lang_data, vcov = main_fixest_vcov, notes = FALSE
))

wage_lang_all <- suppressWarnings(feols(
  lwage ~ afrikaans + speaks_dutch + speaks_african_lang +
    age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = wage_lang_data, vcov = main_fixest_vcov, notes = FALSE
))

cat("\nWage regressions with language controls:\n")
etable(wage_lang_base, wage_lang_dutch, wage_lang_interact, wage_lang_all,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2 + wr2,
       headers = c("Baseline", "+ Dutch", "Afr x Dutch", "+ All lang"))

etable(wage_lang_base, wage_lang_dutch, wage_lang_interact, wage_lang_all,
       keep = c("%afrikaans", "%speaks_dutch", "%speaks_african_lang",
                "%afrikaans:speaks_dutch"),
       se.below = TRUE, fitstat = ~ n + r2 + wr2,
       tex = TRUE, file = file.path(tab_dir, "table_lang_wage_regs.tex"),
       replace = TRUE,
       title = "Wage regressions with language controls")


# ══════════════════════════════════════════════════════════════════════════
# 7.2 PROPOSAL 2: Reclassification Robustness
#
# (a) Exclude Dutch-speaking non-Afrikaners (drop potential misclassification)
# (b) Reclassify Dutch-speaking non-Afrikaners AS Afrikaans (over-inclusive)
# Both should confirm that institutional treatment tracked identity, not language.
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 7.2 Reclassification Robustness ===\n")

# (a) Drop Dutch-speaking non-Afrikaners
reclass_drop <- lang_reg %>%
  filter(!(afrikaans == 0 & speaks_dutch == 1)) %>%
  filter(!is.na(lwage))

cat("  (a) Dropping Dutch-speaking non-Afrikaners: N =", nrow(reclass_drop),
    " (dropped", nrow(wage_lang_data) - nrow(reclass_drop), ")\n")

wage_reclass_drop <- suppressWarnings(feols(
  lwage ~ afrikaans + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = reclass_drop, vcov = "hetero", notes = FALSE
))

# (b) Reclassify Dutch speakers as Afrikaans
reclass_broad <- wage_lang_data %>%
  mutate(afrikaans_broad_lang = as.integer(afrikaans == 1 | speaks_dutch == 1))

n_broad <- sum(reclass_broad$afrikaans_broad_lang == 1)
cat("  (b) Broad definition (Afrikaans OR speaks Dutch): N_treated =", n_broad,
    " (vs baseline", sum(wage_lang_data$afrikaans == 1), ")\n")

wage_reclass_broad <- suppressWarnings(feols(
  lwage ~ afrikaans_broad_lang + age + height_cm + weight_kg + married +
    can_ride + can_shoot + can_swim + prior_army +
    i(class_f) + i(contract_f) + i(region_f) | title_f + enlist_ym_f,
  data = reclass_broad, vcov = "hetero", notes = FALSE
))

# Compare: baseline, drop Dutch non-Afr, broad definition
cat("\nReclassification robustness (wage regressions):\n")
etable(wage_lang_base, wage_reclass_drop, wage_reclass_broad,
       keep = c("%afrikaans", "%afrikaans_broad_lang"),
       se.below = TRUE, fitstat = ~ n + r2 + wr2,
       headers = c("Baseline", "Drop Dutch non-Afr", "Broad (Afr OR Dutch)"))

# Same reclassification for class assignment
class_reclass_drop <- suppressWarnings(feols(
  class ~ afrikaans + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>% filter(!is.na(class), !(afrikaans == 0 & speaks_dutch == 1)),
  vcov = "hetero", notes = FALSE
))

class_reclass_broad <- suppressWarnings(feols(
  class ~ afrikaans_broad_lang + age + height_cm + weight_kg + married +
    farmer + prior_army | enlist_year_f,
  data = lang_reg %>%
    filter(!is.na(class)) %>%
    mutate(afrikaans_broad_lang = as.integer(afrikaans == 1 | speaks_dutch == 1)),
  vcov = "hetero", notes = FALSE
))

cat("\nReclassification robustness (class assignment):\n")
etable(class_lang_base, class_reclass_drop, class_reclass_broad,
       keep = c("%afrikaans", "%afrikaans_broad_lang"),
       se.below = TRUE, fitstat = ~ n + r2,
       headers = c("Baseline", "Drop Dutch non-Afr", "Broad (Afr OR Dutch)"))

# Same reclassification for survival (competing risks)
cat("\nReclassification robustness (competing risks):\n")

reclass_surv_baseline <- fit_crr_summary(sac, afr_var = "afrikaans")

sac_reclass_drop <- sac %>%
  filter(!(afrikaans == 0 & speaks_dutch == 1))
reclass_surv_drop <- fit_crr_summary(sac_reclass_drop, afr_var = "afrikaans")

sac_reclass_broad <- sac %>%
  mutate(afrikaans_broad_lang = as.integer(afrikaans == 1 | speaks_dutch == 1))
reclass_surv_broad <- fit_crr_summary(sac_reclass_broad, afr_var = "afrikaans_broad_lang")

reclass_surv_table <- bind_rows(
  reclass_surv_baseline %>% mutate(Definition = "Baseline"),
  reclass_surv_drop     %>% mutate(Definition = "Drop Dutch non-Afr"),
  reclass_surv_broad    %>% mutate(Definition = "Broad (Afr OR Dutch)")
)

cat("\nReclassification — competing risks:\n")
print(as.data.frame(reclass_surv_table %>%
        select(Definition, event, coef, se, HR, n) %>%
        mutate(across(c(coef, se, HR), ~ round(.x, 4)))))

write.csv(reclass_surv_table,
          file.path(tab_dir, "table_lang_reclassification_survival.csv"),
          row.names = FALSE)


# ══════════════════════════════════════════════════════════════════════════
# 7.3 PROPOSAL 3: African Language Ability as Control
#
# Non-Afrikaners who spoke African languages had direct operational value.
# Adding speaks_african_lang to assignment and survival regressions tests
# whether the SAC valued local knowledge and whether the Afrikaner penalty
# persists after controlling for it.
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 7.3 African Language Ability as Control ===\n")

# 7.3a: Assignment regressions with speaks_african_lang
# (Class results already shown in 7.1 — summarise key finding here)

cat("\n  Class assignment: speaks_african_lang coefficient from Spec (4) above:\n")
cf_al <- coeftable(class_lang_african)
if ("speaks_african_lang" %in% rownames(cf_al)) {
  cat("    speaks_african_lang: coef =",
      round(cf_al["speaks_african_lang", "Estimate"], 4),
      " (se =", round(cf_al["speaks_african_lang", "Std. Error"], 4),
      ", p =", round(cf_al["speaks_african_lang", "Pr(>|t|)"], 4), ")\n")
}
cat("    afrikaans coefficient unchanged from baseline? =",
    round(coeftable(class_lang_base)["afrikaans", "Estimate"], 4), "->",
    round(cf_al["afrikaans", "Estimate"], 4), "\n")

# 7.3b: Survival regressions with language controls
cat("\n  Competing risks with language controls:\n")

surv_lang_data <- sac %>%
  filter(!is.na(tenure_days), tenure_days > 0, !is.na(afrikaans),
         !is.na(lwage), !is.na(age), !is.na(height_cm),
         !is.na(contract_years), !is.na(married),
         !is.na(speaks_dutch), !is.na(speaks_african_lang),
         !is.na(event_type)) %>%
  mutate(across(c(weight_kg, can_ride, can_shoot),
                ~ replace_na(.x, median(.x, na.rm = TRUE))))

# Baseline covariates
covs_base <- model.matrix(
  ~ lwage + afrikaans + age + height_cm + weight_kg + married + contract_years,
  data = surv_lang_data
)[, -1]

# With language controls
covs_lang <- model.matrix(
  ~ lwage + afrikaans + speaks_dutch + speaks_african_lang +
    age + height_cm + weight_kg + married + contract_years,
  data = surv_lang_data
)[, -1]

crr_lang_results <- map_dfr(1:3, function(fc) {
  event_lab <- c("Dismissed", "Voluntary", "TimeExpired")[fc]

  m_base <- tryCatch(
    crr(ftime = surv_lang_data$tenure_days, fstatus = surv_lang_data$event_type,
        cov1 = covs_base, failcode = fc, cencode = 0),
    error = function(e) NULL
  )

  m_lang <- tryCatch(
    crr(ftime = surv_lang_data$tenure_days, fstatus = surv_lang_data$event_type,
        cov1 = covs_lang, failcode = fc, cencode = 0),
    error = function(e) NULL
  )

  rows <- tibble()
  if (!is.null(m_base)) {
    rows <- bind_rows(rows, tibble(
      model = "Baseline", event = event_lab,
      variable = colnames(covs_base),
      coef = m_base$coef, se = sqrt(diag(m_base$var)),
      HR = exp(m_base$coef), n = nrow(surv_lang_data)
    ))
  }
  if (!is.null(m_lang)) {
    rows <- bind_rows(rows, tibble(
      model = "With language", event = event_lab,
      variable = colnames(covs_lang),
      coef = m_lang$coef, se = sqrt(diag(m_lang$var)),
      HR = exp(m_lang$coef), n = nrow(surv_lang_data)
    ))
  }
  rows
})

# Print key variables
key_vars_lang <- c("afrikaans", "speaks_dutch", "speaks_african_lang")
crr_lang_key <- crr_lang_results %>%
  filter(variable %in% key_vars_lang)

cat("\nCompeting risks — key language variables:\n")
print(as.data.frame(crr_lang_key %>%
        select(model, event, variable, coef, se, HR) %>%
        mutate(across(c(coef, se, HR), ~ round(.x, 4)))))

write.csv(crr_lang_results,
          file.path(tab_dir, "table_lang_competing_risks.csv"),
          row.names = FALSE)

# 7.3c: Employer learning with language controls
cat("\n  Employer learning with language controls:\n")

learn_lang_data <- surv_lang_data %>%
  mutate(dismiss_event = as.integer(event_type == 1))

learn_lang_dismiss <- map_dfr(seq_along(period_labels), function(i) {
  t_start <- periods[i]
  t_end   <- periods[i + 1]

  d <- learn_lang_data %>%
    filter(tenure_days > t_start) %>%
    mutate(
      t1  = t_start,
      t2  = pmin(tenure_days, t_end),
      evt = as.integer(dismiss_event == 1 & tenure_days <= t_end)
    )

  n_afr_events <- sum(d$evt[d$afrikaans == 1])
  if (sum(d$evt) < 10 || n_afr_events < 3) {
    return(tibble(period = period_labels[i],
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n = nrow(d)))
  }

  m <- tryCatch(
    coxph(Surv(t1, t2, evt) ~ afrikaans + speaks_dutch + speaks_african_lang +
            lwage + age + height_cm + married + factor(contract_years) + cluster(id),
          data = d),
    error = function(e) NULL
  )

  if (is.null(m) || !"afrikaans" %in% names(coef(m))) {
    return(tibble(period = period_labels[i],
                  HR = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  n_events = sum(d$evt), n = nrow(d)))
  }

  ci <- exp(confint(m)["afrikaans", ])
  tibble(
    period   = period_labels[i],
    HR       = exp(coef(m)["afrikaans"]),
    ci_lo    = ci[1],
    ci_hi    = ci[2],
    n_events = sum(d$evt),
    n        = nrow(d)
  )
})

cat("\nEmployer learning (dismissal) with language controls:\n")
print(as.data.frame(learn_lang_dismiss %>%
        mutate(across(c(HR, ci_lo, ci_hi), ~ round(.x, 3)))))

cat("\nCompare to baseline (without language controls):\n")
print(as.data.frame(learn_dismiss %>%
        filter(!is.na(HR)) %>%
        select(period, HR_baseline = HR) %>%
        left_join(
          learn_lang_dismiss %>% select(period, HR_with_lang = HR),
          by = "period"
        ) %>%
        mutate(across(starts_with("HR"), ~ round(.x, 3)))))


# ── Combined regression table for the paper (Table 4) ──────────────────
# Shows class, contract, and wage regressions side by side,
# with and without language controls.

cat("\n  Generating combined regression table for paper...\n")

# Helper: extract formatted coefficient and SE from a fixest model
.fmt_coef <- function(mod, varname) {
  cf <- coeftable(mod)
  if (!(varname %in% rownames(cf))) return(list(est = "", se = ""))
  est    <- cf[varname, "Estimate"]
  se_val <- cf[varname, "Std. Error"]
  p_val  <- cf[varname, "Pr(>|t|)"]
  star <- if (p_val < 0.01) "$^{***}$" else if (p_val < 0.05) "$^{**}$" else if (p_val < 0.10) "$^{*}$" else ""
  est_str <- if (est < 0) {
    paste0("$-$", sprintf("%.3f", abs(est)), star)
  } else {
    paste0(sprintf("%.3f", est), star)
  }
  se_str <- paste0("(", sprintf("%.3f", se_val), ")")
  list(est = est_str, se = se_str)
}

# Six models: class (base, +lang), contract (base, +lang), wage (base, +lang)
tab_mods <- list(class_lang_base, class_lang_african,
                 contract_lang_base, contract_lang_african,
                 wage_lang_base, wage_lang_all)

# Build coefficient + SE row pair
.build_rows <- function(varname, label) {
  fmts <- lapply(tab_mods, function(m) .fmt_coef(m, varname))
  est_cells <- sapply(fmts, `[[`, "est")
  se_cells  <- sapply(fmts, `[[`, "se")
  c(
    paste0(label, " & ", paste(est_cells, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se_cells, collapse = " & "), " \\\\[6pt]")
  )
}

rows_afr     <- .build_rows("afrikaans", "Afrikaans")
rows_dutch   <- .build_rows("speaks_dutch", "Speaks Dutch")
rows_afrlang <- .build_rows("speaks_african_lang", "Speaks African lang.")

# N and R-squared
n_vals  <- sapply(tab_mods, function(m) format(nobs(m), big.mark = ","))
r2_vals <- sapply(tab_mods, function(m) {
  tryCatch(sprintf("%.3f", r2(m, "r2")), error = function(e) "---")
})
n_row  <- paste0("Observations & ", paste(n_vals, collapse = " & "), " \\\\")
r2_row <- paste0("$R^2$ & ", paste(r2_vals, collapse = " & "), " \\\\")

# Assemble LaTeX
tex4 <- c(
  "\\begin{threeparttable}",
  "\\small",
  "\\begin{tabular}{@{}lcccccc@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Class (1--3)} & \\multicolumn{2}{c}{Contract (years)} & \\multicolumn{2}{c}{Log wage} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  rows_afr, rows_dutch, rows_afrlang,
  "\\midrule",
  "Demographics & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Background & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes & Yes & & \\\\",
  "Rank $+$ year-month FE & & & & & Yes & Yes \\\\",
  "Assignment controls & & & & & Yes & Yes \\\\[3pt]",
  n_row,
  r2_row,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes}: OLS. Columns (1)--(2): class (1--3, higher $=$ better position). Columns (3)--(4): contract length in years. Columns (5)--(6): log daily wage in shillings. Demographics: age, height, weight, marital status. Background: farmer, prior army. Assignment controls (columns 5--6): class, contract length, region. ``Speaks Dutch'' and ``Speaks African lang.'' are binary indicators constructed from the free-text language fields in the enlistment registers. Standard errors are clustered by enlistment year-month. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

writeLines(tex4, file.path(tab_dir, "table_language_robustness.tex"))
cat("  Saved:", file.path(tab_dir, "table_language_robustness.tex"), "\n")


# ── Save all language analysis models ────────────────────────────────────

saveRDS(list(
  # Proposal 1: Assignment
  class_base = class_lang_base, class_dutch = class_lang_dutch,
  class_interact = class_lang_interact, class_african = class_lang_african,
  contract_base = contract_lang_base, contract_dutch = contract_lang_dutch,
  contract_interact = contract_lang_interact, contract_african = contract_lang_african,
  wage_base = wage_lang_base, wage_dutch = wage_lang_dutch,
  wage_interact = wage_lang_interact, wage_all = wage_lang_all,
  # Proposal 2: Reclassification
  wage_drop = wage_reclass_drop, wage_broad = wage_reclass_broad,
  class_drop = class_reclass_drop, class_broad = class_reclass_broad
), file.path(mod_dir, "language_analysis_models.rds"))

cat("\n  Saved language analysis models and tables.\n")

cat("\n")
cat("======================================================================\n")
cat("SECTION 7 COMPLETE\n")
cat("======================================================================\n\n")


###########################################################################
#
#   OUTPUT SUMMARY
#
###########################################################################

cat("\n")
cat("======================================================================\n")
cat("PAPER 2 ANALYSIS COMPLETE\n")
cat("======================================================================\n\n")

cat("Main tables:\n")
cat("  Table 1: ", file.path(tab_dir, "table1_summary_stats.csv"), "\n")
cat("  Table 2: ", file.path(tab_dir, "table2_did_means.csv"), "\n")
cat("  Table 3: ", file.path(tab_dir, "table3_wage_regs.tex"), "\n")
cat("  Table 4: ", file.path(tab_dir, "table4_competing_risks.csv"), "\n")
cat("  Table 5: ", file.path(tab_dir, "table5_rank_assignment.tex"), "\n")

cat("\nMain figures:\n")
for (f in sort(list.files(fig_dir, pattern = "^fig[0-9].*\\.pdf$"))) {
  cat("  ", f, "\n")
}

cat("\nAppendix tables:\n")
for (f in sort(list.files(tab_dir, pattern = "^tableA"))) {
  cat("  ", f, "\n")
}

cat("\nAppendix / extension figures:\n")
for (f in sort(list.files(fig_dir, pattern = "^(figA|fig_ext|fig1[1-2]).*\\.pdf$"))) {
  cat("  ", f, "\n")
}

cat("\nModel objects:\n")
for (f in sort(list.files(mod_dir))) {
  cat("  ", f, "\n")
}

cat("\nKey numbers for text:\n")
cat("  Permutation p-value:      ", round(perm_p, 4), "\n")
cat("  Ordered probit — lwage:    coef =", round(op_key["lwage", "Value"], 3),
    ", t =", round(op_key["lwage", "t value"], 2), "\n")
cat("  Ordered probit — afrikaans: coef =", round(op_key["afrikaans", "Value"], 3),
    ", t =", round(op_key["afrikaans", "t value"], 2), "\n")
cat("  Cost estimate:             ", comma(round(total_cost, 0)), "shillings\n")
cat("  PH global test p-value:   ", round(ph_test$table["GLOBAL", "p"], 4), "\n")

# New analysis key numbers
cat("\nOver-sorting:\n")
cat("  Afrikaans actual 3/c Trooper rate:    ", round(mean(afr_pred$is_trooper_3c), 3), "\n")
cat("  Afrikaans predicted (from observables):", round(mean(afr_pred$predicted_3c), 3), "\n")
cat("  Over-sorting gap:                      ", round(mean(afr_pred$is_trooper_3c) - mean(afr_pred$predicted_3c), 3), "\n")
cat("  Bootstrap 95% CI:                      [", round(oversort_ci[1], 3), ",", round(oversort_ci[2], 3), "]\n")

cat("\nWithin-rank character (3/c Troopers):\n")
cat("  Afr mean:", round(char_ttest$estimate[2], 2),
    ", Non-Afr mean:", round(char_ttest$estimate[1], 2),
    ", p =", round(char_ttest$p.value, 4), "\n")

cat("\nEmployer learning results (Afrikaans HR by period):\n")
print(as.data.frame(learn_results %>%
        select(period, outcome, HR) %>%
        mutate(HR = round(HR, 3)) %>%
        pivot_wider(names_from = outcome, values_from = HR)))

cat("\nExtension results:\n")
cat("  Ext 1 (Dynamic selection): See ext1_survivor_selection.csv\n")
cat("  Ext 2 (Oaxaca-Blinder):    See ext2_oaxaca_decomposition.csv\n")
cat("  Ext 3 (Character bias):    See character_bias_models.rds\n")
cat("  Ext 5 (Contract bunching): See ext5_contract_bunching.csv\n")

cat("\nLanguage analysis (Section 7):\n")
cat("  TABLE 4 (paper):              table_language_robustness.tex\n")
cat("  Language group descriptives:   language_group_descriptives.csv\n")
cat("  Class w/ language controls:    table_lang_class_assignment.tex\n")
cat("  Contract w/ language controls: table_lang_contract_assignment.tex\n")
cat("  Wage w/ language controls:     table_lang_wage_regs.tex\n")
cat("  Reclassification survival:     table_lang_reclassification_survival.csv\n")
cat("  Competing risks w/ language:   table_lang_competing_risks.csv\n")
cat("  Model objects:                 language_analysis_models.rds\n")

cat("\nDone!\n")

# =============================================================================
#  08_figures_tables.R  —  BUILD THE EXHIBITS FOR THE PAPER
#
#  Figures follow the LEAP visual identity defined in 00_setup.R: the LEAP
#  palette, theme_leap(), and 600 DPI output in both PNG and PDF.
#
#  Because these figures are reduced to the text width of a journal page, we
#  raise the base font size so that axis text stays legible in print.
#
#  Figure 1  The rule works, and the decision has changed.
#            Panel A: enforcement by first-innings lead, showing the statutory
#                     discontinuity and the move of the Test line in 1980.
#            Panel B: enforcement among eligible captains by decade.
#  Figure 2  What enforcing does: outcome by distance from the cutoff.
#  Figure 3  The behavioural test: event study around a disaster.
#  Table 1   Regression discontinuity estimates.
#  Table 2   Behavioural estimates and placebos.
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)
suppressWarnings(suppressMessages({ library(ggplot2); library(scales) }))

BASE <- 15   # base font size, chosen so 10-inch figures stay legible at 6.5in

d <- readRDS(file.path(DDIR, "matches.rds")) %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive,
         !is.na(margin))
rd  <- readRDS(file.path(DDIR, "rd_estimates.rds"))
beh <- readRDS(file.path(DDIR, "behavioural.rds"))
mec <- readRDS(file.path(DDIR, "mechanisms.rds"))

# =============================================================================
#  FIGURE 1A — the statutory discontinuity, and the 1980 move in Tests
# =============================================================================
#  Three series, so we use plum, blue and gold, which stay distinct under all
#  common forms of colour vision deficiency, and vary shape and line type too.
# -----------------------------------------------------------------------------
bin_rate <- function(dat, by = 25, lo = 0, hi = 350) {
  dat %>% filter(lead >= lo, lead < hi) %>%
    mutate(bin = by * (lead %/% by) + by / 2) %>%
    group_by(grp, bin) %>%
    summarise(n = n(), rate = mean(enforced), .groups = "drop") %>%
    filter(n >= 15)
}
f1a_dat <- bind_rows(
  d %>% filter(comp != "Test")             %>% mutate(grp = "Domestic first-class"),
  d %>% filter(comp == "Test", yr < 1980)  %>% mutate(grp = "Tests before 1980"),
  d %>% filter(comp == "Test", yr >= 1980) %>% mutate(grp = "Tests from 1980")
) %>% bin_rate() %>%
  mutate(grp = factor(grp, levels = c("Domestic first-class",
                                      "Tests before 1980", "Tests from 1980")))

f1a <- ggplot(f1a_dat, aes(bin, rate, colour = grp, shape = grp, linetype = grp)) +
  geom_vline(xintercept = c(150, 200), colour = LEAP_NONSIG_COLOR,
             linewidth = 0.4) +
  annotate("text", x = 150, y = 1.0, label = "150", size = 3.6,
           hjust = -0.2, colour = "#5A5A5A") +
  annotate("text", x = 200, y = 1.0, label = "200", size = 3.6,
           hjust = -0.2, colour = "#5A5A5A") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6, fill = "white", stroke = 0.9) +
  scale_colour_manual(values = c("Domestic first-class" = "#5C2346",
                                 "Tests before 1980"    = "#3D8EB9",
                                 "Tests from 1980"      = "#D4A03E")) +
  scale_shape_manual(values = c(21, 16, 17)) +
  scale_linetype_manual(values = c("solid", "22", "dotted")) +
  scale_y_continuous(labels = percent_format(1), limits = c(0, 1.03),
                     breaks = seq(0, 1, 0.25)) +
  labs(x = "First-innings lead (runs)",
       y = "Follow-on enforced", colour = NULL, shape = NULL,
       linetype = NULL) +
  theme_leap(BASE)

# =============================================================================
#  FIGURE 1B — enforcement among eligible captains, by decade
# =============================================================================
f1b_dat <- d %>% filter(eligible == 1) %>%
  mutate(decade = 10 * (yr %/% 10),
         grp = ifelse(comp == "Test", "Test cricket", "Domestic first-class")) %>%
  group_by(grp, decade) %>%
  summarise(n = n(), rate = mean(enforced),
            se = sqrt(rate * (1 - rate) / n), .groups = "drop") %>%
  filter(n >= 25)

f1b <- ggplot(f1b_dat, aes(decade, rate, colour = grp, fill = grp,
                           shape = grp, linetype = grp)) +
  geom_ribbon(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se, group = grp),
              alpha = 0.16, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8, stroke = 0.9) +
  scale_colour_manual(values = c("Domestic first-class" = "#5C2346",
                                 "Test cricket"         = "#3D8EB9")) +
  scale_fill_manual(values = c("Domestic first-class" = "#5C2346",
                               "Test cricket"         = "#3D8EB9")) +
  scale_shape_manual(values = c(21, 16)) +
  scale_linetype_manual(values = c("solid", "22")) +
  scale_y_continuous(labels = percent_format(1), limits = c(0.2, 1)) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  labs(x = NULL, y = "Enforced, among eligible captains",
       colour = NULL, fill = NULL, shape = NULL, linetype = NULL) +
  theme_leap(BASE)

save_leap_fig("fig1a_discontinuity", f1a, width = 10, height = 6)
save_leap_fig("fig1b_decline",       f1b, width = 10, height = 6)

# =============================================================================
#  FIGURE 2 — THE DISCONTINUITY IN THE OUTCOME
# =============================================================================
#  This is the picture of the result. Each point is the average outcome for a
#  bin of matches at a given distance from the statutory margin; the lines are
#  local linear fits estimated separately on each side of it. The vertical gap
#  at zero is the effect of ELIGIBILITY, that is the reduced form. Dividing it
#  by the jump in enforcement shown in Figure 1(a) gives the effect of
#  enforcing reported in Table 1.
#
#  We use rdplot() to choose the bins and fit the polynomials, then redraw the
#  result in the LEAP style rather than using its default output.
# -----------------------------------------------------------------------------
suppressWarnings(suppressMessages(library(rdrobust)))

#  We fix the number of bins rather than letting the data choose. Too many bins
#  scatter the averages and hide the step; twenty a side, each of five runs,
#  keeps enough matches per point for the average to be stable.
rd_panel <- function(yvar, label, window = 100, nbin = 20) {
  sub <- d %>% filter(abs(dist) <= window, !is.na(.data[[yvar]]))
  rp <- rdplot(y = sub[[yvar]], x = sub$dist, c = 0, p = 1,
               nbins = c(nbin, nbin), binselect = "es",
               hide = TRUE, masspoints = "off")
  bins <- as_tibble(rp$vars_bins) %>%
    transmute(x = rdplot_mean_x, y = rdplot_mean_y,
              side = ifelse(x >= 0, "Eligible", "Not eligible"),
              outcome = label) %>% filter(is.finite(x), is.finite(y))
  poly <- as_tibble(rp$vars_poly) %>%
    transmute(x = rdplot_x, y = rdplot_y,
              side = ifelse(x >= 0, "Eligible", "Not eligible"),
              outcome = label) %>% filter(is.finite(x), is.finite(y))
  list(bins = bins, poly = poly)
}

p_win  <- rd_panel("lead_win",  "The side batting first wins")
p_loss <- rd_panel("lead_loss", "The side batting first loses")

f2_bins <- bind_rows(p_win$bins, p_loss$bins) %>%
  mutate(outcome = factor(outcome, levels = c("The side batting first wins",
                                              "The side batting first loses")))
f2_poly <- bind_rows(p_win$poly, p_loss$poly) %>%
  mutate(outcome = factor(outcome, levels = c("The side batting first wins",
                                              "The side batting first loses")))

f2 <- ggplot() +
  geom_vline(xintercept = 0, colour = "#4A4A4A", linewidth = 0.5) +
  geom_point(data = f2_bins, aes(x, y, colour = side, shape = side),
             size = 2.4, stroke = 0.9, fill = "white") +
  geom_line(data = f2_poly, aes(x, y, colour = side, group = side),
            linewidth = 1.0) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_colour_manual(values = c("Not eligible" = "#3D8EB9",
                                 "Eligible"     = "#5C2346")) +
  scale_shape_manual(values = c("Not eligible" = 21, "Eligible" = 16)) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = "First-innings lead minus the statutory margin (runs)",
       y = "Share of matches", colour = NULL, shape = NULL) +
  theme_leap(BASE)
save_leap_fig("fig2_rd_outcomes", f2, width = 10, height = 5)

# =============================================================================
#  FIGURE 3 — the behavioural event study
# =============================================================================
#  The reference year is filled in plum; the estimates are open circles. A fall
#  after year zero would be evidence that captains retreat after a disaster.
# -----------------------------------------------------------------------------
es <- beh$event_study %>%
  bind_rows(tibble(k = -1, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)) %>%
  arrange(k) %>%
  mutate(period = factor(ifelse(k < 0, "Before", "After"),
                         levels = c("Before", "After")))

f3 <- ggplot(es, aes(k, estimate)) +
  geom_hline(yintercept = 0, colour = "#4A4A4A", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, colour = LEAP_NONSIG_COLOR, linewidth = 0.4,
             linetype = "dotted") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, colour = period), width = 0.14,
                linewidth = 0.7) +
  geom_point(aes(colour = period), size = 3, shape = 21,
             fill = ifelse(es$k == -1, "#5C2346", "white"), stroke = 1.1) +
  scale_colour_manual(values = c("Before" = "#3D8EB9", "After" = "#5C2346")) +
  scale_x_continuous(breaks = -5:5) +
  labs(x = "Years relative to the match lost after enforcing the follow-on",
       y = "Change in probability of enforcing", colour = NULL) +
  theme_leap(BASE)
save_leap_fig("fig3_event_study", f3, width = 10, height = 6)

# =============================================================================
#  TABLE 1 — regression discontinuity estimates
# =============================================================================
star <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "***",
                    ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))
fmt <- function(b, se, p) sprintf("%.3f%s (%.3f)", b, star(p), se)

t1 <- bind_rows(
  rd$first_stage  %>% mutate(panel = "A. First stage: probability of enforcing"),
  rd$reduced_form %>% mutate(panel = "B. Reduced form: effect of eligibility"),
  rd$fuzzy        %>% mutate(panel = "C. Fuzzy RD: effect of enforcing")) %>%
  transmute(panel, spec, outcome,
            est = fmt(estimate, se_robust, p_robust),
            ci = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
            bandwidth = round(bw, 0), n = n_left + n_right)
write_csv(t1, file.path(TABDIR, "table1_rd.csv"))

# =============================================================================
#  TABLE 2 — behavioural estimates
# =============================================================================
t2 <- tibble(
  window = c("First post-event year", "Five-year average"),
  events = 34,
  estimate = c(es$estimate[es$k == 0], beh$beta),
  se_cluster = c(es$se[es$k == 0], beh$cluster_se),
  ri_p = c(beh$ri_p_first, beh$ri_p),
  ci_lo = c(es$ci_lo[es$k == 0], beh$ri_ci[1]),
  ci_hi = c(es$ci_hi[es$k == 0], beh$ri_ci[2])) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
write_csv(t2, file.path(TABDIR, "table2_behavioural.csv"))

cat("\n===== TABLE 1 =====\n"); print(as.data.frame(t1))
cat("\n===== TABLE 2 =====\n"); print(as.data.frame(t2))
cat("\nLEAP figures written to", FIGDIR, "at 600 DPI\n")
message("08_figures_tables.R done")

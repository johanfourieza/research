# =============================================================================
#  02_figures.R
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Draws the four figures from the aggregate files in data/:
#    Figure 1   probate documents and widow entries around the epidemic
#    Figure 2   how unequal recording changes the mortality comparison
#    Figure S1  linked record rates by adult slaveholding (online appendix)
#    Figure S2  mortality comparisons under source-specific recovery
#               assumptions (online appendix)
#  Figures are saved as PNG and PDF in output/figures/.
#
#  HOW TO USE IT
#  From the package root:  Rscript scripts/02_figures.R
# =============================================================================

source(file.path(if (dir.exists("scripts")) "scripts" else ".", "00_setup.R"))

# -----------------------------------------------------------------------------
#  Figure 1. Two series that use no person linkage. Points mark observed years;
#  years without a surviving roll are missing, not zero.
# -----------------------------------------------------------------------------
docs   <- read_data("probate_documents_by_year.csv")
widows <- read_data("widow_entries_by_year.csv")
series <- bind_rows(
  docs |> filter(year >= 1705, year <= 1722) |>
    transmute(year, y = documents, panel = "(a) Cape MOOC8 probate documents (count)"),
  widows |> filter(year >= 1705, year <= 1722) |>
    transmute(year, y = 100 * widow_share,
              panel = "(b) District widow entries (% of enumerated entries)"))

fig1 <- ggplot(series, aes(year, y)) +
  geom_vline(xintercept = 1713, linetype = 2, colour = LEAP_GREY) +
  geom_line(colour = LEAP[["plum"]], linewidth = 0.7) +
  geom_point(colour = LEAP[["plum"]], size = 2) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(1705, 1708, 1711, 1713, 1716, 1719, 1722)) +
  labs(x = "Year", y = NULL) +
  theme_leap() + theme(panel.spacing = unit(1.2, "lines"))
save_fig(fig1, "figure1_event_series", height = 7.5)

# -----------------------------------------------------------------------------
#  The recording model: theta = r / kappa, where r is the observed low-to-high
#  recorded-death ratio and kappa = C_L / C_H is an assumed relative
#  probability of recovering a death. The curves are conditional calculations,
#  not estimates of recording completeness.
# -----------------------------------------------------------------------------
records <- read_data("death_records_by_group.csv") |> filter(sample == "all_men")
ratio_for <- function(rec) {
  records$rate[records$record == rec & records$group == "low"] /
    records$rate[records$record == rec & records$group == "high"]
}
kappa <- seq(0.15, 1, length.out = 300)
curves <- bind_rows(lapply(
  c(estate_document = "MOOC8 death confirmation", widow_entry = "Widow entry",
    combined_death_record = "Either death record") |> as.list() |> names(),
  function(rec) tibble(record = rec, kappa = kappa, theta = ratio_for(rec) / kappa))) |>
  mutate(label = factor(record,
    levels = c("estate_document", "widow_entry", "combined_death_record"),
    labels = c("MOOC8 death confirmation", "Widow entry", "Either death record")))
r <- ratio_for("combined_death_record")
fmt <- function(x, d) formatC(x, format = "f", digits = d)

# Figure 2 (main text): the combined record and its equal-mortality threshold
fig2 <- ggplot(filter(curves, record == "combined_death_record"), aes(kappa, theta)) +
  geom_hline(yintercept = 1, linetype = 2, colour = LEAP_GREY) +
  geom_vline(xintercept = r, linetype = 3, colour = LEAP_GREY) +
  geom_line(linewidth = 1.3, colour = LEAP[["plum"]]) +
  geom_point(data = tibble(kappa = c(1, r), theta = c(r, 1)),
             size = 3, colour = LEAP[["plum"]]) +
  annotate("text", x = 0.98, y = 0.51, hjust = 0, size = 3.6, colour = LEAP[["plum"]],
           label = paste0("Equal recovery\nMortality ratio = ", fmt(r, 3))) +
  annotate("label", x = 0.57, y = 1.35, size = 3.6, linewidth = 0, fill = "white",
           colour = LEAP[["plum"]],
           label = paste0("Equal mortality\nRecovery ratio = ", fmt(r, 3),
                          "\nHigher-resource advantage = ", fmt(1 / r, 1), " times")) +
  scale_x_reverse(breaks = c(1, 0.75, 0.5, r, 0.15),
                  labels = c("1.00", "0.75", "0.50", fmt(r, 3), "0.15")) +
  scale_y_continuous(limits = c(0, 2.05), breaks = c(0, 0.5, 1, 1.5, 2)) +
  labs(x = "Assumed relative probability of recovering a death (low / high)",
       y = "Implied mortality ratio (low / high)") +
  theme_leap()
save_fig(fig2, "figure2_recording_model")

# -----------------------------------------------------------------------------
#  Figure S1. Record rates with 95% Wilson intervals. Genealogical presence
#  measures representation in reconstructed families, not mortality.
# -----------------------------------------------------------------------------
s1 <- records |>
  filter(record %in% c("estate_document", "widow_entry", "genealogical_presence")) |>
  mutate(record = factor(record,
           levels = c("estate_document", "widow_entry", "genealogical_presence"),
           labels = c("MOOC8 death confirmation", "Widow entry", "Genealogical presence")),
         group = factor(group, GROUP_LEVELS, labels = c("0 recorded", "1-4", "5+")))
figS1 <- ggplot(s1, aes(group, rate, colour = record, group = record)) +
  geom_point(position = position_dodge(width = 0.4), size = 2.6) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.4),
                width = 0.12, linewidth = 0.6) +
  scale_colour_manual(values = unname(LEAP[c("plum", "blue", "gold")])) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Adult slaves recorded in 1712", y = "Share with a linked record", colour = NULL) +
  theme_leap() + theme(legend.position = "bottom")
save_fig(figS1, "figureS1_record_rates")

# -----------------------------------------------------------------------------
#  Figure S2. Each curve applies the model to its own source; the sources need
#  not share a common recovery ratio.
# -----------------------------------------------------------------------------
figS2 <- ggplot(curves, aes(kappa, theta, colour = label)) +
  geom_hline(yintercept = 1, linetype = 2, colour = LEAP_GREY) +
  geom_line(data = filter(curves, record != "combined_death_record"),
            linewidth = 0.7, alpha = 0.75) +
  geom_line(data = filter(curves, record == "combined_death_record"), linewidth = 1.3) +
  geom_point(data = tibble(kappa = r, theta = 1,
                           label = factor("Either death record", levels(curves$label))),
             size = 3) +
  annotate("label", x = 0.58, y = 1.32, size = 3.3, linewidth = 0, fill = "white",
           colour = LEAP[["sage"]],
           label = paste0("Either record: equal mortality at ", fmt(r, 3))) +
  scale_colour_manual(values = unname(LEAP[c("plum", "blue", "sage")])) +
  scale_x_reverse(breaks = c(1, 0.75, 0.5, 0.25)) +
  labs(x = expression(paste("Assumed capture ratio ", C[L]/C[H])),
       y = expression(paste("Implied mortality ratio ", M[L]/M[H])), colour = NULL) +
  theme_leap() + theme(legend.position = "bottom")
save_fig(figS2, "figureS2_source_sensitivity")

cat("All figures drawn.\n")

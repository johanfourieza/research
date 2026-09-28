# =============================================================================
#  03_continuation.R
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Reproduces the record-continuation diagnostics in Online Appendix D.
#  For five windows (1709, 1710, 1713, 1718, 1721), each named entry in the
#  latest preceding roll is flagged if no later name meets a rule's
#  continuation criterion within the search years. Three rules vary the
#  evidence needed: guarded names, a permissive first-name fallback, and a
#  unique-target rule. A flag is NOT a death: it may reflect migration,
#  naming or enumeration changes.
#
#  For each rule the contrast is
#    DD = (log D_low,1713 - log D_owner,1713)
#         - 1/4 * sum over t in {1709, 1710, 1718, 1721} (log D_low,t - log D_owner,t)
#  where D is the flag rate and "owner" means at least one adult slave recorded.
#  The point estimates follow exactly from the cell counts in
#  data/continuation_cells.csv, and this script recomputes them. The standard
#  errors cluster on full-name keys across windows and pair the rules within
#  each entry; that requires the individual-level panel, which is not
#  redistributed, so they are read from data/continuation_estimates.csv.
#
#  HOW TO USE IT
#  From the package root:  Rscript scripts/03_continuation.R
# =============================================================================

source(file.path(if (dir.exists("scripts")) "scripts" else ".", "00_setup.R"))

cells     <- read_data("continuation_cells.csv")
estimates <- read_data("continuation_estimates.csv")
windows   <- read_data("continuation_windows.csv")

# -----------------------------------------------------------------------------
#  Table S4: entries failing each continuation criterion, primary specification
# -----------------------------------------------------------------------------
cat("\nTable S4: flags / entries (primary specification)\n")
tableS4 <- cells |>
  filter(specification == "primary") |>
  mutate(cell = paste0(flags, "/", entries), column = paste(rule, group)) |>
  select(window, column, cell) |>
  pivot_wider(names_from = column, values_from = cell) |>
  select(window, `guarded_names low`, `guarded_names owner`,
         `permissive_fallback low`, `permissive_fallback owner`,
         `unique_target low`, `unique_target owner`)
print(as.data.frame(tableS4), row.names = FALSE)
write_csv(tableS4, file.path(OUT_TAB, "tableS4_continuation_cells.csv"))

# -----------------------------------------------------------------------------
#  Common-weight contrasts, recomputed from the cells
# -----------------------------------------------------------------------------
dd <- cells |>
  mutate(log_rate = log(flags / entries)) |>
  select(specification, window, rule, group, log_rate) |>
  pivot_wider(names_from = group, values_from = log_rate) |>
  mutate(gap = low - owner) |>
  group_by(specification, rule) |>
  summarise(estimate = gap[window == 1713] - mean(gap[window != 1713]), .groups = "drop")

quantity <- c(guarded_names = "DD_name", permissive_fallback = "DD_permissive",
              unique_target = "DD_unique")
recomputed <- bind_rows(
  dd |> mutate(quantity = quantity[rule]),
  dd |> pivot_wider(names_from = rule, values_from = estimate) |>
    transmute(specification,
              permissive_minus_name = permissive_fallback - guarded_names,
              unique_minus_name = unique_target - guarded_names) |>
    pivot_longer(-specification, names_to = "quantity", values_to = "estimate")) |>
  select(specification, quantity, recomputed = estimate)

compare <- estimates |> left_join(recomputed, by = c("specification", "quantity"))
stopifnot(nrow(compare) == 20, max(abs(compare$estimate - compare$recomputed)) < 1e-6)
cat("\nAll 20 point estimates reproduce from the cell counts.\n")

# -----------------------------------------------------------------------------
#  Table S3 (primary) and Table S5 (permissive minus guarded, by specification)
# -----------------------------------------------------------------------------
labels <- c(DD_name = "Guarded names", DD_permissive = "Names plus permissive fallback",
            DD_unique = "Unique target name", permissive_minus_name = "Permissive minus guarded",
            unique_minus_name = "Unique minus guarded")
tableS3 <- estimates |> filter(specification == "primary") |>
  transmute(`Rule or rule contrast` = labels[quantity], Estimate = round(estimate, 3),
            `Standard error` = round(se, 3), p = round(p, 3))
cat("\nTable S3: common-weight comparisons (primary specification)\n")
print(as.data.frame(tableS3), row.names = FALSE)
write_csv(tableS3, file.path(OUT_TAB, "tableS3_continuation_estimates.csv"))

spec_labels <- c(primary = "Latest roll, two-year search",
                 three_year_baseline = "Three-year baseline, two-year search",
                 long_search = "Latest roll, longer search",
                 three_year_baseline_long_search = "Three-year baseline, longer search")
tableS5 <- estimates |> filter(quantity == "permissive_minus_name") |>
  mutate(specification = factor(specification, names(spec_labels))) |>
  arrange(specification) |>
  transmute(`Baseline and search` = spec_labels[as.character(specification)],
            Estimate = round(estimate, 3), `Standard error` = round(se, 3), p = round(p, 3))
cat("\nTable S5: sensitivity of the permissive-minus-guarded contrast\n")
print(as.data.frame(tableS5), row.names = FALSE)
write_csv(tableS5, file.path(OUT_TAB, "tableS5_continuation_sensitivity.csv"))

check("Guarded-name estimate", tableS3$Estimate[1], -0.156, 3)
check("Permissive estimate", tableS3$Estimate[2], -0.410, 3)
check("Permissive minus guarded", tableS3$Estimate[4], -0.254, 3)
check("  standard error", tableS3$`Standard error`[4], 0.321, 3)
check("  p-value", tableS3$p[4], 0.430, 3)
cat("\nThe continuation rules do not identify the mortality ranking: every contrast is imprecise.\n")

# =============================================================================
#  load_data.R  -  Load the data and reproduce the paper's central argument.
#  Run this first. It is written to be read line by line and needs only base R.
# =============================================================================

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
d <- read.csv(file.path(root, "data", "death_records_by_group.csv"))

# -----------------------------------------------------------------------------
#  STEP 1. The puzzle.
#  348 men named in the 1712 Stellenbosch-Drakenstein tax roll, grouped by the
#  adult slaves recorded against them. A death is "confirmed" when an estate
#  document or a later widow entry can be linked to the man, through 1714.
# -----------------------------------------------------------------------------
combined <- d[d$sample == "all_men" & d$record == "combined_death_record", ]
print(combined[, c("adult_slaves", "men", "events", "rate")], row.names = FALSE)
# Deaths are documented for 22.2 per cent of men with five or more adult slaves
# and 6.6 per cent of men with none. Is that mortality, recording, or both?

# -----------------------------------------------------------------------------
#  STEP 2. Both death channels show the same ordering, and so does presence in
#  reconstructed genealogies, which is not a death outcome at all. The men with
#  more resources are simply more visible in the records.
# -----------------------------------------------------------------------------
all_men <- d[d$sample == "all_men", ]
print(xtabs(rate ~ record + adult_slaves, all_men), digits = 3)

# -----------------------------------------------------------------------------
#  STEP 3. Family circumstances. Restricting to men with a wife named narrows
#  the gap from 6.6 vs 22.2 per cent to 14.9 vs 22.0 per cent. This is not an
#  adjusted mortality comparison: the restriction selects different men.
# -----------------------------------------------------------------------------
wife <- d[d$sample == "wife_named" & d$record == "combined_death_record", ]
print(wife[, c("adult_slaves", "men", "events", "rate")], row.names = FALSE)

# -----------------------------------------------------------------------------
#  STEP 4. The recording model. The recorded-death rate D is mortality M times
#  the probability C that a death is recovered: D = M x C. The mortality ratio
#  is therefore (D_low / D_high) / (C_low / C_high).
# -----------------------------------------------------------------------------
D_low  <- combined$rate[combined$group == "low"]
D_high <- combined$rate[combined$group == "high"]
r <- D_low / D_high
cat("\nrecorded-death ratio, low / high:", round(r, 3), "\n")

# Equal mortality requires C_low / C_high = r. Higher-resource deaths would
# have to be 1 / r times as likely to be recovered:
cat("equal-mortality threshold:", round(1 / r, 3), "\n")

# The sources do not estimate C, so this is a benchmark, not an estimate.
# If lower-resource deaths were half or a quarter as likely to be recovered:
cat("implied mortality ratio at C_low / C_high = 1/2:", round(r / 0.5, 2), "\n")
cat("implied mortality ratio at C_low / C_high = 1/4:", round(r / 0.25, 2), "\n")

# Assuming only that higher-resource deaths were at least as recoverable
# (C_low <= C_high) leaves the sharp range [r, 1 / D_high], which contains
# equal mortality and a disadvantage in either direction:
cat("range under C_low <= C_high: [", round(r, 3), ",", round(1 / D_high, 2), "]\n")

# For every table and figure, run:  Rscript scripts/run_all.R

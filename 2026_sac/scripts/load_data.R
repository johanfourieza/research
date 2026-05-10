###############################################################################
# load_data.R
#
# Helper that loads data/sac_clean.csv as a tibble named `sac`, with the
# correct column types, and aliases the reviewed-baseline Afrikaner indicator
# to the working `afrikaans` column (matching the convention used inside
# paper2_analysis.R).
#
# Run this from the 2026_sac/ folder.
###############################################################################

library(readr)
library(dplyr)

sac <- read_csv(
  "data/sac_clean.csv",
  na = "",
  show_col_types = FALSE,
  guess_max = 10000
)

# Cast date columns explicitly (read_csv may infer character if a few rows
# are missing on the first guess_max rows).
date_cols <- intersect(c("date_enlist", "date_discharge"), names(sac))
for (cc in date_cols) sac[[cc]] <- as.Date(sac[[cc]])

# Preserve the legacy hand-coded indicator and promote the reviewed
# baseline (the paper's primary treatment) to the working `afrikaans`
# column. This matches the alias used in paper2_analysis.R.
sac <- sac %>%
  mutate(afrikaans_baseline = afrikaans,
         afrikaans          = afrikaans_reviewed)

cat(sprintf("Loaded sac: %d rows x %d cols\n", nrow(sac), ncol(sac)))
cat(sprintf("  Afrikaner (reviewed baseline): %d / %d\n",
            sum(sac$afrikaans, na.rm = TRUE),
            nrow(sac)))

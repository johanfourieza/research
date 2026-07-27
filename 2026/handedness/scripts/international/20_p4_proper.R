# =============================================================================
# 20_p4_proper.R  —  a correctly specified formal test of Prediction P4
#
# Committed Appendix B10 pools the three formats and imposes a COMMON quality
# gradient on all of them. That is not innocuous: a pre-match average of 40
# does not mean the same thing in a Test as in a T20I, and mean partnership
# runs differ by a third across formats. Forcing one set of control
# coefficients loads the residual format differences onto the interaction.
#
# Symptom (script 19): pooled gives is_mixed_hand x T20I = +2.56 (p < 1e-4) and
# a Tests baseline of -1.21 (p = 0.018), while the format-specific estimates in
# Table 3 are -0.041 / -0.103 / +0.186, all null. Even the Tests coefficient
# moves by more than a run under pooling.
#
# The correct pooled test interacts EVERY control with format. That is
# algebraically equivalent to three separate regressions, so the coefficients
# reproduce Table 3 exactly, while still permitting a joint Wald test of
# equality of the mixed-hand effect across formats -- which is what P4 asserts.
#
# Rscript scripts/20_p4_proper.R
# =============================================================================

library(tidyverse)
library(fixest)

base_dir     <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
analysis_dir <- file.path(base_dir, "data", "analysis")
out_dir      <- file.path(base_dir, "scripts", "output", "resubmit_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

formats <- c("tests", "odis", "t20is")
flab    <- c(tests = "Tests", odis = "ODIs", t20is = "T20Is")

load_p <- function(fmt) {
  read_csv(file.path(analysis_dir, paste0("analysis_", fmt, ".csv")),
           show_col_types = FALSE) %>%
    filter(hand_known == 1, both_avg_known == 1, either_debutant == 0) %>%
    mutate(partnership_number_f = factor(pmin(partnership_number, 10)),
           match_innings_id = if ("match_innings_id" %in% names(.))
             match_innings_id else paste0(match_id, "_", innings))
}

pooled <- bind_rows(map(formats, function(fmt) {
  load_p(fmt) %>% mutate(fmt = flab[fmt],
                         match_innings_id = paste0(fmt, "_", match_innings_id))
})) %>% mutate(fmt = factor(fmt, levels = c("Tests", "ODIs", "T20Is")))

cat("\n=== Fully format-interacted pooled model (equivalent to separate regressions) ===\n")

m <- feols(
  runs_scored ~ fmt / is_mixed_hand +
    fmt:max_pre_match_avg + fmt:min_pre_match_avg + fmt:combined_experience +
    fmt:wickets_at_start + fmt:runs_at_start + fmt:partnership_number_f |
    match_innings_id,
  data = pooled, cluster = ~match_id)

ct <- coeftable(m)
keep <- grep("is_mixed_hand", rownames(ct))
print(ct[keep, ], digits = 4)

cat("\n=== Wald tests of P4 (equality of the mixed-hand effect across formats) ===\n")
nm <- rownames(ct)[keep]
w_all <- wald(m, keep = "is_mixed_hand")
cat("\nJoint test that all three format-specific effects are zero:\n")
print(w_all)

# pairwise equality: T20I vs Tests, ODI vs Tests
h_t20_test <- paste0(grep("T20Is", nm, value = TRUE), " = ",
                     grep("Tests", nm, value = TRUE))
h_odi_test <- paste0(grep("ODIs",  nm, value = TRUE), " = ",
                     grep("Tests", nm, value = TRUE))

for (h in c(h_t20_test, h_odi_test)) {
  cat("\nH0: ", h, "\n", sep = "")
  print(car::linearHypothesis(m, h, test = "Chisq"))
}

res <- as_tibble(ct[keep, ], rownames = "term")
write_csv(res, file.path(out_dir, "E_p4_interacted.csv"))
cat("\nWritten to E_p4_interacted.csv\n")

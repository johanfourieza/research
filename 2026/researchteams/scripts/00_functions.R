# Shared functions for the replication scripts.
# Sourced by 01_table1.R, 02_page_split.R and 05_romano_wolf.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
})

# One thread, so that results do not depend on the number of cores.
setFixest_nthreads(1)

# ---- Data --------------------------------------------------------------------

# papers.csv has one row per paper. task_label classifies each disclosure by the
# tasks it reports. The three indicators below split `disclosed` into three
# exclusive groups; a paper without a disclosure has all three equal to zero.
read_papers <- function() {
  read_csv("data/papers.csv", show_col_types = FALSE, na = "") |>
    mutate(
      disclosed_oper         = as.integer(task_label == "operational-or-mixed"),
      disclosed_writing_only = as.integer(task_label == "writing-only"),
      disclosed_unlabelled   = as.integer(task_label == "unlabelled")
    )
}

read_links <- function() {
  read_csv("data/researcher_links.csv", show_col_types = FALSE)
}

# The regressions have one row per paper-researcher link, not per paper.
# estimation_rows() builds those rows for one outcome.
#
# The set of links differs by outcome because each outcome has its own sample:
# a paper enters only if the outcome is measured, and a researcher enters only
# with at least two such papers issued from January 2023. Each link carries
# weight 1 / (number of retained researchers on the paper), so every paper has
# total weight one.
estimation_rows <- function(papers, links, outcome_name) {
  rows <- links |>
    filter(outcome == outcome_name) |>
    select(uid, researcher, weight) |>
    inner_join(papers, by = "uid")
  paper_weight <- tapply(rows$weight, rows$uid, sum)
  stopifnot(max(abs(paper_weight - 1)) < 1e-12)
  as.data.frame(rows)
}

# ---- Estimation ----------------------------------------------------------------

# Eq. (3) of the letter:
#   Y = researcher FE + source-quarter FE + field FE
#       + b_W * writing flag + b_D * disclosure + two text-length controls.
# `marker` is the writing flag. log_words and log_windows are the controls for
# the length of the text the detector scored.
rhs_main  <- "marker + disclosed + log_words + log_windows"

# Task-split version: the disclosure indicator is replaced by its three groups.
rhs_split <- paste("marker + disclosed_oper + disclosed_writing_only +",
                   "disclosed_unlabelled + log_words + log_windows")

# Weighted least squares with three sets of fixed effects. Standard errors are
# clustered two ways, by researcher and by paper, because one paper contributes
# a row for each of its researchers.
fit_model <- function(rows, y, rhs) {
  feols(as.formula(paste(y, "~", rhs, "| researcher + source_quarter + field")),
        data = rows, weights = ~weight, vcov = ~researcher + uid, notes = FALSE)
}

# Coefficient, standard error and p-value of the named terms, as a data frame.
# `scale = 100` converts a 0/1 outcome to percentage points.
tidy_terms <- function(fit, terms, scale = 1) {
  ct <- coeftable(fit)
  data.frame(term = terms,
             coefficient = ct[terms, 1] * scale,
             se = ct[terms, 2] * scale,
             p = ct[terms, 4],
             row.names = NULL)
}

# Wald test that the operational and writing-only coefficients are equal,
# using the two-way clustered covariance and fixest's t degrees of freedom.
equality_test <- function(fit, scale = 1) {
  a <- "disclosed_oper"; b <- "disclosed_writing_only"
  est <- coef(fit); v <- vcov(fit)
  difference <- est[[a]] - est[[b]]
  se <- sqrt(v[a, a] + v[b, b] - 2 * v[a, b])
  df <- degrees_freedom(fit, "t")
  data.frame(term = "operational_minus_writing_only",
             coefficient = difference * scale, se = se * scale,
             p = pf((difference / se)^2, 1, df, lower.tail = FALSE))
}

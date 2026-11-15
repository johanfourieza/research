# Check every number in the letter that this package reproduces.
#
# Reads   results/*.csv (written by scripts 01 and 02) and data/*.csv
# Stops with an error at the first number that differs from the letter.
#
# Each check() call gives the value computed here, the value printed in the
# letter, and the number of decimals at which the letter prints it.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

est     <- read_csv("results/table1_estimates.csv", show_col_types = FALSE)
samples <- read_csv("results/table1_samples.csv", show_col_types = FALSE)
means   <- read_csv("results/group_means.csv", show_col_types = FALSE)
pages   <- read_csv("results/page_split_estimates.csv", show_col_types = FALSE)
tests   <- read_csv("data/registered_tests.csv", show_col_types = FALSE)
shares  <- read_csv("data/half_year_shares.csv", show_col_types = FALSE)
frame   <- read_csv("data/series_frame.csv", show_col_types = FALSE)

n_checked <- 0
check <- function(label, computed, printed, digits = 3) {
  if (!isTRUE(all.equal(round(computed, digits), printed, tolerance = 1e-9))) {
    stop(sprintf("%s: computed %s, letter prints %s", label,
                 format(round(computed, digits), nsmall = digits), printed))
  }
  n_checked <<- n_checked + 1
}

# ---- Table 1 -----------------------------------------------------------------
# One row per outcome: coefficient and standard error in columns 1 to 4, then
# the p-value of the equality test in column 5.
table1 <- list(
  #                       (1) flag         (2) disclosed  (3) operational (4) writing-only  (5) p
  ra_count     = list(c(-0.131, 0.111), c(0.548, 0.168), c(0.679, 0.199), c(-0.098, 0.209), .003, 3),
  any_ra       = list(c(-3.21, 2.25),   c(6.60, 2.61),   c(6.86, 2.94),   c(1.71, 5.99),    .429, 2),
  author_count = list(c(0.011, 0.061),  c(0.240, 0.093), c(0.300, 0.103), c(0.056, 0.235),  .324, 3),
  pages        = list(c(-0.69, 1.10),   c(5.35, 1.27),   c(6.78, 1.44),   c(0.83, 2.97),    .071, 2)
)
columns <- c("1 writing flag", "2 disclosed", "3 operational", "4 writing-only")
for (y in names(table1)) {
  digits <- table1[[y]][[6]]
  for (k in 1:4) {
    row <- filter(est, outcome == y, column == columns[k])
    check(paste(y, columns[k], "coefficient"), row$coefficient, table1[[y]][[k]][1], digits)
    check(paste(y, columns[k], "standard error"), row$se, table1[[y]][[k]][2], digits)
  }
  check(paste(y, "equality p"), filter(est, outcome == y, column == "5 equality")$p,
        table1[[y]][[5]], 3)
}

# ---- Samples (Section 3 and the note to Table 1) -------------------------------
s <- function(y, column) filter(samples, outcome == y)[[column]]
check("assistant sample, papers",        s("any_ra", "papers"), 7243, 0)
check("named-assistant sample, papers",  s("ra_count", "papers"), 7242, 0)
check("assistant sample, researchers",   s("any_ra", "researchers"), 4604, 0)
check("assistant sample, disclosing",    s("any_ra", "disclosed"), 468, 0)
check("page sample, papers",             s("pages", "papers"), 8502, 0)
check("page sample, researchers",        s("pages", "researchers"), 5285, 0)
check("page sample, disclosing",         s("pages", "disclosed"), 544, 0)
check("operational disclosures",         s("any_ra", "operational"), 361, 0)
check("writing-only disclosures",        s("any_ra", "writing_only"), 68, 0)
check("unlabelled disclosures",          s("any_ra", "unlabelled"), 39, 0)

# ---- Means on papers without a disclosure (Section 4) ---------------------------
m <- function(y) filter(means, outcome == y, task_label == "no-disclosure")$mean
check("mean named assistants, no disclosure", m("ra_count"), 0.83, 2)
check("mean pages, no disclosure",            m("pages"), 52.4, 1)
# 'two-thirds of the mean'
check("named-assistant coefficient / mean",
      filter(est, outcome == "ra_count", column == "2 disclosed")$coefficient / m("ra_count"),
      0.66, 2)

# ---- Page split (Section 4 and Figure 1) -----------------------------------------
g <- function(y, term_name) filter(pages, outcome == y, term == term_name)
check("page-split sample",              g("pages", "disclosed")$papers, 8493, 0)
check("appendix pages, any disclosure", g("appendix_pages", "disclosed")$coefficient, 3.66, 2)
check("main pages, any disclosure",     g("main_pages", "disclosed")$coefficient, 1.63, 2)
check("appendix pages, operational",    g("appendix_pages", "disclosed_oper")$coefficient, 5.00, 2)
# 'about seven-tenths' of the extra length is in appendices
check("appendix share of extra pages",
      g("appendix_pages", "disclosed")$coefficient / g("pages", "disclosed")$coefficient, 0.7, 1)
# writing-only papers 'have no more appendix pages': the estimate is not positive
stopifnot(g("appendix_pages", "disclosed_writing_only")$coefficient < 0)

# ---- Multiple-testing p-values (Section 4 and the note to Table 1) ----------------
# data/registered_tests.csv stores the adjusted p-values. 05_romano_wolf.R
# recomputes the Romano-Wolf values from the data; Holm is recomputed here.
rw <- function(y, x) filter(tests, outcome == y, indicator == x)$rw_p
check("Romano-Wolf p, named assistants", rw("ra_count", "disclosed"), .020, 3)
check("Romano-Wolf p, any assistant",    rw("any_ra", "disclosed"), .157, 3)
check("Romano-Wolf p, authors",          rw("author_count", "disclosed"), .147, 3)
stopifnot(rw("pages", "disclosed") < .001)
stopifnot(min(filter(tests, indicator == "marker")$rw_p) > .64)
stopifnot(max(abs(p.adjust(tests$raw_p, "holm") - tests$holm_p)) < 1e-10)
# 'Holm adjustments imply the same conclusions' at the 5% level
stopifnot(all((tests$holm_p <= .05) == (tests$rw_p <= .05)))
# The coefficients stored with the p-values are those estimated by 01_table1.R.
for (y in c("ra_count", "author_count", "pages")) {
  check(paste("stored coefficient,", y),
        filter(tests, outcome == y, indicator == "disclosed")$coefficient,
        round(filter(est, outcome == y, column == "2 disclosed")$coefficient, 6), 6)
}

# ---- Half-year shares (Section 4 and Figure 2) --------------------------------------
inst <- filter(shares, scope == "institutional")
any_ra    <- filter(inst, outcome == "any_ra")$share_pct
disclosed <- filter(inst, outcome == "disclosed")
stopifnot(all(round(any_ra) >= 27), all(round(any_ra) <= 32))
check("disclosure share, 2022 H1", disclosed$share_pct[disclosed$half_year == "2022 H1"], 0, 0)
check("disclosure share, 2026 H2", disclosed$share_pct[disclosed$half_year == "2026 H2"], 17, 0)

# ---- Frame (abstract and Section 3) ---------------------------------------------------
check("series", nrow(frame), 24, 0)
check("paper records", sum(frame$records), 56585, 0)

cat(n_checked, "numbers match the letter.\n")

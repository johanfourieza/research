# Table 1: within-researcher associations between AI indicators and paper inputs.
#
# Reads   data/papers.csv, data/researcher_links.csv
# Writes  results/table1_estimates.csv   every coefficient in Table 1
#         results/table1_samples.csv     papers, researchers and disclosures by row
#         results/group_means.csv        outcome means by disclosure group

source("scripts/00_functions.R")

papers <- read_papers()
links  <- read_links()

# The four rows of Table 1. `any_ra` is a 0/1 outcome reported in percentage
# points, so its coefficients are multiplied by 100.
table_rows <- c(ra_count = 1, any_ra = 100, author_count = 1, pages = 1)

estimates <- list(); samples <- list(); means <- list()

for (y in names(table_rows)) {
  scale <- table_rows[[y]]
  rows  <- estimation_rows(papers, links, y)

  # Columns 1 and 2: writing flag and disclosure entered together.
  main <- fit_model(rows, y, rhs_main)
  stopifnot(nobs(main) == nrow(rows))          # no row dropped by the estimator

  # Columns 3 and 4: operational and writing-only disclosures. Unlabelled
  # disclosures get their own indicator so that the sample stays the same.
  split <- fit_model(rows, y, rhs_split)
  stopifnot(nobs(split) == nrow(rows))

  estimates[[y]] <- bind_rows(
    cbind(column = c("1 writing flag", "2 disclosed"),
          tidy_terms(main, c("marker", "disclosed"), scale)),
    cbind(column = c("3 operational", "4 writing-only"),
          tidy_terms(split, c("disclosed_oper", "disclosed_writing_only"), scale)),
    cbind(column = "5 equality", equality_test(split, scale))
  ) |> mutate(outcome = y, .before = 1)

  # One row per paper, for counts and means.
  by_paper <- distinct(rows, uid, .keep_all = TRUE)
  samples[[y]] <- data.frame(
    outcome = y,
    papers = nrow(by_paper),
    researchers = n_distinct(rows$researcher),
    paper_researcher_rows = nrow(rows),
    disclosed = sum(by_paper$disclosed),
    operational = sum(by_paper$disclosed_oper),
    writing_only = sum(by_paper$disclosed_writing_only),
    unlabelled = sum(by_paper$disclosed_unlabelled),
    disclosed_with_writing_flag = sum(by_paper$disclosed == 1 & by_paper$marker == 1)
  )
  means[[y]] <- by_paper |>
    group_by(task_label) |>
    summarise(papers = n(), mean = mean(.data[[y]]), .groups = "drop") |>
    mutate(outcome = y, .before = 1)
}

dir.create("results", showWarnings = FALSE)
write_csv(bind_rows(estimates), "results/table1_estimates.csv")
write_csv(bind_rows(samples),   "results/table1_samples.csv")
write_csv(bind_rows(means),     "results/group_means.csv")

print(bind_rows(estimates) |> mutate(across(where(is.numeric), \(x) round(x, 3))))
print(bind_rows(samples))

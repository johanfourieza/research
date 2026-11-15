# Page split: where the extra pages of disclosing papers are (Figure 1 and the
# page-split paragraph of Section 4).
#
# Reads   data/papers.csv, data/researcher_links.csv
# Writes  results/page_split_estimates.csv
#
# main_pages and appendix_pages divide a paper's PDF pages at the first appendix
# heading. They are missing for nine papers of the pages sample whose page text
# could not be verified, which leaves 8,493 papers.

source("scripts/00_functions.R")

papers <- read_papers()
links  <- read_links()

# Same researcher links and weights as the `pages` row of Table 1.
rows <- estimation_rows(papers, links, "pages") |>
  filter(!is.na(main_pages))
stopifnot(n_distinct(rows$uid) == 8493)

estimates <- list()
for (y in c("pages", "main_pages", "appendix_pages")) {
  # fixest drops researchers left with one paper after the nine exclusions.
  main  <- fit_model(rows, y, rhs_main)
  split <- fit_model(rows, y, rhs_split)
  used  <- rows[obs(main), ]
  estimates[[y]] <- bind_rows(
    tidy_terms(main, "disclosed"),
    tidy_terms(split, c("disclosed_oper", "disclosed_writing_only"))
  ) |>
    mutate(outcome = y, .before = 1) |>
    mutate(papers = n_distinct(used$uid),
           disclosed_papers = n_distinct(used$uid[used$disclosed == 1]))
}

dir.create("results", showWarnings = FALSE)
write_csv(bind_rows(estimates), "results/page_split_estimates.csv")
print(bind_rows(estimates) |> mutate(across(where(is.numeric), \(x) round(x, 3))))

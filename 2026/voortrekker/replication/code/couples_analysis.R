# Married-household analyses on the final links of both linkages (spouse-assisted
# and wife-blind). Writes output/couples_analysis/:
#   decomposition.csv            household-size / children gap: married-household
#                                margin vs within-couple margin (A1)
#   couples_reconciliation.csv   couples-only estimates across link sets (A2)
#   marriage_linkage*.csv        genealogy: linkage by marriage before the census (A3)
# Run from the replication root after both pipeline runs (see code/run_all.R).
suppressMessages({library(dplyr); library(sandwich); library(lmtest)})
out <- "output/couples_analysis"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20261006)

# Wild cluster restricted bootstrap by district (as in pipeline.R block A8c).
wcb_p <- function(df, y, B = 9999) {
  df <- df[!is.na(df[[y]]), ]
  g <- as.character(df$district); yv <- df[[y]]; dv <- as.numeric(df$is_voortrekker)
  yt <- yv - ave(yv, g); dt <- dv - ave(dv, g)
  D <- sum(dt^2); beta <- sum(dt * yt) / D; e <- yt - beta * dt
  G <- length(unique(g)); N <- length(yv); adj <- G / (G - 1) * (N - 1) / (N - G - 1)
  Ag <- tapply(dt^2, g, sum); se <- sqrt(adj * sum(tapply(dt * e, g, sum)^2)) / D
  Sg <- tapply(dt * yt, g, sum)
  W <- matrix(sample(c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)), B * G, replace = TRUE), nrow = B)
  bstar <- as.vector(W %*% Sg) / D; sc <- sweep(W, 2, Sg, `*`) - outer(bstar, Ag)
  mean(abs(bstar / (sqrt(adj * rowSums(sc^2)) / D)) >= abs(beta / se))
}
fe <- function(df, y, extra = "") {
  df <- df[!is.na(df[[y]]), ]
  m <- lm(as.formula(paste0(y, " ~ is_voortrekker", extra, " + factor(district)")), data = df)
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))["is_voortrekkerTRUE", ]
  data.frame(coef = ct[1], se = ct[2], lo = ct[1] - 1.96 * ct[2], hi = ct[1] + 1.96 * ct[2], p_hc1 = ct[4],
             n = nrow(df), n_vt = sum(df$is_voortrekker))
}

run_dir <- c(spouse_assisted = "output", wife_blind = "output_wife_blind")
load_run <- function(run) {
  a <- read.csv(file.path(run_dir[[run]], "tables/analysis_dataset.csv"))
  a$is_voortrekker <- toupper(as.character(a$is_voortrekker)) %in% c("TRUE", "1")
  nm <- read.csv(file.path(run_dir[[run]], "tables/census_parsed_names.csv")) %>%
    select(district, source_row, hr = head_role, sp = spouse_name_raw)
  a <- a %>% left_join(nm, by = c("district", "source_row")) %>%
    mutate(couple = hr %in% "male" & !is.na(sp) & sp != "")
  m <- read.csv(file.path(run_dir[[run]], "tables/voortrekker_matches.csv"))
  st <- bind_rows(read.csv(file.path(run_dir[[run]], "tables/linkage/proposals_blind.csv")),
                  read.csv(file.path(run_dir[[run]], "tables/linkage/proposals_spouse.csv"))) %>%
    distinct(row_id, census_id, .keep_all = TRUE)
  m <- m %>% left_join(st %>% select(row_id, census_id, evidence_state), by = c("row_id", "census_id"))
  a$state <- m$evidence_state[match(a$census_id, m$census_id)]
  list(a = a, m = m)
}
R <- load_run("spouse_assisted"); B <- load_run("wife_blind")
common <- intersect(R$m$census_id, B$m$census_id)

# ---- 1: decomposition --------------------------------------------------------
dec <- list()
for (run in c("spouse_assisted", "wife_blind", "spouse_assisted|common links", "wife_blind|common links")) {
  a <- if (startsWith(run, "spouse_assisted")) R$a else B$a
  if (grepl("common", run)) a <- a[!a$is_voortrekker | a$census_id %in% common, ]
  for (y in c("household_size", "settler_children")) {
    u <- fe(a, y); cnd <- fe(a, y, " + couple"); cp <- fe(a[a$couple, ], y)
    # Kitagawa decomposition within districts, weighted by trekker households.
    k <- a %>% filter(!is.na(.data[[y]])) %>% group_by(district) %>%
      summarise(nT = sum(is_voortrekker), sT = mean(couple[is_voortrekker]), sC = mean(couple[!is_voortrekker]),
                yTc = mean(.data[[y]][is_voortrekker & couple]), yTn = mean(.data[[y]][is_voortrekker & !couple]),
                yCc = mean(.data[[y]][!is_voortrekker & couple]), yCn = mean(.data[[y]][!is_voortrekker & !couple]),
                .groups = "drop") %>% filter(nT > 0) %>%
      mutate(yTn = ifelse(is.nan(yTn), yCn, yTn), yTc = ifelse(is.nan(yTc), yCc, yTc),
             gap = (sT * yTc + (1 - sT) * yTn) - (sC * yCc + (1 - sC) * yCn),
             composition = (sT - sC) * (yCc - yCn),
             within = sT * (yTc - yCc) + (1 - sT) * (yTn - yCn))
    w <- k$nT / sum(k$nT)
    dec[[length(dec) + 1]] <- data.frame(
      run = run, outcome = y, fe_unconditional = u$coef, fe_unconditional_p = u$p_hc1,
      fe_controlling_couple = cnd$coef, fe_controlling_couple_p = cnd$p_hc1,
      fe_couples_only = cp$coef, fe_couples_only_p = cp$p_hc1,
      share_married_margin_fe = 1 - cnd$coef / u$coef,
      kitagawa_gap = sum(w * k$gap), kitagawa_composition = sum(w * k$composition),
      kitagawa_within = sum(w * k$within), share_married_margin_kitagawa = sum(w * k$composition) / sum(w * k$gap),
      couple_share_trekkers = mean(a$couple[a$is_voortrekker]), couple_share_controls = mean(a$couple[!a$is_voortrekker]),
      couple_share_controls_district_weighted = sum(w * k$sC),
      couple_share_male_headed_controls = mean(a$couple[!a$is_voortrekker & a$hr %in% "male"]))
  }
}
write.csv(bind_rows(dec), file.path(out, "decomposition.csv"), row.names = FALSE)

# ---- 2: couples across link sets ----------------------------------------------
rec <- list()
samp <- list(
  list("spouse_assisted", "all links", R$a[R$a$couple, ]),
  list("spouse_assisted", "links common to both runs", R$a[R$a$couple & (!R$a$is_voortrekker | R$a$census_id %in% common), ]),
  list("spouse_assisted", "links only in this run", R$a[R$a$couple & (!R$a$is_voortrekker | !R$a$census_id %in% B$m$census_id), ]),
  list("wife_blind", "all links", B$a[B$a$couple, ]),
  list("wife_blind", "links common to both runs", B$a[B$a$couple & (!B$a$is_voortrekker | B$a$census_id %in% common), ]),
  list("wife_blind", "links only in this run", B$a[B$a$couple & (!B$a$is_voortrekker | !B$a$census_id %in% R$m$census_id), ]),
  list("wife_blind", "excluding links whose wife contradicts", B$a[B$a$couple & !(B$a$is_voortrekker & B$a$state %in% "contradicts"), ]))
for (s in samp) for (y in c("household_size", "settler_children", "wealth_index", "total_slaves")) {
  r <- fe(s[[3]], y)
  rec[[length(rec) + 1]] <- cbind(data.frame(run = s[[1]], links = s[[2]], outcome = y), r,
                                  p_wcb = if (y %in% c("household_size", "settler_children")) wcb_p(s[[3]], y) else NA)
}
write.csv(bind_rows(rec), file.path(out, "couples_reconciliation.csv"), row.names = FALSE)

# ---- 3: genealogy marriage and linkage ----------------------------------------
cw <- read.csv("data/inputs/genealogy_row_crosswalk.csv")
source("code/linkage.R")
sp <- vt_spouse_table("data/raw/Voortrekkers 2.xlsx")
cyear <- function(d) ifelse(grepl("SOMERSET|CRADOCK", toupper(d)), 1823, ifelse(grepl("CLANWILLIAM|WORCESTER", toupper(d)), 1824, 1825))
g <- cw %>% mutate(census_year = cyear(distrik)) %>%
  left_join(sp %>% group_by(vt_source_row) %>% summarise(first_marriage = suppressWarnings(min(sp_year, na.rm = TRUE)),
                                                         any_spouse = TRUE, .groups = "drop"), by = "vt_source_row") %>%
  mutate(first_marriage = ifelse(is.finite(first_marriage), first_marriage, NA),
         married = case_when(!is.na(first_marriage) & first_marriage <= census_year ~ "married by census year",
                             !is.na(first_marriage) ~ "married after census year",
                             any_spouse %in% TRUE ~ "spouse recorded, year unknown",
                             TRUE ~ "no spouse recorded"),
         linked = row_id %in% R$m$row_id, linked_blind = row_id %in% B$m$row_id,
         birth = suppressWarnings(as.numeric(birth_yr)),
         cohort = cut(birth, c(-Inf, 1784, 1794, 1799, 1804, 1810), labels = c("before 1785", "1785-94", "1795-99", "1800-04", "1805-10")))
tab <- function(by) g %>% group_by(across(all_of(by))) %>%
  summarise(men = n(), linked_share = mean(linked), linked_blind_share = mean(linked_blind), linked = sum(linked), .groups = "drop")
write.csv(tab("married"), file.path(out, "marriage_linkage.csv"), row.names = FALSE)
write.csv(tab(c("cohort", "married")), file.path(out, "marriage_linkage_by_cohort.csv"), row.names = FALSE)
write.csv(tab(c("distrik", "married")), file.path(out, "marriage_linkage_by_district.csv"), row.names = FALSE)
cat("Couples analysis complete\n")
print(bind_rows(dec)[, c("run", "outcome", "fe_unconditional", "fe_controlling_couple", "fe_couples_only",
                         "share_married_margin_fe", "share_married_margin_kitagawa")])
print(bind_rows(rec) %>% filter(outcome == "household_size") %>% select(run, links, coef, lo, hi, p_hc1, p_wcb, n_vt))
print(tab("married"))

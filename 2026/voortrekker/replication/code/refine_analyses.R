# Exploratory checks reported in the Online Appendix, run on the links of both linkages after
# the main estimation. No outcome changes any link. Writes output/refine_analyses/:
#   married_share_calibration.csv  implied married share among all Trekkers by relative recovery (R1)
#   wealth_equivalence_clustered.csv  90% intervals for the wealth index, HC1 / CR1 / CR2 (R2)
#   departure_window.csv           estimates with only 1835-1840 departures as Trekkers (R3)
#   comp_retention*.csv            claim reconciliation by Trek status, district, departure (R5)
#   probit_separation.csv          probit Models 3 and 5 without the zero-outcome district (R6)
#   loss_flexible.csv              district-FE LPM with loss quartiles and a spline (R7)
#   tiers_within_couples.csv       classifier and adjudicated couples vs married controls (R8)
#   departure_cohorts.csv          district-adjusted wealth by departure cohort (R9)
#   nonlocal_birth.csv             nonlocal birth and wealth with birth-decade controls (R10)
#   duplicate_controls*.csv        repeated control households across returns (R11)
# Run from the replication folder after the pipeline:  Rscript code/refine_analyses.R
suppressMessages({library(dplyr); library(tidyr); library(sandwich); library(lmtest); library(clubSandwich)
  library(splines); library(stringdist)})
out <- "output/refine_analyses"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20261007)
w <- function(x, f) write.csv(x, file.path(out, f), row.names = FALSE)

fe <- function(df, y, extra = "") {
  df <- df[!is.na(df[[y]]), ]
  m <- lm(as.formula(paste0(y, " ~ is_voortrekker", extra, " + factor(district)")), data = df)
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))["is_voortrekkerTRUE", ]
  data.frame(coef = ct[1], se = ct[2], lo = ct[1] - 1.96 * ct[2], hi = ct[1] + 1.96 * ct[2], p_hc1 = ct[4],
             n = nrow(df), n_vt = sum(df$is_voortrekker), row.names = NULL)
}
load_run <- function(run) {
  a <- read.csv(file.path(if (run == "spouse_assisted") "output" else "output_wife_blind", "tables/analysis_dataset.csv"))
  a$is_voortrekker <- toupper(as.character(a$is_voortrekker)) %in% c("TRUE", "1")
  nm <- read.csv(file.path(if (run == "spouse_assisted") "output" else "output_wife_blind", "tables/census_parsed_names.csv")) %>%
    select(district, source_row, hr = head_role, sp = spouse_name_raw)
  a <- a %>% left_join(nm, by = c("district", "source_row")) %>%
    mutate(couple = hr %in% "male" & !is.na(sp) & sp != "")
  fp <- read.csv(file.path(if (run == "spouse_assisted") "output" else "output_wife_blind", "tables/final_pairs_complete.csv"))
  a$move_year <- suppressWarnings(as.numeric(fp$move_year[match(a$census_id, fp$census_id)]))
  a
}
A <- load_run("spouse_assisted"); B <- load_run("wife_blind")
st <- readRDS("output/tables/final_analysis_state.rds")
OUTC <- c("household_size", "settler_children", "wealth_index", "total_slaves")

# ---- R1: married-share calibration ----------------------------------------------------------------
# Among linked Trekkers a share s_L are couples. If married men are recovered r times as often as
# others, the implied share among all Trekkers is s = s_L / (s_L + r (1 - s_L)).
dec <- read.csv("output/couples_analysis/decomposition.csv")
ml <- read.csv("output/couples_analysis/marriage_linkage.csv")
cal <- list()
for (run in c("spouse_assisted", "wife_blind")) {
  d0 <- dec[dec$run == run & dec$outcome == "household_size", ]
  sL <- d0$couple_share_trekkers; sC <- d0$couple_share_controls_district_weighted
  col <- if (run == "spouse_assisted") "linked_share" else "linked_blind_share"
  r_obs <- ml[[col]][ml$married == "married by census year"] / ml[[col]][ml$married == "married after census year"]
  r_tip <- (sL / sC - sL) / (1 - sL)
  rr <- c(1, 1.5, 2, r_obs, r_tip, 3)
  cal[[run]] <- data.frame(run = run, r = rr,
    label = c("equal recovery", "", "", "observed genealogy ratio", "tipping point (equals control share)", ""),
    implied_trekker_couple_share = sL / (sL + rr * (1 - sL)), linked_trekker_couple_share = sL, control_couple_share = sC)
}
cal <- bind_rows(cal)
w(cal, "married_share_calibration.csv")

# ---- R2: wealth equivalence under district clustering --------------------------------------------
sd_w <- sd(A$wealth_index, na.rm = TRUE)
a2 <- A[!is.na(A$wealth_index), ]
m2 <- lm(wealth_index ~ is_voortrekker + factor(district), data = a2)
b2 <- coef(m2)[["is_voortrekkerTRUE"]]; G <- length(unique(a2$district))
se_hc1 <- sqrt(vcovHC(m2, type = "HC1")["is_voortrekkerTRUE", "is_voortrekkerTRUE"])
se_cr1 <- sqrt(vcovCL(m2, cluster = ~district, type = "HC1")["is_voortrekkerTRUE", "is_voortrekkerTRUE"])
cr2 <- coef_test(m2, vcov = "CR2", cluster = a2$district, test = "Satterthwaite", coefs = "is_voortrekkerTRUE")
ci <- function(se, q) c(b2 - q * se, b2 + q * se) / sd_w
eq <- bind_rows(
  data.frame(inference = "HC1, normal", se = se_hc1, df = Inf, t(ci(se_hc1, qnorm(0.95)))),
  data.frame(inference = "CR1 district, normal", se = se_cr1, df = Inf, t(ci(se_cr1, qnorm(0.95)))),
  data.frame(inference = "CR1 district, t(G-1)", se = se_cr1, df = G - 1, t(ci(se_cr1, qt(0.95, G - 1)))),
  data.frame(inference = "CR2 district, Satterthwaite", se = cr2$SE, df = cr2$df_Satt, t(ci(cr2$SE, qt(0.95, cr2$df_Satt))))) %>%
  rename(lo90_sd = X1, hi90_sd = X2) %>%
  mutate(coef_sd = b2 / sd_w, equivalent_within_0.10 = lo90_sd > -0.10 & hi90_sd < 0.10, clusters = G, sd_index = sd_w)
w(eq, "wealth_equivalence_clustered.csv")

# ---- R3: departure window 1835-1840 ---------------------------------------------------------------
dw <- list()
for (run in c("spouse_assisted", "wife_blind")) {
  a <- if (run == "spouse_assisted") A else B
  keep <- !a$is_voortrekker | (!is.na(a$move_year) & a$move_year >= 1835 & a$move_year <= 1840)
  for (smp in c("all links", "departures 1835-1840")) {
    d <- if (smp == "all links") a else a[keep, ]
    for (y in OUTC) dw[[length(dw) + 1]] <- cbind(data.frame(run = run, sample = smp, outcome = y), fe(d, y))
    u <- fe(d, "household_size"); cc <- fe(d, "household_size", " + couple")
    dw[[length(dw) + 1]] <- data.frame(run = run, sample = smp, outcome = "household_size controlling couple",
                                       coef = cc$coef, se = cc$se, lo = cc$lo, hi = cc$hi, p_hc1 = cc$p_hc1, n = cc$n, n_vt = cc$n_vt)
    dw[[length(dw) + 1]] <- data.frame(run = run, sample = smp, outcome = "couple share among Trekkers",
                                       coef = mean(d$couple[d$is_voortrekker]), se = NA, lo = NA, hi = NA, p_hc1 = NA,
                                       n = nrow(d), n_vt = sum(d$is_voortrekker))
  }
}
w(bind_rows(dw), "departure_window.csv")
yr <- A$move_year[A$is_voortrekker]
w(data.frame(category = c("1835-1840", "after 1840", "before 1835", "missing or malformed"),
             n = c(sum(yr >= 1835 & yr <= 1840, na.rm = TRUE), sum(yr > 1840, na.rm = TRUE),
                   sum(yr < 1835 & yr > 1800, na.rm = TRUE), sum(is.na(yr) | yr < 1800))), "departure_years.csv")

# ---- R4: linked owners (the reviewed owner links) ------------------------------------
owners <- st$slave_owners
owners$owner_key <- paste(owners$surname_std, owners$first_name_std, owners$district_std, sep = "|")
excluded <- read.csv("data/inputs/owner_exclusions.csv")$owner_key
owners <- owners[!owners$owner_key %in% excluded, ]
ow <- st$owners_with_valuation
em <- read.csv("output/tables/voortrekker_emancipation_matches.csv")
mv <- suppressWarnings(as.numeric(st$vt_adults$move_year[match(em$vt_row_id, st$vt_adults$row_id)]))
own_lv <- data.frame(owner_key = em$owner_key, move_year = mv) %>% group_by(owner_key) %>%
  summarise(move_year = suppressWarnings(min(move_year, na.rm = TRUE)), .groups = "drop") %>%
  mutate(move_year = ifelse(is.finite(move_year), move_year, NA))
ame <- function(mod, term, rows = NULL, vc = vcov(mod)) {
  b <- coef(mod); b <- b[!is.na(b)]
  X <- model.matrix(mod)[, names(b), drop = FALSE]; if (!is.null(rows)) X <- X[rows, , drop = FALSE]
  V <- vc[names(b), names(b)]; xb <- as.vector(X %*% b); phi <- dnorm(xb)
  grad <- b[[term]] * colMeans(-xb * phi * X); grad[term] <- grad[term] + mean(phi)
  c(ame = 100 * mean(phi) * b[[term]], se = 100 * sqrt(as.numeric(t(grad) %*% V %*% grad)))
}
zero_districts <- function(d) names(which(tapply(d$is_voortrekker, d$district_std, sum) == 0))

# ---- R5: claim reconciliation (retention) by Trek status -------------------------------------------
cov <- read.csv("output/tables/compensation_claim_coverage.csv")
cov$reason <- with(cov, case_when(
  is.na(UCL) ~ "no claim number", source_scope_unresolved ~ "claim scope unresolved in source",
  n_owner_groups_for_claim != 1 ~ "claim shared by several owner groups",
  n_valued != slave_records ~ "valuation missing for some enslaved persons",
  n_paid != 1 ~ "payment missing or recorded more than once",
  n_counts != 1 | is.na(claim_num_slaves) | claim_num_slaves != slave_records ~ "slave count inconsistent",
  TRUE ~ "reconciled"))
cov$owner_key <- paste(toupper(trimws(cov$Owner_surname)), toupper(trimws(cov$Owner_name)),
                       ifelse(cov$District_name == "Graaff Reinet", "Graaff-Reinet", cov$District_name), sep = "|")
okeys <- owners$owner_key
cov <- cov[cov$owner_key %in% okeys, ]
first_reason <- cov %>% group_by(owner_key) %>%
  summarise(reason = { r <- setdiff(unique(reason), "reconciled"); if (length(r)) r[1] else "reconciled" }, .groups = "drop")
o5 <- data.frame(owner_key = okeys, district = owners$district_std) %>%
  left_join(first_reason, by = "owner_key") %>%
  mutate(trekker = owner_key %in% own_lv$owner_key,
         in_sample = owner_key %in% ow$owner_key,
         reason = ifelse(reason == "reconciled" & !in_sample, "reconciled, valuation not positive", reason))
w(o5 %>% count(trekker, reason) %>% group_by(trekker) %>% mutate(share = n / sum(n)), "comp_retention_reasons.csv")
w(o5 %>% group_by(district, trekker) %>% summarise(owners = n(), retained = sum(in_sample), share = mean(in_sample), .groups = "drop"),
  "comp_retention_by_district.csv")
rl <- lm(in_sample ~ trekker + factor(district), data = o5)
rlc <- coeftest(rl, vcov = vcovHC(rl, type = "HC1"))["trekkerTRUE", ]
o5$move_year <- own_lv$move_year[match(o5$owner_key, own_lv$owner_key)]
coh <- o5 %>% filter(trekker) %>%
  mutate(cohort = case_when(is.na(move_year) ~ "missing", move_year < 1837 ~ "before 1837", move_year <= 1838 ~ "1837-1838",
                            move_year <= 1840 ~ "1839-1840", TRUE ~ "after 1840")) %>%
  group_by(cohort) %>% summarise(owners = n(), retained = sum(in_sample), share = mean(in_sample), .groups = "drop")
w(bind_rows(data.frame(cohort = "within-district difference (Trekker - other), pp", owners = NA, retained = NA,
                       share = 100 * rlc[1], se = 100 * rlc[2], p = rlc[4]), coh), "comp_retention_by_cohort.csv")

# ---- R6: probit separation ----------------------------------------------------------------------------
zd <- zero_districts(ow)
ps <- list()
for (mn in c("Model 3", "Model 5")) {
  f <- if (mn == "Model 3") is_voortrekker ~ loss_pct_z + num_slaves_z + factor(district_std) else
    is_voortrekker ~ loss_pct_z + num_slaves_z + log_valuation_z + factor(district_std)
  full <- glm(f, data = ow, family = binomial("probit"))
  sub <- ow[!ow$district_std %in% zd, ]; drop <- glm(f, data = sub, family = binomial("probit"))
  lp <- lm(update(f, . ~ .), data = sub)
  in_sup <- which(!ow$district_std %in% zd)
  for (spec in list(list("original (all owners, model-based)", full, vcov(full), NULL),
                    list("zero-outcome districts dropped, model-based", drop, vcov(drop), NULL),
                    list("zero-outcome districts dropped, sandwich HC0", drop, sandwich(drop), NULL),
                    list("original model, AME over owners in districts with Trekkers", full, vcov(full), in_sup))) {
    mod <- spec[[2]]; V <- spec[[3]]
    zz <- coef(mod)[["loss_pct_z"]] / sqrt(V["loss_pct_z", "loss_pct_z"])
    a1 <- ame(mod, "loss_pct_z", spec[[4]], V); a2 <- ame(mod, "num_slaves_z", spec[[4]], V)
    ps[[length(ps) + 1]] <- data.frame(model = mn, specification = spec[[1]], n = nobs(mod),
      loss_coef = coef(mod)[["loss_pct_z"]], loss_p = 2 * pnorm(-abs(zz)),
      ame_loss_pp = a1[["ame"]], ame_loss_lo = a1[["ame"]] - 1.96 * a1[["se"]], ame_loss_hi = a1[["ame"]] + 1.96 * a1[["se"]],
      ame_slaves_pp = a2[["ame"]], ame_slaves_lo = a2[["ame"]] - 1.96 * a2[["se"]], ame_slaves_hi = a2[["ame"]] + 1.96 * a2[["se"]],
      dropped_districts = paste(zd, collapse = "; "))
  }
  ct <- coeftest(lp, vcov = vcovHC(lp, type = "HC1"))
  ps[[length(ps) + 1]] <- data.frame(model = mn, specification = "LPM on the same sample (HC1)", n = nobs(lp),
    loss_coef = NA, loss_p = ct["loss_pct_z", 4], ame_loss_pp = 100 * ct["loss_pct_z", 1],
    ame_loss_lo = 100 * (ct["loss_pct_z", 1] - 1.96 * ct["loss_pct_z", 2]), ame_loss_hi = 100 * (ct["loss_pct_z", 1] + 1.96 * ct["loss_pct_z", 2]),
    ame_slaves_pp = 100 * ct["num_slaves_z", 1], ame_slaves_lo = 100 * (ct["num_slaves_z", 1] - 1.96 * ct["num_slaves_z", 2]),
    ame_slaves_hi = 100 * (ct["num_slaves_z", 1] + 1.96 * ct["num_slaves_z", 2]), dropped_districts = paste(zd, collapse = "; "))
}
w(bind_rows(ps), "probit_separation.csv")

# ---- R7: flexible loss within districts ------------------------------------------------------------
qb <- quantile(ow$loss_pct, c(0.25, 0.5, 0.75), na.rm = TRUE)
ow$loss_q <- cut(ow$loss_pct, c(-Inf, qb, Inf), labels = c("Q1", "Q2", "Q3", "Q4"))
fl <- list()
for (fx in c("none", "district")) {
  f <- if (fx == "none") is_voortrekker ~ loss_q else is_voortrekker ~ loss_q + num_slaves_z + factor(district_std)
  m <- lm(f, data = ow); V <- vcovHC(m, type = "HC1"); ct <- coeftest(m, vcov = V)
  jt <- waldtest(m, update(m, . ~ . - loss_q), vcov = V)
  for (q in c("Q2", "Q3", "Q4")) fl[[length(fl) + 1]] <- data.frame(model = paste("quartiles,", ifelse(fx == "none", "pooled", "district FE + slaves")),
    term = paste(q, "vs Q1"), coef_pp = 100 * ct[paste0("loss_q", q), 1], lo_pp = 100 * (ct[paste0("loss_q", q), 1] - 1.96 * ct[paste0("loss_q", q), 2]),
    hi_pp = 100 * (ct[paste0("loss_q", q), 1] + 1.96 * ct[paste0("loss_q", q), 2]), p = ct[paste0("loss_q", q), 4], joint_p = jt$`Pr(>F)`[2])
}
kn <- quantile(ow$loss_pct, c(1, 2) / 3, na.rm = TRUE)
ms <- lm(is_voortrekker ~ ns(loss_pct, knots = kn) + num_slaves_z + factor(district_std), data = ow)
Vs <- vcovHC(ms, type = "HC1"); js <- waldtest(ms, lm(is_voortrekker ~ num_slaves_z + factor(district_std), data = ow), vcov = Vs)
grid <- data.frame(loss_pct = quantile(ow$loss_pct, c(0.125, 0.375, 0.625, 0.875), na.rm = TRUE))
Xg <- model.matrix(~ ns(loss_pct, knots = kn, Boundary.knots = range(ow$loss_pct, na.rm = TRUE)), grid)[, -1]
bs <- coef(ms)[2:4]; Vb <- Vs[2:4, 2:4]; base <- Xg[1, ]
for (k in 2:4) {
  dlt <- Xg[k, ] - base
  fl[[length(fl) + 1]] <- data.frame(model = "natural spline, district FE + slaves", term = paste0("quartile midpoint ", k, " vs 1"),
    coef_pp = 100 * sum(dlt * bs), lo_pp = 100 * (sum(dlt * bs) - 1.96 * sqrt(t(dlt) %*% Vb %*% dlt)),
    hi_pp = 100 * (sum(dlt * bs) + 1.96 * sqrt(t(dlt) %*% Vb %*% dlt)), p = NA, joint_p = js$`Pr(>F)`[2])
}
w(bind_rows(fl), "loss_flexible.csv")

# ---- R8: tiers within couples ----------------------------------------------------------------------------
recon <- read.csv("data/linkage/link_decisions.csv")
prop <- paste(recon$row_id, recon$census_id)[recon$classifier == "proposed"]
bm <- st$best_matches
tier <- ifelse(paste(bm$row_id, bm$census_id) %in% prop, "classifier-proposed", "adjudication only")
A$tier <- tier[match(A$census_id, bm$census_id)]
tw <- list()
for (tr in c("classifier-proposed", "adjudication only")) {
  d <- A[A$couple & (!A$is_voortrekker | A$tier %in% tr), ]
  for (y in c(OUTC, "cattle", "sheep")) tw[[length(tw) + 1]] <- cbind(data.frame(tier = tr, outcome = y), fe(d, y))
}
w(bind_rows(tw), "tiers_within_couples.csv")

# ---- R9: departure cohorts -------------------------------------------------------------------------------
tv <- A[A$is_voortrekker & !is.na(A$move_year) & A$move_year >= 1835 & A$move_year <= 1845 & !is.na(A$wealth_index), ]
tv$cohort <- factor(case_when(tv$move_year <= 1836 ~ "1835-1836", tv$move_year == 1837 ~ "1837", tv$move_year == 1838 ~ "1838",
                              tv$move_year <= 1840 ~ "1839-1840", TRUE ~ "1841-1845"),
                    levels = c("1835-1836", "1837", "1838", "1839-1840", "1841-1845"))
mc <- lm(wealth_index ~ cohort + factor(district), data = tv); Vc <- vcovHC(mc, type = "HC1")
jc <- waldtest(mc, lm(wealth_index ~ factor(district), data = tv), vcov = Vc)
mu <- lm(wealth_index ~ cohort, data = tv); ju <- waldtest(mu, lm(wealth_index ~ 1, data = tv), vcov = vcovHC(mu, type = "HC1"))
cc <- coeftest(mc, vcov = Vc)
w(data.frame(cohort = levels(tv$cohort), n = as.vector(table(tv$cohort)),
             raw_mean = as.vector(tapply(tv$wealth_index, tv$cohort, mean)),
             adj_diff_vs_first = c(0, cc[paste0("cohort", levels(tv$cohort)[-1]), 1]),
             adj_se = c(NA, cc[paste0("cohort", levels(tv$cohort)[-1]), 2]),
             joint_p_district_adjusted = jc$`Pr(>F)`[2], joint_p_unadjusted = ju$`Pr(>F)`[2]), "departure_cohorts.csv")

# ---- R10: nonlocal birth with birth-decade controls ---------------------------------------------------
td <- st$tenure_data
td$birth_decade <- factor(pmin(pmax(floor(td$birth_yr / 10) * 10, 1760), 1800))
nb <- list()
for (spec in c("district FE (paper)", "district FE + birth decade")) {
  f <- if (spec == "district FE (paper)") wealth_index ~ born_outside + district_f else wealth_index ~ born_outside + birth_decade + district_f
  m <- lm(f, data = td); ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  r <- grep("^born_outside", rownames(ct))
  nb[[spec]] <- data.frame(specification = spec, coef = ct[r, 1], se = ct[r, 2], p = ct[r, 4], n = nobs(m))
}
w(bind_rows(nb), "nonlocal_birth.csv")

# ---- R11: repeated control households across returns --------------------------------------------------
cn <- A %>% filter(!is_voortrekker, couple) %>%
  mutate(ret = sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", source_key), yr = as.numeric(sub(".*(18[0-9]{2}).*", "\\1", ret)),
         h = tolower(gsub("[^A-Za-z ,]", "", head_name_raw)), s = tolower(gsub("[^A-Za-z ,]", "", sp)),
         hs = sub(",.*", "", h))
pairs <- cn %>% select(census_id, ret, yr, h, s, hs) %>%
  inner_join(cn %>% select(census_id2 = census_id, ret2 = ret, yr2 = yr, h2 = h, s2 = s, hs2 = hs), by = c("hs" = "hs2"),
             relationship = "many-to-many") %>%
  filter(census_id < census_id2, ret != ret2) %>%
  mutate(jh = stringsim(h, h2, method = "jw", p = 0.1), js = stringsim(s, s2, method = "jw", p = 0.1)) %>%
  filter(jh >= 0.92, js >= 0.92)
w(pairs, "duplicate_controls_pairs.csv")
# Keep one record per group: the latest return year (1825 first), then the lower census_id.
drop_ids <- unique(ifelse(pairs$yr2 > pairs$yr | (pairs$yr2 == pairs$yr & pairs$census_id2 < pairs$census_id), pairs$census_id, pairs$census_id2))
dd <- list()
for (smp in c("all controls", "one record per repeated household")) {
  d <- if (smp == "all controls") A else A[!(A$census_id %in% drop_ids), ]
  for (y in OUTC) dd[[length(dd) + 1]] <- cbind(data.frame(sample = smp, outcome = y, dropped = ifelse(smp == "all controls", 0, length(drop_ids))), fe(d, y))
}
w(bind_rows(dd), "duplicate_controls.csv")
cat("refine analyses written to", out, "\n")

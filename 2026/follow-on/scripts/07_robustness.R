# =============================================================================
#  07_robustness.R  —  ROBUSTNESS CHECKS FOR THE DISCONTINUITY DESIGN
#
#  A discontinuity design is only credible if it survives a set of standard
#  checks. This script runs them in the order a referee would ask for them.
#
#   1. Manipulation. Can teams place themselves just above the cutoff? A
#      captain can declare an innings closed, which gives partial control over
#      the exact lead, so this matters here more than in most applications.
#   2. Partial-identification bounds under one-sided manipulation.
#   3. Honest confidence intervals for a discrete running variable.
#   4. Placebo cutoffs, including the 1980 rule change.
#   5. Bandwidth sensitivity and a "donut" that drops the closest matches.
#   6. Balance of predetermined characteristics at the cutoff.
#   7. Local randomisation inference in a narrow window.
#
#  OUTPUT
#     data/processed/robustness.rds ; output/logs/07_robustness.txt
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)
suppressWarnings(suppressMessages({
  library(rdrobust); library(rddensity); library(rdbounds); library(rdlocrand)
}))
set.seed(SEED)

d <- readRDS(file.path(DDIR, "matches.rds")) %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive,
         !is.na(margin), !is.na(dist), !is.na(lead_win))

sink(file.path(LOGDIR, "07_robustness.txt"), split = TRUE)

# =============================================================================
#  1. MANIPULATION OF THE RUNNING VARIABLE
# =============================================================================
cat("########## 1. MANIPULATION TESTS ##########\n")
cat("If captains engineered eligibility we would see excess mass just ABOVE\n")
cat("the cutoff, because a captain who wants to enforce would bat on until the\n")
cat("lead reaches the margin, then declare.\n\n")

cat("--- density of the running variable within 15 runs of the cutoff ---\n")
print(d %>% filter(abs(dist) <= 15) %>% count(dist) %>% as.data.frame())

cat("\n--- McCrary-style density test (Cattaneo, Jansson and Ma) ---\n")
dens <- try(rddensity(X = d$dist, c = 0), silent = TRUE)
if (!inherits(dens, "try-error")) {
  cat(sprintf("   test statistic = %.3f, p = %.3f\n",
              dens$test$t_jk, dens$test$p_jk))
} else cat("   rddensity failed\n")

cat("\n--- binomial test on counts in symmetric windows ---\n")
cat("For a discrete running variable this is more reliable than a density\n")
cat("estimator. Under no manipulation, counts either side should be similar.\n\n")
for (w in c(3, 5, 10, 15, 20)) {
  lo <- sum(d$dist >= -w & d$dist < 0); hi <- sum(d$dist >= 0 & d$dist < w)
  bt <- binom.test(hi, lo + hi, 0.5)
  cat(sprintf("   window +/-%2d runs: below = %4d, above = %4d, p = %.3f\n",
              w, lo, hi, bt$p.value))
}

cat("\n--- WHO could be manipulating, and how? ---\n")
cat("Only a FIRST-INNINGS declaration can place a match on a chosen side of\n")
cat("the line. The side batting first declares before it has seen the\n")
cat("opponent's first innings, so it cannot target the eventual lead. The side\n")
cat("batting second knows the deficit exactly and has a clear motive: to avoid\n")
cat("being made to follow on. So any manipulation should come from the second\n")
cat("side, and should push matches to just BELOW the cutoff.\n\n")
print(d %>% filter(abs(dist) <= 20) %>%
        group_by(side = ifelse(dist >= 0, "above (eligible)", "below")) %>%
        summarise(n = n(),
                  first_side_declared = round(mean(lead_declared_1, na.rm = TRUE), 3),
                  second_side_declared = round(mean(trail_declared_1, na.rm = TRUE), 3),
                  .groups = "drop") %>% as.data.frame())

cat("\nA declaration by the side batting second is a concession of the\n")
cat("follow-on, so it should be rare below the line and is not the mechanism.\n")
cat("The relevant behaviour is batting on to save the follow-on, which shows\n")
cat("up as excess mass at small negative values of the running variable.\n")

cat("\n--- the excess mass, in detail ---\n")
print(d %>% filter(dist >= -6, dist <= 5) %>%
        group_by(dist) %>%
        summarise(n = n(),
                  second_side_all_out = round(mean(trail_wickets_1 >= 10,
                                                   na.rm = TRUE), 3),
                  .groups = "drop") %>% as.data.frame())

# =============================================================================
#  2. BOUNDS UNDER ONE-SIDED MANIPULATION
# =============================================================================
cat("\n\n########## 2. MANIPULATION-ROBUST BOUNDS ##########\n")
cat("The excess mass lies below the cutoff: the side batting second can avoid\n")
cat("eligibility by saving the follow-on. We therefore reverse the running\n")
cat("variable and treatment coding before applying Gerard, Rokkanen and Rothe\n")
cat("(2020). The package estimates the effect of NOT enforcing; the bounds\n")
cat("reported below reverse its sign to recover the effect of enforcing.\n\n")

# Binary outcomes are treated as discrete. The first bandwidth estimates the
# density discontinuity and the second estimates the conditional distributions.
# We show a grid because the package requires both choices from the researcher.
manip_bounds <- map_dfr(c(10, 15, 20), function(h_density) {
  map_dfr(c(60, 87, 120), function(h_outcome) {
    rb <- rdbounds::rdbounds(
      y = d$lead_win, x = -d$dist, treatment = 1 - d$enforced, c = 0,
      discrete_x = TRUE, discrete_y = TRUE,
      bwsx = c(h_density, h_outcome), kernel = "triangular", orders = 1,
      evaluation_ys = c(0, 1), refinement_A = TRUE, refinement_B = TRUE,
      right_effects = TRUE, yextremes = c(0, 1),
      num_bootstraps = c(0, 0), parallelize = FALSE)
    b <- unlist(rb$estimates["TE_FRD_bounds", 1], use.names = FALSE)
    br <- unlist(rb$estimates["TE_FRD_bounds_refinementA", 1],
                 use.names = FALSE)
    tibble(density_bw = h_density, outcome_bw = h_outcome,
           manipulated_share = unlist(rb$estimates["tau_hat", 1],
                                      use.names = FALSE),
           lower = -b[2], upper = -b[1],
           lower_refinement_a = -br[2], upper_refinement_a = -br[1])
  })
})
print(as.data.frame(manip_bounds %>%
  mutate(across(where(is.numeric), ~round(., 4)))))
cat("\nThese are point-identified sets, not confidence intervals. Refinement A\n")
cat("assumes units able to avoid eligibility are at least as likely not to\n")
cat("enforce as units unable to manipulate.\n")

# =============================================================================
#  3. HONEST CONFIDENCE INTERVALS (discrete running variable)
# =============================================================================
cat("\n\n########## 3. HONEST CONFIDENCE INTERVALS ##########\n")
cat("The running variable is a whole number of runs, so it has mass points.\n")
cat("Kolesar and Rothe (2018) show conventional intervals can undercover in\n")
cat("that case, and give intervals that are honest over a class of functions\n")
cat("with second derivative bounded by M.\n\n")

if (requireNamespace("RDHonest", quietly = TRUE)) {
  library(RDHonest)

  cat("--- (i) sharp: the effect of ELIGIBILITY on winning ---\n")
  for (mm in c(0.001, 0.002, 0.005)) {
    h <- try(RDHonest::RDHonest(lead_win ~ dist, data = d, cutoff = 0,
                                M = mm, kern = "triangular"), silent = TRUE)
    if (!inherits(h, "try-error")) {
      co <- h$coefficients
      cat(sprintf("   M = %-6.3f  estimate %+0.4f   honest CI [%+.3f, %+.3f]\n",
                  mm, co$estimate, co$conf.low, co$conf.high))
    }
  }
  hM <- try(RDHonest::RDHonest(lead_win ~ dist, data = d, cutoff = 0,
                               kern = "triangular"), silent = TRUE)
  if (!inherits(hM, "try-error"))
    cat(sprintf("   data-driven M: estimate %+0.4f, CI [%+.3f, %+.3f]\n",
                hM$coefficients$estimate, hM$coefficients$conf.low,
                hM$coefficients$conf.high))

  #  The sharp version above bounds the effect of ELIGIBILITY. The headline
  #  estimate in the paper is the effect of ENFORCING, which is a fuzzy
  #  parameter. RDHonest handles this with the syntax  outcome | treatment ~
  #  running, and requires a curvature bound for the outcome and one for the
  #  first stage. We report a grid rather than a single value, because a
  #  curvature estimated from the data is not an upper bound on curvature.
  cat("\n--- (ii) fuzzy: the effect of ENFORCING on winning ---\n")
  cat("    M is a pair: (bound for the outcome, bound for the first stage)\n\n")
  for (mm in list(c(0.001, 0.002), c(0.002, 0.004), c(0.004, 0.008))) {
    hf <- try(RDHonest::RDHonest(lead_win | enforced ~ dist, data = d,
                                 cutoff = 0, M = mm, kern = "triangular",
                                 T0 = 0.25), silent = TRUE)
    if (!inherits(hf, "try-error")) {
      co <- hf$coefficients
      cat(sprintf("   M = (%.3f, %.3f)  estimate %+0.4f  honest CI [%+.3f, %+.3f]\n",
                  mm[1], mm[2], co$estimate, co$conf.low, co$conf.high))
    } else cat(sprintf("   M = (%.3f, %.3f)  failed\n", mm[1], mm[2]))
  }
  hfM <- try(RDHonest::RDHonest(lead_win | enforced ~ dist, data = d,
                                cutoff = 0, kern = "triangular", T0 = 0.25),
             silent = TRUE)
  if (!inherits(hfM, "try-error"))
    cat(sprintf("   data-driven M: estimate %+0.4f, CI [%+.3f, %+.3f]\n",
                hfM$coefficients$estimate, hfM$coefficients$conf.low,
                hfM$coefficients$conf.high))
} else cat("   RDHonest not installed\n")

# =============================================================================
#  4. PLACEBO CUTOFFS AND THE 1980 RULE CHANGE
# =============================================================================
cat("\n\n########## 4. PLACEBO CUTOFFS ##########\n")
cat("There should be a jump in enforcement only where the Law puts one.\n\n")

#  IMPORTANT. When testing a placebo cutoff we must keep the TRUE cutoff out of
#  the estimation window. Otherwise the local polynomial is fitted across a real
#  discontinuity and reports a spurious jump at the placebo point. We therefore
#  restrict each placebo to leads strictly on one side of the true line, and cap
#  the bandwidth so the window cannot reach it.
fs_at <- function(dat, cut, lab, true_cut = NULL) {
  if (!is.null(true_cut)) {
    # keep the true discontinuity out of the estimation window
    if (cut > true_cut) dat <- dat %>% filter(lead > true_cut + 10)
    if (cut < true_cut) dat <- dat %>% filter(lead < true_cut - 10)
  }
  if (nrow(dat) < 120) return(tibble(sample = lab, cutoff = cut,
        estimate = NA_real_, se = NA_real_, p = NA_real_, n = nrow(dat)))
  o <- if (is.null(true_cut)) {
    try(rdrobust(y = dat$enforced, x = dat$lead, c = cut, p = 1,
                 kernel = "triangular", bwselect = "mserd",
                 masspoints = "adjust"), silent = TRUE)
  } else {
    hmax <- abs(cut - true_cut) - 10          # cannot reach the true cutoff
    try(rdrobust(y = dat$enforced, x = dat$lead, c = cut, p = 1,
                 kernel = "triangular", h = hmax,
                 masspoints = "adjust"), silent = TRUE)
  }
  if (inherits(o, "try-error")) return(tibble(sample = lab, cutoff = cut,
        estimate = NA_real_, se = NA_real_, p = NA_real_, n = nrow(dat)))
  tibble(sample = lab, cutoff = cut,
         estimate = o$coef["Conventional", 1],
         se = o$se["Robust", 1], p = o$pv["Robust", 1],
         n = sum(o$N_h))
}

cat("--- Tests, before and after the 1980 change ---\n")
cat("Before 1980 the Test line was 150; after 1980 it was 200.\n")
cat("So we expect a jump at 150 only before, and at 200 only after.\n\n")
tpre  <- d %>% filter(comp == "Test", yr < 1980, days_for_match >= 5)
tpost <- d %>% filter(comp == "Test", yr >= 1980, days_for_match >= 5)
pl_test <- bind_rows(
  fs_at(tpre,  150, "Tests before 1980 [TRUE cutoff]"),
  fs_at(tpre,  200, "Tests before 1980 [placebo]", true_cut = 150),
  fs_at(tpost, 150, "Tests from 1980 [placebo]",   true_cut = 200),
  fs_at(tpost, 200, "Tests from 1980 [TRUE cutoff]"))
print(as.data.frame(pl_test %>% mutate(across(where(is.numeric), ~round(., 4)))))

cat("\n--- Domestic first-class, same two periods ---\n")
cat("The domestic line stayed at 150 throughout, so the jump should stay at\n")
cat("150 in both periods and there should be none at 200.\n\n")
dpre  <- d %>% filter(comp != "Test", yr < 1980, days_for_match %in% 3:4)
dpost <- d %>% filter(comp != "Test", yr >= 1980, days_for_match %in% 3:4)
pl_dom <- bind_rows(
  fs_at(dpre,  150, "Domestic before 1980 [TRUE cutoff]"),
  fs_at(dpre,  200, "Domestic before 1980 [placebo]", true_cut = 150),
  fs_at(dpost, 150, "Domestic from 1980 [TRUE cutoff]"),
  fs_at(dpost, 200, "Domestic from 1980 [placebo]",   true_cut = 150))
print(as.data.frame(pl_dom %>% mutate(across(where(is.numeric), ~round(., 4)))))

cat("\n--- placebo cutoffs in the outcome, away from the true line ---\n")
pl_out <- map_dfr(c(-100, -75, -50, 50, 75, 100), function(cc) {
  sub <- d %>% filter(abs(dist - cc) <= 60)
  o <- try(rdrobust(y = sub$lead_win, x = sub$dist, c = cc, p = 1,
                    kernel = "triangular", bwselect = "mserd",
                    masspoints = "adjust"), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  tibble(placebo_cutoff = cc, estimate = o$coef["Conventional", 1],
         se = o$se["Robust", 1], p = o$pv["Robust", 1])
})
print(as.data.frame(pl_out %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  5. BANDWIDTH AND DONUT
# =============================================================================
cat("\n\n########## 5. BANDWIDTH AND DONUT ##########\n\n")
base <- rdrobust(y = d$lead_win, x = d$dist, c = 0, fuzzy = d$enforced,
                 p = 1, kernel = "triangular", bwselect = "mserd",
                 masspoints = "adjust")
h0 <- base$bws["h", 1]
bw_tab <- map_dfr(c(0.5, 0.75, 1, 1.5, 2), function(m) {
  o <- try(rdrobust(y = d$lead_win, x = d$dist, c = 0, fuzzy = d$enforced,
                    p = 1, kernel = "triangular", h = h0 * m,
                    masspoints = "adjust"), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  tibble(bandwidth_multiple = m, h = round(h0 * m, 1),
         estimate = o$coef["Conventional", 1], se = o$se["Robust", 1],
         p = o$pv["Robust", 1])
})
print(as.data.frame(bw_tab %>% mutate(across(where(is.numeric), ~round(., 4)))))

cat("\ndonut: drop matches within a few runs of the cutoff\n\n")
donut <- map_dfr(c(0, 1, 2, 3, 5), function(dd) {
  sub <- d %>% filter(abs(dist) > dd | dist == 0 & dd == 0)
  if (dd > 0) sub <- d %>% filter(abs(dist) > dd)
  o <- try(rdrobust(y = sub$lead_win, x = sub$dist, c = 0, fuzzy = sub$enforced,
                    p = 1, kernel = "triangular", bwselect = "mserd",
                    masspoints = "adjust"), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  tibble(donut_radius = dd, n = nrow(sub),
         estimate = o$coef["Conventional", 1], se = o$se["Robust", 1],
         p = o$pv["Robust", 1])
})
print(as.data.frame(donut %>% mutate(across(where(is.numeric), ~round(., 4)))))

cat("\npolynomial order\n\n")
poly <- map_dfr(1:2, function(pp) {
  o <- try(rdrobust(y = d$lead_win, x = d$dist, c = 0, fuzzy = d$enforced,
                    p = pp, kernel = "triangular", bwselect = "mserd",
                    masspoints = "adjust"), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  tibble(p = pp, estimate = o$coef["Conventional", 1], se = o$se["Robust", 1])
})
print(as.data.frame(poly %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  6. BALANCE AT THE CUTOFF
# =============================================================================
cat("\n\n########## 6. BALANCE OF PREDETERMINED VARIABLES ##########\n")
cat("Characteristics fixed before the first innings ended should not jump.\n\n")
bal <- map_dfr(c("days_for_match", "yr"), function(v) {
  o <- try(rdrobust(y = d[[v]], x = d$dist, c = 0, p = 1, kernel = "triangular",
                    bwselect = "mserd", masspoints = "adjust"), silent = TRUE)
  if (inherits(o, "try-error")) return(NULL)
  tibble(variable = v, estimate = o$coef["Conventional", 1],
         se = o$se["Robust", 1], p = o$pv["Robust", 1])
})
print(as.data.frame(bal %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  7. LOCAL RANDOMISATION
# =============================================================================
cat("\n\n########## 7. LOCAL RANDOMISATION IN A NARROW WINDOW ##########\n")
cat("Treat matches within a few runs of the cutoff as if eligibility were\n")
cat("assigned at random, and test the outcome difference by permutation.\n\n")
for (w in c(5, 10, 15)) {
  sub <- d %>% filter(abs(dist) <= w)
  r <- try(rdrandinf(Y = sub$lead_win, R = sub$dist, cutoff = 0,
                     wl = -w, wr = w, reps = 2000, quietly = TRUE), silent = TRUE)
  if (!inherits(r, "try-error"))
    cat(sprintf("   window +/-%2d runs (n = %4d): difference = %+.4f, p = %.3f\n",
                w, nrow(sub), r$obs.stat, r$p.value))
}

saveRDS(list(manipulation_bounds = manip_bounds,
             placebo_test = pl_test, placebo_dom = pl_dom, placebo_out = pl_out,
             bandwidth = bw_tab, donut = donut, poly = poly, balance = bal),
        file.path(DDIR, "robustness.rds"))

sink()
message("07_robustness.R done")

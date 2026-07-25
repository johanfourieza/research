# =============================================================================
#  05_behavioural.R  —  DO CAPTAINS LEARN THE WRONG LESSON FROM A DISASTER?
#
#  We estimate whether a team's propensity to enforce the follow-on changes
#  after it has enforced and lost, relative to matched teams that enforced and
#  did not lose. See 04_events.R for how the comparison groups are built.
#
#  THE SPECIFICATION
#  For team i, in matched event set s, at event time k (seasons relative to the
#  event), observed in eligible match m:
#
#     enforced_m = a_(s,i) + b_(s,k) + SUM_k beta_k * 1[k] * disaster_s
#                  + f(lead_m) + controls + e_m
#
#  a_(s,i)  event-set by team fixed effects: each team is compared with itself
#  b_(s,k)  event-set by event-time fixed effects: absorbs any common change in
#           enforcement over the window, including the long secular decline
#  beta_k   the coefficients of interest: the difference between disaster teams
#           and their matched controls at event time k, relative to k = -1
#
#  Because every comparison happens inside a matched event set, and no team is
#  ever compared with a team already treated, the design avoids the problems of
#  two-way fixed effects with staggered timing (Goodman-Bacon 2021; see also
#  Jensen and Zhang 2026, footnote 17).
#
#  INFERENCE
#  There are only 34 treated events, so conventional standard errors are not
#  enough. We report three things:
#    (1) standard errors clustered on team
#    (2) a wild cluster bootstrap
#    (3) randomisation inference, which is the headline. Within each matched
#        event set we randomly reassign which of the similar enforcements was
#        the one that ended in defeat, and re-estimate. This mirrors the actual
#        source of randomness: which enforcement happened to go wrong.
#
#  OUTPUT
#     data/processed/behavioural.rds ; output/logs/05_behavioural.txt
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)
suppressWarnings(suppressMessages({ library(fixest); library(splines) }))
setFixest_nthreads(1)
set.seed(SEED)

panel <- readRDS(file.path(DDIR, "event_panel.rds"))
sink(file.path(LOGDIR, "05_behavioural.txt"), split = TRUE)

# fixest needs factors for interacted fixed effects
panel <- panel %>%
  mutate(stack = factor(stack), team = factor(team), k = as.integer(k),
         disaster = as.integer(disaster))

# =============================================================================
#  1. STATIC ESTIMATE
# =============================================================================
cat("########## 1. STATIC DIFFERENCE IN DIFFERENCES ##########\n")
cat("Effect of having suffered the disaster on the probability of enforcing\n")
cat("in later eligible matches.\n\n")

#  The interaction post x disaster is the estimate of interest. The two main
#  effects do not need to be included: "disaster" is absorbed by the event-set
#  by team fixed effects, and "post" by the event-set by event-time effects.
panel$treat_post <- panel$post * panel$disaster

m_static <- feols(enforced ~ treat_post + ns(dist, 3) +
                    factor(days_for_match) | stack^team + stack^k,
                  data = panel, cluster = ~team)
print(summary(m_static))

# A simpler version without the lead controls, to show the result is not
# driven by the functional form of the control.
m_simple <- feols(enforced ~ treat_post | stack^team + stack^k,
                  data = panel, cluster = ~team)
cat("\nWithout the lead controls:\n")
print(coeftable(m_simple))

beta_hat <- unname(coef(m_static)["treat_post"])
cat(sprintf("\nPoint estimate: %.4f\n", beta_hat))
cat(sprintf("Baseline enforcement rate before the event (treated teams): %.3f\n",
            mean(panel$enforced[panel$disaster == 1 & panel$post == 0])))

# =============================================================================
#  2. EVENT STUDY
# =============================================================================
cat("\n\n########## 2. EVENT STUDY ##########\n")
cat("Coefficients by season relative to the event, reference k = -1.\n")
cat("Coefficients before the event should be near zero if the two groups were\n")
cat("on parallel paths beforehand.\n\n")

m_es <- feols(enforced ~ i(k, disaster, ref = -1) + ns(dist, 3) +
                factor(days_for_match) | stack^team + stack^k,
              data = panel, cluster = ~team)
es_tab <- as.data.frame(coeftable(m_es)) %>%
  tibble::rownames_to_column("term") %>%
  filter(grepl("^k::", term)) %>%
  mutate(k = as.integer(sub("k::(-?[0-9]+):disaster", "\\1", term))) %>%
  transmute(k, estimate = Estimate, se = `Std. Error`,
            ci_lo = Estimate - 1.96 * `Std. Error`,
            ci_hi = Estimate + 1.96 * `Std. Error`) %>%
  arrange(k)
print(es_tab %>% mutate(across(where(is.numeric), ~round(., 4))) %>% as.data.frame())

cat("\nJoint test that all PRE-event coefficients are zero:\n")
pre_terms <- grep("^k::-[2-5]:disaster", names(coef(m_es)), value = TRUE)
if (length(pre_terms) > 0) {
  w <- wald(m_es, keep = "^k::-[2-5]:disaster", print = FALSE)
  cat(sprintf("   Wald statistic = %.3f, p = %.3f\n", w$stat, w$p))
}

# =============================================================================
#  3. RANDOMISATION INFERENCE  (the headline test)
# =============================================================================
#  Within each matched event set, exactly one enforcement ended in defeat. Our
#  null hypothesis is that which one it was carries no information about later
#  behaviour. We therefore reassign the "disaster" label at random within each
#  event set, holding everything else fixed, and re-estimate. Repeating this
#  many times builds the distribution of the estimate under the null.
# -----------------------------------------------------------------------------
cat("\n\n########## 3. RANDOMISATION INFERENCE ##########\n")
B <- 2000

#  Precompute, once, the list of event identifiers belonging to each event set
#  and the position of every panel row's event within that list. Each draw then
#  only needs to pick one event per set and rebuild a single column, which is
#  fast enough to repeat two thousand times.
panel$row_event <- as.character(panel$event_id)
set_of_row     <- as.character(panel$stack)
events_by_set  <- split(unique(data.frame(s = set_of_row, e = panel$row_event)),
                        ~ s)
events_by_set  <- lapply(events_by_set, function(z) unique(z$e))

#  We permute a STUDENTISED statistic (the cluster-robust t) rather than the
#  raw coefficient. A studentised statistic is robust to the heteroskedasticity
#  and unequal set sizes that a raw-coefficient permutation ignores, and it is
#  what the literature on randomisation tests in clustered settings recommends.
ri_one <- function(b) {
  picked <- vapply(events_by_set, function(e) e[sample.int(length(e), 1)], "")
  dperm  <- as.integer(panel$row_event == picked[set_of_row])
  pp <- panel
  pp$dperm <- dperm
  pp$treat_post <- pp$post * dperm
  fit_static <- try(feols(enforced ~ treat_post + ns(dist, 3) +
                            factor(days_for_match) | stack^team + stack^k,
                          data = pp, cluster = ~team, notes = FALSE,
                          nthreads = 1), silent = TRUE)
  fit_first <- try(feols(enforced ~ i(k, dperm, ref = -1) + ns(dist, 3) +
                           factor(days_for_match) | stack^team + stack^k,
                         data = pp, cluster = ~team, notes = FALSE,
                         nthreads = 1), silent = TRUE)
  if (inherits(fit_static, "try-error") || inherits(fit_first, "try-error"))
    return(rep(NA_real_, 4))
  ct_static <- coeftable(fit_static)
  ct_first <- coeftable(fit_first)
  first_term <- grep("^k::0:dperm$", rownames(ct_first), value = TRUE)
  if (!"treat_post" %in% rownames(ct_static) || length(first_term) != 1)
    return(rep(NA_real_, 4))
  c(unname(ct_static["treat_post", "Estimate"]),
    unname(ct_static["treat_post", "t value"]),
    unname(ct_first[first_term, "Estimate"]),
    unname(ct_first[first_term, "t value"]))
}
ri_mat <- vapply(seq_len(B), ri_one, numeric(4))
keep <- apply(ri_mat, 2, function(z) all(is.finite(z)))
ri_draws <- ri_mat[1, keep]      # permuted coefficients, for scale
ri_t     <- ri_mat[2, keep]      # permuted t statistics, for the test
ri_first <- ri_mat[3, keep]
ri_t_first <- ri_mat[4, keep]
t_obs_main <- unname(coeftable(m_static)["treat_post", "t value"])
first_term_obs <- grep("^k::0:disaster$", names(coef(m_es)), value = TRUE)
t_obs_first <- unname(coeftable(m_es)[first_term_obs, "t value"])

# The finite-simulation p-value adds one to numerator and denominator, which
# keeps the test valid for a finite number of draws.
ri_p <- (1 + sum(abs(ri_t) >= abs(t_obs_main))) / (length(ri_t) + 1)
ri_p_first <- (1 + sum(abs(ri_t_first) >= abs(t_obs_first))) /
  (length(ri_t_first) + 1)

cat(sprintf("draws completed         : %d\n", length(ri_t)))
cat(sprintf("observed estimate       : %+.4f (t = %+.3f)\n", beta_hat, t_obs_main))
cat(sprintf("randomisation s.d. of b : %.4f\n", sd(ri_draws)))
cat(sprintf("two-sided p-value       : %.3f   [(1+r)/(B+1), studentised]\n", ri_p))
cat(sprintf("first post-event year   : t = %+.3f, RI p = %.3f\n",
            t_obs_first, ri_p_first))

# Invert the test to get a confidence interval: the set of null values b0 that
# would not be rejected. We shift the outcome by b0 * treat_post and re-test.
cat("\ninverting the randomisation test for a confidence interval:\n")
grid <- seq(-0.30, 0.30, by = 0.01)
notrej <- vapply(grid, function(b0) {
  pp <- panel; pp$enforced <- pp$enforced - b0 * pp$treat_post
  f <- try(feols(enforced ~ treat_post + ns(dist, 3) + factor(days_for_match) |
                   stack^team + stack^k, data = pp, cluster = ~team,
                 notes = FALSE, nthreads = 1), silent = TRUE)
  if (inherits(f, "try-error")) return(NA_real_)
  unname(coeftable(f)["treat_post", "t value"])
}, numeric(1))
inci <- grid[abs(notrej) <= quantile(abs(ri_t), 0.95, na.rm = TRUE)]
if (length(inci))
  cat(sprintf("   95%% interval: [%+.3f, %+.3f]\n", min(inci), max(inci)))

# =============================================================================
#  4. WILD CLUSTER BOOTSTRAP
# =============================================================================
#  With few treated clusters, cluster-robust standard errors reject too often.
#  The wild cluster bootstrap resamples by flipping the sign of each cluster's
#  residuals. We implement it directly, using Webb's six-point weights, which
#  perform better than Rademacher weights when clusters are few.
# -----------------------------------------------------------------------------
cat("\n\n########## 4. WILD CLUSTER BOOTSTRAP ##########\n")
webb <- function(n) sample(c(-sqrt(1.5), -1, -sqrt(0.5),
                             sqrt(0.5), 1, sqrt(1.5)), n, replace = TRUE)
# restricted model: impose the null that the effect is zero
m0 <- feols(enforced ~ ns(dist, 3) + factor(days_for_match) |
              stack^team + stack^k, data = panel, notes = FALSE)
# residuals and fitted values must align with the rows the model actually used
used  <- obs(m0)
pboot <- panel[used, ]
u0    <- as.numeric(resid(m0)); fit0 <- as.numeric(fitted(m0))
cl    <- as.character(pboot$team); ucl <- unique(cl)
t_obs <- unname(coeftable(m_static)["treat_post", "t value"])

boot_t <- vapply(seq_len(999), function(b) {
  w  <- setNames(webb(length(ucl)), ucl)
  pb <- pboot
  pb$enforced <- fit0 + u0 * w[cl]
  fb <- try(feols(enforced ~ treat_post + ns(dist, 3) +
                    factor(days_for_match) | stack^team + stack^k,
                  data = pb, cluster = ~team, notes = FALSE,
                  nthreads = 1), silent = TRUE)
  if (inherits(fb, "try-error")) return(NA_real_)
  unname(coeftable(fb)["treat_post", "t value"])
}, numeric(1))
boot_t <- boot_t[is.finite(boot_t)]
cat(sprintf("observed t             : %+.3f\n", t_obs))
cat(sprintf("bootstrap p-value      : %.3f  (%d draws)\n",
            mean(abs(boot_t) >= abs(t_obs)), length(boot_t)))

# =============================================================================
#  5. HOW BIG AN EFFECT COULD WE HAVE FOUND?
# =============================================================================
#  A null result is only informative if the test had power. We use the
#  randomisation distribution to report the smallest effect we would have
#  detected 80 per cent of the time.
# -----------------------------------------------------------------------------
cat("\n\n########## 5. MINIMUM DETECTABLE EFFECT ##########\n")
crit <- quantile(abs(ri_draws), 0.95)
mde  <- crit + qnorm(0.80) * sd(ri_draws)
base <- mean(panel$enforced[panel$disaster == 1 & panel$post == 0])
cat(sprintf("randomisation s.d.          : %.3f\n", sd(ri_draws)))
cat(sprintf("5%% critical value (abs)     : %.3f\n", crit))
cat(sprintf("minimum detectable effect   : %.3f  (80%% power, 5%% level)\n", mde))
cat(sprintf("as a share of the baseline  : %.1f%% of %.3f\n",
            100 * mde / base, base))
cat("\nNOTE ON INTERPRETATION. A minimum detectable effect describes what the\n")
cat("design could have found; it is NOT a bound on the true effect. The\n")
cat("inverted confidence interval reported above is the bound. What the two\n")
cat("together support is the narrow claim that a fall of the size implied by\n")
cat("the folk account, an abandonment of the practice, is ruled out.\n")

saveRDS(list(static = m_static, event_study = es_tab, ri_draws = ri_draws,
             ri_t = ri_t, ri_p = ri_p, beta = beta_hat, mde = mde,
             ri_first = ri_first, ri_t_first = ri_t_first,
             ri_p_first = ri_p_first,
             ri_ci = if (length(inci)) range(inci) else c(NA_real_, NA_real_),
             cluster_se = unname(coeftable(m_static)["treat_post", "Std. Error"]),
             boot_p = mean(abs(boot_t) >= abs(t_obs))),
        file.path(DDIR, "behavioural.rds"))
write_csv(es_tab, file.path(TABDIR, "event_study.csv"))

sink()
message("05_behavioural.R done")

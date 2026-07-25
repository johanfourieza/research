# =============================================================================
#  03_rd.R  —  DOES ENFORCING THE FOLLOW-ON HELP?
#              A fuzzy regression discontinuity design
#
#  THE IDEA IN ONE PARAGRAPH
#  We want the causal effect of enforcing the follow-on on the leading side's
#  chance of winning. We cannot simply compare enforcers with non-enforcers,
#  because captains choose. Instead we use the statutory rule. A captain may
#  enforce only if the first-innings lead reaches a stated margin. Consider two
#  matches, one with a lead one run below the margin and one with a lead one run
#  above it. The playing situations are practically identical, but only in the
#  second is enforcement legally possible. Comparing outcomes across that line
#  isolates the effect of making the option available.
#
#  Because not every eligible captain enforces, the design is FUZZY: crossing
#  the line raises the probability of enforcement from zero to about two-thirds,
#  not to one. Dividing the jump in the outcome by the jump in enforcement gives
#  the effect of enforcing for those captains who enforce because they may
#  (a local average treatment effect).
#
#  DEFINITIONS
#     running variable  dist = lead - statutory margin, so the cutoff is 0
#     instrument        eligible = 1 if dist >= 0 (holds exactly, by definition)
#     treatment         enforced
#     outcome           lead_win (the side batting first wins)
#
#  OUTPUT
#     data/processed/rd_estimates.rds ; output/logs/03_rd.txt
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)
suppressWarnings(suppressMessages({
  library(rdrobust); library(rdmulti)
}))

d <- readRDS(file.path(DDIR, "matches.rds")) %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive,
         !is.na(margin), !is.na(dist), !is.na(lead_win))

sink(file.path(LOGDIR, "03_rd.txt"), split = TRUE)
cat("Analysis sample:", nrow(d), "matches,", min(d$yr), "to", max(d$yr), "\n\n")

# Helper: run rdrobust and return a tidy one-row summary.
# We use the package defaults recommended by Calonico, Cattaneo and Titiunik:
#   local linear fit (p = 1), triangular kernel, MSE-optimal bandwidth chosen
#   separately on each side, and robust bias-corrected confidence intervals.
#   masspoints = "adjust" handles the fact that the running variable is an
#   integer number of runs, so many matches share the same value.
rd_row <- function(dat, y, fuzzy = NULL, label = "", cutoff = 0) {
  if (nrow(dat) < 100) return(NULL)
  fz <- if (is.null(fuzzy)) NULL else dat[[fuzzy]]
  out <- try(rdrobust(y = dat[[y]], x = dat$dist, c = cutoff, fuzzy = fz,
                      p = 1, kernel = "triangular", bwselect = "mserd",
                      masspoints = "adjust", vce = "hc3"), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  #  Following Calonico, Cattaneo and Titiunik, we report the conventional
  #  local-linear point estimate together with the robust bias-corrected
  #  confidence interval and p-value. We ALSO report the bias-corrected point
  #  estimate, which is the estimate the robust interval is centred on, so that
  #  the reader can see both and is not left pairing one estimator's point
  #  estimate with another's standard error without being told.
  tibble(spec = label, outcome = y,
         estimate = out$coef["Conventional", 1],
         se_conv = out$se["Conventional", 1],
         estimate_bc = out$coef["Bias-Corrected", 1],
         se_robust = out$se["Robust", 1],
         ci_lo = out$ci["Robust", 1], ci_hi = out$ci["Robust", 2],
         p_robust = out$pv["Robust", 1],
         bw = out$bws["h", 1],
         n_left = out$N_h[1], n_right = out$N_h[2])
}

# =============================================================================
#  1. FIRST STAGE — does crossing the line change behaviour?
# =============================================================================
cat("########## 1. FIRST STAGE ##########\n")
cat("The jump in the probability of enforcement at the cutoff. Without a\n")
cat("first stage there is nothing to instrument, so this comes first.\n\n")

fs <- bind_rows(
  rd_row(d, "enforced", label = "First stage, all"),
  rd_row(d %>% filter(comp == "Test"), "enforced", label = "First stage, Tests"),
  rd_row(d %>% filter(comp != "Test"), "enforced", label = "First stage, domestic"))
print(as.data.frame(fs %>% mutate(across(where(is.numeric), ~round(., 4)))))

cat("\nRaw enforcement rates just either side of the line:\n")
print(d %>% filter(abs(dist) <= 10) %>%
        group_by(side = ifelse(dist >= 0, "eligible (dist 0 to 10)",
                               "not eligible (dist -10 to -1)")) %>%
        summarise(n = n(), enforce_rate = round(mean(enforced), 3),
                  .groups = "drop") %>% as.data.frame())

# =============================================================================
#  2. REDUCED FORM — the effect of ELIGIBILITY on winning
# =============================================================================
cat("\n\n########## 2. REDUCED FORM (intention to treat) ##########\n")
cat("The jump in the outcome at the cutoff, ignoring who actually enforced.\n")
cat("This is the effect of GIVING the captain the option.\n\n")

rf <- bind_rows(
  rd_row(d, "lead_win", label = "ITT, all"),
  rd_row(d, "lead_loss", label = "ITT, all"),
  rd_row(d, "result_decisive", label = "ITT, all"),
  rd_row(d %>% filter(comp == "Test"), "lead_win", label = "ITT, Tests"),
  rd_row(d %>% filter(comp != "Test"), "lead_win", label = "ITT, domestic"))
print(as.data.frame(rf %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  3. FUZZY RD — the effect of ENFORCING
# =============================================================================
cat("\n\n########## 3. FUZZY RD (local average treatment effect) ##########\n")
cat("The reduced form divided by the first stage.\n\n")

fz <- bind_rows(
  rd_row(d, "lead_win",        fuzzy = "enforced", label = "Fuzzy, all"),
  rd_row(d, "lead_loss",       fuzzy = "enforced", label = "Fuzzy, all"),
  rd_row(d, "result_decisive", fuzzy = "enforced", label = "Fuzzy, all"),
  rd_row(d %>% filter(comp == "Test"), "lead_win", fuzzy = "enforced",
         label = "Fuzzy, Tests"),
  rd_row(d %>% filter(comp != "Test"), "lead_win", fuzzy = "enforced",
         label = "Fuzzy, domestic"),
  rd_row(d %>% filter(comp != "Test"), "lead_loss", fuzzy = "enforced",
         label = "Fuzzy, domestic"),
  rd_row(d %>% filter(comp != "Test"), "result_decisive", fuzzy = "enforced",
         label = "Fuzzy, domestic"))
print(as.data.frame(fz %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  4. CUTOFF BY CUTOFF
# =============================================================================
#  The margin is 150 or 200 depending on era and match length, so the pooled
#  estimate above combines several cutoffs after re-centring. Here we look at
#  each one separately. If the rule is what identifies the effect, each cutoff
#  should show the same pattern.
# -----------------------------------------------------------------------------
cat("\n\n########## 4. SEPARATE CUTOFFS ##########\n\n")
cells <- d %>% mutate(
  cell = case_when(
    comp == "Test" & margin == 200 ~ "Test, 200 line (1980+)",
    comp == "Test" & margin == 150 ~ "Test, 150 line (pre-1980)",
    comp != "Test" & margin == 150 ~ "Domestic, 150 line",
    comp != "Test" & margin == 200 ~ "Domestic, 200 line (5-day)",
    TRUE ~ "other"))
cat("cell sizes:\n"); print(cells %>% count(cell) %>% as.data.frame())

percut <- map_dfr(setdiff(unique(cells$cell), "other"), function(cc) {
  sub <- cells %>% filter(cell == cc)
  bind_rows(rd_row(sub, "enforced", label = paste0(cc, " [first stage]")),
            rd_row(sub, "lead_win", fuzzy = "enforced",
                   label = paste0(cc, " [fuzzy]")))
})
print(as.data.frame(percut %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  5. MINIMUM DETECTABLE EFFECT
# =============================================================================
#  An imprecise zero is not evidence of no effect. For each specification we
#  report the smallest true effect we could have detected with 80 per cent
#  power at the 5 per cent level, which is 2.802 times the standard error.
# -----------------------------------------------------------------------------
cat("\n\n########## 5. MINIMUM DETECTABLE EFFECT ##########\n\n")
mde <- bind_rows(fz, percut) %>%
  filter(!is.na(se_robust)) %>%
  transmute(spec, outcome, estimate = round(estimate, 3),
            se = round(se_robust, 3), mde_80 = round(2.802 * se_robust, 3))
print(as.data.frame(mde))
cat("\nAn estimate whose minimum detectable effect exceeds one is uninformative:\n")
cat("no possible probability change could have been detected.\n")

saveRDS(list(first_stage = fs, reduced_form = rf, fuzzy = fz,
             per_cutoff = percut, mde = mde),
        file.path(DDIR, "rd_estimates.rds"))
write_csv(bind_rows(fs, rf, fz, percut), file.path(TABDIR, "rd_estimates.csv"))

sink()
message("03_rd.R done")

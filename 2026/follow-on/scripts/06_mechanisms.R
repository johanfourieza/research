# =============================================================================
#  06_mechanisms.R  —  PLACEBOS, MECHANISMS, AND INDIRECT EXPERIENCE
#
#  The main behavioural test in 05_behavioural.R finds no persistent average
#  fall in enforcement after a team's own disaster, alongside a lower estimate
#  in the first post-event year. This script reports exploratory checks.
#
#   (1) PLACEBO. Do teams change behaviour after losing a match in which they
#       DECLINED to enforce? If they respond to defeat in general, rather than
#       to the enforcement decision, we should see something here too. There
#       are 188 such events, so this test has more power than the main one.
#
#   (2) MEMORY. Is any response confined to matches led by the same captain who
#       lived through the disaster? Experience effects are personal (Malmendier
#       and Nagel 2011), so if the mechanism is personal experience the effect
#       should disappear when the captain changes.
#
#   (3) NEAR MISSES. A defeat is rare, but a scare is common: in many
#       enforcements the opponent bats well enough the second time to make the
#       enforcing side bat again. That is a vivid experience without a defeat.
#       There are over a thousand such events, so this test is well powered.
#
#   (4) INDIRECT EXPERIENCE. Alfonsi, Dossi and Monk (2026) show that people
#       respond to shocks suffered by others they are connected to. Do captains
#       enforce less after ANOTHER team in their competition suffers a
#       disaster, even though they did not experience it themselves?
#
#  OUTPUT
#     data/processed/mechanisms.rds ; output/logs/06_mechanisms.txt
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)
suppressWarnings(suppressMessages({ library(fixest); library(splines) }))
setFixest_nthreads(1)
set.seed(SEED)

d <- readRDS(file.path(DDIR, "matches.rds")) %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive,
         !is.na(margin))
panel <- readRDS(file.path(DDIR, "event_panel.rds")) %>%
  mutate(stack = factor(stack), team = factor(team),
         treat_post = post * disaster)

sink(file.path(LOGDIR, "06_mechanisms.txt"), split = TRUE)

WIN <- 5; SEASON_W <- 2; LEAD_CAL <- 60; MAX_CTRL <- 5

elig <- d %>% filter(eligible == 1) %>%
  transmute(match_id, comp, team = lead_team, captain = lead_captain,
            season = yr, match_date = as.Date(start_date),
            enforced, lead, dist, days_for_match)

# -----------------------------------------------------------------------------
#  A reusable routine. Given a set of treated events and a pool of control
#  events, it matches without replacement, censors each team at its next event,
#  builds the panel and estimates the main specification with studentised
#  randomisation inference.
# -----------------------------------------------------------------------------
run_design <- function(treated, ctrl_pool, label, B = 1000) {
  treated <- treated %>% arrange(season, event_date, event_id)
  used <- character(0)
  stacks <- list()
  for (i in seq_len(nrow(treated))) {
    t <- treated[i, ]
    cand <- ctrl_pool %>%
      filter(!event_id %in% used,
             comp == t$comp, abs(season - t$season) <= SEASON_W,
             abs(lead - t$lead) <= LEAD_CAL, team != t$team) %>%
      mutate(sc = abs(lead - t$lead) / LEAD_CAL +
                  abs(season - t$season) / max(SEASON_W, 1)) %>%
      arrange(sc) %>% distinct(team, .keep_all = TRUE) %>%
      slice_head(n = MAX_CTRL)
    if (nrow(cand) == 0) next
    used <- c(used, cand$event_id)
    stacks[[length(stacks) + 1]] <-
      bind_rows(t %>% mutate(role = "treated", disaster = 1L),
                cand %>% select(-sc) %>%
                  mutate(role = "control", disaster = 0L)) %>%
      mutate(stack = t$event_id)
  }
  mt <- bind_rows(stacks)
  if (nrow(mt) == 0) return(NULL)

  later_events <- treated %>%
    select(comp, team, next_event_date = event_date)

  pn <- mt %>%
    select(stack, disaster, event_id, comp, team, event_season = season,
           event_date, event_captain = captain) %>%
    left_join(elig, by = c("comp", "team"), relationship = "many-to-many") %>%
    filter(match_id != event_id,
           season >= event_season - WIN, season <= event_season + WIN) %>%
    # event time measured from the event DATE, so that a match played later in
    # the same season counts as after the event, not before it
    mutate(days_since = as.numeric(match_date - event_date),
           k = ifelse(is.na(days_since), season - event_season,
                      ifelse(days_since >= 0, floor(days_since / 365.25),
                             ceiling(days_since / 365.25) - 1)),
           post = ifelse(is.na(days_since), as.integer(season > event_season),
                         as.integer(days_since > 0)),
           treat_post = post * disaster,
            stack = factor(stack), team = factor(team)) %>%
    filter(k >= -WIN, k <= WIN) %>%
    left_join(later_events, by = c("comp", "team"),
              relationship = "many-to-many") %>%
    group_by(stack, event_id, match_id) %>%
    mutate(next_event = suppressWarnings(min(
      next_event_date[!is.na(next_event_date) & next_event_date > event_date],
      na.rm = TRUE))) %>%
    ungroup() %>%
    filter(is.infinite(next_event) | is.na(next_event) |
             match_date < next_event) %>%
    select(-next_event_date, -next_event) %>%
    distinct(stack, event_id, match_id, .keep_all = TRUE)
  if (nrow(pn) < 200) return(NULL)

  fit <- feols(enforced ~ treat_post + ns(dist, 3) + factor(days_for_match) |
                 stack^team + stack^k, data = pn, cluster = ~team, notes = FALSE)
  b <- unname(coef(fit)["treat_post"])
  t_obs <- unname(coeftable(fit)["treat_post", "t value"])

  # randomisation inference: reassign which event in each set was the treated one
  pn$row_event <- as.character(pn$event_id)
  sr <- as.character(pn$stack)
  ebs <- split(unique(data.frame(s = sr, e = pn$row_event)), ~ s)
  ebs <- lapply(ebs, function(z) unique(z$e))
  ri <- vapply(seq_len(B), function(bb) {
    picked <- vapply(ebs, function(e) e[sample.int(length(e), 1)], "")
    pp <- pn; pp$treat_post <- pp$post * as.integer(pp$row_event == picked[sr])
    f2 <- try(feols(enforced ~ treat_post + ns(dist, 3) +
                      factor(days_for_match) | stack^team + stack^k,
                    data = pp, cluster = ~team, notes = FALSE, nthreads = 1),
              silent = TRUE)
    if (inherits(f2, "try-error")) return(c(NA_real_, NA_real_))
    ct <- coeftable(f2)
    if (!"treat_post" %in% rownames(ct)) return(c(NA_real_, NA_real_))
    c(unname(ct["treat_post", "Estimate"]),
      unname(ct["treat_post", "t value"]))
  }, numeric(2))
  keep <- is.finite(ri[1, ]) & is.finite(ri[2, ])
  draws <- ri[1, keep]
  draw_t <- ri[2, keep]

  tibble(design = label, n_treated_events = n_distinct(mt$stack),
         n_control_events = sum(mt$role == "control"),
         n_obs = nrow(pn), n_unique_matches = n_distinct(pn$match_id),
         baseline = mean(pn$enforced[pn$disaster == 1 & pn$post == 0]),
         estimate = b,
         se_cluster = unname(coeftable(fit)["treat_post", "Std. Error"]),
         ri_sd = sd(draws),
         ri_p = (1 + sum(abs(draw_t) >= abs(t_obs))) / (length(draw_t) + 1),
         mde = quantile(abs(draws), 0.95) + qnorm(0.80) * sd(draws))
}

# =============================================================================
#  1. PLACEBO: LOSING AFTER DECLINING TO ENFORCE
# =============================================================================
cat("########## 1. PLACEBO: DEFEAT AFTER DECLINING ##########\n\n")
dec <- d %>% filter(eligible == 1, enforced == 0) %>%
  transmute(event_id = match_id, comp, team = lead_team, event_date = as.Date(start_date),
            captain = lead_captain, season = yr, lead, days_for_match,
            lost = lead_loss)
dec_t <- dec %>% filter(lost == 1)
dec_c <- dec %>% filter(lost == 0)
cat("events where a team declined and then lost:", nrow(dec_t), "\n")
cat("control pool (declined, did not lose):", nrow(dec_c), "\n\n")

res_placebo <- run_design(dec_t, dec_c, "Placebo: declined and lost")
print(as.data.frame(res_placebo %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  2. MEMORY: DOES THE CAPTAIN MATTER?
# =============================================================================
cat("\n\n########## 2. IS ANY RESPONSE PERSONAL TO THE CAPTAIN? ##########\n")
cat("Split post-event observations by whether the captain who lived through\n")
cat("the disaster is still leading the side.\n\n")

pc <- panel %>% mutate(
  same_cap  = coalesce(as.integer(captain == event_captain), 0L),
  tp_same   = post * disaster * same_cap,
  tp_diff   = post * disaster * (1 - same_cap))
m_cap <- feols(enforced ~ tp_same + tp_diff + ns(dist, 3) +
                 factor(days_for_match) | stack^team + stack^k,
               data = pc, cluster = ~team)
print(coeftable(m_cap)[c("tp_same", "tp_diff"), ])
cat("\nShare of post-event observations under the same captain: ",
    round(mean(pc$same_cap[pc$post == 1 & pc$disaster == 1]), 3), "\n")

# =============================================================================
#  3. NEAR MISSES
# =============================================================================
cat("\n\n########## 3. NEAR MISSES ##########\n")
cat("Enforcements where the opponent, batting again, erased the deficit.\n")
cat("The match was a scare, but the enforcing side did not lose.\n\n")

nm <- d %>% filter(enforced == 1, lead_loss == 0) %>%
  transmute(event_id = match_id, comp, team = lead_team, event_date = as.Date(start_date),
            captain = lead_captain, season = yr, lead, days_for_match,
            scare = coalesce(near_miss, 0L))
cat("near misses:", sum(nm$scare), " | comfortable wins/draws:",
    sum(nm$scare == 0), "\n\n")
res_nearmiss <- run_design(nm %>% filter(scare == 1),
                           nm %>% filter(scare == 0),
                           "Near miss (scare, no defeat)", B = 500)
print(as.data.frame(res_nearmiss %>% mutate(across(where(is.numeric), ~round(., 4)))))

# =============================================================================
#  4. INDIRECT EXPERIENCE
# =============================================================================
#  Treatment here is a disaster suffered by ANOTHER team in the same
#  competition. Every other team in that competition is exposed to the news
#  without having lived through it. We compare enforcement in the competition
#  before and after such an event, against competitions with no event in the
#  same seasons.
# -----------------------------------------------------------------------------
cat("\n\n########## 4. INDIRECT EXPERIENCE (DESCRIPTIVE) ##########\n")
cat("Does a disaster suffered by a RIVAL change a captain's behaviour?\n\n")

dis <- d %>% filter(enforced == 1, lead_loss == 1) %>%
  select(comp, d_season = yr, d_team = lead_team)
cat("disaster events used:", nrow(dis), "\n")

ind <- map_dfr(seq_len(nrow(dis)), function(i) {
  e <- dis[i, ]
  elig %>%
    filter(season >= e$d_season - WIN, season <= e$d_season + WIN) %>%
    filter(!(comp == e$comp & team == e$d_team)) %>%   # exclude the team itself
    mutate(stack = paste0(e$comp, "_", e$d_season),
           exposed = as.integer(comp == e$comp),
           k = season - e$d_season, post = as.integer(k > 0))
}) %>% mutate(treat_post = post * exposed,
              stack = factor(stack), team = factor(team))

cat("panel observations:", nrow(ind),
    "| exposed:", sum(ind$exposed), "| not exposed:", sum(ind$exposed == 0), "\n\n")
m_ind <- feols(enforced ~ treat_post + ns(dist, 3) + factor(days_for_match) |
                 stack^team + stack^k, data = ind, cluster = ~comp)
print(coeftable(m_ind)["treat_post", , drop = FALSE])
cat("\nThis stacked comparison reuses matches around several events and has only\n")
cat("a few competition clusters. It is descriptive and is not used as a\n")
cat("separate causal test.\n")

# =============================================================================
#  5. TEST CRICKET AFTER KOLKATA 2001
# =============================================================================
#  Every Test captain in the world saw India beat Australia in March 2001
#  having followed on. All Test captains are exposed at once, so there is no
#  control group inside Test cricket. We therefore report a descriptive
#  interrupted series and ask how unusual a break at 2001 is compared with a
#  break at any other year.
# -----------------------------------------------------------------------------
cat("\n\n########## 5. TEST CRICKET AROUND KOLKATA 2001 ##########\n\n")
te <- d %>% filter(comp == "Test", eligible == 1) %>%
  group_by(yr) %>% summarise(n = n(), rate = mean(enforced), .groups = "drop") %>%
  filter(n >= 3)

break_stat <- function(yr0) {
  s <- te %>% mutate(after = as.integer(yr >= yr0))
  if (sum(s$after) < 8 || sum(1 - s$after) < 8) return(NA_real_)
  fit <- lm(rate ~ after + yr, data = s, weights = s$n)
  unname(coef(fit)["after"])
}
cand <- te$yr[te$yr >= min(te$yr) + 10 & te$yr <= max(te$yr) - 10]
stats <- vapply(cand, break_stat, numeric(1))
obs <- break_stat(2001)
cat(sprintf("estimated level shift at 2001 : %+.3f\n", obs))
cat(sprintf("rank among all candidate years: %d of %d (most negative = 1)\n",
            rank(stats, na.last = "keep")[which(cand == 2001)], sum(!is.na(stats))))
cat(sprintf("share of years with a MORE negative shift: %.2f\n",
            mean(stats < obs, na.rm = TRUE)))
cat("\nTest enforcement rate by period:\n")
print(d %>% filter(comp == "Test", eligible == 1) %>%
        mutate(p = cut(yr, c(-Inf, 1980, 2000, Inf),
                       labels = c("1900-1980","1981-2000","2001+"))) %>%
        group_by(p) %>% summarise(n = n(), rate = round(mean(enforced), 3),
                                  .groups = "drop") %>% as.data.frame())

saveRDS(list(placebo = res_placebo, nearmiss = res_nearmiss,
             captain = coeftable(m_cap), indirect = coeftable(m_ind),
             test_series = te, kolkata_shift = obs,
             kolkata_placebo = stats),
        file.path(DDIR, "mechanisms.rds"))

sink()
message("06_mechanisms.R done")

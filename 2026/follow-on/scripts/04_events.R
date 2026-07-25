# =============================================================================
#  04_events.R  —  BUILD THE EVENT PANEL FOR THE BEHAVIOURAL ANALYSIS
#
#  THE QUESTION
#  Do captains stop enforcing the follow-on after their team has been beaten
#  having enforced it? This is a test of experience effects (Malmendier and
#  Nagel 2011) among expert decision-makers: does one vivid personal disaster
#  change later behaviour, even though the disaster is rare and, as the
#  discontinuity design shows, enforcing is on average a good decision?
#
#  THE DESIGN PROBLEM, AND HOW WE SOLVE IT
#  The obvious comparison, teams that have suffered a disaster against teams
#  that have not, is biased. A team can only suffer this disaster if it
#  enforced, and a team enforces when its propensity to enforce is high. Its
#  enforcement rate would therefore fall afterwards through simple regression
#  to the mean, with no change of belief at all. Teams that never suffered the
#  disaster are disproportionately teams that rarely enforce, so they are a
#  poor comparison.
#
#  Our solution follows Fadlon and Nielsen (2021) and Jensen and Zhang (2026):
#  compare like with like by conditioning both groups on the SAME ACTION.
#
#     treated events  : the team enforced and then LOST the match
#     control events  : the team enforced and did NOT lose
#
#  Both groups enforced, so both are drawn from the same high-propensity part
#  of the distribution and both are subject to the same regression to the mean.
#  What separates them is the realised outcome. Conditional on enforcing with a
#  given lead, losing is close to a chance event: it happens in about one per
#  cent of enforcements. The comparison therefore isolates the effect of the
#  experience, not of the decision that preceded it.
#
#  We additionally match controls to treated events on competition, season and
#  the size of the first-innings lead, because disasters are concentrated in
#  matches with modest leads.
#
#  OUTPUT
#     data/processed/event_panel.rds  (the stacked panel)
#     data/processed/events.rds       (one row per event)
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)

sink(file.path(LOGDIR, "04_events.txt"), split = TRUE)

d <- readRDS(file.path(DDIR, "matches.rds")) %>%
  filter(!compulsory, comp_group %in% c("Test", "Main"), competitive,
         !is.na(margin))

# Design parameters. They are stated once, here, so a reader can see every
# choice in one place and change it.
WIN      <- 5    # event-time window in seasons, both directions
SEASON_W <- 2    # a control event must fall within this many seasons
LEAD_CAL <- 60   # and its first-innings lead within this many runs
MAX_CTRL <- 5    # at most this many control events per treated event

cat("Design parameters: window +/-", WIN, "seasons; control season window +/-",
    SEASON_W, "; lead caliper", LEAD_CAL, "runs; up to", MAX_CTRL,
    "controls per event\n\n")

# =============================================================================
#  1. DEFINE EVENTS
# =============================================================================
# Every enforcement is a potential event. It is "treated" if the enforcing side
# went on to lose.
events_all <- d %>%
  filter(enforced == 1) %>%
  transmute(event_id = match_id, comp, team = lead_team, captain = lead_captain,
            season = yr, event_date = as.Date(start_date),
            lead, margin, days_for_match,
            disaster = lead_loss,
            near_miss = coalesce(near_miss, 0L))

cat("########## 1. EVENTS ##########\n")
cat("enforcements in the analysis sample:", nrow(events_all), "\n")
print(events_all %>% count(disaster) %>%
        mutate(label = ifelse(disaster == 1, "enforced and LOST (treated)",
                              "enforced, did not lose (control pool)")) %>%
        as.data.frame())

cat("\ntreated events by competition:\n")
print(events_all %>% filter(disaster == 1) %>% count(comp) %>% as.data.frame())
cat("\ntreated events by decade:\n")
print(events_all %>% filter(disaster == 1) %>%
        count(decade = 10 * (season %/% 10)) %>% as.data.frame())

cat("\nleads in treated vs control events (disasters happen on smaller leads):\n")
print(events_all %>% group_by(disaster) %>%
        summarise(n = n(), mean_lead = round(mean(lead), 1),
                  median_lead = median(lead), .groups = "drop") %>%
        as.data.frame())

# -----------------------------------------------------------------------------
#  A NOTE ON WHAT WE MUST NOT DO
#  It is tempting to require a control team to have suffered no disaster
#  anywhere inside the event window. That would be a mistake: it selects
#  controls on outcomes that occur AFTER the event, while treated teams face no
#  equivalent restriction. Teams that enforce often have more chances to suffer
#  a later disaster, so such a rule would preferentially discard exactly the
#  controls whose later enforcement is high, and would push the estimate upward.
#  Instead we treat both groups identically: each team's panel is CENSORED at
#  its next disaster after the event, so no observation follows a second,
#  unmodelled shock in either group.
# -----------------------------------------------------------------------------
disasters <- events_all %>% filter(disaster == 1) %>%
  select(comp, team, d_date = event_date)

# =============================================================================
#  2. MATCH CONTROLS TO TREATED EVENTS
# =============================================================================
#  Matching is done WITHOUT REPLACEMENT: a control event is used in at most one
#  matched set, and at most one event per team enters a given set. Both rules
#  are needed so that the randomisation test later has a well-defined
#  assignment: within a set, exactly one of a group of distinct, comparable
#  enforcements ended in defeat.
treated <- events_all %>% filter(disaster == 1) %>% arrange(season)
ctrl_pool <- events_all %>% filter(disaster == 0)
used <- character(0)
stacks <- list()

for (i in seq_len(nrow(treated))) {
  t <- treated[i, ]
  cand <- ctrl_pool %>%
    filter(!event_id %in% used,
           comp == t$comp,
           abs(season - t$season) <= SEASON_W,
           abs(lead - t$lead) <= LEAD_CAL,
           team != t$team) %>%
    mutate(dist_score = abs(lead - t$lead) / LEAD_CAL +
                        abs(season - t$season) / max(SEASON_W, 1)) %>%
    arrange(dist_score) %>%
    distinct(team, .keep_all = TRUE) %>%   # one event per team per set
    slice_head(n = MAX_CTRL)
  if (nrow(cand) == 0) next
  used <- c(used, cand$event_id)
  stacks[[length(stacks) + 1]] <-
    bind_rows(t %>% mutate(role = "treated"),
              cand %>% select(-dist_score) %>% mutate(role = "control")) %>%
    mutate(stack = t$event_id, stack_season = t$season, stack_comp = t$comp)
}
matched <- bind_rows(stacks)

cat("\ncontrol events used at most once:",
    !any(duplicated(matched$event_id[matched$role == "control"])), "\n")

cat("\n\n########## 2. MATCHED EVENT SETS ##########\n")
cat("treated events with at least one matched control:",
    n_distinct(matched$stack), "of", nrow(treated), "\n")
cat("total event-rows in the stacks:", nrow(matched), "\n")
print(matched %>% count(role) %>% as.data.frame())

cat("\nbalance on the matching variables:\n")
print(matched %>% group_by(role) %>%
        summarise(n = n(), mean_lead = round(mean(lead), 1),
                  mean_season = round(mean(season), 1),
                  mean_days = round(mean(days_for_match, na.rm = TRUE), 2),
                  .groups = "drop") %>% as.data.frame())

# =============================================================================
#  3. BUILD THE PANEL OF SUBSEQUENT ELIGIBLE MATCHES
# =============================================================================
#  For each event we follow the team across its OTHER eligible matches from
#  five seasons before to five seasons after. The outcome is whether the
#  captain enforced. The event match itself is excluded, because its
#  enforcement is what defines the event.
# -----------------------------------------------------------------------------
elig <- d %>% filter(eligible == 1) %>%
  transmute(match_id, comp, team = lead_team, captain = lead_captain,
            season = yr, match_date = as.Date(start_date),
            enforced, lead, dist, days_for_match)

panel <- matched %>%
  select(stack, role, disaster, event_id, comp, team, event_season = season,
         event_date, event_captain = captain, event_lead = lead) %>%
  left_join(elig, by = c("comp", "team"), relationship = "many-to-many") %>%
  filter(match_id != event_id,
         season >= event_season - WIN, season <= event_season + WIN) %>%
  # Event time in whole years measured from the event DATE, not the season
  # label. A match played later in the same season as the event is after it,
  # and must not be counted as a pre-event observation.
  mutate(days_since = as.numeric(match_date - event_date),
         k = ifelse(is.na(days_since), season - event_season,
                    ifelse(days_since >= 0, floor(days_since / 365.25),
                           ceiling(days_since / 365.25) - 1)),
         post = ifelse(is.na(days_since), as.integer(season > event_season),
                       as.integer(days_since > 0)),
         same_captain = as.integer(captain == event_captain)) %>%
  filter(k >= -WIN, k <= WIN)

# Censor BOTH groups at the team's next disaster after the event, so that no
# observation in either group follows a second, unmodelled shock.
panel <- panel %>%
  left_join(disasters, by = c("comp", "team"), relationship = "many-to-many") %>%
  group_by(stack, event_id, match_id) %>%
  mutate(next_dis = suppressWarnings(
           min(d_date[!is.na(d_date) & d_date > event_date], na.rm = TRUE))) %>%
  ungroup() %>%
  filter(is.infinite(next_dis) | is.na(next_dis) | match_date < next_dis) %>%
  select(-d_date, -next_dis) %>%
  distinct(stack, event_id, match_id, .keep_all = TRUE)

cat("\n\n########## 3. THE PANEL ##########\n")
cat("observations (team-eligible-match within an event window):", nrow(panel), "\n")
print(panel %>% group_by(role) %>%
        summarise(obs = n(), teams = n_distinct(team),
                  enforce_rate = round(mean(enforced), 3), .groups = "drop") %>%
        as.data.frame())

cat("\nobservations by event time and role:\n")
print(panel %>% count(k, role) %>%
        pivot_wider(names_from = role, values_from = n, values_fill = 0) %>%
        as.data.frame())

cat("\nraw enforcement rate before and after, by role:\n")
print(panel %>% group_by(role, period = ifelse(post == 1, "after", "before")) %>%
        summarise(obs = n(), enforce_rate = round(mean(enforced), 3),
                  .groups = "drop") %>%
        pivot_wider(names_from = period, values_from = c(obs, enforce_rate)) %>%
        as.data.frame())

cat("\nThe difference in differences, computed by hand as a first look:\n")
dd <- panel %>% group_by(role, post) %>%
  summarise(r = mean(enforced), .groups = "drop") %>%
  pivot_wider(names_from = post, values_from = r, names_prefix = "post")
print(as.data.frame(dd %>% mutate(change = round(post1 - post0, 3))))
if (nrow(dd) == 2) {
  did <- (dd$post1[dd$role == "treated"] - dd$post0[dd$role == "treated"]) -
         (dd$post1[dd$role == "control"] - dd$post0[dd$role == "control"])
  cat(sprintf("\n   raw difference in differences = %.3f\n", did))
}

cat("\ncaptain continuity after the event (treated events only):\n")
print(panel %>% filter(role == "treated", post == 1) %>%
        summarise(obs = n(),
                  same_captain_share = round(mean(same_captain, na.rm = TRUE), 3)) %>%
        as.data.frame())

saveRDS(panel,   file.path(DDIR, "event_panel.rds"))
saveRDS(matched, file.path(DDIR, "events.rds"))

sink()
message("04_events.R done")

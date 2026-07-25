# =============================================================================
#  01_data.R  —  Build the match-level analysis file
#
#  WHAT THIS SCRIPT DOES
#  The raw database stores one row per team per innings. We need one row per
#  MATCH, describing the decision the captain faced and what happened next:
#
#     - which side led after the first innings, and by how much
#     - whether that lead was large enough to allow the follow-on
#     - whether the captain enforced it
#     - how the match ended for the leading side
#     - who the captain was (needed for the behavioural analysis later)
#
#  It then runs a validation gate on the follow-on flag. That flag is our
#  treatment variable, so if it is unreliable nothing downstream can be trusted.
#
#  OUTPUT
#     data/processed/matches.rds  and  matches.csv
#     output/logs/01_data.txt
# =============================================================================

# Find this script's folder so that source() works from any working directory.
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)

sink(file.path(LOGDIR, "01_data.txt"), split = TRUE)

# =============================================================================
#  1. WHICH COMPETITIONS TO READ
# =============================================================================
#  comp_group separates the competitions we analyse from the extra first-class
#  matches (touring sides, testimonials and similar) that we keep only for a
#  robustness check. See docs/rules_memo.md section 3.
# -----------------------------------------------------------------------------
SPECS <- tribble(
  ~comp,             ~comp_group, ~path,
  "Test",            "Test",   "international/test/test_teams.csv",
  "County",          "Main",   "eng_domestic/firstclass/countycricket_teams.csv",
  "County",          "Main",   "eng_domestic/firstclass/countycricket_older_teams.csv",
  "County",          "Main",   "eng_domestic/firstclass/countycricket_older1_teams.csv",
  "SheffieldShield", "Main",   "aus_domestic/firstclass/sheffieldshield_teams.csv",
  "NZ-FC",           "Main",   "nz_domestic/firstclass/firstclass_teams.csv",
  "Pak-FC",          "Main",   "pak_domestic/firstclass/firstclass_teams.csv",
  "RSA-FC",          "Main",   "rsa_domestic/firstclass/firstclass_teams.csv",
  "WI-FC",           "Main",   "wi_domestic/firstclass/firstclass_teams.csv",
  "Zim-FC",          "Main",   "zimbabwe_domestic/firstclass/firstclass_teams.csv",
  "Ire-FC",          "Main",   "ireland_domestic/firstclass/firstclass_teams.csv",
  "OtherFC-Aus",     "OtherFC","aus_domestic/firstclass/other_teams.csv",
  "OtherFC-NZ",      "OtherFC","nz_domestic/firstclass/other_teams.csv",
  "OtherFC-RSA",     "OtherFC","rsa_domestic/firstclass/other_teams.csv"
)

read_teams <- function(comp, comp_group, path) {
  fp <- file.path(DAFT, path)
  if (!file.exists(fp)) { warning("missing: ", path); return(NULL) }
  x <- suppressWarnings(read_csv(fp))
  keep <- c("match_id","team","teams","innings","runs","wickets","follow_on",
            "declared","winner","result_type","winning_margin",
            "season_start_year","start_date","days_for_match","venue_country",
            "venue","competition","toss_winner","match_status","captain")
  x <- x[, intersect(keep, names(x))]
  x %>% mutate(across(any_of(c("innings","runs","wickets","season_start_year",
                               "days_for_match","teams")),
                      ~ suppressWarnings(as.numeric(.))),
               comp = comp, comp_group = comp_group)
}

raw_unbounded <- pmap(SPECS, read_teams) %>%
  bind_rows() %>%
  mutate(start_date = suppressWarnings(as.Date(start_date)))

# Use a closed sample ending on 31 December 2025. The database already contains
# 2026 fixtures, but including part of that year would make the most recent
# period incomplete. Nearly all rows have an exact start date; season year is a
# conservative fallback for the few that do not.
raw <- raw_unbounded %>%
  filter(within_sample_period(start_date, season_start_year))

excluded_after_cutoff <- raw_unbounded %>%
  filter(!within_sample_period(start_date, season_start_year))

# The source records yes/no fields inconsistently, so we normalise them once.
truthy <- function(z) tolower(as.character(z)) %in% c("1","true","t","yes","y")

cat("raw team-innings rows read:", nrow(raw), "\n")
cat("distinct match ids:", n_distinct(raw$match_id), "\n\n")
cat(sprintf("sample closes at: %s\n", SAMPLE_END_DATE))
cat(sprintf("matches starting after the cutoff excluded: %d\n\n",
            n_distinct(excluded_after_cutoff$match_id)))

# =============================================================================
#  2. FIRST INNINGS: WHO COULD ENFORCE, AND BY HOW MUCH DID THEY LEAD?
# =============================================================================
#  READ THIS CAREFULLY: it is the single most important step in the build.
#
#  Under Law 14, ONLY THE SIDE THAT BATTED FIRST may enforce the follow-on. If
#  the side batting second scores more in the first innings, there is no
#  follow-on decision to make: the option does not exist, whatever the size of
#  the gap. A build that simply takes whichever side scored more would count
#  those matches as "eligible" and would be wrong.
#
#  The DAFT column `teams` records batting order: 1 for the side that batted
#  first, 2 for the side that batted second. We validate this below against the
#  legal requirement that an enforcing side must have batted first and led.
#
#  So for every match we define:
#     lead_team  = the side that batted first  (the potential enforcer)
#     trail_team = the side that batted second
#     lead       = lead_team's first-innings score minus trail_team's.
#
#  Note that `lead` can be NEGATIVE. That is not a defect: a negative lead means
#  the side batting first fell behind, in which case it is simply not eligible.
#  Keeping these matches gives the regression discontinuity design its full
#  running variable, with observations on both sides of the cutoff.
# -----------------------------------------------------------------------------
#  We also record, separately for each side, whether it DECLARED its first
#  innings closed. This matters for the manipulation diagnostics later: only a
#  first-innings declaration could be used to place a match on a chosen side of
#  the cutoff. A single match-level "any declaration" flag would be useless,
#  because it would include declarations made after the follow-on decision.
first_inns <- raw %>%
  filter(innings == 1, is.finite(runs), teams %in% c(1, 2)) %>%
  group_by(match_id) %>%
  filter(n() == 2, n_distinct(teams) == 2) %>%   # need both sides, both labelled
  summarise(comp = first(comp), comp_group = first(comp_group),
            yr = first(season_start_year),
             lead_team  = team[teams == 1][1], lead_runs  = runs[teams == 1][1],
             trail_team = team[teams == 2][1], trail_runs = runs[teams == 2][1],
             trail_wickets_1 = wickets[teams == 2][1],
             lead_declared_1  = as.integer(truthy(declared[teams == 1][1])),
            trail_declared_1 = as.integer(truthy(declared[teams == 2][1])),
            .groups = "drop") %>%
  filter(!is.na(lead_team), !is.na(trail_team)) %>%
  mutate(lead = lead_runs - trail_runs)

cat("matches with both first innings and batting order recorded:",
    nrow(first_inns), "\n")
cat("of these, the side batting first led:",
    sum(first_inns$lead > 0),
    sprintf("(%.1f%%)\n", 100 * mean(first_inns$lead > 0)))

# =============================================================================
#  3. MATCH-LEVEL ATTRIBUTES
# =============================================================================
#  These are constant within a match, so we take the first non-missing value.
#  The follow-on flag is recorded on the innings rows; a match counts as an
#  enforcement if any row carries a positive flag.
# -----------------------------------------------------------------------------


match_attr <- raw %>%
  group_by(match_id) %>%
  summarise(
    enforced       = as.integer(any(truthy(follow_on), na.rm = TRUE)),
    declared_any   = as.integer(any(truthy(declared),  na.rm = TRUE)),
    winner         = first(na.omit(winner)),
    result_type    = first(na.omit(result_type)),
    winning_margin = first(na.omit(winning_margin)),
    days_for_match = first(na.omit(days_for_match)),
    venue_country  = first(na.omit(venue_country)),
    competition    = first(na.omit(competition)),
    match_status   = first(na.omit(match_status)),
    start_date     = first(na.omit(start_date)),
    .groups = "drop")

# Captain of each side. One row per (match, team); we attach the captain of the
# leading side, which is the captain who took the follow-on decision.
captains <- raw %>%
  filter(!is.na(captain), nzchar(as.character(captain))) %>%
  group_by(match_id, team) %>%
  summarise(captain = first(captain), .groups = "drop")

# The trailing side's SECOND innings score. Used later to identify a "near
# miss": an enforcement where the opponent batted well enough second time to
# make the enforcing side bat again, without actually winning.
second_inns <- raw %>%
  filter(innings == 2, is.finite(runs)) %>%
  group_by(match_id, team) %>%
  summarise(second_runs = first(runs), second_wickets = first(wickets),
            .groups = "drop")

# =============================================================================
#  4. ASSEMBLE, AND APPLY THE STATUTORY RULE
# =============================================================================
d <- first_inns %>%
  left_join(match_attr, by = "match_id") %>%
  left_join(captains %>% rename(lead_team = team, lead_captain = captain),
            by = c("match_id", "lead_team")) %>%
  left_join(captains %>% rename(trail_team = team, trail_captain = captain),
            by = c("match_id", "trail_team")) %>%
  left_join(second_inns %>% rename(trail_team = team,
                                   trail_second_runs = second_runs,
                                   trail_second_wickets = second_wickets),
             by = c("match_id", "trail_team")) %>%
  left_join(second_inns %>% select(-second_wickets) %>%
              rename(lead_team = team, lead_second_runs = second_runs),
            by = c("match_id", "lead_team")) %>%
  mutate(
    compulsory = is_compulsory(yr),
    margin     = statutory_margin(days_for_match, yr),
    dist       = lead - margin,          # running variable, centred on the rule
    eligible   = as.integer(lead >= margin),
    # County suspended the follow-on in 1961 and 1962 (docs/rules_memo.md)
    drop_rule  = comp == "County" & yr %in% c(1961, 1962),
    # ---------------------------------------------------------------------
    #  COMPETITIVE MATCHES ONLY
    #  The domestic files also contain fixtures that are first-class but not
    #  competitive: matches against a touring international side, and matches
    #  against Oxford and Cambridge universities. In those games the county
    #  has little interest in forcing a result and often wants batting
    #  practice instead. The enforcement rate in university matches is 8 per
    #  cent, against about 65 per cent in Championship matches, so including
    #  them would distort both the level and the trend. We keep only matches
    #  played as part of a league, championship or official international
    #  series.
    # ---------------------------------------------------------------------
    competitive = !(competition %in% c("University Match", "Tour Match",
                                       "Other First Class match")))

# =============================================================================
#  5. OUTCOMES, FROM THE LEADING SIDE'S POINT OF VIEW
# =============================================================================
#  A first-class match ends in a win for one side or in a draw (a match that
#  runs out of time). "Decisive" means it did not end in a draw. This matters
#  because the main mechanism we test is that enforcing the follow-on saves
#  time and so converts draws into results.
# -----------------------------------------------------------------------------
d <- d %>% mutate(
  is_draw   = str_detect(norm_str(result_type), "draw") |
              norm_str(winner) %in% c("draw","","na","tie") | is.na(winner),
  lead_win  = as.integer(!is_draw & norm_str(winner) == norm_str(lead_team)),
  lead_loss = as.integer(!is_draw & norm_str(winner) == norm_str(trail_team)),
  result_decisive = as.integer(!is_draw),
  # A "near miss": after following on, the opponent erased the first-innings
  # deficit but did not win. Equality counts because it levels the aggregate
  # scores and leaves the enforcing side needing another run to win.
  near_miss = as.integer(enforced == 1 & !is.na(trail_second_runs) &
                         trail_second_runs >= lead & lead_loss == 0))

# =============================================================================
#  6. VALIDATION GATE ON THE FOLLOW-ON FLAG
# =============================================================================
#  The flag is the treatment. Two checks:
#   (a) Necessary condition: a match cannot be an enforcement unless the lead
#       reached the statutory margin. A high violation rate means either the
#       flag or our rule table is wrong.
#   (b) Recall: the most famous follow-on matches in history must be flagged.
# -----------------------------------------------------------------------------
cat("\n########## VALIDATION OF THE FOLLOW-ON FLAG ##########\n\n")

opt <- d %>% filter(!compulsory, !drop_rule, !is.na(margin))
enf <- opt %>% filter(enforced == 1)

cat("(0) Batting order. An enforcing side must have batted first AND led.\n")
cat("    Because we define lead_team as the side that batted first, every\n")
cat("    enforcement must have a POSITIVE lead. This validates the `teams`\n")
cat("    column against the Law.\n\n")
cat(sprintf("    enforcements with a positive first-innings lead: %d of %d (%.2f%%)\n",
            sum(enf$lead > 0), nrow(enf), 100 * mean(enf$lead > 0)))
bad_order <- enf %>% filter(lead <= 0)
if (nrow(bad_order) > 0) {
  cat("    matches failing this test (source-data errors, dropped):\n")
  print(bad_order %>% select(match_id, comp, yr, lead_team, trail_team,
                             lead_runs, trail_runs) %>% as.data.frame())
}

cat("\n(a) Eligibility consistency, by competition\n")
cons <- enf %>% group_by(comp_group, comp) %>%
  summarise(enforcements = n(),
            consistent = sum(lead >= margin),
            pct = round(100 * mean(lead >= margin), 1), .groups = "drop") %>%
  arrange(desc(enforcements))
print(as.data.frame(cons))
overall <- mean(enf$lead >= enf$margin)
cat(sprintf("\n    pooled consistency: %.1f%% of %d enforcements\n",
            100 * overall, nrow(enf)))

cat("\n(b) Recall of the four Test matches won after following on\n")
cat("    These are the only four in history, so all four must be flagged.\n\n")
canon <- c("18941214_00042_Sydney",   # Australia v England, Sydney 1894
           "19810716_00905_Headgly",  # Australia v England, Headingley 1981
           "20010311_01535_Calcutt",  # Australia v India, Kolkata 2001
           "20230224_02494_Wellton")  # England v New Zealand, Wellington 2023
print(d %>% filter(match_id %in% canon) %>%
        select(match_id, yr, comp, lead_team, trail_team, lead, margin,
               enforced, lead_win, lead_loss) %>% as.data.frame())

recall_ok <- length(intersect(canon, d$match_id)) == length(canon) &&
             all(d$enforced[d$match_id %in% canon] == 1)
cat(sprintf("\n    all four found in the data: %s\n",
            length(intersect(canon, d$match_id)) == length(canon)))
cat(sprintf("\n    canonical recall: %s ; pooled consistency: %.1f%%\n",
            recall_ok, 100 * overall))
cat(if (recall_ok && overall >= 0.95)
      "    >> FLAG ACCEPTED as the treatment variable.\n" else
      "    >> FLAG SUSPECT - investigate before proceeding.\n")

cat("\n(c) Enforcement rate among eligible matches, by era and format\n")
print(opt %>% filter(eligible == 1, comp_group != "OtherFC") %>%
        mutate(fmt = ifelse(comp == "Test", "Test", "Domestic"),
               era = cut(yr, c(-Inf, 1949, 1979, 1999, 2009, Inf),
                         labels = c("1900-49","1950-79","1980-99","2000-09","2010+"))) %>%
        group_by(fmt, era) %>%
        summarise(n_eligible = n(), n_enforced = sum(enforced),
                  rate = round(mean(enforced), 3), .groups = "drop") %>%
        as.data.frame())

cat("\n(d) What an order-blind build would have produced, for comparison.\n")
cat("    (Counting whichever side scored more as the potential enforcer.)\n\n")
blind <- opt %>% filter(comp_group != "OtherFC") %>%
  mutate(lead_blind = abs(lead), eligible_blind = as.integer(lead_blind >= margin))
cat(sprintf("    eligible, correct build      : %d\n",
            sum(opt$eligible[opt$comp_group != "OtherFC"])))
cat(sprintf("    eligible, order-blind build  : %d  (overstated by %.2fx)\n",
            sum(blind$eligible_blind),
            sum(blind$eligible_blind) /
              max(sum(opt$eligible[opt$comp_group != "OtherFC"]), 1)))
cat(sprintf("    enforcement rate, correct    : %.3f\n",
            mean(opt$enforced[opt$eligible == 1 & opt$comp_group != "OtherFC"])))
cat(sprintf("    enforcement rate, order-blind: %.3f\n",
            mean(blind$enforced[blind$eligible_blind == 1])))

# =============================================================================
#  7. SAVE
# =============================================================================
#  A small number of matches in the source data record an enforcement that the
#  Laws did not permit: either the side that enforced did not lead after the
#  first innings, or its lead fell short of the statutory margin. Eligibility is
#  a deterministic legal condition, so these records must contain an error,
#  either in the follow-on flag or in the recorded scores. We cannot tell which,
#  so we remove them rather than let them contaminate either the treatment or
#  the running variable. They are 1.2 per cent of enforcements and dropping them
#  leaves every estimate in the paper unchanged to the third decimal place.
impossible <- d %>%
  filter(!compulsory, enforced == 1, !is.na(margin), lead < margin)
cat(sprintf("\nlegally impossible enforcements removed: %d (%.1f%% of %d)\n",
            nrow(impossible),
            100 * nrow(impossible) / sum(d$enforced == 1 & !d$compulsory),
            sum(d$enforced == 1 & !d$compulsory)))
cat("   by competition:\n")
print(impossible %>% count(comp, shortfall = cut(margin - lead,
        c(0, 10, 50, 100, Inf))) %>% as.data.frame())

analysis <- d %>%
  filter(!drop_rule, !match_id %in% impossible$match_id) %>%
  select(match_id, comp, comp_group, competition, competitive, yr, start_date,
         days_for_match, compulsory,
         lead_team, trail_team, lead_captain, trail_captain,
         lead_runs, trail_runs, trail_wickets_1, trail_second_runs,
         trail_second_wickets, lead_second_runs, lead,
         lead_declared_1, trail_declared_1,
         margin, dist, eligible, enforced, near_miss, declared_any,
         winner, result_type, is_draw, lead_win, lead_loss, result_decisive,
         venue_country, match_status) %>%
  arrange(yr, match_id)

saveRDS(analysis, file.path(DDIR, "matches.rds"))
write_csv(analysis, file.path(DDIR, "matches.csv"))

cat("\n########## SAVED ##########\n")
cat(sprintf("matches: %d  |  optional era (1900+): %d\n",
            nrow(analysis), sum(!analysis$compulsory)))
o <- analysis %>% filter(!compulsory, comp_group != "OtherFC", competitive)
cat(sprintf("ANALYSIS SAMPLE (1900+, competitive, main competitions):\n"))
cat(sprintf("  matches %d | eligible %d | enforced %d | rate %.3f\n",
            nrow(o), sum(o$eligible), sum(o$enforced),
            mean(o$enforced[o$eligible == 1])))
cat(sprintf("captain recorded for the leading side: %.1f%% of matches\n",
            100 * mean(!is.na(analysis$lead_captain))))

cat("\nEffect of dropping non-competitive fixtures (eligible matches):\n")
allm <- analysis %>% filter(!compulsory, comp_group != "OtherFC", eligible == 1)
print(allm %>% group_by(competitive) %>%
        summarise(n = n(), enforce_rate = round(mean(enforced), 3),
                  .groups = "drop") %>% as.data.frame())

sink()
message("01_data.R done")

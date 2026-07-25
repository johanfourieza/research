# =============================================================================
#  02_descriptives.R  —  Describe the decision and the fall in enforcement
#
#  WHAT THIS SCRIPT DOES
#  Three things, all descriptive:
#   (1) How often eligible captains enforce, by decade and format. This is the
#       fact the paper sets out to explain.
#   (2) The naive comparison of outcomes between enforcers and non-enforcers.
#       This comparison is confounded, and the script shows why: enforcers have
#       larger leads. It motivates the regression discontinuity design.
#   (3) How rare the feared disaster actually is.
#
#  OUTPUT
#     data/processed/descriptives.rds ; output/logs/02_descriptives.txt
# =============================================================================
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
source(file.path(SCRIPTS, "00_setup.R"), local = TRUE)

d <- readRDS(file.path(DDIR, "matches.rds"))

# Analysis sample: the optional era (from 1900, when enforcement became a
# choice), the main competitions, competitive fixtures only, margin known.
a <- d %>% filter(!compulsory, comp_group %in% c("Test", "Main"),
                  competitive, !is.na(margin))
elig <- a %>% filter(eligible == 1)

sink(file.path(LOGDIR, "02_descriptives.txt"), split = TRUE)

cat("########## SAMPLE ##########\n")
cat(sprintf("matches 1900+ : %d\n", nrow(a)))
cat(sprintf("eligible      : %d (%.1f%% of matches)\n",
            nrow(elig), 100 * mean(a$eligible)))
cat(sprintf("enforced      : %d (%.1f%% of eligible)\n",
            sum(elig$enforced), 100 * mean(elig$enforced)))
cat(sprintf("years         : %d to %d\n", min(a$yr), max(a$yr)))

# =============================================================================
#  1. THE FALL IN ENFORCEMENT
# =============================================================================
cat("\n\n########## 1. ENFORCEMENT AMONG ELIGIBLE CAPTAINS ##########\n\n")

by_decade <- elig %>%
  mutate(decade = 10 * (yr %/% 10),
         fmt = ifelse(comp == "Test", "Test", "Domestic")) %>%
  group_by(fmt, decade) %>%
  summarise(n_eligible = n(), n_enforced = sum(enforced),
            enforce_rate = mean(enforced), .groups = "drop") %>%
  filter(n_eligible >= 20)
print(by_decade %>% mutate(enforce_rate = round(enforce_rate, 3)) %>%
        as.data.frame())

cat("\nPooled, by era:\n")
by_era <- elig %>%
  mutate(fmt = ifelse(comp == "Test", "Test", "Domestic"),
         era = cut(yr, c(-Inf, 1949, 1979, 1999, 2009, Inf),
                   labels = c("1900-49","1950-79","1980-99","2000-09","2010+"))) %>%
  group_by(fmt, era) %>%
  summarise(n_eligible = n(), enforce_rate = round(mean(enforced), 3),
            .groups = "drop")
print(as.data.frame(by_era))

# =============================================================================
#  2. THE NAIVE COMPARISON, AND WHY IT MISLEADS
# =============================================================================
cat("\n\n########## 2. NAIVE OUTCOME COMPARISON ##########\n")
cat("Among eligible matches, split by what the captain chose.\n")
cat("This is NOT causal: see the lead sizes in the last column.\n\n")

naive <- elig %>%
  group_by(choice = ifelse(enforced == 1, "enforced", "batted again")) %>%
  summarise(n = n(),
            win  = round(mean(lead_win), 3),
            draw = round(mean(is_draw), 3),
            loss = round(mean(lead_loss), 3),
            mean_lead = round(mean(lead), 1),
            median_lead = median(lead), .groups = "drop")
print(as.data.frame(naive))

cat("\nThe selection problem in one line: captains who enforce lead by more.\n")
cat(sprintf("  mean lead, enforced      = %.1f runs\n",
            mean(elig$lead[elig$enforced == 1])))
cat(sprintf("  mean lead, batted again  = %.1f runs\n",
            mean(elig$lead[elig$enforced == 0])))

cat("\nSame comparison within a narrow band just above the cutoff (dist 0-25),\n")
cat("where the two groups are much more alike:\n\n")
print(elig %>% filter(dist >= 0, dist <= 25) %>%
        group_by(choice = ifelse(enforced == 1, "enforced", "batted again")) %>%
        summarise(n = n(), win = round(mean(lead_win), 3),
                  draw = round(mean(is_draw), 3),
                  loss = round(mean(lead_loss), 3),
                  mean_lead = round(mean(lead), 1), .groups = "drop") %>%
        as.data.frame())

# =============================================================================
#  3. HOW RARE IS THE DISASTER?
# =============================================================================
cat("\n\n########## 3. THE FEARED OUTCOME ##########\n\n")

cat("Probability the enforcing side LOSES, by format (optional era):\n")
print(a %>% filter(enforced == 1) %>%
        group_by(fmt = ifelse(comp == "Test", "Test", "Domestic")) %>%
        summarise(enforcements = n(), losses = sum(lead_loss),
                  loss_rate = round(mean(lead_loss), 4), .groups = "drop") %>%
        as.data.frame())

cat("\nEvery Test match ever lost after enforcing the follow-on:\n")
print(d %>% filter(comp == "Test", enforced == 1, lead_loss == 1) %>%
        select(yr, lead_team, trail_team, lead, compulsory, venue_country) %>%
        arrange(yr) %>% as.data.frame())

cat("\nFor comparison, the risk of losing after DECLINING to enforce:\n")
print(a %>% filter(eligible == 1, enforced == 0) %>%
        group_by(fmt = ifelse(comp == "Test", "Test", "Domestic")) %>%
        summarise(declines = n(), losses = sum(lead_loss),
                  loss_rate = round(mean(lead_loss), 4), .groups = "drop") %>%
        as.data.frame())

# =============================================================================
#  4. THE RUNNING VARIABLE NEAR THE CUTOFF
# =============================================================================
cat("\n\n########## 4. DISTRIBUTION OF THE RUNNING VARIABLE ##########\n")
cat("dist = lead - statutory margin. A discontinuity design needs a smooth\n")
cat("density here: a spike just above zero would suggest teams manipulate the\n")
cat("lead to become eligible, which they cannot plausibly do precisely.\n\n")
print(a %>% filter(abs(dist) <= 20) %>% count(dist) %>% as.data.frame())

saveRDS(list(by_decade = by_decade, by_era = by_era, naive = naive),
        file.path(DDIR, "descriptives.rds"))
write_csv(by_decade, file.path(TABDIR, "enforce_by_decade.csv"))

sink()
message("02_descriptives.R done")

# =============================================================================
#  00_setup.R
#  Paper: Should cricket captains enforce the follow-on?
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  This is the common header for every other script in the project. It loads the
#  R packages we use, fixes the file paths, sets the random seed, defines the
#  statutory follow-on margin (the rule that creates our natural experiment),
#  and defines the plotting style used for all figures.
#
#  HOW TO USE IT
#  Every other script starts with source("00_setup.R"). Nothing here runs an
#  analysis; it only prepares the workspace.
# =============================================================================

# Repeated fixed-effects fits in the randomisation tests can crash the Windows
# OpenMP runtime when several numerical libraries start worker threads at once.
# One thread is sufficient for this small application and makes the pipeline
# stable across direct and orchestrated runs.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1")

suppressWarnings(suppressMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(purrr); library(ggplot2); library(tibble)
}))
options(readr.show_col_types = FALSE, dplyr.summarise.inform = FALSE,
        stringsAsFactors = FALSE, scipen = 999)

# The seed fixes every random draw (bootstraps, permutation tests) so that
# re-running the code reproduces the numbers in the paper exactly.
SEED <- 20260723
set.seed(SEED)

# The source database is updated during the season. Fixing a closed sample period
# prevents a partial current season from changing the results between runs.
# Match start date is the binding criterion; season year is used only when a
# source row has no start date.
SAMPLE_END_DATE <- as.Date("2025-12-31")
SAMPLE_END_YEAR <- 2025L

within_sample_period <- function(start_date, season_year) {
  date <- suppressWarnings(as.Date(start_date))
  year <- suppressWarnings(as.integer(season_year))
  coalesce(date <= SAMPLE_END_DATE, year <= SAMPLE_END_YEAR, FALSE)
}

# ---- paths ------------------------------------------------------------------
# PROJ is this project. DAFT points at the raw commercial database, which lives
# outside the project and is NEVER copied or redistributed: we read it, build
# derived variables, and publish only the derived file.
PROJ <- "C:/Users/johanf/Dropbox/0Claude0/1Research/FourieSiebrits_Cricket/followon"
DAFT <- "C:/Users/johanf/Dropbox/0Claude0/3OurLongWalk/Books/MythsOfCricket/data/raw/daft/men"

DDIR    <- file.path(PROJ, "data/processed")
IDIR    <- file.path(PROJ, "data/interim")
FIGDIR  <- file.path(PROJ, "output/figures")
TABDIR  <- file.path(PROJ, "output/tables")
LOGDIR  <- file.path(PROJ, "output/logs")
for (d in c(DDIR, IDIR, FIGDIR, TABDIR, LOGDIR))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

SOURCE_LINE <- paste0("DAFT first-class match database ",
                      "(men's Test and domestic first-class cricket).")

# =============================================================================
#  THE STATUTORY FOLLOW-ON MARGIN
# =============================================================================
#  This function is the heart of the identification strategy, so read it slowly.
#
#  The Laws of Cricket let the side batting first enforce the follow-on only if
#  it leads by at least a stated number of runs. That number depends on how many
#  days the match is scheduled to last, and it has changed over time:
#
#    Period        Match length          Margin   Captain's choice?
#    ------------  --------------------  ------   ------------------
#    before 1900   3+ days                  120   No: follow-on compulsory
#    1900-1979     3 or more days           150   Yes
#    1900-1979     2 days                   100   Yes
#    1980-present  5 or more days           200   Yes
#    1980-present  3 or 4 days              150   Yes
#    1980-present  2 days                   100   Yes
#
#  Two features of this table do the identifying work:
#   (1) 1900 is when enforcement became a CHOICE. Before then it was automatic,
#       so there is no decision to study. We use 1900 onwards.
#   (2) In 1980 the threshold for five-day matches moved from 150 to 200, while
#       three- and four-day matches stayed at 150. Test cricket therefore
#       switched cutoff in 1980 and domestic first-class cricket did not. That
#       gives us a placebo test: after 1980 the jump in enforcement should sit
#       at 200 in Tests and at nothing in particular at 150.
#
#  NOTE that the margin is keyed on the SCHEDULED LENGTH of the match, not on
#  whether it is a Test. This matters: a Test scheduled for four days (several
#  were played in the 1970s and one in 2017) takes the 150 rule, not the 200
#  rule. Keying on "Test versus domestic" instead of days would misclassify
#  those matches and put them on the wrong side of the cutoff.
# -----------------------------------------------------------------------------
statutory_margin <- function(days, yr) {
  # days : scheduled days of play. Timeless matches are coded 99 upstream and
  #        are treated as "5 or more days" because they had no scheduled end.
  d <- ifelse(is.na(days), NA_real_, ifelse(days >= 90, 5, days))
  out <- rep(NA_real_, length(d))
  pre  <- yr < 1900                  # compulsory era, kept only for reference
  mid  <- yr >= 1900 & yr < 1980      # optional era, single 150 line for 3+ days
  post <- yr >= 1980                  # optional era, 200 for 5+ days

  out[pre]  <- ifelse(d[pre] >= 3, 120, 100)
  out[mid]  <- ifelse(d[mid] >= 3, 150, 100)
  out[post] <- ifelse(d[post] >= 5, 200, ifelse(d[post] >= 3, 150, 100))
  out
}

# Convenience: was enforcement compulsory (no captain's decision) in that year?
is_compulsory <- function(yr) yr < 1900

# =============================================================================
#  LEAP VISUAL IDENTITY  —  publication-ready graph style
# =============================================================================
#  The LEAP palette. The first four colours are the workhorses. Plum and rose,
#  and sage and earth, are hard to tell apart under deuteranopia, so a figure
#  that must work for colour-blind readers uses plum, blue, gold and teal, and
#  distinguishes series by shape and line type as well as by colour.
# -----------------------------------------------------------------------------
LEAP_COLORS <- c(
  plum  = "#5C2346",
  blue  = "#3D8EB9",
  sage  = "#6B8E5E",
  gold  = "#D4A03E",
  rose  = "#A34466",
  teal  = "#45808B",
  earth = "#8B6B3D",
  mint  = "#97C5B0"
)
LEAP_CYCLE <- unname(LEAP_COLORS)
LEAP_NONSIG_COLOR <- "#AAAAAA"
PAPER <- "#FFFFFF"

scale_fill_leap  <- function(...) scale_fill_manual(values = LEAP_CYCLE, ...)
scale_color_leap <- function(...) scale_color_manual(values = LEAP_CYCLE, ...)

theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = base_size + 1, face = "bold",
                                color = "#2D2D2D",
                                margin = ggplot2::margin(b = 12), hjust = 0),
      axis.title = element_text(size = base_size, color = "#4A4A4A"),
      axis.text  = element_text(size = base_size - 1, color = "#5A5A5A"),
      legend.text = element_text(size = base_size - 1),

      # spines: bottom and left only
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left   = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),

      # grid: horizontal only
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),

      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = "bottom", legend.title = element_blank(),

      plot.background  = element_rect(fill = PAPER, color = NA),
      panel.background = element_rect(fill = PAPER, color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      strip.text = element_text(size = base_size, face = "bold",
                                color = "#2D2D2D",
                                margin = ggplot2::margin(b = 6))
    )
}

#  LEAP figures are saved at 600 DPI in both PNG and PDF. The default LEAP size
#  is 10 by 6 inches. These figures are reduced to the text width of a journal
#  page, so we raise the base font size in the plotting code to keep the axis
#  text legible once the figure is scaled down in print.
save_leap_fig <- function(stem, plot = last_plot(), width = 10, height = 6,
                          dpi = 600) {
  ggsave(file.path(FIGDIR, paste0(stem, ".png")), plot, width = width,
         height = height, dpi = dpi, bg = PAPER)
  ggsave(file.path(FIGDIR, paste0(stem, ".pdf")), plot, width = width,
         height = height, bg = PAPER, device = cairo_pdf)
  invisible(stem)
}

# Small helper used in several scripts: tidy lower-case string comparison.
norm_str <- function(z) str_squish(tolower(as.character(z)))

message("00_setup.R sourced OK  |  seed = ", SEED)

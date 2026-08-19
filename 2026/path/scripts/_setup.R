# =============================================================================
# _setup.R -- shared setup for all scripts in this replication package
# -----------------------------------------------------------------------------
# Sourced at the top of every numbered script. Provides:
#   - package loading (no installation; stops with instructions if missing)
#   - project paths (relative to the replication/ root)
#   - robust-standard-error helpers used throughout
#   - seed constants for the stochastic scripts
#   - logging helpers (one log file per script in output/logs/)
#   - the figure theme and palette (used by 10_figures.R)
#
# Paper: "Testing for path dependence in economic history publications"
# =============================================================================

# --- Locate the replication root ---------------------------------------------
# Works under Rscript (via --file), in RStudio, or interactively if the working
# directory is the replication root or its scripts/ subfolder.
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p)))
  if (file.exists("scripts/_setup.R")) return(normalizePath("scripts"))
  if (file.exists("_setup.R"))         return(normalizePath("."))
  stop("Cannot locate the scripts/ directory. Run from the replication root.")
}
SCRIPT_DIR <- .get_script_dir()
ROOT       <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/")

DATA_RAW    <- file.path(ROOT, "data", "raw")
DATA_CACHE  <- file.path(ROOT, "data", "cache")
RESULTS_DIR <- file.path(ROOT, "results")
FIG_DIR     <- file.path(ROOT, "output", "figures")
TAB_DIR     <- file.path(ROOT, "output", "tables")
LOG_DIR     <- file.path(ROOT, "output", "logs")
for (d in c(RESULTS_DIR, FIG_DIR, TAB_DIR, LOG_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# --- Packages -----------------------------------------------------------------
# Versions used for the published run are listed in README.md and recorded in
# output/logs/sessionInfo.txt. Nothing is installed automatically.
.required <- c("data.table", "lfe", "fixest", "stargazer", "boot",
               "stringdist", "ggplot2", "scales", "igraph", "readxl",
               "patchwork")
.missing <- .required[!vapply(.required, requireNamespace, logical(1),
                              quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\nInstall them with: install.packages(c(",
       paste0('"', .missing, '"', collapse = ", "), "))")
}
suppressMessages({
  library(data.table); library(lfe); library(fixest); library(stargazer)
  library(boot); library(stringdist); library(ggplot2); library(scales)
})

# Shared name-cleaning and conference-workbook readers (single source of truth)
source(file.path(SCRIPT_DIR, "conference_data_helpers.R"))

# --- Seeds ---------------------------------------------------------------------
# Each stochastic script sets its own seed at the top, so scripts are
# individually reproducible regardless of run order.
SEED_BOOT    <- 42   # 03_robustness.R (coefficient bootstrap)
SEED_PLACEBO <- 42   # 04_placebo.R and 05_conference.R (permutation tests)
SEED_LUCK    <- 42   # 08_attenuation_luck.R (decomposition bootstrap)

# --- Standard-error helpers -----------------------------------------------------
# Cross-sectional felm models report heteroskedasticity-robust (White) SEs.
# rob_se() returns the robust SE for one coefficient; rob_se_vec() the full
# vector (for stargazer se = ... lists).
rob_se <- function(m, var) {
  ct <- summary(m, robust = TRUE)$coefficients
  if (!var %in% rownames(ct)) return(NA_real_)
  ct[var, 2]
}
rob_se_vec <- function(m) {
  ct <- summary(m, robust = TRUE)$coefficients
  setNames(ct[, 2], rownames(ct))
}
Fstat    <- function(m, v) as.numeric((coef(m)[v] / rob_se(m, v))^2)
R2manual <- function(m, y) 1 - sum(m$residuals^2) / sum((y - mean(y))^2)

# --- Logging ---------------------------------------------------------------------
# open_log("01_build_sample") tees console output to output/logs/01_build_sample.log
open_log <- function(name) {
  sink(file.path(LOG_DIR, paste0(name, ".log")), split = TRUE)
  cat("====================================================================\n")
  cat(" ", name, "-- run date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("====================================================================\n\n")
}
close_log <- function() while (sink.number() > 0) sink()

# --- Figure theme (LEAP visual identity) ------------------------------------------
LEAP_COLORS <- c(plum = "#5C2346", blue = "#3D8EB9", sage = "#6B8E5E",
                 gold = "#D4A03E", rose = "#A34466", teal = "#45808B",
                 earth = "#8B6B3D", mint = "#97C5B0")
LEAP_NONSIG_COLOR <- "#AAAAAA"

theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = 11, face = "bold", color = "#2D2D2D",
                                margin = ggplot2::margin(b = 12), hjust = 0),
      plot.subtitle = element_text(size = 9, color = "#5A5A5A",
                                   margin = ggplot2::margin(b = 8), hjust = 0),
      axis.title = element_text(size = 10, color = "#4A4A4A"),
      axis.text = element_text(size = 9, color = "#5A5A5A"),
      legend.text = element_text(size = 9),
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      strip.text = element_text(size = 10, face = "bold", color = "#2D2D2D")
    )
}

save_fig <- function(stem, plot, width = 10, height = 6, dpi = 600) {
  ggsave(file.path(FIG_DIR, paste0(stem, ".png")), plot,
         width = width, height = height, dpi = dpi)
  ggsave(file.path(FIG_DIR, paste0(stem, ".pdf")), plot,
         width = width, height = height)
  cat("Saved:", stem, "(.png + .pdf)\n")
}

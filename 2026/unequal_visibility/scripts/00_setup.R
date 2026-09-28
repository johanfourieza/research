# =============================================================================
#  00_setup.R
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  The common header for every other script. It loads the R packages, finds the
#  data/ and output/ folders, and defines three small tools used throughout:
#  the Wilson interval for a binomial proportion, a check() function that stops
#  the run if a number differs from the one printed in the paper, and the LEAP
#  plotting style.
#
#  HOW TO USE IT
#  Every other script starts with source("00_setup.R"). Nothing here runs an
#  analysis; it only prepares the workspace.
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
})
options(readr.show_col_types = FALSE, dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------
#  Paths. The scripts live in scripts/; data/ and output/ sit one level up.
# -----------------------------------------------------------------------------
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPTS <- if (length(.file)) dirname(normalizePath(.file)) else getwd()
if (!file.exists(file.path(SCRIPTS, "00_setup.R"))) SCRIPTS <- file.path(getwd(), "scripts")
ROOT    <- normalizePath(file.path(SCRIPTS, ".."), winslash = "/")
DATA    <- file.path(ROOT, "data")
OUT_TAB <- file.path(ROOT, "output", "tables")
OUT_FIG <- file.path(ROOT, "output", "figures")
for (d in c(OUT_TAB, OUT_FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

read_data <- function(file) read_csv(file.path(DATA, file))

# -----------------------------------------------------------------------------
#  Wilson 95 per cent interval for x events among n people. This is the
#  interval reported in Table 2. It describes sampling uncertainty in the
#  recorded rate under a binomial reference model, conditional on the cohort
#  and the accepted death links. It says nothing about deaths that left no record.
# -----------------------------------------------------------------------------
wilson <- function(x, n, z = qnorm(0.975)) {
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble(lo = pmax(0, centre - half), hi = pmin(1, centre + half))
}

# -----------------------------------------------------------------------------
#  check(): compare a recomputed number with the value printed in the paper,
#  after rounding to the paper's precision. Stops the run on any mismatch.
# -----------------------------------------------------------------------------
check <- function(label, value, paper, digits) {
  got <- round(value, digits)
  ok  <- isTRUE(all.equal(got, paper, tolerance = 1e-9))
  cat(sprintf("  %-58s %10s   paper: %s  %s\n", label,
              formatC(got, format = "f", digits = digits), paper,
              if (ok) "ok" else "MISMATCH"))
  if (!ok) stop("Recomputed value differs from the paper: ", label)
  invisible(ok)
}

# -----------------------------------------------------------------------------
#  LEAP figure style (Laboratory for the Economics of Africa's Past)
# -----------------------------------------------------------------------------
LEAP <- c(plum = "#5C2346", blue = "#3D8EB9", sage = "#6B8E5E", gold = "#D4A03E")
LEAP_GREY <- "#AAAAAA"

theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      axis.title = element_text(size = 10, colour = "#4A4A4A"),
      axis.text  = element_text(size = 9, colour = "#5A5A5A"),
      legend.text = element_text(size = 9),
      axis.line.x.bottom = element_line(colour = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left   = element_line(colour = "#4A4A4A", linewidth = 0.8),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#4A4A4A", linewidth = 0.6),
      legend.background = element_blank(), legend.key = element_blank(),
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      plot.margin = margin(10, 10, 10, 10),
      strip.text = element_text(size = 10, face = "bold", colour = "#2D2D2D")
    )
}

# Save each figure as PNG (600 dpi) and PDF. cairo_pdf embeds the fonts.
save_fig <- function(plot, name, width = 10, height = 6) {
  ggsave(file.path(OUT_FIG, paste0(name, ".png")), plot, width = width,
         height = height, dpi = 600, bg = "white")
  dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggsave(file.path(OUT_FIG, paste0(name, ".pdf")), plot, device = dev,
         width = width, height = height, bg = "white")
  cat("  saved output/figures/", name, ".png and .pdf\n", sep = "")
}

GROUP_LEVELS <- c("low", "middle", "high")   # 0, 1-4 and 5+ adult slaves recorded

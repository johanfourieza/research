# ==============================================================================
# VOORTREKKER SELECTION ANALYSIS — COMPLETE REPRODUCIBLE SCRIPT
# ==============================================================================
# Paper: "Selection into the Great Trek" (Fourie and Links)
# Target: European Review of Economic History
#
# This script combines all analysis code into a single reproducible file.
# It produces all tables, figures and output files referenced in the paper.
#
# STRUCTURE:
#   Section A: Setup and data loading (Paper Section 3)
#   Section B: Record linkage and main analysis (Paper Sections 4-6)
#   Section C: Robustness checks (Paper Section 7 + Appendices)
#   Section D: Maps and district-level figures (Paper Figures 1-3)
#   Section E: Summary and file verification
#
# INSTRUCTIONS:
#   Run code/run_all.R from the replication folder (see README.md); it runs this
#   script for both linkages. Outputs go to output/tables/ and output/figures/.
#
# DEPENDENCIES:
#   R packages: readxl, tidyverse, data.table, stringdist, MatchIt,
#               stargazer, sandwich, lmtest, broom, janitor, nnet,
#               randomForest, writexl, sf, patchwork, maps
#
# RUNTIME: Approximately 15-20 minutes for the full pipeline
# ==============================================================================


# ==============================================================================
# SECTION A: SETUP AND DATA LOADING
# Paper Section 3: Data
# ==============================================================================

# PART 0: SETUP ---------------------------------------------------------------

rm(list = ls())

required_packages <- c(
  "readxl", "tidyverse", "data.table", "stringdist", "MatchIt",
  "stargazer", "sandwich", "lmtest", "broom", "janitor", "nnet",
  "randomForest", "writexl", "sf", "patchwork", "maps", "xgboost"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running voortrekker_final.R.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(data.table)
  library(stringdist)
  library(MatchIt)
  library(stargazer)
  library(sandwich)
  library(lmtest)
  library(broom)
  library(janitor)  # For clean_names()
  library(nnet)     # For multinomial logit
})

resolve_replication_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])

  candidate_dirs <- unique(c(
    if (length(script_path) > 0) dirname(normalizePath(script_path[1], winslash = "/", mustWork = FALSE)) else NULL,
    tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE)), error = function(e) NULL),
    tryCatch(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path, winslash = "/", mustWork = FALSE)), error = function(e) NULL),
    getwd()
  ))

  is_replication_root <- function(dir) {
    file.exists(file.path(dir, "code", "pipeline.R")) &&
      dir.exists(file.path(dir, "data")) &&
      file.exists(file.path(dir, "data", "inputs", "source_md5.txt"))
  }

  for (candidate in candidate_dirs) {
    if (is.null(candidate) || is.na(candidate) || candidate == "") next
    normalized_candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)

    # Direct match: candidate is already replication/ or replication/code/.
    root_guess <- if (basename(normalized_candidate) == "code") dirname(normalized_candidate) else normalized_candidate
    if (is_replication_root(root_guess)) return(root_guess)

    # Ancestor search: walk upward and look for a sibling "replication" folder,
    # so the script runs from anywhere inside the project tree.
    cur <- normalized_candidate
    for (i in seq_len(8)) {
      parent <- dirname(cur)
      if (parent == cur) break
      sibling <- file.path(parent, "replication")
      if (dir.exists(sibling) && is_replication_root(sibling)) {
        return(normalizePath(sibling, winslash = "/", mustWork = FALSE))
      }
      cur <- parent
    }
  }

  stop("Could not find the replication folder. Run this script from a folder inside the project tree.", call. = FALSE)
}

# ============================================================================
# LEAP VISUAL IDENTITY - Publication-Ready Graph Style
# ============================================================================

# LEAP colour palette
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

# Utility colours
LEAP_NONSIG_COLOR <- "#AAAAAA"

# Scale functions for ggplot2
scale_fill_leap <- function(...) {
  scale_fill_manual(values = LEAP_CYCLE, ...)
}

scale_color_leap <- function(...) {
  scale_color_manual(values = LEAP_CYCLE, ...)
}

# LEAP ggplot2 theme
theme_leap <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      # Text
      text = element_text(family = "sans"),
      plot.title = element_text(
        size = 11, face = "bold", color = "#2D2D2D",
        margin = ggplot2::margin(b = 12), hjust = 0
      ),
      axis.title = element_text(size = 10, color = "#4A4A4A"),
      axis.text = element_text(size = 9, color = "#5A5A5A"),
      legend.text = element_text(size = 9),

      # Spines: only bottom and left
      axis.line.x.bottom = element_line(color = "#4A4A4A", linewidth = 0.8),
      axis.line.y.left = element_line(color = "#4A4A4A", linewidth = 0.8),
      panel.border = element_blank(),

      # Grid: horizontal only
      panel.grid.major.y = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      # Ticks
      axis.ticks = element_line(color = "#4A4A4A", linewidth = 0.6),
      axis.ticks.length = unit(3, "pt"),

      # Legend: no frame
      legend.background = element_blank(),
      legend.key = element_blank(),

      # Background
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),

      # Margins
      plot.margin = ggplot2::margin(10, 10, 10, 10),

      # Strip text for facets
      strip.text = element_text(size = 10, face = "bold", color = "#2D2D2D")
    )
}

# Helper: save LEAP figure in both PNG and PDF at 300 DPI
save_leap_fig <- function(fig_path, plot, width, height, dpi = 300) {
  png_path <- sub("\\.[^.]+$", ".png", fig_path)
  ggsave(png_path, plot, width = width, height = height, dpi = dpi)
  pdf_path <- sub("\\.[^.]+$", ".pdf", fig_path)
  ggsave(pdf_path, plot, width = width, height = height)
  cat("Saved:", png_path, "and", pdf_path, "\n")
}

# ---------------------------------------------------------------------------
# Helper: district fixed-effects regression with robust SEs
# ---------------------------------------------------------------------------
run_fe_model <- function(data, outcome, treat = "is_voortrekker",
                         fe = "district", vcov_type = "HC1") {
  rhs <- paste0(treat, " + factor(", fe, ")")
  fml <- as.formula(paste(outcome, "~", rhs))
  mod <- lm(fml, data = data)
  ct <- coeftest(mod, vcov = vcovHC(mod, type = vcov_type))
  treat_row <- grep(paste0("^", treat), rownames(ct), value = TRUE)[1]
  data.frame(
    outcome    = outcome,
    treat      = treat,
    coef       = ct[treat_row, "Estimate"],
    se         = ct[treat_row, "Std. Error"],
    p          = ct[treat_row, "Pr(>|t|)"],
    n_total    = nobs(mod),
    n_treated  = sum(model.frame(mod)[[treat]], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper: run FE model across multiple outcomes
run_fe_batch <- function(data, outcomes, treat = "is_voortrekker",
                         fe = "district", vcov_type = "HC1") {
  bind_rows(lapply(outcomes, function(v) {
    tryCatch(run_fe_model(data, v, treat, fe, vcov_type),
             error = function(e) data.frame(outcome = v, treat = treat,
                                            coef = NA, se = NA, p = NA,
                                            n_total = NA, n_treated = NA))
  }))
}

# Helper: assertion check
assert_count <- function(actual, expected, msg) {
  if (!identical(as.integer(actual), as.integer(expected))) {
    warning(paste0("ASSERTION FAILED: ", msg,
                   " (expected ", expected, ", got ", actual, ")"), call. = FALSE)
  }
}

# Helper: write CSV without aborting the pipeline if a file is open elsewhere
safe_write_csv <- function(x, file, ...) {
  tryCatch({
    write.csv(x, file, ...)
    file
  }, error = function(e) {
    fallback <- file.path(
      tempdir(),
      paste0(tools::file_path_sans_ext(basename(file)),
             "_fallback_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    )
    warning(sprintf("Could not write %s (%s). Writing fallback file instead: %s",
                    file, e$message, fallback),
            call. = FALSE)
    write.csv(x, fallback, ...)
    fallback
  })
}

# ---------------------------------------------------------------------------
# Helpers: LaTeX table fragments
# Fragments written to output/tables/tex/ and \input{} by manuscript.tex so
# that the pipeline, not manual transcription, produces the paper's numbers.
# ---------------------------------------------------------------------------

# Format an estimate with magnitude-dependent decimals
fmt_est <- function(x, digits = NULL) {
  if (is.na(x)) return("")
  if (!is.null(digits)) return(sprintf(paste0("%.", digits, "f"), x))
  ax <- abs(x)
  if (ax >= 100) sprintf("%.1f", x)
  else if (ax >= 10) sprintf("%.2f", x)
  else sprintf("%.3f", x)
}

# Format a p-value
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<$0.001" else sprintf("%.3f", p)
}

# Significance stars (noted in each table's legend)
stars_for <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
}

# Write a character vector of LaTeX lines as a table fragment.
# Adds a "generated" header comment; creates the tex/ dir on first use.
write_tex_fragment <- function(lines, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  header <- c(
    paste0("% Generated by pipeline.R on ", format(Sys.Date(), "%Y-%m-%d"), "."),
    "% Do not edit by hand: rerun the pipeline to refresh."
  )
  writeLines(c(header, lines), file)
  cat("  Exported tex fragment:", file, "\n")
}

# ---------------------------------------------------------------------------
# District metadata (wife-name availability)
# Wife names were recorded as a structured field in Graaff-Reinet and
# Swellendam opgaafrolle. Other districts record names but wife fields
# are less systematically populated.
# ---------------------------------------------------------------------------
district_meta <- tibble::tribble(
  ~district,         ~wife_info_available,
  "Albany",          FALSE,
  "Beaufort",        FALSE,
  "Cape",            FALSE,
  "Clanwilliam",     FALSE,
  "Colesberg",       FALSE,
  "Cradock",         FALSE,
  "George",          FALSE,
  "Graaff-Reinet",   TRUE,
  "Somerset",        FALSE,
  "Stellenbosch",    FALSE,
  "Swellendam",      TRUE,
  "Uitenhage",       FALSE,
  "Worcester",       FALSE
)

collapse_vt_origin_district <- function(x) {
  case_when(
    x %in% c("Cradock", "Somerset_multi") ~ "Somerset",
    x == "Colesberg_multi" ~ "Graaff-Reinet",
    x == "Clanwilliam" ~ "Worcester",
    TRUE ~ x
  )
}

# Set working directory robustly for both RStudio and batch execution.
# The pipeline runs from code/, reads raw data from data/raw/ and writes
# outputs to output/. Setting the working directory to the replication root
# makes all relative paths below valid.
replication_root <- resolve_replication_root()
setwd(replication_root)
cat("Working directory:", getwd(), "\n")
# explicit census name parser and shared name standardisation.
source("code/parse_names.R")

# Output directory structure
out_tables <- "output/tables"
out_figs   <- "output/figures"
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figs,   recursive = TRUE, showWarnings = FALSE)

fig_dir <- out_figs

# Figure counter for numbered output
fig_num <- 0
next_fig <- function(name) {
  fig_num <<- fig_num + 1
  sprintf("%s/Fig%02d_%s", out_figs, fig_num, name)
}

# --- Part 1: Load and clean 1825 census data (Paper Section 3) ---

# ============================================================================
# PART 1: LOAD AND CLEAN 1825 CENSUS DATA
# ============================================================================

# Helper function: clean and convert columns to numeric
to_numeric <- function(x) {
  z <- trimws(as.character(x))
  valid <- is.na(z) | z == "" | grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", z)
  if (any(!valid)) stop("Unrecognised numeric content; resolve it in the source ledger.")
  as.numeric(z)
}

# --------------------------------------------------------------------------
# 1.1 CAPE DISTRICT 1825
# --------------------------------------------------------------------------

df <- read_excel("data/raw/1825 series.xlsx", sheet = "Cape district 1825", col_names = FALSE)

# Build column names from first 3 header rows
col_names <- df %>%
  slice(1:3) %>% t() %>% as.data.frame() %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
# Trim whitespace from column names
col_names <- trimws(col_names)
col_names <- make.unique(col_names)
colnames(df) <- col_names
df$source_row <- seq_len(nrow(df))
df <- df %>% slice(-1:-3)

# Extract names: col 2 (1-indexed) = Names, col 3 = Veld Cornet
# The name column has the long header; the Veld Cornet is col 3
name_col_ct <- names(df)[2]  # The names column
vc_col_ct   <- names(df)[3]  # Veld Cornet

district_ct <- df %>%
  mutate(
    record_nr     = row_number(),
    name_raw      = .data[[name_col_ct]],
    sublocation   = .data[[vc_col_ct]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `MALES ABOVE 16; FEMALES ABOVE 20_Whites_Males`,
    settler_women = Females,
    khoe_men = `Hottentots_Males`,
    khoe_women = `Females.1`,
    freeblacks_men = `Free Blacks_Males`,
    freeblacks_women = `Females.2`,
    prize_men = `Prize Negroes_Males`,
    prize_women = `Females.3`,
    slaves_men = `Slaves_Males`,
    slaves_women = `Females.4`,
    settler_sons = `MALES BELOW 16; FEMALES BELOW 20_Whites_Males`,
    settler_daughters = `Females.5`,
    khoe_sons = `Hottentots_Males.1`,
    khoe_daughters = `Females.6`,
    freeblacks_sons = `Free Blacks_Males.1`,
    freeblacks_daughters = `Females.7`,
    prize_sons = `Prize Negroes_Males.1`,
    prize_daughters = `Females.8`,
    slaves_sons = `Slaves_Males.1`,
    slaves_daughters = `Females.9`,
    horses_saddle = `Horses_Saddle`,
    horses_breeding = Breeding,
    cattle_oxen = `Cattle_Draught Oxen`,
    cattle_breeding = `Breeding stock`,
    sheep_wethers = Wethers,
    sheep_breeding = `Sheep_Breeding`,
    sheep_spanish = Spanish,
    donkeys = `Asses and Mules`,
    goats = Goats,
    pigs = Pigs,
    wheat_sown = `Quantity of Muids sown_Wheat`,
    barley_sown = Barley,
    rye_sown = Rye,
    oats_sown = Oats,
    wheat_reaped = `Quantity of Muids reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    hay = `Pounds of Hay`,
    wine = `Leaguers of wine made`,
    brandy = `Leaguers of Brandy made`
  )

# --------------------------------------------------------------------------
# 1.2 STELLENBOSCH 1825
# --------------------------------------------------------------------------

df_stellenbosch <- read_excel("data/raw/1825 series.xlsx", sheet = "Stellenbosch 1825", col_names = FALSE)

col_names_sb <- df_stellenbosch %>%
  slice(1:3) %>% t() %>% as.data.frame() %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_sb <- trimws(col_names_sb)
col_names_sb <- make.unique(col_names_sb)
colnames(df_stellenbosch) <- col_names_sb
df_stellenbosch$source_row <- seq_len(nrow(df_stellenbosch))
df_stellenbosch <- df_stellenbosch %>% slice(-1:-3)
df_stellenbosch[[2]] <- coalesce(df_stellenbosch[[2]], df_stellenbosch[[3]])
df_stellenbosch <- df_stellenbosch %>% filter(!is.na(df_stellenbosch[[2]]))

# col 2 = men names, col 3 = women names, col 4 = ward
district_sb <- df_stellenbosch %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_stellenbosch[[2]],
    name_women  = df_stellenbosch[[3]],
    sublocation = df_stellenbosch[[4]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `White Inhabitants_Males`,
    settler_women = `Females`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 14.1`,
    freeblacks_men = `Free Blacks_Males`,
    freeblacks_women = `Females.1`,
    prize_men = `Prize Negroes (including those born in this colony)_Males above 16`,
    prize_women = `Females above 14`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 14.2`,
    settler_sons = `Sons below 16`,
    settler_daughters = `Daughters below 20`,
    khoe_sons = `Males below 16.1`,
    khoe_daughters = `Females below 14.1`,
    freeblacks_sons = `Sons below 16.1`,
    freeblacks_daughters = `Daughters below 20.1`,
    prize_sons = `Males below 16`,
    prize_daughters = `Females below 14`,
    slaves_sons = `Males below 16.2`,
    slaves_daughters = `Females below 14.2`,
    horses_saddle = `Horses_Saddle & Waggon`,
    horses_breeding = `Breeding`,
    cattle_oxen = `Black Cattle_Draught Oxen`,
    cattle_breeding = `Breeding Stock`,
    sheep_wethers = `Sheep_Wethers`,
    sheep_breeding = `Breeding.1`,
    sheep_spanish = `Spanish`,
    donkeys = `Asses & Mules`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Quantity of Muids Sown in 1824_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Quantity of Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    hay = `Pounds of Hay`,
    wine = `Leagers made of_Wine`,
    brandy = `Brandy`
  )

# --------------------------------------------------------------------------
# 1.3 GRAAFF-REINET 1825
# --------------------------------------------------------------------------

df_gr <- read_excel("data/raw/1825 series.xlsx", sheet = "Graaff-Reinet 1825", col_names = FALSE)

col_names_gr <- df_gr %>%
  slice(1:4) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_gr[col_names_gr == ""] <- "Unnamed"
col_names_gr <- trimws(col_names_gr)
col_names_gr <- make.unique(col_names_gr)
colnames(df_gr) <- col_names_gr
df_gr$source_row <- seq_len(nrow(df_gr))
df_gr <- df_gr %>% slice(-1:-4)
df_gr[[2]] <- coalesce(df_gr[[2]], df_gr[[3]])
df_gr <- df_gr %>% filter(!is.na(df_gr[[2]]))

# col 2 = names (men), col 3 = names (women)
district_gr <- df_gr %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_gr[[2]],
    wife_name_raw = df_gr[[3]],  # Graaff-Reinet has separate wife column
    sublocation = NA_character_
  ) %>%
  select(
    record_nr, source_row, name_raw, wife_name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women.1`,
    khoe_men = `Hottentots_Males_above 16`,
    khoe_women = `Females_above 20`,
    freeblacks_men = `Free Blacks_Males_above 16`,
    freeblacks_women = `Females_above 20.1`,
    prize_men = `Prize Negroes_Males_above 16`,
    prize_women = `Females_above 20.3`,
    slaves_men = `Slaves_Males_above 16`,
    slaves_women = `Females_above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males_below 16`,
    khoe_daughters = `Females_below 20`,
    freeblacks_sons = `Males_below 16.1`,
    freeblacks_daughters = `Females_below 20.1`,
    prize_sons = `Males_below 16.3`,
    prize_daughters = `Females_below 20.3`,
    slaves_sons = `Males_below 16.2`,
    slaves_daughters = `Females_below 20.2`,
    horses_saddle = `Livestock_Wagon &_saddle_horses`,
    horses_breeding = `Breeding_horses`,
    cattle_oxen = `Oxen`,
    cattle_breeding = `Breeding_cattle`,
    sheep_wethers = `Wethers`,
    sheep_breeding = `Breeding_sheep`,
    sheep_spanish = `Spanish_sheep`,
    donkeys = `Asses`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leggers_of wine`,
    brandy = `Leggers_of brandy`
  )

# --------------------------------------------------------------------------
# 1.4 SWELLENDAM 1825
# --------------------------------------------------------------------------

df_sw <- read_excel("data/raw/1825 series.xlsx", sheet = "Swellendam 1825", col_names = FALSE)

col_names_sw <- df_sw %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_sw[col_names_sw == ""] <- "Unnamed"
col_names_sw <- trimws(col_names_sw)
col_names_sw <- make.unique(col_names_sw)
colnames(df_sw) <- col_names_sw
df_sw$source_row <- seq_len(nrow(df_sw))
df_sw <- df_sw %>% slice(-1:-2)
df_sw$Nr. <- as.numeric(df_sw$Nr.)

# Swellendam has alternating rows: numbered rows = household head (man),
# unnumbered rows following = wife name
# Extract wife names, then remove the wife-only rows
df_sw$settler_women_names <- NA_character_
for (i in seq_len(nrow(df_sw) - 1L)) {
  head_couple <- !is.na(df_sw$Families_Men[i]) && df_sw$Families_Men[i] == "1" &&
                !is.na(df_sw$Women[i]) && df_sw$Women[i] == "1"
  next_counts <- unlist(df_sw[i+1L, c("Families_Men", "Women", "Sons", "Daughters")])
  next_zero <- all(is.na(next_counts) | next_counts %in% c("", "0"))
  if (head_couple && next_zero && is.na(df_sw$Nr.[i+1L]))
    df_sw$settler_women_names[i] <- df_sw[[2]][i+1L]
}
# Eligibility is determined by the actual family counts below; no unnumbered
# independent head is deleted merely because of the preceding row's number.
df_sw_clean <- df_sw[!is.na(df_sw[[2]]), ]

# col 2 = names, col 3 = fieldcornetry
district_sw <- df_sw_clean %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_sw_clean[[2]],
    wife_name_raw = settler_women_names,
    sublocation = df_sw_clean[[3]]
  ) %>%
  select(
    record_nr, source_row, name_raw, wife_name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 20`,
    freeblacks_men = `Free Blacks_Males above 16`,
    freeblacks_women = `Females above 20.1`,
    prize_men = `Prize Negroes_Males above 16`,
    prize_women = `Females above 20.3`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males below 16`,
    khoe_daughters = `Females below 20`,
    freeblacks_sons = `Males below 16.1`,
    freeblacks_daughters = `Females below 20.1`,
    prize_sons = `Males below 16.3`,
    prize_daughters = `Females below 20.3`,
    slaves_sons = `Males below 16.2`,
    slaves_daughters = `Females below 20.2`,
    horses_saddle = `Horses_Saddle & Waggon`,
    horses_breeding = `Breeding`,
    cattle_oxen = `Black Cattle_Draught Oxen`,
    cattle_breeding = `Breeding Stock`,
    sheep_wethers = `Sheep_Wethers`,
    sheep_breeding = `Breeding.1`,
    sheep_spanish = `Spanish`,
    donkeys = `Asses & Mules`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids Sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leagers made of_Wine`,
    brandy = `Brandy`
  )

# --------------------------------------------------------------------------
# 1.5 ALBANY 1825
# --------------------------------------------------------------------------

df_al <- read_excel("data/raw/1825 series.xlsx", sheet = "Albany 1825", col_names = FALSE)

col_names_al <- df_al %>%
  slice(1:3) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_al[col_names_al == ""] <- "Unnamed"
col_names_al <- trimws(col_names_al)
col_names_al <- make.unique(col_names_al)
colnames(df_al) <- col_names_al
df_al$source_row <- seq_len(nrow(df_al))
df_al <- df_al %>% slice(-1:-3)

# col 4 = men names, col 5 = women names, col 2 = district
district_al <- df_al %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_al[[4]],
    sublocation = df_al[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women.1`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 20`,
    freeblacks_men = `Free Blacks_Males above 16`,
    freeblacks_women = `Females above 20.1`,
    prize_men = `Prize Negroes_Males above 16`,
    prize_women = `Females above 20.3`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males under 16`,
    khoe_daughters = `Females under 20`,
    freeblacks_sons = `Males under 16.1`,
    freeblacks_daughters = `Females under 20.1`,
    prize_sons = `Males under 16.3`,
    prize_daughters = `Females under 20.3`,
    slaves_sons = `Males under 16.2`,
    slaves_daughters = `Females under 20.2`,
    horses_saddle = `Cattle_Waggon & Saddle Horses`,
    horses_breeding = `Breeding Horses`,
    cattle_oxen = `Oxen`,
    cattle_breeding = `Breeding Cattle`,
    sheep_wethers = `Wethers`,
    sheep_breeding = `Breeding Sheep`,
    sheep_spanish = `Spanish Sheep`,
    donkeys = `Asses`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids Sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leggers of wine made`,
    brandy = `Leggers of Brandy made`
  )

# --------------------------------------------------------------------------
# 1.6 BEAUFORT 1825
# --------------------------------------------------------------------------

df_bf <- read_excel("data/raw/1825 series.xlsx", sheet = "Beaufort 1825", col_names = FALSE)

col_names_bf <- df_bf %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_bf[col_names_bf == ""] <- "Unnamed"
col_names_bf <- trimws(col_names_bf)
col_names_bf <- make.unique(col_names_bf)
colnames(df_bf) <- col_names_bf
df_bf$source_row <- seq_len(nrow(df_bf))
df_bf <- df_bf %>% slice(-1:-2)

# col 4 = men names, col 5 = women names, col 2 = district/FC
district_bf <- df_bf %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_bf[[4]],
    sublocation = df_bf[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women.1`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 20`,
    freeblacks_men = `Free Blacks_Males above 16`,
    freeblacks_women = `Females above 20.1`,
    prize_men = `Prize Negroes_Males above 16`,
    prize_women = `Females above 20.3`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males under 16`,
    khoe_daughters = `Females under 20`,
    freeblacks_sons = `Males under 16.1`,
    freeblacks_daughters = `Females under 20.1`,
    prize_sons = `Males under 16.3`,
    prize_daughters = `Females under 20.3`,
    slaves_sons = `Males under 16.2`,
    slaves_daughters = `Females under 20.2`,
    horses_saddle = `Cattle_Waggon & Saddle Horses`,
    horses_breeding = `Breeding Horses`,
    cattle_oxen = `Oxen`,
    cattle_breeding = `Breeding Cattle`,
    sheep_wethers = `Wethers`,
    sheep_breeding = `Breeding Sheep`,
    sheep_spanish = `Spanish Sheep`,
    donkeys = `Asses`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids Sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leggers of wine made`,
    brandy = `Leggers of Brandy made`
  )

# --------------------------------------------------------------------------
# 1.7 GEORGE 1825
# --------------------------------------------------------------------------

df_ge <- read_excel("data/raw/1825 series.xlsx", sheet = "George 1825", col_names = FALSE)

col_names_ge <- df_ge %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_ge[col_names_ge == ""] <- "Unnamed"
col_names_ge <- trimws(col_names_ge)
col_names_ge <- make.unique(col_names_ge)
colnames(df_ge) <- col_names_ge
df_ge$source_row <- seq_len(nrow(df_ge))
df_ge <- df_ge %>% slice(-1:-2)

district_ge <- df_ge %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_ge[[4]],
    sublocation = df_ge[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women.1`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 20`,
    freeblacks_men = `Free Blacks_Males above 16`,
    freeblacks_women = `Females above 20.1`,
    prize_men = `Prize Negroes_Males above 16`,
    prize_women = `Females above 20.3`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males under 16`,
    khoe_daughters = `Females under 20`,
    freeblacks_sons = `Males under 16.1`,
    freeblacks_daughters = `Females under 20.1`,
    prize_sons = `Males under 16.3`,
    prize_daughters = `Females under 20.3`,
    slaves_sons = `Males under 16.2`,
    slaves_daughters = `Females under 20.2`,
    horses_saddle = `Cattle_Waggon & Saddle Horses`,
    horses_breeding = `Breeding Horses`,
    cattle_oxen = `Oxen`,
    cattle_breeding = `Breeding Cattle`,
    sheep_wethers = `Wethers`,
    sheep_breeding = `Breeding Sheep`,
    sheep_spanish = `Spanish Sheep`,
    donkeys = `Asses`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids Sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leggers of wine made`,
    brandy = `Leggers of Brandy made`
  )

# --------------------------------------------------------------------------
# 1.8 UITENHAGE 1825
# --------------------------------------------------------------------------

df_ui <- read_excel("data/raw/1825 series.xlsx", sheet = "Uitenhage 1825", col_names = FALSE)

col_names_ui <- df_ui %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_ui[col_names_ui == ""] <- "Unnamed"
col_names_ui <- trimws(col_names_ui)
col_names_ui <- make.unique(col_names_ui)
colnames(df_ui) <- col_names_ui
df_ui$source_row <- seq_len(nrow(df_ui))
df_ui <- df_ui %>% slice(-1:-2)

district_ui <- df_ui %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_ui[[4]],
    sublocation = df_ui[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Families_Men`,
    settler_women = `Women.1`,
    khoe_men = `Hottentots_Males above 16`,
    khoe_women = `Females above 20`,
    freeblacks_men = `Free Blacks_Males above 16`,
    freeblacks_women = `Females above 20.1`,
    prize_men = `Prize Negroes_Males above 16`,
    prize_women = `Females above 20.3`,
    slaves_men = `Slaves_Males above 16`,
    slaves_women = `Females above 20.2`,
    settler_sons = `Sons`,
    settler_daughters = `Daughters`,
    khoe_sons = `Males under 16`,
    khoe_daughters = `Females under 20`,
    freeblacks_sons = `Males under 16.1`,
    freeblacks_daughters = `Females under 20.1`,
    prize_sons = `Males under 16.3`,
    prize_daughters = `Females under 20.3`,
    slaves_sons = `Males under 16.2`,
    slaves_daughters = `Females under 20.2`,
    horses_saddle = `Cattle_Waggon & Saddle Horses`,
    horses_breeding = `Breeding Horses`,
    cattle_oxen = `Oxen`,
    cattle_breeding = `Breeding Cattle`,
    sheep_wethers = `Wethers`,
    sheep_breeding = `Breeding Sheep`,
    sheep_spanish = `Spanish Sheep`,
    donkeys = `Asses`,
    goats = `Goats`,
    pigs = `Pigs`,
    wheat_sown = `Muids Sown_Wheat`,
    barley_sown = `Barley`,
    oats_sown = `Oats`,
    rye_sown = `Rye`,
    wheat_reaped = `Muids Reaped_Wheat`,
    barley_reaped = `Barley.1`,
    rye_reaped = `Rye.1`,
    oats_reaped = `Oats.1`,
    wine = `Leggers of wine made`,
    brandy = `Leggers of Brandy made`
  )

# --------------------------------------------------------------------------
# 1.9 CLANWILLIAM 1824
# --------------------------------------------------------------------------

df_cl <- read_excel("data/raw/1825 series.xlsx", sheet = "Clanwilliam 1824", col_names = FALSE)

col_names_cl <- df_cl %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_cl[col_names_cl == ""] <- "Unnamed"
col_names_cl <- trimws(col_names_cl)
col_names_cl <- make.unique(col_names_cl)
colnames(df_cl) <- col_names_cl
df_cl$source_row <- seq_len(nrow(df_cl))
df_cl <- df_cl %>% slice(-1:-2)

# Clanwilliam: col 3 = men names, col 4 = women names, col 5 = veldcornet/wyk
district_cl <- df_cl %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = coalesce(df_cl[[3]], df_cl[[4]]),
    sublocation = df_cl[[5]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Mans`,
    settler_women = `Vrouwen`,
    khoe_men = `Hottentotten_Mans boven de 16 jaren`,
    khoe_women = `Vrouwen boven de 14 jaren`,
    prize_men = `Prysnegers_Mans`,
    prize_women = `Vrouwen.2`,
    slaves_men = `Slaven_Mans boven de 16 jaren`,
    slaves_women = `Vrouwen boven de 14 jaren.1`,
    settler_sons = `Zoons beneden 16 jaren`,
    settler_daughters = `Dogters beneden 20 jaren`,
    khoe_sons = `Mans beneden de 16 jaren`,
    khoe_daughters = `Vrouwen beneden de 14 jaren`,
    freeblacks_sons = `Zoons in de Colonie geboren`,
    freeblacks_daughters = `Dogters in de Colonie geboren`,
    prize_sons = `Zoons in de Colonie aan gebragt`,
    prize_daughters = `Dogters in de Colonie aan gebragt`,
    slaves_sons = `Mans beneden de 16 jaren.1`,
    slaves_daughters = `Vrouwen beneden de 14 jaren.1`,
    horses_saddle = `Wagen & Ryd Paarden`,
    horses_breeding = `Aanfok Paarden`,
    cattle_oxen = `Trek Ossen`,
    cattle_breeding = `Aanteel Beesten`,
    sheep_wethers = `Schapen_Hamels`,
    sheep_breeding = `Aanteel`,
    sheep_spanish = `Wolgevende`,
    goats = `Bokken`,
    pigs = `Varkens`,
    wheat_sown = `Mudden Gezaaid_Tarwe`,
    barley_sown = `Garst`,
    oats_sown = `Haver`,
    rye_sown = `Rogge`,
    wheat_reaped = `Mudden Gewonnen_Tarwe`,
    barley_reaped = `Garst.1`,
    oats_reaped = `Haver.1`,
    rye_reaped = `Rogge.1`,
    wine = `Leggers Gewonnen_Wyn`,
    brandy = `Brandewyn`
  )

# --------------------------------------------------------------------------
# 1.10 CRADOCK 1823
# --------------------------------------------------------------------------

df_cr <- read_excel("data/raw/1825 series.xlsx", sheet = "Cradock 1823", col_names = FALSE)

col_names_cr <- df_cr %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_cr[col_names_cr == ""] <- "Unnamed"
col_names_cr <- trimws(col_names_cr)
col_names_cr <- make.unique(col_names_cr)
colnames(df_cr) <- col_names_cr
df_cr$source_row <- seq_len(nrow(df_cr))
df_cr <- df_cr %>% slice(-1:-2)

# col 4 = men names, col 5 = women names, col 2 = district
district_cr <- df_cr %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = df_cr[[4]],
    sublocation = df_cr[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Families_Mans`,
    settler_women = `Vrouwens`,
    khoe_men = `Hottentotten_Mans boven de 16 jaren`,
    khoe_women = `Vrouwen boven de 14 jaren`,
    prize_men = `Apprentice_Mans`,
    prize_women = `Vrouwens.1`,
    slaves_men = `Slaven_Mans boven de 16 jaren`,
    slaves_women = `Vrouwen boven de 14 jaren.1`,
    settler_sons = `Zoons`,
    settler_daughters = `Dogters`,
    khoe_sons = `Mans beneden de 16 jaren`,
    khoe_daughters = `Vrouwen beneden de 14 jaren`,
    slaves_sons = `Mans beneden de 16 jaren.1`,
    slaves_daughters = `Vrouwen beneden de 14 jaren.1`,
    horses_saddle = `Paarden_Wagen en Ryd`,
    horses_breeding = `Aanteel Paarden`,
    cattle_oxen = `Trek ossen`,
    cattle_breeding = `Aanteel Beesten`,
    sheep_wethers = `Schaapen_Hamels`,
    sheep_breeding = `Aanteel Schaapen`,
    sheep_spanish = `Spaansche`,
    goats = `Bokken`,
    pigs = `Varkens`,
    wheat_sown = `Mudden Gezaaid_Tarwe`,
    barley_sown = `Garst`,
    oats_sown = `Haver`,
    rye_sown = `Rogge`,
    wheat_reaped = `Mudden Gewonnen_Tarwe`,
    barley_reaped = `Garst.1`,
    oats_reaped = `Haver.1`,
    rye_reaped = `Rogge.1`,
    wine = `Leggens Gewonnen_Wyn`,
    brandy = `Brandewyn`
  )

# --------------------------------------------------------------------------
# 1.11 WORCESTER 1824
# --------------------------------------------------------------------------

df_wo <- read_excel("data/raw/1825 series.xlsx", sheet = "Worcester 1824", col_names = FALSE)

col_names_wo <- df_wo %>%
  slice(1:2) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  unite("col_name", sep = "_", na.rm = TRUE) %>%
  pull(col_name)
col_names_wo[col_names_wo == ""] <- "Unnamed"
col_names_wo <- trimws(col_names_wo)
col_names_wo <- make.unique(col_names_wo)
colnames(df_wo) <- col_names_wo
df_wo$source_row <- seq_len(nrow(df_wo))
df_wo <- df_wo %>% slice(-1:-2)

# col 4 = men names, col 5 = women names, col 2 = district
district_wo <- df_wo %>%
  mutate(
    record_nr   = row_number(),
    name_raw    = coalesce(df_wo[[4]], df_wo[[5]]),
    sublocation = df_wo[[2]]
  ) %>%
  select(
    record_nr, source_row, name_raw, sublocation,
    settler_men = `Mans`,
    settler_women = `Vrouwen`,
    khoe_men = `Hottentotten_Mans boven de 16 jaren`,
    khoe_women = `Vrouwen boven de 14 jaren`,
    prize_men = `Prysnegers_Mans`,
    prize_women = `Vrouwen.2`,
    slaves_men = `Slaven_Mans boven de 16 jaren`,
    slaves_women = `Vrouwen boven de 14 jaren.1`,
    settler_sons = `Zoons beneden 16 jaren`,
    settler_daughters = `Dochters`,
    khoe_sons = `Mans beneden de 16 jaren`,
    khoe_daughters = `Vrouwen beneden de 14 jaren`,
    freeblacks_sons = `Zoons in de Colonie geboren`,
    freeblacks_daughters = `Dochters in de Colonie geboren`,
    prize_sons = `Zoons in de Colonie aan gebragt`,
    prize_daughters = `Dochters in de Colonie aan gebragt`,
    slaves_sons = `Mans beneden de 16 jaren.1`,
    slaves_daughters = `Vrouwen beneden de 14 jaren.1`,
    horses_saddle = `Wagen & Ryd Paarden`,
    horses_breeding = `Aanfok Paarden`,
    cattle_oxen = `Trek Ossen`,
    cattle_breeding = `Aanteel Beesten`,
    sheep_wethers = `Schapen_Hamels`,
    sheep_breeding = `Aanfok`,
    sheep_spanish = `Wolgevende`,
    donkeys = `Ezels`,
    goats = `Bokken`,
    pigs = `Varkens`,
    wheat_sown = `Mudden Gezaaid_Tarwe`,
    barley_sown = `Garst`,
    oats_sown = `Haver`,
    rye_sown = `Rogge`,
    wheat_reaped = `Mudden Gewonnen_Tarwe`,
    barley_reaped = `Garst.1`,
    oats_reaped = `Haver.1`,
    rye_reaped = `Rogge.1`,
    wine = `Leggers Gewonnen_Wyn`,
    brandy = `Brandewyn`
  )

# --------------------------------------------------------------------------
# 1.12 COMBINE ALL DISTRICTS
# --------------------------------------------------------------------------

# Some districts lack certain columns; bind_rows handles this with NAs
all_districts <- bind_rows(
  district_al %>% mutate(district = "Albany"),
  district_bf %>% mutate(district = "Beaufort"),
  district_cl %>% mutate(district = "Clanwilliam"),
  district_cr %>% mutate(district = "Cradock"),
  district_ct %>% mutate(district = "Cape"),
  district_ge %>% mutate(district = "George"),
  district_gr %>% mutate(district = "Graaff-Reinet"),
  district_sb %>% mutate(district = "Stellenbosch"),
  district_sw %>% mutate(district = "Swellendam"),
  district_ui %>% mutate(district = "Uitenhage"),
  district_wo %>% mutate(district = "Worcester")
)

# --------------------------------------------------------------------------
# 1.13 CLEAN THE COMBINED DATA
# --------------------------------------------------------------------------

# Convert economic variables to numeric
econ_vars <- c(
  "settler_men", "settler_women",
  "khoe_men", "khoe_women", "freeblacks_men", "freeblacks_women",
  "prize_men", "prize_women", "slaves_men", "slaves_women",
  "settler_sons", "settler_daughters",
  "khoe_sons", "khoe_daughters", "freeblacks_sons", "freeblacks_daughters",
  "prize_sons", "prize_daughters", "slaves_sons", "slaves_daughters",
  "horses_saddle", "horses_breeding",
  "cattle_oxen", "cattle_breeding",
  "sheep_wethers", "sheep_breeding", "sheep_spanish",
  "donkeys", "goats", "pigs",
  "wheat_sown", "barley_sown", "oats_sown", "rye_sown",
  "wheat_reaped", "barley_reaped", "oats_reaped", "rye_reaped",
  "hay", "wine", "brandy"
)


# Pinned, cell-level numeric corrections generated from this exact raw workbook.
# Blanks retain the original zero convention; unadjudicated content remains NA.
stopifnot(unname(tools::md5sum("data/raw/1825 series.xlsx")) ==
          readLines("data/inputs/source_md5.txt", warn = FALSE))
numeric_review <- read.csv("data/inputs/census_numeric_review.csv", fileEncoding = "UTF-8-BOM")
existing_econ <- intersect(econ_vars, names(all_districts))
stopifnot(!anyDuplicated(numeric_review[c("district", "source_row")]),
          !anyDuplicated(numeric_review$census_id))
all_districts <- all_districts %>%
  select(-all_of(existing_econ)) %>%
  inner_join(numeric_review, by = c("district", "source_row"), relationship = "one-to-one")
# no eligible household may be lost silently in the join.
stopifnot(nrow(all_districts) == nrow(numeric_review))
# husband, wife and female-head names come from the explicit parser
# (code/parse_names.R), for every district, keyed on (district, source_row).
census_names <- parse_census_names("data/raw/1825 series.xlsx", numeric_review)
write.csv(census_names, "output/tables/census_parsed_names.csv", row.names = FALSE, fileEncoding = "UTF-8")
all_districts <- all_districts %>%
  select(-any_of(c("name_raw", "wife_name_raw", "name_women"))) %>%
  inner_join(census_names %>% select(district, source_row, head_role, head_name_raw, spouse_name_raw,
                                     spouse_source_row, husband_named_absent, annotation),
             by = c("district", "source_row"), relationship = "one-to-one") %>%
  mutate(name_raw = head_name_raw, wife_name_raw = spouse_name_raw)
stopifnot(nrow(all_districts) == nrow(numeric_review))
source_issues <- read.csv("data/inputs/census_correction_ledger.csv", fileEncoding = "UTF-8-BOM")
source_issues <- source_issues[grepl("^unresolved", source_issues$status), ]
write.csv(source_issues, "output/tables/unresolved_census_cells.csv", row.names = FALSE)

# Create aggregate variables
all_districts <- all_districts %>%
  mutate(
    horses       = horses_saddle + horses_breeding,
    cattle       = cattle_oxen + cattle_breeding,
    sheep        = sheep_wethers + sheep_breeding + sheep_spanish,
    total_slaves = slaves_men + slaves_women + slaves_sons + slaves_daughters,
    total_khoe   = khoe_men + khoe_women + khoe_sons + khoe_daughters,
    total_grain_sown   = wheat_sown + barley_sown + oats_sown + rye_sown,
    total_grain_reaped = wheat_reaped + barley_reaped + oats_reaped + rye_reaped,
    # Unique row ID across all districts
    census_id    = census_id  # stable source crosswalk, including newly eligible records
  )

# Fill forward sublocation within each district (it's often only listed for
# the first household in a fieldcornetcy)
all_districts <- all_districts %>%
  group_by(district) %>%
  fill(sublocation, .direction = "down") %>%
  ungroup()

cat("Census data loaded:", nrow(all_districts), "household records across",
    n_distinct(all_districts$district), "districts\n")
cat("Records per district:\n")
print(table(all_districts$district))

# Clean up intermediate objects
rm(df, df_stellenbosch, df_gr, df_sw, df_sw_clean, df_al, df_bf, df_ge,
   df_ui, df_cl, df_cr, df_wo,
   district_al, district_bf, district_cl, district_cr, district_ct,
   district_ge, district_gr, district_sb, district_sw, district_ui, district_wo)


# --- Part 2: Load and clean Voortrekker data (Paper Section 3) ---

# ============================================================================
# PART 2: LOAD AND CLEAN VOORTREKKER DATA
# ============================================================================

vt_raw <- read_excel("data/raw/Voortrekkers 2.xlsx", sheet = "Main")

# Debug: print column names to identify any issues
cat("\nVoortrekker file column names (first 40):\n")
print(head(names(vt_raw), 40))

# Helper function to find column by pattern (case-insensitive)
find_col <- function(df, pattern) {
  matches <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) return(matches[1])
  return(NULL)
}

# Find actual column names using patterns that handle R's ...N suffix for duplicates
# R names duplicate columns like: SURNAME, SURNAME...3, SURNAME...24, etc.
col_surname_orig <- find_col(vt_raw, "SURNAME.*oorspronklik")
col_surname <- find_col(vt_raw, "^SURNAME(\\.\\.\\.3)?$")  # SURNAME or SURNAME...3
col_name <- find_col(vt_raw, "^NAME$")
col_id <- find_col(vt_raw, "^ID$")
col_birth_year <- find_col(vt_raw, "^Birth year$")
col_birthyear2 <- find_col(vt_raw, "^Birthyear$")
col_birth_or_bapt <- find_col(vt_raw, "^Birth OR baptise year$")
col_death_year <- find_col(vt_raw, "^death year$")
col_marry_year <- find_col(vt_raw, "^Marry year$")
col_distrik <- find_col(vt_raw, "^DISTRIK$")
col_wyk <- find_col(vt_raw, "^WYK$")
col_move_year <- find_col(vt_raw, "^move year$")
col_move_with <- find_col(vt_raw, "^MOVE WITH$|^MOVE\\.WITH$|^move_with$|^Move With$")
col_move_to <- find_col(vt_raw, "^MOVE TO$|^MOVE\\.TO$|^move_to$|^Move To$")  # Destination

# Wife columns
col_wife_name <- find_col(vt_raw, "^M\\. TO$")  # Wife's first name
col_wife_surname <- find_col(vt_raw, "^SURNAME\\.\\.\\.24$")  # Wife's maiden surname

# Birth/baptism place columns.
# The trekker's own fields are "BIRTH PLACE...9" (readxl dedup suffix) and
# "BAPTISED PLACE"; later BIRTH PLACE/BAPTISE PLACE columns belong to the
# wife's block, so anchor the patterns to the first occurrence.
col_birth_place <- find_col(vt_raw, "^BIRTH PLACE(\\.\\.\\.9)?$")
col_bapt_place  <- find_col(vt_raw, "^BAPTISED PLACE$")

cat("\nColumn mapping found:\n")
cat("  SURNAME (oorspronklik):", ifelse(is.null(col_surname_orig), "NOT FOUND", col_surname_orig), "\n")
cat("  SURNAME:", ifelse(is.null(col_surname), "NOT FOUND", col_surname), "\n")
cat("  NAME:", ifelse(is.null(col_name), "NOT FOUND", col_name), "\n")
cat("  DISTRIK:", ifelse(is.null(col_distrik), "NOT FOUND", col_distrik), "\n")
cat("  Birth year:", ifelse(is.null(col_birth_year), "NOT FOUND", col_birth_year), "\n")

# Build selection list dynamically
select_list <- list()
if (!is.null(col_surname_orig)) select_list$vt_surname_orig <- col_surname_orig
if (!is.null(col_surname)) select_list$vt_surname <- col_surname
if (!is.null(col_name)) select_list$vt_name <- col_name
if (!is.null(col_id)) select_list$vt_id <- col_id
if (!is.null(col_birth_year)) select_list$birth_year <- col_birth_year
if (!is.null(col_birthyear2)) select_list$birthyear2 <- col_birthyear2
if (!is.null(col_birth_or_bapt)) select_list$birth_or_bapt <- col_birth_or_bapt
if (!is.null(col_death_year)) select_list$death_year <- col_death_year
if (!is.null(col_marry_year)) select_list$marry_year <- col_marry_year
if (!is.null(col_distrik)) select_list$distrik <- col_distrik
if (!is.null(col_wyk)) select_list$wyk <- col_wyk
if (!is.null(col_move_year)) select_list$move_year <- col_move_year
if (!is.null(col_move_with)) select_list$move_with <- col_move_with
if (!is.null(col_move_to)) select_list$move_to <- col_move_to
if (!is.null(col_wife_name)) select_list$wife_name <- col_wife_name
if (!is.null(col_wife_surname)) select_list$wife_surname <- col_wife_surname
if (!is.null(col_birth_place)) select_list$birth_place <- col_birth_place
if (!is.null(col_bapt_place)) select_list$bapt_place <- col_bapt_place

cat("\nSelecting", length(select_list), "columns\n")
cat("  Wife name column:", ifelse(is.null(col_wife_name), "NOT FOUND", col_wife_name), "\n")
cat("  Wife surname column:", ifelse(is.null(col_wife_surname), "NOT FOUND", col_wife_surname), "\n")
cat("  MOVE WITH column:", ifelse(is.null(col_move_with), "NOT FOUND", col_move_with), "\n")
cat("  MOVE TO column:", ifelse(is.null(col_move_to), "NOT FOUND", col_move_to), "\n")

# Select and rename columns using the dynamic list
vt <- vt_raw %>%
  select(!!!select_list) %>%
  mutate(vt_source_row = row_number() + 1L,
         vt_source_key = paste0("Voortrekkers 2.xlsx|Main|", vt_source_row))

# Use best available birth year
vt <- vt %>%
  mutate(
    birth_yr = coalesce(birth_year, birthyear2, birth_or_bapt),
    birth_yr = as.numeric(birth_yr),
    move_year = as.numeric(move_year)
  )

cat("\nVoortrekker data loaded:", nrow(vt), "records\n")
cat("Birth year available:", sum(!is.na(vt$birth_yr)), "of", nrow(vt), "\n")

# Filter to adults: born before 1810 OR no birth year available
# (Those born 1810+ would have been under 16 in 1825, unlikely household heads)
vt_adults <- vt %>%
  filter(is.na(birth_yr) | birth_yr < 1810)

cat("After adult filter (born <1810 or no birth year):", nrow(vt_adults), "records\n")

# Further filter: must have a surname and name
vt_adults <- vt_adults %>%
  filter(!is.na(vt_surname) & !is.na(vt_name))

cat("After requiring surname + name:", nrow(vt_adults), "records\n")

# --------------------------------------------------------------------------
# 2.1 MAP VOORTREKKER DISTRICTS TO CENSUS DISTRICTS
# --------------------------------------------------------------------------

# Clean the district field: extract the primary district name
vt_adults <- vt_adults %>%
  mutate(
    distrik_clean = toupper(trimws(distrik)),
    # Map to census district(s)
    census_districts = case_when(
      # Direct matches
      str_detect(distrik_clean, "^UITENHAGE")       ~ "Uitenhage",
      str_detect(distrik_clean, "^BEAUFORT")         ~ "Beaufort",
      str_detect(distrik_clean, "^GRAAFF-REINET|^GRAAF-REINET") ~ "Graaff-Reinet",
      str_detect(distrik_clean, "^SWELLENDAM|^SWELLRNDAM") ~ "Swellendam",
      str_detect(distrik_clean, "^ALBANY")           ~ "Albany",
      str_detect(distrik_clean, "^CRADOCK")          ~ "Cradock",
      str_detect(distrik_clean, "^GEORGE")           ~ "George",
      str_detect(distrik_clean, "^WORCESTER")        ~ "Worcester",
      str_detect(distrik_clean, "^CLANWILLIAM")      ~ "Clanwilliam",
      str_detect(distrik_clean, "^STELLENBOSCH")     ~ "Stellenbosch",
      str_detect(distrik_clean, "^CAPE")             ~ "Cape",
      str_detect(distrik_clean, "^TULBAGH")          ~ "Worcester",
      str_detect(distrik_clean, "^MALMESBURY|^FRANSCHHOEK") ~ "Stellenbosch",
      str_detect(distrik_clean, "^CALEDON")          ~ "Swellendam",
      str_detect(distrik_clean, "^GRAHAMSTOWN")      ~ "Albany",
      # Somerset: carved from Graaff-Reinet / Uitenhage / Albany / Cradock
      str_detect(distrik_clean, "^SOMERSET")         ~ "Somerset_multi",
      # Colesberg: carved from Graaff-Reinet
      str_detect(distrik_clean, "COLESBERG|COLEBERG") ~ "Colesberg_multi",
      # Catch ambiguous cases
      TRUE ~ "Unknown"
    )
  )

cat("\nVoortrekker district mapping:\n")
print(table(vt_adults$census_districts))

# Add row_id to vt_adults (used throughout for joining back to matches)
vt_adults <- vt_adults %>%
  mutate(row_id = row_number())

# --------------------------------------------------------------------------
# 2.2 STANDARDIZE TREK LEADER NAMES (Fuzzy String Matching)
# --------------------------------------------------------------------------
# We apply the same Jaro-Winkler distance methodology used for our main
# record linkage to standardize trek leader names from the raw MOVE WITH
# column. This ensures methodological consistency: the same string distance
# tools that link individuals across datasets also resolve spelling
# variation and ambiguity in leadership attribution.
#
# The approach has three stages:
#   Stage 1: Exact surname pattern matching (handles unambiguous cases)
#   Stage 2: JW-distance disambiguation (for shared surnames: Maritz,
#            Erasmus, Potgieter co-leadership)
#   Stage 3: Fallback JW fuzzy matching (catches spelling variants like
#            Tregardt/Trigardt, Espag/Esbach)

if ("move_with" %in% names(vt_adults)) {

  cat("\n--- LEADER NAME STANDARDIZATION ---\n")
  cat("Using Jaro-Winkler fuzzy matching (same method as record linkage)\n\n")

  # Reference table: all 26 known trek leaders
  # Source: Voortrekker genealogical database leader counts
  leader_ref <- tribble(
    ~leader_id,      ~surname_pattern,                        ~firstname_pattern,             ~label,          ~expected_n,
    "potgieter_ah",  "POTGIETER",                             "ANDRIES|HENDRIK|SAREL|CILLIERS", "Potgieter",   158,
    "retief",        "RETIEF",                                "PIET",                         "Retief",        139,
    "du_plessis",    "DU PLESSIS|DUPLESSIS|DU\\.PLESSIS",     "JAN",                          "Du Plessis",    116,
    "jacobs",        "JACOBS",                                "PIETER|DANIEL",                "Jacobs",        85,
    "uys",           "UYS",                                   "PETRUS|LAFRAS",                "Uys",           79,
    "maritz_js",     "MARITZ",                                "JOHANNES|STEPHANUS",           "Maritz (JS)",   68,
    "maritz_gm",     "MARITZ",                                "GERHARDUS|MARTHINUS|GERT",     "Maritz (GM)",   57,
    "landman",       "LANDMAN",                               "KAREL|PIETER",                 "Landman",       54,
    "de_klerk",      "DE KLERK|DEKLERK|DE\\.KLERK",           "JACOB",                        "De Klerk",      52,
    "opperman",      "OPPERMAN",                              "PHILIPPUS|ALBERTUS",            "Opperman",      41,
    "pretorius",     "PRETORIUS",                             "ANDRIES|WILHELMUS|JACOBUS",     "Pretorius",    38,
    "van_rooyen",    "VAN ROOYEN|VANROOYEN|VAN\\.ROOYEN",     "GERRIT|REYNIER",               "Van Rooyen",    29,
    "rudolph",       "RUDOLPH",                               "GERHARDUS|JACOBUS",            "Rudolph",       26,
    "nel",           "NEL",                                   "LOUIS|JACOBUS",                "Nel",           26,
    "meyer",         "MEYER",                                 "LUCAS|JOHANNES",               "Meyer",         24,
    "espag",         "ESPAG|ESBACH|ESPACH",                   "JOACHIM|CHRISTOFFEL",          "Espag",         23,
    "de_lange",      "DE LANGE|DELANGE|DE\\.LANGE",           "JOHAN|HENDRIK",                "De Lange",      15,
    "malan",         "MALAN",                                 "HERCULES|PHILIP",              "Malan",         13,
    "tregardt",      "TREGARDT|TRIGARDT|TRICHARDT",            "LOUIS",                        "Tregardt",      13,
    "erasmus_sp",    "ERASMUS",                               "STEPHANUS|PETRUS",             "Erasmus (SP)",  9,
    "van_rensburg",  "VAN RENSBURG|VANRENSBURG|VAN\\.RENSBURG","JOHANNES|JACOBUS|HANS|LANG",  "Van Rensburg",  7,
    "visagie",       "VISAGIE|VISAGE",                        "ARIE|ZACHARIAS",               "Visagie",       6,
    "fourie_ds",     "FOURIE",                                "DAVID|STEPHANUS",              "Fourie (DS)",   6,
    "de_beer",       "DE BEER|DEBEER|DE\\.BEER",              "JAN|MATTHYS",                  "De Beer",       6,
    "lombard",       "LOMBARD|LOMBAARD",                      "HERMANUS|STEPHANUS",           "Lombard",       2,
    "erasmus_jj",    "ERASMUS",                               "JOHANNES|JACOBUS",             "Erasmus (JJ)",  1
  )

  # Surnames that appear for multiple leaders (require first-name disambiguation)
  ambiguous_surnames <- c("MARITZ", "ERASMUS", "POTGIETER", "FOURIE")

  # Stage 1+2: Combined pattern matching with disambiguation
  clean_leader <- function(raw_name) {
    if (is.na(raw_name) || trimws(raw_name) == "") return(NA_character_)

    name_upper <- toupper(trimws(as.character(raw_name)))

    # Strip leading/trailing question marks and uncertainty markers
    name_clean <- gsub("^\\(?\\?\\)?\\s*", "", name_upper)
    name_clean <- gsub("\\s*\\(?\\?\\)?$", "", name_clean)
    if (name_clean == "" || name_clean == "?") return(NA_character_)

    # Try each leader's surname pattern FIRST (before FAMILY check),
    # so "AS A FAMILY WITH P.L. UYS" -> "Uys" not "Family/Independent"
    matched_leaders <- leader_ref %>%
      filter(sapply(surname_pattern, function(pat) grepl(pat, name_clean)))

    if (nrow(matched_leaders) == 0) {
      # No leader surname found. Check for family/self-organized moves.
      if (grepl("FAMILY|FAMILIE|FAMILTY|FAMLIY|SELF|ALONE|ONAFHANKLIK", name_clean)) {
        return("Family/Independent")
      }

      # Stage 3: Fallback — extract surname-like tokens and use JW distance
      tokens <- unlist(strsplit(gsub("[,;()]", " ", name_clean), "\\s+"))
      tokens <- tokens[nchar(tokens) >= 3]  # Skip short words

      if (length(tokens) == 0) return("Other")

      # Compute JW distance from each token to all reference surnames
      # (Use the unique, unambiguous base surnames for fuzzy matching)
      unique_surnames <- unique(gsub("\\|.*", "", leader_ref$surname_pattern))
      unique_surnames <- gsub("\\\\.", ".", unique_surnames)

      best_dist <- Inf
      best_label <- "Other"

      for (tok in tokens) {
        for (j in seq_along(unique_surnames)) {
          ref_surname <- unique_surnames[j]
          # Use JW distance (same as main record linkage)
          jw_dist <- stringdist(tok, ref_surname, method = "jw")
          if (jw_dist < best_dist && jw_dist <= 0.15) {  # Strict threshold
            best_dist <- jw_dist
            # Find the label for this surname
            match_row <- leader_ref %>%
              filter(grepl(gsub("\\.", "\\\\.", ref_surname), surname_pattern)) %>%
              slice(1)
            if (nrow(match_row) > 0) {
              best_label <- match_row$label
            }
          }
        }
      }
      return(best_label)
    }

    if (nrow(matched_leaders) == 1) {
      return(matched_leaders$label[1])
    }

    # Multiple matches (ambiguous surname) — disambiguate using first name
    # Extract first name tokens (everything after the surname portion)
    name_tokens <- unlist(strsplit(gsub("[,;()]", " ", name_clean), "\\s+"))

    best_score <- -1
    best_label <- matched_leaders$label[1]  # Default to first match

    for (i in seq_len(nrow(matched_leaders))) {
      fn_patterns <- unlist(strsplit(matched_leaders$firstname_pattern[i], "\\|"))
      score <- 0

      for (pat in fn_patterns) {
        for (tok in name_tokens) {
          # Exact substring check
          if (grepl(pat, tok)) {
            score <- score + 2
          } else {
            # JW distance check for fuzzy first-name matching
            jw_dist <- stringdist(tok, pat, method = "jw")
            if (jw_dist <= 0.12) {
              score <- score + 1
            }
          }
        }
      }

      if (score > best_score) {
        best_score <- score
        best_label <- matched_leaders$label[i]
      }
    }

    return(best_label)
  }

  # Apply the cleaning function to all Voortrekker records
  cat("Cleaning", sum(!is.na(vt_adults$move_with)), "non-NA leader entries...\n")
  vt_adults$leader_std <- sapply(vt_adults$move_with, clean_leader, USE.NAMES = FALSE)

  # Create grouped leader variable for analysis (combine small groups)
  vt_adults <- vt_adults %>%
    mutate(
      # Combine the two Maritz into one for some analyses
      leader_combined = case_when(
        leader_std %in% c("Maritz (JS)", "Maritz (GM)") ~ "Maritz",
        leader_std %in% c("Erasmus (SP)", "Erasmus (JJ)") ~ "Erasmus",
        leader_std == "Fourie (DS)" ~ "Fourie",
        TRUE ~ leader_std
      ),
      # Group for regression: major leaders (n >= 15) vs minor leaders
      leader_group = case_when(
        is.na(leader_std) ~ NA_character_,
        leader_combined %in% c("Potgieter", "Retief", "Du Plessis", "Jacobs",
                               "Uys", "Maritz", "Landman", "De Klerk",
                               "Opperman", "Pretorius", "Van Rooyen",
                               "Rudolph", "Nel", "Meyer", "Espag",
                               "De Lange") ~ leader_combined,
        leader_std == "Family/Independent" ~ "Family/Independent",
        TRUE ~ "Minor Leader"
      )
    )

  # --- Diagnostic Report ---
  cat("\n--- Leader Standardization Results ---\n")
  leader_diag <- vt_adults %>%
    filter(!is.na(leader_std)) %>%
    count(leader_std) %>%
    arrange(desc(n))

  cat("\nIndividual leaders identified:\n")
  print(leader_diag, n = 30)

  cat("\nGrouped leaders (for analysis):\n")
  vt_adults %>%
    filter(!is.na(leader_group)) %>%
    count(leader_group) %>%
    arrange(desc(n)) %>%
    print(n = 20)

  # Compare observed vs expected counts
  leader_validation <- leader_diag %>%
    left_join(leader_ref %>% select(label, expected_n), by = c("leader_std" = "label")) %>%
    mutate(
      diff = n - expected_n,
      pct_captured = round(100 * n / expected_n, 1)
    ) %>%
    filter(!is.na(expected_n))

  cat("\nValidation against expected counts:\n")
  print(leader_validation, n = 30)

  total_assigned <- sum(!is.na(vt_adults$leader_std) & vt_adults$leader_std != "Other")
  total_with_data <- sum(!is.na(vt_adults$move_with) & trimws(vt_adults$move_with) != "")
  cat("\nLeader assignment rate:", total_assigned, "of", total_with_data,
      "(", round(100 * total_assigned / max(total_with_data, 1), 1), "%)\n")
  cat("Unassigned ('Other'):", sum(vt_adults$leader_std == "Other", na.rm = TRUE), "\n")
  cat("NA (no move_with data):", sum(is.na(vt_adults$leader_std)), "\n\n")

} else {
  cat("\nWARNING: move_with column not found in vt_adults. Leader analysis will be skipped.\n")
}

# For Somerset and Colesberg, we'll search across multiple census districts
# Create expanded rows for multi-district searches
vt_expanded <- vt_adults %>%
  {
    # Somerset: search in Graaff-Reinet, Uitenhage, Albany, Cradock
    somerset <- filter(., census_districts == "Somerset_multi") %>%
      crossing(search_district = c("Graaff-Reinet", "Uitenhage", "Albany", "Cradock"))

    # Colesberg: search in Graaff-Reinet, Cradock, Beaufort
    colesberg <- filter(., census_districts == "Colesberg_multi") %>%
      crossing(search_district = c("Graaff-Reinet", "Cradock", "Beaufort"))

    # Direct matches
    direct <- filter(., !census_districts %in% c("Somerset_multi", "Colesberg_multi", "Unknown")) %>%
      mutate(search_district = census_districts)

    bind_rows(direct, somerset, colesberg)
  }

cat("\nExpanded search records (with multi-district):", nrow(vt_expanded), "\n")

# --- Part 3: Name standardisation (Paper Section 3) ---

# ============================================================================
# PART 3: NAME STANDARDIZATION
# ============================================================================

# --------------------------------------------------------------------------
# 3.1 PARSE CENSUS NAMES
# --------------------------------------------------------------------------

# Census names are in format "Surname, First Name(s)" e.g. "Du Plessis, Stephanus Christiaan"
# Some have additional info in parentheses like "(?)" or "snr." or "Sr."

# heads use the same audited standardiser as wives (std_census_name in
# parse_names.R), e.g. "Munro. Andrew" -> MUNRO / ANDREW. A missing first name
# is kept as "" for heads, as before.
head_std <- std_census_name(all_districts$name_raw)
all_districts <- all_districts %>%
  mutate(
    census_surname     = head_std$surname,
    census_first       = coalesce(head_std$first, ""),
    census_surname_std = head_std$surname_std,
    census_first_std   = coalesce(head_std$first_std, ""),
    census_first_only  = head_std$first_only
  )

# --------------------------------------------------------------------------
# 3.1b PARSE CENSUS WIFE NAMES (where available)
# --------------------------------------------------------------------------

# wives are parsed for every district (parse_names.R) and
# standardised with the same rules as husbands (std_census_name: parentheses,
# "?", Sr/Jr, "v. d." -> VAN DER). Format is "Surname, First Name".
wife_std <- std_census_name(all_districts$wife_name_raw)
all_districts <- all_districts %>%
  mutate(
    census_wife_surname     = wife_std$surname,
    census_wife_first       = wife_std$first,
    census_wife_surname_std = wife_std$surname_std,
    census_wife_first_std   = wife_std$first_std,
    census_wife_first_only  = wife_std$first_only
  )

cat("\nWife name data available in census:\n")
cat("  Records with wife name:", sum(!is.na(all_districts$census_wife_surname_std)), "\n")
cat("  Districts with wife data:", paste(unique(all_districts$district[!is.na(all_districts$census_wife_surname_std)]), collapse = ", "), "\n")

# --------------------------------------------------------------------------
# 3.2 STANDARDIZE VOORTREKKER NAMES
# --------------------------------------------------------------------------

vt_expanded <- vt_expanded %>%
  mutate(
    # Clean surname
    vt_surname_std = toupper(trimws(vt_surname)),
    vt_surname_std = str_squish(vt_surname_std),
    # same particle standardisation as census surnames ("v. d." -> VAN DER)
    vt_surname_std = str_replace(vt_surname_std, "^V\\.?\\s*D\\.?\\s+", "VAN DER "),
    vt_surname_std = str_replace(vt_surname_std, "^V\\.?\\s+D\\.?\\s*", "VAN D"),
    vt_surname_std = str_replace(vt_surname_std, "^V\\.?\\s+", "VAN "),

    # Clean first name
    vt_name_clean  = toupper(trimws(vt_name)),
    vt_name_clean  = str_replace_all(vt_name_clean, "\\(.*?\\)", ""),
    vt_name_clean  = str_squish(vt_name_clean),

    # Extract first name only
    vt_first_only  = str_extract(vt_name_clean, "^\\S+")
  )

# ---- WIFE NAME PROCESSING ----
# Add wife columns if they exist in the data
vt_expanded <- vt_expanded %>% mutate(
  wife_after_census = !is.na(suppressWarnings(as.numeric(marry_year))) &
    suppressWarnings(as.numeric(marry_year)) > case_when(
      search_district == "Cradock" ~ 1823,
      search_district %in% c("Clanwilliam", "Worcester") ~ 1824,
      TRUE ~ 1825),
  wife_name = ifelse(wife_after_census, NA_character_, wife_name),
  wife_surname = ifelse(wife_after_census, NA_character_, wife_surname))
if ("wife_name" %in% names(vt_expanded)) {
  vt_expanded <- vt_expanded %>%
    mutate(
      vt_wife_first = toupper(trimws(as.character(wife_name))),
      vt_wife_first = ifelse(is.na(vt_wife_first) | vt_wife_first == "NA" | vt_wife_first == "",
                             NA_character_, str_squish(vt_wife_first)),
      vt_wife_first_only = str_extract(vt_wife_first, "^\\S+")
    )
} else {
  vt_expanded$vt_wife_first <- NA_character_
  vt_expanded$vt_wife_first_only <- NA_character_
}

if ("wife_surname" %in% names(vt_expanded)) {
  vt_expanded <- vt_expanded %>%
    mutate(
      vt_wife_surname = toupper(trimws(as.character(wife_surname))),
      vt_wife_surname = ifelse(is.na(vt_wife_surname) | vt_wife_surname == "NA" | vt_wife_surname == "",
                               NA_character_, str_squish(vt_wife_surname))
    )
} else {
  vt_expanded$vt_wife_surname <- NA_character_
}

vt_expanded <- vt_expanded %>%
  mutate(has_wife_info = !is.na(vt_wife_first) | !is.na(vt_wife_surname)
  )

cat("\nWife name data available in Voortrekker records:\n")
cat("  Records with wife first name:", sum(!is.na(vt_expanded$vt_wife_first)), "\n")
cat("  Records with wife surname:", sum(!is.na(vt_expanded$vt_wife_surname)), "\n")




# ==============================================================================
# SECTION B: RECORD LINKAGE AND MAIN ANALYSIS
# Paper Sections 4-6
# ==============================================================================

# --- Paper Section 4: Who Were the Voortrekkers? ---
# Part 4: Record linkage (Random Forest)
# Part 4.6a: Manual review integration
# Part 5: Match quality assessment
# Part 6: Create analysis dataset
# Parts 7-12: Six comparison methods (Tables 1-4 in paper)
#
# --- Paper Section 5: Testing the Emancipation Hypothesis ---
# Part 16: Slave emancipation analysis (Table 5, Figures 6-10)
#
# --- Paper Section 6: Migration Timing ---
# Part 15C: Timing analysis (Table 6, Figure 11)
#
# --- Paper Section 7/Appendix: Heterogeneity ---
# Part 12D: Leaders and destinations (Figures 7-9)
# Part 15B: Leader/destination analysis


# ============================================================================
# PART 4: RECORD LINKAGE (RANDOM FOREST CLASSIFIER)
# ============================================================================
#
# This implements the machine learning approach from Fourie & Green (2018):
# - Blocking on surname + district (JW distance < 0.15)
# - Rich feature engineering including wife name matching
# - Random Forest classifier trained on expert-labeled training data
# - Wife presence is crucial: "absence of wife makes it far harder to identify a link"
#
# Key insight: ML can learn optimal weighting of wife vs husband info, rather
# than manually specifying bonus weights that may not capture the true value.
# ============================================================================

cat("\n========== RECORD LINKAGE (RANDOM FOREST) ==========\n")

# Load Random Forest library
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Package 'randomForest' is required but not installed.", call. = FALSE)
}
library(randomForest)

# --------------------------------------------------------------------------
# 4.1 STEP 1: BLOCKING - CREATE CANDIDATE PAIRS
# --------------------------------------------------------------------------
# Following the paper: use JW distance on male surnames for blocking
# Select candidates whose length-normalized male surname string distance < 0.15

cat("Creating candidate pairs via blocking on surname + district...\n")

# candidates are male-headed census households only
# (female heads and unresolved heads cannot be the trekker himself), blocked
# within the district search set on exact OR fuzzy surname. Fuzzy: same
# Soundex code or Jaro-Winkler >= 0.92 on the surname with particles and
# spaces removed (Double Metaphone was planned but the phonics package is not
# installed; see locks/deviations.md). Each pair records block_type and the
# surname similarity, which enters the classifier as jw_surname.
surname_key <- function(x) gsub("[^A-Z]", "", toupper(x))          # exact key: full surname
surname_core <- function(x) {                                          # fuzzy key: particles removed
  x <- toupper(gsub("[^A-Za-z ]", " ", x))
  x <- gsub("\\b(VAN|DER|DEN|DE|DU|LE|LA|VON|V|D)\\b", " ", x)
  k <- gsub("[^A-Z]", "", x)
  ifelse(k == "", gsub("[^A-Z]", "", toupper(x)), k)
}
quarantined_ids <- read.csv("data/inputs/quarantined_households.csv")$census_id
census_cand <- all_districts %>%
  filter(head_role == "male", !is.na(census_surname_std), !census_id %in% quarantined_ids) %>%
  select(census_id, census_surname_std, census_first_std, census_first_only, district,
         name_raw, record_nr, census_wife_surname_std, census_wife_first_std, census_wife_first_only)
vt_keys <- vt_expanded %>% filter(!is.na(vt_surname_std)) %>% distinct(search_district, vt_surname_std)
c_keys <- census_cand %>% distinct(district, census_surname_std)
surname_pairs <- bind_rows(lapply(unique(vt_keys$search_district), function(d) {
  a <- vt_keys$vt_surname_std[vt_keys$search_district == d]
  b <- c_keys$census_surname_std[c_keys$district == d]
  if (!length(a) || !length(b)) return(NULL)
  g <- expand.grid(vt_surname_std = a, census_surname_std = b, stringsAsFactors = FALSE)
  ka <- surname_key(g$vt_surname_std); kb <- surname_key(g$census_surname_std)
  ca <- surname_core(g$vt_surname_std); cb <- surname_core(g$census_surname_std)
  g$jw_surname <- 1 - stringdist(ca, cb, method = "jw", p = 0.1)
  g$soundex_match <- stringdist::phonetic(ca) == stringdist::phonetic(cb)
  g$block_type <- ifelse(ka == kb, "exact", "fuzzy")
  g <- g[g$block_type == "exact" | g$jw_surname >= 0.92 | g$soundex_match, ]
  g$jw_surname[g$block_type == "exact"] <- 1
  if (nrow(g)) g$search_district <- d
  g
}))
candidates <- vt_expanded %>%
  inner_join(surname_pairs, by = c("search_district", "vt_surname_std"), relationship = "many-to-many") %>%
  inner_join(census_cand, by = c("search_district" = "district", "census_surname_std"),
             relationship = "many-to-many")

cat("Candidate pairs (surname + district):", nrow(candidates),
    " exact:", sum(candidates$block_type == "exact"), " fuzzy:", sum(candidates$block_type == "fuzzy"), "\n")
cat("Unique Voortrekkers with candidates:", n_distinct(candidates$row_id),
    " (exact only:", n_distinct(candidates$row_id[candidates$block_type == "exact"]), ")\n")

# --------------------------------------------------------------------------
# 4.2 STEP 2: FEATURE ENGINEERING FOR ML MATCHING
# --------------------------------------------------------------------------
# Following the paper, we include:
# - String distances between first names and surnames of husbands and wives
# - Wife-presence variables (crucial)
# - String distances between wife's maiden name and husband's surname
# - Surname frequency (common surnames less predictive)
# - Initials comparison

cat("Computing features for Random Forest...\n")

# Calculate surname frequencies (common surnames are less predictive)
surname_freq <- all_districts %>%
  count(census_surname_std, name = "surname_count") %>%
  mutate(surname_freq = surname_count / nrow(all_districts))

# Name-pair frequency: how many census records share this (surname, first_name)?
# "Dawid de Villiers" is very common → hard to match without wife info
# "Zacharias Boonzaaier" is rare → any close match is likely correct
name_pair_freq <- all_districts %>%
  count(census_surname_std, census_first_only, name = "name_pair_count")

# Helper function to extract initials
get_initials <- function(name) {
  if (is.na(name) || name == "") return("")
  words <- strsplit(toupper(name), "\\s+")[[1]]
  paste(substr(words, 1, 1), collapse = "")
}

# feature construction is a function, so the cross-district pass of
# the linkage builds exactly the same features.
build_pair_features <- function(candidates) {
candidates <- candidates %>%
  left_join(surname_freq, by = "census_surname_std") %>%
  left_join(name_pair_freq, by = c("census_surname_std", "census_first_only")) %>%
  mutate(
    # ---------- MALE NAME FEATURES ----------
    # Jaro-Winkler similarities (converted from distance)
    jw_male_full = 1 - stringdist(vt_name_clean, census_first_std, method = "jw"),
    jw_male_first = 1 - stringdist(vt_first_only, census_first_only, method = "jw"),

    # Levenshtein (normalized)
    lv_male_full = 1 - stringdist(vt_name_clean, census_first_std, method = "lv") /
                     pmax(nchar(vt_name_clean), nchar(census_first_std), 1),

    # Initials comparison (important per the paper)
    vt_initials = sapply(vt_name_clean, get_initials),
    census_initials = sapply(census_first_std, get_initials),
    jw_initials = 1 - stringdist(vt_initials, census_initials, method = "jw"),
    initials_exact = as.integer(vt_initials == census_initials & vt_initials != ""),

    # Multi-name indicators
    has_multi_vt = str_detect(vt_name_clean, "\\s"),
    has_multi_census = str_detect(census_first_std, "\\s"),
    both_multi_name = as.integer(has_multi_vt & has_multi_census),

    # ---------- WIFE PRESENCE FEATURES ----------
    # "The wife-presence variables are important to include because the absence
    # of the wife makes it far harder to identify a link."
    vt_has_wife = as.integer(!is.na(vt_wife_first) | !is.na(vt_wife_surname)),
    census_has_wife = as.integer(!is.na(census_wife_first_std) | !is.na(census_wife_surname_std)),
    both_have_wife = as.integer(vt_has_wife == 1 & census_has_wife == 1),
    neither_has_wife = as.integer(vt_has_wife == 0 & census_has_wife == 0),
    wife_mismatch = as.integer((vt_has_wife == 1 & census_has_wife == 0) |
                               (vt_has_wife == 0 & census_has_wife == 1)),

    # ---------- WIFE NAME FEATURES ----------
    can_match_wife_surname = !is.na(vt_wife_surname) & !is.na(census_wife_surname_std),
    can_match_wife_first = !is.na(vt_wife_first_only) & !is.na(census_wife_first_only),
    can_match_wife = can_match_wife_surname | can_match_wife_first,

    # Wife surname (maiden name) similarity
    jw_wife_surname = ifelse(can_match_wife_surname,
                              1 - stringdist(vt_wife_surname, census_wife_surname_std, method = "jw"),
                              NA_real_),

    # Wife first name similarity (computed whenever both first names are available,
    # regardless of whether wife surnames exist)
    jw_wife_first = ifelse(can_match_wife_first,
                            1 - stringdist(vt_wife_first_only, census_wife_first_only, method = "jw"),
                            NA_real_),

    # Wife's surname vs husband's surname (captures recording variations)
    # "String distances between the name of the husband and wife are meant to
    # capture changes in recording the wife's maiden name or her husband's surname"
    jw_wife_vs_husband_surname = ifelse(!is.na(census_wife_surname_std),
                                         1 - stringdist(vt_surname_std, census_wife_surname_std, method = "jw"),
                                         NA_real_),

    # ---------- SURNAME FREQUENCY FEATURE ----------
    # "Frequency of each surname was also included as a predictor variable,
    # as common surnames are likely to be less predictive of linkages"
    surname_freq = ifelse(is.na(surname_freq), 0, surname_freq),
    surname_freq_log = log1p(surname_count),

    # ---------- NAME UNIQUENESS FEATURES ----------
    # Rare name-pairs are strong identifiers; common ones need more evidence
    name_pair_count = ifelse(is.na(name_pair_count), 1L, name_pair_count),
    name_pair_freq_log = log1p(name_pair_count),
    name_is_rare = as.integer(name_pair_count <= 2),

    # ---------- DISTRICT FEATURES ----------
    # A match is "primary district" if the VT's declared district maps to the
    # census district being searched.  For _multi districts:
    #   Somerset  → primary in Cradock (Somerset's 1825 census name)
    #   Colesberg → primary in Graaff-Reinet (Colesberg's parent district)
    # Searches in other districts for these VTs are genuinely cross-district.
    is_primary_district = case_when(
      !grepl("_multi", census_districts) ~ TRUE,
      census_districts == "Somerset_multi" & search_district == "Cradock" ~ TRUE,
      census_districts == "Colesberg_multi" & search_district == "Graaff-Reinet" ~ TRUE,
      TRUE ~ FALSE
    ),
    district_match_score = ifelse(is_primary_district, 1.0, 0.7),

    # ---------- LENGTH AND WORD COUNT FEATURES ----------
    len_vt_name = nchar(vt_name_clean),
    len_census_name = nchar(census_first_std),
    len_ratio = pmin(len_vt_name, len_census_name) / pmax(len_vt_name, len_census_name, 1),
    n_words_vt = str_count(vt_name_clean, "\\S+"),
    n_words_census = str_count(census_first_std, "\\S+"),
    word_diff = abs(n_words_vt - n_words_census),

    # ---------- EXACT MATCH INDICATORS ----------
    exact_first_match = as.integer(vt_first_only == census_first_only),
    exact_full_match = as.integer(vt_name_clean == census_first_std)
  ) %>%
  # Handle NAs for numeric columns used in RF
  mutate(
    across(c(jw_male_full, jw_male_first, lv_male_full, jw_initials,
             jw_wife_surname, jw_wife_first, jw_wife_vs_husband_surname),
           ~ ifelse(is.na(.), 0, .)),
    across(where(is.numeric), ~ ifelse(is.infinite(.), 0, .))
  )

# Compute composite scores (used in fallback matching and downstream analysis)
candidates <- candidates %>%
  mutate(
    husband_score = case_when(
      both_multi_name == 1 ~ 0.7 * jw_male_full + 0.3 * jw_male_first,
      TRUE ~ 0.4 * jw_male_full + 0.6 * jw_male_first
    ),
    wife_score = case_when(
      can_match_wife_surname & !is.na(jw_wife_first) ~ 0.6 * jw_wife_surname + 0.4 * jw_wife_first,
      can_match_wife_surname ~ jw_wife_surname,
      can_match_wife_first ~ jw_wife_first,  # Use first name alone when surname unavailable
      TRUE ~ NA_real_
    )
  )
candidates
}
candidates <- build_pair_features(candidates)

# --------------------------------------------------------------------------
# 4.3 STEP 3: LOAD EXPERT-LABELED TRAINING DATA
# --------------------------------------------------------------------------
# Four labelers independently labeled ~250 candidate pairs each (~1000 total).
# Labels: 1 = match, 0 = non-match, ? or NA = uncertain (excluded).

cat("\nLoading expert-labeled training data...\n")

# Load the four labeled files
labeled1 <- readxl::read_xlsx("data/raw/training_sample_for_labeling.xlsx")
labeled2 <- readxl::read_xlsx("data/raw/training_sample_labeler2.xlsx")
labeled3 <- readxl::read_xlsx("data/raw/training_sample_labeler3.xlsx")
labeled4 <- readxl::read_xlsx("data/raw/training_sample_labeler4.xlsx")

# Combine and clean labels: convert to integer, exclude uncertain/invalid
labeled_all <- bind_rows(labeled1, labeled2, labeled3, labeled4) %>%
  mutate(LABEL = suppressWarnings(as.integer(LABEL))) %>%
  filter(!is.na(LABEL), LABEL %in% c(0L, 1L))

n_total_raw <- nrow(labeled1) + nrow(labeled2) + nrow(labeled3) + nrow(labeled4)
cat("  Total pairs across 4 files:", n_total_raw, "\n")
cat("  Valid labels (0 or 1):", nrow(labeled_all), "\n")
cat("  Excluded (uncertain/invalid):", n_total_raw - nrow(labeled_all), "\n")

# Training labels (columns row_id, census_id, label). Labeled pairs whose census
# head is not a candidate cannot enter training and are reported.
labels_file <- "data/linkage/training_labels.csv"
stopifnot(file.exists(labels_file))
training_pairs <- read.csv(labels_file)
if (!"accepted_training_order" %in% names(training_pairs)) training_pairs$accepted_training_order <- seq_len(nrow(training_pairs))
train_data <- candidates %>% inner_join(training_pairs %>% select(row_id, census_id, label, accepted_training_order),
  by=c("row_id", "census_id"), relationship="one-to-one") %>%
  arrange(accepted_training_order)
cat("  Training labels from", labels_file, ":", nrow(train_data), "of", nrow(training_pairs),
    "labelled pairs are candidates\n")

cat("  Labeled pairs matched to candidates:", nrow(train_data), "\n")
cat("  Positive examples (matches):", sum(train_data$label == 1), "\n")
cat("  Negative examples (non-matches):", sum(train_data$label == 0), "\n")
cat("  Match rate:", round(100 * mean(train_data$label == 1), 1), "%\n")

# Map expert labels back to candidates for downstream analysis (Part 15 ablation)
label_lookup <- train_data %>% select(row_id, census_id, label)
key_cand <- paste(candidates$row_id, candidates$census_id, sep="|")
key_train <- paste(label_lookup$row_id, label_lookup$census_id, sep="|")
candidates$label <- label_lookup$label[match(key_cand, key_train)]
cat("  Labels mapped to candidates:", sum(!is.na(candidates$label)), "of", nrow(candidates), "\n")

# --------------------------------------------------------------------------
# 4.4 STEP 4: TRAIN RANDOM FOREST WITH CROSS-VALIDATION
# --------------------------------------------------------------------------

# Define feature columns for RF
feature_cols <- c(
  # Male name features
  "jw_male_full", "jw_male_first", "lv_male_full", "jw_initials", "initials_exact",
  "both_multi_name", "len_ratio", "word_diff", "exact_first_match", "exact_full_match",
  # Wife presence features (crucial per paper)
  "vt_has_wife", "census_has_wife", "both_have_wife", "neither_has_wife", "wife_mismatch",
  # Wife name features
  "jw_wife_surname", "jw_wife_first", "jw_wife_vs_husband_surname",
  # Surname and name-pair frequency (rarity)
  "surname_freq_log", "name_pair_freq_log", "name_is_rare",
  # District
  "is_primary_district"
)

# Check we have enough training data
min_positive <- 50
min_negative <- 50

if (sum(train_data$label == 1) >= min_positive && sum(train_data$label == 0) >= min_negative) {

  cat("\nRunning stratified 5-fold cross-validation to select RF threshold...\n")

  # Prepare training matrix
  X_train <- as.data.frame(train_data[, feature_cols])
  X_train <- X_train %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
  y_train <- as.factor(train_data$label)

  # --- Stratified 5-fold CV ---
  set.seed(42)
  n_folds <- 5
  threshold_grid <- seq(0.30, 0.80, by = 0.05)

  # Create stratified folds (equal class proportions in each fold)
  pos_idx <- which(y_train == "1")
  neg_idx <- which(y_train == "0")
  pos_folds <- sample(rep(1:n_folds, length.out = length(pos_idx)))
  neg_folds <- sample(rep(1:n_folds, length.out = length(neg_idx)))
  fold_id <- integer(length(y_train))
  fold_id[pos_idx] <- pos_folds
  fold_id[neg_idx] <- neg_folds

  # Collect out-of-fold predicted probabilities
  all_probs <- numeric(length(y_train))
  for (k in 1:n_folds) {
    train_idx <- fold_id != k
    test_idx <- fold_id == k

    rf_cv <- randomForest(
      x = X_train[train_idx, ],
      y = y_train[train_idx],
      ntree = 500,
      mtry = floor(sqrt(length(feature_cols))),
      classwt = c("0" = 1, "1" = sum(y_train[train_idx] == "0") / sum(y_train[train_idx] == "1"))
    )
    all_probs[test_idx] <- predict(rf_cv, X_train[test_idx, ], type = "prob")[, "1"]
  }

  # Evaluate precision / recall / F0.5 at each threshold
  # F0.5 weights precision 2x over recall: we prefer missing true matches
  # over incorrectly linking the wrong individuals
  cv_results <- data.frame(threshold = threshold_grid,
                           precision = NA_real_, recall = NA_real_,
                           f1 = NA_real_, f0.5 = NA_real_)
  actual <- as.integer(as.character(y_train))

  for (i in seq_along(threshold_grid)) {
    thr <- threshold_grid[i]
    pred <- as.integer(all_probs >= thr)
    tp <- sum(pred == 1 & actual == 1)
    fp <- sum(pred == 1 & actual == 0)
    fn <- sum(pred == 0 & actual == 1)
    prec <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    rec  <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    f1   <- ifelse(prec + rec > 0, 2 * prec * rec / (prec + rec), 0)
    f05  <- ifelse(prec + rec > 0, 1.25 * prec * rec / (0.25 * prec + rec), 0)
    cv_results$precision[i] <- round(prec, 3)
    cv_results$recall[i]    <- round(rec, 3)
    cv_results$f1[i]        <- round(f1, 3)
    cv_results$f0.5[i]      <- round(f05, 3)
  }

  cat("\nCross-validation results by threshold:\n")
  print(cv_results)

  # Select threshold that maximizes F0.5 (precision-weighted)
  best_idx <- which.max(cv_results$f0.5)
  RF_THRESHOLD <- cv_results$threshold[best_idx]
  cat("\nBest threshold (max F0.5 — precision-weighted):", RF_THRESHOLD,
      " (F0.5 =", cv_results$f0.5[best_idx],
      ", Precision =", cv_results$precision[best_idx],
      ", Recall =", cv_results$recall[best_idx], ")\n")

  # ---------- A1: Export CV diagnostics with confusion matrix counts ----------
  # Recompute TP/FP/FN/TN for each threshold and save alongside metrics
  cv_export <- cv_results
  cv_export$tp <- NA_integer_
  cv_export$fp <- NA_integer_
  cv_export$fn <- NA_integer_
  cv_export$tn <- NA_integer_
  for (i in seq_along(threshold_grid)) {
    pred <- as.integer(all_probs >= threshold_grid[i])
    cv_export$tp[i] <- sum(pred == 1 & actual == 1)
    cv_export$fp[i] <- sum(pred == 1 & actual == 0)
    cv_export$fn[i] <- sum(pred == 0 & actual == 1)
    cv_export$tn[i] <- sum(pred == 0 & actual == 0)
  }
  cv_path <- safe_write_csv(cv_export, "output/tables/cv_diagnostics.csv", row.names = FALSE)
  cat("  Exported '", basename(cv_path), "' (", nrow(cv_export), "threshold rows)\n", sep = "")

  # --- Train final RF on ALL expert-labeled data ---
  cat("\nTraining final Random Forest on all labeled data...\n")

  set.seed(42)
  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    ntree = 500,
    mtry = floor(sqrt(length(feature_cols))),
    importance = TRUE,
    classwt = c("0" = 1, "1" = sum(y_train == "0") / sum(y_train == "1"))
  )

  cat("  RF training complete.\n")
  cat("  OOB error rate:", round(rf_model$err.rate[nrow(rf_model$err.rate), "OOB"] * 100, 1), "%\n")

  # Variable importance
  importance_df <- data.frame(
    variable = rownames(rf_model$importance),
    importance = rf_model$importance[, "MeanDecreaseGini"]
  ) %>%
    arrange(desc(importance))

  cat("\nRandom Forest Variable Importance (top 15):\n")
  print(head(importance_df, 15))

  # Save importance plot
  importance_plot_data <- head(importance_df, 15) %>%
    mutate(variable = factor(variable, levels = rev(variable)))

  p_rf_importance <- ggplot(importance_plot_data,
                            aes(x = importance, y = variable)) +
    geom_bar(stat = "identity", fill = "#5C2346") +
    labs(x = "Mean Decrease in Gini", y = "") +
    theme_leap()

  # the figure is drawn from the linkage classifier after it
  # is fitted (see "Variable importance of the linkage classifier" below); the
  # figure number is reserved here so later figure numbers are unchanged.
  fig_rf_importance_file <- next_fig("rf_variable_importance.png")

  # --------------------------------------------------------------------------
  # 4.5 STEP 5: PREDICT ON ALL CANDIDATES
  # --------------------------------------------------------------------------

  cat("\nPredicting match probabilities for all candidates...\n")

  X_all <- as.data.frame(candidates[, feature_cols])
  X_all <- X_all %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))

  # Get probabilities (votes) for class 1
  rf_pred <- predict(rf_model, X_all, type = "prob")
  candidates$rf_score <- rf_pred[, "1"]  # Probability of being a match

  cat("  Prediction complete.\n")
  cat("  RF score distribution:\n")
  print(quantile(candidates$rf_score, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

  # --------------------------------------------------------------------------
  # 4.6 STEP 6: SELECT BEST MATCHES
  # --------------------------------------------------------------------------
  # Precision-first strategy: we prefer missing true matches over false links.
  #
  # Tiered thresholds reflecting strength of evidence:
  #   1. Same district + wife corroboration:  standard threshold (strongest evidence)
  #   2. Same district + male only:           stricter threshold
  #   3. Cross-district + wife corroboration: very strict (near-perfect names needed)
  #   4. Cross-district + male only:          REJECT (insufficient evidence)
  #
  # "Absence of wife makes it far harder to identify a link" — Fourie & Green

  RF_THRESHOLD_MALE_ONLY    <- min(RF_THRESHOLD + 0.15, 0.85)
  RF_THRESHOLD_CROSS_WIFE   <- min(RF_THRESHOLD + 0.20, 0.90)
  cat("  Thresholds:\n")
  cat("    Same district + wife:", RF_THRESHOLD, "\n")
  cat("    Same district + male only:", RF_THRESHOLD_MALE_ONLY, "\n")
  cat("    Cross-district + wife:", RF_THRESHOLD_CROSS_WIFE, "\n")
  cat("    Cross-district + male only: REJECTED (insufficient evidence)\n")

  # For each Voortrekker, select the best census match
  best_matches <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(rf_score)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      # Wife corroboration: both records have wife info AND wife names are similar
      wife_corroborated = both_have_wife == 1 &
        (jw_wife_first >= 0.70 | (!is.na(wife_score) & wife_score >= 0.60)),

      # Effective threshold depends on district + wife evidence
      effective_threshold = case_when(
        is_primary_district & wife_corroborated  ~ RF_THRESHOLD,
        is_primary_district & !wife_corroborated ~ RF_THRESHOLD_MALE_ONLY,
        !is_primary_district & wife_corroborated ~ RF_THRESHOLD_CROSS_WIFE,
        !is_primary_district & !wife_corroborated ~ Inf  # Reject: male-only cross-district
      )
    ) %>%
    filter(rf_score >= effective_threshold)

  # ---------- ONE-TO-ONE MATCHING CONSTRAINT ----------
  # If multiple Voortrekkers claim the same census record, keep only the best.
  # Preserve the accepted implementation: wife corroboration first, then RF score.
  # Exact source adjudication below resolves remaining competing identities.
  n_before_dedup <- nrow(best_matches)
  best_matches <- best_matches %>%
    group_by(census_id) %>%
    arrange(desc(wife_corroborated), desc(rf_score)) %>%
    slice(1) %>%
    ungroup()
  n_dropped <- n_before_dedup - nrow(best_matches)
  if (n_dropped > 0) {
    cat("  One-to-one constraint: removed", n_dropped,
        "duplicate census matches (kept highest-scoring VT per census record)\n")
  }

  best_matches <- best_matches %>%
    mutate(
      match_score = rf_score,
      match_quality = case_when(
        rf_score >= 0.90 & wife_corroborated ~ "Excellent",
        rf_score >= 0.90 ~ "Good",
        rf_score >= 0.70 & wife_corroborated ~ "Good",
        rf_score >= 0.70 ~ "Fair",
        TRUE ~ "Fair"
      ),
      wife_matched = wife_corroborated,
      district_matched = is_primary_district
    )

  # Track whether wife info was available and used
  best_matches <- best_matches %>%
    mutate(
      wife_info_used = both_have_wife == 1,
      wife_helped = wife_info_used & wife_matched
    )

  cat("  Total accepted matches:", nrow(best_matches), "\n")
  cat("    Same district + wife:", sum(best_matches$is_primary_district & best_matches$wife_corroborated), "\n")
  cat("    Same district + male only:", sum(best_matches$is_primary_district & !best_matches$wife_corroborated), "\n")
  cat("    Cross-district + wife:", sum(!best_matches$is_primary_district & best_matches$wife_corroborated), "\n")
  cat("    Cross-district + male only:", sum(!best_matches$is_primary_district & !best_matches$wife_corroborated), "(should be 0)\n")

  # --------------------------------------------------------------------------
  # 4.6a INTEGRATE MANUAL REVIEW DECISIONS
  # --------------------------------------------------------------------------
  # If a reviewed manual_review_all_matches.xlsx exists in Data/, apply the
  # researcher's MANUAL_DECISION overrides:
  #   - Sheet 1 (Accepted): rows marked "N" are removed from best_matches
  #   - Sheet 2 (Rejected): rows marked "Y" are added to best_matches
  # This ensures the final match set reflects expert judgement.

  manual_review_path <- "data/raw/manual_review_all_matches.xlsx"
  if (file.exists(manual_review_path)) {

    cat("\n  --- Applying manual review decisions ---\n")
    accepted_review <- readxl::read_xlsx(manual_review_path, sheet = 1)
    rejected_review <- readxl::read_xlsx(manual_review_path, sheet = 2)

    # Guard: the pipeline also exports a blank review template to
    # output/tables/manual_review_all_matches.xlsx; fail loudly if that template
    # is used in place of a completed review file.
    if (all(is.na(accepted_review$MANUAL_DECISION)) &&
        all(is.na(rejected_review$MANUAL_DECISION))) {
      stop("manual_review_all_matches.xlsx in data/raw/ has no MANUAL_DECISION ",
           "values: it looks like the blank exported template, not the ",
           "completed review.", call. = FALSE)
    }

    review_pairs <- bind_rows(
      accepted_review %>% select(row_id, census_id, MANUAL_DECISION) %>% mutate(MANUAL_DECISION = as.character(MANUAL_DECISION)),
      rejected_review %>% select(row_id, census_id, MANUAL_DECISION) %>% mutate(MANUAL_DECISION = as.character(MANUAL_DECISION))) %>%
      mutate(pair_key = paste(row_id, census_id, sep = "|"),
             decision = toupper(as.character(MANUAL_DECISION)))
    stopifnot(!anyDuplicated(review_pairs$pair_key))
    candidates$pair_key <- paste(candidates$row_id, candidates$census_id, sep = "|")
    best_matches$pair_key <- paste(best_matches$row_id, best_matches$census_id, sep = "|")
    # Explicit negative decisions attach to the exact pair on either worksheet.
    best_matches <- best_matches %>% filter(!pair_key %in%
      review_pairs$pair_key[review_pairs$decision %in% c("N", "FALSE")])
    n_before_manual <- nrow(best_matches)

    # Remove matches the researcher rejected
    if ("MANUAL_DECISION" %in% names(accepted_review)) {
      reject_ids <- review_pairs %>%
        filter(decision %in% c("N", "FALSE")) %>% pull(pair_key)
      if (length(reject_ids) > 0) {
        best_matches <- best_matches %>% filter(!pair_key %in% reject_ids)
        cat("    Removed", length(reject_ids), "matches rejected by researcher\n")
      }
    }

    # Add matches the researcher accepted from the rejected sheet
    if ("MANUAL_DECISION" %in% names(rejected_review)) {
      rescue_pairs <- rejected_review %>%
        filter(toupper(MANUAL_DECISION) %in% c("Y", "TRUE")) %>%
        select(row_id, census_id)
      rescue_ids <- rescue_pairs$row_id

      if (length(rescue_ids) > 0) {
        # Retrieve these from candidates (full feature set)
        rescued <- candidates %>%
          semi_join(rescue_pairs, by = c("row_id", "census_id")) %>%
          mutate(
            wife_corroborated = both_have_wife == 1 &
              (jw_wife_first >= 0.70 | (!is.na(wife_score) & wife_score >= 0.60)),
            effective_threshold = NA_real_,  # Manual override
            match_score = rf_score,
            match_quality = "Manual",
            wife_matched = wife_corroborated,
            district_matched = is_primary_district,
            wife_info_used = both_have_wife == 1,
            wife_helped = wife_info_used & wife_matched
          )

        # Ensure rescued matches don't duplicate census_ids already in best_matches
        rescued <- rescued %>%
          filter(!census_id %in% best_matches$census_id)

        best_matches <- bind_rows(
          best_matches,
          rescued %>% select(any_of(names(best_matches)))
        )
        cat("    Added", nrow(rescued), "matches rescued by researcher\n")
      }
    }

    cat("    Match count: ", n_before_manual, " -> ", nrow(best_matches), "\n")
    cat("    (", nrow(best_matches) - n_before_manual, " net change from manual review)\n")


  } else {
    cat("\n  No manual review file found at '", manual_review_path, "'\n")
    cat("  Proceeding with RF-only matches.\n")
  }

  # --------------------------------------------------------------------------
  # 4.6b EXPORT BOUNDARY CASES FOR VISUAL INSPECTION
  # --------------------------------------------------------------------------
  # Show the best candidate per Voortrekker, sorted by RF score, around the
  # decision thresholds so the researcher can verify match quality at the margin.

  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required but not installed.", call. = FALSE)
  }
  library(writexl)

  # Best candidate per VT (before any threshold filtering)
  boundary_check <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(rf_score)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      wife_corroborated = both_have_wife == 1 &
        (jw_wife_first >= 0.70 | (!is.na(wife_score) & wife_score >= 0.60)),
      effective_threshold = case_when(
        is_primary_district & wife_corroborated  ~ RF_THRESHOLD,
        is_primary_district & !wife_corroborated ~ RF_THRESHOLD_MALE_ONLY,
        !is_primary_district & wife_corroborated ~ RF_THRESHOLD_CROSS_WIFE,
        !is_primary_district & !wife_corroborated ~ Inf
      ),
      ACCEPTED = rf_score >= effective_threshold,
      margin = rf_score - effective_threshold
    ) %>%
    # Take 40 closest to boundary on each side (80 total)
    arrange(abs(margin)) %>%
    slice_head(n = 80) %>%
    arrange(desc(rf_score)) %>%
    select(
      ACCEPTED,
      rf_score,
      margin,
      effective_threshold,
      same_district = is_primary_district,
      wife_corroborated,
      name_pair_count,
      vt_surname = vt_surname_std,
      vt_first_name = vt_name_clean,
      vt_wife_surname = vt_wife_surname,
      vt_wife_first = vt_wife_first,
      census_name = name_raw,
      census_first = census_first_std,
      census_wife_surname = census_wife_surname_std,
      census_wife_first = census_wife_first_std,
      census_district = search_district,
      jw_male_full,
      jw_male_first,
      jw_wife_first,
      jw_wife_surname,
      husband_score,
      wife_score
    )

  writexl::write_xlsx(boundary_check, "output/tables/boundary_cases_for_inspection.xlsx")
  cat("\n  Exported 'output/tables/boundary_cases_for_inspection.xlsx' (",
      sum(boundary_check$ACCEPTED), "accepted /",
      sum(!boundary_check$ACCEPTED), "rejected near the threshold)\n")

  # --------------------------------------------------------------------------
  # 4.6c COMPREHENSIVE MANUAL REVIEW DATASET
  # --------------------------------------------------------------------------
  # Export ALL accepted matches + ALL unmatched VTs' best candidates,
  # so the researcher can (a) verify every accepted match and (b) rescue
  # close misses.

  # Best candidate per VT (regardless of acceptance)
  all_best <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(rf_score)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      wife_corroborated = both_have_wife == 1 &
        (jw_wife_first >= 0.70 | (!is.na(wife_score) & wife_score >= 0.60)),
      effective_threshold = case_when(
        is_primary_district & wife_corroborated  ~ RF_THRESHOLD,
        is_primary_district & !wife_corroborated ~ RF_THRESHOLD_MALE_ONLY,
        !is_primary_district & wife_corroborated ~ RF_THRESHOLD_CROSS_WIFE,
        !is_primary_district & !wife_corroborated ~ Inf
      ),
      ACCEPTED = rf_score >= effective_threshold,
      margin = rf_score - effective_threshold,
      MANUAL_DECISION = ""  # Column for researcher to fill in: "Y" / "N" / "?"
    ) %>%
    arrange(desc(ACCEPTED), desc(rf_score)) %>%
    select(
      ACCEPTED,
      MANUAL_DECISION,
      rf_score,
      margin,
      effective_threshold,
      same_district = is_primary_district,
      wife_corroborated,
      name_pair_count,
      vt_surname = vt_surname_std,
      vt_first_name = vt_name_clean,
      vt_wife_surname = vt_wife_surname,
      vt_wife_first = vt_wife_first,
      census_name = name_raw,
      census_first = census_first_std,
      census_wife_surname = census_wife_surname_std,
      census_wife_first = census_wife_first_std,
      census_district = search_district,
      vt_declared_district = census_districts,
      jw_male_full,
      jw_male_first,
      jw_wife_first,
      jw_wife_surname,
      husband_score,
      wife_score,
      census_id,
      row_id
    )

  # ---------- B1–B3: Improved manual review xlsx ----------
  # Add census household variables and second-best candidate

  # Get second-best candidate per VT
  second_best <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(rf_score)) %>%
    slice(2) %>%
    ungroup() %>%
    select(row_id,
           second_rf_score = rf_score,
           second_census_name = name_raw,
           second_census_first = census_first_std,
           second_census_district = search_district,
           second_census_id = census_id)

  # Join second-best to all_best
  all_best <- all_best %>%
    left_join(second_best, by = "row_id")

  # Join census household variables via census_id
  # all_districts already exists (created in Part 1.12)
  census_hh_vars <- all_districts %>%
    mutate(
      census_total_slaves = coalesce(as.numeric(slaves_men), 0) + coalesce(as.numeric(slaves_women), 0),
      census_total_khoe = coalesce(as.numeric(khoe_men), 0) + coalesce(as.numeric(khoe_women), 0),
      census_horses = coalesce(as.numeric(horses), 0),
      census_cattle = coalesce(as.numeric(cattle), 0),
      census_children = coalesce(as.numeric(settler_sons), 0) + coalesce(as.numeric(settler_daughters), 0),
      census_household_size = coalesce(as.numeric(settler_men), 0) + coalesce(as.numeric(settler_women), 0) +
        coalesce(as.numeric(settler_sons), 0) + coalesce(as.numeric(settler_daughters), 0)
    ) %>%
    select(census_id, census_total_slaves, census_total_khoe, census_horses,
           census_cattle, census_children, census_household_size)

  all_best <- all_best %>%
    left_join(census_hh_vars, by = "census_id")

  # Sort for maximum usefulness: accepted (weakest first), then rejected (strongest first)
  accepted_sheet <- all_best %>% filter(ACCEPTED) %>% arrange(rf_score)
  rejected_sheet <- all_best %>% filter(!ACCEPTED) %>% arrange(desc(rf_score))

  # Summary sheet
  summary_sheet <- data.frame(
    metric = c("Total VTs", "Accepted matches", "Rejected (best candidate shown)",
               "Mean RF score (accepted)", "Mean RF score (rejected best candidate)",
               "Wife corroborated (accepted)", "Same district (accepted)"),
    value = c(nrow(all_best), sum(all_best$ACCEPTED), sum(!all_best$ACCEPTED),
              round(mean(accepted_sheet$rf_score), 3),
              round(mean(rejected_sheet$rf_score, na.rm = TRUE), 3),
              sum(accepted_sheet$wife_corroborated),
              sum(accepted_sheet$same_district))
  )

  writexl::write_xlsx(
    list(
      "Accepted (weakest first)" = accepted_sheet,
      "Rejected (strongest first)" = rejected_sheet,
      "Summary" = summary_sheet
    ),
    "output/tables/manual_review_all_matches.xlsx"
  )
  cat("\n  Exported 'output/tables/manual_review_all_matches.xlsx':\n")
  cat("    Sheet 1: Accepted (", nrow(accepted_sheet), ") sorted weakest first\n")
  cat("    Sheet 2: Rejected (", nrow(rejected_sheet), ") sorted strongest first\n")
  cat("    Sheet 3: Summary statistics\n")
  cat("    Census household variables added: slaves, khoe, horses, cattle, children, household_size\n")
  cat("    Second-best candidate columns added\n")
  cat("    Use MANUAL_DECISION column: 'Y' to accept, 'N' to reject, '?' if unsure\n")

  # ---------- A2: Export matched vs unmatched VT characteristics ----------
  # Use all_best (one row per VT with best candidate) to ensure exactly 917 rows
  matched_ids <- best_matches$row_id
  vt_comparison <- all_best %>%
    select(row_id, vt_surname, vt_first_name, census_district,
           vt_declared_district, vt_wife_surname, vt_wife_first,
           wife_corroborated, ACCEPTED) %>%
    mutate(matched = row_id %in% matched_ids) %>%
    left_join(vt_adults %>% select(row_id, leader_std), by="row_id")

  cat("  VT comparison: ", nrow(vt_comparison), "rows,",
      sum(vt_comparison$matched), "matched\n")

  # Compute surname frequency for each VT from the census
  vt_comparison <- vt_comparison %>%
    mutate(vt_surname_std = toupper(trimws(str_squish(as.character(vt_surname)))))
  vt_comparison <- vt_comparison %>%
    left_join(surname_freq %>% select(census_surname_std, surname_freq),
              by = c("vt_surname_std" = "census_surname_std")) %>%
    rename(vt_surname_freq = surname_freq) %>%
    mutate(vt_surname_freq = coalesce(vt_surname_freq, 0))

  # Wife name availability (use vt_wife_first / vt_wife_surname from all_best)
  vt_comparison <- vt_comparison %>%
    mutate(has_wife = (!is.na(vt_wife_first) & trimws(as.character(vt_wife_first)) != "" &
              toupper(trimws(as.character(vt_wife_first))) != "NA") |
             (!is.na(vt_wife_surname) & trimws(as.character(vt_wife_surname)) != "" &
              toupper(trimws(as.character(vt_wife_surname))) != "NA"))

  # District distribution (use vt_declared_district from all_best)
  district_dist <- vt_comparison %>%
    group_by(matched, vt_declared_district) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(matched) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()

  # Summary by match status
  match_summary <- vt_comparison %>%
    group_by(matched) %>%
    summarise(
      n = n(),
      wife_available_pct = round(mean(has_wife) * 100, 1),
      mean_surname_freq = round(mean(vt_surname_freq, na.rm = TRUE), 4),
      .groups = "drop"
    )

  # Leader distribution
  if ("leader_std" %in% names(vt_comparison)) {
    leader_dist <- vt_comparison %>%
      group_by(matched, leader_std) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(matched) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()
    write.csv(leader_dist, "output/tables/matched_vs_unmatched_leaders.csv", row.names = FALSE)
  }

  # Statistical tests
  wife_test <- chisq.test(table(vt_comparison$matched, vt_comparison$has_wife))
  freq_test <- t.test(vt_surname_freq ~ matched, data = vt_comparison)

  # Combine into single export
  matched_vs_unmatched <- bind_rows(
    match_summary %>% mutate(variable = "summary"),
    data.frame(
      matched = NA, n = NA,
      wife_available_pct = NA, mean_surname_freq = NA,
      variable = "tests",
      wife_chi2_p = round(wife_test$p.value, 4),
      freq_ttest_p = round(freq_test$p.value, 4),
      freq_diff = round(diff(match_summary$mean_surname_freq), 4)
    )
  )
  write.csv(matched_vs_unmatched, "output/tables/matched_vs_unmatched.csv", row.names = FALSE)
  write.csv(district_dist, "output/tables/matched_vs_unmatched_districts.csv", row.names = FALSE)
  cat("  Exported 'matched_vs_unmatched.csv' and 'matched_vs_unmatched_districts.csv'\n")

  rf_model_trained <- TRUE

} else {
  cat("\nWARNING: Insufficient training data for Random Forest.\n")
  cat("  Need at least", min_positive, "positive and", min_negative, "negative examples.\n")
  cat("  Falling back to JW-based scoring.\n")

  # Fallback to JW-based matching
  candidates <- candidates %>%
    mutate(
      match_score = husband_score +
        case_when(
          !is.na(wife_score) & wife_score >= 0.85 ~ 0.10,
          !is.na(wife_score) & wife_score >= 0.70 ~ 0.05,
          !is.na(wife_score) & wife_score >= 0.50 ~ 0.00,
          !is.na(wife_score) ~ -0.05,
          TRUE ~ 0
        ) +
        ifelse(is_primary_district, 0.02, 0)
    )

  best_matches <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(match_score)) %>%
    slice(1) %>%
    ungroup() %>%
    filter(match_score >= 0.70) %>%
    mutate(
      match_quality = case_when(
        match_score >= 0.90 ~ "Excellent",
        match_score >= 0.80 ~ "Good",
        match_score >= 0.70 ~ "Fair",
        TRUE ~ "Poor"
      ),
      wife_matched = !is.na(wife_score) & wife_score >= 0.70,
      district_matched = is_primary_district
    )

  rf_model_trained <- FALSE
}

cat("\nWife matching statistics:\n")
cat("  Candidates with wife comparison possible:", sum(candidates$can_match_wife), "\n")
cat("  Candidates where wife matched (>=0.70):",
    sum(!is.na(candidates$wife_score) & candidates$wife_score >= 0.70, na.rm = TRUE), "\n")

# Save candidates for later analysis in Part 15 (match rate comparison)
candidates_for_analysis <- candidates

# Also save to RDS file for reproducibility (can be loaded by other scripts)
saveRDS(candidates_for_analysis, "output/tables/candidates_for_analysis.rds")
cat("Saved candidates data to: candidates_for_analysis.rds\n")

cat("\nDistrict matching statistics:\n")
cat("  Candidates with primary district match:", sum(candidates$is_primary_district), "\n")
cat("  Candidates from multi-district search:", sum(!candidates$is_primary_district), "\n")
cat("  Pct primary district:", round(100 * mean(candidates$is_primary_district), 1), "%\n")

cat("\nMatch score distribution (RF-based):\n")
cat("  Mean:", round(mean(best_matches$match_score), 3), "\n")
cat("  Median:", round(median(best_matches$match_score), 3), "\n")
print(quantile(best_matches$match_score, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

# --------------------------------------------------------------------------
# 4.7 STEP 7: WIFE MATCHING IMPACT ANALYSIS
# --------------------------------------------------------------------------

cat("\n--- Wife Matching Impact ---\n")
wife_match_summary <- best_matches %>%
  summarise(
    total_matches = n(),
    wife_info_available = sum(can_match_wife, na.rm = TRUE),
    wife_matched_well = sum(wife_matched, na.rm = TRUE),
    pct_with_wife_info = round(100 * wife_info_available / total_matches, 1),
    pct_wife_matched = round(100 * wife_matched_well / total_matches, 1)
  )

cat("  Best matches with wife info available:", wife_match_summary$wife_info_available,
    "(", wife_match_summary$pct_with_wife_info, "%)\n")
cat("  Best matches where wife also matched:", wife_match_summary$wife_matched_well,
    "(", wife_match_summary$pct_wife_matched, "%)\n")

# Compare scores for matches with vs without wife info
if (sum(best_matches$can_match_wife, na.rm = TRUE) > 0) {
  with_wife <- best_matches %>% filter(can_match_wife == TRUE)
  without_wife <- best_matches %>% filter(can_match_wife == FALSE | is.na(can_match_wife))

  cat("\n  Matches WITH wife comparison:\n")
  cat("    N:", nrow(with_wife), "\n")
  cat("    Mean RF score:", round(mean(with_wife$match_score, na.rm = TRUE), 3), "\n")
  cat("    Mean husband score:", round(mean(with_wife$husband_score, na.rm = TRUE), 3), "\n")
  cat("    Mean wife score:", round(mean(with_wife$wife_score, na.rm = TRUE), 3), "\n")

  cat("\n  Matches WITHOUT wife comparison:\n")
  cat("    N:", nrow(without_wife), "\n")
  cat("    Mean RF score:", round(mean(without_wife$match_score, na.rm = TRUE), 3), "\n")
  cat("    Mean husband score:", round(mean(without_wife$husband_score, na.rm = TRUE), 3), "\n")

  # Key comparison: does wife info improve match quality?
  if (nrow(with_wife) > 0 && nrow(without_wife) > 0) {
    cat("\n  WIFE INFO IMPACT:\n")
    score_diff <- mean(with_wife$match_score, na.rm = TRUE) -
                  mean(without_wife$match_score, na.rm = TRUE)
    cat("    RF score difference (with - without wife):", round(score_diff, 3), "\n")

    # Quality distribution comparison
    with_wife_excellent <- mean(with_wife$match_quality == "Excellent")
    without_wife_excellent <- mean(without_wife$match_quality == "Excellent")
    cat("    Pct Excellent (with wife):", round(100 * with_wife_excellent, 1), "%\n")
    cat("    Pct Excellent (without wife):", round(100 * without_wife_excellent, 1), "%\n")
  }
}

cat("\nMatch quality distribution:\n")
print(table(best_matches$match_quality))

# --------------------------------------------------------------------------
# 4.8 STEP 8: CROSS-DISTRICT MATCHING FOR UNMATCHED
# --------------------------------------------------------------------------

# Find Voortrekkers not yet matched
matched_vt_ids <- best_matches$row_id

unmatched_vt <- vt_expanded %>%
  distinct(row_id, .keep_all = TRUE) %>%
  filter(!row_id %in% matched_vt_ids,
         !is.na(vt_surname_std), !is.na(vt_name_clean))

if (nrow(unmatched_vt) > 0 && rf_model_trained) {
  cat("\nAttempting cross-district matching for unmatched Voortrekkers...\n")

  # Try matching against ALL districts (cross-district search)
  # vt_expanded already has standardized names and wife columns
  cross_candidates <- unmatched_vt %>%
    inner_join(
      all_districts %>% select(census_id, census_surname_std, census_first_std,
                                census_first_only, district, name_raw, record_nr,
                                census_wife_surname_std, census_wife_first_std,
                                census_wife_first_only),
      by = c("vt_surname_std" = "census_surname_std"),
      relationship = "many-to-many"
    )

  if (nrow(cross_candidates) > 0) {
    # Compute features for cross-district candidates
    cross_candidates <- cross_candidates %>%
      left_join(surname_freq, by = c("vt_surname_std" = "census_surname_std")) %>%
      left_join(name_pair_freq, by = c("vt_surname_std" = "census_surname_std",
                                        "census_first_only" = "census_first_only")) %>%
      mutate(
        jw_male_full = 1 - stringdist(vt_name_clean, census_first_std, method = "jw"),
        jw_male_first = 1 - stringdist(vt_first_only, census_first_only, method = "jw"),
        lv_male_full = 1 - stringdist(vt_name_clean, census_first_std, method = "lv") /
                         pmax(nchar(vt_name_clean), nchar(census_first_std), 1),
        vt_initials = sapply(vt_name_clean, get_initials),
        census_initials = sapply(census_first_std, get_initials),
        jw_initials = 1 - stringdist(vt_initials, census_initials, method = "jw"),
        initials_exact = as.integer(vt_initials == census_initials & vt_initials != ""),
        has_multi_vt = str_detect(vt_name_clean, "\\s"),
        has_multi_census = str_detect(census_first_std, "\\s"),
        both_multi_name = as.integer(has_multi_vt & has_multi_census),
        vt_has_wife = as.integer(!is.na(vt_wife_first) | !is.na(vt_wife_surname)),
        census_has_wife = as.integer(!is.na(census_wife_first_std) | !is.na(census_wife_surname_std)),
        both_have_wife = as.integer(vt_has_wife == 1 & census_has_wife == 1),
        neither_has_wife = as.integer(vt_has_wife == 0 & census_has_wife == 0),
        wife_mismatch = as.integer((vt_has_wife == 1 & census_has_wife == 0) |
                                   (vt_has_wife == 0 & census_has_wife == 1)),
        can_match_wife_first = !is.na(vt_wife_first_only) & !is.na(census_wife_first_only),
        can_match_wife_surname = !is.na(vt_wife_surname) & !is.na(census_wife_surname_std),
        jw_wife_surname = ifelse(can_match_wife_surname,
                                  1 - stringdist(vt_wife_surname, census_wife_surname_std, method = "jw"), 0),
        jw_wife_first = ifelse(can_match_wife_first,
                                1 - stringdist(vt_wife_first_only, census_wife_first_only, method = "jw"), 0),
        jw_wife_vs_husband_surname = ifelse(!is.na(census_wife_surname_std),
                                             1 - stringdist(vt_surname_std, census_wife_surname_std, method = "jw"), 0),
        surname_freq_log = log1p(ifelse(is.na(surname_count), 0, surname_count)),
        name_pair_count = ifelse(is.na(name_pair_count), 1L, name_pair_count),
        name_pair_freq_log = log1p(name_pair_count),
        name_is_rare = as.integer(name_pair_count <= 2),
        is_primary_district = FALSE,
        len_ratio = pmin(nchar(vt_name_clean), nchar(census_first_std)) /
                    pmax(nchar(vt_name_clean), nchar(census_first_std), 1),
        word_diff = abs(str_count(vt_name_clean, "\\S+") - str_count(census_first_std, "\\S+")),
        exact_first_match = as.integer(vt_first_only == census_first_only),
        exact_full_match = as.integer(vt_name_clean == census_first_std)
      ) %>%
      mutate(across(c(jw_male_full, jw_male_first, lv_male_full, jw_initials),
                    ~ ifelse(is.na(.) | is.infinite(.), 0, .)))

    # Predict using RF model
    X_cross <- as.data.frame(cross_candidates[, feature_cols])
    X_cross <- X_cross %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
    cross_candidates$rf_score <- predict(rf_model, X_cross, type = "prob")[, "1"]

    # Cross-district: only accept with wife corroboration (very strict threshold)
    # Male-only cross-district matches are rejected as insufficient evidence
    cross_best <- cross_candidates %>%
      mutate(
        wife_corroborated = both_have_wife == 1 &
          (jw_wife_first >= 0.70 | (can_match_wife_surname & jw_wife_surname >= 0.60))
      ) %>%
      group_by(row_id) %>%
      arrange(desc(rf_score)) %>%
      slice(1) %>%
      ungroup() %>%
      filter(wife_corroborated & rf_score >= RF_THRESHOLD_CROSS_WIFE) %>%
      mutate(
        match_score = rf_score,
        match_quality = case_when(
          rf_score >= 0.90 & wife_corroborated ~ "Good",
          rf_score >= 0.70 & wife_corroborated ~ "Fair",
          TRUE ~ "Fair"
        ),
        district_matched = FALSE,
        cross_district = TRUE
      )

    cat("  Cross-district matches found:", nrow(cross_best), "\n")

    if (nrow(cross_best) > 0) {
      # Combine with main matches
      best_matches <- best_matches %>%
        mutate(cross_district = FALSE) %>%
        bind_rows(cross_best %>% select(any_of(names(best_matches))))

      # Remove duplicates if any
      best_matches <- best_matches %>%
        group_by(row_id) %>%
        arrange(cross_district, desc(match_score)) %>%
        slice(1) %>%
        ungroup()
    }
  }
}

# Flag if multiple Voortrekkers matched to the same census record
best_matches <- best_matches %>%
  group_by(census_id) %>%
  mutate(n_vt_per_census = n()) %>%
  ungroup()

cat("\n========== FINAL MATCH SUMMARY (RANDOM FOREST) ==========\n")
cat("Total Voortrekkers (adult):", nrow(vt_adults), "\n")
cat("Matched (RF score >= ", RF_THRESHOLD, "):", nrow(best_matches), "\n")
cat("Unique census records matched:", n_distinct(best_matches$census_id), "\n")
cat("Match rate:", round(nrow(best_matches) / nrow(vt_adults) * 100, 1), "%\n")
cat("\nMatch quality:\n")
print(table(best_matches$match_quality))

# District matching summary
cat("\nDistrict matching summary:\n")
if ("cross_district" %in% names(best_matches)) {
  n_primary <- sum(best_matches$is_primary_district & !best_matches$cross_district, na.rm = TRUE)
  n_secondary <- sum(!best_matches$is_primary_district & !best_matches$cross_district, na.rm = TRUE)
  n_cross <- sum(best_matches$cross_district, na.rm = TRUE)
  cat("  Primary district match:", n_primary, "(", round(100*n_primary/nrow(best_matches), 1), "%)\n")
  cat("  Secondary district (multi-district search):", n_secondary, "(", round(100*n_secondary/nrow(best_matches), 1), "%)\n")
  cat("  Cross-district match:", n_cross, "(", round(100*n_cross/nrow(best_matches), 1), "%)\n")
} else {
  n_primary <- sum(best_matches$is_primary_district, na.rm = TRUE)
  n_secondary <- sum(!best_matches$is_primary_district, na.rm = TRUE)
  cat("  Primary district match:", n_primary, "(", round(100*n_primary/nrow(best_matches), 1), "%)\n")
  cat("  Secondary district (multi-district search):", n_secondary, "(", round(100*n_secondary/nrow(best_matches), 1), "%)\n")
}

# Show sample of top matches
cat("\nSample of excellent matches:\n")
best_matches %>%
  filter(match_quality == "Excellent") %>%
  select(vt_surname, vt_name, name_raw, match_score, search_district) %>%
  head(20) %>%
  print(n = 20)


# ============================================================================
# PART 4B: XGBOOST SUPERVISED MACHINE LEARNING MATCHING (OPTIONAL)
# ============================================================================
#
# NOTE: Part 4 now uses Random Forest as the primary ML method (per Fourie & Green 2018).
# This XGBoost section is retained for comparison and backward compatibility.
# The Random Forest model in Part 4 should be preferred as it properly incorporates
# wife information and uses the features described in the published methodology.
#
# Set RUN_XGBOOST_COMPARISON <- TRUE to run this section for comparison.
# ============================================================================

RUN_XGBOOST_COMPARISON <- FALSE  # Set to TRUE to also run XGBoost for comparison

if (RUN_XGBOOST_COMPARISON) {

cat("\n========== XGBOOST RECORD LINKAGE (COMPARISON) ==========\n")

# Load XGBoost library
if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop("Package 'xgboost' is required but not installed.", call. = FALSE)
}
library(xgboost)

# --------------------------------------------------------------------------
# 4B.1 FEATURE ENGINEERING FOR ML MATCHING
# --------------------------------------------------------------------------

# Create rich feature set for each candidate pair
# We'll use the candidates dataframe from the JW matching

xgb_features <- candidates %>%
  mutate(
    # String distance features
    jw_full       = 1 - stringdist(vt_name_clean, census_first_std, method = "jw"),
    jw_first      = 1 - stringdist(vt_first_only, census_first_only, method = "jw"),
    lv_full       = 1 - stringdist(vt_name_clean, census_first_std, method = "lv") /
                      pmax(nchar(vt_name_clean), nchar(census_first_std), 1),
    lv_first      = 1 - stringdist(vt_first_only, census_first_only, method = "lv") /
                      pmax(nchar(vt_first_only), nchar(census_first_only), 1),
    cosine_full   = 1 - stringdist(vt_name_clean, census_first_std, method = "cosine"),
    jaccard_full  = 1 - stringdist(vt_name_clean, census_first_std, method = "jaccard"),

    # Length-based features
    len_vt_name     = nchar(vt_name_clean),
    len_census_name = nchar(census_first_std),
    len_diff        = abs(len_vt_name - len_census_name),
    len_ratio       = pmin(len_vt_name, len_census_name) / pmax(len_vt_name, len_census_name, 1),

    # Word count features
    n_words_vt     = str_count(vt_name_clean, "\\S+"),
    n_words_census = str_count(census_first_std, "\\S+"),
    word_diff      = abs(n_words_vt - n_words_census),

    # First letter match
    first_letter_match = as.integer(substr(vt_first_only, 1, 1) == substr(census_first_only, 1, 1)),

    # Common prefix length
    common_prefix = mapply(function(a, b) {
      if (is.na(a) || is.na(b)) return(0)
      max_len <- min(nchar(a), nchar(b))
      for (i in seq_len(max_len)) {
        if (substr(a, i, i) != substr(b, i, i)) return(i - 1)
      }
      return(max_len)
    }, vt_first_only, census_first_only),

    # Common suffix length
    common_suffix = mapply(function(a, b) {
      if (is.na(a) || is.na(b)) return(0)
      a_rev <- paste(rev(strsplit(a, "")[[1]]), collapse = "")
      b_rev <- paste(rev(strsplit(b, "")[[1]]), collapse = "")
      max_len <- min(nchar(a), nchar(b))
      for (i in seq_len(max_len)) {
        if (substr(a_rev, i, i) != substr(b_rev, i, i)) return(i - 1)
      }
      return(max_len)
    }, vt_first_only, census_first_only),

    # Exact match indicators
    exact_first_match = as.integer(vt_first_only == census_first_only),
    exact_full_match  = as.integer(vt_name_clean == census_first_std),

    # Q-gram (bigram) similarity - captures character sequences
    qgram_full = 1 - stringdist(vt_name_clean, census_first_std, method = "qgram", q = 2) /
                   pmax(nchar(vt_name_clean) + nchar(census_first_std) - 2, 1),
    qgram_first = 1 - stringdist(vt_first_only, census_first_only, method = "qgram", q = 2) /
                    pmax(nchar(vt_first_only) + nchar(census_first_only) - 2, 1),

    # Soundex match (phonetic similarity) - helps with spelling variants
    soundex_match = as.integer(
      !is.na(vt_first_only) & !is.na(census_first_only) &
      nchar(vt_first_only) > 0 & nchar(census_first_only) > 0
    )  # Note: R doesn't have built-in soundex, so this is a placeholder
  ) %>%
  # Handle NAs and Inf values
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | is.infinite(.), 0, .)))

# --------------------------------------------------------------------------
# 4B.2 CREATE TRAINING DATA (SEMI-SUPERVISED)
# --------------------------------------------------------------------------

# Use high-confidence JW matches as positive examples
# and low-confidence matches as negative examples (silver standard labeling)

xgb_features <- xgb_features %>%
  mutate(
    # Create silver-standard labels based on JW score thresholds
    # This is semi-supervised: high JW = likely match, low JW = likely non-match
    # Using broader thresholds to include more training signal
    label = case_when(
      jw_full >= 0.92 & jw_first >= 0.85 ~ 1L,  # High confidence = match
      jw_full >= 0.88 & exact_first_match == 1 ~ 1L,  # Good JW + exact first = match
      exact_full_match == 1 ~ 1L,  # Exact full name = match
      jw_full >= 0.85 & jw_first >= 0.85 ~ 1L,  # Both components high = match
      jw_full < 0.55 & jw_first < 0.55 ~ 0L,  # Both components low = non-match
      jw_full < 0.45 ~ 0L,  # Very low full = non-match
      jw_first < 0.40 ~ 0L,  # Very low first = non-match
      TRUE ~ NA_integer_  # Uncertain cases - let XGBoost decide
    )
  )

# Training set: labeled examples only
train_data <- xgb_features %>%
  filter(!is.na(label))

cat("XGBoost training data:\n")
cat("  Positive examples (matches):", sum(train_data$label == 1), "\n")
cat("  Negative examples (non-matches):", sum(train_data$label == 0), "\n")

# Only proceed if we have enough training data
if (sum(train_data$label == 1) >= 50 && sum(train_data$label == 0) >= 50) {

  # Feature matrix
  feature_cols <- c("jw_full", "jw_first", "lv_full", "lv_first",
                    "cosine_full", "jaccard_full",
                    "qgram_full", "qgram_first",
                    "len_diff", "len_ratio", "word_diff",
                    "first_letter_match", "common_prefix", "common_suffix",
                    "exact_first_match", "exact_full_match")

  X_train <- as.matrix(train_data[, feature_cols])
  y_train <- train_data$label

  # --------------------------------------------------------------------------
  # 4B.3 TRAIN XGBOOST MODEL
  # --------------------------------------------------------------------------

  # XGBoost parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5
  )

  # Create DMatrix for training
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)

  # Cross-validation to find optimal number of rounds
  set.seed(42)
  cv_result <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 200,
    nfold = 5,
    early_stopping_rounds = 20,
    print_every_n = 50,
    verbosity = 0
  )

  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || best_nrounds < 1) best_nrounds <- 100
  cat("  Best XGBoost rounds:", best_nrounds, "\n")
  cat("  CV AUC:", round(max(cv_result$evaluation_log$test_auc_mean), 4), "\n")

  # Train final model using xgb.train (more stable API)
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    watchlist = list(train = dtrain),
    print_every_n = 50,
    verbosity = 0
  )

  # Feature importance
  importance <- xgb.importance(feature_names = feature_cols, model = xgb_model)
  cat("\nXGBoost feature importance (top 10):\n")
  print(head(importance, 10))

  # --------------------------------------------------------------------------
  # 4B.4 PREDICT ON ALL CANDIDATES
  # --------------------------------------------------------------------------

  X_all <- as.matrix(xgb_features[, feature_cols])
  xgb_features$xgb_score <- predict(xgb_model, X_all)

  # --------------------------------------------------------------------------
  # 4B.5 SELECT BEST XGBOOST MATCHES
  # --------------------------------------------------------------------------

  xgb_best_matches <- xgb_features %>%
    group_by(row_id) %>%
    arrange(desc(xgb_score)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      xgb_quality = case_when(
        xgb_score >= 0.90 ~ "Excellent",
        xgb_score >= 0.70 ~ "Good",
        xgb_score >= 0.50 ~ "Fair",
        xgb_score >= 0.30 ~ "Poor",
        TRUE              ~ "Very Poor"
      )
    )

  cat("\n========== XGBOOST MATCH SUMMARY ==========\n")
  cat("XGBoost score distribution:\n")
  cat("  Mean:", round(mean(xgb_best_matches$xgb_score), 3), "\n")
  cat("  Median:", round(median(xgb_best_matches$xgb_score), 3), "\n")
  print(quantile(xgb_best_matches$xgb_score, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

  cat("\nXGBoost match quality:\n")
  print(table(xgb_best_matches$xgb_quality))

  # Good matches (score >= 0.50)
  xgb_good <- xgb_best_matches %>% filter(xgb_score >= 0.50)
  cat("\nXGBoost good matches (>= 0.50):", nrow(xgb_good),
      "(", round(nrow(xgb_good) / nrow(vt_adults) * 100, 1), "%)\n")

  # --------------------------------------------------------------------------
  # 4B.6 COMPARE JW AND XGBOOST METHODS
  # --------------------------------------------------------------------------

  cat("\n========== METHOD COMPARISON: JW vs XGBoost ==========\n")

  comparison <- xgb_best_matches %>%
    select(row_id, vt_surname, vt_name, census_id, name_raw,
           jw_score = match_score, xgb_score) %>%
    mutate(
      jw_good  = jw_score >= 0.70,
      xgb_good = xgb_score >= 0.50,
      agreement = jw_good == xgb_good
    )

  cat("\nAgreement between methods:\n")
  cat("  Both agree match:", sum(comparison$jw_good & comparison$xgb_good), "\n")
  cat("  Both agree non-match:", sum(!comparison$jw_good & !comparison$xgb_good), "\n")
  cat("  JW only:", sum(comparison$jw_good & !comparison$xgb_good), "\n")
  cat("  XGBoost only:", sum(!comparison$jw_good & comparison$xgb_good), "\n")
  cat("  Agreement rate:", round(mean(comparison$agreement) * 100, 1), "%\n")

  # Correlation between scores
  score_cor <- cor(comparison$jw_score, comparison$xgb_score, use = "complete.obs")
  cat("  Score correlation:", round(score_cor, 3), "\n")

  # Cases where methods disagree (for inspection)
  disagreements <- comparison %>%
    filter(!agreement) %>%
    arrange(desc(abs(jw_score - xgb_score)))

  cat("\nSample disagreements (JW says match, XGBoost says no):\n")
  disagreements %>%
    filter(jw_good & !xgb_good) %>%
    select(vt_surname, vt_name, name_raw, jw_score, xgb_score) %>%
    head(10) %>%
    print()

  cat("\nSample disagreements (XGBoost says match, JW says no):\n")
  disagreements %>%
    filter(!jw_good & xgb_good) %>%
    select(vt_surname, vt_name, name_raw, jw_score, xgb_score) %>%
    head(10) %>%
    print()

  # --------------------------------------------------------------------------
  # 4B.7 CREATE ENSEMBLE SCORE
  # --------------------------------------------------------------------------

  # Combine JW and XGBoost into an ensemble
  xgb_best_matches <- xgb_best_matches %>%
    mutate(
      ensemble_score = 0.5 * match_score + 0.5 * xgb_score,
      ensemble_quality = case_when(
        ensemble_score >= 0.80 ~ "Excellent",
        ensemble_score >= 0.65 ~ "Good",
        ensemble_score >= 0.50 ~ "Fair",
        TRUE                   ~ "Poor"
      )
    )

  cat("\nEnsemble (JW + XGBoost) match quality:\n")
  print(table(xgb_best_matches$ensemble_quality))

  ensemble_good <- xgb_best_matches %>% filter(ensemble_score >= 0.50)
  cat("Ensemble good matches (>= 0.50):", nrow(ensemble_good),
      "(", round(nrow(ensemble_good) / nrow(vt_adults) * 100, 1), "%)\n")

  # --------------------------------------------------------------------------
  # 4B.8 PLOT COMPARISON
  # --------------------------------------------------------------------------

  p_compare <- ggplot(xgb_best_matches, aes(x = match_score, y = xgb_score)) +
    geom_point(alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#AAAAAA") +
    geom_hline(yintercept = 0.50, linetype = "dotted", color = "#5C2346") +
    geom_vline(xintercept = 0.70, linetype = "dotted", color = "#5C2346") +
    labs(x = "Jaro-Winkler Score", y = "XGBoost Score") +
    theme_leap() +
    annotate("text", x = 0.15, y = 0.95, label = paste("r =", round(score_cor, 3)))

  print(p_compare)
  fig_file <- next_fig("jw_vs_xgboost_comparison.png")
  save_leap_fig(fig_file, p_compare, width = 8, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # Save XGBoost results
  xgb_matches_final <- xgb_best_matches %>%
    filter(xgb_score >= 0.50) %>%
    select(row_id, vt_surname, vt_name, census_id, name_raw, search_district,
           jw_score = match_score, xgb_score, ensemble_score)

  write.csv(xgb_matches_final, "output/tables/voortrekker_matches_xgboost.csv", row.names = FALSE)

  # Store for later comparison in analysis
  xgb_matched_ids <- xgb_matches_final$census_id

} else {
  cat("Insufficient training data for XGBoost. Skipping ML matching.\n")
  xgb_matched_ids <- NULL
}

} else {
  # XGBoost comparison skipped
  xgb_matched_ids <- NULL
}  # End of RUN_XGBOOST_COMPARISON block


# >>> LINKAGE BEGIN
# The linkage (code/linkage.R) supplies scores, thresholds and proposals. The
# spouse-assisted and wife-blind classifiers are fitted on the same training
# labels; the final links are the reviewed decisions in data/linkage/link_decisions.csv.
source("code/linkage.R")
linkage_dir <- "output/tables/linkage"
vt_spouses <- vt_spouse_table()
candidates <- spouse_evidence(candidates, vt_spouses)
fit_spouse <- fit_linkage(candidates, training_pairs, spouse = TRUE, outdir = linkage_dir, tag = "spouse")
prop_spouse <- propose_links(candidates, fit_spouse, spouse = TRUE)
fit_blind <- fit_linkage(candidates, training_pairs, spouse = FALSE, outdir = linkage_dir, tag = "blind")
prop_blind <- propose_links(candidates, fit_blind, spouse = FALSE)

# Variable importance of the linkage classifier: the run's
# own classifier (spouse-assisted, or wife-blind when VT_LINKAGE = "blind").
run_fit <- if (Sys.getenv("VT_LINKAGE", "spouse") == "blind") fit_blind else fit_spouse
importance_plot_data <- run_fit$diag$importance %>%
  arrange(desc(gini)) %>% head(15) %>%
  mutate(variable = factor(variable, levels = rev(variable)))
p_rf_importance <- ggplot(importance_plot_data, aes(x = gini, y = variable)) +
  geom_bar(stat = "identity", fill = "#5C2346") +
  labs(x = "Mean Decrease in Gini", y = "") +
  theme_leap()
if (!exists("fig_rf_importance_file")) fig_rf_importance_file <- next_fig("rf_variable_importance.png")
save_leap_fig(fig_rf_importance_file, p_rf_importance, width = 8, height = 6, dpi = 300)

# Cross-district pass: persons without an in-district proposal, against male
# heads outside their search districts, exact surname; accepted only with
# spouse agreement and score >= T + d1 (rule in accept_rule()).
# Every eligible genealogy person without an in-district proposal, including
# persons with no in-district candidate at all.
xc_unlinked <- setdiff(unique(vt_expanded$row_id), prop_spouse$top$row_id[prop_spouse$top$status == "proposed"])
xc_vt <- vt_expanded %>% filter(row_id %in% xc_unlinked, !is.na(vt_surname_std))
xc_searched <- xc_vt %>% distinct(row_id, search_district)
xc <- xc_vt %>% group_by(row_id) %>% slice(1) %>% ungroup() %>% select(-search_district) %>%
  inner_join(census_cand %>% rename(search_district = district),
             by = c("vt_surname_std" = "census_surname_std"), relationship = "many-to-many") %>%
  mutate(census_surname_std = vt_surname_std) %>%
  anti_join(xc_searched, by = c("row_id", "search_district"))
if (nrow(xc)) {
  xc <- build_pair_features(xc) %>% mutate(jw_surname = 1, soundex_match = TRUE, block_type = "cross_district",
                                            is_primary_district = FALSE)
  xc <- spouse_evidence(xc, vt_spouses)
  xc_prop <- propose_links(xc, fit_spouse, spouse = TRUE)
  xc_top <- xc_prop$top %>% filter(status == "proposed")
  prop_spouse$top <- bind_rows(prop_spouse$top %>% filter(!row_id %in% xc_top$row_id), xc_top)
  prop_spouse$scored <- bind_rows(prop_spouse$scored, xc_prop$scored)
  prop_spouse$review <- bind_rows(prop_spouse$review, xc_prop$review)
  candidates <- bind_rows(candidates, xc %>% select(any_of(names(candidates))))
}
write.csv(prop_spouse$top %>% select(row_id, census_id, search_district, block_type, evidence_state, score, threshold,
                                 status, competing, is_primary_district),
          file.path(linkage_dir, "proposals_spouse.csv"), row.names = FALSE)
# All review-eligible pairs (any candidate, not only each person's top one).
write.csv(prop_spouse$review %>% select(row_id, census_id, search_district, block_type, evidence_state, score, threshold,
                                    status, is_primary_district),
          file.path(linkage_dir, "review_pairs_spouse.csv"), row.names = FALSE)
write.csv(prop_blind$top %>% select(row_id, census_id, search_district, block_type, evidence_state, score, threshold,
                                    status, competing, is_primary_district),
          file.path(linkage_dir, "proposals_blind.csv"), row.names = FALSE)
cat("Linkage proposals:", sum(prop_spouse$top$status == "proposed"), "spouse-assisted,",
    sum(prop_blind$top$status == "proposed"), "wife-blind; cross-district added:",
    if (exists("xc_top")) nrow(xc_top) else 0, "\n")
# Features and scores for any (row_id, census_id) pair, e.g. a decided link that
# is not among the generated candidates. Same feature code and spouse evidence.
make_pair_rows <- function(pairs) {
  cen <- all_districts %>% filter(census_id %in% pairs$census_id) %>%
    select(census_id, census_surname_std, census_first_std, census_first_only, district, name_raw, record_nr,
           census_wife_surname_std, census_wife_first_std, census_wife_first_only)
  rows <- pairs %>% select(row_id, census_id) %>% left_join(cen, by = "census_id") %>%
    left_join(vt_expanded %>% group_by(row_id) %>% slice(1) %>% ungroup() %>% select(-search_district), by = "row_id") %>%
    rename(search_district = district)
  rows <- build_pair_features(rows) %>%
    mutate(jw_surname = 1 - stringdist(surname_core(vt_surname_std), surname_core(census_surname_std), method = "jw", p = 0.1),
           soundex_match = stringdist::phonetic(surname_core(vt_surname_std)) == stringdist::phonetic(surname_core(census_surname_std)),
           block_type = "decided_pair")
  rows <- spouse_evidence(rows, vt_spouses)
  sc <- function(fit) predict(fit$model, as.data.frame(rows[, fit$diag$features]) %>%
                                mutate(across(everything(), ~ coalesce(as.numeric(.), 0))), type = "prob")[, "1"]
  rows %>% mutate(score_spouse = sc(fit_spouse), score_blind = sc(fit_blind))
}
# Downstream code reads rf_score, cv_export, best_idx, train_data and feature_cols:
# point them at the linkage (the wife-blind score in the wife-blind run).
linkage_scores <- function(p, nm) p$scored %>% distinct(row_id, census_id, .keep_all = TRUE) %>%
  select(row_id, census_id, !!nm := score)
candidates <- candidates %>% select(-any_of(c("score_spouse", "score_blind"))) %>%
  left_join(linkage_scores(prop_spouse, "score_spouse"), by = c("row_id", "census_id")) %>%
  left_join(linkage_scores(prop_blind, "score_blind"), by = c("row_id", "census_id")) %>%
  mutate(rf_score = coalesce(if (Sys.getenv("VT_LINKAGE", "spouse") == "blind") score_blind else score_spouse, 0))
cv_export <- fit_spouse$diag$cv %>% mutate(f1 = ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0))
best_idx <- which(abs(cv_export$threshold - fit_spouse$diag$T) < 1e-9)
feature_cols <- fit_spouse$diag$features
train_data <- candidates %>% select(-any_of("label")) %>%
  inner_join(fit_spouse$labelled %>% select(row_id, census_id, label), by = c("row_id", "census_id"))
candidates$label <- train_data$label[match(paste(candidates$row_id, candidates$census_id),
                                           paste(train_data$row_id, train_data$census_id))]
candidates_for_analysis <- candidates   # Part 15 ablation reads this copy
# >>> LINKAGE END

# Checkpoint: repaired linkage proposals and exact-pair history for review.
saveRDS(list(candidates = candidates, best_matches = best_matches,
             vt = vt, vt_adults = vt_adults, all_districts = all_districts,
             train_data = train_data, feature_cols = feature_cols),
        "output/tables/linkage_review_checkpoint.rds")
write.csv(best_matches, "output/tables/linkage_proposals.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(candidates %>% group_by(row_id) %>% arrange(desc(rf_score), census_id) %>% slice(1) %>% ungroup(),
          "output/tables/top_candidates.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(all_districts, "output/tables/census_after_source_corrections.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
if (TRUE) {
  # VT_LINKAGE selects the link set:
  #   spouse (default)  the final link decisions, data/linkage/link_decisions.csv (md5-pinned);
  #   blind             wife-blind classifier proposals, taken without review.
  linkage_mode <- Sys.getenv("VT_LINKAGE", "spouse")
  cat("Final link set:", linkage_mode, "\n")
  if (linkage_mode == "blind") {
    selected <- prop_blind$top %>% filter(status == "proposed") %>%
      group_by(census_id) %>% arrange(desc(score), row_id) %>% slice(1) %>% ungroup() %>%
      transmute(row_id, census_id, final_quality = "Classifier (wife-blind)", basis = "wife-blind proposal",
                identity_ambiguous = FALSE)
  } else {
    decision_file <- "data/linkage/link_decisions.csv"
    md5_file <- "data/linkage/link_decisions_md5.txt"
    stopifnot(file.exists(decision_file),
      unname(tools::md5sum(decision_file)) == readLines(md5_file, warn=FALSE))
    decisions <- read.csv(decision_file, fileEncoding="UTF-8-BOM")
    selected <- decisions %>% filter(decision == "retain") %>%
      mutate(identity_ambiguous=tolower(as.character(identity_ambiguous)) == "true")
  }
  # candidate rows first (rows from the preliminary RF would carry its
  # scores and no evidence state); decided pairs that are not
  # candidates get features and scores from make_pair_rows().
  pool <- bind_rows(candidates, best_matches) %>% distinct(row_id, census_id, .keep_all=TRUE)
  missing_pairs <- selected %>% anti_join(candidates, by = c("row_id", "census_id"))
  if (nrow(missing_pairs)) {
    extra <- make_pair_rows(missing_pairs) %>%
      mutate(rf_score = if (linkage_mode == "blind") score_blind else score_spouse)
    pool <- bind_rows(pool %>% anti_join(missing_pairs, by = c("row_id", "census_id")), extra)
    cat("  Decided pairs outside the candidate set, featured separately:", nrow(missing_pairs), "\n")
  }
  best_matches <- pool %>% select(-any_of(c("final_quality", "basis", "identity_ambiguous"))) %>%
    inner_join(selected %>% select(row_id, census_id, final_quality, basis, identity_ambiguous),
    by=c("row_id", "census_id"), relationship="one-to-one") %>%
    mutate(match_score=rf_score, match_quality=final_quality,
      # wife corroboration = spouse-evidence state "agrees" (the linkage).
      wife_corroborated = evidence_state %in% "agrees",
      wife_matched=wife_corroborated, district_matched=is_primary_district,
      wife_info_used=both_have_wife == 1, wife_helped=wife_info_used & wife_matched) %>%
    group_by(census_id) %>% mutate(n_vt_per_census=n()) %>% ungroup()
  stopifnot(nrow(best_matches)==nrow(selected), !anyDuplicated(best_matches$row_id))
  quarantine <- read.csv("data/inputs/quarantined_households.csv")
  all_districts <- all_districts %>% filter(!census_id %in% quarantine$census_id)
  # second census records of linked trekker families (men who moved
  # between returns, or double entries) are not controls.
  dup_file <- "data/linkage/duplicate_households.csv"
  if (file.exists(dup_file)) {
    dup_ids <- read.csv(dup_file)$excluded_census_id
    if (linkage_mode == "spouse") stopifnot(!any(best_matches$census_id %in% dup_ids))
    dup_ids <- setdiff(dup_ids, best_matches$census_id)   # wife-blind run: drop only if unlinked
    all_districts <- all_districts %>% filter(!census_id %in% dup_ids)
    cat("  Duplicate records of trekker households excluded from controls:", length(dup_ids), "\n")
  }
  stopifnot(all(best_matches$census_id %in% all_districts$census_id))
  write.csv(best_matches %>% filter(n_vt_per_census > 1),
    "output/tables/unresolved_person_collisions.csv", row.names=FALSE)
  # Exact identity is required for assigning a household an age, leader, or date.
  # Unresolved competing head identities still identify household membership once.
  write.csv(best_matches %>% count(match_quality, n_vt_per_census),
    "output/tables/final_link_unit_counts.csv", row.names=FALSE)
}
if (!"identity_ambiguous" %in% names(best_matches)) best_matches$identity_ambiguous <- FALSE
best_matches <- best_matches %>% group_by(census_id) %>% mutate(n_vt_per_census=n()) %>% ungroup()
# households whose link has spouse agreement (the linkage evidence state).
spouse_agree_ids <- unique(best_matches$census_id[best_matches$wife_corroborated %in% TRUE])
best_matches$enumeration_district <- all_districts$district[match(best_matches$census_id, all_districts$census_id)]
# Baptism dates at/after recorded marriage cannot stand for infant birth dates.
# Main eligibility is preserved; these three records carry no usable age proxy.
vt_adults$birth_yr[vt_adults$row_id %in% c(24,1081,1183)] <- NA_real_
best_matches$birth_yr[best_matches$row_id %in% c(24,1081,1183)] <- NA_real_

# ============================================================================
# PART 5: MATCH QUALITY ASSESSMENT
# ============================================================================

cat("\n========== MATCH QUALITY ASSESSMENT ==========\n")

# 5.1 Score distribution plot
p_scores <- ggplot(best_matches, aes(x = match_score)) +
  geom_histogram(bins = 30, fill = "#5C2346", color = "#FFFFFF") +
  # only the run's own classifier threshold is marked.
  geom_vline(xintercept = run_fit$diag$T, linetype = "dashed", color = "#AAAAAA") +
  labs(x = "Match Score (Random Forest)", y = "Count") +
  theme_leap()

print(p_scores)
fig_file <- next_fig("match_score_distribution.png")
save_leap_fig(fig_file, p_scores, width = 8, height = 5, dpi = 300)
# (output handled by save_leap_fig)

# 5.2 Match quality by Voortrekker district
match_by_district <- best_matches %>%
  count(census_districts, match_quality) %>%
  group_by(census_districts) %>%
  mutate(pct = n / sum(n) * 100)

cat("\nMatch quality by Voortrekker district:\n")
print(match_by_district %>% pivot_wider(names_from = match_quality, values_from = c(n, pct)))

# 5.3 Cross-validation with birth year
# For those with birth year, check if the matched person is plausibly alive in 1825
bv <- best_matches %>% filter(!is.na(birth_yr))
cat("\nBirth year validation (matched with birth year):", nrow(bv), "\n")
cat("  Born before 1810 (adult in 1825):", sum(bv$birth_yr < 1810), "\n")
cat("  Born before 1790 (35+ in 1825):", sum(bv$birth_yr < 1790), "\n")


# ============================================================================
# PART 6: CREATE ANALYSIS DATASET
# ============================================================================

# Mark Voortrekker status in the census
all_districts <- all_districts %>%
  mutate(
    is_voortrekker = census_id %in% best_matches$census_id
  )

cat("\nAnalysis dataset:\n")
cat("  Total households:", nrow(all_districts), "\n")
cat("  Voortrekker matches:", sum(all_districts$is_voortrekker), "\n")
cat("  Non-Voortrekkers:", sum(!all_districts$is_voortrekker), "\n")

# --------------------------------------------------------------------------
# Define outcome variables for all analyses
# --------------------------------------------------------------------------

outcome_vars <- c("horses", "cattle", "sheep", "goats", "pigs",
                  "total_slaves", "total_khoe",
                  "wheat_sown", "wheat_reaped",
                  "total_grain_sown", "total_grain_reaped",
                  "wine", "brandy",
                  "horses_saddle", "cattle_oxen", "cattle_breeding",
                  "sheep_breeding", "sheep_wethers",
                  # Household composition variables (age proxy)
                  "settler_men", "settler_women", "settler_sons", "settler_daughters",
                  "settler_children", "settler_adults", "household_size", "children_ratio")

# PCA-based wealth index
pca_vars <- c("horses", "cattle", "sheep", "goats", "pigs",
              "total_slaves", "total_khoe", "wheat_reaped", "wine")

pca_data <- all_districts[, pca_vars]
pca_complete <- complete.cases(pca_data)
pca_result <- prcomp(pca_data[pca_complete, ], scale. = TRUE)
# Positive average asset loading fixes the arbitrary sign before reading results.
pca_sign <- ifelse(sum(pca_result$rotation[, 1]) < 0, -1, 1)
pca_result$rotation[, 1] <- pca_sign * pca_result$rotation[, 1]
pca_result$x[, 1] <- pca_sign * pca_result$x[, 1]
all_districts$wealth_index <- NA_real_
all_districts$wealth_index[pca_complete] <- pca_result$x[, 1]
write.csv(all_districts %>% mutate(pca_complete = pca_complete) %>%
  count(district, is_voortrekker, pca_complete), "output/tables/pca_sample_coverage.csv", row.names = FALSE)

cat("\nPCA variance explained (first 3 components):\n")
print(summary(pca_result)$importance[, 1:3])

# --------------------------------------------------------------------------
# Wealth-index diagnostics
# PC1 loadings, variance explained, and the index's location/scale by VT
# status, so the manuscript can define the index in the main text and
# interpret coefficient magnitudes against the index SD.
# --------------------------------------------------------------------------
wealth_loadings <- data.frame(
  statistic = paste0("loading_", pca_vars),
  value = unname(pca_result$rotation[pca_vars, 1])
)
wealth_scalars <- data.frame(
  statistic = c("variance_explained_pc1",
                "index_mean_all", "index_sd_all",
                "index_min_all", "index_max_all",
                "index_mean_vt", "index_sd_vt",
                "index_mean_nonvt", "index_sd_nonvt",
                "cor_index_cattle"),
  value = c(summary(pca_result)$importance["Proportion of Variance", 1],
            mean(all_districts$wealth_index, na.rm = TRUE), sd(all_districts$wealth_index, na.rm = TRUE),
            min(all_districts$wealth_index, na.rm = TRUE), max(all_districts$wealth_index, na.rm = TRUE),
            mean(all_districts$wealth_index[all_districts$is_voortrekker], na.rm = TRUE),
            sd(all_districts$wealth_index[all_districts$is_voortrekker], na.rm = TRUE),
            mean(all_districts$wealth_index[!all_districts$is_voortrekker], na.rm = TRUE),
            sd(all_districts$wealth_index[!all_districts$is_voortrekker], na.rm = TRUE),
            cor(all_districts$wealth_index, all_districts$cattle,
                use = "complete.obs"))
)
wealth_diag <- bind_rows(wealth_loadings, wealth_scalars)
write.csv(wealth_diag, "output/tables/wealth_index_diagnostics.csv",
          row.names = FALSE)
cat("  Exported wealth_index_diagnostics.csv\n")
# Orientation check only (sign of PC1 is arbitrary but deterministic for
# fixed data; do NOT flip it, or every reported coefficient changes sign).
cat("  PC1 orientation: cor(index, cattle) =",
    round(cor(all_districts$wealth_index, all_districts$cattle,
              use = "complete.obs"), 3),
    "(positive = higher index means wealthier)\n")

# Also create a simple additive index (standardized)
all_districts <- all_districts %>%
  mutate(
    wealth_simple = scale(horses)[,1] + scale(cattle)[,1] + scale(sheep)[,1] +
                    scale(total_slaves)[,1] + scale(wheat_reaped)[,1] + scale(wine)[,1]
  )

# Create household composition variables (age proxy)
all_districts <- all_districts %>%
  mutate(
    settler_children = settler_sons + settler_daughters,
    settler_adults = settler_men + settler_women,
    household_size = settler_men + settler_women + settler_sons + settler_daughters,
    children_ratio = ifelse(household_size > 0, settler_children / household_size, 0)
  )

cat("\nHousehold composition summary:\n")
cat("  Mean household size:", round(mean(all_districts$household_size, na.rm = TRUE), 2), "\n")
cat("  Mean settler children:", round(mean(all_districts$settler_children, na.rm = TRUE), 2), "\n")

# ---------- A3: Export analysis dataset ----------
write.csv(all_districts, "output/tables/analysis_dataset.csv", row.names = FALSE)
cat("\n  Exported 'analysis_dataset.csv' (", nrow(all_districts), "rows,",
    sum(all_districts$is_voortrekker), "Voortrekkers)\n")

# ===========================================================================
# FREEZE CANONICAL ANALYSIS DATASET
# All subsequent analyses should start from this immutable object.
# ===========================================================================
analysis_dataset_main <- all_districts
cat("Frozen analysis_dataset_main:", nrow(analysis_dataset_main), "rows,",
    sum(analysis_dataset_main$is_voortrekker), "Voortrekkers\n")

# Assertions on canonical dataset
n_vt <- sum(analysis_dataset_main$is_voortrekker)
n_total <- nrow(analysis_dataset_main)
assert_count(n_vt, n_vt, paste("Matched Voortrekkers:", n_vt))
cat("  Canonical dataset: N =", n_total, ", N_VT =", n_vt, "\n")


# ============================================================================
# PART 7: ANALYSIS A - ROW-BASED NEAREST NEIGHBOR
# ============================================================================

cat("\n========== ANALYSIS A: ROW-BASED NEAREST NEIGHBOR ==========\n")

# For each matched Voortrekker, compare to the household immediately above
# and below them in the census records (within the same district)

all_districts <- all_districts %>%
  group_by(district) %>%
  arrange(source_row) %>%
  mutate(district_row = row_number()) %>%
  ungroup()

# Get Voortrekker records and their row neighbors
vt_records <- all_districts %>% filter(is_voortrekker)

neighbor_comparisons <- vt_records %>%
  select(census_id, district, district_row) %>%
  # Join to get neighbors
  left_join(
    all_districts %>%
      select(district, district_row, all_of(c(outcome_vars, "wealth_index", "wealth_simple",
                                               "is_voortrekker", "census_id"))) %>%
      rename_with(~ paste0("neighbor_", .), -c(district, district_row)),
    by = c("district"),
    relationship = "many-to-many"
  ) %>%
  # Keep only immediate neighbors (row above and below)
  filter(abs(district_row.x - district_row.y) == 1) %>%
  # Exclude if the neighbor is also a Voortrekker
  filter(!neighbor_is_voortrekker)

cat("Voortrekker-neighbor pairs (row-based):", nrow(neighbor_comparisons), "\n")

# Now create a paired dataset: Voortrekker vs. neighbor
# Get the Voortrekker's own economic data
vt_econ <- all_districts %>%
  filter(is_voortrekker) %>%
  select(census_id, district, all_of(c(outcome_vars, "wealth_index", "wealth_simple")))

# Get neighbor economic data
neighbor_econ <- neighbor_comparisons %>%
  group_by(census_id) %>%  # census_id of the Voortrekker
  summarise(
    across(starts_with("neighbor_") & !matches("is_voortrekker|census_id"),
           ~ mean(., na.rm = TRUE))
  )

# Rename neighbor columns
names(neighbor_econ) <- str_replace(names(neighbor_econ), "^neighbor_", "nb_")
names(neighbor_econ)[1] <- "census_id"

# Merge
row_comparison <- vt_econ %>%
  inner_join(neighbor_econ, by = "census_id")

# Calculate differences
for (v in outcome_vars) {
  nb_v <- paste0("nb_", v)
  diff_v <- paste0("diff_", v)
  if (nb_v %in% names(row_comparison)) {
    row_comparison[[diff_v]] <- row_comparison[[v]] - row_comparison[[nb_v]]
  }
}
row_comparison$diff_wealth_index <- row_comparison$wealth_index - row_comparison$nb_wealth_index
row_comparison$diff_wealth_simple <- row_comparison$wealth_simple - row_comparison$nb_wealth_simple

# T-tests on differences
cat("\nRow-Based Nearest Neighbor: Voortrekker minus Neighbor\n")
cat(sprintf("%-25s %8s %8s %8s %8s\n", "Variable", "VT Mean", "NB Mean", "Diff", "p-value"))
cat(paste(rep("-", 65), collapse = ""), "\n")

row_results <- data.frame()
for (v in c(outcome_vars, "wealth_index", "wealth_simple")) {
  nb_v <- paste0("nb_", v)
  if (nb_v %in% names(row_comparison) && sum(!is.na(row_comparison[[v]])) > 2) {
    tt <- tryCatch(
      t.test(row_comparison[[v]], row_comparison[[nb_v]], paired = TRUE),
      error = function(e) NULL
    )
    if (!is.null(tt)) {
      valid_pairs <- complete.cases(row_comparison[, c(v, nb_v)])
      vt_mean <- mean(row_comparison[[v]][valid_pairs])
      nb_mean <- mean(row_comparison[[nb_v]][valid_pairs])
      cat(sprintf("%-25s %8.2f %8.2f %8.2f %8.4f %s\n",
                  v, vt_mean, nb_mean, tt$estimate, tt$p.value,
                  ifelse(tt$p.value < 0.05, "*", "")))
      row_results <- rbind(row_results, data.frame(
        variable = v, vt_mean = vt_mean, nb_mean = nb_mean,
        difference = tt$estimate, p_value = tt$p.value,
        method = "Row Neighbor"
      ))
    }
  }
}


# ============================================================================
# PART 8: ANALYSIS B - SAME SURNAME, SAME DISTRICT
# ============================================================================

cat("\n========== ANALYSIS B: SAME SURNAME, SAME DISTRICT ==========\n")

# For each matched Voortrekker, find non-Voortrekker households with the
# same surname in the same district

surname_comparison <- all_districts %>%
  filter(!is.na(census_surname_std) & census_surname_std != "") %>%
  # Group by surname and district
  group_by(census_surname_std, district) %>%
  # Only keep groups that have at least one VT and one non-VT
  filter(any(is_voortrekker) & any(!is_voortrekker)) %>%
  # Calculate within-group means for VT and non-VT
  summarise(
    n_vt = sum(is_voortrekker),
    n_non_vt = sum(!is_voortrekker),
    across(all_of(c(outcome_vars, "wealth_index", "wealth_simple")),
           list(
             vt_mean = ~ mean(.[is_voortrekker], na.rm = TRUE),
             nonvt_mean = ~ mean(.[!is_voortrekker], na.rm = TRUE)
           )),
    .groups = "drop"
  )

cat("Surname-district groups with both VT and non-VT:", nrow(surname_comparison), "\n")
cat("Total VT covered:", sum(surname_comparison$n_vt), "\n")

# Test differences
cat("\nSame Surname, Same District: Voortrekker minus Same-Name Non-Voortrekker\n")
cat(sprintf("%-25s %8s %8s %8s %8s\n", "Variable", "VT Mean", "NonVT", "Diff", "p-value"))
cat(paste(rep("-", 65), collapse = ""), "\n")

surname_results <- data.frame()
for (v in c(outcome_vars, "wealth_index", "wealth_simple")) {
  vt_col <- paste0(v, "_vt_mean")
  nv_col <- paste0(v, "_nonvt_mean")
  if (vt_col %in% names(surname_comparison) && nv_col %in% names(surname_comparison)) {
    vt_vals <- surname_comparison[[vt_col]]
    nv_vals <- surname_comparison[[nv_col]]
    diffs <- vt_vals - nv_vals
    valid <- !is.na(diffs)
    if (sum(valid) > 2) {
      tt <- t.test(diffs[valid])
      cat(sprintf("%-25s %8.2f %8.2f %8.2f %8.4f %s\n",
                  v, mean(vt_vals[valid], na.rm = TRUE), mean(nv_vals[valid], na.rm = TRUE),
                  tt$estimate, tt$p.value,
                  ifelse(tt$p.value < 0.05, "*", "")))
      surname_results <- rbind(surname_results, data.frame(
        variable = v, vt_mean = mean(vt_vals[valid], na.rm = TRUE),
        nonvt_mean = mean(nv_vals[valid], na.rm = TRUE),
        difference = tt$estimate, p_value = tt$p.value,
        method = "Same Surname"
      ))
    }
  }
}


# ============================================================================
# PART 9: ANALYSIS C - DISTRICT-LEVEL REGRESSION
# ============================================================================

cat("\n========== ANALYSIS C: DISTRICT-LEVEL REGRESSION ==========\n")

# OLS with district fixed effects
# Voortrekker indicator on each outcome variable

reg_results <- list()
for (v in c(outcome_vars, "wealth_index", "wealth_simple")) {
  formula_str <- paste0(v, " ~ is_voortrekker + factor(district)")
  reg <- lm(as.formula(formula_str), data = all_districts)

  # Robust standard errors
  robust_se <- sqrt(diag(vcovHC(reg, type = "HC1")))

  reg_results[[v]] <- list(
    model = reg,
    robust_se = robust_se,
    coef = coef(reg)["is_voortrekkerTRUE"],
    se = robust_se["is_voortrekkerTRUE"],
    p = coeftest(reg, vcov = vcovHC(reg, type = "HC1"))["is_voortrekkerTRUE", 4]
  )
}

cat("\nDistrict Fixed Effects Regressions: Coefficient on Voortrekker Indicator\n")
cat(sprintf("%-25s %10s %10s %10s\n", "Outcome", "Coef", "Rob. SE", "p-value"))
cat(paste(rep("-", 60), collapse = ""), "\n")

reg_summary <- data.frame()
for (v in names(reg_results)) {
  r <- reg_results[[v]]
  cat(sprintf("%-25s %10.3f %10.3f %10.4f %s\n",
              v, r$coef, r$se, r$p, ifelse(r$p < 0.05, "*", "")))
  reg_summary <- rbind(reg_summary, data.frame(
    variable = v, difference = r$coef, robust_se = r$se, p_value = r$p,
    method = "District FE"
  ))
}

# Stargazer table for key regressions
key_vars <- c("cattle", "sheep", "horses", "total_slaves", "wheat_reaped",
              "wine", "wealth_index")
key_models <- lapply(key_vars, function(v) reg_results[[v]]$model)
key_se <- lapply(key_vars, function(v) reg_results[[v]]$robust_se)

stargazer(key_models, type = "text",
          se = key_se,
          dep.var.labels = key_vars,
          keep = "is_voortrekker",
          covariate.labels = "Voortrekker",
          out = "output/tables/regression_table.txt")


# ============================================================================
# PART 10: ANALYSIS D - WITHIN-DISTRICT QUANTILE COMPARISON
# ============================================================================

cat("\n========== ANALYSIS D: WITHIN-DISTRICT QUANTILE POSITION ==========\n")

# Where do Voortrekkers fall in the within-district wealth distribution?
all_districts <- all_districts %>%
  group_by(district) %>%
  mutate(
    wealth_percentile = percent_rank(wealth_index) * 100,
    cattle_percentile = percent_rank(cattle) * 100,
    sheep_percentile  = percent_rank(sheep) * 100,
    slave_percentile  = percent_rank(total_slaves) * 100,
    grain_percentile  = percent_rank(total_grain_reaped) * 100
  ) %>%
  ungroup()

cat("\nMean percentile rank by Voortrekker status:\n")
percentile_comp <- all_districts %>%
  group_by(is_voortrekker) %>%
  summarise(
    n = n(),
    wealth_pctl = mean(wealth_percentile, na.rm = TRUE),
    cattle_pctl = mean(cattle_percentile, na.rm = TRUE),
    sheep_pctl  = mean(sheep_percentile, na.rm = TRUE),
    slave_pctl  = mean(slave_percentile, na.rm = TRUE),
    grain_pctl  = mean(grain_percentile, na.rm = TRUE)
  )
print(percentile_comp)

# Wilcoxon rank-sum tests
cat("\nWilcoxon rank-sum tests (VT vs Non-VT percentile ranks):\n")
for (v in c("wealth_percentile", "cattle_percentile", "sheep_percentile",
            "slave_percentile", "grain_percentile")) {
  wt <- wilcox.test(
    all_districts[[v]][all_districts$is_voortrekker],
    all_districts[[v]][!all_districts$is_voortrekker]
  )
  cat(sprintf("  %-25s p = %.4f\n", v, wt$p.value))
}

# >>> FIG_WEALTH_DIST BEGIN
# Distribution of within-district wealth percentiles, as the percentage of each
# group's households in each 5-point bin (the groups differ twentyfold in size,
# so raw counts would hide the Voortrekker distribution).
wealth_pctl_bins <- all_districts %>%
  filter(!is.na(wealth_percentile)) %>%
  mutate(bin = cut(wealth_percentile, breaks = seq(0, 100, 5),
                   include.lowest = TRUE, right = FALSE),
         bin_mid = (as.integer(bin) - 0.5) * 5,
         group = ifelse(is_voortrekker, "Voortrekker", "Non-Voortrekker")) %>%
  count(group, bin_mid) %>%
  group_by(group) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()
stopifnot(all(abs(tapply(wealth_pctl_bins$pct, wealth_pctl_bins$group, sum) - 100) < 1e-8))
write.csv(wealth_pctl_bins, "output/tables/wealth_percentile_bins.csv", row.names = FALSE)
p_wealth <- ggplot(wealth_pctl_bins, aes(x = bin_mid, y = pct, fill = group)) +
  geom_col(position = position_dodge(width = 4.4), width = 4.2) +
  scale_fill_manual(values = c("Non-Voortrekker" = "#3D8EB9", "Voortrekker" = "#5C2346")) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  labs(x = "Wealth Percentile (within district)",
       y = "Percentage of Group's Households", fill = "") +
  theme_leap() +
  theme(legend.position = "bottom")
print(p_wealth)
fig_file <- next_fig("wealth_distribution_vt.png")
save_leap_fig(fig_file, p_wealth, width = 8, height = 5, dpi = 300)
# (output handled by save_leap_fig)
# >>> FIG_WEALTH_DIST END


# ============================================================================
# PART 11: ANALYSIS E - PROPENSITY SCORE MATCHING
# ============================================================================

cat("\n========== ANALYSIS E: PROPENSITY SCORE / CEM MATCHING ==========\n")

# Coarsened Exact Matching on district, then compare outcomes
# This is equivalent to exact matching on district + comparing within strata

# Using MatchIt for nearest-neighbor matching within district
# Match on district (exact) and propensity score estimated from observables

# First, prepare data for matching (only complete cases)
match_data <- all_districts %>%
  filter(!is.na(wealth_index)) %>%
  mutate(
    vt = as.integer(is_voortrekker),
    district_num = as.numeric(factor(district))
  )

# Exact matching on district
m_exact <- matchit(vt ~ district_num,
                   data = match_data,
                   method = "exact")

matched_exact <- match.data(m_exact)

# Compare outcomes in exact-matched sample
cat("\nExact matching on district:\n")
cat(sprintf("%-25s %10s %10s %10s %10s\n", "Variable", "VT Mean", "NonVT", "Diff", "p-value"))
cat(paste(rep("-", 65), collapse = ""), "\n")

exact_results <- data.frame()
for (v in c(outcome_vars, "wealth_index", "wealth_simple")) {
  vt_vals <- matched_exact[[v]][matched_exact$vt == 1]
  nv_vals <- matched_exact[[v]][matched_exact$vt == 0]
  vt_wts <- matched_exact$weights[matched_exact$vt == 1]
  nv_wts <- matched_exact$weights[matched_exact$vt == 0]

  reg <- tryCatch(
    lm(as.formula(paste(v, "~ vt")), data = matched_exact, weights = weights),
    error = function(e) NULL
  )
  if (!is.null(reg)) {
    robust <- tryCatch(coeftest(reg, vcov = vcovHC(reg, type = "HC1")), error = function(e) NULL)
    if (!is.null(robust) && "vt" %in% rownames(robust)) {
      vt_m <- weighted.mean(vt_vals, w = vt_wts, na.rm = TRUE)
      nv_m <- weighted.mean(nv_vals, w = nv_wts, na.rm = TRUE)
      diff_m <- robust["vt", 1]
      p_m <- robust["vt", 4]
    cat(sprintf("%-25s %10.2f %10.2f %10.2f %10.4f %s\n",
                v, vt_m, nv_m, diff_m, p_m,
                ifelse(p_m < 0.05, "*", "")))
      exact_results <- rbind(exact_results, data.frame(
        variable = v, vt_mean = vt_m, nonvt_mean = nv_m,
        difference = diff_m, p_value = p_m,
        method = "Exact Match (District)"
      ))
    }
  }
}

# --------------------------------------------------------------------------
# 11E: MATCHING ON DISTRICT + FAMILY SIZE (Number of Children)
# --------------------------------------------------------------------------

cat("\n\n--- District + Family Size Matching ---\n")
cat("Matching Voortrekker households to same-sized families in same district\n")
cat("(Exact integer number of children as proxy for household lifecycle stage)\n\n")

# Create total children variable if not already present
if (!"settler_children" %in% names(all_districts)) {
  all_districts <- all_districts %>%
    mutate(settler_children = settler_sons + settler_daughters)
}

# Use exact integer children counts for family-size matching
all_districts <- all_districts %>%
  mutate(children_exact = settler_children)

# Check distribution of children by Voortrekker status
cat("Distribution of children categories:\n")
child_dist <- all_districts %>%
  filter(!is.na(children_exact)) %>%
  count(is_voortrekker, children_exact) %>%
  pivot_wider(names_from = is_voortrekker, values_from = n, values_fill = 0) %>%
  rename(NonVT = `FALSE`, VT = `TRUE`)
print(child_dist)

# Method 1: Exact matching on district + children category using MatchIt
match_data_family <- all_districts %>%
  filter(!is.na(wealth_index) & !is.na(children_exact)) %>%
  mutate(
    vt = as.integer(is_voortrekker),
    district_num = as.numeric(factor(district)),
    children_num = as.numeric(factor(children_exact))
  )

cat("\nHouseholds available for family size matching:", nrow(match_data_family), "\n")
cat("  Voortrekkers:", sum(match_data_family$vt == 1), "\n")
cat("  Non-Voortrekkers:", sum(match_data_family$vt == 0), "\n")

# Exact matching on district AND exact number of children
m_family <- tryCatch({
  matchit(vt ~ district_num + children_num,
          data = match_data_family,
          method = "exact")
}, error = function(e) {
  cat("Note: Exact matching failed, trying CEM...\n")
  matchit(vt ~ district_num + children_num,
          data = match_data_family,
          method = "cem")
})

matched_family <- match.data(m_family)

cat("\nMatched sample size:", nrow(matched_family), "\n")
cat("  Matched Voortrekkers:", sum(matched_family$vt == 1), "\n")
cat("  Matched Non-Voortrekkers:", sum(matched_family$vt == 0), "\n")

# Compare outcomes in family-size-matched sample
cat("\nFamily Size + District Matching Results:\n")
cat(sprintf("%-25s %10s %10s %10s %10s\n", "Variable", "VT Mean", "NonVT", "Diff", "p-value"))
cat(paste(rep("-", 65), collapse = ""), "\n")

family_results <- data.frame()
for (v in c(outcome_vars, "wealth_index", "wealth_simple")) {
  vt_vals <- matched_family[[v]][matched_family$vt == 1]
  vt_wts <- matched_family$weights[matched_family$vt == 1]
  nv_vals <- matched_family[[v]][matched_family$vt == 0]
  nv_wts <- matched_family$weights[matched_family$vt == 0]

  # Weighted t-test consistent with the matching weights used for the means.
  # Weighted OLS with HC1 inference on the treatment coefficient uses
  # the same MatchIt weights as the displayed means.
  wdat <- rbind(
    data.frame(y = vt_vals, d = 1, w = vt_wts),
    data.frame(y = nv_vals, d = 0, w = nv_wts)
  )
  wdat <- wdat[is.finite(wdat$y) & is.finite(wdat$w) & wdat$w > 0, , drop = FALSE]
  reg_ft <- tryCatch(
    lm(y ~ d, data = wdat, weights = w),
    error = function(e) NULL
  )
  if (!is.null(reg_ft) && "d" %in% rownames(summary(reg_ft)$coefficients)) {
    pval <- coeftest(reg_ft, vcov = vcovHC(reg_ft, type = "HC1"))["d", 4]
    vt_m <- weighted.mean(vt_vals, w = vt_wts, na.rm = TRUE)
    nv_m <- weighted.mean(nv_vals, w = nv_wts, na.rm = TRUE)
    cat(sprintf("%-25s %10.2f %10.2f %10.2f %10.4f %s\n",
                v, vt_m, nv_m, vt_m - nv_m, pval,
                ifelse(pval < 0.05, "*", "")))
    family_results <- rbind(family_results, data.frame(
      variable = v, vt_mean = vt_m, nonvt_mean = nv_m,
      difference = vt_m - nv_m, p_value = pval,
      method = "Family Size Match"
    ))
  }
}

# Method 2: Regression with district + children fixed effects
cat("\n\nRegression with District + Children Category Fixed Effects:\n")
cat(sprintf("%-25s %12s %10s %10s\n", "Variable", "Coef", "SE", "p-value"))
cat(paste(rep("-", 60), collapse = ""), "\n")

family_fe_results <- data.frame()
for (v in c(outcome_vars, "wealth_index")) {
  formula <- as.formula(paste0(v, " ~ is_voortrekker + factor(district) + factor(children_exact)"))
  reg <- tryCatch(lm(formula, data = all_districts), error = function(e) NULL)

  if (!is.null(reg)) {
    robust <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))
    coef_val <- robust["is_voortrekkerTRUE", 1]
    se_val <- robust["is_voortrekkerTRUE", 2]
    p_val <- if (v == "settler_children") NA_real_ else robust["is_voortrekkerTRUE", 4]

    cat(sprintf("%-25s %12.3f %10.3f %10.4f %s\n",
                v, coef_val, se_val, p_val,
                ifelse(p_val < 0.05, "*", "")))

    family_fe_results <- rbind(family_fe_results, data.frame(
      variable = v,
      vt_mean = NA, nonvt_mean = NA,
      difference = coef_val,
      p_value = p_val,
      method = "District+Children FE"
    ))
  }
}

cat("\nInterpretation: These estimates compare Voortrekker families to non-Voortrekker\n")
cat("families in the SAME district with the SAME number of children.\n")
cat("This controls for lifecycle stage (similar age households).\n")


# ============================================================================
# PART 12: COMBINED RESULTS SUMMARY
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("         COMBINED RESULTS: VOORTREKKER SELECTION ANALYSIS        \n")
cat("================================================================\n\n")

# Combine all results
all_results <- bind_rows(row_results, surname_results, reg_summary, exact_results,
                          family_results, family_fe_results)

# Pivot to show all methods side by side for key variables
key_outcomes <- c("cattle", "sheep", "horses", "total_slaves", "total_khoe",
                  "wheat_reaped", "wine", "wealth_index")

summary_table <- all_results %>%
  filter(variable %in% key_outcomes) %>%
  select(variable, method, difference, p_value) %>%
  pivot_wider(names_from = method,
              values_from = c(difference, p_value),
              names_glue = "{method}_{.value}")

cat("\nKey outcomes across all methods:\n")
print(summary_table, width = Inf)

# --------------------------------------------------------------------------
# Calculate standard deviations for normalization (effect size calculation)
# --------------------------------------------------------------------------
var_sds <- all_districts %>%
  summarise(across(all_of(key_outcomes), ~sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sd")

cat("\nStandard deviations for effect size calculation:\n")
print(var_sds)

# Merge SDs into results and calculate standardized effect sizes
all_results_std <- all_results %>%
  filter(variable %in% key_outcomes) %>%
  left_join(var_sds, by = "variable") %>%
  mutate(
    effect_size = difference / sd  # Standardized difference (like Cohen's d)
  )

# Create standardized summary table
std_summary_table <- all_results_std %>%
  select(variable, method, effect_size, p_value) %>%
  pivot_wider(names_from = method,
              values_from = c(effect_size, p_value),
              names_glue = "{method}_{.value}")

cat("\nStandardized effect sizes (in SD units) across all methods:\n")
print(std_summary_table, width = Inf)

# --------------------------------------------------------------------------
# Create visualization with STANDARDIZED differences (effect sizes)
# --------------------------------------------------------------------------
plot_data <- all_results_std %>%
  mutate(
    significant = p_value < 0.05,
    variable = factor(variable, levels = key_outcomes)
  )

p_coefs <- ggplot(plot_data, aes(x = variable, y = effect_size, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Standardized Effect Size (SD units)",
       fill = "Method") +
  theme_leap() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = LEAP_CYCLE)

print(p_coefs)
fig_file <- next_fig("selection_coefficients_standardized.png")
save_leap_fig(fig_file, p_coefs, width = 10, height = 6, dpi = 300)
# (output handled by save_leap_fig)

# Also save the raw (unstandardized) version for reference
plot_data_raw <- all_results %>%
  filter(variable %in% key_outcomes) %>%
  mutate(
    significant = p_value < 0.05,
    variable = factor(variable, levels = key_outcomes)
  )

p_coefs_raw <- ggplot(plot_data_raw, aes(x = variable, y = difference, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Raw Difference (Voortrekker - Non-Voortrekker)",
       fill = "Method") +
  theme_leap() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = LEAP_CYCLE)

fig_file <- next_fig("selection_coefficients_raw.png")
save_leap_fig(fig_file, p_coefs_raw, width = 10, height = 6, dpi = 300)
# (output handled by save_leap_fig)


# ============================================================================
# PART 12A: HARMONISED MAIN RESULTS ACROSS THE FOUR DESIGNS
#
# One harmonised variable set -- now including cattle and sheep -- reported
# identically across (i) row-based nearest neighbour, (ii) district FE,
# (iii) exact district matching, (iv) district + family-size matching.
# Feeds the combined main-text table and the per-design appendix tables.
# ============================================================================

cat("\n========== PART 12A: HARMONISED MAIN RESULTS ==========\n")

harmon_spec <- tibble::tribble(
  ~variable,          ~label,                    ~group,
  "horses",           "Horses",                  "Livestock",
  "cattle",           "Cattle",                  "Livestock",
  "sheep",            "Sheep",                   "Livestock",
  "total_slaves",     "Slaves",                  "Labor",
  "total_khoe",       "Khoekhoe workers",        "Labor",
  "wheat_sown",       "Wheat sown (muids)",      "Agriculture",
  "wheat_reaped",     "Wheat reaped (muids)",    "Agriculture",
  "wine",             "Wine (leaguers)",         "Agriculture",
  "settler_men",      "Settler men",             "Household composition",
  "settler_women",    "Settler women",           "Household composition",
  "settler_children", "Settler children",        "Household composition",
  "household_size",   "Household size",          "Household composition",
  "children_ratio",   "Children ratio",          "Household composition",
  "wealth_index",     "Wealth index",            "Composite"
)

pick_design <- function(res) {
  out <- res %>%
    filter(variable %in% harmon_spec$variable) %>%
    select(any_of(c("variable", "vt_mean", "nb_mean", "nonvt_mean",
                    "difference", "robust_se", "p_value")))
  if ("nb_mean" %in% names(out) && !"nonvt_mean" %in% names(out)) {
    out <- out %>% rename(nonvt_mean = nb_mean)
  }
  out
}

harmon_nn     <- pick_design(row_results)    %>% rename_with(~ paste0("nn_", .),     -variable)
harmon_fe     <- pick_design(reg_summary)    %>% rename_with(~ paste0("fe_", .),     -variable)
harmon_exact  <- pick_design(exact_results)  %>% rename_with(~ paste0("exact_", .),  -variable)
harmon_family <- pick_design(family_results) %>% rename_with(~ paste0("family_", .), -variable)

main_results_harmonised <- harmon_spec %>%
  left_join(harmon_nn,     by = "variable") %>%
  left_join(harmon_fe,     by = "variable") %>%
  left_join(harmon_exact,  by = "variable") %>%
  left_join(harmon_family, by = "variable")

write.csv(main_results_harmonised,
          "output/tables/main_results_harmonised.csv", row.names = FALSE)
cat("  Exported main_results_harmonised.csv (",
    nrow(main_results_harmonised), "variables x 4 designs )\n")

# ---- Sample sizes for table notes ----
harmon_ns <- list(
  nn_pairs   = nrow(row_comparison),
  fe_total   = nrow(all_districts),
  fe_vt      = sum(all_districts$is_voortrekker),
  exact_vt   = sum(matched_exact$vt == 1),
  exact_ctrl = sum(matched_exact$vt == 0),
  fam_vt     = sum(matched_family$vt == 1),
  fam_ctrl   = sum(matched_family$vt == 0)
)

# ---- Combined main-text table: four estimate columns with stars ----
tex_rows <- character(0)
last_group <- ""
for (i in seq_len(nrow(main_results_harmonised))) {
  r <- main_results_harmonised[i, ]
  if (r$group != last_group) {
    tex_rows <- c(tex_rows,
                  sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", r$group))
    last_group <- r$group
  }
  est_line <- sprintf("%s & %s%s & %s%s & %s%s & %s%s \\\\",
                      r$label,
                      fmt_est(r$nn_difference),     stars_for(r$nn_p_value),
                      fmt_est(r$fe_difference),     stars_for(r$fe_p_value),
                      fmt_est(r$exact_difference),  stars_for(r$exact_p_value),
                      fmt_est(r$family_difference), stars_for(r$family_p_value))
  p_line <- sprintf(" & (%s) & (%s) & (%s) & (%s) \\\\",
                    fmt_p(r$nn_p_value), fmt_p(r$fe_p_value),
                    fmt_p(r$exact_p_value), fmt_p(r$family_p_value))
  tex_rows <- c(tex_rows, est_line, p_line)
}
write_tex_fragment(tex_rows, "output/tables/tex/tab_main_combined_body.tex")

# ---- Per-design appendix tables (VT mean / non-VT mean / diff / p) ----
design_fragment <- function(prefix, file, means = TRUE) {
  rows <- character(0)
  lg <- ""
  for (i in seq_len(nrow(main_results_harmonised))) {
    r <- main_results_harmonised[i, ]
    ncols <- if (means) 5 else 4
    if (r$group != lg) {
      rows <- c(rows, sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\",
                              ncols, r$group))
      lg <- r$group
    }
    if (means) {
      rows <- c(rows, sprintf("%s & %s & %s & %s & %s \\\\",
                              r$label,
                              fmt_est(r[[paste0(prefix, "_vt_mean")]]),
                              fmt_est(r[[paste0(prefix, "_nonvt_mean")]]),
                              fmt_est(r[[paste0(prefix, "_difference")]]),
                              fmt_p(r[[paste0(prefix, "_p_value")]])))
    } else {
      rows <- c(rows, sprintf("%s & %s & %s & %s \\\\",
                              r$label,
                              fmt_est(r[[paste0(prefix, "_difference")]]),
                              fmt_est(r[[paste0(prefix, "_robust_se")]]),
                              fmt_p(r[[paste0(prefix, "_p_value")]])))
    }
  }
  write_tex_fragment(rows, file)
}

design_fragment("nn",     "output/tables/tex/tab_design_nn_body.tex")
design_fragment("fe",     "output/tables/tex/tab_design_fe_body.tex", means = FALSE)
design_fragment("exact",  "output/tables/tex/tab_design_exact_body.tex")
design_fragment("family", "output/tables/tex/tab_design_family_body.tex")

# ---- Six-method appendix table ----
# tab:all_methods is generated as a fragment so that it always matches the
# pipeline output.
am_spec <- tibble::tribble(
  ~variable,          ~label,
  "horses_saddle",    "Horses (saddle)",
  "total_slaves",     "Slaves",
  "total_khoe",       "Khoekhoe workers",
  "wheat_sown",       "Wheat sown",
  "wine",             "Wine",
  "wealth_index",     "Wealth index",
  "settler_men",      "Settler men",
  "settler_children", "Settler children",
  "household_size",   "Household size",
  "children_ratio",   "Children ratio"
)
am_methods <- c("Row Neighbor", "Same Surname", "District FE",
                "Exact Match (District)", "Family Size Match",
                "District+Children FE")
tex_am <- character(0)
for (i in seq_len(nrow(am_spec))) {
  v <- am_spec$variable[i]
  est_cells <- p_cells <- character(0)
  for (m in am_methods) {
    r <- all_results %>% filter(variable == v, method == m)
    if (nrow(r) == 1) {
      est_cells <- c(est_cells,
                     paste0(fmt_est(r$difference, 2), stars_for(r$p_value)))
      # no p-value where the outcome is fixed by construction.
      p_cells <- c(p_cells, if (is.na(r$p_value)) "---" else paste0("(", fmt_p(r$p_value), ")"))
    } else {
      est_cells <- c(est_cells, ""); p_cells <- c(p_cells, "")
    }
  }
  tex_am <- c(tex_am,
              paste0(am_spec$label[i], " & ",
                     paste(est_cells, collapse = " & "), " \\\\"),
              paste0(" & ", paste(p_cells, collapse = " & "), " \\\\"))
}
write_tex_fragment(tex_am, "output/tables/tex/tab_all_methods_body.tex")

# ---- Descriptive statistics ----
# Mean, SD, min, max for the harmonised variable set over the full estimation
# sample, so magnitudes (e.g. the wealth-index coefficient) can be judged
# against the variables' own scale.
desc_rows <- character(0)
lg <- ""
for (i in seq_len(nrow(harmon_spec))) {
  v <- harmon_spec$variable[i]
  if (!v %in% names(all_districts)) next
  x <- all_districts[[v]]
  if (harmon_spec$group[i] != lg) {
    desc_rows <- c(desc_rows,
                   sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\",
                           harmon_spec$group[i]))
    lg <- harmon_spec$group[i]
  }
  desc_rows <- c(desc_rows,
    sprintf("%s & %s & %s & %s & %s \\\\",
            harmon_spec$label[i],
            fmt_est(mean(x, na.rm = TRUE)), fmt_est(sd(x, na.rm = TRUE)),
            fmt_est(min(x, na.rm = TRUE)),  fmt_est(max(x, na.rm = TRUE))))
}
desc_rows <- c(desc_rows,
  "\\midrule",
  sprintf("Households & \\multicolumn{4}{c}{%s} \\\\",
          format(nrow(all_districts), big.mark = ",")))
write_tex_fragment(desc_rows, "output/tables/tex/tab_descriptives_body.tex")

descriptives_csv <- harmon_spec %>%
  rowwise() %>%
  mutate(mean = mean(all_districts[[variable]], na.rm = TRUE),
         sd   = sd(all_districts[[variable]],   na.rm = TRUE),
         min  = min(all_districts[[variable]],  na.rm = TRUE),
         max  = max(all_districts[[variable]],  na.rm = TRUE)) %>%
  ungroup()
write.csv(descriptives_csv, "output/tables/descriptive_statistics.csv",
          row.names = FALSE)
cat("  Exported descriptive_statistics.csv\n")

cat("  Sample sizes: NN pairs =", harmon_ns$nn_pairs,
    "| FE N =", harmon_ns$fe_total, "( VT =", harmon_ns$fe_vt, ")",
    "| Exact VT/ctrl =", harmon_ns$exact_vt, "/", harmon_ns$exact_ctrl,
    "| Family VT/ctrl =", harmon_ns$fam_vt, "/", harmon_ns$fam_ctrl, "\n")


# ============================================================================
# PART 12B: HOUSEHOLD COMPOSITION ANALYSIS
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("      HOUSEHOLD COMPOSITION: AGE PROXY ANALYSIS                  \n")
cat("================================================================\n\n")

# Create household composition variables
all_districts <- all_districts %>%
  mutate(
    settler_children = settler_sons + settler_daughters,
    settler_adults = settler_men + settler_women,
    household_size = settler_men + settler_women + settler_sons + settler_daughters,
    children_ratio = ifelse(household_size > 0, settler_children / household_size, 0)
  )

household_vars <- c("settler_sons", "settler_daughters", "settler_children",
                    "settler_adults", "household_size", "children_ratio")

cat("Household composition variables created.\n")
cat("Mean household size:", round(mean(all_districts$household_size, na.rm = TRUE), 2), "\n")
cat("Mean children per household:", round(mean(all_districts$settler_children, na.rm = TRUE), 2), "\n")

# --------------------------------------------------------------------------
# 12B.1 ROW NEIGHBOR COMPARISON - HOUSEHOLD COMPOSITION
# --------------------------------------------------------------------------

cat("\n--- Row Neighbor Comparison: Household Composition ---\n")

hh_row_results <- data.frame()

for (v in household_vars) {
  nb_v <- paste0("nb_", v)
  if (v %in% names(row_comparison) && nb_v %in% names(row_comparison)) {
    valid_pairs <- row_comparison %>%
      filter(!is.na(.data[[v]]) & !is.na(.data[[nb_v]]))

    if (nrow(valid_pairs) > 10) {
      tt <- t.test(valid_pairs[[v]], valid_pairs[[nb_v]], paired = TRUE)
      vt_mean <- mean(valid_pairs[[v]], na.rm = TRUE)
      nb_mean <- mean(valid_pairs[[nb_v]], na.rm = TRUE)

      hh_row_results <- rbind(hh_row_results, data.frame(
        variable = v, vt_mean = vt_mean, nb_mean = nb_mean,
        difference = tt$estimate, p_value = tt$p.value,
        method = "Row Neighbor"
      ))
    }
  }
}

# --------------------------------------------------------------------------
# 12B.2 SAME SURNAME COMPARISON - HOUSEHOLD COMPOSITION
# --------------------------------------------------------------------------

cat("--- Same Surname Comparison: Household Composition ---\n")

hh_surname_results <- data.frame()

for (v in household_vars) {
  if (v %in% names(surname_comparison)) {
    valid <- surname_comparison %>%
      filter(!is.na(.data[[v]]) & !is.na(non_vt_mean))

    # Need to recalculate non_vt_mean for this variable
    surname_means <- all_districts %>%
      filter(!is_voortrekker) %>%
      group_by(census_surname_std, district) %>%
      summarise(non_vt_mean = mean(.data[[v]], na.rm = TRUE), .groups = "drop")

    vt_with_surname <- all_districts %>%
      filter(is_voortrekker) %>%
      select(census_id, census_surname_std, district, all_of(v)) %>%
      left_join(surname_means, by = c("census_surname_std", "district")) %>%
      filter(!is.na(non_vt_mean) & !is.na(.data[[v]]))

    if (nrow(vt_with_surname) > 10) {
      tt <- t.test(vt_with_surname[[v]], vt_with_surname$non_vt_mean, paired = TRUE)

      hh_surname_results <- rbind(hh_surname_results, data.frame(
        variable = v, vt_mean = mean(vt_with_surname[[v]], na.rm = TRUE),
        nonvt_mean = mean(vt_with_surname$non_vt_mean, na.rm = TRUE),
        difference = tt$estimate, p_value = tt$p.value,
        method = "Same Surname"
      ))
    }
  }
}

# --------------------------------------------------------------------------
# 12B.3 DISTRICT FE REGRESSION - HOUSEHOLD COMPOSITION
# --------------------------------------------------------------------------

cat("--- District FE Regressions: Household Composition ---\n")

hh_reg_results <- data.frame()

for (v in household_vars) {
  if (v %in% names(all_districts)) {
    formula <- as.formula(paste(v, "~ is_voortrekker + factor(district)"))
    reg <- lm(formula, data = all_districts)
    robust_se <- sqrt(diag(vcovHC(reg, type = "HC1")))["is_voortrekkerTRUE"]
    coef_val <- coef(reg)["is_voortrekkerTRUE"]
    p_val <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))["is_voortrekkerTRUE", 4]

    hh_reg_results <- rbind(hh_reg_results, data.frame(
      variable = v, difference = coef_val, robust_se = robust_se, p_value = p_val,
      method = "District FE"
    ))
  }
}

# --------------------------------------------------------------------------
# 12B.4 EXACT MATCH BY DISTRICT - HOUSEHOLD COMPOSITION
# --------------------------------------------------------------------------

cat("--- Exact Match (District): Household Composition ---\n")

hh_exact_results <- data.frame()

for (v in household_vars) {
  if (v %in% names(all_districts)) {
    # Get districts where we have Voortrekkers
    vt_districts <- all_districts %>%
      filter(is_voortrekker) %>%
      pull(district) %>%
      unique()

    comparison_data <- all_districts %>%
      filter(district %in% vt_districts)

    exact_comp_data <- matched_exact %>% filter(district %in% vt_districts)
    vt_vals <- exact_comp_data %>% filter(vt == 1) %>% pull(v)
    nonvt_vals <- exact_comp_data %>% filter(vt == 0) %>% pull(v)
    vt_wts <- exact_comp_data %>% filter(vt == 1) %>% pull(weights)
    nonvt_wts <- exact_comp_data %>% filter(vt == 0) %>% pull(weights)

    if (length(vt_vals) > 10 && length(nonvt_vals) > 10) {
      reg <- tryCatch(
        lm(as.formula(paste(v, "~ vt")), data = exact_comp_data, weights = weights),
        error = function(e) NULL
      )
      robust <- tryCatch(if (!is.null(reg)) coeftest(reg, vcov = vcovHC(reg, type = "HC1")) else NULL,
                         error = function(e) NULL)

      if (!is.null(robust) && "vt" %in% rownames(robust)) {
        hh_exact_results <- rbind(hh_exact_results, data.frame(
          variable = v, vt_mean = weighted.mean(vt_vals, vt_wts, na.rm = TRUE),
          nonvt_mean = weighted.mean(nonvt_vals, nonvt_wts, na.rm = TRUE),
          difference = robust["vt", 1],
          p_value = robust["vt", 4],
          method = "Exact Match (District)"
        ))
      }
    }
  }
}

# --------------------------------------------------------------------------
# 12B.5 FAMILY SIZE MATCHING - HOUSEHOLD COMPOSITION
# --------------------------------------------------------------------------

cat("--- Family Size + District Matching: Household Composition ---\n")

hh_family_results <- data.frame()

# Use the already-matched family data from section 11E
if (exists("matched_family") && nrow(matched_family) > 0) {
  for (v in household_vars) {
    if (v %in% names(matched_family)) {
      vt_vals <- matched_family[[v]][matched_family$vt == 1]
      vt_wts <- matched_family$weights[matched_family$vt == 1]
      nonvt_vals <- matched_family[[v]][matched_family$vt == 0]
      nonvt_wts <- matched_family$weights[matched_family$vt == 0]

      wdat <- data.frame(y = matched_family[[v]], d = matched_family$vt,
                         w = matched_family$weights)
      wdat <- wdat[is.finite(wdat$y) & is.finite(wdat$w) & wdat$w > 0, ]
      tt <- tryCatch({
        fit <- lm(y ~ d, data = wdat, weights = w)
        list(p.value = coeftest(fit, vcov = vcovHC(fit, type = "HC1"))["d", 4])
      }, error = function(e) NULL)
      if (!is.null(tt)) {
        vt_m <- weighted.mean(vt_vals, w = vt_wts, na.rm = TRUE)
        nv_m <- weighted.mean(nonvt_vals, w = nonvt_wts, na.rm = TRUE)

        hh_family_results <- rbind(hh_family_results, data.frame(
          variable = v, vt_mean = vt_m, nonvt_mean = nv_m,
          difference = vt_m - nv_m,
          p_value = tt$p.value,
          method = "Family Size Match"
        ))
      }
    }
  }
}

# Also run District + Children FE regression for household vars
cat("--- District + Children FE: Household Composition ---\n")

hh_family_fe_results <- data.frame()

for (v in household_vars) {
  if (v %in% names(all_districts) && "children_exact" %in% names(all_districts)) {
    formula <- as.formula(paste(v, "~ is_voortrekker + factor(district) + factor(children_exact)"))
    reg <- tryCatch(lm(formula, data = all_districts), error = function(e) NULL)

    if (!is.null(reg)) {
      robust <- tryCatch(coeftest(reg, vcov = vcovHC(reg, type = "HC1")), error = function(e) NULL)
      if (!is.null(robust) && "is_voortrekkerTRUE" %in% rownames(robust)) {
        coef_val <- robust["is_voortrekkerTRUE", 1]
        p_val <- if (v == "settler_children") NA_real_ else robust["is_voortrekkerTRUE", 4]

        hh_family_fe_results <- rbind(hh_family_fe_results, data.frame(
          variable = v, vt_mean = NA, nonvt_mean = NA,
          difference = coef_val,
          p_value = p_val,
          method = "District+Children FE"
        ))
      }
    }
  }
}

# --------------------------------------------------------------------------
# 12B.6 COMBINE AND VISUALIZE HOUSEHOLD RESULTS
# --------------------------------------------------------------------------

hh_all_results <- bind_rows(hh_row_results, hh_surname_results, hh_reg_results,
                             hh_exact_results, hh_family_results, hh_family_fe_results)

# Calculate standard deviations for normalization
hh_var_sds <- all_districts %>%
  summarise(across(any_of(household_vars), ~sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sd")

cat("\nHousehold variable standard deviations:\n")
print(hh_var_sds)

# Merge and calculate standardized effect sizes
hh_results_std <- hh_all_results %>%
  left_join(hh_var_sds, by = "variable") %>%
  mutate(
    effect_size = difference / sd,
    variable = factor(variable, levels = household_vars)
  )

# Print summary table
cat("\n\nHousehold Composition: Standardized Effect Sizes\n")
cat(sprintf("%-20s %10s %10s %10s %10s %10s\n", "Variable", "RowNeigh", "Surname", "DistFE", "Exact", "FamilyFE"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (v in household_vars) {
  row_val <- hh_results_std %>% filter(variable == v, method == "Row Neighbor") %>% pull(effect_size)
  sur_val <- hh_results_std %>% filter(variable == v, method == "Same Surname") %>% pull(effect_size)
  fe_val <- hh_results_std %>% filter(variable == v, method == "District FE") %>% pull(effect_size)
  ex_val <- hh_results_std %>% filter(variable == v, method == "Exact Match (District)") %>% pull(effect_size)
  fam_val <- hh_results_std %>% filter(variable == v, method == "District+Children FE") %>% pull(effect_size)

  cat(sprintf("%-20s %10s %10s %10s %10s %10s\n", v,
              ifelse(length(row_val) > 0, sprintf("%.3f", row_val), "NA"),
              ifelse(length(sur_val) > 0, sprintf("%.3f", sur_val), "NA"),
              ifelse(length(fe_val) > 0, sprintf("%.3f", fe_val), "NA"),
              ifelse(length(ex_val) > 0, sprintf("%.3f", ex_val), "NA"),
              ifelse(length(fam_val) > 0, sprintf("%.3f", fam_val), "NA")))
}

# Create household composition graph
p_household <- ggplot(hh_results_std, aes(x = variable, y = effect_size, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Standardized Effect Size (SD units)",
       fill = "Method") +
  theme_leap() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = LEAP_CYCLE)

print(p_household)
fig_file <- next_fig("household_composition.png")
save_leap_fig(fig_file, p_household, width = 10, height = 6, dpi = 300)
# (output handled by save_leap_fig)

# Figure already saved above


# ============================================================================
# PART 12B2: ROBUSTNESS - IS THE HOUSEHOLD SIZE FINDING GENUINE?
# ============================================================================
#
# Two methodological concerns about the household size result:
#
# CONCERN 1 (AGE/COHORT): We may disproportionately match younger VTs who
#   are in their child-rearing prime in 1825. Older census households (where
#   the man died before 1835 or was too old to trek) would not appear as VTs.
#   This creates a compositional difference unrelated to selection.
#
# CONCERN 2 (MATCHING BIAS): The RF matching assigns higher scores when wife
#   names corroborate the match. Households WITH a wife recorded are therefore
#   easier to match. These are also the households most likely to have children.
#   The larger household finding could be an artifact of the matching process.
#
# Below we test both concerns systematically.
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("  ROBUSTNESS: IS THE HOUSEHOLD SIZE FINDING GENUINE?             \n")
cat("================================================================\n\n")

# ==========================================================================
# TEST 1: AGE/COHORT EFFECTS
# ==========================================================================

cat("---------- TEST 1: AGE/COHORT EFFECTS ----------\n\n")

# --------------------------------------------------------------------------
# Test 1a: Age distribution of matched Voortrekkers
# --------------------------------------------------------------------------

cat("--- Test 1a: Age Distribution of Matched VTs ---\n\n")

# Join birth year from VT data onto matched census records
matched_vt_ids <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
  select(row_id, census_id, match_score, wife_corroborated) %>%
  distinct(census_id, .keep_all = TRUE)

vt_birth_data <- vt_adults %>%
  select(row_id, birth_yr, vt_surname, vt_name) %>%
  filter(!is.na(birth_yr))

matched_with_age <- matched_vt_ids %>%
  left_join(vt_birth_data, by = "row_id") %>%
  filter(!is.na(birth_yr)) %>%
  mutate(age_in_1825 = 1825 - birth_yr)

cat(sprintf("VTs with birth year data: %d of %d matched (%.1f%%)\n",
            nrow(matched_with_age), nrow(matched_vt_ids),
            nrow(matched_with_age) / nrow(matched_vt_ids) * 100))
cat(sprintf("Age in 1825: mean = %.1f, median = %.0f, range = %d-%d\n",
            mean(matched_with_age$age_in_1825),
            median(matched_with_age$age_in_1825),
            min(matched_with_age$age_in_1825),
            max(matched_with_age$age_in_1825)))

# Age distribution
cat("\nAge distribution in 1825:\n")
age_breaks <- c(0, 25, 35, 45, 55, 100)
age_labels <- c("15-24", "25-34", "35-44", "45-54", "55+")
matched_with_age$age_group <- cut(matched_with_age$age_in_1825,
                                   breaks = age_breaks, labels = age_labels,
                                   right = FALSE)
age_dist <- table(matched_with_age$age_group)
for (a in names(age_dist)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", a, age_dist[a],
              age_dist[a] / sum(age_dist) * 100))
}

# --------------------------------------------------------------------------
# Test 1b: Household size by VT age cohort
# --------------------------------------------------------------------------

cat("\n--- Test 1b: Household Size by VT Age Cohort ---\n\n")

# Join age data onto all_districts for matched VTs
all_districts_age <- all_districts %>%
  left_join(matched_with_age %>% select(census_id, birth_yr, age_in_1825, age_group),
            by = "census_id")

# Among VTs with age data, compare household size by age group
vt_by_age <- all_districts_age %>%
  filter(is_voortrekker & !is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_hh_size = mean(household_size, na.rm = TRUE),
    mean_children = mean(settler_children, na.rm = TRUE),
    mean_children_ratio = mean(children_ratio, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("%-10s %6s %12s %12s %15s\n",
            "Age group", "N", "HH size", "Children", "Children ratio"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in 1:nrow(vt_by_age)) {
  cat(sprintf("%-10s %6d %12.2f %12.2f %15.3f\n",
              as.character(vt_by_age$age_group[i]),
              vt_by_age$n[i], vt_by_age$mean_hh_size[i],
              vt_by_age$mean_children[i], vt_by_age$mean_children_ratio[i]))
}

# Non-VT mean for reference
non_vt_hh <- all_districts %>% filter(!is_voortrekker) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE),
            mean_ch = mean(settler_children, na.rm = TRUE))
cat(sprintf("\nNon-VT reference: HH size = %.2f, Children = %.2f\n",
            non_vt_hh$mean_hh, non_vt_hh$mean_ch))

# Test: Are VTs larger than non-VTs within EACH age cohort?
cat("\nVT - Non-VT difference by age cohort:\n")
cat("(If positive across all cohorts, age is not the driver)\n\n")

for (ag in levels(matched_with_age$age_group)) {
  vt_ag <- all_districts_age %>%
    filter(is_voortrekker & age_group == ag)
  if (nrow(vt_ag) >= 5) {
    diff_hh <- mean(vt_ag$household_size, na.rm = TRUE) - non_vt_hh$mean_hh
    diff_ch <- mean(vt_ag$settler_children, na.rm = TRUE) - non_vt_hh$mean_ch
    cat(sprintf("  %s (n=%d): HH size diff = %+.2f, Children diff = %+.2f\n",
                ag, nrow(vt_ag), diff_hh, diff_ch))
  }
}

# --------------------------------------------------------------------------
# Test 1c: Lifecycle-matched comparison
# --------------------------------------------------------------------------

cat("\n\n--- Test 1c: Lifecycle-Matched Comparison ---\n\n")
cat("Restricting to households with wife + at least 1 child (prime child-rearing).\n")
cat("This compares VTs to non-VTs at the same lifecycle stage.\n\n")

# Restrict to households likely in the same lifecycle stage
lifecycle_sample <- all_districts %>%
  filter(settler_women >= 1 & settler_children >= 1)

n_vt_lc <- sum(lifecycle_sample$is_voortrekker)
n_non_vt_lc <- sum(!lifecycle_sample$is_voortrekker)
cat(sprintf("Lifecycle-matched sample: %d VTs, %d non-VTs\n", n_vt_lc, n_non_vt_lc))

if (n_vt_lc >= 20) {
  lc_comparison <- lifecycle_sample %>%
    group_by(is_voortrekker) %>%
    summarise(
      n = n(),
      mean_hh_size = mean(household_size, na.rm = TRUE),
      mean_children = mean(settler_children, na.rm = TRUE),
      mean_children_ratio = mean(children_ratio, na.rm = TRUE),
      mean_settler_men = mean(settler_men, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("%-20s %10s %10s\n", "Variable", "VT", "Non-VT"))
  cat(paste(rep("-", 45), collapse = ""), "\n")
  cat(sprintf("%-20s %10d %10d\n", "N",
              lc_comparison$n[2], lc_comparison$n[1]))
  cat(sprintf("%-20s %10.2f %10.2f\n", "HH size",
              lc_comparison$mean_hh_size[2], lc_comparison$mean_hh_size[1]))
  cat(sprintf("%-20s %10.2f %10.2f\n", "Children",
              lc_comparison$mean_children[2], lc_comparison$mean_children[1]))
  cat(sprintf("%-20s %10.3f %10.3f\n", "Children ratio",
              lc_comparison$mean_children_ratio[2], lc_comparison$mean_children_ratio[1]))
  cat(sprintf("%-20s %10.2f %10.2f\n", "Settler men",
              lc_comparison$mean_settler_men[2], lc_comparison$mean_settler_men[1]))

  # Re-run regression on lifecycle-matched sample
  for (v in c("household_size", "settler_children", "children_ratio")) {
    reg_lc <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                  data = lifecycle_sample)
    robust_lc <- coeftest(reg_lc, vcov = vcovHC(reg_lc, type = "HC1"))
    cat(sprintf("\n  Lifecycle-matched regression: %s ~ VT + District FE\n", v))
    cat(sprintf("    Coef = %.3f, SE = %.3f, p = %.4f %s\n",
                robust_lc["is_voortrekkerTRUE", 1],
                robust_lc["is_voortrekkerTRUE", 2],
                robust_lc["is_voortrekkerTRUE", 4],
                ifelse(robust_lc["is_voortrekkerTRUE", 4] < 0.05, "*", "")))
  }
}

# ==========================================================================
# TEST 2: MATCHING BIAS (WIFE NAME SELECTION)
# ==========================================================================

cat("\n\n---------- TEST 2: MATCHING BIAS (WIFE NAME SELECTION) ----------\n\n")

# --------------------------------------------------------------------------
# Test 2a: Wife-matched vs male-only matched VTs
# --------------------------------------------------------------------------

cat("--- Test 2a: Wife-Matched vs Male-Only Matched VTs ---\n\n")

# Add wife_corroborated flag to census records
all_districts_match <- all_districts %>%
  left_join(
    best_matches %>%
      group_by(census_id) %>% summarise(wife_corroborated=any(wife_corroborated,na.rm=TRUE), wife_info_used=any(wife_info_used,na.rm=TRUE), .groups="drop"),
    by = "census_id"
  )

vt_wife <- all_districts_match %>% filter(is_voortrekker & wife_corroborated == TRUE)
vt_male <- all_districts_match %>% filter(is_voortrekker & (is.na(wife_corroborated) | wife_corroborated == FALSE))

cat(sprintf("Wife-corroborated matches: %d\n", nrow(vt_wife)))
cat(sprintf("Male-only matches: %d\n", nrow(vt_male)))

if (nrow(vt_wife) >= 10 & nrow(vt_male) >= 10) {
  cat(sprintf("\n%-20s %12s %12s %12s\n",
              "Variable", "Wife match", "Male-only", "Difference"))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  for (v in c("household_size", "settler_children", "children_ratio",
              "settler_men", "settler_women", "total_slaves", "cattle")) {
    wife_mean <- mean(vt_wife[[v]], na.rm = TRUE)
    male_mean <- mean(vt_male[[v]], na.rm = TRUE)
    diff_val <- wife_mean - male_mean

    # T-test between the two groups
    tt <- tryCatch(
      t.test(vt_wife[[v]], vt_male[[v]]),
      error = function(e) list(p.value = NA)
    )

    cat(sprintf("%-20s %12.2f %12.2f %+11.2f  (p=%.3f)\n",
                v, wife_mean, male_mean, diff_val,
                ifelse(is.na(tt$p.value), 1, tt$p.value)))
  }

  cat("\nInterpretation: If wife-matched VTs have MUCH larger households\n")
  cat("than male-only VTs, matching bias may inflate the household finding.\n")
} else {
  cat("Insufficient observations in one group for comparison.\n")
  cat(sprintf("Wife-matched: %d, Male-only: %d\n", nrow(vt_wife), nrow(vt_male)))
}

# --------------------------------------------------------------------------
# Test 2b: TREKKER LINKS WITHOUT SPOUSE AGREEMENT
# --------------------------------------------------------------------------

cat("\n\n--- Test 2b: Trekker Links Without Spouse Agreement ---\n\n")
cat("Trekker households linked without spouse agreement, against all controls.\n")
cat("If VTs still have larger households here, wife matching is not the driver.\n\n")

# wife_info_available marks trekker households whose link has spouse
# agreement; this sample is all controls plus trekker households linked
# without spouse agreement.
no_agree_sample <- all_districts %>%
  mutate(wife_info_available = census_id %in% spouse_agree_ids) %>%
  filter(!wife_info_available)

n_vt_nw <- sum(no_agree_sample$is_voortrekker)
n_non_vt_nw <- sum(!no_agree_sample$is_voortrekker)
cat(sprintf("Sample without spouse agreement: %d VTs, %d non-VTs, %d districts\n",
            n_vt_nw, n_non_vt_nw, n_distinct(no_agree_sample$district)))

if (n_vt_nw >= 15) {
  # Descriptive comparison
  nw_desc <- no_agree_sample %>%
    group_by(is_voortrekker) %>%
    summarise(
      n = n(),
      mean_hh_size = mean(household_size, na.rm = TRUE),
      mean_children = mean(settler_children, na.rm = TRUE),
      mean_children_ratio = mean(children_ratio, na.rm = TRUE),
      mean_settler_men = mean(settler_men, na.rm = TRUE),
      mean_settler_women = mean(settler_women, na.rm = TRUE),
      mean_slaves = mean(total_slaves, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("%-20s %10s %10s %12s\n", "Variable", "VT", "Non-VT", "Diff"))
  cat(paste(rep("-", 55), collapse = ""), "\n")
  for (v in c("mean_hh_size", "mean_children", "mean_children_ratio",
              "mean_settler_men", "mean_settler_women", "mean_slaves")) {
    vt_val <- nw_desc[[v]][nw_desc$is_voortrekker == TRUE]
    nv_val <- nw_desc[[v]][nw_desc$is_voortrekker == FALSE]
    cat(sprintf("%-20s %10.2f %10.2f %+11.2f\n",
                gsub("mean_", "", v), vt_val, nv_val, vt_val - nv_val))
  }

  # Formal regressions in links without spouse agreement
  cat("\nDistrict FE regressions (links without spouse agreement only):\n\n")

  nw_results <- data.frame()
  for (v in c("household_size", "settler_children", "children_ratio",
              "settler_men", "total_slaves", "cattle", "sheep")) {
    reg_nw <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                  data = no_agree_sample)
    robust_nw <- coeftest(reg_nw, vcov = vcovHC(reg_nw, type = "HC1"))

    coef_nw <- robust_nw["is_voortrekkerTRUE", 1]
    se_nw <- robust_nw["is_voortrekkerTRUE", 2]
    p_nw <- robust_nw["is_voortrekkerTRUE", 4]

    cat(sprintf("  %-20s: coef = %+.3f, SE = %.3f, p = %.4f %s\n",
                v, coef_nw, se_nw, p_nw,
                ifelse(p_nw < 0.05, "*", ifelse(p_nw < 0.10, ".", ""))))

    nw_results <- rbind(nw_results, data.frame(
      variable = v, coef = coef_nw, se = se_nw, p_value = p_nw,
      sample = "no_spouse_agreement"
    ))
  }

  # Compare to full-sample results
  cat("\nComparison: Full sample vs No spouse agreement\n\n")
  cat(sprintf("%-20s %15s %15s\n", "Variable", "Full sample", "No agreement"))
  cat(paste(rep("-", 55), collapse = ""), "\n")

  for (v in c("household_size", "settler_children", "children_ratio")) {
    reg_full <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                    data = all_districts)
    robust_full <- coeftest(reg_full, vcov = vcovHC(reg_full, type = "HC1"))

    coef_full <- robust_full["is_voortrekkerTRUE", 1]
    p_full <- robust_full["is_voortrekkerTRUE", 4]

    nw_row <- nw_results %>% filter(variable == v)

    cat(sprintf("%-20s %+.3f (p=%.3f) %+.3f (p=%.3f)\n",
                v, coef_full, p_full, nw_row$coef, nw_row$p_value))
  }

  cat("\nIf coefficients remain similar/positive in links without spouse agreement,\n")
  cat("the household size finding is NOT driven by wife matching bias.\n")

} else {
  cat("Insufficient VT matches in links without spouse agreement for analysis.\n")
}

# --------------------------------------------------------------------------
# Test 2c: Wife availability and household size in full census
# --------------------------------------------------------------------------

cat("\n\n--- Test 2c: Baseline - Wife Name Availability and HH Size ---\n\n")
cat("Among ALL census households, is wife data availability correlated\n")
cat("with household size? If yes, this is the mechanical channel.\n\n")

# In districts recording wives, does having a wife name recorded predict HH size?
wife_district_data <- all_districts %>%   # every district records wives

  mutate(has_wife_name = !is.na(census_wife_first_std) & census_wife_first_std != "")

if ("has_wife_name" %in% names(wife_district_data)) {
  wife_hh <- wife_district_data %>%
    group_by(has_wife_name) %>%
    summarise(
      n = n(),
      mean_hh_size = mean(household_size, na.rm = TRUE),
      mean_children = mean(settler_children, na.rm = TRUE),
      mean_settler_women = mean(settler_women, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("%-25s %10s %10s\n", "Metric", "Has wife", "No wife"))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  cat(sprintf("%-25s %10d %10d\n", "N",
              wife_hh$n[wife_hh$has_wife_name == TRUE],
              wife_hh$n[wife_hh$has_wife_name == FALSE]))
  cat(sprintf("%-25s %10.2f %10.2f\n", "HH size",
              wife_hh$mean_hh_size[wife_hh$has_wife_name == TRUE],
              wife_hh$mean_hh_size[wife_hh$has_wife_name == FALSE]))
  cat(sprintf("%-25s %10.2f %10.2f\n", "Children",
              wife_hh$mean_children[wife_hh$has_wife_name == TRUE],
              wife_hh$mean_children[wife_hh$has_wife_name == FALSE]))
  cat(sprintf("%-25s %10.2f %10.2f\n", "Settler women",
              wife_hh$mean_settler_women[wife_hh$has_wife_name == TRUE],
              wife_hh$mean_settler_women[wife_hh$has_wife_name == FALSE]))

  cat("\nThis shows the mechanical relationship: households with wife names\n")
  cat("recorded tend to have more women and thus more children. This is\n")
  cat("the channel through which matching bias could operate.\n")
}

# --------------------------------------------------------------------------
# Test 2d: Matching success and wife availability
# --------------------------------------------------------------------------

cat("\n\n--- Test 2d: Does Wife Availability Predict Matching Success? ---\n\n")

# Among all VTs, does having wife name data predict being matched?
vt_match_status <- vt_adults %>%
  mutate(
    matched = row_id %in% best_matches$row_id,
    has_wife = !is.na(wife_name) & wife_name != "",
    in_wife_district = TRUE   # every district records wives
  )

cat(sprintf("%-30s %10s %10s\n", "Group", "Match rate", "N"))
cat(paste(rep("-", 55), collapse = ""), "\n")

# By wife availability
for (hw in c(TRUE, FALSE)) {
  sub <- vt_match_status %>% filter(has_wife == hw)
  rate <- mean(sub$matched) * 100
  cat(sprintf("%-30s %9.1f%% %10d\n",
              ifelse(hw, "VT with wife name", "VT without wife name"),
              rate, nrow(sub)))
}

# By district type
for (wd in c(TRUE, FALSE)) {
  sub <- vt_match_status %>% filter(in_wife_district == wd)
  rate <- mean(sub$matched) * 100
  cat(sprintf("%-30s %9.1f%% %10d\n",
              ifelse(wd, "VT, district records wives", "VT, district records no wives"),
              rate, nrow(sub)))
}

# Formal test: logit of match success
match_logit <- glm(matched ~ has_wife + in_wife_district,
                    data = vt_match_status, family = binomial)
cat("\nLogit: P(matched) ~ has_wife + in_wife_district\n")
s_ml <- summary(match_logit)$coefficients
for (rn in rownames(s_ml)[-1]) {
  cat(sprintf("  %s: coef = %.3f, p = %.4f\n",
              rn, s_ml[rn, 1], s_ml[rn, 4]))
}

# --------------------------------------------------------------------------
# Test 2e: Matched vs unmatched VTs - balance check
# --------------------------------------------------------------------------

cat("\n\n--- Test 2e: Matched vs Unmatched VTs (Balance Check) ---\n\n")
cat("If unmatched VTs are systematically different, the matched sample\n")
cat("is not representative. Checking observable characteristics.\n\n")

# Compare matched vs unmatched VTs on available characteristics
vt_balance <- vt_match_status %>%
  filter(!is.na(birth_yr)) %>%
  group_by(matched) %>%
  summarise(
    n = n(),
    mean_birth_yr = mean(birth_yr, na.rm = TRUE),
    pct_has_wife = mean(has_wife, na.rm = TRUE) * 100,
    pct_wife_district = mean(in_wife_district, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat(sprintf("%-25s %12s %12s\n", "Characteristic", "Matched", "Unmatched"))
cat(paste(rep("-", 52), collapse = ""), "\n")
cat(sprintf("%-25s %12d %12d\n", "N",
            vt_balance$n[vt_balance$matched == TRUE],
            vt_balance$n[vt_balance$matched == FALSE]))
cat(sprintf("%-25s %12.1f %12.1f\n", "Mean birth year",
            vt_balance$mean_birth_yr[vt_balance$matched == TRUE],
            vt_balance$mean_birth_yr[vt_balance$matched == FALSE]))
cat(sprintf("%-25s %11.1f%% %11.1f%%\n", "Has wife name (%)",
            vt_balance$pct_has_wife[vt_balance$matched == TRUE],
            vt_balance$pct_has_wife[vt_balance$matched == FALSE]))
cat(sprintf("%-25s %11.1f%% %11.1f%%\n", "In district recording wives (%)",
            vt_balance$pct_wife_district[vt_balance$matched == TRUE],
            vt_balance$pct_wife_district[vt_balance$matched == FALSE]))

# ==========================================================================
# COMBINED SUMMARY: HOUSEHOLD SIZE ROBUSTNESS
# ==========================================================================

cat("\n\n")
cat("================================================================\n")
cat("  HOUSEHOLD SIZE ROBUSTNESS: SUMMARY                             \n")
cat("================================================================\n\n")

cat("CONCERN 1 (AGE/COHORT):\n")
if (exists("vt_by_age") && nrow(vt_by_age) >= 2) {
  # Check if VT advantage is consistent across age groups
  all_positive <- all(vt_by_age$mean_hh_size > non_vt_hh$mean_hh)
  cat(sprintf("  VTs have larger households in ALL age cohorts: %s\n",
              ifelse(all_positive, "YES - age is not the driver",
                     "NO - age may explain part of the effect")))
  cat(sprintf("  HH size advantage ranges from %.2f to %.2f across cohorts\n",
              min(vt_by_age$mean_hh_size) - non_vt_hh$mean_hh,
              max(vt_by_age$mean_hh_size) - non_vt_hh$mean_hh))
}

cat("\nCONCERN 2 (MATCHING BIAS):\n")
if (exists("nw_results") && nrow(nw_results) > 0) {
  nw_hh <- nw_results %>% filter(variable == "household_size")
  if (nrow(nw_hh) > 0) {
    cat(sprintf("  No-spouse-agreement HH size coef: %+.3f (p = %.4f)\n",
                nw_hh$coef, nw_hh$p_value))
    if (nw_hh$coef > 0 && nw_hh$p_value < 0.10) {
      cat("  RESULT: HH size effect SURVIVES in links without spouse agreement.\n")
      cat("  The finding is NOT an artifact of wife-name matching bias.\n")
    } else if (nw_hh$coef > 0 && nw_hh$p_value >= 0.10) {
      cat("  RESULT: HH size effect is POSITIVE but imprecise in links without spouse agreement.\n")
      cat("  Direction is consistent; loss of significance may reflect smaller sample.\n")
      cat("  Cannot definitively rule out matching bias, but direction is reassuring.\n")
    } else {
      cat("  RESULT: HH size effect DISAPPEARS in links without spouse agreement.\n")
      cat("  WARNING: The household size finding may be driven by matching bias.\n")
      cat("  Wife-name corroboration selects for households with wives/children.\n")
    }
  }
}

# ----- GRAPH: Household size by match type and VT status -----
if (nrow(vt_wife) >= 10 & nrow(vt_male) >= 10) {

  robustness_plot_data <- bind_rows(
    all_districts %>% filter(!is_voortrekker) %>%
      mutate(group = "Non-VT"),
    all_districts_match %>% filter(is_voortrekker & wife_corroborated == TRUE) %>%
      mutate(group = "VT (wife match)"),
    all_districts_match %>% filter(is_voortrekker & (is.na(wife_corroborated) | wife_corroborated == FALSE)) %>%
      mutate(group = "VT (male-only)")
  ) %>%
    mutate(group = factor(group, levels = c("Non-VT", "VT (male-only)", "VT (wife match)")))

  robustness_summary <- robustness_plot_data %>%
    group_by(group) %>%
    summarise(
      n = n(),
      mean_hh = mean(household_size, na.rm = TRUE),
      se_hh = sd(household_size, na.rm = TRUE) / sqrt(n()),
      mean_ch = mean(settler_children, na.rm = TRUE),
      se_ch = sd(settler_children, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  p_robustness_hh <- ggplot(robustness_summary,
                              aes(x = group, y = mean_hh, fill = group)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = mean_hh - 1.96*se_hh, ymax = mean_hh + 1.96*se_hh),
                  width = 0.2) +
    geom_text(aes(label = sprintf("%.2f\n(n=%d)", mean_hh, n)),
              vjust = -0.3, size = 3.5, color = "#2D2D2D") +
    scale_fill_manual(values = c("Non-VT" = "#3D8EB9",
                                  "VT (male-only)" = "#6B8E5E",
                                  "VT (wife match)" = "#5C2346")) +
    labs(x = "", y = "Mean Household Size") +
    theme_leap() +
    theme(legend.position = "none") +
    ylim(0, max(robustness_summary$mean_hh) * 1.25)

  print(p_robustness_hh)
  fig_file <- next_fig("household_robustness_match_type.png")
  save_leap_fig(fig_file, p_robustness_hh, width = 8, height = 5, dpi = 300)
}

# ----- GRAPH: No spouse agreement coefficient comparison -----
# >>> FIG_NO_AGREE BEGIN
if (exists("no_agree_sample") && nrow(no_agree_sample) > 0) {

  # Full-sample and no-spouse-agreement coefficients, with the wealth index shown
  # alongside the composition and slave-holding outcomes.
  nw_plot_vars <- c("household_size" = "Household size", "settler_children" = "Settler children",
                    "children_ratio" = "Children ratio", "settler_men" = "Settler men",
                    "total_slaves" = "Slaves", "wealth_index" = "Wealth index")
  compare_rows <- list()
  for (smp in c("Full sample", "No spouse agreement")) {
    dat <- if (smp == "Full sample") all_districts else no_agree_sample
    for (v in names(nw_plot_vars)) {
      reg_f <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")), data = dat)
      rob_f <- coeftest(reg_f, vcov = vcovHC(reg_f, type = "HC1"))
      compare_rows[[length(compare_rows) + 1]] <- data.frame(
        variable = v, coef = rob_f["is_voortrekkerTRUE", 1],
        se = rob_f["is_voortrekkerTRUE", 2], p_value = rob_f["is_voortrekkerTRUE", 4],
        sample = smp)
    }
  }

  compare_data <- bind_rows(compare_rows) %>%
    mutate(
      ci_lower = coef - 1.96 * se,
      ci_upper = coef + 1.96 * se,
      significant = p_value < 0.05,
      variable = factor(nw_plot_vars[variable], levels = rev(unname(nw_plot_vars)))
    )

  p_nw_compare <- ggplot(compare_data,
                           aes(x = coef, y = variable, color = sample, shape = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                   position = position_dodge(width = 0.5), height = 0.2) +
    scale_color_manual(values = c("Full sample" = "#5C2346", "No spouse agreement" = "#3D8EB9")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
    labs(x = "Coefficient on Voortrekker Indicator",
         y = "",
         color = "Sample",
         shape = "Significance") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_nw_compare)
  fig_file <- next_fig("household_robustness_no_spouse_agreement.png")
  save_leap_fig(fig_file, p_nw_compare, width = 8, height = 5, dpi = 300)
}
# >>> FIG_NO_AGREE END

cat("\n========== HOUSEHOLD SIZE ROBUSTNESS TESTS COMPLETE ==========\n")


# ============================================================================
# PART 12D: SELECTION INTO TREK BANDS AND DESTINATIONS
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("      SELECTION INTO TREK BANDS AND DESTINATIONS                 \n")
cat("================================================================\n\n")
cat(">>> PART 12D ENTERED <<<\n")

tryCatch({

# Check if move_with and move_to columns exist in vt_adults
# Debug: show available columns
cat("Checking for trek columns in vt_adults...\n")
cat("  Available columns:", paste(names(vt_adults), collapse = ", "), "\n\n")

has_move_with <- "move_with" %in% names(vt_adults)
has_move_to <- "move_to" %in% names(vt_adults)

cat("  move_with column found:", has_move_with, "\n")
cat("  move_to column found:", has_move_to, "\n")

# Check if columns have any non-NA values
if (has_move_with) {
  n_move_with <- sum(!is.na(vt_adults$move_with))
  cat("  move_with non-NA values:", n_move_with, "\n")
} else {
  n_move_with <- 0
}

if (has_move_to) {
  n_move_to <- sum(!is.na(vt_adults$move_to))
  cat("  move_to non-NA values:", n_move_to, "\n")
} else {
  n_move_to <- 0
}

# Leader analysis only needs move_with; destination analysis needs move_to
has_trek_data <- (has_move_with && n_move_with > 0) || (has_move_to && n_move_to > 0)
cat(">>> has_trek_data:", has_trek_data, "\n")

if (!has_trek_data) {
  cat("\nNOTE: Trek leader/destination data not available or all values are NA.\n")
  cat("Trek leader and destination analysis will be skipped.\n")
  cat("Check that the Excel file has columns named 'MOVE WITH' and 'MOVE TO' with data.\n\n")
}

if (has_trek_data) {

  # Merge economic data from census with matched Voortrekker records.
  # NOTE: move_with, move_to, leader_std, leader_combined, leader_group
  # are already in best_matches (inherited from vt_adults -> vt_expanded ->
  # candidates -> best_matches). Do NOT re-join them from vt_adults, as that
  # creates .x/.y suffix duplicates that break downstream column references.
  matched_with_trek <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    # No score filter: leader and destination analyses use every link.
    left_join(
      all_districts %>%
        select(census_id, cattle, sheep, horses, total_slaves, total_khoe,
               wheat_reaped, wine, wealth_index, district,
               any_of(c("settler_men", "settler_women", "settler_children",
                         "household_size", "children_ratio"))),
      by = "census_id"
    )

  # Verify columns exist in the joined data
  has_leader_col <- "move_with" %in% names(matched_with_trek) || "leader_combined" %in% names(matched_with_trek)
  has_dest_col <- "move_to" %in% names(matched_with_trek)
  cat(">>> has_leader_col:", has_leader_col, ", has_dest_col:", has_dest_col, "\n")
  cat(">>> matched_with_trek columns:", paste(names(matched_with_trek), collapse = ", "), "\n")
  cat(">>> nrow(matched_with_trek):", nrow(matched_with_trek), "\n")
  if (!has_leader_col && !has_dest_col) {
    cat("ERROR: Neither move_with/leader_combined nor move_to found after join. Skipping trek analysis.\n")
  } else {

    cat("\nMatched Voortrekkers with economic data:", nrow(matched_with_trek), "\n")
    if ("move_with" %in% names(matched_with_trek))
      cat("  With trek leader info:", sum(!is.na(matched_with_trek$move_with)), "\n")
    if ("leader_combined" %in% names(matched_with_trek))
      cat("  With leader_combined:", sum(!is.na(matched_with_trek$leader_combined)), "\n")
    if ("move_to" %in% names(matched_with_trek))
      cat("  With destination info:", sum(!is.na(matched_with_trek$move_to)), "\n")

    # Continue if we have leader or destination data
    has_any_data <- FALSE
    if ("leader_combined" %in% names(matched_with_trek))
      has_any_data <- has_any_data || sum(!is.na(matched_with_trek$leader_combined)) > 0
    if ("move_with" %in% names(matched_with_trek))
      has_any_data <- has_any_data || sum(!is.na(matched_with_trek$move_with)) > 0
    if ("move_to" %in% names(matched_with_trek))
      has_any_data <- has_any_data || sum(!is.na(matched_with_trek$move_to)) > 0
    cat(">>> has_any_data:", has_any_data, "\n")
    if (has_any_data) {

      # --------------------------------------------------------------------------
      # 12D.1 LEADER NAMES (Use pre-cleaned column from PART 2.2)
      # --------------------------------------------------------------------------

      # Leader names were already cleaned in PART 2.2 using JW fuzzy matching
      # and joined to matched_with_trek via the left_join above.
      # If leader_std wasn't carried through (e.g., move_with column issues),
      # fall back to simple pattern matching.
      if (!"leader_std" %in% names(matched_with_trek) ||
          all(is.na(matched_with_trek$leader_std))) {
        cat("NOTE: Pre-cleaned leader_std not found. Applying simple pattern matching.\n")
        matched_with_trek <- matched_with_trek %>%
          mutate(
            leader_raw = toupper(trimws(as.character(move_with))),
            leader_std = case_when(
              grepl("POTGIETER", leader_raw) ~ "Potgieter",
              grepl("RETIEF", leader_raw) ~ "Retief",
              grepl("DU PLESSIS|DUPLESSIS", leader_raw) ~ "Du Plessis",
              grepl("JACOBS", leader_raw) ~ "Jacobs",
              grepl("UYS", leader_raw) ~ "Uys",
              grepl("MARITZ", leader_raw) ~ "Maritz",
              grepl("LANDMAN", leader_raw) ~ "Landman",
              grepl("DE KLERK|DEKLERK", leader_raw) ~ "De Klerk",
              grepl("OPPERMAN", leader_raw) ~ "Opperman",
              grepl("PRETORIUS", leader_raw) ~ "Pretorius",
              grepl("VAN ROOYEN|VANROOYEN", leader_raw) ~ "Van Rooyen",
              grepl("RUDOLPH", leader_raw) ~ "Rudolph",
              grepl("NEL", leader_raw) ~ "Nel",
              grepl("MEYER", leader_raw) ~ "Meyer",
              grepl("ESPAG|ESBACH", leader_raw) ~ "Espag",
              grepl("DE LANGE|DELANGE", leader_raw) ~ "De Lange",
              grepl("MALAN", leader_raw) ~ "Malan",
              grepl("TREGARDT|TRIGARDT|TRICHARDT", leader_raw) ~ "Tregardt",
              grepl("ERASMUS", leader_raw) ~ "Erasmus",
              grepl("VAN RENSBURG|VANRENSBURG", leader_raw) ~ "Van Rensburg",
              grepl("VISAGIE", leader_raw) ~ "Visagie",
              grepl("DE BEER|DEBEER", leader_raw) ~ "De Beer",
              grepl("LOMBARD", leader_raw) ~ "Lombard",
              grepl("FAMILY|FAMILIE", leader_raw) ~ "Family/Independent",
              !is.na(leader_raw) & leader_raw != "" ~ "Other",
              TRUE ~ NA_character_
            ),
            leader_combined = leader_std,
            leader_group = case_when(
              is.na(leader_std) ~ NA_character_,
              leader_std %in% c("Potgieter", "Retief", "Du Plessis", "Jacobs",
                                "Uys", "Maritz", "Landman", "De Klerk",
                                "Opperman", "Pretorius") ~ leader_std,
              leader_std == "Family/Independent" ~ "Family/Independent",
              TRUE ~ "Minor Leader"
            )
          )
      }

      # Use leader_combined for most analyses (merges sub-leaders of same family)
      # leader_std retains the fine-grained distinction (e.g., Maritz JS vs GM)

  # Standardize destinations (only if move_to exists)
  if ("move_to" %in% names(matched_with_trek)) {
    matched_with_trek <- matched_with_trek %>%
      mutate(
        dest_raw = toupper(trimws(as.character(move_to))),
        destination_std = case_when(
          grepl("NATAL|PIETERMARITZBURG", dest_raw) ~ "Natal",
          grepl("TRANSORANJE|TRANSGARIEP|OVS|WINBURG|BLOEMFONTEIN|FAURESMITH|SMITHFIELD|KROONSTAD", dest_raw) ~ "Orange Free State",
          grepl("POTCHEFSTROOM|RUSTENBURG|MAGALIESBERG|MARICO|PRETORIA|HEIDELBERG", dest_raw) ~ "Western Transvaal",
          grepl("OHRIGSTAD|LYDENBURG|MIDDELBURG", dest_raw) ~ "Eastern Transvaal",
          grepl("VAAL", dest_raw) ~ "Vaal River",
          !is.na(dest_raw) & dest_raw != "" ~ "Other",
          TRUE ~ NA_character_
        )
      )
  }

cat("\n--- Trek Leader Distribution (combined, matched sample) ---\n")
leader_counts <- matched_with_trek %>%
  filter(!is.na(leader_combined)) %>%
  count(leader_combined) %>%
  arrange(desc(n))
print(leader_counts)

cat("\n--- Trek Leader Distribution (fine-grained, matched sample) ---\n")
leader_counts_fine <- matched_with_trek %>%
  filter(!is.na(leader_std)) %>%
  count(leader_std) %>%
  arrange(desc(n))
print(leader_counts_fine)

if ("destination_std" %in% names(matched_with_trek)) {
  cat("\n--- Destination Distribution (standardized) ---\n")
  dest_counts <- matched_with_trek %>%
    filter(!is.na(destination_std)) %>%
    count(destination_std) %>%
    arrange(desc(n))
  print(dest_counts)
} else {
  cat("\n--- Destination data not available ---\n")
  dest_counts <- data.frame(destination_std = character(0), n = integer(0))
}

# --------------------------------------------------------------------------
# 12D.1b REPRESENTATIVENESS: Are some leaders underrepresented in matches?
# --------------------------------------------------------------------------

cat("\n\n========================================\n")
cat("LEADER REPRESENTATIVENESS IN MATCHED DATA\n")
cat("========================================\n\n")
cat("Comparing leader distribution: full VT database vs matched sample.\n")
cat("If matching disproportionately fails for certain leaders' followers,\n")
cat("our leader heterogeneity analysis could be biased.\n\n")

# Get leader distribution in full VT data (all adults, not just matched)
if ("leader_combined" %in% names(vt_adults)) {

  full_vt_leaders <- vt_adults %>%
    filter(!is.na(leader_combined)) %>%
    count(leader_combined, name = "n_full") %>%
    arrange(desc(n_full))

  matched_vt_leaders <- matched_with_trek %>%
    filter(!is.na(leader_combined)) %>%
    count(leader_combined, name = "n_matched") %>%
    arrange(desc(n_matched))

  representativeness <- full_vt_leaders %>%
    left_join(matched_vt_leaders, by = "leader_combined") %>%
    mutate(
      n_matched = replace_na(n_matched, 0),
      match_rate = round(100 * n_matched / n_full, 1),
      pct_full = round(100 * n_full / sum(n_full), 1),
      pct_matched = round(100 * n_matched / sum(n_matched), 1),
      pct_diff = pct_matched - pct_full
    ) %>%
    arrange(desc(n_full))

  cat("Leader match rates (full VT database vs matched sample):\n")
  print(representativeness, n = 30)

  # Chi-squared test: is the leader distribution significantly different?
  # Compare observed (matched) vs expected (proportional to full)
  rep_for_test <- representativeness %>%
    filter(n_full >= 5, leader_combined != "Other",
           leader_combined != "Family/Independent")

  if (nrow(rep_for_test) >= 3) {
    expected_props <- rep_for_test$n_full / sum(rep_for_test$n_full)
    observed_counts <- rep_for_test$n_matched

    if (sum(observed_counts) > 0) {
      chi_test <- chisq.test(observed_counts, p = expected_props)
      cat("\nChi-squared test for equal representation:\n")
      cat("  Chi-sq =", round(chi_test$statistic, 2),
          ", df =", chi_test$parameter,
          ", p =", round(chi_test$p.value, 4), "\n")

      if (chi_test$p.value < 0.05) {
        cat("  WARNING: Leader distribution in matched sample differs significantly\n")
        cat("  from full VT database. Some leaders may be over/underrepresented.\n")

        # Which leaders deviate most?
        chi_residuals <- (observed_counts - sum(observed_counts) * expected_props) /
          sqrt(sum(observed_counts) * expected_props)
        rep_for_test$std_residual <- round(chi_residuals, 2)
        cat("\n  Standardized residuals (>|2| = significant over/underrepresentation):\n")
        print(rep_for_test %>%
                select(leader_combined, n_full, n_matched, match_rate, std_residual) %>%
                arrange(std_residual))
      } else {
        cat("  No significant difference: matched sample is representative by leader.\n")
      }
    }
  }

  # Visualize match rates by leader
  plot_data_rep <- representativeness %>%
    filter(n_full >= 8, leader_combined != "Other",
           leader_combined != "Family/Independent") %>%
    mutate(leader_combined = fct_reorder(leader_combined, match_rate))

  if (nrow(plot_data_rep) >= 3) {
    overall_rate <- sum(plot_data_rep$n_matched) / sum(plot_data_rep$n_full) * 100

    p_rep <- ggplot(plot_data_rep,
                    aes(x = match_rate, y = leader_combined)) +
      geom_vline(xintercept = overall_rate, linetype = "dashed",
                 color = "#AAAAAA", linewidth = 0.8) +
      geom_segment(aes(x = 0, xend = match_rate, yend = leader_combined),
                   color = "#4A4A4A", linewidth = 0.6) +
      geom_point(aes(size = n_full, color = match_rate), shape = 19) +
      geom_text(aes(label = paste0(match_rate, "%")),
                hjust = -0.3, size = 3, color = "#4A4A4A") +
      scale_color_gradient(low = "#D4A03E", high = "#3D8EB9",
                           name = "Match\nRate (%)") +
      scale_size_continuous(range = c(3, 10), name = "N in\nFull VT") +
      scale_x_continuous(limits = c(0, max(plot_data_rep$match_rate) * 1.15)) +
      labs(
        x = "Match Rate (%)",
        y = "",
        caption = paste0("Dashed line = overall match rate (",
                         round(overall_rate, 1), "%)")
      ) +
      theme_leap() +
      theme(panel.grid.major.y = element_blank())

    print(p_rep)
    fig_file <- next_fig("leader_match_rates.png")
    save_leap_fig(fig_file, p_rep, width = 10, height = 7, dpi = 300)
  }

  # Save representativeness results
  write.csv(representativeness, "output/tables/leader_representativeness.csv", row.names = FALSE)
  cat("\nSaved: leader_representativeness.csv\n")
}

# --------------------------------------------------------------------------
# 12D.2 SELECTION BY TREK LEADER
# --------------------------------------------------------------------------

cat("\n\n========================================\n")
cat("SELECTION HETEROGENEITY BY TREK LEADER\n")
cat("========================================\n\n")
cat("Do different leaders attract different types of migrants?\n")
cat("Key question: Is the slave grievance narrative driven by specific leaders\n")
cat("(e.g., Retief) whose followers were wealthier, while the overall Trek\n")
cat("shows no positive wealth selection?\n\n")

# Use leader_combined for analysis (avoids splitting Maritz/Erasmus into tiny groups)
major_leaders <- leader_counts %>%
  filter(n >= 8, leader_combined != "Other",
         leader_combined != "Family/Independent") %>%
  pull(leader_combined)
cat(">>> major_leaders (n >= 8):", paste(major_leaders, collapse = ", "), "\n")
cat(">>> length(major_leaders):", length(major_leaders), "\n")

if (length(major_leaders) >= 2) {

  econ_vars_trek <- c("cattle", "sheep", "horses", "total_slaves", "total_khoe",
                      "wheat_reaped", "wine", "wealth_index")

  # Also include household variables if available
  hh_vars_available <- intersect(
    c("settler_men", "settler_women", "settler_children", "household_size", "children_ratio"),
    names(matched_with_trek)
  )
  all_vars_trek <- c(econ_vars_trek, hh_vars_available)

  # Calculate mean economic characteristics by leader
  leader_means <- matched_with_trek %>%
    filter(leader_combined %in% major_leaders) %>%
    group_by(leader_combined) %>%
    summarise(
      n = n(),
      across(any_of(all_vars_trek), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  cat("Mean characteristics by trek leader (n >= 8):\n")
  print(leader_means %>% arrange(desc(n)))

  # ANOVA tests for each variable
  cat("\n\nANOVA tests (do means differ across leaders?):\n")
  cat(sprintf("%-20s %10s %10s %8s\n", "Variable", "F-stat", "p-value", "Sig"))
  cat(paste(rep("-", 52), collapse = ""), "\n")

  leader_anova_results <- data.frame()

  for (v in all_vars_trek) {
    test_data <- matched_with_trek %>%
      filter(leader_combined %in% major_leaders, !is.na(.data[[v]]))

    if (n_distinct(test_data$leader_combined) >= 2 && nrow(test_data) >= 20) {
      aov_result <- aov(as.formula(paste(v, "~ leader_combined")), data = test_data)
      aov_summary <- summary(aov_result)
      f_stat <- aov_summary[[1]]["leader_combined", "F value"]
      p_val <- aov_summary[[1]]["leader_combined", "Pr(>F)"]

      sig <- ifelse(p_val < 0.01, "***", ifelse(p_val < 0.05, "**",
              ifelse(p_val < 0.1, "*", "")))
      cat(sprintf("%-20s %10.2f %10.4f %8s\n", v, f_stat, p_val, sig))

      leader_anova_results <- rbind(leader_anova_results, data.frame(
        variable = v, f_stat = f_stat, p_value = p_val
      ))
    }
  }

  # Save ANOVA results
  write.csv(leader_anova_results, "output/tables/leader_anova_results.csv", row.names = FALSE)

  # --------------------------------------------------------------------------
  # 12D.2a REGRESSION: Leader FE with district controls
  # --------------------------------------------------------------------------

  cat("\n\n--- LEADER SELECTION REGRESSIONS ---\n")
  cat("OLS regressions with leader as key predictor, controlling for district.\n")
  cat("Reference category: largest leader group.\n\n")

  # Set reference category to the largest leader group
  largest_leader <- leader_counts %>%
    filter(leader_combined != "Other", leader_combined != "Family/Independent") %>%
    slice(1) %>%
    pull(leader_combined)

  reg_data <- matched_with_trek %>%
    filter(leader_combined %in% major_leaders) %>%
    mutate(leader_combined = relevel(factor(leader_combined), ref = largest_leader))

  leader_reg_results <- list()

  for (v in econ_vars_trek) {
    if (sum(!is.na(reg_data[[v]])) >= 30) {
      # Model 1: Leader only
      m1 <- lm(as.formula(paste(v, "~ leader_combined")), data = reg_data)

      # Model 2: Leader + District FE
      if ("district" %in% names(reg_data) && n_distinct(reg_data$district) >= 2) {
        m2 <- lm(as.formula(paste(v, "~ leader_combined + factor(district)")),
                  data = reg_data)
      } else {
        m2 <- m1
      }

      leader_reg_results[[v]] <- list(m1 = m1, m2 = m2)

      # Print key results for model 2
      coefs <- summary(m2)$coefficients
      leader_rows <- grep("leader_combined", rownames(coefs))
      if (length(leader_rows) > 0) {
        cat(paste0("\n", v, " (with district FE, ref = ", largest_leader, "):\n"))
        for (r in leader_rows) {
          leader_name <- gsub("leader_combined", "", rownames(coefs)[r])
          sig <- ifelse(coefs[r, 4] < 0.01, "***", ifelse(coefs[r, 4] < 0.05, "**",
                  ifelse(coefs[r, 4] < 0.1, "*", "")))
          cat(sprintf("  %-15s  coef = %8.3f  se = %7.3f  p = %6.4f %s\n",
                      leader_name, coefs[r, 1], coefs[r, 2], coefs[r, 4], sig))
        }
      }
    }
  }

  # --------------------------------------------------------------------------
  # 12D.2b FOCUSED VISUALIZATION: Key Leaders Heatmap
  # --------------------------------------------------------------------------

  cat("\n\n--- LEADER SELECTION HEATMAP ---\n")

  # Get leaders with n >= 8 for visualization
  top_leaders <- leader_counts %>%
    filter(n >= 8, leader_combined != "Other",
           leader_combined != "Family/Independent") %>%
    pull(leader_combined)

  if (length(top_leaders) >= 3) {

    # Calculate detailed statistics for top leaders
    top_leader_stats <- matched_with_trek %>%
      filter(leader_combined %in% top_leaders) %>%
      group_by(leader_combined) %>%
      summarise(
        n_followers = n(),
        mean_cattle = mean(cattle, na.rm = TRUE),
        mean_sheep = mean(sheep, na.rm = TRUE),
        mean_horses = mean(horses, na.rm = TRUE),
        mean_slaves = mean(total_slaves, na.rm = TRUE),
        mean_khoe = mean(total_khoe, na.rm = TRUE),
        mean_wheat = mean(wheat_reaped, na.rm = TRUE),
        mean_wine = mean(wine, na.rm = TRUE),
        mean_wealth = mean(wealth_index, na.rm = TRUE),
        se_wealth = sd(wealth_index, na.rm = TRUE) / sqrt(sum(!is.na(wealth_index))),
        mean_hh_size = if ("household_size" %in% names(matched_with_trek))
          mean(household_size, na.rm = TRUE) else NA_real_,
        mean_children = if ("settler_children" %in% names(matched_with_trek))
          mean(settler_children, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(n_followers))

    cat("\nDetailed statistics by leader:\n")
    print(top_leader_stats)

    # Save leader statistics
    write.csv(top_leader_stats, "output/tables/leader_selection_stats.csv", row.names = FALSE)
    cat("\nSaved: leader_selection_stats.csv\n")

    # --- Heatmap of z-scores ---
    overall_stats <- matched_with_trek %>%
      filter(leader_combined %in% top_leaders) %>%
      summarise(
        across(any_of(econ_vars_trek),
               list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)))
      )

    # Build z-score columns dynamically
    heatmap_data <- top_leader_stats %>%
      mutate(leader_label = paste0(leader_combined, " (n=", n_followers, ")"))

    z_cols <- list()
    for (v in econ_vars_trek) {
      mean_col <- paste0("mean_", gsub("total_", "", v))
      if (!mean_col %in% names(heatmap_data)) {
        mean_col <- paste0("mean_", v)
      }
      overall_mean_col <- paste0(v, "_mean")
      overall_sd_col <- paste0(v, "_sd")
      if (mean_col %in% names(heatmap_data) &&
          overall_mean_col %in% names(overall_stats) &&
          overall_sd_col %in% names(overall_stats)) {
        z_name <- paste0("z_", gsub("total_", "", v))
        sd_val <- overall_stats[[overall_sd_col]]
        if (!is.na(sd_val) && sd_val > 0) {
          heatmap_data[[z_name]] <- (heatmap_data[[mean_col]] -
                                       overall_stats[[overall_mean_col]]) / sd_val
        }
      }
    }

    z_cols_available <- grep("^z_", names(heatmap_data), value = TRUE)

    if (length(z_cols_available) >= 4) {
      heatmap_long <- heatmap_data %>%
        select(leader_label, all_of(z_cols_available)) %>%
        pivot_longer(cols = all_of(z_cols_available),
                     names_to = "variable", values_to = "z_score") %>%
        mutate(
          variable = gsub("^z_", "", variable),
          variable = case_when(
            variable == "cattle" ~ "Cattle",
            variable == "sheep" ~ "Sheep",
            variable == "horses" ~ "Horses",
            variable == "slaves" ~ "Slaves",
            variable == "khoe" ~ "Khoe Workers",
            variable == "wheat_reaped" ~ "Wheat",
            variable == "wine" ~ "Wine",
            variable == "wealth_index" ~ "Wealth Index",
            TRUE ~ str_to_title(variable)
          ),
          variable = factor(variable, levels = c("Cattle", "Sheep", "Horses", "Slaves",
                                                  "Khoe Workers", "Wheat", "Wine",
                                                  "Wealth Index"))
        )

      # Order leaders by wealth
      wealth_order <- heatmap_data %>%
        arrange(desc(mean_wealth)) %>%
        pull(leader_label)
      heatmap_long$leader_label <- factor(heatmap_long$leader_label,
                                           levels = rev(wealth_order))

      p_heatmap <- ggplot(heatmap_long,
                          aes(x = variable, y = leader_label, fill = z_score)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = sprintf("%.2f", z_score)),
                  color = ifelse(abs(heatmap_long$z_score) > 0.8, "white", "black"),
                  size = 3) +
        scale_fill_gradient2(
          low = "#3D8EB9", mid = "#FFFFFF", high = "#5C2346",
          midpoint = 0, limits = c(-2, 2), oob = scales::squish,
          name = "Z-Score\n(SD from mean)"
        ) +
        labs(x = "", y = "") +
        theme_leap() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        )

      print(p_heatmap)
      fig_file <- next_fig("leaders_heatmap.png")
      save_leap_fig(fig_file, p_heatmap, width = 11, height = 8, dpi = 300)

    }

    # --- Wealth dot plot with CIs ---
    # Non-Voortrekker mean for reference line
    non_vt_mean_wealth <- all_districts %>%
      filter(!is_voortrekker) %>%
      summarise(m = mean(wealth_index, na.rm = TRUE)) %>%
      pull(m)

    top_leader_ci <- top_leader_stats %>%
      mutate(
        ci_low = mean_wealth - 1.96 * se_wealth,
        ci_high = mean_wealth + 1.96 * se_wealth,
        leader_label = paste0(leader_combined, "\n(n=", n_followers, ")")
      )

    wealth_order <- top_leader_ci %>% arrange(mean_wealth) %>% pull(leader_label)
    top_leader_ci$leader_label <- factor(top_leader_ci$leader_label,
                                          levels = wealth_order)

    p_wealth_dot <- ggplot(top_leader_ci,
                           aes(x = mean_wealth, y = leader_label)) +
      geom_vline(xintercept = non_vt_mean_wealth, linetype = "dashed",
                 color = "#AAAAAA", linewidth = 0.8) +
      geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                     height = 0.3, color = "#4A4A4A", linewidth = 0.8) +
      geom_point(aes(size = n_followers, color = mean_wealth), shape = 19) +
      scale_color_gradient2(low = "#3D8EB9", mid = "#F5F3F0", high = "#5C2346",
                            midpoint = non_vt_mean_wealth,
                            name = "Wealth\nIndex") +
      scale_size_continuous(range = c(4, 12), name = "Followers") +
      labs(x = "Mean Wealth Index (with 95% CI)", y = "") +
      annotate("text", x = non_vt_mean_wealth, y = 0.5,
               label = "Non-Voortrekker mean", hjust = -0.05,
               color = "#AAAAAA", size = 3) +
      theme_leap() +
      theme(panel.grid.minor = element_blank(), legend.position = "right")

    print(p_wealth_dot)
    fig_file <- next_fig("selection_by_leader.png")
    save_leap_fig(fig_file, p_wealth_dot, width = 10, height = 7, dpi = 300)

    # --- Bar chart: Cattle, Sheep, Slaves, Wealth by leader ---
    top_leader_long <- top_leader_stats %>%
      select(leader_combined, n_followers,
             mean_cattle, mean_sheep, mean_slaves, mean_wealth) %>%
      pivot_longer(cols = starts_with("mean_"),
                   names_to = "variable", values_to = "value") %>%
      mutate(
        variable = gsub("mean_", "", variable),
        variable = case_when(
          variable == "cattle" ~ "Cattle",
          variable == "sheep" ~ "Sheep",
          variable == "slaves" ~ "Slaves",
          variable == "wealth" ~ "Wealth Index",
          TRUE ~ variable
        ),
        leader_combined = factor(leader_combined,
                                  levels = top_leader_stats$leader_combined)
      )

    overall_means <- matched_with_trek %>%
      filter(leader_combined %in% top_leaders) %>%
      summarise(
        Cattle = mean(cattle, na.rm = TRUE),
        Sheep = mean(sheep, na.rm = TRUE),
        Slaves = mean(total_slaves, na.rm = TRUE),
        `Wealth Index` = mean(wealth_index, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "overall_mean")

    top_leader_long <- top_leader_long %>%
      left_join(overall_means, by = "variable")

    p_leaders_bar <- ggplot(top_leader_long,
                            aes(x = leader_combined, y = value, fill = leader_combined)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_hline(aes(yintercept = overall_mean), linetype = "dashed",
                 color = "#AAAAAA", linewidth = 0.8) +
      facet_wrap(~variable, scales = "free_y", nrow = 1) +
      labs(x = "", y = "Mean Value",
           caption = "Dashed line = overall VT mean") +
      theme_leap() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 11)
      ) +
      # recycle the palette, since all links give more than eight leaders.
      scale_fill_manual(values = rep_len(LEAP_CYCLE, n_distinct(top_leader_long$leader_combined)))

    print(p_leaders_bar)
    fig_file <- next_fig("leaders_bar_chart.png")
    save_leap_fig(fig_file, p_leaders_bar, width = 14, height = 6, dpi = 300)

    # --------------------------------------------------------------------------
    # 12D.2c PAIRWISE COMPARISONS: Retief vs Others
    # --------------------------------------------------------------------------

    cat("\n\n--- PAIRWISE LEADER COMPARISONS ---\n")
    cat("Testing whether specific leaders' followers differ from the rest.\n")
    cat("Historiographical focus: Retief is most associated with the slave\n")
    cat("grievance narrative. Were his followers actually wealthier?\n\n")

    pairwise_results <- data.frame()

    for (ldr in major_leaders) {
      ldr_data <- matched_with_trek %>%
        filter(leader_combined %in% major_leaders) %>%
        mutate(is_leader = ifelse(leader_combined == ldr, 1, 0))

      for (v in econ_vars_trek) {
        ldr_vals <- ldr_data %>% filter(is_leader == 1) %>% pull(!!sym(v))
        other_vals <- ldr_data %>% filter(is_leader == 0) %>% pull(!!sym(v))
        ldr_vals <- ldr_vals[!is.na(ldr_vals)]
        other_vals <- other_vals[!is.na(other_vals)]

        if (length(ldr_vals) >= 5 && length(other_vals) >= 5) {
          t_result <- t.test(ldr_vals, other_vals)
          pairwise_results <- rbind(pairwise_results, data.frame(
            leader = ldr,
            variable = v,
            leader_mean = mean(ldr_vals),
            others_mean = mean(other_vals),
            diff = mean(ldr_vals) - mean(other_vals),
            t_stat = t_result$statistic,
            p_value = t_result$p.value,
            n_leader = length(ldr_vals),
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    if (nrow(pairwise_results) > 0) {
      pairwise_results$sig <- ifelse(pairwise_results$p_value < 0.01, "***",
                               ifelse(pairwise_results$p_value < 0.05, "**",
                               ifelse(pairwise_results$p_value < 0.1, "*", "")))

      # Show significant results
      sig_results <- pairwise_results %>% filter(p_value < 0.10)
      if (nrow(sig_results) > 0) {
        cat("Significant differences (p < 0.10):\n")
        print(sig_results %>%
                select(leader, variable, leader_mean, others_mean, diff, p_value, sig) %>%
                arrange(leader, p_value))
      } else {
        cat("No significant pairwise differences found (all p > 0.10).\n")
      }

      # Specific focus: Retief
      retief_results <- pairwise_results %>% filter(leader == "Retief")
      if (nrow(retief_results) > 0) {
        cat("\n--- Retief vs All Others (the 'slave narrative' test) ---\n")
        print(retief_results %>%
                select(variable, leader_mean, others_mean, diff, p_value, sig) %>%
                mutate(across(c(leader_mean, others_mean, diff), ~round(.x, 2))))
      }

      # Save full pairwise results
      write.csv(pairwise_results, "output/tables/leader_pairwise_comparisons.csv", row.names = FALSE)
      cat("\nSaved: leader_pairwise_comparisons.csv\n")

      # --- Forest plot: Leader-specific deviations for key variables ---
      forest_data <- pairwise_results %>%
        filter(variable %in% c("total_slaves", "wealth_index", "cattle")) %>%
        mutate(
          variable = case_when(
            variable == "total_slaves" ~ "Slaves",
            variable == "wealth_index" ~ "Wealth Index",
            variable == "cattle" ~ "Cattle",
            TRUE ~ variable
          ),
          leader = factor(leader, levels = rev(sort(unique(leader))))
        )

      if (nrow(forest_data) >= 6) {
        p_forest <- ggplot(forest_data,
                           aes(x = diff, y = leader, color = variable)) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA") +
          geom_point(aes(shape = sig != ""), size = 3, position = position_dodge(0.5)) +
          facet_wrap(~variable, scales = "free_x") +
          scale_color_manual(values = c("Cattle" = LEAP_COLORS["sage"],
                                        "Slaves" = LEAP_COLORS["plum"],
                                        "Wealth Index" = LEAP_COLORS["blue"])) +
          scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                             name = "p < 0.10") +
          labs(
            x = "Difference from Other Leaders' Followers",
            y = ""
          ) +
          theme_leap() +
          theme(legend.position = "bottom", panel.grid.major.y = element_blank())

        print(p_forest)
        fig_file <- next_fig("leader_pairwise_forest.png")
        save_leap_fig(fig_file, p_forest, width = 12, height = 7, dpi = 300)
      }
    }

    # --------------------------------------------------------------------------
    # 12D.2d SUMMARY: Narrative link
    # --------------------------------------------------------------------------

    cat("\n\n--- LEADER HETEROGENEITY: HISTORIOGRAPHICAL IMPLICATIONS ---\n\n")

    # Check if Retief's followers are richer
    retief_wealth <- pairwise_results %>%
      filter(leader == "Retief", variable == "wealth_index")
    if (nrow(retief_wealth) > 0) {
      if (retief_wealth$diff > 0 && retief_wealth$p_value < 0.10) {
        cat("FINDING: Retief's followers were WEALTHIER than other leaders' followers\n")
        cat("  (wealth diff = ", round(retief_wealth$diff, 3),
            ", p = ", round(retief_wealth$p_value, 4), ")\n")
        cat("  This may explain why the slave emancipation narrative dominates:\n")
        cat("  Retief, the most literate and vocal leader, attracted wealthier settlers\n")
        cat("  whose grievances about compensation shaped the historiography.\n\n")
      } else if (retief_wealth$diff > 0) {
        cat("FINDING: Retief's followers were somewhat wealthier, but not significantly.\n")
        cat("  (wealth diff = ", round(retief_wealth$diff, 3),
            ", p = ", round(retief_wealth$p_value, 4), ")\n\n")
      } else {
        cat("FINDING: Retief's followers were NOT wealthier than other leaders' followers.\n")
        cat("  (wealth diff = ", round(retief_wealth$diff, 3),
            ", p = ", round(retief_wealth$p_value, 4), ")\n\n")
      }
    }

    retief_slaves <- pairwise_results %>%
      filter(leader == "Retief", variable == "total_slaves")
    if (nrow(retief_slaves) > 0) {
      cat("Retief's followers - slaves: mean =", round(retief_slaves$leader_mean, 1),
          "vs others =", round(retief_slaves$others_mean, 1),
          "(diff =", round(retief_slaves$diff, 1),
          ", p =", round(retief_slaves$p_value, 4), ")\n")
    }

    # Print overall disparity summary
    cat("\n--- DISPARITIES SUMMARY ---\n")
    richest <- top_leader_stats %>% filter(mean_wealth == max(mean_wealth, na.rm = TRUE))
    poorest <- top_leader_stats %>% filter(mean_wealth == min(mean_wealth, na.rm = TRUE))
    cat("Wealthiest followers:", richest$leader_combined,
        "(wealth =", round(richest$mean_wealth, 2), ")\n")
    cat("Least wealthy followers:", poorest$leader_combined,
        "(wealth =", round(poorest$mean_wealth, 2), ")\n")
    cat("Wealth gap:", round(richest$mean_wealth - poorest$mean_wealth, 2), "\n")

  }
}

# --------------------------------------------------------------------------
# 12D.3 SELECTION BY DESTINATION
# --------------------------------------------------------------------------

cat("\n\n--- SELECTION BY DESTINATION ---\n")
cat("Does wealth predict where you end up?\n\n")

major_dests <- dest_counts %>% filter(n >= 15) %>% pull(destination_std)

if (length(major_dests) >= 2) {

  # Calculate mean economic characteristics by destination
  dest_means <- matched_with_trek %>%
    filter(destination_std %in% major_dests) %>%
    group_by(destination_std) %>%
    summarise(
      n = n(),
      across(all_of(econ_vars_trek), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  cat("Mean economic characteristics by destination:\n")
  print(dest_means %>% arrange(desc(n)))

  # ANOVA tests
  cat("\n\nANOVA tests (do means differ across destinations?):\n")
  cat(sprintf("%-20s %10s %10s\n", "Variable", "F-stat", "p-value"))
  cat(paste(rep("-", 45), collapse = ""), "\n")

  dest_anova_results <- data.frame()

  for (v in econ_vars_trek) {
    test_data <- matched_with_trek %>%
      filter(destination_std %in% major_dests, !is.na(.data[[v]]))

    if (n_distinct(test_data$destination_std) >= 2 && nrow(test_data) >= 20) {
      aov_result <- aov(as.formula(paste(v, "~ destination_std")), data = test_data)
      aov_summary <- summary(aov_result)
      f_stat <- aov_summary[[1]]["destination_std", "F value"]
      p_val <- aov_summary[[1]]["destination_std", "Pr(>F)"]

      cat(sprintf("%-20s %10.2f %10.4f %s\n", v, f_stat, p_val,
                  ifelse(p_val < 0.05, "*", "")))

      dest_anova_results <- rbind(dest_anova_results, data.frame(
        variable = v, f_stat = f_stat, p_value = p_val
      ))
    }
  }

  # Multinomial regression: does wealth predict destination?
  cat("\n\n--- Multinomial Logit: Wealth Predicting Destination ---\n")

  mlogit_data <- matched_with_trek %>%
    filter(destination_std %in% major_dests) %>%
    mutate(destination_std = factor(destination_std))

  if (nrow(mlogit_data) >= 50) {
    mlogit_data$wealth_scaled <- scale(mlogit_data$wealth_index)[,1]

    mlogit_model <- multinom(destination_std ~ wealth_scaled + cattle + total_slaves,
                              data = mlogit_data, trace = FALSE)

    cat("\nMultinomial logit results (base category:", levels(mlogit_data$destination_std)[1], "):\n")
    print(summary(mlogit_model))

    z <- summary(mlogit_model)$coefficients / summary(mlogit_model)$standard.errors
    p <- (1 - pnorm(abs(z), 0, 1)) * 2

    cat("\nP-values for coefficients:\n")
    print(round(p, 4))
  }

  # --- Wealth CI dot plot by destination (main figure for paper) ---
  non_vt_mean_dest <- all_districts %>%
    filter(!is_voortrekker) %>%
    summarise(m = mean(wealth_index, na.rm = TRUE)) %>%
    pull(m)

  dest_ci <- matched_with_trek %>%
    filter(destination_std %in% major_dests) %>%
    group_by(destination_std) %>%
    summarise(
      n_dest = n(),
      mean_wealth = mean(wealth_index, na.rm = TRUE),
      se_wealth = sd(wealth_index, na.rm = TRUE) / sqrt(sum(!is.na(wealth_index))),
      ci_low = mean_wealth - 1.96 * se_wealth,
      ci_high = mean_wealth + 1.96 * se_wealth,
      .groups = "drop"
    ) %>%
    mutate(dest_label = paste0(destination_std, "\n(n=", n_dest, ")"))

  dest_order <- dest_ci %>% arrange(mean_wealth) %>% pull(dest_label)
  dest_ci$dest_label <- factor(dest_ci$dest_label, levels = dest_order)

  p_dest_ci <- ggplot(dest_ci, aes(x = mean_wealth, y = dest_label)) +
    geom_vline(xintercept = non_vt_mean_dest, linetype = "dashed",
               color = "#AAAAAA", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                   height = 0.3, color = "#4A4A4A", linewidth = 0.8) +
    geom_point(aes(size = n_dest, color = mean_wealth), shape = 19) +
    scale_color_gradient2(low = "#3D8EB9", mid = "#F5F3F0", high = "#5C2346",
                          midpoint = non_vt_mean_dest, name = "Wealth\nIndex") +
    scale_size_continuous(range = c(4, 12), name = "Households") +
    labs(x = "Mean Wealth Index (with 95% CI)", y = "") +
    annotate("text", x = non_vt_mean_dest, y = 0.5,
             label = "Non-Voortrekker mean", hjust = -0.05,
             color = "#AAAAAA", size = 3) +
    theme_leap() +
    theme(panel.grid.minor = element_blank(), legend.position = "right")

  print(p_dest_ci)
  fig_file <- next_fig("selection_by_destination.png")
  save_leap_fig(fig_file, p_dest_ci, width = 10, height = 6, dpi = 300)

  # --- Bar chart: multi-variable comparison by destination ---
  dest_plot_data <- matched_with_trek %>%
    filter(destination_std %in% major_dests) %>%
    select(destination_std, all_of(econ_vars_trek)) %>%
    pivot_longer(cols = all_of(econ_vars_trek), names_to = "variable", values_to = "value") %>%
    group_by(destination_std, variable) %>%
    summarise(mean_val = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(n()),
              .groups = "drop")

  dest_plot_std <- dest_plot_data %>%
    group_by(variable) %>%
    mutate(
      grand_mean = mean(mean_val, na.rm = TRUE),
      grand_sd = sd(mean_val, na.rm = TRUE),
      std_mean = (mean_val - grand_mean) / grand_sd
    ) %>%
    ungroup()

  p_dests_bar <- ggplot(dest_plot_std %>% filter(variable %in% c("cattle", "sheep", "total_slaves", "wealth_index")),
                    aes(x = destination_std, y = std_mean, fill = destination_std)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~variable, scales = "free_y", nrow = 1) +
    labs(x = "Destination", y = "Standardized Mean\n(deviation from overall mean)") +
    theme_leap() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_fill_manual(values = LEAP_CYCLE)

  print(p_dests_bar)
  fig_file <- next_fig("destination_bar_chart.png")
  save_leap_fig(fig_file, p_dests_bar, width = 12, height = 5, dpi = 300)
}

# --------------------------------------------------------------------------
# 12D.4 ORIGIN DISTRICT BY LEADER AND DESTINATION
# --------------------------------------------------------------------------

cat("\n\n--- ORIGIN PATTERNS ---\n")
cat("Where did different leaders' followers come from?\n\n")

if (length(major_leaders) >= 2) {
  origin_by_leader <- matched_with_trek %>%
    filter(leader_combined %in% major_leaders) %>%
    count(leader_combined, district) %>%
    group_by(leader_combined) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    arrange(leader_combined, desc(n))

  cat("Top origin districts by leader:\n")
  print(origin_by_leader %>% filter(pct >= 10))

  # Visualize origin patterns for major leaders
  top_5_leaders <- leader_counts %>%
    filter(n >= 15, leader_combined != "Other",
           leader_combined != "Family/Independent") %>%
    head(8) %>%
    pull(leader_combined)

  if (length(top_5_leaders) >= 3) {
    origin_plot <- matched_with_trek %>%
      filter(leader_combined %in% top_5_leaders, !is.na(district)) %>%
      count(leader_combined, district) %>%
      group_by(leader_combined) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup()

    p_origin <- ggplot(origin_plot,
                       aes(x = leader_combined, y = pct, fill = district)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "", y = "Percentage of Followers (%)",
           fill = "Origin District") +
      theme_leap() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = LEAP_CYCLE)

    print(p_origin)
    fig_file <- next_fig("leader_origin_districts.png")
    save_leap_fig(fig_file, p_origin, width = 12, height = 6, dpi = 300)
  }
}

    } else {
      cat("\nNo trek data available to analyze (all move_with and move_to values are NA).\n")
    }  # End of if (has trek data to analyze) block
  }  # End of else block - move_with column exists in joined data
}  # End of if (has_trek_data) block

cat(">>> PART 12D COMPLETED SUCCESSFULLY <<<\n")
}, error = function(e) {
  cat(">>> PART 12D ERROR:", conditionMessage(e), "\n")
  cat(">>> Error occurred in:", deparse(conditionCall(e)), "\n")
})


# ============================================================================
# PART 12C: ROBUSTNESS - 1830s CENSUS DATA
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("      ROBUSTNESS CHECK: 1830s CENSUS DATA                        \n")
cat("================================================================\n\n")

# Load and clean 1830s census data from frontier districts
# These censuses are from 1833-1834, closer to the Great Trek (1836-1838)

# --------------------------------------------------------------------------
# 12C.1 LOAD AND CLEAN 1830s DATA
# --------------------------------------------------------------------------

cat("Loading 1830s census data...\n")

# Helper function to convert £/s/d to decimal pounds
parse_source_quantity <- function(x) {
  z <- trimws(as.character(x))
  blank <- is.na(z) | z == ""
  out <- suppressWarnings(as.numeric(z))
  fractions <- c("\u00bc"=.25, "\u00bd"=.5, "\u00be"=.75,
                 "\u215b"=.125, "\u215c"=.375, "\u215d"=.625, "\u215e"=.875)
  for (symbol in names(fractions)) {
    hit <- !blank & grepl(paste0("^[0-9]*", symbol, "$"), z)
    whole <- sub(symbol, "", z[hit], fixed=TRUE)
    whole[whole == ""] <- "0"
    out[hit] <- as.numeric(whole) + fractions[[symbol]]
  }
  out[blank] <- 0
  out
}

convert_lsd_to_pounds <- function(pounds, shillings, pence) {
  parse_source_quantity(pounds) + parse_source_quantity(shillings)/20 +
    parse_source_quantity(pence)/240
}

# Load Beaufort West 1833
df_bw <- read_excel("data/raw/1830s series.xlsx", sheet = "Beaufort West 1833", col_names = FALSE, skip = 2)
df_bw <- df_bw %>%
  filter(!is.na(...1) & !grepl("^FOLIO", ...1)) %>%
  transmute(
    record_nr = as.numeric(...1),
    surname = toupper(trimws(as.character(...3))),
    first_name = toupper(trimws(as.character(...4))),
    horses_trade = parse_source_quantity(...15),
    cattle = parse_source_quantity(...20),
    sheep = parse_source_quantity(...21),
    grain_reaped = parse_source_quantity(...25),
    total_tax = convert_lsd_to_pounds(...44, ...45, ...46),
    district = "Beaufort West"
  ) %>%
  filter(!is.na(surname) & surname != "" & surname != "NAN")

cat("  Beaufort West 1833:", nrow(df_bw), "records\n")

# Load Graaff-Reinet 1834
df_gr <- read_excel("data/raw/1830s series.xlsx", sheet = "Graaff Reinet 1834", col_names = FALSE, skip = 2)
df_gr <- df_gr %>%
  filter(!is.na(...1) & !grepl("^FOLIO", ...1)) %>%
  transmute(
    record_nr = as.numeric(...1),
    surname = toupper(trimws(as.character(...3))),
    first_name = toupper(trimws(as.character(...4))),
    horses_trade = parse_source_quantity(...15),
    cattle = parse_source_quantity(...20),
    sheep = parse_source_quantity(...21),
    grain_reaped = parse_source_quantity(...25),
    total_tax = convert_lsd_to_pounds(...44, ...45, ...46),
    district = "Graaff-Reinet"
  ) %>%
  filter(!is.na(surname) & surname != "" & surname != "NAN")

cat("  Graaff-Reinet 1834:", nrow(df_gr), "records\n")

# Load Swellendam 1834
df_sw34 <- read_excel("data/raw/1830s series.xlsx", sheet = "Swellendam 1834", col_names = FALSE, skip = 2)
df_sw34 <- df_sw34 %>%
  filter(!is.na(...1) & !grepl("^FOLIO", ...1)) %>%
  transmute(
    record_nr = as.numeric(...1),
    surname = toupper(trimws(as.character(...3))),
    first_name = toupper(trimws(as.character(...4))),
    horses_trade = parse_source_quantity(...15),
    cattle = parse_source_quantity(...20),
    sheep = parse_source_quantity(...21),
    grain_reaped = parse_source_quantity(...25),
    total_tax = convert_lsd_to_pounds(...44, ...45, ...46),
    district = "Swellendam"
  ) %>%
  filter(!is.na(surname) & surname != "" & surname != "NAN")

cat("  Swellendam 1834:", nrow(df_sw34), "records\n")

# Load Worcester 1834 (has extra District column, offset by 1)
df_wo <- read_excel("data/raw/1830s series.xlsx", sheet = "Worcester 1834", col_names = FALSE, skip = 2)
df_wo <- df_wo %>%
  filter(!is.na(...1) & !grepl("^FOLIO", ...1)) %>%
  transmute(
    record_nr = as.numeric(...1),
    surname = toupper(trimws(as.character(...3))),
    first_name = toupper(trimws(as.character(...4))),
    horses_trade = parse_source_quantity(...16),  # Offset by 1
    cattle = parse_source_quantity(...21),         # Offset by 1
    sheep = parse_source_quantity(...22),          # Offset by 1
    grain_reaped = parse_source_quantity(...26),   # Offset by 1
    total_tax = convert_lsd_to_pounds(...45, ...46, ...47),  # Offset by 1
    district = "Worcester"
  ) %>%
  filter(!is.na(surname) & surname != "" & surname != "NAN")

cat("  Worcester 1834:", nrow(df_wo), "records\n")

# Combine all 1830s data
census_1830s <- bind_rows(df_bw, df_gr, df_sw34, df_wo)
census_1830s$census_id_1830s <- 1:nrow(census_1830s)

cat("\nTotal 1830s census records:", nrow(census_1830s), "\n")

# Check for data quality
cat("\nData quality check:\n")
cat("  Records with NA surname:", sum(is.na(census_1830s$surname)), "\n")
cat("  Records with NA first_name:", sum(is.na(census_1830s$first_name)), "\n")
cat("  Records with NA total_tax:", sum(is.na(census_1830s$total_tax)), "\n")

# Clean up names for matching
census_1830s <- census_1830s %>%
  mutate(
    surname_clean = gsub("[^A-Z]", "", toupper(as.character(surname))),
    first_clean = gsub("[^A-Z ]", "", toupper(as.character(first_name))),
    first_clean = trimws(first_clean),
    first_only = word(first_clean, 1)
  )

# Replace NA with 0 for economic variables
# Blanks have already been parsed under the original zero convention.
# Unresolved or damaged source entries remain NA.
write.csv(census_1830s, "output/tables/census_1830s_source_corrected.csv", row.names=FALSE)

cat("\nEconomic variable summary (1830s):\n")
cat("  Mean total tax: £", round(mean(census_1830s$total_tax, na.rm = TRUE), 2), "\n")
cat("  Mean cattle:", round(mean(census_1830s$cattle, na.rm = TRUE), 1), "\n")
cat("  Mean sheep:", round(mean(census_1830s$sheep, na.rm = TRUE), 1), "\n")

# --------------------------------------------------------------------------
# 12C.2 MATCH VOORTREKKERS TO 1830s CENSUS
# --------------------------------------------------------------------------

cat("\n--- Matching Voortrekkers to 1830s Census ---\n")

# Map Voortrekker districts to 1830s districts
map_to_1830s_districts <- function(d) {
  if (is.na(d)) return(c("Graaff-Reinet", "Beaufort West"))  # Default to frontier
  d <- toupper(trimws(d))

  if (grepl("BEAUFORT", d)) return("Beaufort West")
  if (grepl("GRAAFF|GRAAF", d)) return("Graaff-Reinet")
  if (grepl("SWELLENDAM", d)) return("Swellendam")
  if (grepl("WORCESTER", d)) return("Worcester")
  # Somerset and Colesberg: search in Graaff-Reinet (parent district)
  if (grepl("SOMERSET|COLESBERG|COLEBERG", d)) return(c("Graaff-Reinet", "Beaufort West"))
  # Other frontier districts
  if (grepl("UITENHAGE|ALBANY|CRADOCK", d)) return(c("Graaff-Reinet", "Beaufort West"))
  # Default
  return(c("Graaff-Reinet", "Beaufort West", "Swellendam", "Worcester"))
}

# Prepare Voortrekker data for matching
# Note: vt_adults has vt_surname and vt_name columns (not vt_surname_std)
vt_for_1830s <- vt_adults %>%
  mutate(
    row_id_1830s = row_number(),  # Create unique ID for matching
    vt_surname_std = toupper(trimws(as.character(vt_surname))),
    vt_name_clean = toupper(trimws(as.character(vt_name))),
    vt_name_clean = gsub("\\(.*?\\)", "", vt_name_clean),
    vt_surname_clean = gsub("[^A-Z]", "", vt_surname_std),
    vt_first_clean = gsub("[^A-Z ]", "", vt_name_clean),
    vt_first_clean = trimws(vt_first_clean),
    vt_first_only = word(vt_first_clean, 1)
  ) %>%
  # Ensure we have valid names for matching
 filter(!is.na(vt_surname_clean) & vt_surname_clean != "" &
         !is.na(vt_first_clean) & vt_first_clean != "")

cat("Voortrekkers prepared for 1830s matching:", nrow(vt_for_1830s), "\n")

# Also ensure census_1830s has clean names
census_1830s <- census_1830s %>%
  mutate(
    first_clean = ifelse(is.na(first_clean) | first_clean == "", "UNKNOWN", first_clean),
    first_only = ifelse(is.na(first_only) | first_only == "", "UNKNOWN", first_only)
  )

# Perform matching using Jaro-Winkler (same as 1825 matching)
matches_1830s <- list()

cat("Starting matching loop...\n")

for (i in 1:nrow(vt_for_1830s)) {
  vt_row <- vt_for_1830s[i, ]

  # Skip if missing key fields
  if (is.na(vt_row$vt_surname_clean) || vt_row$vt_surname_clean == "") next
  if (is.na(vt_row$vt_first_clean) || vt_row$vt_first_clean == "") next

  search_dists <- map_to_1830s_districts(vt_row$distrik)

  # Find candidates with exact surname match
  candidates <- census_1830s %>%
    filter(surname_clean == vt_row$vt_surname_clean,
           district %in% search_dists)

  if (nrow(candidates) == 0) next

  # Score candidates using Jaro-Winkler
  best_score <- 0
  best_match <- NULL

  for (j in 1:nrow(candidates)) {
    cand <- candidates[j, ]

    # Skip if candidate has missing names
    if (is.na(cand$first_clean) || cand$first_clean == "" ||
        cand$first_clean == "UNKNOWN") next

    # Calculate Jaro-Winkler similarity with error handling
    jw_full <- tryCatch(
      stringdist::stringsim(as.character(vt_row$vt_first_clean),
                            as.character(cand$first_clean),
                            method = "jw", p = 0.1),
      error = function(e) 0
    )

    jw_first <- tryCatch(
      stringdist::stringsim(as.character(vt_row$vt_first_only),
                            as.character(cand$first_only),
                            method = "jw", p = 0.1),
      error = function(e) 0
    )

    # Handle NA values
    if (is.na(jw_full)) jw_full <- 0
    if (is.na(jw_first)) jw_first <- 0

    # Weighted name score
    has_multi_vt <- grepl(" ", vt_row$vt_first_clean)
    has_multi_cand <- grepl(" ", cand$first_clean)

    if (has_multi_vt && has_multi_cand) {
      name_score <- 0.7 * jw_full + 0.3 * jw_first
    } else {
      name_score <- 0.4 * jw_full + 0.6 * jw_first
    }

    # Ensure name_score is not NA
    if (is.na(name_score)) name_score <- 0

    # District score: 1.0 for single expected district, 0.7 for multi-district search
    district_score <- ifelse(length(search_dists) == 1, 1.0, 0.7)

    # Combined score: 90% name, 10% district (consistent with 1825 matching)
    score <- 0.90 * name_score + 0.10 * district_score

    if (score > best_score) {
      best_score <- score
      best_match <- cand
    }
  }

  if (!is.null(best_match) && best_score >= 0.70) {
    matches_1830s[[length(matches_1830s) + 1]] <- data.frame(
      row_id_1830s = vt_row$row_id_1830s,
      vt_surname = vt_row$vt_surname_std,
      vt_name = vt_row$vt_name,
      census_id_1830s = best_match$census_id_1830s,
      census_surname = best_match$surname,
      census_first = best_match$first_name,
      district_1830s = best_match$district,
      match_score = best_score
    )
  }

  # Progress indicator every 200 records
  if (i %% 200 == 0) cat("  Processed", i, "of", nrow(vt_for_1830s), "Voortrekkers\n")
}

cat("Matching loop complete.\n")

# Handle case of no matches
if (length(matches_1830s) == 0) {
  matches_1830s_df <- data.frame(
    row_id_1830s = integer(0),
    vt_surname = character(0),
    vt_name = character(0),
    census_id_1830s = integer(0),
    census_surname = character(0),
    census_first = character(0),
    district_1830s = character(0),
    match_score = numeric(0)
  )
} else {
  matches_1830s_df <- bind_rows(matches_1830s)
}

cat("Voortrekkers matched to 1830s census:", nrow(matches_1830s_df), "\n")
if (nrow(vt_for_1830s) > 0) {
  cat("Match rate:", round(nrow(matches_1830s_df) / nrow(vt_for_1830s) * 100, 1), "%\n")
}

# --------------------------------------------------------------------------
# 12C.3 ANALYZE 1830s MATCHES
# --------------------------------------------------------------------------

if (nrow(matches_1830s_df) >= 20) {

  # Mark matched records in 1830s census
  census_1830s$is_voortrekker <- census_1830s$census_id_1830s %in% matches_1830s_df$census_id_1830s

  cat("\n--- 1830s Census: Voortrekker vs Non-Voortrekker Comparison ---\n")

  outcome_vars_1830s <- c("total_tax", "cattle", "sheep", "horses_trade", "grain_reaped")

  results_1830s <- data.frame()

  cat(sprintf("\n%-20s %12s %12s %12s %10s\n", "Variable", "VT Mean", "Non-VT Mean", "Difference", "p-value"))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  for (v in outcome_vars_1830s) {
    vt_vals <- census_1830s %>% filter(is_voortrekker) %>% pull(!!sym(v))
    nonvt_vals <- census_1830s %>% filter(!is_voortrekker) %>% pull(!!sym(v))

    tt <- t.test(vt_vals, nonvt_vals)
    vt_mean <- mean(vt_vals, na.rm = TRUE)
    nonvt_mean <- mean(nonvt_vals, na.rm = TRUE)
    diff <- vt_mean - nonvt_mean

    cat(sprintf("%-20s %12.2f %12.2f %12.2f %10.4f %s\n",
                v, vt_mean, nonvt_mean, diff, tt$p.value,
                ifelse(tt$p.value < 0.05, "*", "")))

    results_1830s <- rbind(results_1830s, data.frame(
      variable = v, vt_mean = vt_mean, nonvt_mean = nonvt_mean,
      difference = diff, p_value = tt$p.value, method = "1830s Simple"
    ))
  }

  # District FE regression for 1830s
  cat("\n--- 1830s District Fixed Effects Regressions ---\n")

  reg_1830s_results <- data.frame()

  for (v in outcome_vars_1830s) {
    formula <- as.formula(paste(v, "~ is_voortrekker + factor(district)"))
    reg <- lm(formula, data = census_1830s)
    robust_se <- sqrt(diag(vcovHC(reg, type = "HC1")))

    if ("is_voortrekkerTRUE" %in% names(coef(reg))) {
      coef_val <- coef(reg)["is_voortrekkerTRUE"]
      se_val <- robust_se["is_voortrekkerTRUE"]
      p_val <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))["is_voortrekkerTRUE", 4]

      cat(sprintf("%-20s: coef = %8.3f, SE = %8.3f, p = %.4f %s\n",
                  v, coef_val, se_val, p_val, ifelse(p_val < 0.05, "*", "")))

      reg_1830s_results <- rbind(reg_1830s_results, data.frame(
        variable = v, difference = coef_val, robust_se = se_val,
        p_value = p_val, method = "1830s District FE"
      ))
    }
  }

  # --------------------------------------------------------------------------
  # 12C.4 CREATE COMPARISON GRAPH: 1825 vs 1830s
  # --------------------------------------------------------------------------

  # Calculate standardized effect sizes for 1830s
  var_sds_1830s <- census_1830s %>%
    summarise(across(all_of(outcome_vars_1830s), ~sd(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "sd")

  results_1830s_std <- bind_rows(results_1830s, reg_1830s_results) %>%
    left_join(var_sds_1830s, by = "variable") %>%
    mutate(effect_size = difference / sd)

  # Create comparison with 1825 results for overlapping variables
  # Map 1830s variable names to 1825 names
  var_mapping <- c(
    "cattle" = "cattle",
    "sheep" = "sheep",
    "total_tax" = "wealth_index",  # Proxy comparison
    "grain_reaped" = "wheat_reaped"
  )

  # Get 1825 results for comparison (check if exists first)
  if (!exists("all_results_std") || nrow(all_results_std) == 0) {
    cat("Warning: 1825 results not available for comparison.\n")
    results_1825_compare <- data.frame()
  } else {
    results_1825_compare <- all_results_std %>%
      filter(variable %in% c("cattle", "sheep", "wheat_reaped", "wealth_index"),
             method %in% c("District FE", "Exact Match (District)")) %>%
      mutate(census_year = "1825")
  }

  results_1830s_compare <- results_1830s_std %>%
    filter(variable %in% c("cattle", "sheep", "grain_reaped", "total_tax")) %>%
    mutate(
      variable = case_when(
        variable == "grain_reaped" ~ "wheat_reaped",
        variable == "total_tax" ~ "wealth_index",
        TRUE ~ variable
      ),
      census_year = "1830s"
    )

  # Combine for plotting
  if (nrow(results_1825_compare) > 0) {
    comparison_data <- bind_rows(
      results_1825_compare %>% select(variable, method, effect_size, census_year),
      results_1830s_compare %>% select(variable, method, effect_size, census_year)
    )
  } else {
    comparison_data <- results_1830s_compare %>%
      select(variable, method, effect_size, census_year)
  }

  # Create comparison plot if we have data
  if (nrow(comparison_data) > 0) {

    # ----- GRAPH 1: Grouped bar chart by variable -----
    p_1830s <- ggplot(comparison_data,
                      aes(x = variable, y = effect_size, fill = interaction(method, census_year))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "", y = "Standardized Effect Size (SD units)",
           fill = "Method & Year") +
      theme_leap() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      scale_fill_manual(values = LEAP_CYCLE)

    print(p_1830s)
    fig_file <- next_fig("1825_vs_1830s_comparison.png")
    save_leap_fig(fig_file, p_1830s, width = 12, height = 7, dpi = 300)
    # (output handled by save_leap_fig)

    # ----- GRAPH 2: Cleaner side-by-side comparison (District FE only) -----
    # Focus on District FE method for cleaner comparison
    # Note: 1825 has "District FE", 1830s has "1830s District FE"
    comparison_fe <- comparison_data %>%
      filter(grepl("District FE", method)) %>%
      mutate(method = "District FE")  # Standardize method name

    if (nrow(comparison_fe) > 0) {
      # Create nice labels
      comparison_fe <- comparison_fe %>%
        mutate(
          variable_label = case_when(
            variable == "cattle" ~ "Cattle",
            variable == "sheep" ~ "Sheep",
            variable == "wheat_reaped" ~ "Grain/Wheat",
            variable == "wealth_index" ~ "Wealth Proxy\n(PCA/Total Tax)",
            TRUE ~ variable
          ),
          variable_label = factor(variable_label,
                                  levels = c("Cattle", "Sheep", "Grain/Wheat",
                                            "Wealth Proxy\n(PCA/Total Tax)"))
        )

      p_compare_clean <- ggplot(comparison_fe,
                                aes(x = census_year, y = effect_size, fill = census_year)) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#AAAAAA") +
        facet_wrap(~variable_label, nrow = 1, scales = "free_x") +
        labs(x = "Census Period",
             y = "Standardized Effect Size\n(SD units)") +
        theme_leap() +
        theme(
          legend.position = "none",
          strip.text = element_text(face = "bold", size = 11),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10)
        ) +
        scale_fill_manual(values = c("1825" = "#5C2346", "1830s" = "#3D8EB9")) +
        geom_text(aes(label = sprintf("%.2f", effect_size),
                      vjust = ifelse(effect_size >= 0, -0.5, 1.5)),
                  size = 3.5)

      print(p_compare_clean)
      fig_file <- next_fig("1825_vs_1830s_clean.png")
      save_leap_fig(fig_file, p_compare_clean, width = 10, height = 5, dpi = 300)
      # (output handled by save_leap_fig)
    }

    # Figure already saved above

    # Summary comparison
    cat("\n\n=== SUMMARY: 1825 vs 1830s Results ===\n")
    cat("Both censuses show similar patterns if results are robust.\n\n")

    summary_compare <- comparison_data %>%
      select(variable, method, effect_size, census_year) %>%
      pivot_wider(names_from = census_year, values_from = effect_size,
                  names_prefix = "effect_")

    print(summary_compare)
  } else {
    cat("No data available for comparison plot.\n")
  }

} else {
  cat("\nInsufficient matches for 1830s analysis (need >= 20, got", nrow(matches_1830s_df), ")\n")
}


# ============================================================================
# PART 13: ROBUSTNESS - MATCH QUALITY SENSITIVITY
# ============================================================================

cat("\n========== ROBUSTNESS: SENSITIVITY TO MATCH QUALITY ==========\n")

# Re-run the district FE regression using different match score thresholds
thresholds <- c(0.70, 0.80, 0.90)

for (thresh in thresholds) {
  high_quality_ids <- best_matches %>%
    filter(match_score >= thresh) %>%
    pull(census_id)

  test_data <- all_districts %>%
    filter(!is_voortrekker | census_id %in% high_quality_ids) %>%
    mutate(is_vt_strict = census_id %in% high_quality_ids)

  reg <- lm(wealth_index ~ is_vt_strict + factor(district), data = test_data)
  robust_test <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))

  cat(sprintf("\nThreshold >= %.2f: N matched = %d\n", thresh, sum(test_data$is_vt_strict)))
  cat(sprintf("  Wealth index coef: %.3f (SE: %.3f, p: %.4f)\n",
              robust_test["is_vt_strictTRUE", 1],
              robust_test["is_vt_strictTRUE", 2],
              robust_test["is_vt_strictTRUE", 4]))
}

# ============================================================================
# PART 13B: ROBUSTNESS - XGBOOST VS JARO-WINKLER MATCHING
# ============================================================================

# Check if XGBoost was run and produced results
if (exists("xgb_matched_ids") && !is.null(xgb_matched_ids) && length(xgb_matched_ids) > 0) {

  cat("\n========== ROBUSTNESS: XGBOOST vs JW MATCHING ==========\n")

  # Create alternative Voortrekker indicator using XGBoost matches
  all_districts <- all_districts %>%
    mutate(is_voortrekker_xgb = census_id %in% xgb_matched_ids)

  cat("\nMatching method comparison:\n")
  cat("  JW matches:", sum(all_districts$is_voortrekker), "\n")
  cat("  XGBoost matches:", sum(all_districts$is_voortrekker_xgb), "\n")
  cat("  Overlap (both methods):", sum(all_districts$is_voortrekker & all_districts$is_voortrekker_xgb), "\n")
  cat("  JW only:", sum(all_districts$is_voortrekker & !all_districts$is_voortrekker_xgb), "\n")
  cat("  XGBoost only:", sum(!all_districts$is_voortrekker & all_districts$is_voortrekker_xgb), "\n")

  # Run key regressions with XGBoost matches
  cat("\nDistrict FE Regressions: JW vs XGBoost Matching\n")
  cat(sprintf("%-25s %12s %12s %12s %12s\n", "Outcome", "JW Coef", "JW p", "XGB Coef", "XGB p"))
  cat(paste(rep("-", 80), collapse = ""), "\n")

  xgb_reg_results <- data.frame()
  for (v in c("cattle", "sheep", "horses", "total_slaves", "wheat_reaped", "wine", "wealth_index")) {

    # JW regression
    formula_jw <- as.formula(paste0(v, " ~ is_voortrekker + factor(district)"))
    reg_jw <- lm(formula_jw, data = all_districts)
    robust_jw <- coeftest(reg_jw, vcov = vcovHC(reg_jw, type = "HC1"))

    # XGBoost regression
    formula_xgb <- as.formula(paste0(v, " ~ is_voortrekker_xgb + factor(district)"))
    reg_xgb <- lm(formula_xgb, data = all_districts)
    robust_xgb <- coeftest(reg_xgb, vcov = vcovHC(reg_xgb, type = "HC1"))

    jw_coef <- robust_jw["is_voortrekkerTRUE", 1]
    jw_p    <- robust_jw["is_voortrekkerTRUE", 4]
    xgb_coef <- robust_xgb["is_voortrekker_xgbTRUE", 1]
    xgb_p    <- robust_xgb["is_voortrekker_xgbTRUE", 4]

    cat(sprintf("%-25s %12.3f %12.4f %12.3f %12.4f\n", v, jw_coef, jw_p, xgb_coef, xgb_p))

    xgb_reg_results <- rbind(xgb_reg_results, data.frame(
      variable = v,
      jw_coef = jw_coef, jw_p = jw_p,
      xgb_coef = xgb_coef, xgb_p = xgb_p,
      coef_diff = jw_coef - xgb_coef
    ))
  }

  # Summary plot comparing methods
  xgb_plot_data <- xgb_reg_results %>%
    pivot_longer(cols = c(jw_coef, xgb_coef),
                 names_to = "method",
                 values_to = "coefficient") %>%
    mutate(method = ifelse(method == "jw_coef", "Jaro-Winkler", "XGBoost"))

  p_method_compare <- ggplot(xgb_plot_data, aes(x = variable, y = coefficient, fill = method)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "", y = "Coefficient on Voortrekker Indicator",
         fill = "Matching Method") +
    theme_leap() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    scale_fill_manual(values = c("#5C2346", "#3D8EB9"))

  print(p_method_compare)
  fig_file <- next_fig("jw_vs_xgboost_regression.png")
  save_leap_fig(fig_file, p_method_compare, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # Save XGBoost regression results
  write.csv(xgb_reg_results, "output/tables/xgboost_jw_regression_comparison.csv", row.names = FALSE)

  cat("\nConclusion: Results are ", ifelse(
    cor(xgb_reg_results$jw_coef, xgb_reg_results$xgb_coef) > 0.8,
    "ROBUST to matching method choice.",
    "SENSITIVE to matching method - interpret with caution."
  ), "\n")
}


# ============================================================================
# PART 14: DESCRIPTIVE STATISTICS TABLE
# ============================================================================

cat("\n========== DESCRIPTIVE STATISTICS ==========\n")

desc_stats <- all_districts %>%
  group_by(is_voortrekker) %>%
  summarise(
    N = n(),
    across(all_of(c("horses", "cattle", "sheep", "goats", "pigs",
                     "total_slaves", "total_khoe",
                     "wheat_sown", "wheat_reaped",
                     "total_grain_reaped", "wine", "brandy",
                     "wealth_index")),
           list(mean = ~ mean(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                median = ~ median(., na.rm = TRUE)))
  )

cat("\nDescriptive statistics by Voortrekker status:\n")
print(desc_stats %>% t())

# Save results
write.csv(all_results, "output/tables/voortrekker_results_all_methods.csv", row.names = FALSE)
write.csv(best_matches %>% select(row_id, vt_surname, vt_name, name_raw, district = enumeration_district,
                                    match_score, match_quality, census_id),
          "output/tables/voortrekker_matches.csv", row.names = FALSE)

cat("\n\nResults saved to:\n")
cat("  Output/voortrekker_results_all_methods.csv\n")
cat("  Output/voortrekker_matches.csv\n")
cat("  Output/voortrekker_matches_xgboost.csv (if XGBoost ran)\n")
cat("  match_score_distribution.png/.pdf\n")
cat("  wealth_distribution_vt.png/.pdf\n")
cat("  voortrekker_selection_coefficients.png/.pdf\n")
cat("  jw_vs_xgboost_comparison.png/.pdf (if XGBoost ran)\n")
cat("  jw_vs_xgboost_regression_comparison.png/.pdf (if XGBoost ran)\n")
cat("  xgboost_jw_regression_comparison.csv (if XGBoost ran)\n")
cat("  Output/regression_table.txt\n")

# Final summary table
cat("\n\n")
cat("================================================================\n")
cat("                    MATCHING METHOD SUMMARY                      \n")
cat("================================================================\n")
cat(sprintf("%-30s %10s %10s\n", "Metric", "JW", "XGBoost"))
cat(paste(rep("-", 55), collapse = ""), "\n")
cat(sprintf("%-30s %10d %10d\n", "Adult Voortrekkers", nrow(vt_adults), nrow(vt_adults)))

# Check if XGBoost results exist
xgb_ran <- exists("xgb_matched_ids") && !is.null(xgb_matched_ids) &&
           "is_voortrekker_xgb" %in% names(all_districts)

cat(sprintf("%-30s %10d %10s\n", "Good matches",
            sum(all_districts$is_voortrekker),
            ifelse(xgb_ran, as.character(sum(all_districts$is_voortrekker_xgb)), "N/A")))
cat(sprintf("%-30s %9.1f%% %9s\n", "Match rate",
            sum(all_districts$is_voortrekker) / nrow(vt_adults) * 100,
            ifelse(xgb_ran,
                   paste0(round(sum(all_districts$is_voortrekker_xgb) / nrow(vt_adults) * 100, 1), "%"),
                   "N/A")))

cat("\n========== ANALYSIS COMPLETE ==========\n")


# ============================================================================
# PART 15: MATCH RATE COMPARISON VISUALIZATION (RANDOM FOREST)
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("      MATCH RATE COMPARISON: RANDOM FOREST vs ABLATED MODELS     \n")
cat("================================================================\n\n")

# This section compares match rates when using different information:
# a) Male name only (RF with wife features zeroed out)
# b) Male + Wife (full RF model)
# c) Male + Wife + District (full RF model with district features)
#
# The key insight from the paper: "absence of wife makes it far harder to identify a link"
# We expect to see a meaningful improvement when wife info is included.

# --------------------------------------------------------------------------
# 15.1 TRAIN ABLATED RANDOM FOREST MODELS
# --------------------------------------------------------------------------

cat("Calculating match rates under different feature configurations...\n")

# Check if we have the candidates dataframe with all scores
if (exists("candidates_for_analysis") && nrow(candidates_for_analysis) > 0) {
  candidates <- candidates_for_analysis
  cat("Using saved candidates data for analysis.\n")
} else if (!exists("candidates") || nrow(candidates) == 0) {
  cat("Note: Candidates dataframe not available. Run matching first.\n")
  candidates <- NULL
}

if (!is.null(candidates) && exists("candidates") && nrow(candidates) > 0 && rf_model_trained) {

  # --------------------------------------------------------------------------
  # DIAGNOSTIC: Check wife information availability
  # --------------------------------------------------------------------------
  cat("\n--- Wife Information Diagnostics ---\n")
  cat("Total candidate pairs:", nrow(candidates), "\n")
  cat("Candidates with both_have_wife = 1:", sum(candidates$both_have_wife == 1, na.rm = TRUE), "\n")
  cat("Candidates with wife_score available:", sum(!is.na(candidates$wife_score)), "\n")
  cat("Candidates with wife_score >= 0.70:", sum(!is.na(candidates$wife_score) & candidates$wife_score >= 0.70, na.rm = TRUE), "\n")

  # Check how many unique Voortrekkers have wife info in at least one candidate
  vt_with_wife_info <- candidates %>%
    group_by(row_id) %>%
    summarise(
      has_any_wife_info = any(both_have_wife == 1),
      best_wife_score = max(wife_score, na.rm = TRUE),
      .groups = "drop"
    )
  cat("Unique Voortrekkers with wife comparison possible:", sum(vt_with_wife_info$has_any_wife_info, na.rm = TRUE),
      "of", nrow(vt_with_wife_info), "\n")

  # --------------------------------------------------------------------------
  # 15.1.1 MALE ONLY MODEL (Wife features ablated)
  # --------------------------------------------------------------------------

  cat("\nTraining 'Male Only' ablated model (wife features zeroed)...\n")

  # Create feature matrix with wife features set to 0
  candidates_male_only <- candidates %>%
    mutate(
      # Zero out all wife-related features
      vt_has_wife = 0L,
      census_has_wife = 0L,
      both_have_wife = 0L,
      neither_has_wife = 1L,
      wife_mismatch = 0L,
      jw_wife_surname = 0,
      jw_wife_first = 0,
      jw_wife_vs_husband_surname = 0,
      wife_best_surname = 0,    # spouse features
      wife_best_first = 0
    )

  # Train male-only RF on same training data structure
  train_male_only <- candidates_male_only %>%
    filter(!is.na(label))

  if (nrow(train_male_only) >= 100) {
    X_train_male <- as.data.frame(train_male_only[, feature_cols])
    X_train_male <- X_train_male %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
    y_train_male <- as.factor(train_male_only$label)

    set.seed(42)
    rf_male_only <- randomForest(
      x = X_train_male,
      y = y_train_male,
      ntree = 500,
      mtry = floor(sqrt(length(feature_cols))),
      classwt = c("0" = 1, "1" = sum(y_train_male == "0") / sum(y_train_male == "1"))
    )

    cat("  Male-only RF trained. OOB error:", round(rf_male_only$err.rate[500, "OOB"] * 100, 1), "%\n")

    # Predict on ALL candidates (not just male_only ablated ones - we want to see
    # how performance differs when we ADD wife info)
    X_all_male <- as.data.frame(candidates_male_only[, feature_cols])
    X_all_male <- X_all_male %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
    candidates$score_male_only <- predict(rf_male_only, X_all_male, type = "prob")[, "1"]

    male_only_model_trained <- TRUE
  } else {
    cat("  WARNING: Insufficient training data for male-only RF.\n")
    candidates$score_male_only <- candidates$husband_score
    male_only_model_trained <- FALSE
  }

  # --------------------------------------------------------------------------
  # 15.1.2 MALE + WIFE MODEL (District features ablated)
  # --------------------------------------------------------------------------

  cat("Training 'Male + Wife' ablated model (district feature zeroed)...\n")

  # Create feature matrix with district feature set to neutral
  candidates_male_wife <- candidates %>%
    mutate(
      is_primary_district = FALSE  # Neutral district (no bonus/penalty)
    )

  train_male_wife <- candidates_male_wife %>%
    filter(!is.na(label))

  if (nrow(train_male_wife) >= 100) {
    X_train_wife <- as.data.frame(train_male_wife[, feature_cols])
    X_train_wife <- X_train_wife %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
    y_train_wife <- as.factor(train_male_wife$label)

    set.seed(42)
    rf_male_wife <- randomForest(
      x = X_train_wife,
      y = y_train_wife,
      ntree = 500,
      mtry = floor(sqrt(length(feature_cols))),
      classwt = c("0" = 1, "1" = sum(y_train_wife == "0") / sum(y_train_wife == "1"))
    )

    cat("  Male+Wife RF trained. OOB error:", round(rf_male_wife$err.rate[500, "OOB"] * 100, 1), "%\n")

    # Predict on candidates with actual wife features (not ablated)
    X_all_wife <- as.data.frame(candidates_male_wife[, feature_cols])
    X_all_wife <- X_all_wife %>% mutate(across(where(is.logical), as.integer), across(everything(), ~ ifelse(is.na(.), 0, .)))
    candidates$score_male_wife <- predict(rf_male_wife, X_all_wife, type = "prob")[, "1"]

    male_wife_model_trained <- TRUE
  } else {
    cat("  WARNING: Insufficient training data for male+wife RF.\n")
    candidates$score_male_wife <- candidates$rf_score
    male_wife_model_trained <- FALSE
  }

  # Full model score is already stored as rf_score
  candidates$score_full <- candidates$rf_score

  # --------------------------------------------------------------------------
  # 15.2 COMPARE MODELS - SELECT BEST MATCHES
  # --------------------------------------------------------------------------

  cat("\nSelecting best matches under each model...\n")

  # Get best match per Voortrekker for each model
  best_male_only <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(score_male_only)) %>%
    slice(1) %>%
    ungroup()

  best_male_wife <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(score_male_wife)) %>%
    slice(1) %>%
    ungroup()

  best_full <- candidates %>%
    group_by(row_id) %>%
    arrange(desc(score_full)) %>%
    slice(1) %>%
    ungroup()

  # --------------------------------------------------------------------------
  # 15.3 CALCULATE MATCH RATES AT DIFFERENT THRESHOLDS
  # --------------------------------------------------------------------------

  # Use the RF threshold from the paper (0.56)
  RF_THRESH <- RF_THRESHOLD

  match_rate_summary <- data.frame(
    criteria = c("Male Name Only", "Male + Wife Names", "Male + Wife + District"),
    method = c("RF (ablated)", "RF (ablated)", "RF (full)"),
    n_vt = rep(nrow(vt_adults), 3),
    threshold_56 = c(
      sum(best_male_only$score_male_only >= RF_THRESH),
      sum(best_male_wife$score_male_wife >= RF_THRESH),
      sum(best_full$score_full >= RF_THRESH)
    ),
    threshold_70 = c(
      sum(best_male_only$score_male_only >= 0.70),
      sum(best_male_wife$score_male_wife >= 0.70),
      sum(best_full$score_full >= 0.70)
    ),
    threshold_80 = c(
      sum(best_male_only$score_male_only >= 0.80),
      sum(best_male_wife$score_male_wife >= 0.80),
      sum(best_full$score_full >= 0.80)
    )
  ) %>%
    mutate(
      rate_56 = round(threshold_56 / n_vt * 100, 1),
      rate_70 = round(threshold_70 / n_vt * 100, 1),
      rate_80 = round(threshold_80 / n_vt * 100, 1)
    )

  cat("\n========== MATCH RATE COMPARISON ==========\n")
  cat("(Threshold = RF paper threshold of", RF_THRESH, "and comparison at 0.70, 0.80)\n\n")
  cat(sprintf("%-30s %10s %10s %10s\n", "Model", paste0(">=", RF_THRESH), ">=0.70", ">=0.80"))
  cat(paste(rep("-", 65), collapse = ""), "\n")
  for (i in 1:nrow(match_rate_summary)) {
    cat(sprintf("%-30s %9.1f%% %9.1f%% %9.1f%%\n",
                match_rate_summary$criteria[i],
                match_rate_summary$rate_56[i],
                match_rate_summary$rate_70[i],
                match_rate_summary$rate_80[i]))
  }

  # Show improvement from wife info
  cat("\n--- VALUE OF WIFE INFORMATION ---\n")
  wife_improvement_56 <- match_rate_summary$rate_56[2] - match_rate_summary$rate_56[1]
  wife_improvement_70 <- match_rate_summary$rate_70[2] - match_rate_summary$rate_70[1]
  full_improvement_56 <- match_rate_summary$rate_56[3] - match_rate_summary$rate_56[1]
  full_improvement_70 <- match_rate_summary$rate_70[3] - match_rate_summary$rate_70[1]

  cat(sprintf("Adding wife info: +%.1f percentage points (threshold=%.2f)\n", wife_improvement_56, RF_THRESH))
  cat(sprintf("Adding wife info: +%.1f percentage points (threshold=0.70)\n", wife_improvement_70))
  cat(sprintf("Full model vs male-only: +%.1f percentage points (threshold=%.2f)\n", full_improvement_56, RF_THRESH))
  cat(sprintf("Full model vs male-only: +%.1f percentage points (threshold=0.70)\n", full_improvement_70))

  # --------------------------------------------------------------------------
  # 15.4 CALCULATE MATCH RATES BY DISTRICT
  # --------------------------------------------------------------------------

  cat("\nCalculating match rates by district...\n")

  # Get district lookup from vt_adults
  if (!"census_districts" %in% names(vt_adults)) {
    cat("ERROR: census_districts not found in vt_adults.\n")
    valid_lookup <- FALSE
  } else {
    vt_district_lookup <- vt_adults %>%
      mutate(row_id = row_number()) %>%
      select(row_id, census_districts)

    valid_lookup <- nrow(vt_district_lookup) > 0
    if (valid_lookup) {
      district_vec <- setNames(vt_district_lookup$census_districts, vt_district_lookup$row_id)
    }
  }

  if (!valid_lookup) {
    cat("WARNING: Could not create district lookup. Skipping district-level analysis.\n")
    district_rates_rf <- data.frame()
  } else {

    # Add census_districts to best matches
    best_male_only$census_districts <- district_vec[as.character(best_male_only$row_id)]
    best_male_wife$census_districts <- district_vec[as.character(best_male_wife$row_id)]
    best_full$census_districts <- district_vec[as.character(best_full$row_id)]

    district_rates_male_only <- best_male_only %>%
      filter(!is.na(census_districts)) %>%
      mutate(is_matched = score_male_only >= RF_THRESH) %>%
      group_by(census_districts) %>%
      summarise(
        n_vt = n(),
        n_matched = sum(is_matched),
        match_rate = n_matched / n_vt * 100,
        .groups = "drop"
      ) %>%
      mutate(criteria = "Male Only", method = "RF")

    district_rates_male_wife <- best_male_wife %>%
      filter(!is.na(census_districts)) %>%
      mutate(is_matched = score_male_wife >= RF_THRESH) %>%
      group_by(census_districts) %>%
      summarise(
        n_vt = n(),
        n_matched = sum(is_matched),
        match_rate = n_matched / n_vt * 100,
        .groups = "drop"
      ) %>%
      mutate(criteria = "Male + Wife", method = "RF")

    district_rates_full <- best_full %>%
      filter(!is.na(census_districts)) %>%
      mutate(is_matched = score_full >= RF_THRESH) %>%
      group_by(census_districts) %>%
      summarise(
        n_vt = n(),
        n_matched = sum(is_matched),
        match_rate = n_matched / n_vt * 100,
        .groups = "drop"
      ) %>%
      mutate(criteria = "Male + Wife + District", method = "RF")

    # Combine RF results
    district_rates_rf <- bind_rows(
      district_rates_male_only,
      district_rates_male_wife,
      district_rates_full
    )
  }

  district_rates_all <- district_rates_rf

  # --------------------------------------------------------------------------
  # 15.5 ADD 1830s RESULTS (if available)
  # --------------------------------------------------------------------------

  if (exists("matches_1830s_df") && nrow(matches_1830s_df) > 0) {

    cat("\nCalculating 1830s match rates...\n")

    # Get district info for 1830s matched Voortrekkers
    district_rates_1830s <- vt_for_1830s %>%
      mutate(
        is_matched = row_id_1830s %in% matches_1830s_df$row_id_1830s,
        # Map distrik to standard district names
        district_std = case_when(
          grepl("BEAUFORT", toupper(distrik)) ~ "Beaufort",
          grepl("GRAAFF|GRAAF", toupper(distrik)) ~ "Graaff-Reinet",
          grepl("SWELLENDAM", toupper(distrik)) ~ "Swellendam",
          grepl("WORCESTER", toupper(distrik)) ~ "Worcester",
          grepl("SOMERSET", toupper(distrik)) ~ "Somerset_multi",
          grepl("COLESBERG", toupper(distrik)) ~ "Colesberg_multi",
          grepl("UITENHAGE", toupper(distrik)) ~ "Uitenhage",
          grepl("ALBANY", toupper(distrik)) ~ "Albany",
          grepl("CRADOCK", toupper(distrik)) ~ "Cradock",
          TRUE ~ "Other"
        )
      ) %>%
      group_by(district_std) %>%
      summarise(
        n_vt = n(),
        n_matched = sum(is_matched),
        match_rate = n_matched / n_vt * 100,
        .groups = "drop"
      ) %>%
      rename(census_districts = district_std) %>%
      mutate(criteria = "1830s Census", method = "JW")

    district_rates_all <- bind_rows(district_rates_all, district_rates_1830s)
  }

  # --------------------------------------------------------------------------
  # 15.5 CREATE HEATMAP VISUALIZATION
  # --------------------------------------------------------------------------

  # Check if we have district-level data to visualize
  if (!exists("district_rates_all") || nrow(district_rates_all) == 0) {
    cat("\nNo district-level match rate data available. Skipping visualizations.\n")
  } else {

  cat("\nCreating match rate heatmap...\n")

  # Clean up district names for display
  district_rates_all <- district_rates_all %>%
    filter(!is.na(census_districts) & census_districts != "") %>%
    mutate(
      district_clean = str_replace_all(census_districts, "_multi", "*"),
      # Create combined method-criteria label
      method_criteria = paste0(method, ": ", criteria)
    )

  # Order districts by overall match rate (highest first)
  district_order <- district_rates_all %>%
    group_by(district_clean) %>%
    summarise(avg_rate = mean(match_rate, na.rm = TRUE)) %>%
    arrange(desc(avg_rate)) %>%
    pull(district_clean)

  district_rates_all$district_clean <- factor(district_rates_all$district_clean,
                                               levels = rev(district_order))

  # Order method-criteria combinations logically
  method_criteria_order <- c(
    "RF: Male Only",
    "RF: Male + Wife",
    "RF: Male + Wife + District",
    "JW: 1830s Census"
  )

  district_rates_all$method_criteria <- factor(
    district_rates_all$method_criteria,
    levels = method_criteria_order[method_criteria_order %in% unique(district_rates_all$method_criteria)]
  )

  # Create main heatmap
  p_heatmap <- ggplot(district_rates_all %>% filter(n_vt >= 5),  # Filter small samples
                      aes(x = method_criteria, y = district_clean, fill = match_rate)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", match_rate)),
              color = "black", size = 3) +
    scale_fill_gradient2(
      low = "#F5F3F0",
      mid = "#3D8EB9",
      high = "#5C2346",
      midpoint = 50,
      limits = c(0, 100),
      name = "Match\nRate (%)"
    ) +
    labs(
      x = "",
      y = "Voortrekker Origin District"
    ) +
    theme_leap() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "#5A5A5A")
    )

  print(p_heatmap)
  fig_file <- next_fig("match_rate_heatmap.png")
  save_leap_fig(fig_file, p_heatmap, width = 12, height = 8, dpi = 300)
  # (output handled by save_leap_fig)

  # --------------------------------------------------------------------------
  # 15.6 CREATE SUMMARY BAR CHART (Overall rates by criteria/method)
  # --------------------------------------------------------------------------

  overall_rates <- district_rates_all %>%
    group_by(method_criteria, method, criteria) %>%
    summarise(
      total_vt = sum(n_vt),
      total_matched = sum(n_matched),
      overall_rate = total_matched / total_vt * 100,
      .groups = "drop"
    )

  p_overall <- ggplot(overall_rates, aes(x = method_criteria, y = overall_rate, fill = method)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", overall_rate)),
              vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("RF" = "#5C2346", "JW" = "#3D8EB9")) +
    labs(
      x = "",
      y = "Overall Match Rate (%)",
      fill = "Method"
    ) +
    theme_leap() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ylim(0, max(overall_rates$overall_rate) * 1.15)

  print(p_overall)
  fig_file <- next_fig("match_rate_overall.png")
  save_leap_fig(fig_file, p_overall, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # --------------------------------------------------------------------------
  # 15.7 CREATE CRITERIA IMPROVEMENT VISUALIZATION
  # --------------------------------------------------------------------------

  # Show how match rates improve as we add criteria
  criteria_progression <- data.frame(
    criteria = factor(c("Male Only", "Male + Wife", "Male + Wife + District"),
                      levels = c("Male Only", "Male + Wife", "Male + Wife + District")),
    rate_70 = c(
      sum(best_male_only$score_male_only >= 0.70),
      sum(best_male_wife$score_male_wife >= 0.70),
      sum(best_full$score_full >= 0.70)
    ),
    rate_80 = c(
      sum(best_male_only$score_male_only >= 0.80),
      sum(best_male_wife$score_male_wife >= 0.80),
      sum(best_full$score_full >= 0.80)
    ),
    rate_90 = c(
      sum(best_male_only$score_male_only >= 0.90),
      sum(best_male_wife$score_male_wife >= 0.90),
      sum(best_full$score_full >= 0.90)
    )
  ) %>%
    mutate(across(starts_with("rate_"), ~ . / nrow(vt_adults) * 100))

  criteria_long <- criteria_progression %>%
    pivot_longer(cols = starts_with("rate_"),
                 names_to = "threshold",
                 values_to = "match_rate") %>%
    mutate(threshold = case_when(
      threshold == "rate_70" ~ "Score >= 0.70 (Fair+)",
      threshold == "rate_80" ~ "Score >= 0.80 (Good+)",
      threshold == "rate_90" ~ "Score >= 0.90 (Excellent)"
    ))

  p_progression <- ggplot(criteria_long,
                          aes(x = criteria, y = match_rate, fill = threshold)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", match_rate)),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("#5C2346", "#3D8EB9", "#6B8E5E")) +
    labs(
      x = "Matching Criteria",
      y = "Match Rate (%)",
      fill = "Quality Threshold"
    ) +
    theme_leap() +
    theme(legend.position = "bottom") +
    ylim(0, max(criteria_long$match_rate) * 1.15)

  print(p_progression)
  fig_file <- next_fig("match_rate_criteria_progression.png")
  save_leap_fig(fig_file, p_progression, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # --------------------------------------------------------------------------
  # 15.8 CREATE COMBINED MULTI-PANEL FIGURE
  # --------------------------------------------------------------------------

  # Use patchwork or gridExtra if available, otherwise create separate
  if (require(patchwork, quietly = TRUE)) {

    combined_fig <- (p_overall / p_progression) | p_heatmap

    print(combined_fig)
    fig_file <- next_fig("match_rate_comprehensive.png")
    save_leap_fig(fig_file, combined_fig, width = 16, height = 10, dpi = 300)
    # (output handled by save_leap_fig)

  } else if (require(gridExtra, quietly = TRUE)) {

    combined_fig <- grid.arrange(
      p_overall, p_progression, p_heatmap,
      ncol = 2, nrow = 2,
      layout_matrix = rbind(c(1, 3), c(2, 3))
    )

    fig_file <- next_fig("match_rate_comprehensive.png")
    save_leap_fig(fig_file, combined_fig, width = 16, height = 10, dpi = 300)
    # (output handled by save_leap_fig)

  } else {
    cat("\nNote: Install 'patchwork' or 'gridExtra' package for combined multi-panel figure.\n")
    cat("Individual figures have been saved separately.\n")
  }

  # --------------------------------------------------------------------------
  # 15.9 PRINT SUMMARY TABLE
  # --------------------------------------------------------------------------

  cat("\n\n========== MATCH RATE SUMMARY TABLE ==========\n")

  summary_table <- district_rates_all %>%
    select(district_clean, method_criteria, match_rate, n_vt) %>%
    pivot_wider(
      names_from = method_criteria,
      values_from = c(match_rate, n_vt),
      names_glue = "{method_criteria}_{.value}"
    )

  cat("\nMatch rates by district and method (%):\n")
  print(summary_table, width = Inf)

  # Save summary data
  write.csv(district_rates_all, "output/tables/match_rates_by_district_method.csv", row.names = FALSE)
  cat("\nData saved to: match_rates_by_district_method.csv\n")

  }  # End of else block (district_rates_all available)

} else {
  # RF model not trained or candidates not available - provide fallback message
  cat("\n========== MATCH RATE ANALYSIS ==========\n")
  if (!exists("rf_model_trained") || !rf_model_trained) {
    cat("Note: Random Forest model was not trained. Using fallback JW-based scores.\n")
    cat("Match rate comparison uses Jaro-Winkler with bonus scoring.\n")
  } else {
    cat("Note: Candidates data not available. Run matching first.\n")
  }
}

cat("\n========== MATCH RATE ANALYSIS COMPLETE ==========\n")


# ============================================================================
# PART 15B: SELECTION BY LEADER AND DESTINATION
# ============================================================================
# NOTE: This section is now superseded by the expanded analysis in PART 12D,
# which includes comprehensive leader name cleaning (JW fuzzy matching),
# representativeness tests, pairwise comparisons, and the Retief narrative test.
# Skipping to avoid duplicate figure generation.

cat("\n\nPART 15B skipped: Leader/destination analysis now in PART 12D.\n")
if (FALSE) {  # Wrapped in if(FALSE) to skip execution
# This section analyzes whether there was differential selection into the Trek
# by which leader they followed and where they ultimately settled.

# --------------------------------------------------------------------------
# 15B.1 SELECTION BY LEADER (MOVE_WITH)
# --------------------------------------------------------------------------

if ("move_with" %in% names(best_matches) || "move_with" %in% names(vt_adults)) {

  cat("Analyzing selection by trek leader...\n")

  # Get standardized leader info from vt_adults and merge with matched data
  # Use leader_combined (from Section 2.2 standardization), NOT raw move_with
  if (!"leader_combined" %in% names(best_matches)) {
    leader_lookup <- vt_adults %>%
      select(row_id, move_with, leader_std, leader_combined)

    best_matches <- best_matches %>%
      left_join(leader_lookup, by = "row_id")
  }

  # Join with census wealth data
  leader_analysis <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    left_join(
      all_districts %>% select(census_id, wealth_index, cattle, sheep, horses, total_slaves),
      by = "census_id"
    ) %>%
    filter(!is.na(leader_combined) & !is.na(wealth_index)) %>%
    # Exclude Family/Independent and Other — these are not trek leaders
    filter(!leader_combined %in% c("Family/Independent", "Other", "Minor Leader")) %>%
    mutate(leader = leader_combined)

  # Get top leaders by number of matched Voortrekkers
  top_leaders <- leader_analysis %>%
    count(leader, sort = TRUE) %>%
    filter(n >= 5) %>%  # Minimum 5 observations
    pull(leader)

  if (length(top_leaders) >= 3) {

    # Calculate means and CIs for top leaders
    leader_stats <- leader_analysis %>%
      filter(leader %in% top_leaders) %>%
      group_by(leader) %>%
      summarise(
        n = n(),
        mean_wealth = mean(wealth_index, na.rm = TRUE),
        se_wealth = sd(wealth_index, na.rm = TRUE) / sqrt(n()),
        ci_lower = mean_wealth - 1.96 * se_wealth,
        ci_upper = mean_wealth + 1.96 * se_wealth,
        mean_cattle = mean(cattle, na.rm = TRUE),
        mean_sheep = mean(sheep, na.rm = TRUE),
        mean_slaves = mean(total_slaves, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_wealth))

    # Non-Voortrekker mean for reference line
    non_vt_mean_wealth <- all_districts %>%
      filter(!is_voortrekker) %>%
      summarise(mean_wealth = mean(wealth_index, na.rm = TRUE)) %>%
      pull(mean_wealth)

    cat("\nWealth by Trek Leader (Top 8):\n")
    cat(sprintf("%-25s %6s %10s %10s\n", "Leader", "N", "Mean Wealth", "95% CI"))
    cat(paste(rep("-", 55), collapse = ""), "\n")
    for (i in 1:nrow(leader_stats)) {
      cat(sprintf("%-25s %6d %10.2f [%5.2f, %5.2f]\n",
                  substr(leader_stats$leader[i], 1, 25),
                  leader_stats$n[i],
                  leader_stats$mean_wealth[i],
                  leader_stats$ci_lower[i],
                  leader_stats$ci_upper[i]))
    }
    cat(sprintf("\n%-25s %6s %10.2f\n", "Non-Voortrekkers", "", non_vt_mean_wealth))

    # Create plot
    leader_stats$leader <- factor(leader_stats$leader, levels = leader_stats$leader)

    p_leader <- ggplot(leader_stats, aes(x = leader, y = mean_wealth)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      geom_hline(yintercept = non_vt_mean_wealth, linetype = "dashed", color = "#AAAAAA") +
      geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3) +
      labs(
        x = "Trek Leader",
        y = "Mean Wealth Index"
      ) +
      theme_leap() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      annotate("text", x = length(top_leaders), y = non_vt_mean_wealth,
               label = "Non-Voortrekker mean", hjust = 1, vjust = -0.5, color = "#AAAAAA", size = 3)

    print(p_leader)
    fig_file <- next_fig("selection_by_leader.png")
    save_leap_fig(fig_file, p_leader, width = 10, height = 6, dpi = 300)
    # (output handled by save_leap_fig)

  } else {
    cat("Insufficient data by leader for analysis (need at least 3 leaders with 5+ observations).\n")
  }

} else {
  cat("Note: 'move_with' (leader) column not available in data.\n")
}

# --------------------------------------------------------------------------
# 15B.2 SELECTION BY DESTINATION (MOVE_TO)
# --------------------------------------------------------------------------

if ("move_to" %in% names(best_matches) || "move_to" %in% names(vt_adults)) {

  cat("\nAnalyzing selection by destination...\n")

  # Get destination info from vt_adults and merge with matched data
  if (!"move_to" %in% names(best_matches)) {
    dest_lookup <- vt_adults %>%
      select(row_id, move_to)

    best_matches <- best_matches %>%
      left_join(dest_lookup, by = "row_id")
  }

  # Join with census wealth data
  dest_analysis <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    left_join(
      all_districts %>% select(census_id, wealth_index, cattle, sheep, horses, total_slaves),
      by = "census_id"
    ) %>%
    filter(!is.na(move_to) & move_to != "" & !is.na(wealth_index))

  # Clean and standardize destination names
  dest_analysis <- dest_analysis %>%
    mutate(
      destination = toupper(trimws(move_to)),
      destination = str_replace_all(destination, "\\s+", " "),
      # Group similar destinations
      destination_group = case_when(
        grepl("NATAL|DURBAN|PIETERMARITZ", destination) ~ "Natal",
        grepl("TRANSVAAL|PRETORIA|POTCHEFSTROOM|RUSTENBURG|LYDENBURG", destination) ~ "Transvaal",
        grepl("ORANGE|VRYSTAAT|BLOEMFONTEIN|WINBURG|HARRISMITH", destination) ~ "Orange Free State",
        grepl("GRIQUA", destination) ~ "Griqualand",
        TRUE ~ "Other/Unknown"
      )
    )

  # Calculate stats by destination group
  dest_stats <- dest_analysis %>%
    group_by(destination_group) %>%
    summarise(
      n = n(),
      mean_wealth = mean(wealth_index, na.rm = TRUE),
      se_wealth = sd(wealth_index, na.rm = TRUE) / sqrt(n()),
      ci_lower = mean_wealth - 1.96 * se_wealth,
      ci_upper = mean_wealth + 1.96 * se_wealth,
      mean_cattle = mean(cattle, na.rm = TRUE),
      mean_sheep = mean(sheep, na.rm = TRUE),
      mean_slaves = mean(total_slaves, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%  # Minimum 5 observations
    arrange(desc(mean_wealth))

  if (nrow(dest_stats) >= 2) {

    # Non-Voortrekker mean for reference
    non_vt_mean_wealth <- all_districts %>%
      filter(!is_voortrekker) %>%
      summarise(mean_wealth = mean(wealth_index, na.rm = TRUE)) %>%
      pull(mean_wealth)

    cat("\nWealth by Destination:\n")
    cat(sprintf("%-25s %6s %10s %10s\n", "Destination", "N", "Mean Wealth", "95% CI"))
    cat(paste(rep("-", 55), collapse = ""), "\n")
    for (i in 1:nrow(dest_stats)) {
      cat(sprintf("%-25s %6d %10.2f [%5.2f, %5.2f]\n",
                  dest_stats$destination_group[i],
                  dest_stats$n[i],
                  dest_stats$mean_wealth[i],
                  dest_stats$ci_lower[i],
                  dest_stats$ci_upper[i]))
    }
    cat(sprintf("\n%-25s %6s %10.2f\n", "Non-Voortrekkers", "", non_vt_mean_wealth))

    # Create plot
    dest_stats$destination_group <- factor(dest_stats$destination_group,
                                            levels = dest_stats$destination_group)

    p_dest <- ggplot(dest_stats, aes(x = destination_group, y = mean_wealth)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      geom_hline(yintercept = non_vt_mean_wealth, linetype = "dashed", color = "#AAAAAA") +
      geom_text(aes(label = paste0("n=", n)), vjust = -1.5, size = 3) +
      labs(
        x = "Final Destination",
        y = "Mean Wealth Index"
      ) +
      theme_leap() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      annotate("text", x = nrow(dest_stats), y = non_vt_mean_wealth,
               label = "Non-Voortrekker mean", hjust = 1, vjust = -0.5, color = "#AAAAAA", size = 3)

    print(p_dest)
    fig_file <- next_fig("selection_by_destination.png")
    save_leap_fig(fig_file, p_dest, width = 10, height = 6, dpi = 300)
    # (output handled by save_leap_fig)

  } else {
    cat("Insufficient data by destination for analysis (need at least 2 destinations with 5+ observations).\n")
  }

} else {
  cat("Note: 'move_to' (destination) column not available in data.\n")
}

cat("\n========== LEADER/DESTINATION ANALYSIS COMPLETE ==========\n")
}  # End of if(FALSE) block wrapping old PART 15B


# ============================================================================
# PART 15C: TIMING OF MIGRATION - WHO MOVED FIRST?
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("      TIMING OF MIGRATION: SELECTION INTO EARLY VS LATE TREK    \n")
cat("================================================================\n\n")

# This section analyzes what factors predicted WHEN Voortrekkers left.
# Did wealthier people leave earlier? Those with more slaves? From certain districts?

# --------------------------------------------------------------------------
# 15C.1 PREPARE DATA FOR TIMING ANALYSIS
# --------------------------------------------------------------------------

if ("move_year" %in% names(best_matches) || "move_year" %in% names(vt_adults)) {

  cat("Analyzing timing of migration...\n")

  # Get move_year from vt_adults if not already in best_matches
  if (!"move_year" %in% names(best_matches)) {
    year_lookup <- vt_adults %>%
      mutate(row_id = row_number()) %>%
      select(row_id, move_year)

    best_matches <- best_matches %>%
      left_join(year_lookup, by = "row_id")
  }

  # Join with census wealth data
  timing_analysis <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    left_join(
      all_districts %>% select(census_id, wealth_index, cattle, sheep, horses,
                                total_slaves, total_khoe, settler_children,
                                wheat_reaped, wine, district),
      by = "census_id"
    ) %>%
    filter(!is.na(move_year) & move_year >= 1835 & move_year <= 1845)

  cat("Voortrekkers with valid move_year (1835-1845):", nrow(timing_analysis), "\n")

  if (nrow(timing_analysis) >= 50) {

    # --------------------------------------------------------------------------
    # 15C.2 DESCRIPTIVE: WEALTH BY YEAR OF MIGRATION
    # --------------------------------------------------------------------------

    cat("\nDescriptive statistics by year of migration:\n")

    year_stats <- timing_analysis %>%
      group_by(move_year) %>%
      summarise(
        n = n(),
        mean_wealth = mean(wealth_index, na.rm = TRUE),
        se_wealth = sd(wealth_index, na.rm = TRUE) / sqrt(n()),
        ci_lower = mean_wealth - 1.96 * se_wealth,
        ci_upper = mean_wealth + 1.96 * se_wealth,
        mean_cattle = mean(cattle, na.rm = TRUE),
        mean_sheep = mean(sheep, na.rm = TRUE),
        mean_slaves = mean(total_slaves, na.rm = TRUE),
        mean_children = mean(settler_children, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n >= 5)  # At least 5 observations per year

    cat(sprintf("\n%-6s %6s %10s %10s %10s %10s\n",
                "Year", "N", "Wealth", "Cattle", "Slaves", "Children"))
    cat(paste(rep("-", 60), collapse = ""), "\n")
    for (i in 1:nrow(year_stats)) {
      cat(sprintf("%-6d %6d %10.2f %10.1f %10.1f %10.1f\n",
                  year_stats$move_year[i],
                  year_stats$n[i],
                  year_stats$mean_wealth[i],
                  year_stats$mean_cattle[i],
                  year_stats$mean_slaves[i],
                  year_stats$mean_children[i]))
    }

    # Plot: Wealth over time
    if (nrow(year_stats) >= 3) {
      p_wealth_year <- ggplot(year_stats, aes(x = move_year, y = mean_wealth)) +
        geom_point(aes(size = n), color = "#5C2346") +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
        geom_smooth(method = "lm", se = TRUE, color = "#A34466", linetype = "dashed") +
        scale_size_continuous(name = "N", range = c(2, 8)) +
        labs(
          x = "Year of Migration",
          y = "Mean Wealth Index"
        ) +
        theme_leap() +
        scale_x_continuous(breaks = seq(1835, 1845, 1))

      print(p_wealth_year)
      fig_file <- next_fig("wealth_by_migration_year.png")
      save_leap_fig(fig_file, p_wealth_year, width = 10, height = 6, dpi = 300)
      # (output handled by save_leap_fig)
    }

    # --------------------------------------------------------------------------
    # 15C.3 REGRESSION: PREDICTORS OF EARLIER MIGRATION
    # --------------------------------------------------------------------------

    cat("\n--- Regression Analysis: What Predicted Earlier Migration? ---\n")

    # Prepare data for regression
    timing_reg_data <- timing_analysis %>%
      mutate(
        # Standardize continuous variables for interpretation
        wealth_std = scale(wealth_index)[,1],
        cattle_std = scale(cattle)[,1],
        sheep_std = scale(sheep)[,1],
        slaves_std = scale(total_slaves)[,1],
        children_std = scale(settler_children)[,1],
        # Log wealth for robustness
        log_wealth = log1p(wealth_index),
        log_cattle = log1p(cattle),
        log_slaves = log1p(total_slaves),
        # District factor
        district_f = factor(district)
      ) %>%
      filter(!is.na(wealth_index) & !is.na(district))

    cat("\nObservations for regression:", nrow(timing_reg_data), "\n")

    # Model 1: Basic - just wealth
    model1 <- lm(move_year ~ wealth_std, data = timing_reg_data)

    # Model 2: Add district FE
    model2 <- lm(move_year ~ wealth_std + district_f, data = timing_reg_data)

    # Model 3: Multiple wealth components
    model3 <- lm(move_year ~ cattle_std + sheep_std + slaves_std + district_f,
                 data = timing_reg_data)

    # Model 4: Full model with family size
    model4 <- lm(move_year ~ wealth_std + slaves_std + children_std + district_f,
                 data = timing_reg_data)

    # Robust standard errors
    robust1 <- coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
    robust2 <- coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
    robust3 <- coeftest(model3, vcov = vcovHC(model3, type = "HC1"))
    robust4 <- coeftest(model4, vcov = vcovHC(model4, type = "HC1"))

    cat("\n========== TIMING OF MIGRATION REGRESSIONS ==========\n")
    cat("Outcome: Year of Migration (earlier = lower value)\n")
    cat("Negative coefficient = associated with EARLIER migration\n\n")

    cat(sprintf("%-20s %12s %12s %12s %12s\n", "Variable", "(1)", "(2)", "(3)", "(4)"))
    cat(paste(rep("-", 72), collapse = ""), "\n")

    # Extract coefficients for key variables
    vars_to_show <- c("wealth_std", "cattle_std", "sheep_std", "slaves_std", "children_std")

    for (v in vars_to_show) {
      coefs <- c(
        ifelse(v %in% rownames(robust1), sprintf("%.3f", robust1[v, 1]), ""),
        ifelse(v %in% rownames(robust2), sprintf("%.3f", robust2[v, 1]), ""),
        ifelse(v %in% rownames(robust3), sprintf("%.3f", robust3[v, 1]), ""),
        ifelse(v %in% rownames(robust4), sprintf("%.3f", robust4[v, 1]), "")
      )
      ses <- c(
        ifelse(v %in% rownames(robust1), sprintf("(%.3f)", robust1[v, 2]), ""),
        ifelse(v %in% rownames(robust2), sprintf("(%.3f)", robust2[v, 2]), ""),
        ifelse(v %in% rownames(robust3), sprintf("(%.3f)", robust3[v, 2]), ""),
        ifelse(v %in% rownames(robust4), sprintf("(%.3f)", robust4[v, 2]), "")
      )
      stars <- c(
        ifelse(v %in% rownames(robust1) && robust1[v, 4] < 0.05, "*", ""),
        ifelse(v %in% rownames(robust2) && robust2[v, 4] < 0.05, "*", ""),
        ifelse(v %in% rownames(robust3) && robust3[v, 4] < 0.05, "*", ""),
        ifelse(v %in% rownames(robust4) && robust4[v, 4] < 0.05, "*", "")
      )

      var_label <- case_when(
        v == "wealth_std" ~ "Wealth (std)",
        v == "cattle_std" ~ "Cattle (std)",
        v == "sheep_std" ~ "Sheep (std)",
        v == "slaves_std" ~ "Slaves (std)",
        v == "children_std" ~ "Children (std)",
        TRUE ~ v
      )

      cat(sprintf("%-20s %11s%s %11s%s %11s%s %11s%s\n",
                  var_label, coefs[1], stars[1], coefs[2], stars[2],
                  coefs[3], stars[3], coefs[4], stars[4]))
      if (any(ses != "")) {
        cat(sprintf("%-20s %12s %12s %12s %12s\n", "", ses[1], ses[2], ses[3], ses[4]))
      }
    }

    cat(paste(rep("-", 72), collapse = ""), "\n")
    cat(sprintf("%-20s %12s %12s %12s %12s\n", "District FE", "No", "Yes", "Yes", "Yes"))
    cat(sprintf("%-20s %12d %12d %12d %12d\n", "N",
                nrow(model1$model), nrow(model2$model), nrow(model3$model), nrow(model4$model)))
    cat(sprintf("%-20s %12.3f %12.3f %12.3f %12.3f\n", "R-squared",
                summary(model1)$r.squared, summary(model2)$r.squared,
                summary(model3)$r.squared, summary(model4)$r.squared))
    cat("\n* p < 0.05. Robust standard errors in parentheses.\n")

    # --------------------------------------------------------------------------
    # 15C.4 COEFFICIENT PLOT
    # --------------------------------------------------------------------------

    # Create coefficient plot from Model 4 (or best model)
    coef_data <- data.frame(
      variable = c("Wealth", "Slaves", "Children"),
      coef = c(robust4["wealth_std", 1], robust4["slaves_std", 1], robust4["children_std", 1]),
      se = c(robust4["wealth_std", 2], robust4["slaves_std", 2], robust4["children_std", 2])
    ) %>%
      mutate(
        ci_lower = coef - 1.96 * se,
        ci_upper = coef + 1.96 * se,
        variable = factor(variable, levels = rev(variable))
      )

    p_timing_coef <- ggplot(coef_data, aes(x = coef, y = variable)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA") +
      geom_point(size = 3, color = "#5C2346") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
      labs(
        x = "Coefficient (Effect on Migration Year)",
        y = "",
        caption = "Negative = earlier migration. 95% CI shown. District FE included."
      ) +
      theme_leap() +
      theme(
        axis.text.y = element_text(size = 11),
        plot.caption = element_text(hjust = 0, size = 9, color = "#5A5A5A")
      )

    print(p_timing_coef)
    fig_file <- next_fig("migration_timing_coefficients.png")
    save_leap_fig(fig_file, p_timing_coef, width = 8, height = 5, dpi = 300)
    # (output handled by save_leap_fig)

    # --------------------------------------------------------------------------
    # 15C.5 DISTRICT-LEVEL TIMING
    # --------------------------------------------------------------------------

    cat("\n--- Migration Timing by Origin District ---\n")

    district_timing <- timing_analysis %>%
      group_by(district) %>%
      summarise(
        n = n(),
        mean_year = mean(move_year, na.rm = TRUE),
        median_year = median(move_year, na.rm = TRUE),
        earliest = min(move_year, na.rm = TRUE),
        latest = max(move_year, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n >= 5) %>%
      arrange(mean_year)

    cat(sprintf("\n%-20s %6s %10s %10s %10s\n",
                "District", "N", "Mean Year", "Earliest", "Latest"))
    cat(paste(rep("-", 60), collapse = ""), "\n")
    for (i in 1:nrow(district_timing)) {
      cat(sprintf("%-20s %6d %10.1f %10d %10d\n",
                  district_timing$district[i],
                  district_timing$n[i],
                  district_timing$mean_year[i],
                  district_timing$earliest[i],
                  district_timing$latest[i]))
    }

    # Plot: Box plot of migration year by district
    if (nrow(district_timing) >= 3) {
      # Order districts by mean year
      timing_analysis$district <- factor(timing_analysis$district,
                                          levels = district_timing$district)

      p_district_timing <- ggplot(timing_analysis %>% filter(district %in% district_timing$district),
                                   aes(x = district, y = move_year)) +
        geom_boxplot(fill = "#5C2346", alpha = 0.3, outlier.alpha = 0.5) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
        labs(
          x = "Census District",  # plotted by census enumeration district
          y = "Year of Migration"
        ) +
        theme_leap() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(1835, 1845, 1))

      print(p_district_timing)
      fig_file <- next_fig("migration_timing_by_district.png")
      save_leap_fig(fig_file, p_district_timing, width = 10, height = 6, dpi = 300)
      # (output handled by save_leap_fig)
    }

    # Save timing regression results
    timing_results <- data.frame(
      model = c("(1) Basic", "(2) + District FE", "(3) Wealth Components", "(4) Full"),
      wealth_coef = c(robust1["wealth_std", 1], robust2["wealth_std", 1], NA, robust4["wealth_std", 1]),
      wealth_p = c(robust1["wealth_std", 4], robust2["wealth_std", 4], NA, robust4["wealth_std", 4]),
      slaves_coef = c(NA, NA, robust3["slaves_std", 1], robust4["slaves_std", 1]),
      slaves_p = c(NA, NA, robust3["slaves_std", 4], robust4["slaves_std", 4]),
      children_coef = c(NA, NA, NA, robust4["children_std", 1]),
      children_p = c(NA, NA, NA, robust4["children_std", 4]),
      r_squared = c(summary(model1)$r.squared, summary(model2)$r.squared,
                    summary(model3)$r.squared, summary(model4)$r.squared),
      n = c(nrow(model1$model), nrow(model2$model), nrow(model3$model), nrow(model4$model))
    )

    write.csv(timing_results, "output/tables/migration_timing_regressions.csv", row.names = FALSE)
    cat("\nRegression results saved to: migration_timing_regressions.csv\n")

    # ------------------------------------------------------------------------
    # 15C.6 TENURE / RECENCY-OF-ARRIVAL PROXY
    # For matched Voortrekkers, map the genealogical birth/baptism place to an
    # 1825 census district and flag households whose head was born or baptised
    # outside the district where the household was enumerated in 1825. This is
    # a recency-of-mobility proxy available for the trekker side only (the
    # census records no birthplaces), so it enters the timing regressions and
    # a wealth-on-recency check within the matched VT sample.
    # Coverage is limited (~30% of genealogy adults record either place);
    # results are reported as suggestive, with coverage disclosed.
    # ------------------------------------------------------------------------

    cat("\n--- 15C.6: Tenure / recency-of-arrival proxy ---\n")

    map_place_to_district <- function(x) {
      x <- toupper(trimws(as.character(x)))
      dplyr::case_when(
        is.na(x) | x == "" ~ NA_character_,
        # Foreign-born: unambiguously outside any Cape district
        str_detect(x, "GERMANY|NEDERLAND|HOLLAND|FRANCE|SCOTLAND|ENGLAND|IRELAND|ILE DE FRANCE|PRUSSIA|DENMARK") ~ "Foreign",
        # District names (any spelling variant, anywhere in the string, so
        # "RENOSTERBERG, GRAAFF-REINET" resolves via its district suffix)
        str_detect(x, "GRAAFF?-? ?REINETT?") ~ "Graaff-Reinet",
        str_detect(x, "SWELLENDAM|SWELLRNDAM|GROOTVADERSBOS|KLEINRIVIERSKLOOF|CALEDON") ~ "Swellendam",
        str_detect(x, "TULBAGH|TULBACH|ROODEZAND") ~ "Worcester",
        str_detect(x, "WORCESTER") ~ "Worcester",
        str_detect(x, "STELLE?N?BOSCH|PAARL|DRAKENSTEIN|DRANKENSTEIN|FRANSCHHOEK|MALMESBURY|SWARTLAND") ~ "Stellenbosch",
        str_detect(x, "CAPE TOWN|KAAPSTAD|RONDEBOSCH|NOORDHOEK|CAPE DISTRICT|WYNBERG|SIMONS") ~ "Cape",
        str_detect(x, "GEORGE") ~ "George",
        str_detect(x, "UITENHAGE") ~ "Uitenhage",
        str_detect(x, "BEAUFORT") ~ "Beaufort",
        str_detect(x, "ALBANY|GRAHAMSTOWN") ~ "Albany",
        str_detect(x, "CRADOCK") ~ "Cradock",
        str_detect(x, "CLANWILLIAM") ~ "Clanwilliam",
        # Ambiguous frontier localities spanning 1825 boundaries: leave NA
        TRUE ~ NA_character_
      )
    }

    if (all(c("birth_place", "bapt_place") %in% names(vt_adults))) {

      # Rename on selection: best_matches/timing_reg_data can already carry
      # birth_place/bapt_place via the candidate feature set, which would
      # otherwise produce .x/.y suffixes on the join.
      place_lookup <- vt_adults %>%
        mutate(row_id = row_number()) %>%
        select(row_id, birth_place_g = birth_place, bapt_place_g = bapt_place)

      tenure_data <- timing_reg_data %>%
        select(-any_of(c("birth_place_g", "bapt_place_g"))) %>%
        left_join(place_lookup, by = "row_id") %>%
        mutate(
          birth_district = map_place_to_district(birth_place_g),
          bapt_district  = map_place_to_district(bapt_place_g),
          origin_district = coalesce(birth_district, bapt_district),
          born_outside = case_when(
            is.na(origin_district) ~ NA,
            origin_district == "Foreign" ~ TRUE,
            TRUE ~ origin_district != district
          )
        )

      n_place_any <- sum(!is.na(tenure_data$birth_place_g) & trimws(tenure_data$birth_place_g) != "" |
                         !is.na(tenure_data$bapt_place_g) & trimws(tenure_data$bapt_place_g) != "")
      n_mapped <- sum(!is.na(tenure_data$born_outside))
      cat("  Timing sample:", nrow(tenure_data), "households\n")
      cat("  With any birth/baptism place recorded:", n_place_any, "\n")
      cat("  Mapped to an 1825 district (usable):", n_mapped, "\n")
      cat("  Born/baptised outside 1825 district:",
          sum(tenure_data$born_outside, na.rm = TRUE), "of", n_mapped, "\n")

      tenure_sub <- tenure_data %>% filter(!is.na(born_outside))

      if (nrow(tenure_sub) >= 50 && n_distinct(tenure_sub$born_outside) == 2) {

        # (5) timing on recency alone; (6) full timing model + recency;
        # (W) wealth on recency within matched VT sample (is middling wealth
        #     a consequence of recent mobility rather than a stable trait?)
        model5 <- lm(move_year ~ born_outside + district_f, data = tenure_sub)
        model6 <- lm(move_year ~ wealth_std + slaves_std + children_std +
                       born_outside + district_f, data = tenure_sub)
        modelW <- lm(wealth_index ~ born_outside + district_f, data = tenure_sub)

        robust5 <- coeftest(model5, vcov = vcovHC(model5, type = "HC1"))
        robust6 <- coeftest(model6, vcov = vcovHC(model6, type = "HC1"))
        robustW <- coeftest(modelW, vcov = vcovHC(modelW, type = "HC1"))

        get_row <- function(ct, v) {
          if (v %in% rownames(ct)) c(ct[v, 1], ct[v, 2], ct[v, 4]) else rep(NA_real_, 3)
        }

        tenure_results <- bind_rows(
          data.frame(model = "(5) Move year ~ recency + district FE",
                     outcome = "move_year", term = "born_outsideTRUE",
                     t(get_row(robust5, "born_outsideTRUE")),
                     n = nrow(model5$model), r_squared = summary(model5)$r.squared),
          data.frame(model = "(6) Full timing model + recency",
                     outcome = "move_year", term = "born_outsideTRUE",
                     t(get_row(robust6, "born_outsideTRUE")),
                     n = nrow(model6$model), r_squared = summary(model6)$r.squared),
          data.frame(model = "(6) Full timing model + recency",
                     outcome = "move_year", term = "wealth_std",
                     t(get_row(robust6, "wealth_std")),
                     n = nrow(model6$model), r_squared = summary(model6)$r.squared),
          data.frame(model = "(6) Full timing model + recency",
                     outcome = "move_year", term = "slaves_std",
                     t(get_row(robust6, "slaves_std")),
                     n = nrow(model6$model), r_squared = summary(model6)$r.squared),
          data.frame(model = "(6) Full timing model + recency",
                     outcome = "move_year", term = "children_std",
                     t(get_row(robust6, "children_std")),
                     n = nrow(model6$model), r_squared = summary(model6)$r.squared),
          data.frame(model = "(W) Wealth ~ recency + district FE",
                     outcome = "wealth_index", term = "born_outsideTRUE",
                     t(get_row(robustW, "born_outsideTRUE")),
                     n = nrow(modelW$model), r_squared = summary(modelW)$r.squared)
        )
        names(tenure_results)[names(tenure_results) %in% c("X1", "X2", "X3")] <-
          c("coef", "se", "p_value")

        tenure_results$n_timing_sample <- nrow(tenure_data)
        tenure_results$n_place_recorded <- n_place_any
        tenure_results$n_mapped <- n_mapped
        tenure_results$share_born_outside <-
          mean(tenure_sub$born_outside, na.rm = TRUE)

        write.csv(tenure_results, "output/tables/timing_with_tenure.csv",
                  row.names = FALSE)
        cat("  Exported timing_with_tenure.csv\n")

        cat("\n  Recency-of-arrival results (born/baptised outside 1825 district):\n")
        print(tenure_results %>% select(model, term, coef, se, p_value, n))

        # Tex fragment: three-column tenure table
        lab <- function(t) dplyr::case_when(
          t == "born_outsideTRUE" ~ "Born/baptized outside 1825 district",
          t == "wealth_std" ~ "Wealth index (std)",
          t == "slaves_std" ~ "Slaves (std)",
          t == "children_std" ~ "Children (std)",
          TRUE ~ t)
        cell <- function(ct, v) {
          r <- get_row(ct, v)
          if (is.na(r[1])) return(c("", ""))
          c(paste0(fmt_est(r[1], 3), stars_for(r[3])),
            paste0("(", fmt_est(r[2], 3), ")"))
        }
        tex_ten <- character(0)
        for (v in c("born_outsideTRUE", "wealth_std", "slaves_std", "children_std")) {
          c5 <- cell(robust5, v); c6 <- cell(robust6, v); cW <- cell(robustW, v)
          tex_ten <- c(tex_ten,
                       sprintf("%s & %s & %s & %s \\\\", lab(v), c5[1], c6[1], cW[1]),
                       sprintf(" & %s & %s & %s \\\\", c5[2], c6[2], cW[2]))
        }
        tex_ten <- c(tex_ten,
                     "\\midrule",
                     sprintf("District FE & Yes & Yes & Yes \\\\"),
                     sprintf("Observations & %d & %d & %d \\\\",
                             nrow(model5$model), nrow(model6$model), nrow(modelW$model)),
                     sprintf("R$^2$ & %.3f & %.3f & %.3f \\\\",
                             summary(model5)$r.squared, summary(model6)$r.squared,
                             summary(modelW)$r.squared))
        write_tex_fragment(tex_ten, "output/tables/tex/tab_timing_tenure_body.tex")

      } else {
        cat("  Too few mapped observations (", nrow(tenure_sub),
            ") for the tenure regressions; skipping.\n")
      }

    } else {
      cat("  birth_place/bapt_place not found in vt_adults; tenure proxy skipped.\n")
    }

  } else {
    cat("Insufficient observations with valid move_year for timing analysis.\n")
  }

} else {
  cat("Note: 'move_year' column not available in data.\n")
}

cat("\n========== TIMING ANALYSIS COMPLETE ==========\n")


# ============================================================================
# PART 16: SLAVE EMANCIPATION AND VOORTREKKER SELECTION
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("    SLAVE EMANCIPATION HYPOTHESIS: DID SLAVE LOSSES DRIVE THE TREK?\n")
cat("================================================================\n\n")

# This section tests two hypotheses about why Voortrekkers left:
# H1: Settlers with higher-valued slaves were more likely to trek
# H2: Settlers who lost more (valuation - compensation gap) were more likely to trek

# --------------------------------------------------------------------------
# 16.1 LOAD AND PREPARE SLAVE EMANCIPATION DATA
# --------------------------------------------------------------------------

cat("Loading Slave Emancipation Dataset...\n")

emancipation_raw <- read_excel("data/raw/Slave Emancipation Dataset.xlsx", sheet = 1)
cat("  Raw records (slave-level):", nrow(emancipation_raw), "\n")

# Aggregate to owner level in two stages: first one row per claim (UCL), keeping
# the per-claim slave count (Num_slaves repeats on every slave record of a claim);
# then sum slave counts, valuation and compensation over each owner's claims,
# so that an owner with several claims is counted once.
scope_path <- "data/inputs/compensation_scope_decisions.csv"
stopifnot(unname(tools::md5sum(scope_path)) == readLines("data/inputs/compensation_scope_md5.txt",warn=FALSE))
scope_decisions <- read.csv(scope_path, colClasses="character", fileEncoding="UTF-8-BOM") %>%
  select(UCL,Owner_surname,Owner_name,District_name,scope_decision=decision)
claim_level <- emancipation_raw %>%
  group_by(UCL, Owner_surname, Owner_name, District_name) %>%
  summarise(
    valuation_observed = if (all(is.na(Valuation))) NA_real_ else sum(Valuation, na.rm = TRUE),
    compensation_observed = if (all(is.na(Compensation))) NA_real_ else sum(Compensation, na.rm = TRUE),
    n_valued = sum(!is.na(Valuation)), n_paid = sum(!is.na(Compensation)),
    n_counts = n_distinct(Num_slaves, na.rm = TRUE),
    claim_num_slaves = first(Num_slaves), slave_records = n(), .groups = "drop") %>%
  group_by(District_name, UCL) %>% mutate(n_owner_groups_for_claim = n()) %>% ungroup() %>%
  mutate(UCL=as.character(UCL)) %>%
  left_join(scope_decisions,by=c("UCL","Owner_surname","Owner_name","District_name"),relationship="one-to-one") %>%
  mutate(source_scope_unresolved=coalesce(scope_decision == "exclude",FALSE)) %>%
  mutate(coverage_complete = !is.na(UCL) & !source_scope_unresolved & n_owner_groups_for_claim == 1 &
           n_valued == slave_records & n_paid == 1 & n_counts == 1 &
           !is.na(claim_num_slaves) & claim_num_slaves == slave_records,
         claim_valuation = ifelse(coverage_complete, valuation_observed, NA_real_),
         claim_compensation = ifelse(coverage_complete, compensation_observed, NA_real_))
write.csv(claim_level, "output/tables/compensation_claim_coverage.csv", row.names = FALSE)

slave_owners <- claim_level %>%
  group_by(Owner_surname, Owner_name, District_name) %>%
  summarise(
    coverage_complete = all(coverage_complete),
    total_valuation = if (all(coverage_complete)) sum(claim_valuation) else NA_real_,
    total_compensation = if (all(coverage_complete)) sum(claim_compensation) else NA_real_,
    num_slaves = sum(claim_num_slaves, na.rm = TRUE), n_claims = n(),
    slave_records = sum(slave_records), .groups = "drop") %>%
  mutate(
    # Calculate loss (positive = lost money)
    loss = total_valuation - total_compensation,
    loss_pct = ifelse(total_valuation > 0,
                      (total_valuation - total_compensation) / total_valuation * 100,
                      NA),
    # Mean value per slave
    mean_slave_value = ifelse(num_slaves > 0, total_valuation / num_slaves, NA),
    # Standardize names for matching
    surname_std = toupper(trimws(as.character(Owner_surname))),
    surname_clean = gsub("[^A-Z]", "", surname_std),
    first_name_std = toupper(trimws(as.character(Owner_name))),
    first_name_clean = gsub("[^A-Z ]", "", first_name_std),
    first_name_clean = trimws(first_name_clean),
    first_only = word(first_name_clean, 1),
    # Standardize district names to match census/Voortrekker format
    district_std = case_when(
      District_name == "Graaff Reinet" ~ "Graaff-Reinet",
      District_name == "Cape" ~ "Cape",
      TRUE ~ District_name
    )
  ) %>%
  # Filter out rows with missing names
  filter(!is.na(surname_clean) & surname_clean != "" &
         !is.na(first_name_clean) & first_name_clean != "")

cat("  Unique slave owners:", nrow(slave_owners), "\n")

# Summary statistics
cat("\nSlave ownership summary:\n")
cat(sprintf("  Mean slaves per owner: %.1f\n", mean(slave_owners$num_slaves, na.rm = TRUE)))
cat(sprintf("  Mean total valuation: £%.1f\n", mean(slave_owners$total_valuation, na.rm = TRUE)))
cat(sprintf("  Mean total compensation: £%.1f\n", mean(slave_owners$total_compensation, na.rm = TRUE)))
cat(sprintf("  Mean loss: £%.1f (%.1f%%)\n",
            mean(slave_owners$loss, na.rm = TRUE),
            mean(slave_owners$loss_pct, na.rm = TRUE)))

cat("\nSlave owners by district:\n")
print(table(slave_owners$district_std))

# --------------------------------------------------------------------------
# 16.2 MATCH VOORTREKKERS TO SLAVE OWNERS
# --------------------------------------------------------------------------

cat("\n\n--- Matching Voortrekkers to Slave Emancipation Records ---\n")

# Carry through census corroboration so the emancipation linkage can be
# described in transparent owner-level terms.
vt_census_lookup <- NULL
if (exists("best_matches") &&
    all(c("row_id", "census_id", "match_score") %in% names(best_matches))) {
  vt_census_lookup <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    select(row_id, census_id, census_match_score = match_score) %>%
    distinct(row_id, .keep_all = TRUE)
  cat("Census corroboration available for emancipation linkage.\n")
} else {
  cat("WARNING: Census corroboration lookup unavailable for emancipation linkage.\n")
}

# Prepare Voortrekker data for matching
# Use the vt_adults dataframe - create cleaned name columns from base columns
vt_for_emancipation <- vt_adults %>%
  mutate(
    vt_row_id = if ("row_id" %in% names(.)) row_id else row_number(),
    # Create standardized surname from base vt_surname column
    vt_surname_std = toupper(trimws(as.character(vt_surname))),
    vt_surname_clean = gsub("[^A-Z]", "", vt_surname_std),
    # Create standardized first name from base vt_name column
    vt_name_std = toupper(trimws(as.character(vt_name))),
    vt_name_std = gsub("\\(.*?\\)", "", vt_name_std),  # Remove parentheticals
    vt_first_clean = gsub("[^A-Z ]", "", vt_name_std),
    vt_first_clean = trimws(vt_first_clean),
    vt_first_only = word(vt_first_clean, 1)
  ) %>%
  {
    if (!is.null(vt_census_lookup)) {
      left_join(., vt_census_lookup, by = c("vt_row_id" = "row_id"))
    } else {
      mutate(., census_id = NA_integer_, census_match_score = NA_real_)
    }
  } %>%
  mutate(census_corroborated = !is.na(census_id)) %>%
  filter(!is.na(vt_surname_clean) & vt_surname_clean != "" &
         !is.na(vt_first_clean) & vt_first_clean != "")

cat("Voortrekkers prepared for emancipation matching:", nrow(vt_for_emancipation), "\n")

# Map Voortrekker districts to emancipation districts
# Note: Emancipation districts are broader and may not have all frontier districts
map_to_emancipation_districts <- function(census_dist) {
  if (is.na(census_dist)) return(c("Graaff-Reinet", "Swellendam"))

  # Somerset and Colesberg: search in Graaff-Reinet and Somerset
  if (census_dist == "Somerset_multi") {
    return(c("Somerset", "Graaff-Reinet", "Uitenhage", "Albany"))
  }
  if (census_dist == "Colesberg_multi") {
    return(c("Graaff-Reinet", "Somerset", "Beaufort"))
  }
  # Cradock: search in Graaff-Reinet (Cradock was carved from it)
  if (census_dist == "Cradock") {
    return(c("Graaff-Reinet", "Somerset"))
  }
  # Direct matches
  return(census_dist)
}

# Perform Jaro-Winkler matching with district weighting
# Since we don't have wife names, district becomes more important (20% weight)

matches_emancipation <- list()
match_idx <- 1

cat("Starting matching loop (JW with district weighting)...\n")

for (i in 1:nrow(vt_for_emancipation)) {
  vt_row <- vt_for_emancipation[i, ]

  # Skip if missing key fields
  if (is.na(vt_row$vt_surname_clean) || vt_row$vt_surname_clean == "") next
  if (is.na(vt_row$vt_first_clean) || vt_row$vt_first_clean == "") next

  # Get search districts
  search_dists <- map_to_emancipation_districts(vt_row$census_districts)

  # Find candidates with exact surname match in relevant districts
  candidates <- slave_owners %>%
    filter(surname_clean == vt_row$vt_surname_clean,
           district_std %in% search_dists)

  if (nrow(candidates) == 0) next

  # Score each candidate
  best_score <- 0
  best_match <- NULL

  for (j in 1:nrow(candidates)) {
    cand <- candidates[j, ]

    # Jaro-Winkler on full first names
    jw_full <- stringdist::stringsim(
      tolower(vt_row$vt_first_clean),
      tolower(cand$first_name_clean),
      method = "jw", p = 0.1
    )

    # Jaro-Winkler on first name only
    jw_first <- stringdist::stringsim(
      tolower(vt_row$vt_first_only),
      tolower(cand$first_only),
      method = "jw", p = 0.1
    )

    # Name score: weighted combination
    has_multi_vt <- grepl(" ", vt_row$vt_first_clean)
    has_multi_cand <- grepl(" ", cand$first_name_clean)

    if (has_multi_vt && has_multi_cand) {
      name_score <- 0.7 * jw_full + 0.3 * jw_first
    } else {
      name_score <- 0.4 * jw_full + 0.6 * jw_first
    }

    # District score: 1.0 for primary match, 0.7 for secondary
    is_primary <- (cand$district_std == vt_row$census_districts)
    district_score <- ifelse(is_primary, 1.0, 0.7)

    # Combined score: 80% name, 20% district (district more important without wife names)
    combined_score <- 0.80 * name_score + 0.20 * district_score

    if (combined_score > best_score) {
      best_score <- combined_score
      best_match <- cand
    }
  }

  # Record match if found
  if (!is.null(best_match) && best_score >= 0.70) {
    matches_emancipation[[match_idx]] <- data.frame(
      vt_row_id = vt_row$vt_row_id,
      census_id = vt_row$census_id,
      census_corroborated = vt_row$census_corroborated,
      census_match_score = vt_row$census_match_score,
      vt_surname = vt_row$vt_surname,
      vt_name = vt_row$vt_name,
      vt_district = vt_row$census_districts,
      owner_surname = best_match$surname_std,
      owner_name = best_match$first_name_std,
      owner_district = best_match$district_std,
      total_valuation = best_match$total_valuation,
      total_compensation = best_match$total_compensation,
      num_slaves = best_match$num_slaves,
      loss = best_match$loss,
      loss_pct = best_match$loss_pct,
      mean_slave_value = best_match$mean_slave_value,
      match_score = best_score,
      stringsAsFactors = FALSE
    )
    match_idx <- match_idx + 1
  }
}

# Combine matches
if (length(matches_emancipation) > 0) {
  emancipation_matches <- bind_rows(matches_emancipation) %>%
    mutate(owner_key = paste(owner_surname, owner_name, owner_district, sep = "|"))
  cat("\nMatches found:", nrow(emancipation_matches), "\n")
  cat("Match rate:", round(nrow(emancipation_matches) / nrow(vt_for_emancipation) * 100, 1), "%\n")

  # Match quality distribution
  cat("\nMatch quality distribution:\n")
  cat("  Excellent (>= 0.90):", sum(emancipation_matches$match_score >= 0.90), "\n")
  cat("  Good (0.80-0.90):", sum(emancipation_matches$match_score >= 0.80 & emancipation_matches$match_score < 0.90), "\n")
  cat("  Fair (0.70-0.80):", sum(emancipation_matches$match_score >= 0.70 & emancipation_matches$match_score < 0.80), "\n")

  # Sample matches
  cat("\nSample matched Voortrekker slave owners:\n")
  sample_matches <- emancipation_matches %>%
    arrange(desc(match_score)) %>%
    head(15)
  for (k in 1:nrow(sample_matches)) {
    m <- sample_matches[k, ]
    cat(sprintf("  VT: %s, %s [%s] -> Owner: %s, %s [%s] | Slaves: %d, Val: £%.0f, Loss: £%.0f (%.2f)\n",
                m$vt_surname, substr(m$vt_name, 1, 20), m$vt_district,
                m$owner_surname, substr(m$owner_name, 1, 20), m$owner_district,
                m$num_slaves, m$total_valuation, m$loss, m$match_score))
  }

} else {
  cat("\nWARNING: No matches found between Voortrekkers and slave owners.\n")
  emancipation_matches <- NULL
}

# --------------------------------------------------------------------------
# 16.3 TEST HYPOTHESES: COMPARE VOORTREKKER VS NON-VOORTREKKER SLAVE OWNERS
# --------------------------------------------------------------------------

if (!is.null(emancipation_matches) && nrow(emancipation_matches) >= 20) {

  cat("\n\n--- HYPOTHESIS TESTING: SLAVE EMANCIPATION ---\n")
  cat("Comparing Voortrekker slave owners to non-Voortrekker slave owners\n\n")

  # Create indicator for Voortrekker status in slave owners dataset
  matched_owner_ids <- emancipation_matches %>%
    distinct(owner_key, owner_surname, owner_name, owner_district)

  matched_owner_ids_census <- emancipation_matches %>%
    filter(census_corroborated %in% TRUE) %>%
    distinct(owner_key, owner_surname, owner_name, owner_district)

  slave_owners_analysis <- slave_owners %>%
    mutate(
      owner_key = paste(surname_std, first_name_std, district_std, sep = "|"),
      is_voortrekker = owner_key %in% matched_owner_ids$owner_key,
      is_voortrekker_census = owner_key %in% matched_owner_ids_census$owner_key
    )

  n_vt_owners <- sum(slave_owners_analysis$is_voortrekker)
  n_non_vt_owners <- sum(!slave_owners_analysis$is_voortrekker)
  n_vt_owners_census <- sum(slave_owners_analysis$is_voortrekker_census)

  cat(sprintf("Voortrekker slave owners: %d\n", n_vt_owners))
  cat(sprintf("Non-Voortrekker slave owners: %d\n", n_non_vt_owners))
  cat(sprintf("Census-corroborated Voortrekker slave owners: %d\n", n_vt_owners_census))

  # --------------------------------------------------------------------------
  # 16.3.1 DESCRIPTIVE COMPARISON
  # --------------------------------------------------------------------------

  cat("\n--- Descriptive Statistics by Voortrekker Status ---\n\n")

  desc_emancipation <- slave_owners_analysis %>%
    group_by(is_voortrekker) %>%
    summarise(
      n = n(),
      mean_slaves = mean(num_slaves, na.rm = TRUE),
      mean_valuation = mean(total_valuation, na.rm = TRUE),
      mean_compensation = mean(total_compensation, na.rm = TRUE),
      mean_loss = mean(loss, na.rm = TRUE),
      mean_loss_pct = mean(loss_pct, na.rm = TRUE),
      median_valuation = median(total_valuation, na.rm = TRUE),
      median_loss = median(loss, na.rm = TRUE),
      sd_valuation = sd(total_valuation, na.rm = TRUE),
      sd_loss = sd(loss, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("%-25s %15s %15s\n", "Metric", "Voortrekkers", "Non-Voortrekkers"))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  vt_stats <- desc_emancipation %>% filter(is_voortrekker == TRUE)
  non_vt_stats <- desc_emancipation %>% filter(is_voortrekker == FALSE)

  cat(sprintf("%-25s %15d %15d\n", "N", vt_stats$n, non_vt_stats$n))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Mean # slaves", vt_stats$mean_slaves, non_vt_stats$mean_slaves))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Mean valuation (£)", vt_stats$mean_valuation, non_vt_stats$mean_valuation))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Mean compensation (£)", vt_stats$mean_compensation, non_vt_stats$mean_compensation))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Mean loss (£)", vt_stats$mean_loss, non_vt_stats$mean_loss))
  cat(sprintf("%-25s %15.1f%% %14.1f%%\n", "Mean loss (%)", vt_stats$mean_loss_pct, non_vt_stats$mean_loss_pct))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Median valuation (£)", vt_stats$median_valuation, non_vt_stats$median_valuation))
  cat(sprintf("%-25s %15.1f %15.1f\n", "Median loss (£)", vt_stats$median_loss, non_vt_stats$median_loss))

  # --------------------------------------------------------------------------
  # 16.3.2 T-TESTS
  # --------------------------------------------------------------------------

  cat("\n\n--- T-Tests: Voortrekker vs Non-Voortrekker Slave Owners ---\n\n")

  # H1: Higher-valued slaves
  t_valuation <- t.test(total_valuation ~ is_voortrekker, data = slave_owners_analysis)
  t_num_slaves <- t.test(num_slaves ~ is_voortrekker, data = slave_owners_analysis)

  # H2: Greater loss
  t_loss <- t.test(loss ~ is_voortrekker, data = slave_owners_analysis)
  t_loss_pct <- t.test(loss_pct ~ is_voortrekker, data = slave_owners_analysis)

  cat("HYPOTHESIS 1: Voortrekkers had higher-valued slaves\n")
  cat(sprintf("  Total valuation: VT mean = £%.1f, Non-VT mean = £%.1f\n",
              t_valuation$estimate["mean in group TRUE"], t_valuation$estimate["mean in group FALSE"]))
  cat(sprintf("    Difference: £%.1f (t = %.2f, p = %.4f) %s\n",
              t_valuation$estimate["mean in group TRUE"] - t_valuation$estimate["mean in group FALSE"],
              t_valuation$statistic, t_valuation$p.value,
              ifelse(t_valuation$p.value < 0.05, "*", "")))

  cat(sprintf("\n  Number of slaves: VT mean = %.1f, Non-VT mean = %.1f\n",
              t_num_slaves$estimate["mean in group TRUE"], t_num_slaves$estimate["mean in group FALSE"]))
  cat(sprintf("    Difference: %.1f (t = %.2f, p = %.4f) %s\n",
              t_num_slaves$estimate["mean in group TRUE"] - t_num_slaves$estimate["mean in group FALSE"],
              t_num_slaves$statistic, t_num_slaves$p.value,
              ifelse(t_num_slaves$p.value < 0.05, "*", "")))

  cat("\n\nHYPOTHESIS 2: Voortrekkers lost more from emancipation\n")
  cat(sprintf("  Absolute loss (£): VT mean = £%.1f, Non-VT mean = £%.1f\n",
              t_loss$estimate["mean in group TRUE"], t_loss$estimate["mean in group FALSE"]))
  cat(sprintf("    Difference: £%.1f (t = %.2f, p = %.4f) %s\n",
              t_loss$estimate["mean in group TRUE"] - t_loss$estimate["mean in group FALSE"],
              t_loss$statistic, t_loss$p.value,
              ifelse(t_loss$p.value < 0.05, "*", "")))

  cat(sprintf("\n  Percentage loss: VT mean = %.1f%%, Non-VT mean = %.1f%%\n",
              t_loss_pct$estimate["mean in group TRUE"], t_loss_pct$estimate["mean in group FALSE"]))
  cat(sprintf("    Difference: %.1f%% (t = %.2f, p = %.4f) %s\n",
              t_loss_pct$estimate["mean in group TRUE"] - t_loss_pct$estimate["mean in group FALSE"],
              t_loss_pct$statistic, t_loss_pct$p.value,
              ifelse(t_loss_pct$p.value < 0.05, "*", "")))

  # --------------------------------------------------------------------------
  # 16.3.3 REGRESSION ANALYSIS WITH DISTRICT FIXED EFFECTS
  # --------------------------------------------------------------------------

  cat("\n\n--- Regression Analysis: District Fixed Effects ---\n")
  cat("Testing whether Voortrekker status predicts slave ownership characteristics\n")
  cat("(controlling for district)\n\n")

  # Standardize variables for effect size interpretation
  slave_owners_analysis <- slave_owners_analysis %>%
    mutate(
      valuation_std = scale(total_valuation)[,1],
      loss_std = scale(loss)[,1],
      num_slaves_std = scale(num_slaves)[,1],
      loss_pct_std = scale(loss_pct)[,1]
    )

  # Run regressions
  reg_valuation <- lm(total_valuation ~ is_voortrekker + factor(district_std),
                       data = slave_owners_analysis)
  reg_loss <- lm(loss ~ is_voortrekker + factor(district_std),
                  data = slave_owners_analysis)
  reg_slaves <- lm(num_slaves ~ is_voortrekker + factor(district_std),
                    data = slave_owners_analysis)
  reg_loss_pct <- lm(loss_pct ~ is_voortrekker + factor(district_std),
                      data = slave_owners_analysis)

  # Standardized regressions for effect sizes
  reg_valuation_std <- lm(valuation_std ~ is_voortrekker + factor(district_std),
                           data = slave_owners_analysis)
  reg_loss_std <- lm(loss_std ~ is_voortrekker + factor(district_std),
                      data = slave_owners_analysis)

  # Robust standard errors
  robust_val <- coeftest(reg_valuation, vcov = vcovHC(reg_valuation, type = "HC1"))
  robust_loss <- coeftest(reg_loss, vcov = vcovHC(reg_loss, type = "HC1"))
  robust_slaves <- coeftest(reg_slaves, vcov = vcovHC(reg_slaves, type = "HC1"))
  robust_loss_pct <- coeftest(reg_loss_pct, vcov = vcovHC(reg_loss_pct, type = "HC1"))
  robust_val_std <- coeftest(reg_valuation_std, vcov = vcovHC(reg_valuation_std, type = "HC1"))
  robust_loss_std <- coeftest(reg_loss_std, vcov = vcovHC(reg_loss_std, type = "HC1"))

  cat("District FE Regression Results (Robust SE):\n")
  cat(sprintf("%-25s %12s %12s %12s %12s\n", "Outcome", "Coef", "SE", "t-stat", "p-value"))
  cat(paste(rep("-", 75), collapse = ""), "\n")

  cat(sprintf("%-25s %12.2f %12.2f %12.2f %12.4f %s\n",
              "Total valuation (£)",
              robust_val["is_voortrekkerTRUE", 1],
              robust_val["is_voortrekkerTRUE", 2],
              robust_val["is_voortrekkerTRUE", 3],
              robust_val["is_voortrekkerTRUE", 4],
              ifelse(robust_val["is_voortrekkerTRUE", 4] < 0.05, "*", "")))

  cat(sprintf("%-25s %12.2f %12.2f %12.2f %12.4f %s\n",
              "Number of slaves",
              robust_slaves["is_voortrekkerTRUE", 1],
              robust_slaves["is_voortrekkerTRUE", 2],
              robust_slaves["is_voortrekkerTRUE", 3],
              robust_slaves["is_voortrekkerTRUE", 4],
              ifelse(robust_slaves["is_voortrekkerTRUE", 4] < 0.05, "*", "")))

  cat(sprintf("%-25s %12.2f %12.2f %12.2f %12.4f %s\n",
              "Absolute loss (£)",
              robust_loss["is_voortrekkerTRUE", 1],
              robust_loss["is_voortrekkerTRUE", 2],
              robust_loss["is_voortrekkerTRUE", 3],
              robust_loss["is_voortrekkerTRUE", 4],
              ifelse(robust_loss["is_voortrekkerTRUE", 4] < 0.05, "*", "")))

  cat(sprintf("%-25s %12.2f %12.2f %12.2f %12.4f %s\n",
              "Percentage loss",
              robust_loss_pct["is_voortrekkerTRUE", 1],
              robust_loss_pct["is_voortrekkerTRUE", 2],
              robust_loss_pct["is_voortrekkerTRUE", 3],
              robust_loss_pct["is_voortrekkerTRUE", 4],
              ifelse(robust_loss_pct["is_voortrekkerTRUE", 4] < 0.05, "*", "")))

  cat("\nStandardized Effect Sizes (SD units):\n")
  cat(sprintf("  Valuation: %.3f SD\n", robust_val_std["is_voortrekkerTRUE", 1]))
  cat(sprintf("  Loss: %.3f SD\n", robust_loss_std["is_voortrekkerTRUE", 1]))

  # Save district FE results to CSV for cross-validation reference
  emancipation_fe_results <- data.frame(
    variable = c("Total valuation (£)", "Number of slaves", "Absolute loss (£)", "Percentage loss"),
    coefficient = c(robust_val["is_voortrekkerTRUE", 1],
                    robust_slaves["is_voortrekkerTRUE", 1],
                    robust_loss["is_voortrekkerTRUE", 1],
                    robust_loss_pct["is_voortrekkerTRUE", 1]),
    se = c(robust_val["is_voortrekkerTRUE", 2],
           robust_slaves["is_voortrekkerTRUE", 2],
           robust_loss["is_voortrekkerTRUE", 2],
           robust_loss_pct["is_voortrekkerTRUE", 2]),
    t_stat = c(robust_val["is_voortrekkerTRUE", 3],
               robust_slaves["is_voortrekkerTRUE", 3],
               robust_loss["is_voortrekkerTRUE", 3],
               robust_loss_pct["is_voortrekkerTRUE", 3]),
    p_value = c(robust_val["is_voortrekkerTRUE", 4],
                robust_slaves["is_voortrekkerTRUE", 4],
                robust_loss["is_voortrekkerTRUE", 4],
                robust_loss_pct["is_voortrekkerTRUE", 4]),
    stringsAsFactors = FALSE
  )
  write.csv(emancipation_fe_results, "output/tables/emancipation_district_fe_results.csv", row.names = FALSE)
  cat("\nSaved emancipation district FE results to emancipation_district_fe_results.csv\n")

  # --------------------------------------------------------------------------
  # 16.3.4 VISUALIZATION
  # --------------------------------------------------------------------------

  cat("\n\n--- Creating Visualizations ---\n")

  # Prepare data for plotting
  emancipation_plot_data <- slave_owners_analysis %>%
    mutate(group = ifelse(is_voortrekker, "Voortrekker", "Non-Voortrekker"))

  # ----- GRAPH 1: Distribution of slave valuations -----
  p_valuation_dist <- ggplot(emancipation_plot_data,
                              aes(x = total_valuation, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.6, position = "identity") +
    geom_density(alpha = 0.3) +
    scale_x_continuous(limits = c(0, quantile(emancipation_plot_data$total_valuation, 0.95, na.rm = TRUE))) +
    scale_fill_manual(values = c("Voortrekker" = "#5C2346", "Non-Voortrekker" = "#3D8EB9")) +
    labs(x = "Total Slave Valuation (£)",
         y = "Density",
         fill = "Group") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_valuation_dist)
  fig_file <- next_fig("emancipation_valuation_distribution.png")
  save_leap_fig(fig_file, p_valuation_dist, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # ----- GRAPH 2: Distribution of losses -----
  p_loss_dist <- ggplot(emancipation_plot_data,
                         aes(x = loss, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.6, position = "identity") +
    geom_density(alpha = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA") +
    scale_x_continuous(limits = c(quantile(emancipation_plot_data$loss, 0.01, na.rm = TRUE),
                                   quantile(emancipation_plot_data$loss, 0.95, na.rm = TRUE))) +
    scale_fill_manual(values = c("Voortrekker" = "#5C2346", "Non-Voortrekker" = "#3D8EB9")) +
    labs(x = "Loss from Emancipation (£)",
         y = "Density",
         fill = "Group") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_loss_dist)
  fig_file <- next_fig("emancipation_loss_distribution.png")
  save_leap_fig(fig_file, p_loss_dist, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # ----- GRAPH 3: Comparison bar chart with CIs -----
  # >>> FIG_EMANC_COMPARISON BEGIN
  comparison_summary <- emancipation_plot_data %>%
    group_by(group) %>%
    summarise(
      mean_val = mean(total_valuation, na.rm = TRUE),
      se_val = sd(total_valuation, na.rm = TRUE) / sqrt(sum(!is.na(total_valuation))),
      mean_loss = mean(loss, na.rm = TRUE),
      se_loss = sd(loss, na.rm = TRUE) / sqrt(sum(!is.na(loss))),
      mean_slaves = mean(num_slaves, na.rm = TRUE),
      se_slaves = sd(num_slaves, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Reshape for plotting
  comparison_long <- comparison_summary %>%
    pivot_longer(
      cols = c(mean_val, mean_loss, mean_slaves),
      names_to = "metric",
      values_to = "mean"
    ) %>%
    mutate(
      se = case_when(
        metric == "mean_val" ~ comparison_summary$se_val[match(group, comparison_summary$group)],
        metric == "mean_loss" ~ comparison_summary$se_loss[match(group, comparison_summary$group)],
        metric == "mean_slaves" ~ comparison_summary$se_slaves[match(group, comparison_summary$group)]
      ),
      metric_label = factor(case_when(
        metric == "mean_val" ~ "(b) Total valuation (£)",
        metric == "mean_loss" ~ "(c) Absolute loss (£)",
        metric == "mean_slaves" ~ "(a) Number of slaves"
      ), levels = c("(a) Number of slaves", "(b) Total valuation (£)", "(c) Absolute loss (£)"))
    )

  # One panel per measure, each on its own scale, so that slave counts are not
  # drawn on the pound-sterling axis.
  p_comparison <- ggplot(comparison_long,
                          aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.2) +
    facet_wrap(~ metric_label, scales = "free_y") +
    scale_fill_manual(values = c("Voortrekker" = "#5C2346", "Non-Voortrekker" = "#3D8EB9")) +
    labs(x = "",
         y = "Mean per slave owner",
         fill = "Group") +
    theme_leap() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(), axis.ticks.x = element_blank())

  print(p_comparison)
  fig_file <- next_fig("emancipation_vt_comparison.png")
  save_leap_fig(fig_file, p_comparison, width = 10, height = 5, dpi = 300)
  # (output handled by save_leap_fig)
  # >>> FIG_EMANC_COMPARISON END

  # ----- GRAPH 4: Effect sizes visualization -----
  effect_data <- data.frame(
    variable = c("Total Valuation", "Number of Slaves", "Absolute Loss", "Percentage Loss"),
    effect = c(
      robust_val_std["is_voortrekkerTRUE", 1],
      coeftest(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                  data = slave_owners_analysis),
               vcov = vcovHC(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                                data = slave_owners_analysis), type = "HC1"))["is_voortrekkerTRUE", 1],
      robust_loss_std["is_voortrekkerTRUE", 1],
      coeftest(lm(loss_pct_std ~ is_voortrekker + factor(district_std),
                  data = slave_owners_analysis),
               vcov = vcovHC(lm(loss_pct_std ~ is_voortrekker + factor(district_std),
                                data = slave_owners_analysis), type = "HC1"))["is_voortrekkerTRUE", 1]
    ),
    se = c(
      robust_val_std["is_voortrekkerTRUE", 2],
      coeftest(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                  data = slave_owners_analysis),
               vcov = vcovHC(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                                data = slave_owners_analysis), type = "HC1"))["is_voortrekkerTRUE", 2],
      robust_loss_std["is_voortrekkerTRUE", 2],
      coeftest(lm(loss_pct_std ~ is_voortrekker + factor(district_std),
                  data = slave_owners_analysis),
               vcov = vcovHC(lm(loss_pct_std ~ is_voortrekker + factor(district_std),
                                data = slave_owners_analysis), type = "HC1"))["is_voortrekkerTRUE", 2]
    ),
    hypothesis = c("H1: Higher Value", "H1: Higher Value", "H2: Greater Loss", "H2: Greater Loss")
  )

  p_effects <- ggplot(effect_data, aes(x = reorder(variable, effect), y = effect, fill = hypothesis)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(aes(ymin = effect - 1.96*se, ymax = effect + 1.96*se), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c("H1: Higher Value" = "#5C2346", "H2: Greater Loss" = "#3D8EB9")) +
    labs(x = "",
         y = "Standardized Effect Size (SD units)",
         fill = "Hypothesis") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_effects)
  fig_file <- next_fig("emancipation_effect_sizes.png")
  save_leap_fig(fig_file, p_effects, width = 10, height = 6, dpi = 300)
  # (output handled by save_leap_fig)

  # ==========================================================================
  # 16.3.5 THE COMPENSATION GAP: EXPANDED ANALYSIS
  # ==========================================================================
  #
  # The key question is not just whether Voortrekkers owned more slaves, but

  # whether they suffered disproportionately from the *arbitrariness* of the
  # compensation process. Compensation was set centrally and bore an uneven
  # relationship to assessed valuations. If Voortrekkers received a lower
  # percentage of their slaves' assessed value back as compensation, this
  # could have generated genuine grievance independent of total holdings.
  # ==========================================================================

  cat("\n\n")
  cat("================================================================\n")
  cat("  EXPANDED ANALYSIS: THE COMPENSATION GAP                       \n")
  cat("================================================================\n\n")

  # --------------------------------------------------------------------------
  # 16.3.5a CREATE COMPENSATION RATE VARIABLE
  # --------------------------------------------------------------------------

  slave_owners_analysis <- slave_owners_analysis %>%
    mutate(
      # Compensation rate: what fraction of assessed value was actually paid?
      compensation_rate = ifelse(total_valuation > 0,
                                  total_compensation / total_valuation,
                                  NA),
      # Log loss (for skewed distribution)
      log_loss = ifelse(loss > 0, log(loss), NA),
      log_valuation = ifelse(total_valuation > 0, log(total_valuation), NA),
      # Standardized versions
      compensation_rate_std = scale(compensation_rate)[,1],
      log_loss_std = scale(log_loss)[,1],
      mean_slave_value_std = scale(mean_slave_value)[,1]
    )

  # Filter to owners with positive valuations (meaningful loss calculation)
  owners_with_valuation <- slave_owners_analysis %>%
    filter(total_valuation > 0 & !is.na(compensation_rate))

  n_vt_val <- sum(owners_with_valuation$is_voortrekker)
  n_non_vt_val <- sum(!owners_with_valuation$is_voortrekker)

  cat(sprintf("Slave owners with positive valuations: %d\n", nrow(owners_with_valuation)))
  cat(sprintf("  Voortrekker: %d\n", n_vt_val))
  cat(sprintf("  Non-Voortrekker: %d\n", n_non_vt_val))

  # --------------------------------------------------------------------------
  # 16.3.5b DESCRIPTIVE: COMPENSATION RATES
  # --------------------------------------------------------------------------

  cat("\n--- Compensation Rates by Voortrekker Status ---\n\n")

  comp_desc <- owners_with_valuation %>%
    group_by(is_voortrekker) %>%
    summarise(
      n = n(),
      mean_comp_rate = mean(compensation_rate, na.rm = TRUE),
      median_comp_rate = median(compensation_rate, na.rm = TRUE),
      sd_comp_rate = sd(compensation_rate, na.rm = TRUE),
      p25_comp_rate = quantile(compensation_rate, 0.25, na.rm = TRUE),
      p75_comp_rate = quantile(compensation_rate, 0.75, na.rm = TRUE),
      mean_loss_pct = mean(loss_pct, na.rm = TRUE),
      mean_loss_abs = mean(loss, na.rm = TRUE),
      median_loss_abs = median(loss, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("%-30s %15s %15s\n", "Metric", "Voortrekkers", "Non-Voortrekkers"))
  cat(paste(rep("-", 65), collapse = ""), "\n")

  vt_comp <- comp_desc %>% filter(is_voortrekker == TRUE)
  non_vt_comp <- comp_desc %>% filter(is_voortrekker == FALSE)

  cat(sprintf("%-30s %15d %15d\n", "N (with valuation > 0)", vt_comp$n, non_vt_comp$n))
  cat(sprintf("%-30s %14.1f%% %14.1f%%\n", "Mean compensation rate",
              vt_comp$mean_comp_rate * 100, non_vt_comp$mean_comp_rate * 100))
  cat(sprintf("%-30s %14.1f%% %14.1f%%\n", "Median compensation rate",
              vt_comp$median_comp_rate * 100, non_vt_comp$median_comp_rate * 100))
  cat(sprintf("%-30s %14.1f%% %14.1f%%\n", "IQR: 25th percentile",
              vt_comp$p25_comp_rate * 100, non_vt_comp$p25_comp_rate * 100))
  cat(sprintf("%-30s %14.1f%% %14.1f%%\n", "IQR: 75th percentile",
              vt_comp$p75_comp_rate * 100, non_vt_comp$p75_comp_rate * 100))
  cat(sprintf("%-30s %14.1f%% %14.1f%%\n", "Mean loss (%)",
              vt_comp$mean_loss_pct, non_vt_comp$mean_loss_pct))
  cat(sprintf("%-30s %15.1f %15.1f\n", "Mean absolute loss (£)",
              vt_comp$mean_loss_abs, non_vt_comp$mean_loss_abs))
  cat(sprintf("%-30s %15.1f %15.1f\n", "Median absolute loss (£)",
              vt_comp$median_loss_abs, non_vt_comp$median_loss_abs))

  # Wilcoxon rank-sum (non-parametric, robust to skewness)
  wt_comp_rate <- wilcox.test(compensation_rate ~ is_voortrekker,
                               data = owners_with_valuation)
  wt_loss_pct <- wilcox.test(loss_pct ~ is_voortrekker,
                              data = owners_with_valuation)

  cat("\nNon-parametric tests (Wilcoxon rank-sum):\n")
  cat(sprintf("  Compensation rate: W = %.0f, p = %.4f\n",
              wt_comp_rate$statistic, wt_comp_rate$p.value))
  cat(sprintf("  Loss percentage:   W = %.0f, p = %.4f\n",
              wt_loss_pct$statistic, wt_loss_pct$p.value))

  # --------------------------------------------------------------------------
  # 16.3.5c WITHIN-DISTRICT COMPENSATION RATE COMPARISONS
  # --------------------------------------------------------------------------

  cat("\n\n--- Within-District Compensation Rate Comparisons ---\n\n")

  district_comp <- owners_with_valuation %>%
    group_by(district_std, is_voortrekker) %>%
    summarise(
      n = n(),
      mean_comp_rate = mean(compensation_rate, na.rm = TRUE),
      mean_loss_pct = mean(loss_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = is_voortrekker,
      values_from = c(n, mean_comp_rate, mean_loss_pct),
      names_sep = "_"
    ) %>%
    filter(!is.na(n_TRUE) & n_TRUE >= 3) %>%
    mutate(
      comp_rate_diff = mean_comp_rate_TRUE - mean_comp_rate_FALSE,
      loss_pct_diff = mean_loss_pct_TRUE - mean_loss_pct_FALSE
    )

  cat(sprintf("%-20s %8s %8s %12s %12s %12s\n",
              "District", "N(VT)", "N(nonVT)",
              "VT comp%", "nonVT comp%", "Diff (pp)"))
  cat(paste(rep("-", 80), collapse = ""), "\n")

  for (i in 1:nrow(district_comp)) {
    d <- district_comp[i, ]
    cat(sprintf("%-20s %8d %8d %11.1f%% %11.1f%% %+11.1f\n",
                d$district_std,
                d$n_TRUE, d$n_FALSE,
                d$mean_comp_rate_TRUE * 100,
                d$mean_comp_rate_FALSE * 100,
                d$comp_rate_diff * 100))
  }

  # Count districts where VTs had lower compensation rate
  n_districts_lower <- sum(district_comp$comp_rate_diff < 0, na.rm = TRUE)
  n_districts_total <- nrow(district_comp)
  cat(sprintf("\nDistricts where VTs had LOWER compensation rate: %d of %d\n",
              n_districts_lower, n_districts_total))

  # ==========================================================================
  # 16.3.6 PROBIT MODELS: DOES LOSS PREDICT SELECTION INTO THE TREK?
  # ==========================================================================

  cat("\n\n")
  cat("================================================================\n")
  cat("  PROBIT ANALYSIS: LOSS AS PREDICTOR OF TREKKING                \n")
  cat("================================================================\n\n")

  cat("Among slave owners, does the compensation gap predict who trekked?\n")
  cat("This isolates the 'grievance' channel: holding number of slaves\n")
  cat("constant, did those who received worse compensation terms leave?\n\n")

  # --------------------------------------------------------------------------
  # 16.3.6a PROBIT MODELS (among owners with positive valuations)
  # --------------------------------------------------------------------------

  # Re-standardize within this sample for interpretability
  owners_with_valuation <- owners_with_valuation %>%
    mutate(
      loss_pct_z = scale(loss_pct)[,1],
      compensation_rate_z = scale(compensation_rate)[,1],
      num_slaves_z = scale(num_slaves)[,1],
      mean_slave_value_z = scale(mean_slave_value)[,1],
      log_valuation_z = scale(log_valuation)[,1]
    )

  # Model 1: Just loss percentage
  probit_1 <- glm(is_voortrekker ~ loss_pct_z,
                   data = owners_with_valuation, family = binomial(link = "probit"))

  # Model 2: Loss percentage + district FE
  probit_2 <- glm(is_voortrekker ~ loss_pct_z + factor(district_std),
                   data = owners_with_valuation, family = binomial(link = "probit"))

  # Model 3: Loss percentage + num slaves + district FE
  # This isolates the "unfairness" channel: holding scale of ownership constant,
  # did worse compensation terms predict emigration?
  probit_3 <- glm(is_voortrekker ~ loss_pct_z + num_slaves_z + factor(district_std),
                   data = owners_with_valuation, family = binomial(link = "probit"))

  # Model 4: Compensation rate (inverse of loss) + controls
  probit_4 <- glm(is_voortrekker ~ compensation_rate_z + num_slaves_z +
                     mean_slave_value_z + factor(district_std),
                   data = owners_with_valuation, family = binomial(link = "probit"))

  # Model 5: Full model with log valuation
  probit_5 <- glm(is_voortrekker ~ loss_pct_z + num_slaves_z +
                     log_valuation_z + factor(district_std),
                   data = owners_with_valuation, family = binomial(link = "probit"))

  # --------------------------------------------------------------------------
  # 16.3.6b MARGINAL EFFECTS
  # --------------------------------------------------------------------------

  cat("--- Probit Results: Coefficient Estimates ---\n\n")
  cat("(Standardized predictors: 1 unit = 1 SD increase)\n\n")

  cat(sprintf("%-45s %10s %10s %10s %10s\n",
              "Predictor", "Coef", "SE", "z-stat", "p-value"))
  cat(paste(rep("-", 90), collapse = ""), "\n")

  # Model 1
  s1 <- summary(probit_1)$coefficients
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "M1: Loss % (std)",
              s1["loss_pct_z", 1], s1["loss_pct_z", 2],
              s1["loss_pct_z", 3], s1["loss_pct_z", 4]))

  # Model 2
  s2 <- summary(probit_2)$coefficients
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "M2: Loss % (std) + District FE",
              s2["loss_pct_z", 1], s2["loss_pct_z", 2],
              s2["loss_pct_z", 3], s2["loss_pct_z", 4]))

  # Model 3
  s3 <- summary(probit_3)$coefficients
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "M3: Loss % (std) | + N slaves + District FE",
              s3["loss_pct_z", 1], s3["loss_pct_z", 2],
              s3["loss_pct_z", 3], s3["loss_pct_z", 4]))
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "    N slaves (std)",
              s3["num_slaves_z", 1], s3["num_slaves_z", 2],
              s3["num_slaves_z", 3], s3["num_slaves_z", 4]))

  # Model 4
  s4 <- summary(probit_4)$coefficients
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "M4: Comp rate (std) | + N slaves + value + Dist FE",
              s4["compensation_rate_z", 1], s4["compensation_rate_z", 2],
              s4["compensation_rate_z", 3], s4["compensation_rate_z", 4]))

  # Model 5
  s5 <- summary(probit_5)$coefficients
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "M5: Loss % (std) | + N slaves + log val + Dist FE",
              s5["loss_pct_z", 1], s5["loss_pct_z", 2],
              s5["loss_pct_z", 3], s5["loss_pct_z", 4]))
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "    N slaves (std)",
              s5["num_slaves_z", 1], s5["num_slaves_z", 2],
              s5["num_slaves_z", 3], s5["num_slaves_z", 4]))
  cat(sprintf("%-45s %10.4f %10.4f %10.3f %10.4f\n",
              "    Log valuation (std)",
              s5["log_valuation_z", 1], s5["log_valuation_z", 2],
              s5["log_valuation_z", 3], s5["log_valuation_z", 4]))

  # Average marginal effects for interpretation
  cat("\n\n--- Average Marginal Effects (percentage point change in P(Trek)) ---\n\n")

  # Compute AME for the key model (Model 3)
  # For probit: AME = phi(X'beta) * beta_j, averaged over all observations
  xb3 <- predict(probit_3, type = "link")
  phi3 <- dnorm(xb3)
  ame_loss_pct_3 <- mean(phi3) * coef(probit_3)["loss_pct_z"]
  ame_nslaves_3 <- mean(phi3) * coef(probit_3)["num_slaves_z"]

  xb5 <- predict(probit_5, type = "link")
  phi5 <- dnorm(xb5)
  ame_loss_pct_5 <- mean(phi5) * coef(probit_5)["loss_pct_z"]

  cat(sprintf("Model 3 (Loss %% + N slaves + District FE):\n"))
  cat(sprintf("  1 SD increase in loss %%: %+.2f pp change in P(Trek)\n",
              ame_loss_pct_3 * 100))
  cat(sprintf("  1 SD increase in N slaves: %+.2f pp change in P(Trek)\n",
              ame_nslaves_3 * 100))
  cat(sprintf("\nModel 5 (Loss %% + N slaves + log val + District FE):\n"))
  cat(sprintf("  1 SD increase in loss %%: %+.2f pp change in P(Trek)\n",
              ame_loss_pct_5 * 100))

  # Baseline probability for context
  base_rate <- mean(owners_with_valuation$is_voortrekker)
  cat(sprintf("\nBaseline: %.1f%% of slave owners were Voortrekkers\n", base_rate * 100))
  cat(sprintf("So an AME of %.2f pp represents a %.1f%% relative change from baseline\n",
              ame_loss_pct_3 * 100,
              abs(ame_loss_pct_3 / base_rate) * 100))

  # --------------------------------------------------------------------------
  # 16.3.6c SAVE PROBIT REGRESSION TABLE
  # --------------------------------------------------------------------------

  # Stargazer table for the paper
  tryCatch({
    stargazer(probit_1, probit_2, probit_3, probit_4, probit_5,
              type = "text",
              title = "Probit Models: Does Emancipation Loss Predict Selection into the Trek?",
              dep.var.labels = "Voortrekker (0/1)",
              covariate.labels = c("Loss percentage (std)", "N slaves (std)",
                                   "Compensation rate (std)", "Mean slave value (std)",
                                   "Log total valuation (std)"),
              omit = "factor\\(district_std\\)",
              omit.labels = "District FE",
              add.lines = list(
                c("District FE", "No", "Yes", "Yes", "Yes", "Yes"),
                c("N slaves control", "No", "No", "Yes", "Yes", "Yes")
              ),
              omit.stat = c("aic", "ll"),
              no.space = TRUE,
              out = "output/tables/emancipation_probit_table.txt")
    cat("\nProbit table saved to: Output/emancipation_probit_table.txt\n")
  }, error = function(e) {
    cat("Note: Could not save stargazer table:", e$message, "\n")
  })

  # ==========================================================================
  # 16.3.7 LOSS INTENSITY ANALYSIS: QUARTILES
  # ==========================================================================

  cat("\n\n")
  cat("================================================================\n")
  cat("  LOSS INTENSITY: DID THE WORST-AFFECTED SLAVE OWNERS TREK?     \n")
  cat("================================================================\n\n")

  # Divide into quartiles of loss percentage
  owners_with_valuation <- owners_with_valuation %>%
    mutate(
      loss_quartile = ntile(loss_pct, 4),
      loss_quartile_label = case_when(
        loss_quartile == 1 ~ "Q1 (lowest loss)",
        loss_quartile == 2 ~ "Q2",
        loss_quartile == 3 ~ "Q3",
        loss_quartile == 4 ~ "Q4 (highest loss)"
      ),
      loss_quartile_label = factor(loss_quartile_label,
                                    levels = c("Q1 (lowest loss)", "Q2", "Q3", "Q4 (highest loss)"))
    )

  quartile_rates <- owners_with_valuation %>%
    group_by(loss_quartile_label) %>%
    summarise(
      n_total = n(),
      n_vt = sum(is_voortrekker),
      vt_rate = mean(is_voortrekker) * 100,
      mean_loss_pct = mean(loss_pct, na.rm = TRUE),
      mean_comp_rate = mean(compensation_rate, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  cat(sprintf("%-25s %8s %8s %12s %15s %15s\n",
              "Loss Quartile", "N total", "N VT", "VT rate (%)",
              "Mean loss (%)", "Mean comp rate"))
  cat(paste(rep("-", 90), collapse = ""), "\n")

  for (i in 1:nrow(quartile_rates)) {
    q <- quartile_rates[i, ]
    cat(sprintf("%-25s %8d %8d %11.1f%% %14.1f%% %14.1f%%\n",
                as.character(q$loss_quartile_label),
                q$n_total, q$n_vt, q$vt_rate,
                q$mean_loss_pct, q$mean_comp_rate))
  }

  # Chi-squared test for trend
  chisq_result <- chisq.test(table(owners_with_valuation$loss_quartile,
                                    owners_with_valuation$is_voortrekker))
  cat(sprintf("\nChi-squared test: X2 = %.2f, df = %d, p = %.4f\n",
              chisq_result$statistic, chisq_result$parameter, chisq_result$p.value))

  # Cochran-Armitage trend test (linear trend in proportions)
  # Manual computation: correlation between quartile and VT rate
  trend_cor <- cor.test(owners_with_valuation$loss_quartile,
                         as.numeric(owners_with_valuation$is_voortrekker),
                         method = "spearman")
  cat(sprintf("Spearman rank correlation (loss quartile vs VT): rho = %.3f, p = %.4f\n",
              trend_cor$estimate, trend_cor$p.value))

  # Compare top quartile vs bottom quartile directly
  q1_rate <- quartile_rates$vt_rate[1]
  q4_rate <- quartile_rates$vt_rate[4]
  cat(sprintf("\nQ1 (lowest loss) VT rate: %.1f%%\n", q1_rate))
  cat(sprintf("Q4 (highest loss) VT rate: %.1f%%\n", q4_rate))
  cat(sprintf("Ratio Q4/Q1: %.2f\n", q4_rate / q1_rate))

  # Fisher exact test: Q4 vs Q1
  q1_data <- owners_with_valuation %>% filter(loss_quartile == 1)
  q4_data <- owners_with_valuation %>% filter(loss_quartile == 4)
  fisher_q4q1 <- fisher.test(
    matrix(c(sum(q4_data$is_voortrekker), sum(!q4_data$is_voortrekker),
             sum(q1_data$is_voortrekker), sum(!q1_data$is_voortrekker)),
           nrow = 2)
  )
  cat(sprintf("Fisher exact test (Q4 vs Q1): OR = %.2f, p = %.4f\n",
              fisher_q4q1$estimate, fisher_q4q1$p.value))

  # ==========================================================================
  # 16.3.8 OLS ROBUSTNESS: LINEAR PROBABILITY MODELS
  # ==========================================================================

  cat("\n\n--- OLS Linear Probability Models (for robustness) ---\n\n")

  lpm_1 <- lm(is_voortrekker ~ loss_pct_z + factor(district_std),
               data = owners_with_valuation)
  lpm_2 <- lm(is_voortrekker ~ loss_pct_z + num_slaves_z + factor(district_std),
               data = owners_with_valuation)
  lpm_3 <- lm(is_voortrekker ~ compensation_rate_z + num_slaves_z +
                 mean_slave_value_z + factor(district_std),
               data = owners_with_valuation)

  robust_lpm1 <- coeftest(lpm_1, vcov = vcovHC(lpm_1, type = "HC1"))
  robust_lpm2 <- coeftest(lpm_2, vcov = vcovHC(lpm_2, type = "HC1"))
  robust_lpm3 <- coeftest(lpm_3, vcov = vcovHC(lpm_3, type = "HC1"))

  cat(sprintf("%-50s %10s %10s %10s\n", "Predictor", "LPM(1)", "LPM(2)", "LPM(3)"))
  cat(paste(rep("-", 85), collapse = ""), "\n")
  cat(sprintf("%-50s %10.4f %10.4f %10s\n", "Loss % (std)",
              robust_lpm1["loss_pct_z", 1], robust_lpm2["loss_pct_z", 1], ""))
  cat(sprintf("%-50s %10s %10.4f %10s\n", "  p-value",
              sprintf("%.4f", robust_lpm1["loss_pct_z", 4]),
              robust_lpm2["loss_pct_z", 4], ""))
  cat(sprintf("%-50s %10s %10.4f %10.4f\n", "N slaves (std)",
              "", robust_lpm2["num_slaves_z", 1], robust_lpm3["num_slaves_z", 1]))
  cat(sprintf("%-50s %10s %10s %10.4f\n", "Compensation rate (std)",
              "", "", robust_lpm3["compensation_rate_z", 1]))
  cat(sprintf("%-50s %10s %10s %10s\n", "  p-value",
              "", "", sprintf("%.4f", robust_lpm3["compensation_rate_z", 4])))
  cat(sprintf("%-50s %10s %10s %10.4f\n", "Mean slave value (std)",
              "", "", robust_lpm3["mean_slave_value_z", 1]))
  cat(sprintf("%-50s %10.4f %10.4f %10.4f\n", "R-squared",
              summary(lpm_1)$r.squared, summary(lpm_2)$r.squared,
              summary(lpm_3)$r.squared))
  cat(sprintf("%-50s %10d %10d %10d\n", "N",
              nobs(lpm_1), nobs(lpm_2), nobs(lpm_3)))
  cat("District FE included in all models. Robust (HC1) standard errors.\n")

  # ==========================================================================
  # 16.3.9 VISUALIZATIONS: COMPENSATION GAP
  # ==========================================================================

  cat("\n\n--- Creating Compensation Gap Visualizations ---\n")

  # ----- GRAPH: Distribution of compensation rates by VT status -----
  p_comp_rate <- ggplot(owners_with_valuation %>%
                           mutate(group = ifelse(is_voortrekker, "Voortrekker", "Non-Voortrekker")),
                         aes(x = compensation_rate, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.025, alpha = 0.6, position = "identity") +
    geom_density(alpha = 0.3) +
    scale_x_continuous(labels = scales::percent_format()) +
    coord_cartesian(xlim=c(0,1.2)) +
    scale_fill_manual(values = c("Voortrekker" = "#5C2346", "Non-Voortrekker" = "#3D8EB9")) +
    labs(x = "Compensation Rate (Compensation / Valuation)",
         y = "Density",
         fill = "Group") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_comp_rate)
  fig_file <- next_fig("emancipation_compensation_rate_dist.png")
  save_leap_fig(fig_file, p_comp_rate, width = 8, height = 5, dpi = 300)

  # ----- GRAPH: VT selection rate by loss quartile -----
  p_quartile <- ggplot(quartile_rates,
                        aes(x = loss_quartile_label, y = vt_rate)) +
    geom_bar(stat = "identity", fill = "#5C2346", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", vt_rate, n_total)),
              vjust = -0.3, size = 3.5, color = "#2D2D2D") +
    geom_hline(yintercept = base_rate * 100, linetype = "dashed", color = "#AAAAAA") +
    annotate("text", x = 4, y = base_rate * 100,
             label = sprintf("Overall VT rate: %.1f%%", base_rate * 100),
             hjust = 1, vjust = -0.5, color = "#5A5A5A", size = 3) +
    labs(x = "Loss Percentage Quartile",
         y = "Voortrekker Rate (%)") +
    theme_leap() +
    ylim(0, max(quartile_rates$vt_rate) * 1.3)

  print(p_quartile)
  fig_file <- next_fig("emancipation_loss_quartile_vt_rate.png")
  save_leap_fig(fig_file, p_quartile, width = 8, height = 5, dpi = 300)

  # ----- GRAPH: Probit coefficient forest plot -----
  probit_coef_data <- data.frame(
    model = c("M1: Loss % only",
              "M2: + District FE",
              "M3: + N slaves + Dist FE",
              "M5: + N slaves + log val + Dist FE"),
    coef = c(s1["loss_pct_z", 1], s2["loss_pct_z", 1],
             s3["loss_pct_z", 1], s5["loss_pct_z", 1]),
    se = c(s1["loss_pct_z", 2], s2["loss_pct_z", 2],
           s3["loss_pct_z", 2], s5["loss_pct_z", 2]),
    p_val = c(s1["loss_pct_z", 4], s2["loss_pct_z", 4],
              s3["loss_pct_z", 4], s5["loss_pct_z", 4])
  ) %>%
    mutate(
      ci_lower = coef - 1.96 * se,
      ci_upper = coef + 1.96 * se,
      significant = p_val < 0.05,
      model = factor(model, levels = rev(model))
    )

  p_probit_forest <- ggplot(probit_coef_data, aes(x = coef, y = model,
                                                    color = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, linewidth = 1.5) +
    geom_point(size = 4) +
    scale_color_manual(values = c("TRUE" = "#5C2346", "FALSE" = "#AAAAAA"),
                       labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
    labs(x = "Probit Coefficient on Loss Percentage (standardized)",
         y = "",
         color = "Significance") +
    theme_leap() +
    theme(legend.position = "bottom")

  print(p_probit_forest)
  fig_file <- next_fig("emancipation_probit_forest.png")
  save_leap_fig(fig_file, p_probit_forest, width = 8, height = 5, dpi = 300)

  # ----- GRAPH: Within-district loss comparison -----
  if (nrow(district_comp) >= 3) {
    district_comp_plot <- district_comp %>%
      mutate(district_std = reorder(district_std, comp_rate_diff))

    p_district_loss <- ggplot(district_comp_plot,
                               aes(x = comp_rate_diff * 100, y = district_std)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.8) +
      geom_point(size = 4, color = "#5C2346") +
      geom_text(aes(label = sprintf("VT n=%d", n_TRUE)),
                hjust = -0.2, size = 3, color = "#5A5A5A") +
      labs(x = "Compensation Rate Difference (VT - Non-VT, percentage points)",
           y = "") +
      theme_leap()

    print(p_district_loss)
    fig_file <- next_fig("emancipation_within_district_comp_rate.png")
    save_leap_fig(fig_file, p_district_loss, width = 8, height = 5, dpi = 300)
  }

  # Save expanded emancipation results
  emancipation_expanded_results <- data.frame(
    model = c("Probit M1", "Probit M2 (Dist FE)", "Probit M3 (+ N slaves)",
              "Probit M4 (Comp rate)", "Probit M5 (+ log val)",
              "LPM 1 (Dist FE)", "LPM 2 (+ N slaves)", "LPM 3 (Comp rate)"),
    key_predictor = c(rep("Loss %", 3), "Comp rate", "Loss %",
                      "Loss %", "Loss %", "Comp rate"),
    coef = c(s1["loss_pct_z", 1], s2["loss_pct_z", 1], s3["loss_pct_z", 1],
             s4["compensation_rate_z", 1], s5["loss_pct_z", 1],
             robust_lpm1["loss_pct_z", 1], robust_lpm2["loss_pct_z", 1],
             robust_lpm3["compensation_rate_z", 1]),
    p_value = c(s1["loss_pct_z", 4], s2["loss_pct_z", 4], s3["loss_pct_z", 4],
                s4["compensation_rate_z", 4], s5["loss_pct_z", 4],
                robust_lpm1["loss_pct_z", 4], robust_lpm2["loss_pct_z", 4],
                robust_lpm3["compensation_rate_z", 4]),
    district_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    n_slaves_control = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )

  write.csv(emancipation_expanded_results,
            "output/tables/emancipation_expanded_probit_results.csv", row.names = FALSE)
  cat("\nExpanded results saved to: Output/emancipation_expanded_probit_results.csv\n")

  # ------------------------------------------------------------------------
  # 16.3.9b ROBUSTNESS: CENSUS-CORROBORATED OWNER MATCHES ONLY
  # ------------------------------------------------------------------------

  if (sum(owners_with_valuation$is_voortrekker_census, na.rm = TRUE) >= 20) {
    cat("\n--- Census-Corroborated Emancipation Robustness ---\n")
    cat("Restricting treated owners to those matched through both the census and compensation linkage.\n")

    probit_c1 <- glm(is_voortrekker_census ~ loss_pct_z,
                     data = owners_with_valuation, family = binomial(link = "probit"))
    probit_c2 <- glm(is_voortrekker_census ~ loss_pct_z + factor(district_std),
                     data = owners_with_valuation, family = binomial(link = "probit"))
    probit_c3 <- glm(is_voortrekker_census ~ loss_pct_z + num_slaves_z + factor(district_std),
                     data = owners_with_valuation, family = binomial(link = "probit"))
    probit_c4 <- glm(is_voortrekker_census ~ compensation_rate_z + num_slaves_z +
                       mean_slave_value_z + factor(district_std),
                     data = owners_with_valuation, family = binomial(link = "probit"))
    probit_c5 <- glm(is_voortrekker_census ~ loss_pct_z + num_slaves_z +
                       log_valuation_z + factor(district_std),
                     data = owners_with_valuation, family = binomial(link = "probit"))

    lpm_c1 <- lm(is_voortrekker_census ~ loss_pct_z + factor(district_std),
                 data = owners_with_valuation)
    lpm_c2 <- lm(is_voortrekker_census ~ loss_pct_z + num_slaves_z + factor(district_std),
                 data = owners_with_valuation)
    lpm_c3 <- lm(is_voortrekker_census ~ compensation_rate_z + num_slaves_z +
                   mean_slave_value_z + factor(district_std),
                 data = owners_with_valuation)

    sc1 <- summary(probit_c1)$coefficients
    sc2 <- summary(probit_c2)$coefficients
    sc3 <- summary(probit_c3)$coefficients
    sc4 <- summary(probit_c4)$coefficients
    sc5 <- summary(probit_c5)$coefficients
    rlpm_c1 <- coeftest(lpm_c1, vcov = vcovHC(lpm_c1, type = "HC1"))
    rlpm_c2 <- coeftest(lpm_c2, vcov = vcovHC(lpm_c2, type = "HC1"))
    rlpm_c3 <- coeftest(lpm_c3, vcov = vcovHC(lpm_c3, type = "HC1"))

    census_results <- data.frame(
      sample = "census_corroborated_owner_subset",
      model = c("Probit M1", "Probit M2 (Dist FE)", "Probit M3 (+ N slaves)",
                "Probit M4 (Comp rate)", "Probit M5 (+ log val)",
                "LPM 1 (Dist FE)", "LPM 2 (+ N slaves)", "LPM 3 (Comp rate)"),
      key_predictor = c(rep("Loss %", 3), "Comp rate", "Loss %",
                        "Loss %", "Loss %", "Comp rate"),
      coef = c(sc1["loss_pct_z", 1], sc2["loss_pct_z", 1], sc3["loss_pct_z", 1],
               sc4["compensation_rate_z", 1], sc5["loss_pct_z", 1],
               rlpm_c1["loss_pct_z", 1], rlpm_c2["loss_pct_z", 1],
               rlpm_c3["compensation_rate_z", 1]),
      p_value = c(sc1["loss_pct_z", 4], sc2["loss_pct_z", 4], sc3["loss_pct_z", 4],
                  sc4["compensation_rate_z", 4], sc5["loss_pct_z", 4],
                  rlpm_c1["loss_pct_z", 4], rlpm_c2["loss_pct_z", 4],
                  rlpm_c3["compensation_rate_z", 4]),
      district_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      n_slaves_control = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
      treated_owner_n = sum(owners_with_valuation$is_voortrekker_census, na.rm = TRUE),
      total_owner_n = nrow(owners_with_valuation)
    )

    write.csv(census_results,
              "output/tables/emancipation_census_corroborated_results.csv", row.names = FALSE)
    cat("Saved census-corroborated emancipation robustness results.\n")
  } else {
    cat("\nSkipping census-corroborated emancipation regressions: fewer than 20 treated owners.\n")
  }

  # --------------------------------------------------------------------------
  # 16.3.10 NUANCED SUMMARY AND CONCLUSIONS
  # --------------------------------------------------------------------------

  cat("\n\n")
  cat("================================================================\n")
  cat("    SLAVE EMANCIPATION: COMPREHENSIVE SUMMARY                   \n")
  cat("================================================================\n\n")

  cat("FINDING 1: SLAVE OWNERSHIP (Scale)\n")
  cat(sprintf("  Voortrekkers owned %s slaves than non-Voortrekkers.\n",
              ifelse(robust_slaves["is_voortrekkerTRUE", 1] > 0, "MORE", "FEWER")))
  cat(sprintf("  Coefficient: %.2f slaves (p = %.4f)\n",
              robust_slaves["is_voortrekkerTRUE", 1],
              robust_slaves["is_voortrekkerTRUE", 4]))
  cat(sprintf("  Effect size: %.3f SD\n\n",
              coeftest(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                          data = slave_owners_analysis),
                       vcov = vcovHC(lm(num_slaves_std ~ is_voortrekker + factor(district_std),
                                        data = slave_owners_analysis), type = "HC1"))["is_voortrekkerTRUE", 1]))

  cat("FINDING 2: ABSOLUTE LOSS\n")
  cat(sprintf("  Voortrekkers lost %s in absolute terms from emancipation.\n",
              ifelse(robust_loss["is_voortrekkerTRUE", 1] > 0, "MORE", "LESS")))
  cat(sprintf("  Coefficient: £%.2f (p = %.4f)\n",
              robust_loss["is_voortrekkerTRUE", 1],
              robust_loss["is_voortrekkerTRUE", 4]))
  cat(sprintf("  Effect size: %.3f SD\n\n", robust_loss_std["is_voortrekkerTRUE", 1]))

  cat("FINDING 3: COMPENSATION RATE (the 'unfairness' channel)\n")
  cat(sprintf("  VT mean compensation rate: %.1f%%\n", vt_comp$mean_comp_rate * 100))
  cat(sprintf("  Non-VT mean compensation rate: %.1f%%\n", non_vt_comp$mean_comp_rate * 100))
  cat(sprintf("  Difference: %+.1f pp\n", (vt_comp$mean_comp_rate - non_vt_comp$mean_comp_rate) * 100))
  cat(sprintf("  Probit (M3, loss %%, controlling for N slaves + district):\n"))
  cat(sprintf("    Coefficient: %.4f, p = %.4f\n", s3["loss_pct_z", 1], s3["loss_pct_z", 4]))
  cat(sprintf("    Average marginal effect: %+.2f pp per SD\n\n", ame_loss_pct_3 * 100))

  cat("FINDING 4: LOSS INTENSITY GRADIENT\n")
  cat(sprintf("  VT rate in Q1 (lowest loss): %.1f%%\n", q1_rate))
  cat(sprintf("  VT rate in Q4 (highest loss): %.1f%%\n", q4_rate))
  cat(sprintf("  Q4/Q1 ratio: %.2f\n", q4_rate / q1_rate))
  cat(sprintf("  Trend test: rho = %.3f, p = %.4f\n\n",
              trend_cor$estimate, trend_cor$p.value))

  cat("INTERPRETATION: The conditional probit coefficient and unconditional rank/quartile tests answer different questions.\n")
  cat(sprintf("  Model 3 loss coefficient p=%.6f; quartile chi-square p=%.6f; Q4/Q1 Fisher p=%.6f.\n", s3["loss_pct_z",4],chisq_result$p.value,fisher_q4q1$p.value))
  cat("  A small, imprecise conditional coefficient does not prove no grievance effect.\n")
  # Save results
  write.csv(emancipation_matches, "output/tables/voortrekker_emancipation_matches.csv", row.names = FALSE)
  cat("\nResults saved to: voortrekker_emancipation_matches.csv\n")

  owner_link_summary <- emancipation_matches %>%
    group_by(owner_key, owner_surname, owner_name, owner_district) %>%
    summarise(
      n_link_rows = n(),
      n_vt_rows = n_distinct(vt_row_id),
      any_census_corroborated = any(census_corroborated %in% TRUE),
      n_census_corroborated_vt_rows = n_distinct(vt_row_id[census_corroborated %in% TRUE]),
      max_match_score = max(match_score, na.rm = TRUE),
      mean_match_score = mean(match_score, na.rm = TRUE),
      .groups = "drop"
    )

  emancipation_linkage_diagnostics <- tibble(
    metric = c(
      "link_rows_total",
      "unique_vt_rows_total",
      "unique_owner_keys_total",
      "link_rows_census_corroborated",
      "unique_vt_rows_census_corroborated",
      "unique_owner_keys_census_corroborated",
      "link_rows_uncorroborated",
      "unique_vt_rows_uncorroborated",
      "unique_owner_keys_uncorroborated"
    ),
    value = c(
      nrow(emancipation_matches),
      n_distinct(emancipation_matches$vt_row_id),
      n_distinct(emancipation_matches$owner_key),
      sum(emancipation_matches$census_corroborated %in% TRUE),
      n_distinct(emancipation_matches$vt_row_id[emancipation_matches$census_corroborated %in% TRUE]),
      sum(owner_link_summary$any_census_corroborated %in% TRUE),
      sum(emancipation_matches$census_corroborated %in% FALSE),
      n_distinct(emancipation_matches$vt_row_id[emancipation_matches$census_corroborated %in% FALSE]),
      sum(owner_link_summary$any_census_corroborated %in% FALSE)
    )
  )

  write.csv(emancipation_linkage_diagnostics,
            "output/tables/emancipation_linkage_diagnostics.csv", row.names = FALSE)
  write.csv(owner_link_summary,
            "output/tables/emancipation_owner_linkage_summary.csv", row.names = FALSE)
  cat("Saved emancipation linkage diagnostics and owner-level summary.\n")

} else {
  cat("\nInsufficient matches for emancipation analysis (need >= 20).\n")
  if (!is.null(emancipation_matches)) {
    cat("Matches found:", nrow(emancipation_matches), "\n")
  }
}



cat("\n========== SLAVE EMANCIPATION ANALYSIS COMPLETE ==========\n")


# ============================================================================
# FIGURE SUMMARY
# ============================================================================

cat("\n\n")
cat("================================================================\n")
cat("                    FIGURES CREATED                              \n")
cat("================================================================\n\n")

cat("All figures saved to: Voortrekker/Figures/\n\n")
cat(sprintf("Total figures created: %d\n\n", fig_num))

cat("Figure listing:\n")
fig_files <- list.files("Figures", pattern = "^Fig.*\\.png$", full.names = FALSE)
for (f in sort(fig_files)) {
  cat(" ", f, "\n")
}

# ============================================================================
# COPY KEY OUTPUTS SUMMARY
# ============================================================================
cat("\nKey output files in Output/ directory:\n")
for (f in c("output/tables/voortrekker_matches.csv",
            "output/tables/voortrekker_results_all_methods.csv",
            "output/tables/voortrekker_emancipation_matches.csv",
            "output/tables/voortrekker_matches_xgboost.csv",
            "output/tables/migration_timing_regressions.csv",
            "output/tables/emancipation_expanded_probit_results.csv",
            "output/tables/manual_review_all_matches.xlsx")) {
  if (file.exists(f)) cat("  ✓", f, "\n")
}
cat("\nFigures are in Voortrekker/Figures/.\n")

cat("\n================================================================\n")
cat("                    ANALYSIS COMPLETE                            \n")
cat("================================================================\n")

# household-level spouse corroboration replaces the district flag
# (see the sample without spouse agreement above): TRUE for trekker households whose link has
# spouse agreement; FALSE for controls and for links without it.
analysis_dataset_main <- analysis_dataset_main %>%
  mutate(wife_info_available = census_id %in% spouse_agree_ids)


# ==============================================================================
# SECTION C: ROBUSTNESS CHECKS
# Paper Section 7: Robustness + Appendices B-G
# ==============================================================================

# Reload from frozen canonical dataset for robustness
all_districts <- analysis_dataset_main
vt_matches <- read.csv("output/tables/voortrekker_matches.csv", stringsAsFactors = FALSE)
outdir <- "output/tables/"

cat("Loading data...\n")
cat("  Analysis dataset:", nrow(all_districts), "rows,",
    sum(all_districts$is_voortrekker), "Voortrekkers\n")
cat("  VT matches:", nrow(vt_matches), "rows\n")

# Define variable families for multiple testing
wealth_vars <- c("horses", "cattle", "sheep", "total_slaves", "total_khoe",
                 "wheat_sown", "wheat_reaped", "wine", "wealth_index")
hh_vars <- c("settler_men", "settler_women", "settler_children",
             "settler_adults", "household_size", "children_ratio")
all_outcome_vars <- c(wealth_vars, hh_vars)


# ============================================================================
# A1. HIGH-CONFIDENCE MATCH ROBUSTNESS (E2)
# ============================================================================

cat("\n========== A1: HIGH-CONFIDENCE MATCH ROBUSTNESS ==========\n")

df_highconf <- analysis_dataset_main

# Join RF scores to analysis dataset
score_lookup <- vt_matches %>%
  select(census_id, match_score) %>%
  distinct(census_id, .keep_all = TRUE)
n_before_score_join <- nrow(all_districts)
all_districts <- all_districts %>%
  left_join(score_lookup, by = "census_id")
stopifnot(nrow(all_districts) == n_before_score_join)

# Define high-confidence subset: top quartile of RF scores among matched VTs
vt_scores <- all_districts %>% filter(is_voortrekker) %>% pull(match_score)
q75_cutoff <- quantile(vt_scores, 0.75, na.rm = TRUE)
cat("  RF score quartile cutoff (75th pct):", round(q75_cutoff, 3), "\n")
cat("  High-confidence matches (score >=", round(q75_cutoff, 3), "):",
    sum(vt_scores >= q75_cutoff, na.rm = TRUE), "\n")

# wife_info_available: the household's link has spouse agreement (set above)
# Used for the spouse-agreement robustness check

# Create indicators
all_districts <- all_districts %>%
  mutate(
    is_vt_highconf = is_voortrekker & !is.na(match_score) & match_score >= q75_cutoff
  )

# Run district FE regressions for full sample, high-confidence, and top-half
key_vars <- c("wealth_index", "total_slaves", "household_size",
              "settler_children", "horses", "cattle", "wheat_sown",
              "settler_men", "children_ratio")

highconf_results <- data.frame()
for (v in key_vars) {
  if (!v %in% names(all_districts)) next

  # Full sample
  reg_full <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                 data = all_districts)
  rob_full <- coeftest(reg_full, vcov = vcovHC(reg_full, type = "HC1"))

  # Retain original controls; exclude other known trekkers from this restriction.
  df_hc <- all_districts %>%
    filter(!is_voortrekker | is_vt_highconf) %>%
    mutate(is_voortrekker_hc = is_vt_highconf)
  reg_hc <- lm(as.formula(paste0(v, " ~ is_voortrekker_hc + factor(district)")),
               data = df_hc)
  rob_hc <- coeftest(reg_hc, vcov = vcovHC(reg_hc, type = "HC1"))

  highconf_results <- rbind(highconf_results, data.frame(
    variable = v,
    coef_full = rob_full["is_voortrekkerTRUE", 1],
    se_full = rob_full["is_voortrekkerTRUE", 2],
    p_full = rob_full["is_voortrekkerTRUE", 4],
    coef_highconf = rob_hc["is_voortrekker_hcTRUE", 1],
    se_highconf = rob_hc["is_voortrekker_hcTRUE", 2],
    p_highconf = rob_hc["is_voortrekker_hcTRUE", 4],
    n_full = sum(all_districts$is_voortrekker),
    n_highconf = sum(df_hc$is_voortrekker_hc),
    n_total_highconf = nobs(reg_hc),
    n_controls_highconf = sum(!df_hc$is_voortrekker_hc),
    score_cutoff = q75_cutoff
  ))
}

write.csv(highconf_results, paste0(outdir, "robustness_highconf.csv"), row.names = FALSE)
cat("  Exported robustness_highconf.csv\n")
print(highconf_results %>% select(variable, coef_full, p_full, coef_highconf, p_highconf))


# ============================================================================
# A2. FORMAL TABLE: LINKS WITHOUT SPOUSE AGREEMENT (E3)
# ============================================================================

cat("\n========== A2: ROBUSTNESS: LINKS WITHOUT SPOUSE AGREEMENT ==========\n")

# wife_info_available marks trekker links with spouse agreement
df_no_agree <- analysis_dataset_main %>%
  filter(!wife_info_available)
no_agree_sample <- df_no_agree
cat("  No spouse agreement:", n_distinct(no_agree_sample$district), "\n")
cat("  VTs in links without spouse agreement:", sum(no_agree_sample$is_voortrekker), "\n")

nw_vars <- c("household_size", "settler_children", "children_ratio",
             "settler_men", "total_slaves", "cattle", "sheep",
             "wealth_index", "horses", "wheat_sown")

no_agree_results <- data.frame()
for (v in nw_vars) {
  if (!v %in% names(all_districts)) next

  # Full sample
  reg_full <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                 data = all_districts)
  rob_full <- coeftest(reg_full, vcov = vcovHC(reg_full, type = "HC1"))

  # No agreement
  reg_nw <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
               data = no_agree_sample)
  rob_nw <- coeftest(reg_nw, vcov = vcovHC(reg_nw, type = "HC1"))

  no_agree_results <- rbind(no_agree_results, data.frame(
    variable = v,
    coef_full = rob_full["is_voortrekkerTRUE", 1],
    se_full = rob_full["is_voortrekkerTRUE", 2],
    p_full = rob_full["is_voortrekkerTRUE", 4],
    coef_no_agree = rob_nw["is_voortrekkerTRUE", 1],
    se_no_agree = rob_nw["is_voortrekkerTRUE", 2],
    p_no_agree = rob_nw["is_voortrekkerTRUE", 4],
    n_full = nrow(all_districts),
    n_no_agree = nrow(no_agree_sample),
    n_vt_full = sum(all_districts$is_voortrekker),
    n_vt_no_agree = sum(no_agree_sample$is_voortrekker)
  ))
}

write.csv(no_agree_results, paste0(outdir, "robustness_no_spouse_agreement.csv"), row.names = FALSE)
cat("  Exported robustness_no_spouse_agreement.csv\n")
print(no_agree_results %>% select(variable, coef_full, p_full, coef_no_agree, p_no_agree))


# ============================================================================
# A2b. MALE-HEADED CONTROL GROUP
# The genealogical matching sample requires an adult male on the trekker side.
# If an adult male was a de facto precondition for trekking, control
# households without one are a less meaningful comparison group. Re-run the
# district FE specification restricting controls to households with at least
# one adult settler man.
# ============================================================================

cat("\n========== A2b: MALE-HEADED CONTROL GROUP ==========\n")

df_male <- analysis_dataset_main %>%
  filter(is_voortrekker | settler_men >= 1)

cat("  Full sample:", nrow(analysis_dataset_main), "households (",
    sum(analysis_dataset_main$is_voortrekker), "VT )\n")
cat("  Male-headed-control sample:", nrow(df_male), "households (",
    sum(df_male$is_voortrekker), "VT )\n")
cat("  Female-headed control households dropped:",
    nrow(analysis_dataset_main) - nrow(df_male), "\n")
cat("  VT households with settler_men == 0 (retained):",
    sum(analysis_dataset_main$is_voortrekker &
        analysis_dataset_main$settler_men == 0), "\n")

male_vars <- c("wealth_index", "total_slaves", "total_khoe",
               "horses", "cattle", "sheep", "wheat_sown", "wheat_reaped",
               "wine", "settler_men", "settler_women", "settler_children",
               "household_size", "children_ratio")

male_results <- data.frame()
for (v in male_vars) {
  if (!v %in% names(df_male)) next

  reg_full <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                 data = analysis_dataset_main)
  rob_full <- coeftest(reg_full, vcov = vcovHC(reg_full, type = "HC1"))

  reg_male <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
                 data = df_male)
  rob_male <- coeftest(reg_male, vcov = vcovHC(reg_male, type = "HC1"))

  male_results <- rbind(male_results, data.frame(
    variable = v,
    coef_full = rob_full["is_voortrekkerTRUE", 1],
    se_full = rob_full["is_voortrekkerTRUE", 2],
    p_full = rob_full["is_voortrekkerTRUE", 4],
    coef_male = rob_male["is_voortrekkerTRUE", 1],
    se_male = rob_male["is_voortrekkerTRUE", 2],
    p_male = rob_male["is_voortrekkerTRUE", 4],
    n_full = nrow(analysis_dataset_main),
    n_male = nrow(df_male),
    n_vt = sum(df_male$is_voortrekker)
  ))
}

write.csv(male_results, paste0(outdir, "robustness_male_headed.csv"),
          row.names = FALSE)
cat("  Exported robustness_male_headed.csv\n")
print(male_results %>% select(variable, coef_full, p_full, coef_male, p_male))

# Tex fragment: full-sample vs male-headed-control estimates side by side
male_labels <- c(
  wealth_index = "Wealth index", total_slaves = "Slaves",
  total_khoe = "Khoekhoe workers", horses = "Horses", cattle = "Cattle",
  sheep = "Sheep", wheat_sown = "Wheat sown (muids)",
  wheat_reaped = "Wheat reaped (muids)", wine = "Wine (leaguers)",
  settler_men = "Settler men", settler_women = "Settler women",
  settler_children = "Settler children", household_size = "Household size",
  children_ratio = "Children ratio"
)
tex_male <- character(0)
for (i in seq_len(nrow(male_results))) {
  r <- male_results[i, ]
  tex_male <- c(tex_male,
    sprintf("%s & %s%s & (%s) & %s%s & (%s) \\\\",
            male_labels[[r$variable]],
            fmt_est(r$coef_full), stars_for(r$p_full), fmt_p(r$p_full),
            fmt_est(r$coef_male), stars_for(r$p_male), fmt_p(r$p_male)))
}
tex_male <- c(tex_male,
  "\\midrule",
  sprintf("Observations & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          format(male_results$n_full[1], big.mark = ","),
          format(male_results$n_male[1], big.mark = ",")))
write_tex_fragment(tex_male, "output/tables/tex/tab_male_headed_body.tex")


# ============================================================================
# A3. WITHIN-COHORT HOUSEHOLD-SIZE TABLE (E4)
# ============================================================================

cat("\n========== A3: AGE-COHORT ANALYSIS ==========\n")

age_lookup <- NULL

if (exists("best_matches") && exists("vt_adults") &&
    "row_id" %in% names(best_matches) && "row_id" %in% names(vt_adults) &&
    "birth_yr" %in% names(vt_adults)) {
  age_lookup <- best_matches %>%
    filter(n_vt_per_census == 1 & !identity_ambiguous) %>%
    select(census_id, row_id) %>%
    left_join(vt_adults %>% select(row_id, birth_yr), by = "row_id") %>%
    filter(!is.na(birth_yr) & birth_yr > 1700 & birth_yr < 1825) %>%
    mutate(age_in_1825 = 1825 - birth_yr) %>%
    distinct(census_id, .keep_all = TRUE)

  cat("  Using row_id-based age lookup from best_matches/vt_adults.\n")
} else {
  cat("  Falling back to name-based age lookup.\n")
  vt_raw <- read_xlsx("data/raw/Voortrekkers 2.xlsx")
  birth_col <- names(vt_raw)[names(vt_raw) %in% c("Birth year", "Birthyear", "Birth OR baptise year")]

  if (length(birth_col) > 0) {
    birth_col <- birth_col[1]
    vt_births <- vt_raw %>%
      mutate(birth_yr = suppressWarnings(as.integer(.[[birth_col]]))) %>%
      filter(!is.na(birth_yr) & birth_yr > 1700 & birth_yr < 1825) %>%
      mutate(
        age_in_1825 = 1825 - birth_yr,
        surname_std = toupper(trimws(as.character(`surname (no spaces)`))),
        name_std = toupper(trimws(as.character(NAME)))
      ) %>%
      group_by(surname_std, name_std) %>%
      summarise(
        birth_yr = suppressWarnings(first(sort(unique(birth_yr)))),
        age_in_1825 = suppressWarnings(first(sort(unique(age_in_1825)))),
        .groups = "drop"
      )

    age_lookup <- vt_matches %>%
      mutate(
        surname_std = toupper(trimws(vt_surname)),
        name_std = toupper(trimws(vt_name))
      ) %>%
      left_join(vt_births %>% select(surname_std, name_std, birth_yr, age_in_1825),
                by = c("surname_std", "name_std")) %>%
      filter(!is.na(age_in_1825)) %>%
      select(census_id, birth_yr, age_in_1825) %>%
      distinct(census_id, .keep_all = TRUE)
  }
}

if (!is.null(age_lookup) && nrow(age_lookup) > 0) {
  cat("  VTs with birth year data:", nrow(age_lookup), "of", nrow(vt_matches), "\n")

  for (col in c("birth_yr", "age_in_1825", "age_group")) {
    if (col %in% names(all_districts)) all_districts[[col]] <- NULL
  }

  n_before_age_join <- nrow(all_districts)
  all_districts <- all_districts %>%
    left_join(age_lookup, by = "census_id")
  stopifnot(nrow(all_districts) == n_before_age_join)

  all_districts <- all_districts %>%
    mutate(age_group = case_when(
      age_in_1825 >= 15 & age_in_1825 < 25 ~ "15-24",
      age_in_1825 >= 25 & age_in_1825 < 35 ~ "25-34",
      age_in_1825 >= 35 & age_in_1825 < 45 ~ "35-44",
      age_in_1825 >= 45 & age_in_1825 < 55 ~ "45-54",
      age_in_1825 >= 55 ~ "55+",
      TRUE ~ NA_character_
    ))

  nonvt_means <- all_districts %>%
    filter(!is_voortrekker) %>%
    summarise(
      nonvt_mean_hhsize = mean(household_size, na.rm = TRUE),
      nonvt_mean_children = mean(settler_children, na.rm = TRUE),
      nonvt_mean_chratio = mean(children_ratio, na.rm = TRUE)
    )

  cohort_results <- all_districts %>%
    filter(is_voortrekker & !is.na(age_group)) %>%
    group_by(age_group) %>%
    summarise(
      n_vt = n(),
      vt_mean_hhsize = mean(household_size, na.rm = TRUE),
      vt_mean_children = mean(settler_children, na.rm = TRUE),
      vt_mean_chratio = mean(children_ratio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      nonvt_mean_hhsize = nonvt_means$nonvt_mean_hhsize,
      nonvt_mean_children = nonvt_means$nonvt_mean_children,
      diff_hhsize = vt_mean_hhsize - nonvt_means$nonvt_mean_hhsize,
      diff_children = vt_mean_children - nonvt_means$nonvt_mean_children
    )

  total_vt <- all_districts %>% filter(is_voortrekker & !is.na(age_group))
  cohort_results <- bind_rows(cohort_results, data.frame(
    age_group = "All with birth year",
    n_vt = nrow(total_vt),
    vt_mean_hhsize = mean(total_vt$household_size, na.rm = TRUE),
    vt_mean_children = mean(total_vt$settler_children, na.rm = TRUE),
    vt_mean_chratio = mean(total_vt$children_ratio, na.rm = TRUE),
    nonvt_mean_hhsize = nonvt_means$nonvt_mean_hhsize,
    nonvt_mean_children = nonvt_means$nonvt_mean_children,
    diff_hhsize = mean(total_vt$household_size, na.rm = TRUE) - nonvt_means$nonvt_mean_hhsize,
    diff_children = mean(total_vt$settler_children, na.rm = TRUE) - nonvt_means$nonvt_mean_children
  ))

  write.csv(cohort_results, paste0(outdir, "robustness_age_cohorts.csv"), row.names = FALSE)
  cat("  Exported robustness_age_cohorts.csv\n")
  print(cohort_results)
} else {
  cat("  WARNING: no valid age lookup could be constructed.\n")
}

# Settler men distribution
settler_men_dist <- all_districts %>%
  group_by(is_voortrekker) %>%
  summarise(
    n = n(),
    mean = mean(settler_men, na.rm = TRUE),
    sd = sd(settler_men, na.rm = TRUE),
    min = min(settler_men, na.rm = TRUE),
    max = max(settler_men, na.rm = TRUE),
    pct_exactly_1 = mean(settler_men == 1, na.rm = TRUE) * 100,
    pct_gte_2 = mean(settler_men >= 2, na.rm = TRUE) * 100,
    .groups = "drop"
  )
write.csv(settler_men_dist, paste0(outdir, "settler_men_distribution.csv"), row.names = FALSE)
cat("\n  Exported settler_men_distribution.csv\n")
print(settler_men_dist)


# ============================================================================
# A4. MULTIPLE TESTING CORRECTIONS (E5)
# ============================================================================

cat("\n========== A4: MULTIPLE TESTING CORRECTIONS ==========\n")

# Run all outcome regressions and collect results
all_reg_results <- data.frame()
for (v in all_outcome_vars) {
  if (!v %in% names(all_districts)) next
  reg <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
            data = all_districts)
  rob <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))

  family <- ifelse(v %in% wealth_vars, "wealth", "household_composition")

  all_reg_results <- rbind(all_reg_results, data.frame(
    variable = v,
    family = family,
    coef = rob["is_voortrekkerTRUE", 1],
    se = rob["is_voortrekkerTRUE", 2],
    p_value = rob["is_voortrekkerTRUE", 4]
  ))
}

# Apply multiple-testing corrections within each family: Holm (conservative)
# and Benjamini-Hochberg FDR (less conservative, appropriate when outcomes are
# mechanically correlated through accounting identities, e.g. settler_children
# + settler_adults = household_size).
all_reg_results <- all_reg_results %>%
  group_by(family) %>%
  mutate(
    p_holm = p.adjust(p_value, method = "holm"),
    p_bh = p.adjust(p_value, method = "BH"),
    sig_original = p_value < 0.05,
    sig_holm = p_holm < 0.05,
    sig_bh = p_bh < 0.05
  ) %>%
  ungroup()

write.csv(all_reg_results, paste0(outdir, "regression_results_adjusted.csv"), row.names = FALSE)
cat("  Exported regression_results_adjusted.csv\n")
cat("\n  Results with Holm and Benjamini-Hochberg corrections:\n")
print(all_reg_results %>% select(variable, family, coef, p_value, p_holm, p_bh, sig_original, sig_holm, sig_bh))


# ============================================================================
# A5. TOST AT MULTIPLE BOUNDS (E6)
# ============================================================================

cat("\n========== A5: TOST SENSITIVITY ANALYSIS ==========\n")

# >>> A5 BEGIN
# All nine wealth components plus the index, so that equivalence claims can be
# stated variable by variable.
tost_vars <- c("wealth_index", "total_slaves", "total_khoe", "horses", "cattle", "sheep",
               "wheat_sown", "wheat_reaped", "wine", "household_size", "settler_children")
sesoi_values <- c(0.05, 0.10, 0.15, 0.20)

tost_results <- data.frame()
for (v in tost_vars) {
  if (!v %in% names(all_districts)) next

  # Get the standardised coefficient
  sd_v <- sd(all_districts[[v]], na.rm = TRUE)
  reg <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")),
            data = all_districts)
  rob <- coeftest(reg, vcov = vcovHC(reg, type = "HC1"))
  coef_val <- rob["is_voortrekkerTRUE", 1]
  se_val <- rob["is_voortrekkerTRUE", 2]
  coef_std <- coef_val / sd_v

  for (sesoi in sesoi_values) {
    # TOST: two one-sided tests
    # H0: |effect| >= sesoi, H1: |effect| < sesoi
    bound_raw <- sesoi * sd_v  # convert SESOI from SD units to raw units

    # Test 1: effect > -bound (lower bound test)
    t_lower <- (coef_val - (-bound_raw)) / se_val
    p_lower <- pt(t_lower, df = reg$df.residual, lower.tail = FALSE)
    # Wait, TOST for equivalence: we want to reject that the effect is outside [-bound, bound]
    # Test 1: H0: beta <= -bound => t = (beta - (-bound))/se, reject if t > t_alpha
    p_lower <- 1 - pt((coef_val - (-bound_raw)) / se_val, df = reg$df.residual)
    # Test 2: H0: beta >= bound => t = (beta - bound)/se, reject if t < -t_alpha
    p_upper <- pt((coef_val - bound_raw) / se_val, df = reg$df.residual)

    # TOST p-value = max of the two one-sided p-values
    tost_p <- max(p_lower, p_upper)

    # 90% CI for the coefficient (corresponds to alpha=0.05 two one-sided)
    ci_lower <- coef_val - qt(0.95, df = reg$df.residual) * se_val
    ci_upper <- coef_val + qt(0.95, df = reg$df.residual) * se_val

    tost_results <- rbind(tost_results, data.frame(
      variable = v,
      sd = sd_v,
      coef_raw = coef_val,
      coef_std = coef_std,
      se = se_val,
      sesoi_sd = sesoi,
      sesoi_raw = bound_raw,
      ci90_lower = ci_lower,
      ci90_upper = ci_upper,
      ci90_lower_std = ci_lower / sd_v,
      ci90_upper_std = ci_upper / sd_v,
      tost_p = tost_p,
      equivalent = tost_p < 0.05
    ))
  }
}

write.csv(tost_results, paste0(outdir, "tost_sensitivity.csv"), row.names = FALSE)
cat("  Exported tost_sensitivity.csv\n")
# >>> A5 END
cat("\n  TOST results (wealth_index):\n")
print(tost_results %>% filter(variable == "wealth_index") %>%
        select(sesoi_sd, coef_std, ci90_lower_std, ci90_upper_std, tost_p, equivalent))


# ============================================================================
# A6. KHOEKHOE BETWEEN/WITHIN VARIANCE DECOMPOSITION
# ============================================================================

cat("\n========== A6: KHOEKHOE VARIANCE DECOMPOSITION ==========\n")

khoe_data <- all_districts %>%
  filter(!is.na(total_khoe)) %>%
  group_by(district) %>%
  mutate(district_mean_khoe = mean(total_khoe, na.rm = TRUE)) %>%
  ungroup()

total_var <- var(khoe_data$total_khoe, na.rm = TRUE)
between_var <- var(khoe_data$district_mean_khoe, na.rm = TRUE)
within_var <- mean((khoe_data$total_khoe - khoe_data$district_mean_khoe)^2, na.rm = TRUE)

# More precise: weighted between-group variance
grand_mean <- mean(khoe_data$total_khoe, na.rm = TRUE)
between_ss <- sum((khoe_data$district_mean_khoe - grand_mean)^2)
within_ss <- sum((khoe_data$total_khoe - khoe_data$district_mean_khoe)^2)
total_ss <- sum((khoe_data$total_khoe - grand_mean)^2)

khoe_decomp <- data.frame(
  component = c("Total", "Between-district", "Within-district"),
  sum_of_squares = c(total_ss, between_ss, within_ss),
  fraction = c(1.0, between_ss / total_ss, within_ss / total_ss),
  description = c(
    "Total variance in Khoekhoe workers",
    "Variance explained by district means (absorbed by FE)",
    "Residual variance available for VT coefficient identification"
  )
)

# Also: VT coefficient on Khoekhoe with and without district FE
reg_khoe_nofe <- lm(total_khoe ~ is_voortrekker, data = all_districts)
rob_khoe_nofe <- coeftest(reg_khoe_nofe, vcov = vcovHC(reg_khoe_nofe, type = "HC1"))
reg_khoe_fe <- lm(total_khoe ~ is_voortrekker + factor(district), data = all_districts)
rob_khoe_fe <- coeftest(reg_khoe_fe, vcov = vcovHC(reg_khoe_fe, type = "HC1"))

khoe_reg <- data.frame(
  specification = c("No FE", "District FE"),
  coef = c(rob_khoe_nofe["is_voortrekkerTRUE", 1],
           rob_khoe_fe["is_voortrekkerTRUE", 1]),
  se = c(rob_khoe_nofe["is_voortrekkerTRUE", 2],
         rob_khoe_fe["is_voortrekkerTRUE", 2]),
  p_value = c(rob_khoe_nofe["is_voortrekkerTRUE", 4],
              rob_khoe_fe["is_voortrekkerTRUE", 4])
)

write.csv(khoe_decomp, paste0(outdir, "khoe_variance_decomposition.csv"), row.names = FALSE)
write.csv(khoe_reg, paste0(outdir, "khoe_regression_comparison.csv"), row.names = FALSE)
cat("  Exported khoe_variance_decomposition.csv and khoe_regression_comparison.csv\n")
print(khoe_decomp)
cat("\n  Khoekhoe regression with/without FE:\n")
print(khoe_reg)


# ============================================================================
# A7. INTER-RATER RELIABILITY
# ============================================================================

cat("\n========== A7: INTER-RATER RELIABILITY ==========\n")

# Load the four labeller files
lab1 <- read_xlsx("data/raw/training_sample_for_labeling.xlsx")
lab2 <- read_xlsx("data/raw/training_sample_labeler2.xlsx")
lab3 <- read_xlsx("data/raw/training_sample_labeler3.xlsx")
lab4 <- read_xlsx("data/raw/training_sample_labeler4.xlsx")

# Check for sample_id overlap
has_sample_id <- all(c("sample_id" %in% names(lab1), "sample_id" %in% names(lab2),
                       "sample_id" %in% names(lab3), "sample_id" %in% names(lab4)))

if (has_sample_id) {
  # Clean labels
  clean_lab <- function(df, labeller_name) {
    df %>%
      mutate(LABEL = suppressWarnings(as.integer(LABEL))) %>%
      filter(!is.na(LABEL), LABEL %in% c(0L, 1L)) %>%
      select(sample_id, LABEL) %>%
      rename(!!labeller_name := LABEL)
  }

  l1 <- clean_lab(lab1, "labeller_1")
  l2 <- clean_lab(lab2, "labeller_2")
  l3 <- clean_lab(lab3, "labeller_3")
  l4 <- clean_lab(lab4, "labeller_4")

  # Find pairwise overlaps
  irr_results <- data.frame()

  pairs <- list(
    c("labeller_1", "labeller_2"),
    c("labeller_1", "labeller_3"),
    c("labeller_1", "labeller_4"),
    c("labeller_2", "labeller_3"),
    c("labeller_2", "labeller_4"),
    c("labeller_3", "labeller_4")
  )

  lab_list <- list(labeller_1 = l1, labeller_2 = l2, labeller_3 = l3, labeller_4 = l4)

  for (pair in pairs) {
    a_name <- pair[1]
    b_name <- pair[2]
    overlap <- inner_join(lab_list[[a_name]], lab_list[[b_name]], by = "sample_id")

    if (nrow(overlap) > 0) {
      a_vals <- overlap[[a_name]]
      b_vals <- overlap[[b_name]]
      agreement <- mean(a_vals == b_vals)

      # Cohen's kappa
      # Observed agreement
      po <- agreement
      # Expected agreement by chance
      p_a1 <- mean(a_vals == 1)
      p_b1 <- mean(b_vals == 1)
      p_a0 <- 1 - p_a1
      p_b0 <- 1 - p_b1
      pe <- p_a1 * p_b1 + p_a0 * p_b0
      kappa <- (po - pe) / (1 - pe)

      irr_results <- rbind(irr_results, data.frame(
        labeller_a = a_name,
        labeller_b = b_name,
        n_overlap = nrow(overlap),
        pct_agreement = round(agreement * 100, 1),
        cohens_kappa = round(kappa, 3),
        n_agree_match = sum(a_vals == 1 & b_vals == 1),
        n_agree_nonmatch = sum(a_vals == 0 & b_vals == 0),
        n_disagree = sum(a_vals != b_vals)
      ))
    }
  }

  if (nrow(irr_results) > 0) {
    write.csv(irr_results, paste0(outdir, "interrater_reliability.csv"), row.names = FALSE)
    cat("  Exported interrater_reliability.csv\n")
    print(irr_results)
  } else {
    cat("  No overlapping pairs found across labellers.\n")
    # Report that labellers evaluated non-overlapping sets
    irr_summary <- data.frame(
      finding = "Labellers evaluated non-overlapping sample sets",
      labeller_1_n = nrow(l1),
      labeller_2_n = nrow(l2),
      labeller_3_n = nrow(l3),
      labeller_4_n = nrow(l4),
      note = "Inter-rater reliability cannot be computed"
    )
    write.csv(irr_summary, paste0(outdir, "interrater_reliability.csv"), row.names = FALSE)
    cat("  Exported interrater_reliability.csv (no overlaps)\n")
  }

} else {
  cat("  sample_id column not found in all labeller files.\n")
  cat("  Checking for overlap by name matching...\n")

  # Try matching by name columns
  make_key <- function(df) {
    paste(df$vt_surname, df$vt_first_name, df$census_name, df$census_district, sep = "|")
  }
  k1 <- make_key(lab1)
  k2 <- make_key(lab2)
  overlap_12 <- sum(k1 %in% k2)
  cat("  Overlap between labeller 1 and 2:", overlap_12, "pairs\n")

  irr_summary <- data.frame(
    finding = "sample_id not available; name-based overlap check performed",
    overlap_1_2 = overlap_12,
    note = "Limited inter-rater reliability assessment possible"
  )
  write.csv(irr_summary, paste0(outdir, "interrater_reliability.csv"), row.names = FALSE)
}


# ============================================================================
# A8. MATCH-TIER DECOMPOSITION (RF vs Manual matches)
# ============================================================================

cat("\n========== A8: MATCH-TIER DECOMPOSITION ==========\n")

df_tiers <- analysis_dataset_main

# Create match tier indicator in analysis dataset
# "RF" = the linkage-final classifier proposed the link; "Manual" = the
# link was retained only through review (adjudicated although not proposed).
proposed_keys <- with(prop_spouse$top %>% filter(status == "proposed"), paste(row_id, census_id))
tier_lookup <- vt_matches %>%
  mutate(match_tier = ifelse(paste(row_id, census_id) %in% proposed_keys |
                               Sys.getenv("VT_LINKAGE", "spouse") == "blind", "RF", "Manual")) %>%
  group_by(census_id) %>%
  summarise(match_tier=ifelse(any(match_tier == "RF"), "RF", "Manual"), .groups="drop")

# Remove match_tier if it already exists from a prior run
if ("match_tier" %in% names(all_districts)) {
  all_districts <- all_districts %>% select(-match_tier)
}

all_districts <- all_districts %>%
  left_join(tier_lookup, by = "census_id") %>%
  mutate(match_tier = ifelse(is.na(match_tier), "Non-VT", match_tier))

cat("  RF matches:", sum(all_districts$match_tier == "RF"), "\n")
cat("  Manual matches:", sum(all_districts$match_tier == "Manual"), "\n")
cat("  Non-VT:", sum(all_districts$match_tier == "Non-VT"), "\n")

# Run district FE regressions for each tier
tier_vars <- c("wealth_index", "household_size", "settler_children", "settler_men",
               "total_slaves", "horses", "cattle", "wheat_sown")

tier_results <- data.frame()

for (tier in c("Full", "RF", "Manual")) {
  for (v in tier_vars) {
    if (tier == "Full") {
      df_sub <- all_districts
    } else if (tier == "RF") {
      df_sub <- all_districts %>% filter(match_tier %in% c("RF", "Non-VT"))
    } else {
      df_sub <- all_districts %>% filter(match_tier %in% c("Manual", "Non-VT"))
    }

    # Create VT indicator for this subsample
    df_sub <- df_sub %>% mutate(is_vt_sub = match_tier != "Non-VT")

    fml <- as.formula(paste0(v, " ~ is_vt_sub + factor(district)"))
    fit <- tryCatch({
      mod <- lm(fml, data = df_sub)
      ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
      data.frame(
        tier = tier,
        variable = v,
        n_total = nrow(df_sub),
        n_vt = sum(df_sub$is_vt_sub),
        coef = ct["is_vt_subTRUE", "Estimate"],
        se = ct["is_vt_subTRUE", "Std. Error"],
        p = ct["is_vt_subTRUE", "Pr(>|t|)"]
      )
    }, error = function(e) {
      data.frame(tier = tier, variable = v, n_total = NA, n_vt = NA,
                 coef = NA, se = NA, p = NA)
    })

    tier_results <- bind_rows(tier_results, fit)
  }
}

write.csv(tier_results, paste0(outdir, "match_tier_decomposition.csv"), row.names = FALSE)
cat("  Exported match_tier_decomposition.csv\n")
print(tier_results %>% filter(variable %in% c("wealth_index", "household_size",
                                                "settler_children", "settler_men")))

# Tex fragment: tier decomposition table,
# foregrounded from the appendix into the robustness section.
tier_labels <- c(
  wealth_index = "Wealth index", household_size = "Household size",
  settler_children = "Settler children", settler_men = "Settler men",
  total_slaves = "Slaves", horses = "Horses", cattle = "Cattle",
  wheat_sown = "Wheat sown (muids)"
)
tier_wide <- tier_results %>%
  select(tier, variable, coef, se, p) %>%
  tidyr::pivot_wider(names_from = tier, values_from = c(coef, se, p))
tex_tier <- character(0)
for (v in names(tier_labels)) {
  r <- tier_wide %>% filter(variable == v)
  if (nrow(r) != 1) next
  tex_tier <- c(tex_tier,
    sprintf("%s & %s%s & %s%s & %s%s \\\\",
            tier_labels[[v]],
            fmt_est(r$coef_Full),   stars_for(r$p_Full),
            fmt_est(r$coef_RF),     stars_for(r$p_RF),
            fmt_est(r$coef_Manual), stars_for(r$p_Manual)),
    sprintf(" & (%s) & (%s) & (%s) \\\\",
            fmt_p(r$p_Full), fmt_p(r$p_RF), fmt_p(r$p_Manual)))
}
tier_ns <- tier_results %>%
  group_by(tier) %>% summarise(n_vt = suppressWarnings(max(n_vt, na.rm = TRUE)), .groups = "drop") %>%
  mutate(n_vt = as.integer(ifelse(is.finite(n_vt), n_vt, 0)))   # an empty tier prints 0
for (tt in c("Full", "RF", "Manual")) if (!tt %in% tier_ns$tier) tier_ns <- bind_rows(tier_ns, tibble(tier = tt, n_vt = 0L))
tex_tier <- c(tex_tier,
  "\\midrule",
  sprintf("Matched VT households & %d & %d & %d \\\\",
          tier_ns$n_vt[tier_ns$tier == "Full"],
          tier_ns$n_vt[tier_ns$tier == "RF"],
          tier_ns$n_vt[tier_ns$tier == "Manual"]))
write_tex_fragment(tex_tier, "output/tables/tex/tab_match_tiers_body.tex")


# ============================================================================
# A8b. MALE-HEADED CONTROLS WITHIN THE RF-ACCEPTED TIER AND THE NO-SPOUSE-AGREEMENT
# DISTRICTS
# Intersects existing checks. The male-headed control restriction of A2b is
# applied (i) within the RF-accepted tier of A8 and (ii) within the no-spouse-agreement
# districts of A2. The first two column pairs of the tex fragment repeat the
# A2b estimates so that the appendix table reads from one source.
# ============================================================================
# >>> A8b BEGIN
cat("\n========== A8b: MALE-HEADED CONTROLS BY SUBSAMPLE ==========\n")

a8b_vars <- c("wealth_index", "total_slaves", "total_khoe",
              "horses", "cattle", "sheep", "wheat_sown", "wheat_reaped",
              "wine", "settler_men", "settler_women", "settler_children",
              "household_size", "children_ratio")
a8b_labels <- c(
  wealth_index = "Wealth index", total_slaves = "Slaves",
  total_khoe = "Khoekhoe workers", horses = "Horses", cattle = "Cattle",
  sheep = "Sheep", wheat_sown = "Wheat sown (muids)",
  wheat_reaped = "Wheat reaped (muids)", wine = "Wine (leaguers)",
  settler_men = "Settler men", settler_women = "Settler women",
  settler_children = "Settler children", household_size = "Household size",
  children_ratio = "Children ratio"
)
# Same estimation data as A2b (analysis_dataset_main); the tier flag built in
# A8 is attached by census household.
stopifnot("match_tier" %in% names(all_districts),
          "wife_info_available" %in% names(analysis_dataset_main))
a8b_base <- analysis_dataset_main %>%
  select(-any_of("match_tier")) %>%
  left_join(all_districts %>% select(census_id, match_tier), by = "census_id")
stopifnot(!any(is.na(a8b_base$match_tier)),
          all((a8b_base$match_tier != "Non-VT") == a8b_base$is_voortrekker))
a8b_male <- a8b_base %>% filter(is_voortrekker | settler_men >= 1)
a8b_samples <- list(
  full_controls = a8b_base,
  male_controls = a8b_male,
  male_rf       = a8b_male %>% filter(match_tier %in% c("RF", "Non-VT")),
  male_no_agree  = a8b_male %>% filter(!wife_info_available)
)
a8b_results <- data.frame()
for (smp in names(a8b_samples)) {
  df_sub <- a8b_samples[[smp]]
  for (v in a8b_vars) {
    mod <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")), data = df_sub)
    ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
    a8b_results <- rbind(a8b_results, data.frame(
      sample = smp, variable = v,
      coef = ct["is_voortrekkerTRUE", 1], se = ct["is_voortrekkerTRUE", 2],
      p = ct["is_voortrekkerTRUE", 4],
      n = nrow(df_sub), n_vt = sum(df_sub$is_voortrekker)))
  }
}
write.csv(a8b_results, paste0(outdir, "robustness_male_headed_subsamples.csv"), row.names = FALSE)
cat("  Exported robustness_male_headed_subsamples.csv\n")
print(a8b_results %>% filter(variable %in% c("household_size", "settler_children", "children_ratio", "wealth_index")))

a8b_cell <- function(smp, v) {
  r <- a8b_results[a8b_results$sample == smp & a8b_results$variable == v, ]
  sprintf("%s%s & (%s)", fmt_est(r$coef), stars_for(r$p), fmt_p(r$p))
}
a8b_n <- function(smp, col) format(a8b_results[a8b_results$sample == smp, col][1], big.mark = ",")
tex_a8b <- character(0)
for (v in a8b_vars) {
  tex_a8b <- c(tex_a8b, sprintf("%s & %s \\\\", a8b_labels[[v]],
    paste(sapply(names(a8b_samples), a8b_cell, v = v), collapse = " & ")))
}
tex_a8b <- c(tex_a8b, "\\midrule",
  sprintf("Observations & %s \\\\", paste(sprintf("\\multicolumn{2}{c}{%s}",
    sapply(names(a8b_samples), a8b_n, col = "n")), collapse = " & ")),
  sprintf("Matched VT households & %s \\\\", paste(sprintf("\\multicolumn{2}{c}{%s}",
    sapply(names(a8b_samples), a8b_n, col = "n_vt")), collapse = " & ")))
write_tex_fragment(tex_a8b, "output/tables/tex/tab_male_headed_subsamples_body.tex")
# >>> A8b END


# >>> A8c BEGIN
# ============================================================================
# A8c. DISTRICT-CLUSTERED INFERENCE: WILD CLUSTER BOOTSTRAP
# ============================================================================
# District FE regressions with errors clustered by district (ten districts).
# With so few clusters, conventional cluster-robust (CR1) t-tests over-reject,
# so we report wild cluster restricted bootstrap p-values (WCR; null imposed,
# Webb six-point weights, CR1-studentized, symmetric) alongside CR1. Because
# the fixed effects are at the cluster level, the regression is estimated by
# the within transformation, which reproduces the lm() coefficient exactly.
cat("\n========== A8c: WILD CLUSTER BOOTSTRAP (DISTRICT) ==========\n")

wcb_fe <- function(df, y, B = 99999, seed = 20261005) {
  df <- df[!is.na(df[[y]]), ]
  g <- as.character(df$district); yv <- df[[y]]; dv <- as.numeric(df$is_voortrekker)
  yt <- yv - ave(yv, g); dt <- dv - ave(dv, g)
  D <- sum(dt^2); beta <- sum(dt * yt) / D
  e <- yt - beta * dt
  G <- length(unique(g)); N <- length(yv); K <- G + 1
  adj <- G / (G - 1) * (N - 1) / (N - K)
  Ag <- tapply(dt^2, g, sum)
  se_cr1 <- sqrt(adj * sum(tapply(dt * e, g, sum)^2)) / D
  t_obs <- beta / se_cr1
  Sg <- tapply(dt * yt, g, sum)   # restricted residuals under beta = 0 are yt
  set.seed(seed)
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  W <- matrix(sample(webb, B * G, replace = TRUE), nrow = B)
  bstar <- as.vector(W %*% Sg) / D
  sc <- sweep(W, 2, Sg, `*`) - outer(bstar, Ag)
  tstar <- bstar / (sqrt(adj * rowSums(sc^2)) / D)
  data.frame(variable = y, coef = beta, se_cr1 = se_cr1, t_cr1 = t_obs,
             p_cr1 = 2 * pt(-abs(t_obs), df = G - 1),
             p_wcb = mean(abs(tstar) >= abs(t_obs)),
             n = N, n_vt = sum(dv), clusters = G,
             clusters_with_vt = sum(tapply(dv, g, sum) > 0), B = B)
}

# Same estimation data and tier flag as A8b.
wcb_no_agree_ids <- analysis_dataset_main$census_id[!analysis_dataset_main$wife_info_available]
wcb_samples <- list(
  full    = list(data = a8b_base,
                 vars = c("wealth_index", "household_size", "settler_children",
                          "settler_men", "total_khoe")),
  rf      = list(data = a8b_base %>% filter(match_tier %in% c("RF", "Non-VT")),
                 vars = c("household_size", "settler_children")),
  no_agree = list(data = a8b_base %>% filter(!wife_info_available),
                 vars = c("household_size", "settler_children"))
)
wcb_results <- bind_rows(lapply(names(wcb_samples), function(smp) {
  bind_rows(lapply(wcb_samples[[smp]]$vars, function(v)
    cbind(sample = smp, wcb_fe(wcb_samples[[smp]]$data, v))))
}))
write.csv(wcb_results, paste0(outdir, "wild_cluster_bootstrap.csv"), row.names = FALSE)
cat("  Exported wild_cluster_bootstrap.csv\n")
print(wcb_results)
# >>> A8c END


# >>> A8d BEGIN
# ============================================================================
# A8d. CONFIDENCE INTERVALS FOR THE EMANCIPATION AVERAGE MARGINAL EFFECTS
# ============================================================================
# Delta-method standard errors for the probit AMEs reported in Section 5
# (Models 3 and 5), using the model's conventional covariance matrix, as in
# Table 2.
cat("\n========== A8d: AME CONFIDENCE INTERVALS ==========\n")
ame_delta <- function(mod, term) {
  b <- coef(mod); b <- b[!is.na(b)]
  X <- model.matrix(mod)[, names(b), drop = FALSE]
  V <- vcov(mod)[names(b), names(b)]
  xb <- as.vector(X %*% b); phi <- dnorm(xb)
  ame <- mean(phi) * b[[term]]
  grad <- b[[term]] * colMeans(-xb * phi * X)
  grad[term] <- grad[term] + mean(phi)
  se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  c(ame = ame, se = se)
}
ame_m3 <- glm(is_voortrekker ~ loss_pct_z + num_slaves_z + factor(district_std),
              data = owners_with_valuation, family = binomial(link = "probit"))
ame_m5 <- glm(is_voortrekker ~ loss_pct_z + num_slaves_z + log_valuation_z + factor(district_std),
              data = owners_with_valuation, family = binomial(link = "probit"))
ame_ci <- bind_rows(lapply(list(c("m3", "loss_pct_z"), c("m3", "num_slaves_z"), c("m5", "loss_pct_z")),
  function(x) {
    r <- ame_delta(if (x[1] == "m3") ame_m3 else ame_m5, x[2])
    data.frame(model = x[1], term = x[2], ame_pp = 100 * r[["ame"]], se_pp = 100 * r[["se"]],
               lo_pp = 100 * (r[["ame"]] - 1.96 * r[["se"]]), hi_pp = 100 * (r[["ame"]] + 1.96 * r[["se"]]),
               trek_rate = 100 * mean(owners_with_valuation$is_voortrekker), n = nobs(ame_m3))
  }))
write.csv(ame_ci, paste0(outdir, "compensation_ame_ci.csv"), row.names = FALSE)
cat("  Exported compensation_ame_ci.csv\n")
print(ame_ci)
# >>> A8d END


# >>> A8e BEGIN
# ============================================================================
# A8e. ROBUSTNESS: SPOUSE EVIDENCE, COUPLES, DECISION PROVENANCE
# ============================================================================
# Samples:
#   spouse_agrees / spouse_not_agree: trekker households by the spouse-evidence
#     state of their link, each against all controls;
#   couples: male-headed households with a named wife, trekkers and controls
#     alike (changes the estimand; reported alongside the full sample);
#   models_agreed / adjudicated: trekker households by decision provenance.
cat("\n========== A8e: SPOUSE EVIDENCE, COUPLES, PROVENANCE ==========\n")
a8e_base <- analysis_dataset_main %>%
  left_join(all_districts %>% select(census_id, a8e_role = head_role, a8e_wife = wife_name_raw), by = "census_id")
prov <- best_matches %>% group_by(census_id) %>%
  summarise(state = if (any(evidence_state %in% "agrees")) "agrees" else
                      if (any(evidence_state %in% "contradicts")) "contradicts" else "not_comparable",
            provenance = if (Sys.getenv("VT_LINKAGE", "spouse") == "blind") "classifier_only" else
                           if (any(grepl("Johan", final_quality))) "adjudicated" else "models_agreed",
            .groups = "drop")
a8e_base <- a8e_base %>% left_join(prov, by = "census_id")
ctrl <- a8e_base %>% filter(!is_voortrekker)
trt <- a8e_base %>% filter(is_voortrekker)
a8e_samples <- list(
  full                  = a8e_base,
  spouse_agrees         = bind_rows(ctrl, trt %>% filter(state %in% "agrees")),
  spouse_not_comparable = bind_rows(ctrl, trt %>% filter(state %in% "not_comparable")),
  spouse_contradicts    = bind_rows(ctrl, trt %>% filter(state %in% "contradicts")),
  couples               = a8e_base %>% filter(a8e_role == "male", !is.na(a8e_wife)),
  models_agreed         = bind_rows(ctrl, trt %>% filter(provenance %in% "models_agreed")),
  adjudicated           = bind_rows(ctrl, trt %>% filter(provenance %in% "adjudicated")))
a8e_vars <- c("household_size", "settler_children", "children_ratio", "wealth_index", "total_slaves", "sheep")
a8e_results <- bind_rows(lapply(names(a8e_samples), function(smp) {
  dd <- a8e_samples[[smp]]
  if (sum(dd$is_voortrekker) < 10) return(NULL)
  bind_rows(lapply(a8e_vars, function(v) {
    mod <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")), data = dd)
    ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
    data.frame(sample = smp, variable = v, coef = ct["is_voortrekkerTRUE", 1], se = ct["is_voortrekkerTRUE", 2],
               p = ct["is_voortrekkerTRUE", 4], n = nobs(mod), n_vt = sum(dd$is_voortrekker))
  }))
}))
write.csv(a8e_results, paste0(outdir, "robustness_spouse_couples_provenance.csv"), row.names = FALSE)
cat("  Exported robustness_spouse_couples_provenance.csv\n")
print(a8e_results %>% filter(variable %in% c("household_size", "settler_children", "wealth_index")))
# >>> A8e END


# ============================================================================
# A9. AGE-COHORT DISTRICT FE REGRESSIONS
# ============================================================================

cat("\n========== A9: AGE-COHORT DISTRICT FE REGRESSIONS ==========\n")

# NOTE: Controls are not age-matched. This is a cohort-specific treated-group
# diagnostic, not a life-cycle-adjusted test. Interpret with caution.

# Uses age_group already created in A3 above
# Run household_size regression within each age cohort
cohort_fe_vars <- c("household_size", "settler_children", "wealth_index", "settler_men")

if ("age_group" %in% names(all_districts)) {
  cohorts <- c("25-34", "35-44", "45-54", "55+")
  cohort_fe_results <- data.frame()

  for (coh in cohorts) {
    for (v in cohort_fe_vars) {
      # Comparison: cohort-specific VTs vs ALL non-VTs (not age-matched)
      df_coh <- all_districts %>%
        filter(!is_voortrekker | (!is.na(age_group) & age_group == coh))

      n_vt_coh <- sum(df_coh$is_voortrekker)
      if (n_vt_coh < 5) next

      fml <- as.formula(paste0(v, " ~ is_voortrekker + factor(district)"))
      fit <- tryCatch({
        mod <- lm(fml, data = df_coh)
        ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
        data.frame(
          age_cohort = coh,
          variable = v,
          n_vt = n_vt_coh,
          n_total = nrow(df_coh),
          coef = ct["is_voortrekkerTRUE", "Estimate"],
          se = ct["is_voortrekkerTRUE", "Std. Error"],
          p = ct["is_voortrekkerTRUE", "Pr(>|t|)"]
        )
      }, error = function(e) {
        data.frame(age_cohort = coh, variable = v, n_vt = n_vt_coh,
                   n_total = NA, coef = NA, se = NA, p = NA)
      })

      cohort_fe_results <- bind_rows(cohort_fe_results, fit)
    }
  }

  # Add full-sample row for comparison
  for (v in cohort_fe_vars) {
    fml <- as.formula(paste0(v, " ~ is_voortrekker + factor(district)"))
    mod <- lm(fml, data = all_districts)
    ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
    cohort_fe_results <- bind_rows(cohort_fe_results, data.frame(
      age_cohort = "All",
      variable = v,
      n_vt = sum(all_districts$is_voortrekker),
      n_total = nrow(all_districts),
      coef = ct["is_voortrekkerTRUE", "Estimate"],
      se = ct["is_voortrekkerTRUE", "Std. Error"],
      p = ct["is_voortrekkerTRUE", "Pr(>|t|)"]
    ))
  }

  write.csv(cohort_fe_results, paste0(outdir, "age_cohort_fe_regressions.csv"), row.names = FALSE)
  cat("  Exported age_cohort_fe_regressions.csv\n")
  print(cohort_fe_results %>% filter(variable == "household_size"))
} else {
  cat("  WARNING: age_group not available (birth year data missing)\n")
}


# ============================================================================
# A10. ATTENUATION BIAS SIMULATION
# ============================================================================
#
# False positives in the treated group attenuate coefficients toward zero.
# This simulation shows how much the main results would shrink under
# alternative contamination rates.
# ============================================================================

cat("\n========== A10: ATTENUATION BIAS SIMULATION ==========\n")

set.seed(20260413)
n_sims <- 500
rf_precision <- cv_export$tp[best_idx] / (cv_export$tp[best_idx] + cv_export$fp[best_idx])
n_rf_accepted <- sum(tier_lookup$match_tier == "RF")
n_manual_rescued <- sum(tier_lookup$match_tier == "Manual")
rf_contamination_rate <- 1 - rf_precision
composite_upper_bound <- (n_manual_rescued + rf_contamination_rate * n_rf_accepted) /
  (n_rf_accepted + n_manual_rescued)

contamination_grid <- data.frame(
  scenario = c("baseline", "moderate_10", "moderate_20", "rf_benchmark",
               "composite_scenario"),
  fp_rate = c(0.00, 0.10, 0.20, rf_contamination_rate, composite_upper_bound)
)

sim_vars <- c("wealth_index", "household_size", "settler_children", "settler_men",
              "total_slaves")

vt_rows <- which(all_districts$is_voortrekker)
nonvt_rows <- which(!all_districts$is_voortrekker)
n_vt <- length(vt_rows)
nonvt_rows_by_district <- split(nonvt_rows, all_districts$district[nonvt_rows])

sample_same_district_donors <- function(rows_to_replace, donor_index_by_district, district_vec) {
  vapply(rows_to_replace, function(r) {
    donor_pool <- donor_index_by_district[[as.character(district_vec[r])]]
    if (is.null(donor_pool) || length(donor_pool) == 0) {
      stop("No non-Voortrekker donors available in district: ", as.character(district_vec[r]))
    }
    sample(donor_pool, 1, replace = TRUE)
  }, integer(1))
}

attenuation_results <- data.frame()
attenuation_significance <- data.frame()

for (i in seq_len(nrow(contamination_grid))) {
  scenario <- contamination_grid$scenario[i]
  fp_rate <- contamination_grid$fp_rate[i]
  n_contaminate <- round(n_vt * fp_rate)
  cat("  Scenario:", scenario, "- contamination rate:", round(fp_rate, 3),
      "- replacing", n_contaminate, "of", n_vt, "VTs\n")

  sim_coefs <- matrix(NA_real_, nrow = n_sims, ncol = length(sim_vars))
  colnames(sim_coefs) <- sim_vars

  for (s in seq_len(n_sims)) {
    df_sim <- all_districts

    if (n_contaminate > 0) {
      rows_to_replace <- sample(vt_rows, n_contaminate)
      donor_rows <- sample_same_district_donors(
        rows_to_replace,
        nonvt_rows_by_district,
        all_districts$district
      )

      for (v in sim_vars) {
        df_sim[rows_to_replace, v] <- df_sim[donor_rows, v]
      }
    }

    for (j in seq_along(sim_vars)) {
      v <- sim_vars[j]
      mod <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")), data = df_sim)
      sim_coefs[s, j] <- coef(mod)["is_voortrekkerTRUE"]
    }
  }

  for (j in seq_along(sim_vars)) {
    attenuation_results <- bind_rows(attenuation_results, data.frame(
      scenario = scenario,
      fp_rate = fp_rate,
      variable = sim_vars[j],
      mean_coef = mean(sim_coefs[, j]),
      sd_coef = sd(sim_coefs[, j]),
      ci_lower = quantile(sim_coefs[, j], 0.025),
      ci_upper = quantile(sim_coefs[, j], 0.975)
    ))
  }

  # Use robust inference to measure significance retention at each contamination rate.
  if (fp_rate > 0) {
    sig_check <- data.frame()

    for (s in seq_len(min(n_sims, 200))) {
      df_sim <- all_districts
      rows_to_replace <- sample(vt_rows, n_contaminate)
      donor_rows <- sample_same_district_donors(
        rows_to_replace,
        nonvt_rows_by_district,
        all_districts$district
      )

      for (v in sim_vars) {
        df_sim[rows_to_replace, v] <- df_sim[donor_rows, v]
      }

      for (v in sim_vars) {
        mod <- lm(as.formula(paste0(v, " ~ is_voortrekker + factor(district)")), data = df_sim)
        ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
        sig_check <- bind_rows(sig_check, data.frame(
          scenario = scenario,
          fp_rate = fp_rate,
          variable = v,
          coef = ct["is_voortrekkerTRUE", "Estimate"],
          p_value = ct["is_voortrekkerTRUE", "Pr(>|t|)"],
          significant_05 = ct["is_voortrekkerTRUE", "Pr(>|t|)"] < 0.05
        ))
      }
    }

    attenuation_significance <- bind_rows(
      attenuation_significance,
      sig_check %>%
        group_by(scenario, fp_rate, variable) %>%
        summarise(
          mean_coef = mean(coef),
          mean_p = mean(p_value),
          pct_sig_05 = mean(significant_05) * 100,
          .groups = "drop"
        )
    )
  }
}

cat("  Significance retention summary:\n")
print(attenuation_significance)

write.csv(attenuation_results, paste0(outdir, "attenuation_simulation.csv"), row.names = FALSE)
write.csv(attenuation_significance, paste0(outdir, "attenuation_significance.csv"), row.names = FALSE)
cat("  Exported attenuation_simulation.csv and attenuation_significance.csv\n")


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n========== OUTPUTS SUMMARY ==========\n")
cat("All files saved to:", outdir, "\n\n")

output_files <- c(
  "robustness_highconf.csv",
  "robustness_no_spouse_agreement.csv",
  "robustness_age_cohorts.csv",
  "settler_men_distribution.csv",
  "regression_results_adjusted.csv",
  "tost_sensitivity.csv",
  "khoe_variance_decomposition.csv",
  "khoe_regression_comparison.csv",
  "interrater_reliability.csv",
  "match_tier_decomposition.csv",
  "robustness_male_headed_subsamples.csv",
  "age_cohort_fe_regressions.csv",
  "attenuation_simulation.csv",
  "attenuation_significance.csv"
)

for (f in output_files) {
  full_path <- paste0(outdir, f)
  if (file.exists(full_path)) {
    cat("  [OK]", f, "\n")
  } else {
    cat("  [MISSING]", f, "\n")
  }
}

cat("\nDone.\n")



# ==============================================================================
# SECTION D: MAPS AND DISTRICT-LEVEL FIGURES
# Paper Figures 1-3
# Sources: extract_district_means.R, extract_conditional_means.R,
#          create_map.R, create_map2.R, create_map3.R
# ==============================================================================

# Additional libraries needed for maps (not loaded in Section A)
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(maps)

# --------------------------------------------------------------------------
# D1: Extract district means (from extract_district_means.R)
# Uses all_districts already in memory from Section A
# --------------------------------------------------------------------------

# Compute aggregates
all_districts <- all_districts %>%
  mutate(
    total_slaves     = slaves_men + slaves_women + slaves_sons + slaves_daughters,
    total_khoe       = khoe_men + khoe_women + khoe_sons + khoe_daughters,
    settler_children = settler_sons + settler_daughters,
    household_size   = settler_men + settler_women + settler_sons + settler_daughters,
    horses           = horses_saddle + horses_breeding,
    cattle           = cattle_oxen + cattle_breeding,
    sheep            = sheep_wethers + sheep_breeding,
    wealth_simple    = as.numeric(scale(horses)) +
                       as.numeric(scale(cattle)) +
                       as.numeric(scale(sheep)) +
                       as.numeric(scale(total_slaves)) +
                       as.numeric(scale(wheat_reaped)) +
                       as.numeric(scale(wine))
  )

# Rename Cradock to Somerset (same district, renamed in 1825; census is from 1823)
# and merge Clanwilliam into Worcester (1825 boundaries)
all_districts$district[all_districts$district == "Cradock"] <- "Somerset"
all_districts$district[all_districts$district == "Clanwilliam"] <- "Worcester"

# District means
district_means <- all_districts %>%
  group_by(district) %>%
  summarise(
    mean_slaves   = mean(total_slaves, na.rm = TRUE),
    mean_children = mean(settler_children, na.rm = TRUE),
    mean_wealth   = mean(wealth_index, na.rm = TRUE),
    n_hh          = n(),
    .groups = "drop"
  )

write.csv(district_means, "output/tables/district_means_census.csv", row.names = FALSE)
cat("Saved district_means_census.csv\n")
print(as.data.frame(district_means))

# Load emancipation data
slave_owners <- slave_owners_analysis %>% mutate(
  district=case_when(district_std == "Clanwilliam" ~ "Worcester", TRUE ~ district_std))

emancipation_means <- slave_owners %>%
  group_by(district) %>%
  summarise(mean_loss = mean(loss, na.rm = TRUE),
            n_owners  = sum(!is.na(loss)), .groups = "drop")

write.csv(emancipation_means, "output/tables/district_means_emancipation.csv", row.names = FALSE)
cat("Saved district_means_emancipation.csv\n")
print(as.data.frame(emancipation_means))


# --------------------------------------------------------------------------
# D2: Extract conditional means - VT vs non-VT within-district differences
# (from extract_conditional_means.R)
# Uses all_districts already in memory from Section A
# --------------------------------------------------------------------------

# Compute children
all_districts <- all_districts %>%
  mutate(
    settler_children = settler_sons + settler_daughters
  )

# Rename Cradock to Somerset (same district, renamed in 1825; census is from 1823)
# and merge Clanwilliam into Worcester (1825 boundaries)
all_districts$district[all_districts$district == "Cradock"] <- "Somerset"
all_districts$district[all_districts$district == "Clanwilliam"] <- "Worcester"

# Flag VT households using saved matches
vt_matches <- read.csv("output/tables/voortrekker_matches.csv")
all_districts <- all_districts %>%
  mutate(is_voortrekker = census_id %in% vt_matches$census_id)

cat("Census VT matches:", sum(all_districts$is_voortrekker), "\n")

# Within-district mean differences: children
children_diff <- all_districts %>%
  group_by(district) %>%
  summarise(
    mean_children_vt    = mean(settler_children[is_voortrekker], na.rm = TRUE),
    mean_children_nonvt = mean(settler_children[!is_voortrekker], na.rm = TRUE),
    diff_children       = mean_children_vt - mean_children_nonvt,
    n_vt                = sum(is_voortrekker),
    n_nonvt             = sum(!is_voortrekker),
    .groups = "drop"
  )

cat("\nChildren differences by district:\n")
print(as.data.frame(children_diff))

# --------------------------------------------------------------------------
# 2. Emancipation: compensation loss, VT vs non-VT by district
# --------------------------------------------------------------------------

slave_owners <- slave_owners_analysis %>% mutate(
  district=case_when(district_std == "Clanwilliam" ~ "Worcester", TRUE ~ district_std))

cat("\nEmancipation VT matches:", sum(slave_owners$is_voortrekker), "\n")

# Within-district mean differences: compensation loss
loss_diff <- slave_owners %>%
  group_by(district) %>%
  summarise(
    mean_loss_vt    = mean(loss[is_voortrekker], na.rm = TRUE),
    mean_loss_nonvt = mean(loss[!is_voortrekker], na.rm = TRUE),
    diff_loss       = mean_loss_vt - mean_loss_nonvt,
    n_vt            = sum(is_voortrekker),
    n_nonvt         = sum(!is_voortrekker),
    .groups = "drop"
  )

cat("\nCompensation loss differences by district:\n")
print(as.data.frame(loss_diff))

# --------------------------------------------------------------------------
# 3. Save
# --------------------------------------------------------------------------

write.csv(children_diff, "output/tables/district_diff_children.csv", row.names = FALSE)
write.csv(loss_diff, "output/tables/district_diff_loss.csv", row.names = FALSE)
cat("\nSaved Output/district_diff_children.csv and Output/district_diff_loss.csv\n")


# >>> MAPS BEGIN
# Map extent: the full colony, taken from the shapefile's bounding box plus a
# small margin.
map_extent <- function(shp, pad_x = 0.2, pad_y = 0.15) {
  bb <- st_bbox(shp)
  list(x = c(bb[["xmin"]] - pad_x, bb[["xmax"]] + pad_x),
       y = c(bb[["ymin"]] - pad_y, bb[["ymax"]] + pad_y))
}
# The Cape and Stellenbosch districts are too small to hold their labels,
# which overlapped each other and the district boundaries. Both labels are
# placed in the adjacent ocean, in black, with a short leader line that ends
# inside the district.
outside_labels <- data.frame(
  district = c("Cape", "Stellenbosch"),
  x     = c(17.45, 18.65), y     = c(-33.40, -34.72),  # label centre
  lx    = c(17.75, 18.90), ly    = c(-33.40, -34.56),  # leader start, label side
  lxend = c(18.35, 18.90), lyend = c(-33.40, -34.28),  # leader end, inside district
  stringsAsFactors = FALSE
)
place_outside <- function(pos) {
  i <- match(pos$district, outside_labels$district)
  pos$x[!is.na(i)] <- outside_labels$x[i[!is.na(i)]]
  pos$y[!is.na(i)] <- outside_labels$y[i[!is.na(i)]]
  pos
}
# --------------------------------------------------------------------------
# D3: Figure 1 - Voortrekker records by district (from create_map.R)
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# 1. Load shapefile
# --------------------------------------------------------------------------

shp <- st_read("data/raw/Cape_colony_1825/Cape Colony 1825.shp",
               quiet = TRUE)
shp <- st_make_valid(shp)

# --------------------------------------------------------------------------
# 2. Voortrekker records per district
#    These counts are derived directly from the genealogical data and then
#    collapsed to 1825 district boundaries for mapping.
# --------------------------------------------------------------------------

vt_data <- vt_adults %>%
  count(census_districts, name = "n_vt") %>%
  mutate(district = collapse_vt_origin_district(census_districts)) %>%
  filter(!is.na(district), district != "Unknown") %>%
  group_by(district) %>%
  summarise(n_vt = sum(n_vt), .groups = "drop")

shp_data <- merge(shp, vt_data, by = "district", all.x = TRUE)
shp_data$n_vt[is.na(shp_data$n_vt)] <- 0

# --------------------------------------------------------------------------
# 3. Label positions (centroid-based, then manually adjusted)
# --------------------------------------------------------------------------

# Start from guaranteed-interior points
centroids <- st_point_on_surface(shp_data) %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(district, x, y)

# Manual nudges for districts where centroid falls near a border
nudge <- data.frame(
  district     = c("Cape",  "Stellenbosch", "Swellendam", "George",
                    "Uitenhage", "Albany", "Worcester", "Somerset"),
  dx           = c(-0.1,    0.0,            0.0,          0.0,
                    0.4,     0.0,           0.5,         -0.4),
  dy           = c( 0.15,   0.1,            0.05,         0.1,
                   -0.15,    0.0,           -0.3,         -0.5),
  stringsAsFactors = FALSE
)

label_pos <- centroids %>%
  left_join(nudge, by = "district") %>%
  mutate(x = x + ifelse(is.na(dx), 0, dx),
         y = y + ifelse(is.na(dy), 0, dy)) %>%
  select(district, x, y) %>%
  left_join(vt_data, by = "district") %>%
  mutate(
    vt_label = paste0(district, "\n(n = ", n_vt, ")"),
    text_col = ifelse(n_vt >= 120, "white", "black")
  ) %>%
  place_outside()

cape_town <- data.frame(x = 18.4241, y = -33.9249)

# --------------------------------------------------------------------------
# 4. Main map
# --------------------------------------------------------------------------

p_main <- ggplot(shp_data) +
  geom_sf(aes(fill = n_vt), colour = "grey30", linewidth = 0.4) +
  scale_fill_gradient(
    low  = "#F0E8ED",
    high = LEAP_COLORS["plum"],
    name = "Voortrekker\nrecords",
    breaks = c(0, 100, 200, 300)
  ) +
  geom_point(data = cape_town, aes(x = x, y = y),
             shape = 18, size = 3.5, colour = "black") +
  annotate("text", x = 18.4241, y = -33.9249, label = "Cape Town",
           hjust = 1.15, vjust = 0.3, size = 3, fontface = "italic") +
  geom_segment(data = outside_labels %>%
                 filter(district %in% label_pos$district[!is.na(label_pos$n_vt)]),
               aes(x = lx, y = ly, xend = lxend, yend = lyend),
               colour = "grey30", linewidth = 0.25) +
  geom_text(data = label_pos,
            aes(x = x, y = y, label = vt_label, colour = text_col),
            size = 2.8, lineheight = 0.85, show.legend = FALSE) +
  scale_colour_identity() +
  coord_sf(xlim = c(map_extent(shp)$x[1] - 2.4, map_extent(shp)$x[2]),
           ylim = map_extent(shp)$y, expand = FALSE) +
  theme_void() +
  theme(
    legend.position      = "none",
    plot.margin          = ggplot2::margin(5, 5, 5, 5)
  )

# --------------------------------------------------------------------------
# 5. Africa inset
# --------------------------------------------------------------------------

world <- map_data("world")

# Dissolve all district polygons into a single Cape Colony outline
colony <- st_union(shp) %>% st_as_sf()

p_inset <- ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               fill = "grey85", colour = "grey50", linewidth = 0.15) +
  geom_sf(data = colony, fill = LEAP_COLORS["plum"], colour = LEAP_COLORS["plum"], linewidth = 0.3) +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 40), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey30",
                                    linewidth = 0.4),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )

# --------------------------------------------------------------------------
# 6. Combine: main map with inset in top-left corner
# --------------------------------------------------------------------------

p_combined <- p_main +
  inset_element(p_inset,
                left = 0.005, right = 0.18,
                bottom = 0.64, top = 0.995,
                align_to = "panel")

ggsave("output/figures/Fig00_cape_colony_map.png", p_combined,
       width = 8, height = 4.5, dpi = 300, bg = "white")
ggsave("output/figures/Fig00_cape_colony_map.pdf", p_combined,
       width = 8, height = 4.5, bg = "white")

cat("Figure saved to Voortrekker/Figures/Fig00_cape_colony_map.png and .pdf\n")


# --------------------------------------------------------------------------
# D4: Figure 2 - District-level characteristic choropleths (from create_map2.R)
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# 1. Load pre-computed district means
# --------------------------------------------------------------------------

district_means    <- read.csv("output/tables/district_means_census.csv")
emancipation_means <- read.csv("output/tables/district_means_emancipation.csv")

cat("Census means:\n")
print(district_means)
cat("\nEmancipation means:\n")
print(emancipation_means)

# --------------------------------------------------------------------------
# 2. Load shapefile and merge
# --------------------------------------------------------------------------

shp <- st_read("data/raw/Cape_colony_1825/Cape Colony 1825.shp",
               quiet = TRUE)
shp <- st_make_valid(shp)

map_data <- shp %>%
  left_join(district_means, by = "district") %>%
  left_join(emancipation_means, by = "district")

# --------------------------------------------------------------------------
# 3. Label positions (centroid-based, matching create_map.R)
# --------------------------------------------------------------------------

centroids <- st_point_on_surface(map_data) %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(district, x, y)

# Manual nudges (same as create_map.R)
nudge <- data.frame(
  district     = c("Cape",  "Stellenbosch", "Swellendam", "George",
                    "Uitenhage", "Albany", "Worcester", "Somerset"),
  dx           = c(-0.1,    0.0,            0.0,          0.0,
                    0.4,     0.0,           0.5,         -0.4),
  dy           = c( 0.15,   0.1,            0.05,         0.1,
                   -0.15,    0.0,           -0.3,         -0.5),
  stringsAsFactors = FALSE
)

label_pos <- centroids %>%
  left_join(nudge, by = "district") %>%
  mutate(x = x + ifelse(is.na(dx), 0, dx),
         y = y + ifelse(is.na(dy), 0, dy)) %>%
  select(district, x, y) %>%
  place_outside()

xlims <- map_extent(shp)$x
ylims <- map_extent(shp)$y

# --------------------------------------------------------------------------
# 4. Panel builder
# --------------------------------------------------------------------------

make_panel <- function(shp_data, fill_var, title,
                       label_data, fmt = "%.1f") {

  vals <- st_drop_geometry(shp_data) %>% select(district, value = !!sym(fill_var))
  rng  <- range(vals$value, na.rm = TRUE)

  lbl <- label_data %>%
    left_join(vals, by = "district") %>%
    mutate(
      lbl_text = ifelse(is.na(value), district,
                        paste0(district, "\n(", sprintf(fmt, value), ")")),
      text_col = ifelse(!is.na(value) &
                        value > (rng[1] + 0.65 * diff(rng)),
                        "white", "black")
    )

  lbl$text_col[lbl$district %in% outside_labels$district] <- "black"

  ggplot(shp_data) +
    geom_sf(aes(fill = .data[[fill_var]]), colour = "grey30", linewidth = 0.3) +
    scale_fill_gradient(low = "#F0E8ED", high = LEAP_COLORS["plum"],
                        na.value = "white") +
    geom_segment(data = outside_labels,
                 aes(x = lx, y = ly, xend = lxend, yend = lyend),
                 colour = "grey30", linewidth = 0.25) +
    geom_text(data = lbl,
              aes(x = x, y = y, label = lbl_text, colour = text_col),
              size = 2.2, lineheight = 0.85, show.legend = FALSE) +
    scale_colour_identity() +
    labs(title = title) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 10, hjust = 0.5, margin = ggplot2::margin(b = 3)),
      legend.position = "none",
      plot.margin     = ggplot2::margin(2, 2, 2, 2)
    )
}

# --------------------------------------------------------------------------
# 5. Build the four panels
#    Panels (a), (c), (d): 1825 census (opgaafrolle)
#    Panel (b): Slave Emancipation Dataset (Ekama 2021)
# --------------------------------------------------------------------------

p_a <- make_panel(map_data, "mean_slaves",
                  "(a) Mean slaves per household (census)",
                  label_pos)

p_b <- make_panel(map_data, "mean_loss",
                  "(b) Mean compensation loss per slave owner (emancipation records)",
                  label_pos, fmt = "%.0f")

p_c <- make_panel(map_data, "mean_wealth",
                  "(c) Mean wealth index per household (census)",
                  label_pos)

p_d <- make_panel(map_data, "mean_children",
                  "(d) Mean children per household (census)",
                  label_pos)

# --------------------------------------------------------------------------
# 6. Combine 2x2 and save
# --------------------------------------------------------------------------

p_combined <- (p_a + p_b) / (p_c + p_d)

ggsave("output/figures/Fig00b_district_characteristics.png", p_combined,
       width = 10, height = 7.4, dpi = 300, bg = "white")
ggsave("output/figures/Fig00b_district_characteristics.pdf", p_combined,
       width = 10, height = 7.4, bg = "white")

cat("\nFigure saved to Voortrekker/Figures/Fig00b_district_characteristics.png and .pdf\n")


# --------------------------------------------------------------------------
# D5: Figure 3 - Within-district VT vs non-VT differences (from create_map3.R)
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# 1. Load pre-computed within-district differences
# --------------------------------------------------------------------------

children_diff <- read.csv("output/tables/district_diff_children.csv")
loss_diff     <- read.csv("output/tables/district_diff_loss.csv")

cat("Children differences:\n")
print(children_diff)
cat("\nLoss differences:\n")
print(loss_diff)

# --------------------------------------------------------------------------
# 2. Load shapefile and merge
# --------------------------------------------------------------------------

shp <- st_read("data/raw/Cape_colony_1825/Cape Colony 1825.shp",
               quiet = TRUE)
shp <- st_make_valid(shp)

map_data <- shp %>%
  left_join(children_diff %>% select(district, diff_children), by = "district") %>%
  left_join(loss_diff %>% select(district, diff_loss), by = "district")

# --------------------------------------------------------------------------
# 3. Label positions (centroid-based, matching create_map.R)
# --------------------------------------------------------------------------

centroids <- st_point_on_surface(map_data) %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(district, x, y)

# Manual nudges (same as create_map.R / create_map2.R)
nudge <- data.frame(
  district     = c("Cape",  "Stellenbosch", "Swellendam", "George",
                    "Uitenhage", "Albany", "Worcester", "Somerset"),
  dx           = c(-0.1,    0.0,            0.0,          0.0,
                    0.4,     0.0,           0.5,         -0.4),
  dy           = c( 0.15,   0.1,            0.05,         0.1,
                   -0.15,    0.0,           -0.3,         -0.5),
  stringsAsFactors = FALSE
)

label_pos <- centroids %>%
  left_join(nudge, by = "district") %>%
  mutate(x = x + ifelse(is.na(dx), 0, dx),
         y = y + ifelse(is.na(dy), 0, dy)) %>%
  select(district, x, y) %>%
  place_outside()

xlims <- map_extent(shp)$x
ylims <- map_extent(shp)$y

# --------------------------------------------------------------------------
# 4. Panel builder (diverging scale: dark = VT > non-VT, light = VT < non-VT)
# --------------------------------------------------------------------------

make_panel <- function(shp_data, fill_var, title,
                       label_data, fmt = "%.1f") {

  vals <- st_drop_geometry(shp_data) %>% select(district, value = !!sym(fill_var))
  rng  <- range(vals$value, na.rm = TRUE)
  abs_max <- max(abs(rng), na.rm = TRUE)

  lbl <- label_data %>%
    left_join(vals, by = "district") %>%
    mutate(
      lbl_text = ifelse(is.na(value), district,
                        paste0(district, "\n(",
                               ifelse(value > 0, "+", ""),
                               sprintf(fmt, value), ")")),
      text_col = ifelse(!is.na(value) &
                        value > (0.3 * abs_max),
                        "white", "black")
    )

  lbl$text_col[lbl$district %in% outside_labels$district] <- "black"

  ggplot(shp_data) +
    geom_sf(aes(fill = .data[[fill_var]]), colour = "grey30", linewidth = 0.3) +
    scale_fill_gradient2(low = LEAP_COLORS["blue"], mid = "#F5F0F3", high = LEAP_COLORS["plum"],
                         midpoint = 0, na.value = "white") +
    geom_segment(data = outside_labels,
                 aes(x = lx, y = ly, xend = lxend, yend = lyend),
                 colour = "grey30", linewidth = 0.25) +
    geom_text(data = lbl,
              aes(x = x, y = y, label = lbl_text, colour = text_col),
              size = 2.2, lineheight = 0.85, show.legend = FALSE) +
    scale_colour_identity() +
    labs(title = title) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 10, hjust = 0.5, margin = ggplot2::margin(b = 3)),
      legend.position = "none",
      plot.margin     = ggplot2::margin(2, 2, 2, 2)
    )
}

# --------------------------------------------------------------------------
# 5. Build the two panels
# --------------------------------------------------------------------------

p_a <- make_panel(map_data, "diff_loss",
                  "(a) VT - non-VT mean compensation loss (emancipation records)",
                  label_pos, fmt = "%.0f")

p_b <- make_panel(map_data, "diff_children",
                  "(b) VT - non-VT mean children per household (census)",
                  label_pos)

# --------------------------------------------------------------------------
# 6. Combine side-by-side and save
# --------------------------------------------------------------------------

p_combined <- p_a + p_b

ggsave("output/figures/Fig00c_conditional_differences.png", p_combined,
       width = 10, height = 3.9, dpi = 300, bg = "white")
ggsave("output/figures/Fig00c_conditional_differences.pdf", p_combined,
       width = 10, height = 3.9, bg = "white")

cat("\nFigure saved to Voortrekker/Figures/Fig00c_conditional_differences.png and .pdf\n")
# >>> MAPS END



# ==============================================================================
# SECTION E: RESULTS REGISTRY, MANIFEST AND VERIFICATION
# ==============================================================================

# ---------------------------------------------------------------------------
# E1. Build canonical results registry
# ---------------------------------------------------------------------------
# One long data frame from which all manuscript tables can be rebuilt.

cat("\n\n========== BUILDING RESULTS REGISTRY ==========\n")

# Read back all key output files
registry_parts <- list()

# Main district FE results
if (file.exists("output/tables/voortrekker_results_all_methods.csv")) {
  registry_parts$all_methods <- read.csv("output/tables/voortrekker_results_all_methods.csv",
                                          stringsAsFactors = FALSE) %>%
    mutate(result_set = "all_methods")
}

# Bonferroni-Holm adjusted results
if (file.exists("output/tables/regression_results_adjusted.csv")) {
  registry_parts$adjusted <- read.csv("output/tables/regression_results_adjusted.csv",
                                       stringsAsFactors = FALSE) %>%
    mutate(result_set = "bonferroni_holm")
}

# No spouse agreement results
if (file.exists("output/tables/robustness_no_spouse_agreement.csv")) {
  registry_parts$no_agree <- read.csv("output/tables/robustness_no_spouse_agreement.csv",
                                      stringsAsFactors = FALSE) %>%
    mutate(result_set = "no_spouse_agreement")
}

# High-confidence results
if (file.exists("output/tables/robustness_highconf.csv")) {
  registry_parts$highconf <- read.csv("output/tables/robustness_highconf.csv",
                                       stringsAsFactors = FALSE) %>%
    mutate(result_set = "high_confidence")
}

# Match-tier decomposition
if (file.exists("output/tables/match_tier_decomposition.csv")) {
  registry_parts$tiers <- read.csv("output/tables/match_tier_decomposition.csv",
                                    stringsAsFactors = FALSE) %>%
    mutate(result_set = "match_tiers")
}

# TOST equivalence
if (file.exists("output/tables/tost_sensitivity.csv")) {
  registry_parts$tost <- read.csv("output/tables/tost_sensitivity.csv",
                                   stringsAsFactors = FALSE) %>%
    mutate(result_set = "tost_equivalence")
}

# Age-cohort FE
if (file.exists("output/tables/age_cohort_fe_regressions.csv")) {
  registry_parts$cohorts <- read.csv("output/tables/age_cohort_fe_regressions.csv",
                                      stringsAsFactors = FALSE) %>%
    mutate(result_set = "age_cohorts")
}

# Emancipation probit
if (file.exists("output/tables/emancipation_expanded_probit_results.csv")) {
  registry_parts$emancipation <- read.csv("output/tables/emancipation_expanded_probit_results.csv",
                                           stringsAsFactors = FALSE) %>%
    mutate(result_set = "emancipation_probit")
}

if (file.exists("output/tables/emancipation_census_corroborated_results.csv")) {
  registry_parts$emancipation_census <- read.csv("output/tables/emancipation_census_corroborated_results.csv",
                                                  stringsAsFactors = FALSE) %>%
    mutate(result_set = "emancipation_census_subset")
}

if (file.exists("output/tables/emancipation_linkage_diagnostics.csv")) {
  registry_parts$emancipation_linkage <- read.csv("output/tables/emancipation_linkage_diagnostics.csv",
                                                   stringsAsFactors = FALSE) %>%
    mutate(result_set = "emancipation_linkage_diagnostics")
}

# Migration timing
if (file.exists("output/tables/migration_timing_regressions.csv")) {
  registry_parts$timing <- read.csv("output/tables/migration_timing_regressions.csv",
                                     stringsAsFactors = FALSE) %>%
    mutate(result_set = "migration_timing")
}

# Combine into registry (keep all columns, fill missing with NA)
results_registry <- bind_rows(registry_parts, .id = "source")
write.csv(results_registry, "output/tables/results_registry.csv", row.names = FALSE)
cat("  Exported results_registry.csv:", nrow(results_registry), "rows\n")

# ---------------------------------------------------------------------------
# E2. Output manifest
# ---------------------------------------------------------------------------
cat("\n========== WRITING OUTPUT MANIFEST ==========\n")

manifest <- data.frame(
  file = character(),
  type = character(),
  description = character(),
  paper_reference = character(),
  stringsAsFactors = FALSE
)

# Core outputs
manifest <- bind_rows(manifest, tribble(
  ~file, ~type, ~description, ~paper_reference,
  "voortrekker_matches.csv", "data", "549 retained VT-census record links", "Section 4",
  "voortrekker_emancipation_matches.csv", "data", "Raw VT-owner link rows from emancipation matching", "Section 5",
  "emancipation_owner_linkage_summary.csv", "data", "Owner-level collapse of the raw emancipation matches", "Section 5",
  "emancipation_linkage_diagnostics.csv", "diagnostics", "Link-row vs owner-level counts, including census-corroborated subset", "Section 5",
  "analysis_dataset.csv", "data", "Full census with is_voortrekker flag", "All sections",
  "voortrekker_results_all_methods.csv", "results", "All 6 comparison methods", "Tables 1-4, App Table 5",
  "regression_results_adjusted.csv", "results", "Bonferroni-Holm corrections", "Appendix C",
  "tost_sensitivity.csv", "results", "TOST equivalence at 4 SESOI bounds", "Appendix C",
  "robustness_highconf.csv", "results", "High-confidence match subset", "Appendix C",
  "robustness_no_spouse_agreement.csv", "results", "Regressions on links without spouse agreement", "Table 7",
  "match_tier_decomposition.csv", "results", "RF vs Manual match decomposition", "Appendix E",
  "age_cohort_fe_regressions.csv", "results", "Age-cohort FE regressions", "Appendix E",
  "emancipation_expanded_probit_results.csv", "results", "Probit/LPM emancipation models", "Table 5",
  "emancipation_census_corroborated_results.csv", "results", "Census-corroborated owner-sample emancipation models", "Appendix C",
  "migration_timing_regressions.csv", "results", "Migration timing regressions", "Table 6",
  "cv_diagnostics.csv", "diagnostics", "RF cross-validation metrics", "Appendix D",
  "matched_vs_unmatched.csv", "diagnostics", "Match quality comparison", "Appendix E",
  "matched_vs_unmatched_districts.csv", "diagnostics", "District distribution by match status", "Appendix E",
  "match_rates_by_district_method.csv", "diagnostics", "District match rates by method", "App Table 8",
  "khoe_variance_decomposition.csv", "diagnostics", "Khoekhoe between/within variance", "Appendix F",
  "khoe_regression_comparison.csv", "diagnostics", "Khoekhoe with/without FE", "Appendix F",
  "interrater_reliability.csv", "diagnostics", "Inter-rater agreement and kappa", "Appendix D",
  "settler_men_distribution.csv", "diagnostics", "Settler men distribution", "Section 4 footnote",
  "attenuation_simulation.csv", "diagnostics", "Coefficient attenuation under treated-group contamination", "Appendix D",
  "attenuation_significance.csv", "diagnostics", "Significance retention at 33.9% contamination", "Appendix D",
  "main_results_harmonised.csv", "results", "Harmonised variable set across the four designs", "Table 1, App tables",
  "wealth_index_diagnostics.csv", "diagnostics", "PC1 loadings, variance explained, index moments by VT status", "Section 3/4",
  "robustness_male_headed.csv", "results", "District FE with controls restricted to male-headed households", "Appendix (R1.3)",
  "robustness_male_headed_subsamples.csv", "results", "Male-headed controls within the RF-accepted tier and the links without spouse agreement", "Appendix (male-headed controls)",
  "timing_with_tenure.csv", "results", "Timing regressions with recency-of-arrival proxy", "Section 6 (R2.2)",
  "results_registry.csv", "meta", "Canonical results registry", "All tables"
))

manifest$exists <- file.exists(file.path(out_tables, manifest$file))
manifest$timestamp <- Sys.time()

write.csv(manifest, "output/tables/output_manifest.csv", row.names = FALSE)
cat("  Exported output_manifest.csv\n")

# ---------------------------------------------------------------------------
# E3. Final assertions
# ---------------------------------------------------------------------------
cat("\n========== FINAL ASSERTIONS ==========\n")

# Check critical files exist
critical_files <- c("voortrekker_matches.csv", "analysis_dataset.csv",
                     "voortrekker_results_all_methods.csv",
                     "results_registry.csv")
for (f in critical_files) {
  if (file.exists(file.path(out_tables, f))) {
    cat("  [OK]", f, "\n")
  } else {
    warning("  [MISSING] ", f)
  }
}

# Check match counts from the canonical source
if (exists("analysis_dataset_main")) {
  n_vt_final <- sum(analysis_dataset_main$is_voortrekker)
  n_total_final <- nrow(analysis_dataset_main)
  cat("\n  Canonical dataset: N =", n_total_final, ", N_VT =", n_vt_final, "\n")

  # Match tier counts
  if (file.exists("output/tables/voortrekker_matches.csv")) {
    vm <- read.csv("output/tables/voortrekker_matches.csv", stringsAsFactors = FALSE)
    n_rf <- sum(vm$match_quality != "Manual")
    n_manual <- sum(vm$match_quality == "Manual")
    n_links <- nrow(vm)
    n_unique_census <- dplyr::n_distinct(vm$census_id)
    cat("  RF matches:", n_rf, "\n")
    cat("  Manual matches:", n_manual, "\n")
    cat("  Accepted record links:", n_links, "\n")
    cat("  Unique matched census households:", n_unique_census, "\n")
    assert_count(n_unique_census, n_vt_final,
                 "Unique matched census households do not equal analysis dataset VT count")
  }
}

# ---------------------------------------------------------------------------
# E3b. Stage canonical data files
# Copy the three canonical data files to data/analysis/ and data/linked/
# after every run.
# ---------------------------------------------------------------------------
cat("\n========== STAGING CANONICAL DATA FILES ==========\n")

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("data/linked",   recursive = TRUE, showWarnings = FALSE)

staging_map <- c(
  "output/tables/analysis_dataset.csv"                = "data/analysis/analysis_dataset.csv",
  "output/tables/voortrekker_matches.csv"             = "data/linked/voortrekker_matches.csv",
  "output/tables/voortrekker_emancipation_matches.csv" = "data/linked/voortrekker_emancipation_matches.csv"
)
for (src in names(staging_map)) {
  dst <- staging_map[[src]]
  if (file.exists(src)) {
    ok <- file.copy(src, dst, overwrite = TRUE)
    cat("  ", ifelse(ok, "[STAGED]", "[FAILED]"), src, "->", dst, "\n")
  } else {
    warning("  [MISSING] ", src, " - not staged to ", dst)
  }
}

# ---------------------------------------------------------------------------
# E4. File listing
# ---------------------------------------------------------------------------
cat("\n========== COMPLETE PIPELINE FINISHED ==========\n\n")
cat("All outputs saved to:\n")
cat("  Figures: Voortrekker/Figures/\n")
cat("  Tables/Data: Output/\n\n")

cat("Output files:\n")
for (f in sort(list.files("output/tables/", pattern = "\\.(csv|rds)$"))) {
  cat("  [OK]", f, "\n")
}
cat("\nFigure files:\n")
n_png <- length(list.files("output/figures/", pattern = "\\.png$"))
n_pdf <- length(list.files("output/figures/", pattern = "\\.pdf$"))
cat("  PNG:", n_png, "files\n")
cat("  PDF:", n_pdf, "files\n")

cat("\n========== DONE ==========\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

write.csv(as.data.frame(harmon_ns), "output/tables/design_sample_counts.csv", row.names=FALSE)

# Audit exports: no new estimation; expose existing fitted results.
audit_names <- ls(envir=.GlobalEnv)
model_export <- list()
for (nm in audit_names) {
  obj <- get(nm, envir=.GlobalEnv)
  if (inherits(obj,"lm")) {
    cc <- tryCatch(as.data.frame(coef(summary(obj))), error=function(e) NULL)
    if (!is.null(cc) && ncol(cc)>=4) {
      z <- data.frame(model=nm, term=rownames(cc), coef=cc[[1]], se=cc[[2]], p=cc[[4]], n=nobs(obj))
      hc <- tryCatch(coeftest(obj, vcov=vcovHC(obj,type="HC1")),error=function(e) NULL)
      if (!is.null(hc)) {
        z$hc1_se <- hc[match(z$term,rownames(hc)),2]
        z$hc1_p <- hc[match(z$term,rownames(hc)),4]
      }
      model_export[[nm]] <- z
    }
  }
}
write.csv(bind_rows(model_export), "output/tables/existing_model_coefficients.csv", row.names=FALSE)
state_names <- intersect(c("best_matches","vt","vt_adults","all_districts","analysis_dataset_main",
  "slave_owners","slave_owners_analysis","owners_with_valuation","quartile_rates","comp_desc",
  "timing_analysis","year_stats","leader_stats","dest_stats","cv_export","var_importance",
  "birthplace_summary","tenure_data","timing_with_tenure","matched_vs_unmatched","desc_emancipation",
  "dest_ci","dest_anova_results","dest_means","mlogit_model","matched_with_trek","top_leader_stats","wt_comp_rate","wt_loss_pct","chisq_result","trend_cor","fisher_q4q1",
  "ame_loss_pct_3","ame_nslaves_3","ame_loss_pct_5"),audit_names)
saveRDS(mget(state_names,envir=.GlobalEnv), "output/tables/final_analysis_state.rds")
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
source("code/finalize_outputs.R")

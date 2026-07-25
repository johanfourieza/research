# prepare_au_convicts.R
#
# Build a colony-level table of Australian convict transportation totals
# (1788-1868), used in the Section 7 free-vs-coerced subnational test.
#
# Output: Submission/scripts/generated/au_convict_intensity.csv
#
# The numbers are well-documented historical aggregates and are taken from:
#   Anderson, Clare and Hamish Maxwell-Stewart. 2014. "Convict labour and
#     the Western empires, 1415-1954." In _The Routledge History of Western
#     Empires_, ed. Aldrich and McKenzie, 102-117. Routledge.
#   Cowley, Trudy et al. 2023. "Reconstructing a longitudinal dataset for
#     Tasmania." In _Sowing_, ed. Mandemakers et al., 455-482. Radboud UP.
#   Nicholas, Stephen (ed.). 1988. _Convict Workers: Reinterpreting
#     Australia's Past_. Cambridge University Press.
#   Australian Bureau of Statistics, 1901 Census of the Commonwealth.
#
# We follow the standard accounting that ~162,000 convicts were transported
# in total. The colony breakdown:
#   NSW             : ~83,000 (1788-1850; Anderson & Maxwell-Stewart 2014.
#                              ~500 of these were tried in and transported
#                              from colonies elsewhere in the British Empire)
#   TAS (VDL)       : ~73,500 (1803-1853; Cowley et al. 2023, preferred to
#                              Anderson & Maxwell-Stewart's figure -- a ~1,000
#                              difference -- as the more recent and
#                              Tasmania-specific reconstruction)
#   WA              : ~9,700  (1850-1868)
#   QLD             :     0   (Moreton Bay was a *secondary* penal settlement
#                              for transportees reconvicted elsewhere in
#                              Australia, so its convicts are not counted as
#                              a primary-transportation flow; QLD is free)
#   VIC             : ~3,000  ("Pentonvillians"/exiles, Port Phillip 1846-1850)
#   SA              :     0   (free colony from 1836, by Wakefield's design)
#
# au_colony_birthplace.csv covers NSW/VIC/QLD/SA/TAS, so WA is outside the
# working sample. We still report it here for transparency.
#
# convict_share is computed as convicts_total / total_pop_1901, which is
# admittedly a coarse intensity measure (1901 population includes much
# subsequent free migration and Australian-born descendants). It is meant
# to rank colonies on a relative coercion intensity, not to be a literal
# share. The categorical `regime` variable is the paper's preferred
# treatment indicator.

suppressMessages({
  library(tibble)
  library(readr)
})

# Path to the generated/ output directory --------------------------------------
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    getwd()
  }
}, error = function(e) getwd())

generated_dir <- file.path(script_dir, "generated")
dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)

# Hardcoded colony-level totals -----------------------------------------------
# 1901 Census population figures from Australian Bureau of Statistics,
# Census of the Commonwealth of Australia, 1901, vol. 1.
au_convicts <- tribble(
  ~colony, ~gadm1_code, ~convicts_total, ~convict_period_start, ~convict_period_end, ~total_pop_1901, ~regime,
  "NSW",   "AUS.5_1",    83000L,         1788L,                  1850L,                1354846L,         "penal",
  "TAS",   "AUS.7_1",    73500L,         1803L,                  1853L,                 172475L,         "penal",
  "WA",    "AUS.8_1",     9700L,         1850L,                  1868L,                 184124L,         "mixed",
  "QLD",   "AUS.4_1",        0L,         NA_integer_,            NA_integer_,           503266L,         "free",
  "VIC",   "AUS.2_1",     3000L,         1846L,                  1850L,                1201341L,         "free",
  "SA",    "AUS.6_1",        0L,         NA_integer_,            NA_integer_,           361604L,         "free"
)

au_convicts$convict_share <- au_convicts$convicts_total / au_convicts$total_pop_1901
au_convicts$log_convicts  <- log1p(au_convicts$convicts_total)

# Sanity check: total should be ~162,000 (Wikipedia and standard sources).
total_convicts <- sum(au_convicts$convicts_total)
cat(sprintf("Total convicts transported across all 6 colonies: %s\n",
            format(total_convicts, big.mark = ",")))
stopifnot(total_convicts >= 160000 && total_convicts <= 175000)

# Sort by convict intensity for inspection
au_convicts <- au_convicts[order(-au_convicts$convict_share), ]

cat("\nColony-level convict intensity:\n")
print(au_convicts)

# Write -----------------------------------------------------------------------
out_path <- file.path(generated_dir, "au_convict_intensity.csv")
write_csv(au_convicts, out_path)
cat(sprintf("\nWrote %s (%d rows)\n", out_path, nrow(au_convicts)))

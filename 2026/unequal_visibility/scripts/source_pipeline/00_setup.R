# =============================================================================
#  00_setup.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Common header for the source pipeline: packages, paths to the restricted
#  sources, the resource index and the name-matching helpers.
#
#  INPUTS   none (sourced by the other scripts)
#  OUTPUTS  none
#
#  This is the script that produced the released files. It needs restricted
#  inputs that are not redistributed (the full linked tax-roll panel, the SAF
#  genealogy and the source transcriptions), so it cannot run from this
#  package. The decision registers it reads are released in data/microdata/.
#  File paths refer to the author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

## =============================================================================
## 00_setup.R — paths, packages, helpers for the Fourie_Smallpox project
## =============================================================================
suppressMessages({
  library(data.table)
  library(stringdist)
  library(stringr)
})

## --- paths -------------------------------------------------------------------
## PROJ is this paper's project folder; raw data lives in the shared
## ../sources/data folder.
PROJ   <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
if (!file.exists(file.path(PROJ, "R/00_setup.R")))
  stop("Run R from the project root.")
SIB    <- "C:/Users/johanf/Dropbox/0Claude0/1Research/FourieMcCantsWalters_Probates"
DATA   <- normalizePath(file.path(PROJ, "..", "sources", "data"), mustWork = FALSE)
PANEL_GZ <- file.path(DATA, "stel-L-L/stellenbosch_long_linked_wsaf_fixed_feb2026_.csv.gz")
SAF_GZ   <- file.path(DATA, "stel-L-L/saf4spouselinkage_clean.csv.gz")
PROBATE_RDS <- file.path(PROJ, "R/explore/mooc8_inventories.rds")
PROBATE_UPDATE <- file.path(PROJ, 'revision/probate_update_2026-09-28')
MOOC_XML_DIR <- file.path(PROBATE_UPDATE, 'inputs/data/sources/probate/MOOC8')
if (!dir.exists(MOOC_XML_DIR)) stop('Missing frozen current probate sources; run python R/prepare_probate_update.py.')
STELLENBOSCH_REGISTER <- file.path(PROBATE_UPDATE, 'stellenbosch_inventory_register.csv')
OUT <- file.path(PROJ, "output"); dir.create(OUT, showWarnings = FALSE)

## --- helpers (name standardisation; vendored locally so the package is
## self-contained — originally from the sibling probates project) --------------
source(file.path(PROJ, "R/helpers/name_standardize.R"))  # standardize_name(s)
source(file.path(PROJ, "R/helpers/death_record_checks.R"))

## numeric coercion: strip stray chars, NA -> 0
z <- function(x){ x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

## standardise a raw "Surname, First" or "First Surname" name -> list of parts
std1 <- function(nm) standardize_name(nm)

## wealth index (this period: slaves + livestock; tax fields empty pre-1750s)
wealth_index <- function(dt){
  z(dt$slave_men) + z(dt$slave_women) +
    0.5*(z(dt$cattle_cows) + z(dt$cattle_work)) +
    0.5*z(dt$horses) + 0.1*z(dt$sheep)
}
adult_slaves <- function(dt) z(dt$slave_men) + z(dt$slave_women)

## --- matching helpers --------------------------------------------------------
## last token of a (possibly multi-word) surname: "van der merwe" -> "merwe".
## The discriminating part of Cape Dutch surnames is the final token; matching on
## it prevents first-name + confusable-surname false matches that defeat full-string
## Jaro-Winkler (e.g. Cloete/Coetzee, van der Merwe/van der Schelde, de Bruijn/de Buys).
surname_token <- function(s) sub("^.*[[:space:]]", "", as.character(s))

## TRUE if two records are the same person: surname last-token close AND full
## name-key close. Vectorised over the second argument (candidate set).
is_match <- function(sur1, key1, sur2, key2, tok = 0.10, key_thr = 0.12){
  stringdist(surname_token(sur1), surname_token(sur2), method="jw", p=0.1) <= tok &
  stringdist(key1, key2, method="jw", p=0.1) <= key_thr
}

# =============================================================================
#  01_probate_documents.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Parses the MOOC8 Orphan Chamber XML volumes and counts dated estate
#  documents per year (Figure 1a).
#
#  INPUTS   MOOC8 XML transcriptions (TEPC/TANAP)
#  OUTPUTS  mooc8_inventories.rds, annual counts
#
#  This is the script that produced the released aggregates. It needs the
#  restricted individual-level sources and decision registers, which are not
#  redistributed, so it cannot run from this package. File paths refer to the
#  author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

## Clean mortality signal: probate (MOOC8) inventory counts by year.
## Independent of opgaafrolle enumeration completeness.
suppressMessages({library(xml2); library(data.table)})
source("R/helpers/xml_helpers.R")

xml_dir <- "../sources/data/XML files"
files <- list.files(xml_dir, pattern="MOOC8.*\\.xml$", full.names=TRUE)
cat("MOOC8 files:", length(files), "\n")
if (!length(files)) stop("No MOOC8 XML source files found: ", xml_dir)

inv <- rbindlist(lapply(files, function(f){
  out <- parse_mooc8_file(f, inventories_only=TRUE)
  out$inventories
}), fill=TRUE)

cat("total inventory records parsed:", nrow(inv), "\n")
inv[, year := parse_mooc_years(date_value)]
cat("records with usable year:", sum(!is.na(inv$year)),
    " (", round(100*mean(!is.na(inv$year)),1), "%)\n", sep="")

## counts per year over the relevant window
tab <- inv[!is.na(year) & year>=1695 & year<=1770, .N, by=year][order(year)]
saveRDS(inv, "R/explore/mooc8_inventories.rds")
fwrite(tab, "R/explore/mooc8_counts_by_year.csv")

cat("\n=== inventory counts by year, 1695-1770 (★ = epidemic year) ===\n")
for(i in seq_len(nrow(tab))){
  y <- tab$year[i]; n <- tab$N[i]
  star <- if(y %in% c(1713,1755)) " <-- EPIDEMIC" else ""
  bar <- paste(rep("#", n), collapse="")
  cat(sprintf("%4d %4d %s%s\n", y, n, bar, star))
}

## local spike check: ratio of epidemic-year count to mean of +/-3 surrounding (excl. itself & 1715 gap)
spike <- function(y){
  near <- tab[year %in% c((y-3):(y-1),(y+1):(y+3))]
  c(epi=tab[year==y, N], nbr_mean=round(mean(near$N),1),
    ratio=round(tab[year==y,N]/mean(near$N),2))
}
cat("\n=== local spike ratios ===\n")
cat("1713:", paste(names(spike(1713)),spike(1713),collapse="  "), "\n")
cat("1755:", paste(names(spike(1755)),spike(1755),collapse="  "), "\n")

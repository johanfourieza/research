# Shared revision helpers. Original files are read-only; all writes stay in revision.
suppressPackageStartupMessages(library(data.table))
set.seed(2412)
arg_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
revision_dir <- normalizePath(file.path(dirname(arg_file), ".."), winslash = "/")
project_dir <- dirname(revision_dir)
book_data <- normalizePath(Sys.getenv("ALLROUNDERS_AUDIT_ARCHIVE",file.path(project_dir, "../../../3OurLongWalk/Books/MythsOfCricket/data")), winslash = "/",mustWork=FALSE)
for (folder in c("data/staging", "data/corrected", "audit/execution", "sources", "config"))
  dir.create(file.path(revision_dir, folder), recursive = TRUE, showWarnings = FALSE)
configs <- list(tests=c("international/test", "test"), odis=c("international/oneday", "oneday"),
  t20is=c("international/t20", "t20"), county=c("county", "countycricket", "countycricket_older", "countycricket_older1"),
  sheffield=c("sheffield", "sheffieldshield"))
read_csv <- function(path) fread(path, sep=",", na.strings=c("", "NA"), showProgress=FALSE)
write_audit <- function(x, name) fwrite(x, file.path(revision_dir, "audit/execution", paste0(name, ".csv")))
read_comp <- function(comp) {
  cfg <- configs[[comp]]
  setNames(lapply(c("teams", "players", "batting", "bowling", "batting_noshow"), function(kind) {
    rbindlist(lapply(cfg[-1], function(prefix) {
      relative <- file.path("data/raw/daft", cfg[1], paste0(prefix, "_", kind, ".csv"))
      z <- read_csv(file.path(project_dir, relative))
      z[, `:=`(source_file=relative, source_row=seq_len(.N), vintage=prefix)]
      z
    }), use.names=TRUE, fill=TRUE)
  }), c("teams", "players", "batting", "bowling", "batting_noshow"))
}
assert_unique <- function(x, cols, label) {
  if (anyDuplicated(x, by=cols)) stop("Nonunique ", label, ": ", paste(cols, collapse=", "))
  invisible(TRUE)
}
mode_position <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  counts <- table(x); candidates <- as.integer(names(counts)[counts==max(counts)])
  candidates[order(abs(candidates-median(x)), candidates)][1]
}
require_data_gate<-function(scope="all substantive analyses") {
 gate<-jsonlite::read_json(file.path(revision_dir,"config/data_gate.json"),simplifyVector=TRUE)
 if(!identical(gate$status,"OPEN") || !isTRUE(gate$estimation_authorised))
  stop("Data gate CLOSED for ",scope,". Resolve the signed issue register before substantive estimation.")
 if(!scope %in% gate$scope)
  stop("Data gate does not authorise this scope: ",scope)
 invisible(TRUE)
}

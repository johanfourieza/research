args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)>=1L,grepl('^[A-Za-z0-9_]+$',args[1]))
script <- args[1]; script_args <- args[-1]
here::i_am('replication/run_all.R')
options(encoding='UTF-8')
if(.Platform$OS.type=='windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
suppressPackageStartupMessages(library(tidyverse))
# Preserve the paper's CSV representation except that missing values are blank.
write_csv <- function(x,file,...,na='') readr::write_csv(x,file,...,na=na)
commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) script_args else base::commandArgs()
source(file.path('replication','R',paste0(script,'.R')),local=.GlobalEnv,encoding='UTF-8')

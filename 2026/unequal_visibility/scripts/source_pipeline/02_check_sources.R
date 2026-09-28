# =============================================================================
#  02_check_sources.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Reconciles every retained 1712 row to the original workbook (row number,
#  names, assets) and checks that each asset column has positive entries, which
#  justifies the zero-coding rule for blank cells.
#
#  INPUTS   1712 opgaaf workbook; linked panel; baseline decision register
#  OUTPUTS  data/asset_column_coverage_1712.csv; source checks
#
#  This is the script that produced the released aggregates. It needs the
#  restricted individual-level sources and decision registers, which are not
#  redistributed, so it cannot run from this package. File paths refer to the
#  author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

# Reconcile the fixed decision registers to the original supplied sources.
source('R/00_setup.R')
suppressPackageStartupMessages(library(readxl))
B<-fread('R/decisions/baseline_rows.csv',encoding='UTF-8')
wb<-file.path(DATA,'Stellenbosch-Drakenstein Earlier Opgaafrolle - Hague & Cape Archives - including Full Indexes June 2022.xlsx')
raw<-as.data.table(read_excel(wb,sheet='1712',col_names=FALSE,col_types='text',.name_repair='unique_quiet'))
P<-fread(PANEL_GZ,select=c('year','hhobs','names_men','names_women','widow',
 'slave_men','slave_women','cattle_cows','cattle_work','horses','sheep'),
 encoding='Latin-1',showProgress=FALSE)
stopifnot(!anyDuplicated(B$raw_excel_row),nrow(B)==354L,
 setequal(B$hhobs,P[year==1712&!is.na(names_men)&nzchar(names_men),hhobs]))
txt<-function(x){x[is.na(x)]<-'';trimws(x)}
stopifnot(identical(txt(raw[[3]][B$raw_excel_row]),txt(B$names_men)),
 identical(txt(raw[[4]][B$raw_excel_row]),txt(B$names_women)))
Q<-P[match(B$hhobs,hhobs)]
stopifnot(identical(txt(Q$names_men),txt(B$names_men)))
asset_map<-c(slave_men=10,slave_women=11,cattle_work=15,cattle_cows=16,horses=14,sheep=20)
coverage<-rbindlist(lapply(names(asset_map),function(a){
 v<-raw[[asset_map[[a]]]][B$raw_excel_row]
 v_num<-suppressWarnings(as.numeric(v))
 stopifnot(all(is.na(v)|!nzchar(trimws(v))|!is.na(v_num)),identical(z(v_num),z(Q[[a]])))
 x<-data.table(district=B$district,value=v_num)
 x[,.(variable=a,positive_entries=sum(value>0,na.rm=TRUE),nonmissing=sum(!is.na(value))),by=district]
}))
stopifnot(all(coverage$positive_entries>0))
fwrite(coverage,'revision/output/current_raw_asset_coverage.csv')
docs<-fread('R/decisions/document_screening.csv',colClasses=c(date_value='character'))
wid<-fread('R/decisions/widow_screening.csv')
inv<-readRDS(PROBATE_RDS)
inv<-inv[substr(date_value,1,4)%in%c('1713','1714')]
dkey<-function(d)paste(d$source_file,d$div_id,d$date_value,sep='|')
stopifnot(nrow(docs)==90L,all(docs$screened),!anyDuplicated(dkey(docs)),
 setequal(dkey(docs),dkey(inv)),nrow(wid)==33L,all(wid$screened),
 !anyDuplicated(wid$hhobs),setequal(wid$hhobs,P[year%in%1713:1714&widow==1,hhobs]))
inputs<-c(PANEL_GZ,SAF_GZ,wb,file.path(DATA,'vc_datastel (v3).xlsx'),
 list.files(file.path(DATA,'XML files'),pattern='\\.xml$',full.names=TRUE),
 list.files('R/decisions',pattern='\\.csv$',full.names=TRUE))
stopifnot(all(file.exists(inputs)))
hashes<-data.table(path=normalizePath(inputs,winslash='/'),md5=unname(tools::md5sum(inputs)))
fwrite(hashes,'revision/output/current_input_hashes.csv')
writeLines(c('Every baseline row reconciles to original workbook number and names.',
 'All six asset columns reconcile to the linked panel and are recorded in each district.',
 'All 90 probate documents and all 33 widow entries have screening dispositions.',
 'Input and decision-register MD5 hashes saved for this run.'),
 'revision/output/current_source_checks.txt')
cat('Source reconciliation and screening-coverage checks passed.\n')

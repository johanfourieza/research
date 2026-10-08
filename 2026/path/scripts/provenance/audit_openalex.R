# =============================================================================
# PROVENANCE SCRIPT -- NOT RUN BY run_all.R
# Copied unchanged from the replication audit of 23 September 2026 that produced
# the reviewed conference records, the candidate links and the OpenAlex metadata
# screen shipped in data/. Paths are relative to the audit tree (project root
# with Cliometrica/audit_2026-09-23/ and the raw programme HTML/PDF files in
# WorkingPaper/Conference_Programs/, which are not redistributed). Included so
# that every step from raw programme to shipped record is readable; the shipped
# CSV files are the inputs that scripts 05, 06 and 10 use.
# =============================================================================

library(data.table); library(httr); library(jsonlite); library(stringi); library(stringdist)
setDTthreads(1)
A<-'Cliometrica/audit_2026-09-23';P<-file.path(A,'github_snapshot/2026/path')
oa<-as.data.table(readRDS(file.path(P,'data/cache/openalex_paper_matches.rds')));oa[,id:=as.integer(id)]
jn<-fread(file.path(P,'data/raw/Journals_2026_clean.csv'),encoding='UTF-8')
oa<-oa[id %in% jn$ID]
ids<-unique(sub('https://openalex.org/','',oa$openalex_id,fixed=TRUE))
dir.create(file.path(A,'sources/openalex'),showWarnings=FALSE)
out<-list()
for(k in seq_along(split(ids,ceiling(seq_along(ids)/100)))) {
 chunk<-split(ids,ceiling(seq_along(ids)/100))[[k]]
 f<-file.path(A,'sources/openalex',sprintf('batch_%02d.json',k))
 if(!file.exists(f)) {
  resp<-GET('https://api.openalex.org/works',query=list(filter=paste0('openalex_id:',paste(chunk,collapse='|')),per_page=100,select='id,title,publication_year,doi,authorships,primary_location'),timeout(45))
  stop_for_status(resp); writeBin(content(resp,'raw'),f)
 }
 z<-fromJSON(f,simplifyVector=FALSE)
 out[[k]]<-rbindlist(lapply(z$results,function(w)data.table(openalex_id=w$id,verified_title=w$title,verified_year=w$publication_year,doi=if(is.null(w$doi))NA_character_ else w$doi,verified_authors=paste(vapply(w$authorships,function(a)a$author$display_name,''),collapse='; '),verified_journal=if(is.null(w$primary_location$source))NA_character_ else w$primary_location$source$display_name)),fill=TRUE)
 cat('Batch',k,'rows',nrow(out[[k]]),'\n');flush.console()
}
v<-merge(oa,rbindlist(out),by='openalex_id',all.x=TRUE)
v<-merge(v,jn[,.(id=ID,year=Year,journal=Journal,title=Title,author1=Author1)],by='id')
norm<-function(x)trimws(gsub(' +',' ',gsub('[^a-z0-9]+',' ',tolower(stri_trans_general(x,'Latin-ASCII')))))
v[,title_distance:=stringdist(norm(title),norm(verified_title),method='jw',nthread=1)]
v[,flag:=is.na(verified_title)|is.na(verified_year)|is.na(title_distance)|title_distance>.15|abs(year-verified_year)>1]
fwrite(v,file.path(A,'exports/openalex_verified_metadata.csv'))
fwrite(v[flag==TRUE],file.path(A,'exports/openalex_metadata_flags.csv'))
cat('Verified',sum(!is.na(v$verified_title)),'of',nrow(v),'flagged',sum(v$flag),'\n')

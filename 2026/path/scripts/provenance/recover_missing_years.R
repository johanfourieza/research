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

library(data.table); library(xml2); library(rvest); library(stringi)
A<-'Cliometrica/audit_2026-09-23'
sp<-function(x) trimws(gsub('[[:space:]\u00a0]+',' ',x,perl=TRUE))
out<-list()
f<-file.path(A,'sources/EHS_2022.html'); doc<-read_html(f,encoding='UTF-8')
ps<-html_elements(doc,'p')
for(i in seq_along(ps)) {
 p<-ps[[i]]; its<-html_elements(p,'em,i'); if(!length(its)) next
 title<-sp(paste(html_text(its),collapse=' ')); raw<-sp(html_text(p))
 if(nchar(title)<15||grepl('^\\(|^chair:',title,ignore.case=TRUE)||grepl('^[A-Z]+[IVX0-9]*[A-Z]?:.*chair:',raw,ignore.case=TRUE)) next
 cp<-read_html(as.character(p)); xml_remove(xml_find_all(cp,'.//em|.//i'))
 au<-sp(html_text(html_element(cp,'p')))
 if(nchar(au)<3&&i<length(ps)) {
  nxt<-sp(html_text(ps[[i+1]]))
  if(grepl('\\([^)]+\\)',nxt)&&!length(html_elements(ps[[i+1]],'em,i'))) au<-nxt
 }
 au<-sp(gsub('\\([^)]*\\)','',au))
 out[[length(out)+1]]<-data.table(conference='EHS',year=2022L,title=title,authors=au,raw_text=raw,source_file='EHS_2022.html',source_paragraph=i,source_url='https://ehs.org.uk/conference/2022-provisional-programme/')
}
# 2021 PDF is laid out in one text column. Blank lines/session headings reset
# a title buffer; affiliation parentheses delimit the subsequent author block.
ls<-readLines(file.path(A,'sources/EHS_2021.txt'),encoding='UTF-8',warn=FALSE)
buf<-character(); lastrow<-NA_integer_
for(i in seq_along(ls)) {
 x<-sp(gsub('\f','',ls[i]))
 if(!nzchar(x)||grepl('^[0-9]+$',x)) next
 if(grepl('^(TUESDAY|WEDNESDAY|THURSDAY|FRIDAY|SATURDAY|SUNDAY)|^[0-9]{4}[^0-9][0-9]{4}|^(NR|AS)[IVX]+[A-Z]?:|^\\(chair:|^SESSION|^CONFERENCE|^Economic History Society',x,ignore.case=FALSE)) {buf<-character();lastrow<-NA_integer_;next}
 if(grepl('\\([^)]+\\)',x) && !grepl('^(chair:|[0-9])',x,ignore.case=TRUE)) {
  # Author lines start with a name and have an affiliation, sometimes split.
  if(length(buf)) {
   title<-sp(paste(buf,collapse=' ')); au<-sp(gsub('\\([^)]*\\)','',x))
   if(nchar(title)>=15) {
    out[[length(out)+1]]<-data.table(conference='EHS',year=2021L,title=title,authors=au,raw_text=paste(title,x),source_file='EHS_2021.pdf',source_paragraph=i,source_url='https://files.ehs.org.uk/wp-content/uploads/2021/04/29060655/Conference-programme-2021.pdf')
    lastrow<-length(out)
   }
   buf<-character()
  } else if(!is.na(lastrow) && grepl('^[&A-Z]',x)) {
   out[[lastrow]]$authors<-paste(out[[lastrow]]$authors,sp(gsub('\\([^)]*\\)','',x)))
  }
 } else {buf<-c(buf,x);lastrow<-NA_integer_}
}
d<-unique(rbindlist(out,fill=TRUE),by=c('year','title'))
fwrite(d,file.path(A,'exports/recovered_2021_2022.csv'))
print(d[, .N,by=year])

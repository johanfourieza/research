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

# Independent audit; never writes to the accepted replication package.
library(data.table)
library(stringdist)
library(lfe)
library(xml2)
library(rvest)
library(stringi)
setDTthreads(1)
AUDIT <- 'Cliometrica/audit_2026-09-23'
PKG <- file.path(AUDIT, 'github_snapshot/2026/path')
ad <- readRDS(file.path(PKG, 'results/analysis_data.rds'))
jn <- copy(ad$jn); est <- copy(ad$est)
conf <- as.data.table(readRDS(file.path(PKG, 'data/cache/conference_parsed_data.rds')))
conf[, row_id := .I]
norm <- function(x) {
 x <- stri_trans_general(x, 'Latin-ASCII')
 x <- tolower(x)
 trimws(gsub(' +', ' ', gsub('[^a-z0-9]+', ' ', x)))
}
clean_space <- function(x) trimws(gsub('[[:space:]\u00a0]+', ' ', x, perl=TRUE))
old_norm <- function(x) gsub('\\s+', ' ', trimws(tolower(gsub('[^a-z0-9 ]','',x))))
old_surnames <- function(x) {
 if(is.na(x) || nchar(trimws(x))<2) return(character())
 p <- trimws(unlist(strsplit(x, '[,;/&]|\\band\\b')))
 unique(unlist(lapply(p[nchar(p)>1], function(z) {
  w <- strsplit(trimws(z),'\\s+')[[1]]; w <- w[nchar(w)>1]
  if(length(w)) tolower(tail(w,1)) else character()
 })))
}
new_surnames <- function(x) {
 x <- gsub('\\([^)]*\\)', '', x)
 old_surnames(norm(gsub('[,;/&]|\\band\\b', ';', x)))
}
# Keep author boundaries before normalising punctuation.
new_surnames <- function(x) {
 if(is.na(x)) return(character())
 p <- unlist(strsplit(gsub('\\([^)]*\\)', '', x), '[,;/&]|\\band\\b'))
 unique(unlist(lapply(p, function(z) {
  w <- strsplit(norm(z),' +')[[1]]; w <- w[nchar(w)>1]
  if(length(w)) tail(w,1) else character()
 })))
}
author_sets <- function(d, fun) lapply(seq_len(nrow(d)), function(i)
 unique(unlist(lapply(unlist(d[i, paste0('author',1:5),with=FALSE]),fun))))
old_js <- author_sets(jn,old_surnames); new_js <- author_sets(jn,new_surnames)

# Independent EHS extraction from archived official HTML, removing title DOM
# nodes before obtaining authors (avoids failed literal substitutions).
rows <- list()
for(f in list.files('WorkingPaper/Conference_Programs', '^EHS_.*html$',full.names=TRUE)) {
 yr <- as.integer(sub('EHS_([0-9]+).html','\\1',basename(f)))
 doc <- read_html(f,encoding='UTF-8'); ps <- html_elements(doc,'p')
 for(i in seq_along(ps)) {
  p <- ps[[i]]; its <- xml_find_all(p,'.//em|.//i')
  if(!length(its)) next
  title <- clean_space(paste(html_text(its),collapse=' '))
  if(nchar(title)<15 || grepl('^\\(|^chair:|^[0-9]{4}[-:]',title,ignore.case=TRUE)) next
  raw <- clean_space(html_text(p))
  # Drop session headers and administrative blocks, not paper titles containing Session.
  if(grepl('^[A-Z]+[IVX0-9]*[A-Z]?:.*chair:',raw,ignore.case=TRUE)) next
  pcopy <- read_html(paste0('<html><body>',as.character(p),'</body></html>'))
  xml_remove(xml_find_all(pcopy,'.//em|.//i'))
  au <- clean_space(html_text(html_element(pcopy,'p')))
  if(nchar(au)<3 && i<length(ps)) {
   nxt <- clean_space(html_text(ps[[i+1]]))
   if(grepl('\\([^)]+\\)',nxt) && !length(html_elements(ps[[i+1]],'em,i'))) au <- nxt
  }
  au <- clean_space(gsub('\\([^)]*\\)','',au))
  au <- sub('^[,;: ]+','',au)
  rows[[length(rows)+1]] <- data.table(conference='EHS',year=yr,title=title,authors=au,
    raw_text=raw,source_file=basename(f),source_paragraph=i,
    source_url=sprintf('https://ehs.org.uk/society/resources/ehs-annual-conference-archive/%d-ehs-annual-conference/',yr))
 }
}
ehs <- unique(rbindlist(rows,fill=TRUE),by=c('year','title'))
rebuilt <- rbindlist(list(conf[conference=='EHA'],ehs),fill=TRUE)
rebuilt[, row_id:=.I]
fwrite(ehs,file.path(AUDIT,'exports/ehs_independent_parse.csv'))

match_rows <- function(cd,normalizer,sfun,js,author_required=FALSE,maxdist=.25,pre_only=FALSE) {
 jt <- normalizer(jn$title)
 out <- copy(cd)
 out[, `:=`(matched_id=NA_integer_,dist=NA_real_,tier=NA_character_,overlap=FALSE)]
 for(i in seq_len(nrow(out))) {
  ct <- normalizer(out$title[i]); cy <- out$year[i]
  if(is.na(ct)||nchar(ct)<15||grepl('[0-9]:[0-9]{2}',out$title[i])) next
  cand <- which(jn$year >= cy - (if(pre_only) 0 else 1) & jn$year <= cy+5)
  if(!length(cand)) next
  d <- stringdist(ct,jt[cand],method='jw',nthread=1); k <- which.min(d)
  ns <- sfun(out$authors[i]); ov <- vapply(js[cand],function(z) any(ns %in% z),logical(1))
  chosen_tier <- NA_character_; j <- NA_integer_
  if(!author_required && d[k]<.10) {j<-k;chosen_tier<-'A'}
  else if(length(ns)) {
   b <- which(ov & d<maxdist)
   if(length(b)) {j<-b[which.min(d[b])];chosen_tier<-'B'}
  } else if(!author_required && d[k]<.15) {j<-k;chosen_tier<-'C'}
  if(!is.na(j)) out[i, `:=`(matched_id=jn$id[cand[j]], dist=d[j],tier=chosen_tier,overlap=ov[j])]
 }
 out[, `:=`(article_title=jn$title[match(matched_id,jn$id)],
             article_authors=vapply(match(matched_id,jn$id),function(i) if(is.na(i)) NA_character_ else paste(na.omit(unlist(jn[i,paste0('author',1:5),with=FALSE])),collapse='; '),''),
             publication_year=jn$year[match(matched_id,jn$id)],
             in_estimation=matched_id %in% est$id)]
 out
}
scenarios <- list(
 published=list(conf,old_norm,old_surnames,lapply(old_js,list),FALSE,.25,FALSE),
 surname_list_fix_only=list(conf,old_norm,old_surnames,old_js,FALSE,.25,FALSE),
 case_fix_only=list(conf,function(x) gsub('\\s+',' ',trimws(gsub('[^a-z0-9 ]','',tolower(x)))),old_surnames,lapply(old_js,list),FALSE,.25,FALSE),
 case_and_list_fix=list(conf,function(x) gsub('\\s+',' ',trimws(gsub('[^a-z0-9 ]','',tolower(x)))),old_surnames,old_js,FALSE,.25,FALSE),
 unicode_fix=list(conf,norm,new_surnames,new_js,FALSE,.25,FALSE),
 reparse_same_rules=list(rebuilt,norm,new_surnames,new_js,FALSE,.25,FALSE),
 author_required_025=list(rebuilt,norm,new_surnames,new_js,TRUE,.25,FALSE),
 author_required_015=list(rebuilt,norm,new_surnames,new_js,TRUE,.15,FALSE),
 author_required_010=list(rebuilt,norm,new_surnames,new_js,TRUE,.10,FALSE),
 author_required_prepublication=list(rebuilt,norm,new_surnames,new_js,TRUE,.15,TRUE)
)
matches <- list(); results <- list()
for(s in names(scenarios)) {
 cat('SCENARIO',s,'\n')
 m <- do.call(match_rows,scenarios[[s]]); matches[[s]]<-m
 fwrite(m[!is.na(matched_id)],file.path(AUDIT,'exports',paste0('matches_',s,'.csv')))
 d <- copy(est); d[,presented:=as.integer(id %in% m$matched_id)]
 fit <- felm(log_longrun ~ presented + log_early + n_authors + any_top_inst + log_article_length + title_nchar + article_position + issue_no | journal + year,data=d)
 ct <- summary(fit,robust=TRUE)$coefficients
 results[[s]]<-data.table(scenario=s,entries=sum(!is.na(m$matched_id)),papers=uniqueN(na.omit(m$matched_id)),presenters=sum(d$presented),coef=ct['presented',1],se=ct['presented',2],p=ct['presented',4],lower=ct['presented',1]-1.96*ct['presented',2],upper=ct['presented',1]+1.96*ct['presented',2])
 print(results[[s]])
}
saveRDS(matches,file.path(AUDIT,'audit_matches.rds'))
fwrite(rbindlist(results),file.path(AUDIT,'exports/conference_sensitivity.csv'))
print(conf[,.(entries=.N,authors_equal_title=sum(norm(title)==norm(authors),na.rm=TRUE),room_titles=sum(grepl('^\\(',title))),by=.(conference,year)])

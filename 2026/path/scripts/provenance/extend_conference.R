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

# Further audit scenarios, retaining the accepted programme-matching window.
src<-readLines('Cliometrica/audit_2026-09-23/audit_conference.R',warn=FALSE)
cut<-grep('^scenarios <-',src)[1]-1L
eval(parse(text=src[seq_len(cut)]))
extra<-fread(file.path(AUDIT,'exports/recovered_2021_2022.csv'),encoding='UTF-8')
expanded<-rbindlist(list(rebuilt,extra),fill=TRUE); expanded[,row_id:=.I]
extra_scenarios<-list(
 recovered_years_025=list(expanded,norm,new_surnames,new_js,TRUE,.25,FALSE),
 recovered_years_015=list(expanded,norm,new_surnames,new_js,TRUE,.15,FALSE),
 recovered_years_010=list(expanded,norm,new_surnames,new_js,TRUE,.10,FALSE),
 recovered_years_prepublication=list(expanded,norm,new_surnames,new_js,TRUE,.15,TRUE)
)
mm<-list();rr<-list()
for(s in names(extra_scenarios)) {
 m<-do.call(match_rows,extra_scenarios[[s]]);mm[[s]]<-m
 fwrite(m[!is.na(matched_id)],file.path(AUDIT,'exports',paste0('matches_',s,'.csv')))
 d<-copy(est); d[,presented:=as.integer(id %in% m$matched_id)]
 fit<-felm(log_longrun ~ presented+log_early+n_authors+any_top_inst+log_article_length+title_nchar+article_position+issue_no | journal+year,data=d)
 ct<-summary(fit,robust=TRUE)$coefficients
 rr[[s]]<-data.table(scenario=s,entries=sum(!is.na(m$matched_id)),papers=uniqueN(na.omit(m$matched_id)),presenters=sum(d$presented),coef=ct['presented',1],se=ct['presented',2],p=ct['presented',4])
 print(rr[[s]])
}
saveRDS(mm,file.path(AUDIT,'extended_matches.rds'))
fwrite(rbindlist(rr),file.path(AUDIT,'exports/extended_conference_sensitivity.csv'))
# Re-estimate the published within-author specifications with each alternative.
allm<-c(readRDS(file.path(AUDIT,'audit_matches.rds')),mm)
basepanel<-melt(jn,id.vars=c('id','year','author1','n_authors'),measure.vars=paste0('g',14:26),variable.name='snap',value.name='cit_cum')
basepanel[,cite_year:=2000+as.integer(sub('g','',snap))]
basepanel[,paper_age:=cite_year-year];basepanel<-basepanel[paper_age>=0]
setorder(basepanel,id,cite_year)
basepanel[,`:=`(cit_new=cit_cum-shift(cit_cum),lag=shift(cit_cum)),by=id]
basepanel[paper_age==0,cit_new:=cit_cum]
basepanel<-basepanel[cit_new>=0 & !is.na(author1) & author1!='']
basepanel[,`:=`(log_new=log1p(cit_new),log_lag=log1p(lag),author_year=paste0(author1,'_',year))]
pp<-lapply(names(allm),function(s){
 d<-copy(basepanel);d[,presented:=as.integer(id %in% allm[[s]]$matched_id)]
 a<-felm(log_new ~ presented+log_lag+n_authors | author1+cite_year+paper_age | 0 | id,data=d)
 b<-felm(log_new ~ presented+log_lag | author_year+cite_year+paper_age | 0 | id,data=d)
 ta<-summary(a)$coefficients;tb<-summary(b)$coefficients
 data.table(scenario=s,wa1=ta['presented',1],wa1_se=ta['presented',2],wa1_p=ta['presented',4],wa2=tb['presented',1],wa2_se=tb['presented',2],wa2_p=tb['presented',4],author_years_vary=d[,.(v=uniqueN(presented)>1),by=author_year][,sum(v)])
})
fwrite(rbindlist(pp),file.path(AUDIT,'exports/panel_sensitivity.csv'))
print(rbindlist(pp))

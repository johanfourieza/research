# =============================================================================
#  06_continuation_panel.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Record-continuation diagnostic (Online Appendix D). Not a mortality
#  estimator: no probate death flags enter it.
#
#  INPUTS   linked opgaaf panel 1705-1725
#  OUTPUTS  data/continuation_cells.csv; data/continuation_estimates.csv;
#  data/continuation_windows.csv
#
#  This is the script that produced the released aggregates. It needs the
#  restricted individual-level sources and decision registers, which are not
#  redistributed, so it cannot run from this package. File paths refer to the
#  author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

# Record-continuation diagnostic. This is not a mortality estimator or a
# historically adjudicated survival panel. No probate death flags enter it.
source('R/00_setup.R')
source('R/helpers/current_names.R')
check_current_names()
AD <- file.path(PROJ,'revision/output')
dir.create(AD,recursive=TRUE,showWarnings=FALSE)
put <- function(x,s) fwrite(x,file.path(AD,paste0('current_panel_',s,'.csv')))
P <- fread(PANEL_GZ,select=c('hhobs','year','names_men','names_women','widow','widow_of',
  'slave_men','slave_women'),encoding='Latin-1',showProgress=FALSE)
P <- P[year>=1705 & year<=1725]
stopifnot(!anyDuplicated(P$hhobs))
H <- current_names(P$names_men)
WN <- current_names(P$widow_of)
name_cols <- names(H)
P[,c(paste0('h_',name_cols)):=H]
P[,c(paste0('w_',name_cols)):=WN]
P[,`:=`(slaves=adult_slaves(.SD),is_widow=!is.na(widow)&widow==1)]
# Source-level female-name exceptions in the nominal male-name field. The
# 1714 transcription is uncertain (Johanna ?? Groenewald); exclusion avoids
# treating this questionable name as a male continuation.
female_hhobs <- c(6388L,7903L,7948L,8042L,9882L,10027L)
P[,eligible:=h_valid & !is_widow & !hhobs %in% female_hhobs &
    !grepl('&',names_men,fixed=TRUE) & !grepl('^gemagtigde',tolower(trimws(names_men)))]
put(P[hhobs %in% female_hhobs | grepl('&',names_men,fixed=TRUE),
      .(hhobs,year,names_men,widow,reason=ifelse(hhobs %in% female_hhobs,
        'female or uncertain female name in male-name field','explicit joint-name entry'))],
    'eligibility_exceptions')
windows<-c(1709L,1710L,1713L,1718L,1721L)
parsed <- function(d,prefix='h_') {
  x<-copy(d[,paste0(prefix,name_cols),with=FALSE]);setnames(x,name_cols);x
}
# Retain every source row, including same-year full-name collisions.
build_window <- function(center,baseline='last_roll',post=1L,cap=Inf) {
  last_year<-max(P[year<center,year]);hi<-min(center+post,cap)
  a<-if(baseline=='last_roll') copy(P[year==last_year & eligible]) else
    copy(P[year>=center-3L & year<center & eligible])
  if(baseline!='last_roll') {
    a[,last_for_key:=max(year),by=h_key];a<-a[year==last_for_key];a[,last_for_key:=NULL]
  }
  setorder(a,hhobs)
  men<-P[year>=center & year<=hi & eligible]
  wid<-P[year>=center & year<=hi & is_widow & w_valid]
  am<-parsed(a);mn<-parsed(men);wn<-parsed(wid,'w_')
  out<-rbindlist(lapply(seq_len(nrow(a)),function(i){
    nm<-current_name_match(am[i],mn);nw<-current_name_match(am[i],wn)
    # Unique target identity keys are counted across all observation years;
    # same-key repeated records do not create fictitious additional candidates.
    targets<-unique(c(mn$key[nm],wn$key[nw]))
    first_ok<-!is.na(am$first_token[i])&nchar(am$first_token[i])>=4 &
      stringdist::stringdist(am$first_token[i],mn$first_token,method='jw',p=.1)<=.10
    fallback<-first_ok & abs(men$slaves-a$slaves[i])<=2 &
      current_qualifier_compatible(am$qualifier[i],mn$qualifier)
    fallback[is.na(fallback)]<-FALSE
    data.table(hhobs=a$hhobs[i],year=a$year[i],head=a$names_men[i],
      cluster_key=a$h_key[i],center=center,low=a$slaves[i]==0,slaves=a$slaves[i],
      dissolved_name=length(targets)==0L,
      dissolved_permissive=!(length(targets)>0L|any(fallback)),
      dissolved_unique=length(targets)!=1L,
      name_candidate_keys=length(targets),name_candidate_rows=sum(nm)+sum(nw),
      fallback_candidate_keys=uniqueN(mn$key[fallback]),
      fallback_candidate_rows=sum(fallback),
      name_examples=paste(head(targets,5),collapse=';'),
      fallback_examples=paste(head(unique(mn$key[fallback]),5),collapse=';'))
  }))
  attr(out,'coverage')<-data.table(center=center,baseline_rule=baseline,
    baseline_rolls=paste(sort(unique(a$year)),collapse=';'),
    first_search_year=center,last_search_year=hi,
    search_rolls=paste(sort(unique(P[year>=center & year<=hi,year])),collapse=';'),
    n_search_rolls=uniqueN(P[year>=center & year<=hi,year]))
  out
}
specs<-list(
  primary=list(baseline='last_roll',post=1L),
  three_year_baseline=list(baseline='three_year',post=1L),
  long_search=list(baseline='last_roll',post=4L),
  three_year_baseline_long_search=list(baseline='three_year',post=4L))
panels<-list();coverage<-list()
for(s in names(specs)) {
  parts<-lapply(windows,function(cc) build_window(cc,baseline=specs[[s]]$baseline,
    post=specs[[s]]$post,cap=if(cc<1713)1712 else Inf))
  coverage[[s]]<-rbindlist(lapply(parts,attr,'coverage'))[,spec:=s]
  panels[[s]]<-rbindlist(parts)[,spec:=s]
}
W<-rbindlist(panels);stopifnot(!anyDuplicated(W[,.(spec,center,hhobs)]))
saveRDS(W,file.path(AD,'current_panel_records.rds'))
put(rbindlist(coverage),'coverage')
outcomes<-c('dissolved_name','dissolved_permissive','dissolved_unique')
cells<-rbindlist(lapply(outcomes,function(y) W[,.(outcome=y,n=.N,
  events=sum(get(y)),rate=mean(get(y))),by=.(spec,center,low)]))
put(cells,'cells')
put(W[,.(n=.N,distinct_full_keys=uniqueN(cluster_key),
  same_year_key_collision_rows=sum(duplicated(cluster_key)|duplicated(cluster_key,fromLast=TRUE))),
  by=.(spec,center)],'risk_records')
put(W[,.(n=.N,ambiguous_name_keys=sum(name_candidate_keys>1),
  name_unlinked=sum(dissolved_name),fallback_reclassified=sum(dissolved_name & !dissolved_permissive),
  fallback_reclassified_multiple=sum(dissolved_name & !dissolved_permissive & fallback_candidate_keys>1),
  median_fallback_keys=if(any(dissolved_name & !dissolved_permissive))
    as.numeric(median(fallback_candidate_keys[dissolved_name & !dissolved_permissive])) else NA_real_),
  by=.(spec,center,low)],'ambiguity')
# All comparisons use the same fixed weights on the four placebo windows.
# Multiyear repeats and same-year name collisions share full-name-key clusters.
estimate <- function(d,y1,y2=NULL) {
  c<-d[,.(n=.N,p1=mean(get(y1)),p2=if(is.null(y2))1 else mean(get(y2))),by=.(center,low)]
  stopifnot(all(c$p1>0),all(c$p2>0),uniqueN(c$center)==5L)
  c[,weight:=ifelse(center==1713,1,-.25)*ifelse(low,1,-1)]
  theta<-sum(c$weight*log(c$p1/c$p2))
  x<-merge(d,c,by=c('center','low'))
  x[,influence:=weight*((get(y1)-p1)/p1-
    (if(is.null(y2)) 0 else (get(y2)-p2)/p2))/n]
  cl<-x[,.(u=sum(influence)),by=cluster_key];G<-nrow(cl)
  se<-sqrt(sum(cl$u^2)*G/(G-1))
  data.table(estimate=theta,se=se,lo=theta-1.96*se,hi=theta+1.96*se,
    p=2*pnorm(-abs(theta/se)),clusters=G)
}
results<-rbindlist(lapply(names(panels),function(s){
 d<-panels[[s]]
 rbindlist(list(
   cbind(spec=s,quantity='DD_name',estimate(d,'dissolved_name')),
   cbind(spec=s,quantity='DD_permissive',estimate(d,'dissolved_permissive')),
   cbind(spec=s,quantity='DD_unique',estimate(d,'dissolved_unique')),
   cbind(spec=s,quantity='permissive_minus_name',estimate(d,'dissolved_permissive','dissolved_name')),
   cbind(spec=s,quantity='unique_minus_name',estimate(d,'dissolved_unique','dissolved_name'))))
}))
put(results,'estimates')
# Independent saturated Poisson regression + cluster matrix sandwich verifies
# both the fixed-weight estimate and paired-rule covariance on primary data.
d<-panels$primary
Z<-rbind(d[,.(cluster_key,center,low,y=as.integer(dissolved_permissive),rule='permissive')],
         d[,.(cluster_key,center,low,y=as.integer(dissolved_name),rule='name')])
Z[,cell:=paste(center,low,rule,sep='_')]
fit<-glm(y~0+factor(cell),family=poisson(),data=Z)
X<-model.matrix(fit);bread<-solve(crossprod(X,fit$fitted.values*X))
scores<-rowsum(X*residuals(fit,type='response'),Z$cluster_key,reorder=FALSE)
G<-nrow(scores);V<-bread%*%crossprod(scores)%*%bread*G/(G-1)
b<-vapply(colnames(X),function(n){
 p<-strsplit(sub('factor(cell)','',n,fixed=TRUE),'_',fixed=TRUE)[[1]]
 (if(p[1]=='1713')1 else -.25)*(if(p[2]=='TRUE')1 else -1)*(if(p[3]=='permissive')1 else -1)
},numeric(1))
glm_est<-sum(b*coef(fit));glm_se<-sqrt(drop(t(b)%*%V%*%b))
ref<-results[spec=='primary'&quantity=='permissive_minus_name']
stopifnot(abs(glm_est-ref$estimate)<1e-8,abs(glm_se-ref$se)<1e-8,
  all(W$dissolved_permissive<=W$dissolved_name),
  all(W$dissolved_name<=W$dissolved_unique),
  all(rbindlist(coverage)[spec=='primary',n_search_rolls]==2L))
# Row-order invariance of grouping and the estimator; no first-row selection.
reordered<-d[rev(seq_len(nrow(d)))]; rev_est<-estimate(reordered,'dissolved_permissive','dissolved_name')
stopifnot(abs(rev_est$estimate-ref$estimate)<1e-12,abs(rev_est$se-ref$se)<1e-12)
put(data.table(check=c('parser cases','independent saturated GLM estimate and covariance',
  'row-order invariance','nested rule events','two observed rolls per primary search'),passed=TRUE),'checks')
print(results[spec=='primary'])
cat('Current record-continuation diagnostic and independent checks passed.\n')

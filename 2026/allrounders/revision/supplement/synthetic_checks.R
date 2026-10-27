# Standalone mathematical and timing counterexamples; no licensed inputs.
stopifnot(sum(pmax(c(10,-9),0))==10,pmax(sum(c(10,-9)),0)==1)
# With a white-ball pool batting share .9, a pure batter falls in the legacy band.
S<-.9;share<-1;rca<-share/S;symmetric<-(rca-1)/(rca+1)
stopifnot(abs(symmetric)<=.25,share>=.6*S,share<=min(1,(5/3)*S))
# A specialist-relative joint probability is not identified by marginal probabilities.
p<-.89;q<-.60
stopifnot(max(0,p+q-1)<.5,p*q>.5,min(p,q)>.5)
# A chronological landmark must not see the result of a match still in progress.
match_start<-as.Date('2000-01-01');match_end<-as.Date('2000-01-04');event<-as.Date('2000-01-03')
stopifnot(match_start<event,!(match_end<event),match_end+1>event)
# A match beginning inside the forecast window but ending outside contributes no outcome.
horizon<-as.Date('2005-01-04');future_start<-as.Date('2005-01-03');future_end<-as.Date('2005-01-06')
stopifnot(future_start<=horizon,!(future_end<=horizon))
# Pooling coherent weights gives the exact three-margin identity.
M<-100;Mb<-60;balls<-1200;W<-45
stopifnot(abs(W/M-(Mb/M)*(balls/Mb)*(W/balls))<1e-12)
# Aggregating a rate by players changes weights: these are different estimands.
stopifnot(mean(c(1/1,10/100))!=11/101)
# Five stated threshold axes have 108 combinations, or 216 with two activity rules.
stopifnot(3*3*2*2*3==108,3*3*2*2*3*2==216)
script_dir <- dirname(normalizePath(sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value=TRUE)[1])))
source(file.path(script_dir, '../scripts/promotion_helpers.R'))
suppressPackageStartupMessages(library(survival))
# The event at completion day uses the old history; the next-day event uses
# the new one. Exercise the actual counting-process representation as well.
u <- history_update(as.Date('2000-01-04'), as.Date('2000-01-01'))
intervals <- Surv(c(0,u), c(u,5), c(0,1))
state_at <- function(t) which(intervals[,1] < t & t <= intervals[,2])
stopifnot(state_at(3)==1L, state_at(4)==2L, u==3.5)
# At fixed beta=0, coxph.detail reports the actual risk-set mean. A second
# player's event on day 3 must see x=0; a day-4 event must see x=1 for the
# player whose match ended on day 3 (one of two people still at risk).
toy <- data.frame(start=c(0,u,0,0),stop=c(u,5,3,4),event=c(0,0,1,1),x=c(0,1,0,0))
fit <- coxph(Surv(start,stop,event)~x,data=toy,init=0,
             control=coxph.control(iter.max=0),x=TRUE)
detail <- coxph.detail(fit)
stopifnot(identical(as.numeric(detail$time),c(3,4)),
          max(abs(as.numeric(detail$means)-c(0,.5)))<1e-12,
          identical(as.numeric(detail$nrisk),c(3,2)))
# Verify weighted incidence against explicit row replication with censoring,
# competing death, tied events, and resamples with an empty role.
suppressPackageStartupMessages(library(data.table))
d <- data.table(entry_cluster=c('a','a','b','b','b'),
                follow=c(1,3,1,2,3),status=c(1,0,2,1,0),
                role=c('All-round role','Batting role','All-round role','All-round role','Batting role'))
scalar <- function(z) {
  if (!nrow(z)) return(NA_real_)
  S<-1; F<-0
  for(t in sort(unique(z$follow[z$status>0]))) {
    n<-sum(z$follow>=t); e1<-sum(z$follow==t & z$status==1); e2<-sum(z$follow==t & z$status==2)
    F<-F+S*e1/n; S<-S*(1-(e1+e2)/n)
  }
  F
}
w<-rbind(c(2,1,0),c(0,1,2))
expanded<-lapply(seq_len(ncol(w)),function(j)rbindlist(lapply(1:2,function(k)d[rep(which(entry_cluster==c('a','b')[k]),w[k,j])])) )
stopifnot(max(abs(cif_weighted(d,c('a','b'),w)-vapply(expanded,scalar,numeric(1))))<1e-12)
stopifnot(is.na(cif_weighted(d[entry_cluster=='a'],c('a','b'),w)[3]))
exact<-landmark_difference_interval(d,exact=TRUE)
delta<-vapply(expanded,function(z)scalar(z[role=='All-round role'])-scalar(z[role=='Batting role']),numeric(1))
stopifnot(exact$evaluated_resamples==3,exact$valid_probability==1,
          exact$lower==min(delta),exact$upper==max(delta))
cat('Eight original examples and new survival-boundary/weighted-bootstrap checks passed.\n')

source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
suppressPackageStartupMessages(library(survival))
source(file.path(revision_dir, 'scripts/promotion_helpers.R'))
require_data_gate('domestic-to-Test observed transitions')
add_years<-function(x,k)as.IDate(vapply(as.Date(x),function(d)as.numeric(seq(as.Date(d,origin='1970-01-01'),by=paste(k,'years'),length.out=2)[2]),numeric(1)),origin='1970-01-01')
dom<-rbindlist(lapply(c('county','sheffield'),function(co){z<-readRDS(file.path(revision_dir,'data/analysis',paste0('player_match_',co,'.rds')));z[,comp:=co];z}),fill=TRUE)
bb<-rbindlist(lapply(c('county','sheffield'),function(co){z<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',co,'.rds')))$batting;z[,comp:=co];z}))
setorder(dom,player,start_date,comp,match_id)
entry<-dom[,.(entry=min(start_date),entry_comp=comp[1],entry_team=team[1],last_end=max(end_date),tie=uniqueN(comp[start_date==min(start_date)])>1),by=player]
stopifnot(!any(entry$tie))
tt<-readRDS(file.path(revision_dir,'data/analysis/player_match_tests.rds'))[,.(test=min(start_date)),by=player]
entry<-merge(entry,tt,by='player',all.x=TRUE)
entry[,`:=`(horizon=add_years(entry,5),landmark=add_years(entry,2),entry_decade=10L*(as.integer(format(entry,'%Y'))%/%10L))]
entry[,cohort_decision:=fcase(!is.na(test)&test<=entry,'Test before or on domestic entry',entry_comp=='county' & entry<as.IDate('1950-01-01'),'County boundary 1946-1949',entry_comp=='sheffield' & entry<as.IDate('1897-01-01'),'Shield boundary 1892-1896',default='Included')]
put(entry[,.(players=.N),by=.(entry_comp,cohort_decision)],'promotion_cohort_flow')
entry<-entry[cohort_decision=='Included']
death<-read_csv(file.path(revision_dir,'config/promotion_mortality.csv'))[,.(player,death=as.IDate(analysis_death),source_strength)]
entry<-merge(entry,death,by='player',all.x=TRUE)
entry[,admin_end:=pmin(horizon,cutoff)]
entry[,end:=pmin(admin_end,test,death,na.rm=TRUE)]
entry[,status:=fcase(!is.na(test)&test==end,1L,!is.na(death)&death==end,2L,default=0L)]
entry[,`:=`(follow_days=as.numeric(end-entry),entry_cluster=paste(entry_comp,entry_team,sep=':'))]
stopifnot(all(entry$follow_days>0),!anyDuplicated(entry$player))
dom<-merge(dom,entry[,.(player,entry,end,entry_comp,entry_decade,entry_cluster,status,landmark,horizon,admin_end)],by='player')
dom<-dom[end_date<end];setorder(dom,player,end_date,start_date,match_id)
bb<-merge(bb,dom[,.(player,match_id,comp,end_date)],by=c('player','match_id','comp'))
setkey(dom,player);setkey(bb,player)
risk<-vector('list',nrow(entry));landrows<-list();checks<-list()
for(i in seq_len(nrow(entry))) {
 e<-entry[i];pp<-dom[J(e$player)];pp<-pp[!is.na(match_id)]
 bp<-bb[J(e$player)];bp<-bp[!is.na(match_id)]
 total<-as.numeric(e$end-e$entry)
 initial<-data.table(update=0,past_matches=0L,past_runs=0,past_wickets=0,past_balls=0,past_innings=0L,position=NA_integer_,last_completed=as.IDate(NA))
 if(nrow(pp)) {
  updates<-pp[,.(nr=.N,r=sum(bat_runs),w=sum(wickets),balls=sum(scorecard_balls),inn=sum(innings_batted)),by=end_date]
  setorder(updates,end_date)
  updates[,`:=`(past_matches=cumsum(nr),past_runs=cumsum(r),past_wickets=cumsum(w),past_balls=cumsum(balls),past_innings=cumsum(inn),update=history_update(end_date,e$entry))]
  updates[,position:=vapply(end_date,function(dd)mode_position(bp[end_date<=dd,actual_position]),integer(1))]
  states<-rbind(initial,updates[,.(update,past_matches,past_runs,past_wickets,past_balls,past_innings,position,last_completed=end_date)])
 }else states<-initial
 states<-states[update<total];setorder(states,update)
 states[,`:=`(tstart=update,tstop=c(update[-1],total))]
 states[,role:=factor(fcase(past_innings>=20 & !is.na(position)&position<=7 & past_balls>=2000,'All-round role',past_innings>=20 & !is.na(position)&position<=7,'Batting role',past_balls>=2000,'Bowling role',default='Insufficient/other'),levels=c('Batting role','All-round role','Bowling role','Insufficient/other'))]
 states[,`:=`(bat_rate=fifelse(past_matches>0,past_runs/past_matches,0),wicket_rate=fifelse(past_matches>0,past_wickets/past_matches,0),event=0L)]
 states[.N,event:=as.integer(e$status==1)]
 states[,`:=`(player=e$player,entry_comp=e$entry_comp,entry_decade=e$entry_decade,entry_cluster=e$entry_cluster,calendar_start=as.Date(e$entry)+tstart)]
 stopifnot(all(states$tstop>states$tstart),all(is.na(states$last_completed)|states$last_completed<states$calendar_start))
 risk[[i]]<-states
 # Landmark is a separate conditional estimand; all earlier transitions retained above.
 if(e$end>e$landmark) {
  jj<-findInterval(as.numeric(e$landmark-e$entry),states$update)
  ls<-states[max(1,jj)]
  landrows[[length(landrows)+1L]]<-data.table(player=e$player,entry_comp=e$entry_comp,entry_decade=e$entry_decade,entry_cluster=e$entry_cluster,role=ls$role,past_matches=ls$past_matches,bat_rate=ls$bat_rate,wicket_rate=ls$wicket_rate,entry=e$entry,landmark=e$landmark,horizon=e$horizon,end=e$end,status=e$status,follow=as.numeric(e$end-e$landmark),complete=e$horizon<=cutoff)
 }
}
risk<-rbindlist(risk);land<-rbindlist(landrows)
saveRDS(list(entry=entry,risk=risk,landmark=land),file.path(revision_dir,'data/analysis/promotion.rds'))
put(entry[,.(entrants=.N,Test_events=sum(status==1),deaths=sum(status==2),admin_censored=sum(status==0 & horizon>cutoff),five_year_non_events=sum(status==0 & horizon<=cutoff),events_first_two_years=sum(status==1 & test<=landmark)),by=entry_comp],'promotion_sample')
put(risk[event==1,.(events=.N),by=.(entry_comp,role)],'promotion_event_history_roles')
fits<-list();diagnostics<-list()
fit_one<-function(d)coxph(Surv(tstart,tstop,event)~role+splines::ns(log1p(past_matches),knots=log1p(c(5,30)),Boundary.knots=c(0,log1p(200)))+splines::ns(bat_rate,knots=c(20,50),Boundary.knots=c(0,150))+splines::ns(wicket_rate,knots=c(.5,1.5),Boundary.knots=c(0,10))+factor(entry_decade)+strata(entry_comp),data=d,ties='efron',x=TRUE,model=TRUE)
for(scope in c('pooled','county','sheffield')) {
 dd<-if(scope=='pooled')risk else risk[entry_comp==scope]
 f<-fit_one(dd);term<-'roleAll-round role';beta<-coef(f)[term]
 # Delete-one entry-team jackknife; t reference uses G-1, including six Shield teams.
 teams<-sort(unique(dd$entry_cluster));loo<-vapply(teams,function(cl){ff<-try(fit_one(dd[entry_cluster!=cl]),silent=TRUE);if(inherits(ff,'try-error'))return(NA_real_);unname(coef(ff)[term])},numeric(1))
 stopifnot(all(is.finite(loo)))
 se<-sqrt((length(teams)-1)/length(teams)*sum((loo-mean(loo))^2));df<-length(teams)-1
 fits[[scope]]<-data.table(scope,players=uniqueN(dd$player),intervals=nrow(dd),events=sum(dd$event),clusters=length(teams),log_hr=unname(beta),se_jackknife=se,df,HR=exp(beta),lower=exp(beta-qt(.975,df)*se),upper=exp(beta+qt(.975,df)*se),p=2*pt(-abs(beta/se),df))
 ph<-cox.zph(f,transform='rank');diagnostics[[scope]]<-cbind(scope,as.data.table(ph$table,keep.rownames='term'))
 saveRDS(f,file.path(revision_dir,'data/analysis',paste0('promotion_cox_',scope,'.rds')))
 cat(scope,'Cox and',length(teams),'entry-team jackknifes complete\n')
}
fits<-rbindlist(fits);fits[,p_holm:=p.adjust(p,'holm')];put(fits,'promotion_cox');put(rbindlist(diagnostics),'promotion_ph_diagnostics')
# Competing-risk cumulative incidence from the two-year landmark, with censoring.
cif<-function(d) {
 if(!nrow(d))return(NA_real_)
 ev<-d[status>0,.(d1=sum(status==1),d2=sum(status==2)),by=follow];setorder(ev,follow)
 surv<-1;F1<-0
 for(i in seq_len(nrow(ev))){n<-sum(d$follow>=ev$follow[i]);F1<-F1+surv*ev$d1[i]/n;surv<-surv*(1-(ev$d1[i]+ev$d2[i])/n)}
 F1
}
risks<-list();diffs<-list()
for(co in c('county','sheffield')) {
 z<-land[entry_comp==co]
 for(rr in levels(z$role)) {
  d<-z[role==rr];if(!nrow(d))next
  # Point risks describe the full observed competing-risk process. Player bootstrap
  # intervals are supplementary; differences use entry-team resampling below.
  set.seed(2412L + 10L * match(co,c('county','sheffield')) + match(rr,levels(z$role)))
  est<-cif(d);boot<-replicate(300,cif(d[sample.int(nrow(d),nrow(d),TRUE)]))
  risks[[length(risks)+1L]]<-data.table(comp=co,role=rr,n=nrow(d),events=sum(d$status==1),risk=est,lower=quantile(boot,.025,names=FALSE),upper=quantile(boot,.975,names=FALSE),complete_n=sum(d$complete),complete_risk=d[complete==TRUE,mean(status==1)])
 }
 dr<-z[role%in%c('All-round role','Batting role')]
 ci<-landmark_difference_interval(dr,exact=co=='sheffield')
 stopifnot(abs(ci$difference-(cif(dr[role=='All-round role'])-cif(dr[role=='Batting role'])))<1e-12)
 diffs[[co]]<-cbind(data.table(comp=co),as.data.table(ci))
}
put(rbindlist(risks),'promotion_landmark_risks');put(rbindlist(diffs),'promotion_landmark_differences')
# Calendar-era differences are descriptive and explicitly exploratory.
land[,era:=ifelse(entry<as.IDate('2000-01-01'),'Before 2000','2000 onwards')]
put(land[complete==TRUE,.(n=.N,events=sum(status==1),risk=mean(status==1)),by=.(entry_comp,era,role)],'promotion_era_risks')
# Sensitivity to uncertain death timing: extend all deceased non-events through the
# full administrative horizon, retaining their final past-only state. This bounds
# recorded death versus no recorded competing exit without imputing an age.
rs<-copy(risk)
for(pl in entry[status==2,player]) {
 ee<-entry[player==pl];idx<-which(rs$player==pl);set(rs,tail(idx,1),'tstop',as.numeric(ee$admin_end-ee$entry))
}
fs<-fit_one(rs);put(data.table(specification='Ignore all recorded deaths',pooled_HR=exp(coef(fs)['roleAll-round role'])),'promotion_mortality_sensitivity')
cat('Promotion analyses complete; age and national eligibility not inferred\n')

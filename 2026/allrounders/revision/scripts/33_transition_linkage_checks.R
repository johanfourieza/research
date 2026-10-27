source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('domestic-to-Test observed transitions')
t<-readRDS(file.path(revision_dir,'data/analysis/player_match_tests.rds'))
setorder(t,player,start_date,match_id)
t<-t[,.(test=min(start_date),nation=team[1]),by=player]
rows<-list()
for(co in c('county','sheffield')) {
 d<-readRDS(file.path(revision_dir,'data/analysis',paste0('player_match_',co,'.rds')))
 dd<-d[,.(domestic=min(start_date)),by=player]
 target_nation<-if(co=='county')'England'else'Australia'
 z<-merge(t[nation==target_nation],dd,by='player',all.x=TRUE)
 z[,comp:=co]
 z[,decision:=fcase(test<min(d$start_date),'Test debut before source begins',is.na(domestic),'No accepted same-key domestic record',domestic<=test,'Domestic record before or on Test debut',default='Domestic record after Test debut')]
 rows[[co]]<-z
}
z<-rbindlist(rows);write_audit(z,'final_test_debut_linkage')
put(z[,.(players=.N),by=.(comp,decision)],'test_debut_linkage_summary')
print(z[decision%in%c('No accepted same-key domestic record','Domestic record after Test debut')])
# More flexible baseline hazards, exploratory.
suppressPackageStartupMessages(library(survival))
d<-readRDS(file.path(revision_dir,'data/analysis/promotion.rds'))$risk
base<-paste('role + splines::ns(log1p(past_matches),knots=log1p(c(5,30)),Boundary.knots=c(0,log1p(200))) +',
 'splines::ns(bat_rate,knots=c(20,50),Boundary.knots=c(0,150)) + splines::ns(wicket_rate,knots=c(.5,1.5),Boundary.knots=c(0,10))')
specs<-c(era_strata=paste(base,'+ strata(entry_comp,entry_decade)'))
out<-lapply(names(specs),function(n){f<-coxph(as.formula(paste('Surv(tstart,tstop,event) ~',specs[[n]])),data=d,ties='efron');data.table(specification=n,HR=exp(coef(f)['roleAll-round role']),interpretation='Exploratory point estimate; no additional hypothesis test')})
put(rbindlist(out),'promotion_additional_sensitivity')

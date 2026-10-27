source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('workload and composition')
distros<-list();currency<-list();alt<-list();index<-list();checks<-list();examples<-list();predictive<-list();influence<-list()
for(comp in names(configs)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')));p<-a$pm;c<-a$career
 du<-readRDS(file.path(revision_dir,'data/analysis',paste0('dual_',comp,'.rds')))
 for(v in c('bat_per_match','wickets_per_match','bowling_volume','batting_share','scorecard_balls')) {
  z<-c[[v]];distros[[length(distros)+1L]]<-data.table(comp,variable=v,n=sum(!is.na(z)),mean=mean(z,na.rm=TRUE),sd=sd(z,na.rm=TRUE),min=min(z,na.rm=TRUE),q25=quantile(z,.25,na.rm=TRUE,names=FALSE),median=median(z,na.rm=TRUE),q75=quantile(z,.75,na.rm=TRUE,names=FALSE),max=max(z,na.rm=TRUE))
 }
 long<-comp%in%c('tests','county','sheffield')
 for(setting in if(long)c('era price x0.8','era price','era price x1.2','constant 30')else c('economy p65','economy p75','economy p85')) {
  v<-switch(setting,'era price x0.8'=.8*c$bowling_volume,'era price'=c$bowling_volume,'era price x1.2'=1.2*c$bowling_volume,'constant 30'=30*c$wickets,'economy p65'=c$low_economy_volume,'economy p75'=c$bowling_volume,'economy p85'=c$high_economy_volume)
  s<-c$bat_runs/(c$bat_runs+pmax(0,v));S<-sum(c$bat_runs)/(sum(c$bat_runs)+sum(pmax(0,v)))
  sym<-(s-S)/(s+S);syw<-((1-s)/(1-S)-1)/((1-s)/(1-S)+1)
  currency[[length(currency)+1L]]<-data.table(comp,setting,pool_share=S,evaluable=sum(c$evaluable),legacy_band=sum(c$evaluable & abs(sym)<=.25,na.rm=TRUE),both_bands=sum(c$evaluable & abs(sym)<=.25 & abs(syw)<=.25,na.rm=TRUE),zero_bowling=sum(v<=0),median_share=median(s[c$evaluable],na.rm=TRUE),rank_correlation=cor(s,c$batting_share,method='spearman',use='complete.obs'))
 }
 examples[[comp]]<-du$career[evaluable==TRUE][order(-prob_independent)][1:5,.(comp=comp,player,matches,role,batting_share,prob_bat,prob_bowl,prob_independent,prob_lower,prob_upper)]
 # Verify sufficient-statistic REML against an independent lme4 implementation.
 if(comp=='tests') {
  z<-p[player%in%c[prior_eligible==TRUE,player]]
  for(v in c('y_bat','y_bowl')) {
   f<-lme4::lmer(as.formula(paste(v,'~1+(1|player)')),data=z,REML=TRUE,control=lme4::lmerControl(optimizer='bobyqa',optCtrl=list(rhoend=1e-10)))
   ours<-if(v=='y_bat')du$bat_fit else du$bowl_fit
   ref<-c(mu=unname(lme4::fixef(f)[1]),sigma2=sigma(f)^2,tau2=as.numeric(lme4::VarCorr(f)$player[1]))
   delta<-max(abs(ref-unlist(ours[c('mu','sigma2','tau2')])));print(list(discipline=v,reference=ref,ours=ours,delta=delta));stopifnot(delta<1e-4)
   checks[[length(checks)+1L]]<-data.table(comp,check=paste('REML vs lme4',v),max_error=delta,tolerance=1e-4)
  }
 }
 # Whole-career exclusion of every player in the quarantined match: a conservative
 # influence check beyond omitting that match's uncertain scorecard.
 if(comp=='county') {
  q<-read_csv(file.path(revision_dir,'audit/execution/match_accounting_quarantine.csv'))$player
  cc<-du$career[!player%in%q];bf<-safe_q(cc[role=='Batting role',bat_standardised]);wf<-safe_q(cc[role=='Bowling role',bowl_standardised]);sb<-du$bat_suff[player%in%cc$player];sw<-du$bowl_suff[player%in%cc$player]
  setkey(cc,player);setkey(sb,player);setkey(sw,player);fb<-fit_reml(sb[cc$prior_eligible]);fw<-fit_reml(sw[cc$prior_eligible]);pb<-posterior(sb,fb);pw<-posterior(sw,fw);u<-pnorm((pb$mean-bf)/pb$sd);v<-pnorm((pw$mean-wf)/pw$sd)
  influence[[comp]]<-data.table(comp,excluded_players=sum(du$career$player%in%q),eligible=sum(cc$evaluable),independent=sum(cc$evaluable & u*v>=.5),certain=sum(cc$evaluable & pmax(0,u+v-1)>=.5),possible=sum(cc$evaluable & pmin(u,v)>=.5))
 }
 fpath<-file.path(revision_dir,'data/analysis',paste0('forecast_',comp,'.rds'));if(!file.exists(fpath))next
 fc<-readRDS(fpath);d<-fc[partition=='test' & future_matches>0]
 d[,`:=`(actual_output_role=role=='All-round role',traditional_label=traditional_positive,sadekar_label=matches>=25 & top7_innings/matches>=.5 & bowl_matches/matches>=.5)]
 for(def in c('actual_output_role','traditional_label',if(comp=='odis')'sadekar_label')) {
  for(disc in c('bat','bowl')) {
   ref<-if(disc=='bat')'Batting role'else'Bowling role';vals<-if(disc=='bat')d$future_runs/d$future_matches else d$future_wickets/d$future_matches
   flag<-d[[def]];base<-d$role==ref & !flag
   alt[[length(alt)+1L]]<-data.table(comp,definition=def,discipline=disc,labelled=sum(flag),reference=sum(base),labelled_mean=mean(vals[flag]),reference_mean=mean(vals[base]),difference=mean(vals[flag])-mean(vals[base]))
  }
 }
 ixfun<-function(d){m<-d[,.(b=mean(future_runs/future_matches),w=mean(future_wickets/future_matches)),by=role];if(!all(c('All-round role','Batting role','Bowling role')%in%m$role))return(NA_real_);m[role=='All-round role',b]/m[role=='Batting role',b]+m[role=='All-round role',w]/m[role=='Bowling role',w]}
 est<-ixfun(d);groups<-unique(d$first_team);loo<-vapply(groups,function(cl)ixfun(d[first_team!=cl]),numeric(1));se<-sqrt((length(groups)-1)/length(groups)*sum((loo-mean(loo))^2));crit<-qt(.975,length(groups)-1)
 index[[comp]]<-data.table(comp,allround_n=sum(d$role=='All-round role'),index=est,lower=est-crit*se,upper=est+crit*se,clusters=length(groups),interpretation='Additive specialist-relative native output; not roster slots')
 # Training-only early-career prior; subsequent native mean output conditional on
 # at least one future appearance. This checks calibration under career evolution.
 ids<-fc[partition=='training',player];pp<-p[player%in%fc$player];setorder(pp,player,start_date,match_id);pp<-pp[,head(.SD,30),by=player]
 for(v in c('bat_runs','wickets')) {
  ss<-suff(pp,v);f<-fit_reml(ss[player%in%ids]);ss<-ss[player%in%d$player];setkey(ss,player);setkey(d,player);pr<-posterior(ss,f)
  observed<-if(v=='bat_runs')d$future_runs/d$future_matches else d$future_wickets/d$future_matches
  sdev<-sqrt(pr$sd^2+f$sigma2/d$future_matches)
  for(level in c(.5,.8,.95))predictive[[length(predictive)+1L]]<-data.table(comp,discipline=v,nominal_coverage=level,n=nrow(d),observed_coverage=mean(abs(observed-pr$mean)<=qnorm((1+level)/2)*sdev),mean_error=mean(observed-pr$mean),root_mean_square_error=sqrt(mean((observed-pr$mean)^2)),negative_lower_fraction=mean(pr$mean-qnorm((1+level)/2)*sdev<0))
 }
 cat(comp,'distribution, comparator and predictive checks complete\n')
}
put(rbindlist(distros),'output_distributions');put(rbindlist(currency),'currency_sensitivity');put(rbindlist(alt),'alternative_definition_outputs');put(rbindlist(index),'additive_output_index');put(rbindlist(checks),'reml_independent_checks');put(rbindlist(examples),'probability_examples');put(rbindlist(predictive),'eb_future_calibration');put(rbindlist(influence),'quarantined_match_influence')
g<-read_csv(file.path(results_dir,'prospective_output_gaps.csv'));g[,p_holm_primary:=p.adjust(p,'holm',n=10)];put(g,'prospective_output_gaps')

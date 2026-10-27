source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
suppressPackageStartupMessages(library(survival))
for(scope in c('verified scorecard output','workload and composition','output uncertainty','chronologically held-out prediction','domestic-to-Test observed transitions'))require_data_gate(scope)
set.seed(2412)
# Robustness checks requested in the 2026-10-09 feedback. The script only reads
# objects written by 22-28 and never changes a baseline result. Each block names
# the feedback item it answers.

# ---------------------------------------------------------------------------
# B1, B4, B5: classification sensitivities (feedback overall-1, #2, #4, #5)
# ---------------------------------------------------------------------------
# Player-specific residual variance: the player's own within-career sum of
# squares shrunk towards the pooled REML variance with nu prior degrees of freedom.
hetero_posterior<-function(s,fit,nu) {
 s2p<-(nu*fit$sigma2+s$ss)/(nu+pmax(s$n-1,0))
 v<-1/(1/fit$tau2+s$n/s2p)
 data.table(mean=v*(fit$mu/fit$tau2+s$n*s$m/s2p),sd=sqrt(v))
}
count_row<-function(comp,variant,c,u,v,dual) data.table(comp,variant,dual_role=sum(dual),eligible=sum(c$evaluable),
 independent=sum(c$evaluable & u*v>=.5),all_couplings=sum(c$evaluable & pmax(0,u+v-1)>=.5),
 some_coupling=sum(c$evaluable & pmin(u,v)>=.5),expected=sum((u*v)[c$evaluable]))
cls<-list();examples<-list()
for(comp in names(configs)) {
 d<-readRDS(file.path(revision_dir,'data/analysis',paste0('dual_',comp,'.rds')));c<-d$career;sb<-d$bat_suff;sw<-d$bowl_suff
 stopifnot(identical(c$player,sb$player),identical(c$player,sw$player))
 bmin<-ball_min[[comp]]
 base_bat<-!is.na(c$actual_modal_position) & c$actual_modal_position<=7 & c$innings_batted>=20
 base_bowl<-c$scorecard_balls>=bmin
 stopifnot(all((base_bat & base_bowl)==(c$role=='All-round role')))
 floors_obs<-function(batpool,bowlpool)c(safe_q(c$bat_standardised[batpool]),safe_q(c$bowl_standardised[bowlpool]))
 probs<-function(f,pb=data.table(mean=c$post_bat,sd=c$sd_bat),pw=data.table(mean=c$post_bowl,sd=c$sd_bowl))list(u=pnorm((pb$mean-f[1])/pb$sd),v=pnorm((pw$mean-f[2])/pw$sd))
 variants<-list()
 # Baseline: reproduces dual_counts exactly.
 f0<-floors_obs(base_bat & !base_bowl,base_bowl & !base_bat);p0<-probs(f0)
 stopifnot(max(abs(p0$u*p0$v-c$prob_independent))<1e-10)
 variants$Baseline<-list(p=p0,dual=base_bat & base_bowl)
 # B1a: participation intensity. A batting role also needs at least half of all
 # appearances with an innings at seven or higher; a bowling role needs bowling in
 # at least half of all appearances. Specialist pools and floors are rebuilt.
 bt<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')))$batting
 t7<-bt[actual_position<=7,.(top7_matches=uniqueN(match_id)),by=player]
 top7_matches<-t7$top7_matches[match(c$player,t7$player)];top7_matches[is.na(top7_matches)]<-0L
 bat_i<-base_bat & top7_matches/c$matches>=.5;bowl_i<-base_bowl & c$bowl_matches/c$matches>=.5
 variants$`Participation intensity`<-list(p=probs(floors_obs(bat_i & !bowl_i,bowl_i & !bat_i)),dual=bat_i & bowl_i)
 # B1b: ball intensity. The ball minimum must be reached at the rate implied by
 # 30 appearances (2,000/30 = 66.7 balls per appearance in the long formats).
 bowl_r<-base_bowl & c$scorecard_balls/c$matches>=bmin/30
 variants$`Ball intensity`<-list(p=probs(floors_obs(base_bat & !bowl_r,bowl_r & !base_bat)),dual=base_bat & bowl_r)
 # B4: floors from the specialists' posterior means rather than observed means.
 f3<-c(safe_q(c$post_bat[base_bat & !base_bowl]),safe_q(c$post_bowl[base_bowl & !base_bat]))
 variants$`Posterior-mean floors`<-list(p=probs(f3),dual=base_bat & base_bowl)
 # B5: heteroscedastic shrinkage with weak and moderate variance pooling.
 for(nu in c(5,20)) {
  hb<-hetero_posterior(sb,d$bat_fit,nu);hw<-hetero_posterior(sw,d$bowl_fit,nu)
  variants[[paste0('Player-specific variance, nu=',nu)]]<-list(p=probs(f0,hb,hw),dual=base_bat & base_bowl)
 }
 top<-c[evaluable==TRUE][order(-prob_independent)][1:8,player]
 for(nm in names(variants)) {
  vv<-variants[[nm]]
  cls[[length(cls)+1L]]<-count_row(comp,nm,c,vv$p$u,vv$p$v,vv$dual)
  ii<-match(top,c$player)
  examples[[length(examples)+1L]]<-data.table(comp,variant=nm,player=top,prob_bat=vv$p$u[ii],prob_bowl=vv$p$v[ii],joint_independent=(vv$p$u*vv$p$v)[ii],joint_lower=pmax(0,vv$p$u+vv$p$v-1)[ii],joint_upper=pmin(vv$p$u,vv$p$v)[ii])
 }
}
cls<-rbindlist(cls);put(cls,'feedback_classification_sensitivity');put(rbindlist(examples),'feedback_probability_examples')

# Cell-specific denominators for the threshold grid (feedback #1).
grid<-read_csv(file.path(results_dir,'threshold_grid.csv'))
put(grid[,.(max_count=max(independent),max_share=max(independent/eligible),eligible_at_max=eligible[which.max(independent)],min_eligible=min(eligible),max_eligible=max(eligible)),by=.(comp,floor_quantile)],'feedback_grid_denominators')

# ---------------------------------------------------------------------------
# B3: exposure predictors and a second chronological split (overall-3, #9)
# ---------------------------------------------------------------------------
forms<-function(kn,bd,S)list(exposure_only='1',workload='role',traditional='role + traditional_diff + average_missing',share='role + log_volume + splines::ns(share, knots=kn, Boundary.knots=bd)',rca='role + log_volume + splines::ns(rca, knots=kn/S, Boundary.knots=bd/S)',continuous='role + splines::ns(bat_signal,df=3) + splines::ns(bowl_signal,df=3) + bowling_participation')
exposure<-' + log(days_to_landmark) + recent_apps + early_runs_annual + early_wkts_annual'
fperf<-list()
# The 2016 T20I alternative leaves too few training careers and is skipped by
# the sample-size rule below, so the alternative split covers four formats.
splits<-list(Baseline=c(default='2010-01-01',t20is='2019-01-01'),Alternative=c(default='2000-01-01',t20is='2016-01-01'))
for(comp in names(configs)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')));p<-a$pm
 fc<-readRDS(file.path(revision_dir,'data/analysis',paste0('forecast_',comp,'.rds')))
 setorder(p,player,start_date,match_id);p[,seq30:=seq_len(.N),by=player]
 ex<-merge(p[seq30<=30,.(player,start_date)],fc[,.(player,landmark)],by='player')[,.(recent_apps=sum(start_date>landmark-365L)),by=player]
 fc<-merge(fc,ex,by='player')
 # Elapsed time to the 30th appearance; annual production floors the denominator
 # at one year so very fast starts do not produce extreme rates.
 fc[,days_to_landmark:=as.numeric(landmark-first_date)+1]
 fc[,`:=`(early_runs_annual=bat_runs/(pmax(days_to_landmark,365.25)/365.25),early_wkts_annual=wickets/(pmax(days_to_landmark,365.25)/365.25))]
 fc[,traditional_raw:=bat_average-bowling_average]
 for(sp in names(splits)) {
  splitdate<-as.IDate(if(comp=='t20is')splits[[sp]][['t20is']]else splits[[sp]][['default']])
  cc<-copy(fc);cc[,partition:=fcase(horizon<splitdate,'training',landmark>=splitdate,'test',default='embargo')]
  train<-cc[partition=='training'];test<-cc[partition=='test']
  if(nrow(train)<40 || nrow(test)<30)next
  S<-sum(train$bat_runs)/(sum(train$bat_runs)+sum(train$volume_bowl));med<-median(train$traditional_raw,na.rm=TRUE)
  cc[,`:=`(rca=share/S,average_missing=as.integer(!is.finite(traditional_raw)),traditional_diff=fifelse(is.finite(traditional_raw),traditional_raw,med))]
  train<-cc[partition=='training'];test<-cc[partition=='test']
  bd<-range(train$share);kn<-bd[1]+diff(bd)*c(1/3,2/3);ff<-forms(kn,bd,S)
  for(outcome in c('future_bat_annual','future_bowl_annual'))for(model in names(ff))for(expo in c(FALSE,TRUE)) {
   fit<-lm(as.formula(paste(outcome,'~',ff[[model]],if(expo)exposure else '')),data=train)
   pr<-as.numeric(predict(fit,newdata=test))
   fperf[[length(fperf)+1L]]<-data.table(comp,split=sp,split_date=splitdate,outcome,model,exposure=expo,n_train=nrow(train),n_test=nrow(test),rmse=sqrt(mean((test[[outcome]]-pr)^2)))
  }
 }
}
fperf<-rbindlist(fperf)
# The baseline split without exposure terms must reproduce Table 3.
base<-merge(fperf[split=='Baseline' & exposure==FALSE],read_csv(file.path(results_dir,'prediction_performance.csv'))[outcome!='future_dual',.(comp,outcome,model,rmse_reported=rmse)],by=c('comp','outcome','model'))
stopifnot(nrow(base)==50,max(abs(base$rmse-base$rmse_reported))<1e-8)
put(fperf,'feedback_forecast_exposure')

# ---------------------------------------------------------------------------
# B6, B7: per-ball wicket ratios and the additive index weighting (#8, #13)
# ---------------------------------------------------------------------------
ratios<-list();index<-list()
pooled_ratios<-function(d){a<-d[role=='All-round role'];s<-d[role=='Bowling role']
 r<-function(x)c(participation=sum(x$future_bowl_matches)/sum(x$future_matches),balls_conditional=sum(x$future_ball)/sum(x$future_bowl_matches),wickets_per_ball=sum(x$future_wickets)/sum(x$future_ball),wickets_per_match=sum(x$future_wickets)/sum(x$future_matches))
 r(a)/r(s)}
index_fun<-function(d,weight){
 m<-if(weight=='Player-weighted')d[,.(b=mean(future_runs/future_matches),w=mean(future_wickets/future_matches)),by=role] else d[,.(b=sum(future_runs)/sum(future_matches),w=sum(future_wickets)/sum(future_matches)),by=role]
 if(!all(c('All-round role','Batting role','Bowling role')%in%m$role))return(c(NA_real_,NA_real_))
 c(m[role=='All-round role',b]/m[role=='Batting role',b],m[role=='All-round role',w]/m[role=='Bowling role',w])}
for(comp in names(configs)) {
 fc<-readRDS(file.path(revision_dir,'data/analysis',paste0('forecast_',comp,'.rds')));d<-fc[partition=='test' & future_matches>0]
 est<-pooled_ratios(d);teams<-unique(d$first_team)
 # Team-cluster bootstrap: resample first teams with replacement.
 bs<-t(replicate(2000,{tt<-sample(teams,length(teams),replace=TRUE);dd<-rbindlist(lapply(seq_along(tt),function(k)d[first_team==tt[k]]));z<-try(pooled_ratios(dd),silent=TRUE);if(inherits(z,'try-error')||any(!is.finite(z)))rep(NA_real_,4)else z}))
 # Leave out the dual-role player with the most later balls.
 heavy<-d[role=='All-round role'][which.max(future_ball),player]
 lo<-pooled_ratios(d[player!=heavy])
 ratios[[comp]]<-data.table(comp,measure=names(est),estimate=est,lower=apply(bs,2,quantile,.025,na.rm=TRUE),upper=apply(bs,2,quantile,.975,na.rm=TRUE),valid_replicates=colSums(is.finite(bs)),without_heaviest=lo,dual_n=sum(d$role=='All-round role'),clusters=length(teams))
 for(wt in c('Player-weighted','Appearance-pooled')) {
  comps<-index_fun(d,wt);est<-sum(comps)
  loo<-vapply(teams,function(cl)sum(index_fun(d[first_team!=cl],wt)),numeric(1))
  se<-sqrt((length(teams)-1)/length(teams)*sum((loo-mean(loo))^2));crit<-qt(.975,length(teams)-1)
  index[[length(index)+1L]]<-data.table(comp,weighting=wt,batting_ratio=comps[1],wicket_ratio=comps[2],index=est,lower=est-crit*se,upper=est+crit*se,clusters=length(teams))
 }
}
index<-rbindlist(index)
reported<-read_csv(file.path(results_dir,'additive_output_index.csv'))
chk<-merge(index[weighting=='Player-weighted'],reported[,.(comp,reported=index)],by='comp');stopifnot(max(abs(chk$index-chk$reported))<1e-10)
put(rbindlist(ratios),'feedback_wicket_ratio_intervals');put(index,'feedback_additive_index_weighting')

# ---------------------------------------------------------------------------
# B8: covariate overlap for the first-Test transition models (#3)
# ---------------------------------------------------------------------------
pm<-readRDS(file.path(revision_dir,'data/analysis/promotion.rds'));risk<-pm$risk;land<-pm$landmark
risk[,len:=tstop-tstart]
covs<-c('past_matches','bat_rate','wicket_rate')
overlap<-list()
for(sc in c('county','sheffield')) {
 r<-risk[entry_comp==sc]
 box<-sapply(covs,function(v)quantile(r[role=='Batting role'][[v]],c(.05,.95),names=FALSE))
 inside<-function(x)Reduce(`&`,lapply(covs,function(v)x[[v]]>=box[1,v] & x[[v]]<=box[2,v]))
 du<-r[role=='All-round role'];ba<-r[role=='Batting role']
 overlap[[sc]]<-data.table(entry_comp=sc,dual_person_years=sum(du$len)/365.25,dual_events=sum(du$event),dual_share_time_in_batting_support=sum(du$len[inside(du)])/sum(du$len),dual_share_events_in_batting_support=if(sum(du$event))sum(du$event[inside(du)])/sum(du$event) else NA_real_,
  batting_median_matches=median(rep(ba$past_matches,1)),dual_median_matches=median(du$past_matches),batting_median_bat_rate=median(ba$bat_rate),dual_median_bat_rate=median(du$bat_rate),batting_median_wicket_rate=median(ba$wicket_rate),dual_median_wicket_rate=median(du$wicket_rate))
}
overlap<-rbindlist(overlap)
# Refit the pooled fully adjusted model on intervals inside the batting-role support.
fit_one<-function(d)coxph(Surv(tstart,tstop,event)~role+splines::ns(log1p(past_matches),knots=log1p(c(5,30)),Boundary.knots=c(0,log1p(200)))+splines::ns(bat_rate,knots=c(20,50),Boundary.knots=c(0,150))+splines::ns(wicket_rate,knots=c(.5,1.5),Boundary.knots=c(0,10))+factor(entry_decade)+strata(entry_comp),data=d,ties='efron')
trim<-rbindlist(lapply(c('county','sheffield'),function(sc){r<-risk[entry_comp==sc];box<-sapply(covs,function(v)quantile(r[role=='Batting role'][[v]],c(.05,.95),names=FALSE));r[Reduce(`&`,lapply(covs,function(v)r[[v]]>=box[1,v] & r[[v]]<=box[2,v]))|role%in%c('Insufficient/other','Bowling role')]}))
full<-fit_one(risk);trm<-fit_one(trim);term<-'roleAll-round role'
cox_trim<-data.table(sample=c('All intervals','Batting-role support'),intervals=c(nrow(risk),nrow(trim)),events=c(sum(risk$event),sum(trim$event)),dual_events=c(sum(risk[role=='All-round role',event]),sum(trim[role=='All-round role',event])),HR=exp(c(coef(full)[term],coef(trm)[term])),
 lower_model=exp(c(confint(full)[term,1],confint(trm)[term,1])),upper_model=exp(c(confint(full)[term,2],confint(trm)[term,2])))
put(overlap,'feedback_transition_overlap');put(cox_trim,'feedback_transition_support_refit')

# ---------------------------------------------------------------------------
# B9: are wides and no-balls charged to bowlers consistently? (#12)
# ---------------------------------------------------------------------------
extras<-list()
for(comp in c('odis','t20is')) {
 x<-readRDS(file.path(revision_dir,'data/corrected',paste0('source_',comp,'.rds')))
 ids<-unique(readRDS(file.path(revision_dir,'data/analysis',paste0('player_match_',comp,'.rds')))$match_id)
 tm<-as.data.table(x$teams)[match_id%in%ids];bw<-as.data.table(x$bowling)[match_id%in%ids]
 # Bowlers of team k bowl to the opposing team (code 3-k) in the same innings.
 bi<-bw[,.(bowler_runs=sum(runs),bowler_wides=sum(wides,na.rm=TRUE),bowler_noballs=sum(noballs,na.rm=TRUE)),by=.(match_id,innings,bat_team=3L-as.integer(teams))]
 m<-merge(bi,tm[,.(match_id,innings,bat_team=as.integer(teams),start_date,runs,wides,noballs,byes,legbyes,penalties)],by=c('match_id','innings','bat_team'))
 for(v in c('wides','noballs','byes','legbyes','penalties'))set(m,which(is.na(m[[v]])),v,0)
 m[,`:=`(decade=10L*(as.integer(format(start_date,'%Y'))%/%10L),charged=bowler_runs==runs-byes-legbyes-penalties,not_charged=bowler_runs==runs-byes-legbyes-penalties-wides-noballs)]
 extras[[comp]]<-m[,.(innings=.N,innings_with_wides_or_noballs=sum(wides+noballs>0),consistent_with_charging=sum(charged),consistent_with_not_charging=sum(not_charged),only_charging=sum(charged & !not_charged),only_not_charging=sum(not_charged & !charged),neither=sum(!charged & !not_charged)),by=decade][order(decade)][,comp:=comp]
}
put(rbindlist(extras),'feedback_extras_convention')

# B9b: ODI benchmark counts with harmonised runs conceded. In innings where the
# scorecard does not charge wides and no-balls to bowlers (1970s and part of the
# 1980s), the team's wides and no-balls are allocated to its bowlers in proportion
# to balls bowled. The containment construction of 22_construct_analysis.R is then
# repeated for ODIs. With no reallocation the block reproduces the baseline counts.
x<-readRDS(file.path(revision_dir,'data/corrected/source_odis.rds'))
pm0<-readRDS(file.path(revision_dir,'data/analysis/player_match_odis.rds'));ids<-unique(pm0$match_id)
tm<-as.data.table(x$teams)[match_id%in%ids];bw<-as.data.table(x$bowling)[match_id%in%ids]
bw[,`:=`(teams=as.integer(teams),balls=overs*balls_per_over+extraballs)]
bi<-bw[,.(bowler_runs=sum(runs),tb=sum(balls)),by=.(match_id,innings,teams)]
fl<-merge(bi,tm[,.(match_id,innings,teams=3L-as.integer(teams),runs,wides,noballs,byes,legbyes,penalties)],by=c('match_id','innings','teams'))
for(v in c('wides','noballs','byes','legbyes','penalties'))set(fl,which(is.na(fl[[v]])),v,0)
fl[,uncharged:=bowler_runs!=runs-byes-legbyes-penalties & bowler_runs==runs-byes-legbyes-penalties-wides-noballs]
alloc<-merge(bw[,.(match_id,innings,teams,player=bowler,balls)],fl[,.(match_id,innings,teams,tb,extra=fifelse(uncharged,wides+noballs,0))],by=c('match_id','innings','teams'))
alloc<-alloc[,.(added=sum(fifelse(tb>0,extra*balls/tb,0))),by=.(match_id,player)]
dodi<-readRDS(file.path(revision_dir,'data/analysis/dual_odis.rds'));c0<-dodi$career
odi_counts<-function(harmonise) {
 p<-merge(copy(pm0),alloc,by=c('match_id','player'),all.x=TRUE);p[is.na(added),added:=0]
 if(harmonise)p[,runs_conceded:=runs_conceded+added]
 p[,decade:=10L*(as.integer(format(start_date,'%Y'))%/%10L)];decs<-sort(unique(p$decade))
 for(dec in decs) {
  lower<-dec;upper<-dec;refs<-p[decade==dec]
  while(uniqueN(refs$match_id)<100 && lower>min(decs)){lower<-lower-10L;refs<-p[decade>=lower & decade<=upper]}
  while(uniqueN(refs$match_id)<100 && upper<max(decs)){upper<-upper+10L;refs<-p[decade>=lower & decade<=upper]}
  econ<-function(r)r[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[['odis']],6*runs/balls]
  e<-econ(refs)
  while(length(e)<30 && lower>min(decs)){lower<-lower-10L;refs<-p[decade>=lower & decade<=upper];e<-econ(refs)}
  while(length(e)<30 && upper<max(decs)){upper<-upper+10L;refs<-p[decade>=lower & decade<=upper];e<-econ(refs)}
  e75<-as.numeric(quantile(e,.75));p[decade==dec,`:=`(economy75=e75,bowl_ref=mean(e75*refs$scorecard_balls/6-refs$runs_conceded))]
 }
 p[,y_bowl:=(economy75*scorecard_balls/6-runs_conceded)/bowl_ref]
 sw<-suff(p,'y_bowl');setkey(sw,player);c<-copy(c0);setkey(c,player);stopifnot(identical(c$player,sw$player))
 c<-merge(c,p[,.(bowl_std=mean(y_bowl)),by=player],by='player')
 fw<-fit_reml(sw[which(c$prior_eligible)]);pw<-posterior(sw,fw)
 wf<-safe_q(c[role=='Bowling role',bowl_std]);u<-c$prob_bat;v<-pnorm((pw$mean-wf)/pw$sd);ok<-c$evaluable
 data.table(comp='odis',runs_conceded=if(harmonise)'Harmonised' else 'As recorded',innings_reallocated=sum(fl$uncharged),runs_reallocated=sum(alloc$added),
  independent=sum(ok & u*v>=.5),all_couplings=sum(ok & pmax(0,u+v-1)>=.5),some_coupling=sum(ok & pmin(u,v)>=.5),expected=sum((u*v)[ok]))
}
oc<-rbind(odi_counts(FALSE),odi_counts(TRUE))
base<-read_csv(file.path(results_dir,'dual_counts.csv'))[comp=='odis' & floor_quantile==.5]
stopifnot(oc$independent[1]==base$independent,oc$all_couplings[1]==base$frechet_certain,oc$some_coupling[1]==base$frechet_possible)
put(oc,'feedback_odi_extras_harmonised')
cat('Feedback robustness checks complete\n')

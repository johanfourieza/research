source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('output uncertainty')
countries<-c('Afghanistan','Australia','Bangladesh','England','India','Ireland','New Zealand','Pakistan','South Africa','Sri Lanka','West Indies','Zimbabwe')
out<-list();refs<-list()
for(comp in c('odis','t20is'))for(pool in c('All countries','Fixed twelve-country subset'))for(q in c(.65,.75,.85)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')));p<-a$pm
 if(pool!='All countries')p<-p[team%in%countries]
 b<-merge(a$batting,p[,.(player,match_id)],by=c('player','match_id'))
 c<-career_summary(p,b);c[,role:=roles(.SD,bmin=ball_min[[comp]])];c[,`:=`(evaluable=innings_batted>=20 &(wickets>=20|scorecard_balls>=500),prior_eligible=innings_batted>=20|scorecard_balls>=ball_min[[comp]])]
 decs<-sort(unique(p$decade))
 for(dec in decs) {
  lo<-hi<-dec;z<-p[decade==dec]
  ec<-z[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]
  while((uniqueN(z$match_id)<100|length(ec)<30)&lo>min(decs)){lo<-lo-10;z<-p[decade>=lo&decade<=hi];ec<-z[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]}
  while((uniqueN(z$match_id)<100|length(ec)<30)&hi<max(decs)){hi<-hi+10;z<-p[decade>=lo&decade<=hi];ec<-z[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]}
  e<-as.numeric(quantile(ec,q));mb<-mean(z$bat_runs);mw<-mean(e*z$scorecard_balls/6-z$runs_conceded)
  stopifnot(mw>0,mb>0)
  p[decade==dec,`:=`(y_bat=bat_runs/mb,y_bowl=(e*scorecard_balls/6-runs_conceded)/mw,bowl_value=e*scorecard_balls/6-runs_conceded)]
  refs[[length(refs)+1L]]<-data.table(comp,pool,percentile=q,decade=dec,lo,hi,e,players=length(ec))
 }
 means<-p[,.(bat_standardised=mean(y_bat),bowl_standardised=mean(y_bowl)),by=player];c<-merge(c,means,by='player')
 sb<-suff(p,'y_bat');sw<-suff(p,'y_bowl');setkey(c,player);setkey(sb,player);setkey(sw,player)
 fb<-fit_reml(sb[c$prior_eligible]);fw<-fit_reml(sw[c$prior_eligible]);pb<-posterior(sb,fb);pw<-posterior(sw,fw)
 bf<-safe_q(c[role=='Batting role',bat_standardised]);wf<-safe_q(c[role=='Bowling role',bowl_standardised]);u<-pnorm((pb$mean-bf)/pb$sd);v<-pnorm((pw$mean-wf)/pw$sd)
 out[[length(out)+1L]]<-data.table(comp,pool,percentile=q,players=nrow(c),evaluable=sum(c$evaluable),workload=sum(c$role=='All-round role'),independent=sum(c$evaluable &u*v>=.5),certain=sum(c$evaluable&pmax(0,u+v-1)>=.5),possible=sum(c$evaluable&pmin(u,v)>=.5),expected=sum((u*v)[c$evaluable]),bat_floor=bf,bowl_floor=wf)
 cat(comp,pool,q,'recomputed\n')
}
put(rbindlist(out),'whiteball_reference_sensitivity');put(rbindlist(refs),'whiteball_reference_cells')

source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('verified scorecard output')
reference<-list();samples<-list();zeroes<-list();comparators<-list();algebra<-list()
for(comp in names(configs)) {
 p<-readRDS(file.path(revision_dir,'data/analysis',paste0('player_match_',comp,'.rds')))
 x<-readRDS(file.path(revision_dir,'data/corrected',paste0('source_',comp,'.rds')))
 b<-x$batting[,.(match_id,player=batter,actual_position=player)]
 b<-merge(b,p[,.(match_id,player)],by=c('match_id','player'))
 c<-career_summary(p,b)
 c[,role:=roles(.SD,bmin=ball_min[[comp]])]
 c[,evaluable:=innings_batted>=20 & (wickets>=20|scorecard_balls>=500)]
 c[,prior_eligible:=innings_batted>=20|scorecard_balls>=ball_min[[comp]]]
 c[,sadekar_role:=matches>=25 & top7_innings/matches>=.5 & bowl_matches/matches>=.5]
 p[,year:=as.integer(format(start_date,'%Y'))];p[,decade:=10L*(year%/%10L)]
 decs<-sort(unique(p$decade));long<-comp%in%c('tests','county','sheffield')
 for(dec in decs) {
  refs<-p[decade==dec];lower<-dec;upper<-dec
  while(uniqueN(refs$match_id)<100 && lower>min(decs)){lower<-lower-10L;refs<-p[decade>=lower & decade<=upper]}
  while(uniqueN(refs$match_id)<100 && upper<max(decs)){upper<-upper+10L;refs<-p[decade>=lower & decade<=upper]}
  # Economy reference from bowlers with a format-specific exposure minimum.
  econ<-refs[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]
  while(!long & length(econ)<30 && lower>min(decs)) {
   lower<-lower-10L;refs<-p[decade>=lower & decade<=upper]
   econ<-refs[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]
  }
  if(!long && length(econ)<30) {
   while(length(econ)<30 && upper<max(decs)){upper<-upper+10L;refs<-p[decade>=lower & decade<=upper];econ<-refs[,.(balls=sum(scorecard_balls),runs=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*runs/balls]}
  }
  price<-sum(refs$runs_conceded)/sum(refs$wickets)
  eq<-if(long)rep(NA_real_,3)else as.numeric(quantile(econ,c(.65,.75,.85)))
  mb<-mean(refs$bat_runs);mw<-mean(refs$wickets)
  mv<-if(long)price*mw else mean(eq[2]*refs$scorecard_balls/6-refs$runs_conceded)
  stopifnot(price>0,mb>0,mw>0,mv>0)
  p[decade==dec,`:=`(wicket_price=price,economy65=eq[1],economy75=eq[2],economy85=eq[3],bat_ref=mb,wicket_ref=mw,bowl_ref=mv)]
  reference[[length(reference)+1L]]<-data.table(comp,decade=dec,reference_start=lower,reference_end=upper+9,matches=uniqueN(refs$match_id),economy_bowlers=length(econ),wicket_price=price,economy65=eq[1],economy75=eq[2],economy85=eq[3],bat_ref=mb,bowl_ref=mv,wicket_ref=mw)
 }
 p[,bowl_value:=if(long)wickets*wicket_price else economy75*scorecard_balls/6-runs_conceded]
 p[,`:=`(y_bat=bat_runs/bat_ref,y_bowl=bowl_value/bowl_ref,y_wickets=wickets/wicket_ref)]
 vals<-p[,.(bowling_volume=sum(bowl_value),bat_standardised=mean(y_bat),bowl_standardised=mean(y_bowl),wickets_standardised=mean(y_wickets),
  low_economy_volume=if(long)sum(bowl_value)else sum(economy65*scorecard_balls/6-runs_conceded),high_economy_volume=if(long)sum(bowl_value)else sum(economy85*scorecard_balls/6-runs_conceded)),by=player]
 c<-merge(c,vals,by='player');c[,batting_share:=fifelse(bat_runs+pmax(0,bowling_volume)>0,bat_runs/(bat_runs+pmax(0,bowling_volume)),NA_real_)]
 S<-sum(c$bat_runs)/(sum(c$bat_runs)+sum(pmax(0,c$bowling_volume)))
 c[,`:=`(pool_batting_share=S,rca_bat=batting_share/S,rca_bowl=(1-batting_share)/(1-S),sym_bat=(batting_share-S)/(batting_share+S))]
 c[,sym_bowl:=(rca_bowl-1)/(rca_bowl+1)]
 c[,legacy_balanced:=!is.na(sym_bat)&abs(sym_bat)<=.25]
 c[,both_bands:=legacy_balanced & abs(sym_bowl)<=.25]
 c[,bowl_zero_reason:=fcase(scorecard_balls==0 & runs_conceded==0,'No recorded bowling',bowling_volume<=0,'Bowled; net nonpositive',default='Positive bowling output')]
 zeroes[[comp]]<-cbind(comp,c[,.(players=.N,evaluable=sum(evaluable)),by=bowl_zero_reason])
 samples[[comp]]<-data.table(comp,first_year=min(p$year),last_year=max(p$year),matches=uniqueN(p$match_id),players=nrow(c),appearances=nrow(p),evaluable=sum(c$evaluable),batting_role=sum(c$role=='Batting role'),allround_role=sum(c$role=='All-round role'),bowling_role=sum(c$role=='Bowling role'),other=sum(c$role=='Insufficient/other'),S=S)
 comparators[[comp]]<-data.table(comp,traditional=sum(c$traditional_positive & c$evaluable),workload=sum(c$role=='All-round role' & c$evaluable),sadekar=sum(c$sadekar_role),sadekar_workload_overlap=sum(c$sadekar_role & c$role=='All-round role'),traditional_workload_overlap=sum(c$traditional_positive & c$role=='All-round role' & c$evaluable))
 algebra[[comp]]<-data.table(comp,S=S,legacy_lower=S*.75/1.25,legacy_upper=min(1,S*1.25/.75),max_share_roundtrip_error=max(abs(c$batting_share-c$rca_bat*S),na.rm=TRUE),zero_volume=sum(is.na(c$batting_share)),symmetric_zero_meaning='player share equals pooled share')
 saveRDS(list(pm=p,batting=b,career=c),file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')))
 cat(comp,nrow(c),'careers constructed\n')
}
put(rbindlist(reference),'reference_prices');put(rbindlist(samples),'sample');put(rbindlist(zeroes),'zero_mass');put(rbindlist(comparators),'classification_comparators');put(rbindlist(algebra),'share_algebra')
cat('Verified composition identities and completed descriptive construction\n')

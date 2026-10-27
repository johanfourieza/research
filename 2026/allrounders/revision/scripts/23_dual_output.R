source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('output uncertainty')
fits<-list();counts<-list();deps<-list();grid<-list();boots<-list();native<-list();groupchecks<-list()
for(comp in names(configs)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')));p<-a$pm;c<-a$career
 sb<-suff(p,'y_bat');sw<-suff(p,'y_bowl');sn<-suff(p,'y_wickets')
 setkey(sb,player);setkey(sw,player);setkey(sn,player);setkey(c,player)
 stopifnot(identical(c$player,sb$player),identical(c$player,sw$player))
 ib<-which(c$prior_eligible);fb<-fit_reml(sb[ib]);fw<-fit_reml(sw[ib]);fn<-fit_reml(sn[ib])
 for(item in c('bat','bowl','native')){f<-switch(item,bat=fb,bowl=fw,native=fn);fits[[length(fits)+1L]]<-data.table(comp,discipline=item,mu=f$mu,sigma2=f$sigma2,tau2=f$tau2,convergence=f$convergence,prior_players=length(ib))}
 pb<-posterior(sb,fb);pw<-posterior(sw,fw);pn<-posterior(sn,fn)
 c[,`:=`(post_bat=pb$mean,sd_bat=pb$sd,post_bowl=pw$mean,sd_bowl=pw$sd)]
 for(q in c(.5,.75)) {
  bf<-safe_q(c[role=='Batting role',bat_standardised],q);wf<-safe_q(c[role=='Bowling role',bowl_standardised],q);nf<-safe_q(c[role=='Bowling role',wickets_standardised],q)
  stopifnot(is.finite(bf),is.finite(wf))
  u<-pnorm((pb$mean-bf)/pb$sd);v<-pnorm((pw$mean-wf)/pw$sd);vn<-pnorm((pn$mean-nf)/pn$sd);ok<-c$evaluable
  lower<-pmax(0,u+v-1);upper<-pmin(u,v)
  for(rho in c(-.75,-.5,-.25,0,.25,.5,.75)) {
   pr<-copula_joint(u,v,rho)
   deps[[length(deps)+1L]]<-data.table(comp,floor_quantile=q,rho,eligible=sum(ok),classified=sum(pr[ok]>=.5),expected_count=sum(pr[ok]))
  }
  counts[[length(counts)+1L]]<-data.table(comp,floor_quantile=q,bat_floor=bf,bowl_floor=wf,eligible=sum(ok),independent=sum((u*v)[ok]>=.5),expected_independent=sum((u*v)[ok]),frechet_certain=sum(lower[ok]>=.5),frechet_possible=sum(upper[ok]>=.5),expected_lower=sum(lower[ok]),expected_upper=sum(upper[ok]),raw_both=sum(c$bat_standardised[ok]>=bf & c$bowl_standardised[ok]>=wf),traditional=sum(c$traditional_positive[ok]))
  native[[length(native)+1L]]<-data.table(comp,floor_quantile=q,native_wicket_floor=nf,independent=sum((u*vn)[ok]>=.5),certain=sum(pmax(0,u+vn-1)[ok]>=.5),possible=sum(pmin(u,vn)[ok]>=.5))
  if(q==.5)c[,`:=`(prob_bat=u,prob_bowl=v,prob_independent=u*v,prob_lower=lower,prob_upper=upper)]
 }
 # 108 distinct five-axis combinations; repeat with absence-in-2025 convention:
 # 216 cells in total, with the convention explicitly visible.
 for(activity in c('All observed','Absent in 2025'))for(im in c(15,20,25))for(bm in ball_min[[comp]]+c(-1,0,1)*ball_step[[comp]]) {
  cc<-copy(c);active<-if(activity=='All observed')rep(TRUE,nrow(cc))else cc$last_date<as.IDate('2025-01-01')
  prior<-(cc$innings_batted>=im | cc$scorecard_balls>=bm)&active
  fbg<-fit_reml(sb[prior]);fwg<-fit_reml(sw[prior]);pbg<-posterior(sb,fbg);pwg<-posterior(sw,fwg)
  for(pos in c('actual_modal_position','nominal_modal_position')) {
   rr<-roles(cc,im,bm,pos);eligible<-active & cc$innings_batted>=im &(cc$wickets>=20|cc$scorecard_balls>=500)
   for(q in c(.5,.75)) {
    bf<-safe_q(cc[active & rr=='Batting role',bat_standardised],q);wf<-safe_q(cc[active & rr=='Bowling role',bowl_standardised],q)
    u<-pnorm((pbg$mean-bf)/pbg$sd);v<-pnorm((pwg$mean-wf)/pwg$sd)
    for(pc in c(.3,.5,.7))grid[[length(grid)+1L]]<-data.table(comp,activity,innings_min=im,balls_min=bm,position=pos,floor_quantile=q,probability_cutoff=pc,eligible=sum(eligible),workload=sum(active & rr=='All-round role'),independent=sum(eligible & u*v>=pc,na.rm=TRUE),certain=sum(eligible & pmax(0,u+v-1)>=pc,na.rm=TRUE),possible=sum(eligible & pmin(u,v)>=pc,na.rm=TRUE),with_legacy_band=sum(eligible & u*v>=pc & cc$legacy_balanced,na.rm=TRUE),with_both_bands=sum(eligible & u*v>=pc & cc$both_bands,na.rm=TRUE),bat_floor=bf,bowl_floor=wf)
   }
  }
 }
 # Heterogeneous priors: refit within workload roles where at least 30 players.
 uu<-vv<-rep(NA_real_,nrow(c));bf<-safe_q(c[role=='Batting role',bat_standardised]);wf<-safe_q(c[role=='Bowling role',bowl_standardised])
 for(rr in levels(c$role)) {
  ii<-which(c$role==rr & c$prior_eligible);jj<-which(c$role==rr)
  if(length(ii)>=30){ppb<-posterior(sb[jj],fit_reml(sb[ii]));ppw<-posterior(sw[jj],fit_reml(sw[ii]));uu[jj]<-pnorm((ppb$mean-bf)/ppb$sd);vv[jj]<-pnorm((ppw$mean-wf)/ppw$sd)}
 }
 groupchecks[[comp]]<-data.table(comp,eligible=sum(c$evaluable),covered=sum(c$evaluable & !is.na(uu*vv)),independent=sum(c$evaluable & uu*vv>=.5,na.rm=TRUE))
 # Joint player bootstrap propagates specialist-floor and prior uncertainty.
 # Reference decade prices/scales remain fixed; report that conditioning explicitly.
 for(bb in 1:200) {
  ww<-tabulate(sample.int(nrow(c),nrow(c),replace=TRUE),nbins=nrow(c))
  fbb<-fit_reml(sb[ib],ww[ib]);fww<-fit_reml(sw[ib],ww[ib]);ppb<-posterior(sb,fbb);ppw<-posterior(sw,fww)
  ix<-rep(seq_len(nrow(c)),ww);bf<-safe_q(c$bat_standardised[ix[c$role[ix]=='Batting role']]);wf<-safe_q(c$bowl_standardised[ix[c$role[ix]=='Bowling role']])
  u<-pnorm((ppb$mean-bf)/ppb$sd);v<-pnorm((ppw$mean-wf)/ppw$sd);ok<-c$evaluable
  boots[[length(boots)+1L]]<-data.table(comp,replicate=bb,bat_floor=bf,bowl_floor=wf,eligible=sum(ww[ok]),independent=sum(ww[ok]*(u*v>=.5)[ok]),expected=sum(ww[ok]*(u*v)[ok]),certain=sum(ww[ok]*(pmax(0,u+v-1)>=.5)[ok]),possible=sum(ww[ok]*(pmin(u,v)>=.5)[ok]),converged=fbb$convergence==0 & fww$convergence==0)
 }
 saveRDS(list(career=c,bat_suff=sb,bowl_suff=sw,bat_fit=fb,bowl_fit=fw),file.path(revision_dir,'data/analysis',paste0('dual_',comp,'.rds')))
 cat(comp,'posterior, 216-cell grid and 200 bootstrap replications complete\n')
}
put(rbindlist(fits),'eb_fits');put(rbindlist(counts),'dual_counts');put(rbindlist(deps),'dependence_sensitivity');put(rbindlist(grid),'threshold_grid');put(rbindlist(boots),'dual_bootstrap');put(rbindlist(native),'native_wicket_sensitivity');put(rbindlist(groupchecks),'role_prior_sensitivity')
# Code-level checks against independent probability and mixed-model implementations.
test<-CJ(p=c(.1,.5,.9),q=c(.1,.5,.9),rho=c(-.75,0,.75));test[,ours:=copula_joint(p,q,rho),by=rho]
test[,reference:=vapply(seq_len(.N),function(i)as.numeric(mvtnorm::pmvnorm(upper=qnorm(c(p[i],q[i])),corr=matrix(c(1,rho[i],rho[i],1),2))),numeric(1))]
stopifnot(max(abs(test$ours-test$reference))<1e-7)
put(test,'copula_independent_checks')
cat('All output models complete\n')

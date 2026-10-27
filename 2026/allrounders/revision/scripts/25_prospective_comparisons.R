source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('chronologically held-out prediction')
performance<-list();coverage<-list();gaps<-list();margins<-list();coefrows<-list();checks<-list()
auc<-function(y,p){if(length(unique(y))<2)return(NA_real_);r<-rank(p);(sum(r[y==1])-sum(y==1)*(sum(y==1)+1)/2)/(sum(y==1)*sum(y==0))}
for(comp in names(configs)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',comp,'.rds')));p<-a$pm;b<-a$batting
 setorder(p,player,start_date,match_id);p[,seq30:=seq_len(.N),by=player]
 land<-p[seq30==30,.(player,landmark=end_date)]
 land[,horizon:=as.IDate(vapply(as.Date(landmark),function(d)as.numeric(seq(as.Date(d,origin='1970-01-01'),by='5 years',length.out=2)[2]),numeric(1)),origin='1970-01-01')]
 land<-land[horizon<=cutoff]
 pp<-merge(p,land,by='player');early<-pp[seq30<=30];future<-pp[start_date>landmark & end_date<=horizon]
 # All first 30 match results must be complete at the landmark; verify overlaps.
 stopifnot(all(early$end_date<=early$landmark))
 eb<-merge(b,early[,.(match_id,player)],by=c('match_id','player'))
 c<-career_summary(early,eb);c<-merge(c,land,by='player')
 c[,role:=roles(.SD,bmin=ball_min[[comp]])]
 ff<-future[,.(future_matches=.N,future_runs=sum(bat_runs),future_wickets=sum(wickets),future_ball=sum(scorecard_balls),future_bowl_matches=sum(bowling_row_present),future_innings=sum(innings_batted),future_dismissals=sum(dismissals),future_conceded=sum(runs_conceded)),by=player]
 c<-merge(c,ff,by='player',all.x=TRUE)
 for(v in names(ff)[-1])set(c,which(is.na(c[[v]])),v,0)
 c[,`:=`(future_bat_annual=future_runs/5,future_bowl_annual=future_wickets/5,index_year=as.integer(format(landmark,'%Y')))]
 # Currency references precede the landmark calendar year, with no future records.
 for(yy in sort(unique(c$index_year))) {
  lim<-as.IDate(paste0(yy,'-01-01'));hist<-p[end_date<lim & start_date>=as.IDate(paste0(yy-5,'-01-01'))]
  if(uniqueN(hist$match_id)<100)hist<-p[end_date<lim & start_date>=as.IDate(paste0(yy-10,'-01-01'))]
  if(uniqueN(hist$match_id)<100)hist<-p[end_date<lim]
  price<-if(uniqueN(hist$match_id)>=20)sum(hist$runs_conceded)/sum(hist$wickets)else NA_real_
  ec<-hist[,.(balls=sum(scorecard_balls),r=sum(runs_conceded)),by=player][balls>=ball_min[[comp]],6*r/balls]
  e<-if(length(ec)>0 && uniqueN(hist$match_id)>=20)as.numeric(quantile(ec,.75))else NA_real_
  c[index_year==yy,`:=`(prior_price=price,prior_economy=e,reference_matches=uniqueN(hist$match_id),reference_bowlers=length(ec),reference_last_date=max(hist$end_date))]
 }
 long<-comp%in%c('tests','county','sheffield')
 c[,volume_bowl:=if(long)prior_price*wickets else pmax(0,prior_economy*scorecard_balls/6-runs_conceded)]
 c[,`:=`(share=bat_runs/(bat_runs+volume_bowl),log_volume=log1p((bat_runs+volume_bowl)/matches),bowling_participation=bowl_matches/matches,bat_signal=bat_runs/matches,bowl_signal=wickets/matches,traditional_diff=bat_average-bowling_average)]
 c<-c[is.finite(share)&is.finite(log_volume)]
 splitdate<-as.IDate(if(comp=='t20is')'2019-01-01'else '2010-01-01')
 c[,partition:=fcase(horizon<splitdate,'training',landmark>=splitdate,'test',default='embargo')]
 train<-c[partition=='training'];test<-c[partition=='test']
 coverage[[comp]]<-data.table(comp,landmarks=nrow(land),usable=nrow(c),train=nrow(train),embargo=sum(c$partition=='embargo'),test=nrow(test),train_last_outcome=if(nrow(train))max(train$horizon)else as.IDate(NA),test_first_landmark=if(nrow(test))min(test$landmark)else as.IDate(NA),test_zero_future=sum(test$future_matches==0),minimum_reference_matches=min(c$reference_matches))
 if(nrow(train)<40 || nrow(test)<30){cat(comp,'insufficient prospective training/test sample\n');next}
 stopifnot(max(train$horizon)<min(test$landmark),all(c$reference_last_date<c$landmark))
 S<-sum(train$bat_runs)/(sum(train$bat_runs)+sum(train$volume_bowl))
 med<-median(train$traditional_diff,na.rm=TRUE)
 c[,`:=`(rca=share/S,average_missing=as.integer(!is.finite(traditional_diff)))]
 c[!is.finite(traditional_diff),traditional_diff:=med]
 c[,role:=factor(role,levels=levels(a$career$role))]
 train<-c[partition=='training'];test<-c[partition=='test']
 # Share and RCA use the same spline basis: transform knots/boundaries with S.
 # Mass at share=1 makes quantile knots degenerate in short formats. Fixed
 # interior fractions of the training support preserve matched flexibility.
 bd<-range(train$share);kn<-bd[1]+diff(bd)*c(1/3,2/3)
 forms<-list(workload='role',traditional='role + traditional_diff + average_missing',share='role + log_volume + splines::ns(share, knots=kn, Boundary.knots=bd)',rca='role + log_volume + splines::ns(rca, knots=kn/S, Boundary.knots=bd/S)',continuous='role + splines::ns(bat_signal,df=3) + splines::ns(bowl_signal,df=3) + bowling_participation')
 for(outcome in c('future_bat_annual','future_bowl_annual')) {
  preds<-list()
  for(model in names(forms)) {
   fit<-lm(as.formula(paste(outcome,'~',forms[[model]])),data=train)
   pr<-as.numeric(predict(fit,newdata=test));preds[[model]]<-pr
   err<-(test[[outcome]]-pr)^2
   performance[[length(performance)+1L]]<-data.table(comp,outcome,model,n_train=nrow(train),n_test=nrow(test),rmse=sqrt(mean(err)),mae=mean(abs(test[[outcome]]-pr)),brier=NA_real_,auc=NA_real_)
  }
  checks[[length(checks)+1L]]<-data.table(comp,outcome,max_share_rca_prediction_difference=max(abs(preds$share-preds$rca)))
  stopifnot(max(abs(preds$share-preds$rca))<1e-6)
 }
 # Prospective dual-output target: training-only specialist annual-output floors.
 bf<-safe_q(train[role=='Batting role',future_bat_annual]);wf<-safe_q(train[role=='Bowling role',future_bowl_annual])
 if(is.finite(bf)&is.finite(wf)&bf>0&wf>0) {
  train[,future_dual:=as.integer(future_bat_annual>=bf & future_bowl_annual>=wf)];test[,future_dual:=as.integer(future_bat_annual>=bf & future_bowl_annual>=wf)]
  for(model in names(forms)) {
   # Fixed ridge penalty prevents separation in sparse dual-output targets.
   # Standardisation and spline bases are learned on training records alone.
   design<-lm(as.formula(paste('future_dual ~',forms[[model]])),data=train)
   X<-model.matrix(design);Xt<-model.matrix(delete.response(terms(design)),data=test,contrasts.arg=design$contrasts,xlev=design$xlevels)
   centre<-colMeans(X);scale<-apply(X,2,sd);centre[1]<-0;scale[1]<-1;scale[!is.finite(scale)|scale==0]<-1
   X<-sweep(sweep(X,2,centre),2,scale,'/');Xt<-sweep(sweep(Xt,2,centre),2,scale,'/')
   loss<-function(beta){eta<-as.vector(X%*%beta);sum(pmax(eta,0)+log1p(exp(-abs(eta)))-train$future_dual*eta)+.5*sum(beta[-1]^2)}
   grad<-function(beta){g<-as.vector(crossprod(X,plogis(X%*%beta)-train$future_dual));g[-1]<-g[-1]+beta[-1];g}
   fit<-optim(rep(0,ncol(X)),loss,grad,method='BFGS',control=list(maxit=1000,reltol=1e-10));stopifnot(fit$convergence==0)
   pr<-pmin(1-1e-8,pmax(1e-8,plogis(as.vector(Xt%*%fit$par))))
   performance[[length(performance)+1L]]<-data.table(comp,outcome='future_dual',model,n_train=nrow(train),n_test=nrow(test),rmse=NA_real_,mae=NA_real_,brier=mean((test$future_dual-pr)^2),auc=auc(test$future_dual,pr),bat_floor=bf,bowl_floor=wf)
  }
 }
 # Descriptive role contrasts measured only in held-out careers with later play.
 test[,`:=`(bat_future_per_match=fifelse(future_matches>0,future_runs/future_matches,NA_real_),bowl_future_per_match=fifelse(future_matches>0,future_wickets/future_matches,NA_real_),index_decade=10L*(index_year%/%10L))]
 for(disc in c('bat','bowl')) {
  reference_role<-if(disc=='bat')'Batting role'else'Bowling role';outcome<-paste0(disc,'_future_per_match')
  dd<-test[role%in%c('All-round role',reference_role)&future_matches>0];dd[,ar:=as.integer(role=='All-round role')]
  if(sum(dd$ar)<5 || sum(dd$ar==0)<5)next
  fit<-lm(as.formula(paste(outcome,'~ ar + factor(index_decade)')),data=dd)
  ct<-as.data.table(clubSandwich::coef_test(fit,vcov='CR2',cluster=dd$first_team,test='Satterthwaite'),keep.rownames='term')
  row<-ct[Coef=='ar'];crit<-qt(.975,row$df_Satt)
  gaps[[length(gaps)+1L]]<-data.table(comp,discipline=disc,n=nrow(dd),allround_n=sum(dd$ar),reference_n=sum(dd$ar==0),clusters=uniqueN(dd$first_team),difference=row$beta,se=row$SE,df=row$df_Satt,lower=row$beta-crit*row$SE,upper=row$beta+crit*row$SE,p=row$p_Satt,allround_mean=mean(dd[ar==1][[outcome]]),specialist_mean=mean(dd[ar==0][[outcome]]))
 }
 for(rr in c('All-round role','Bowling role','Batting role')) {
  dd<-test[role==rr&future_matches>0]
  # Common aggregate weights make the three bowling margins multiply exactly.
  margins[[length(margins)+1L]]<-data.table(comp,role=rr,n=nrow(dd),bat_per_match=sum(dd$future_runs)/sum(dd$future_matches),bat_per_innings=sum(dd$future_runs)/sum(dd$future_innings),wickets_per_match=sum(dd$future_wickets)/sum(dd$future_matches),bowling_participation=sum(dd$future_bowl_matches)/sum(dd$future_matches),balls_conditional=sum(dd$future_ball)/sum(dd$future_bowl_matches),wickets_per_six_balls=6*sum(dd$future_wickets)/sum(dd$future_ball),economy=6*sum(dd$future_conceded)/sum(dd$future_ball))
 }
 saveRDS(c,file.path(revision_dir,'data/analysis',paste0('forecast_',comp,'.rds')))
 cat(comp,nrow(train),'training;',nrow(test),'test; forecasts and role contrasts complete\n')
}
g<-rbindlist(gaps,fill=TRUE);g[,p_holm_primary:=p.adjust(p,'holm',n=10)]
put(rbindlist(performance,fill=TRUE),'prediction_performance');put(rbindlist(coverage),'prediction_coverage');put(g,'prospective_output_gaps');put(rbindlist(margins),'bowling_margin_decomposition');put(rbindlist(checks),'share_rca_prediction_equivalence')

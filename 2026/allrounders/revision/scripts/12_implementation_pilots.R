# Implementation validation only. The user declined independent human role labels.
# No new empirical accuracy claim, fitted replacement rates, regressions or substantive estimates.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
frames<-list();results<-list();identities<-list();empty<-list()
independent_mode<-function(v) {
 v<-sort(v[!is.na(v)]);if(!length(v))return(NA_integer_)
 distinct<-unique(v);counts<-vapply(distinct,function(a)sum(v==a),integer(1))
 best<-distinct[counts==max(counts)];distance<-abs(best-stats::median(v))
 min(best[distance==min(distance)])
}
for(co in names(configs)) {
 x<-readRDS(file.path(revision_dir,"data/corrected",paste0("source_",co,".rds")))
 pm<-readRDS(file.path(revision_dir,"data/corrected",paste0("player_match_",co,".rds")))
 ca<-read_csv(file.path(revision_dir,"data/corrected",paste0("careers_",co,".csv")))
 minimum<-switch(co,odis=1000,t20is=500,2000)
 # Sample on input support and exposure boundaries, before any revised high-output outcome exists.
 keepers<-unique(pm[wicketkeeper_nominated==TRUE,player]);mixed<-unique(pm[bowling_style%in%c("mixed","unknown"),player])
 pools<-list(boundary=ca[actual_modal_position%in%6:8 | innings_batted%between%c(18,22) | legal_balls%between%c(.9*minimum,1.1*minimum),player],
  keeper=intersect(ca$player,keepers),mixed_or_unknown=intersect(ca$player,mixed),recent=ca[last_date>=as.IDate("2024-01-01"),player],random=ca$player)
 wanted<-c(boundary=4L,keeper=2L,mixed_or_unknown=2L,recent=2L,random=2L)
 chosen<-character();selected<-list()
 for(stratum in names(pools)) {
  pool<-sort(setdiff(pools[[stratum]],chosen));n<-min(wanted[[stratum]],length(pool));take<-if(n)pool[sample.int(length(pool),n)]else character()
  chosen<-c(chosen,take);if(n)selected[[stratum]]<-data.table(comp=co,player=take,stratum)
 }
 frame<-rbindlist(selected);frames[[co]]<-frame
 fwrite(frame,file.path(revision_dir,"config",paste0("implementation_pilot_",co,".csv")))
 for(who in chosen) {
  # Deliberately recompute with direct raw row selection and base R, independently of grouped rebuild.
  b<-as.data.frame(x$batting[x$batting$batter==who]);w<-as.data.frame(x$bowling[x$bowling$bowler==who]);p<-as.data.frame(x$players[x$players$player==who])
  m<-independent_mode(b$player);balls<-sum(w$overs*w$balls_per_over+w$extraballs)
  raw<-c(matches=nrow(p),bat_runs=sum(b$score),innings_batted=nrow(b),dismissals=sum(b$out),wickets=sum(w$wickets),legal_balls=balls,runs_conceded=sum(w$runs),actual_modal_position=m)
  reconstructed<-unlist(ca[player==who,names(raw),with=FALSE],use.names=TRUE)
  equal<-(is.na(raw)&is.na(reconstructed))|(!is.na(raw)&!is.na(reconstructed)&raw==reconstructed)
  role<-nrow(b)>=20L && !is.na(m) && m<=7L && balls>=minimum
  role_rebuilt<-ca[player==who,innings_batted>=20L & !is.na(actual_modal_position) & actual_modal_position<=7L & legal_balls>=minimum]
  stopifnot(all(equal),identical(as.logical(role),as.logical(role_rebuilt)))
  results[[length(results)+1L]]<-data.table(comp=co,player=who,field=names(raw),direct=unname(raw),rebuilt=as.numeric(reconstructed),agree=equal,
    direct_workload_role=role,rebuilt_workload_role=role_rebuilt,validation="independent_code_path_only",human_role_label=NA_character_)
 }
 # Empty source innings slots are structural when team_batted is false. Retain anomalous played-zero slots for review.
 ac<-read_csv(file.path(revision_dir,"audit/execution/corrected_innings_accounting.csv"))[comp==co & empty_zero_record==TRUE]
 empty[[co]]<-ac[,.(records=.N),by=.(comp,team_batted,declared,wickets,runs)]
 if(co%in%c("odis","t20is"))for(ref in c(4,5,6)) {
  # Fixed arbitrary audit constants, NOT fitted replacement prices or estimated results.
  direct<-x$bowling[,.(signed=sum(ref*(overs*balls_per_over+extraballs)/6-runs)),by=.(player=bowler)]
  aggregate<-pm[,.(signed_from_components=ref*sum(legal_balls)/6-sum(runs_conceded)),by=player]
  joined<-merge(direct,aggregate,by="player");gap<-abs(joined$signed-joined$signed_from_components)
  stopifnot(all(gap<1e-7))
  identities[[length(identities)+1L]]<-data.table(comp=co,arbitrary_reference=ref,careers=nrow(joined),maximum_error=max(gap),pass=TRUE)
 }
 cat(co,"12-career independent implementation pilot passed\n")
}
# Edge cases establish clipping order and zero-ball behavior without observing any study outcome.
toy<-data.table(balls=c(6,6,0),conceded=c(0,10,2));signed<-5*toy$balls/6-toy$conceded
stopifnot(sum(signed)==-2,pmax(0,sum(signed))==0,sum(pmax(0,signed))==5,signed[3]==-2)
stopifnot(independent_mode(c(6L,8L))==6L,independent_mode(c(6L,8L,8L,6L,9L))==8L,is.na(independent_mode(integer())))
write_audit(rbindlist(frames),"implementation_pilot_frame")
write_audit(rbindlist(results),"implementation_pilot_results")
write_audit(rbindlist(identities),"additive_identity_checks")
write_audit(rbindlist(empty),"empty_innings_slot_checks")
cat("60 careers validated mechanically; zero human labels collected. This pilot does not alter the data gate.\n")

# Provisional corrected inputs. This is an auditable reconstruction, not permission
# to estimate substantive models. Analysis scripts must check data_gate.json.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
setDTthreads(2)
pilot_queries<-rbindlist(lapply(c("batting","bowling"),function(kind) {
 f<-file.path(revision_dir,"audit/execution",paste0("corrected_cricsheet_",kind,"_comparison.csv"))
 if(file.exists(f))read_csv(f)[status!="agree",.(comp,match_id)]else data.table(comp=character(),match_id=character())
}))
registry_path<-file.path(revision_dir,"audit/execution/registry_collision_queries.csv")
registry_keys<-if(file.exists(registry_path))read_csv(registry_path)$player else character()
split_path<-file.path(revision_dir,"audit/execution/registry_split_queries.csv")
if(file.exists(split_path))registry_keys<-unique(c(registry_keys,unlist(strsplit(read_csv(split_path)$source_keys,";",fixed=TRUE))))
adjudication_path<-file.path(revision_dir,"audit/execution/cricsheet_pilot_adjudications.csv")
open_pilot_queries<-if(file.exists(adjudication_path))read_csv(adjudication_path)[decision=="open",.(comp,match_id)]else pilot_queries
checks<-list(); accounting<-list(); identities<-list(); crosswalk<-list(); deltas<-list()
add<-function(comp,check,n,note="")checks[[length(checks)+1L]]<<-data.table(comp,check,n,note)
for(comp in names(configs)) {
 x<-readRDS(file.path(revision_dir,"data/corrected",paste0("source_",comp,".rds")))
 t<-x$teams;p<-x$players;b<-x$batting;w<-x$bowling;ns<-x$batting_noshow
 b[,actual_position:=player]
 # With exactly two teams, the bowler's innings number refers to the opposing team's innings.
 stopifnot(all(t$teams%in%1:2),!anyNA(b$score),!anyNA(b$out),!anyNA(w$runs),!anyNA(w$wickets))
 w[,legal_balls:=overs*balls_per_over+extraballs]
 stopifnot(!anyNA(w$legal_balls),all(w$legal_balls>=0))
 lookup<-unique(t[,.(match_id,teams,team,start_date=as.IDate(start_date),end_date=as.IDate(end_date),competition,season_start_year,season_end_year,wicketkeeper)])
 assert_unique(lookup,c("match_id","teams"),paste(comp,"match-team lookup"))
 nominated<-lookup[!is.na(wicketkeeper),.(match_id,teams,player=wicketkeeper)]
 keeper_queries<-nominated[!p,on=.(match_id,teams,player)]
 add(comp,"wicketkeeper_nomination_missing_own_lineup",nrow(keeper_queries))
 if(nrow(keeper_queries))identities[[paste0(comp,"_keeper")]]<-cbind(comp,issue="keeper_not_in_own_lineup",keeper_queries)
 pm<-merge(p[,.(match_id,teams,player,nominal_position=position,birth_date,death_date,rh_bat,lh_bat,rh_bowl,lh_bowl,
  fast,fast_medium,medium_fast,medium,slow_medium,seam,lob,off_break,leg_break,googly,style_override)],lookup,by=c("match_id","teams"),all.x=TRUE)
 stopifnot(nrow(pm)==nrow(p),!anyNA(pm$team),!anyNA(pm$start_date),!anyNA(pm$end_date))
 assert_unique(pm,c("match_id","player"),paste(comp,"player-match spine"))
 bm<-b[,.(bat_runs=sum(score),innings_batted=.N,dismissals=sum(out),batting_position=mode_position(actual_position),
   recorded_balls_faced=sum(ballsfaced,na.rm=TRUE),balls_faced_coverage_unknown=any(is.na(ballsfaced)|ballsfaced==0)),by=.(match_id,teams,player=batter)]
 wm<-w[,.(wickets=sum(wickets),legal_balls=sum(legal_balls),runs_conceded=sum(runs),innings_bowled=.N,
   zero_ball_concessions=any(legal_balls==0 & runs>0)),by=.(match_id,teams,player=bowler)]
 add(comp,"batting_missing_lineup",bm[!pm,on=.(match_id,teams,player),.N])
 add(comp,"bowling_missing_lineup",wm[!pm,on=.(match_id,teams,player),.N])
 stopifnot(nrow(bm[!pm,on=.(match_id,teams,player)])==0,nrow(wm[!pm,on=.(match_id,teams,player)])==0)
 pm<-merge(pm,bm,by=c("match_id","teams","player"),all.x=TRUE)
 pm<-merge(pm,wm,by=c("match_id","teams","player"),all.x=TRUE)
 # Zero only means absence of a row with a verified appearance. Quarantine flags below
 # distinguish unresolved missing source rows from verified nonparticipation.
 pm[,batting_row_present:=!is.na(innings_batted)]
 pm[,bowling_row_present:=!is.na(innings_bowled)]
 for(v in c("bat_runs","innings_batted","dismissals","wickets","legal_balls","runs_conceded","innings_bowled"))set(pm,which(is.na(pm[[v]])),v,0L)
 pm[,wicketkeeper_nominated:=player==wicketkeeper]
 pm[,wicketkeeper_nomination_query:=match_id%in%keeper_queries$match_id]
 # The historical scorecard convention can differ from physical legal deliveries when an umpire miscounts an over.
 pm[,scorecard_balls:=legal_balls]
 pm[,birth:=suppressWarnings(as.IDate(birth_date))]
 pm[,death:=suppressWarnings(as.IDate(death_date))]
 pm[as.character(death_date)=="9999-12-31",death:=as.IDate(NA)]
 pm[,age_at_start:=as.numeric(start_date-birth)/365.2425]
 age_bad<-pm[!is.na(age_at_start) & (age_at_start<10 | age_at_start>65),.(match_id,player,team,start_date,birth,age_at_start)]
 add(comp,"implausible_age_appearance",nrow(age_bad));if(nrow(age_bad))identities[[paste0(comp,"_age")]]<-cbind(comp,issue="implausible_age",age_bad)
 # Existing metadata are retained as explicit observations, not silently voted into certainty.
 pm[,batting_hand:=fcase(rh_bat==1 & lh_bat==0,"right",rh_bat==0 & lh_bat==1,"left",rh_bat==0 & lh_bat==0,"unknown",default="conflicting")]
 pm[,pace:=rowSums(.SD)>0,.SDcols=c("fast","fast_medium","medium_fast","medium","slow_medium","seam")]
 pm[,spin:=rowSums(.SD)>0,.SDcols=c("off_break","leg_break","googly")]
 pm[,bowling_style:=fcase(pace & spin,"mixed",pace & !spin,"pace",!pace & spin,"spin",default="unknown")]
 pm[!is.na(style_override),bowling_style:=style_override]
 pm[,metadata_independently_validated:=FALSE]
 # Exhaustive score identities after the explicit correction overlay.
 bt<-b[,.(batter_runs=sum(score),outs=sum(out),batters=.N),by=.(match_id,teams,innings)]
 ac<-merge(t[,.(match_id,teams,innings,team_batted,runs,wickets,declared,wides,noballs,byes,legbyes,penalties)],bt,by=c("match_id","teams","innings"),all.x=TRUE)
 ac[,run_gap:=runs-batter_runs-wides-noballs-byes-legbyes-penalties]
 ac[,empty_zero_record:=is.na(batters) & runs==0 & wickets==0]
 ac[,unresolved_score:=(!empty_zero_record & (is.na(run_gap)|run_gap!=0)) | (!is.na(outs)&outs!=wickets)]
 accounting[[comp]]<-ac[,comp:=comp]
 # Also retain any unresolved source-level pilot differences as quarantines, not automatic source edits.
 # Evaluate credit and all-out completeness again after corrections, so resolved records are not retained as failures.
 credit<-b[out==1 & !is.na(bowler),.(credited=.N),by=.(match_id,teams=3L-teams,innings,bowler)]
 wc<-merge(w[,.(match_id,teams,innings,bowler,wickets)],credit,by=c("match_id","teams","innings","bowler"),all=TRUE)
 wc[is.na(credited),credited:=0L];wc[is.na(wickets),wickets:=0L]
 nsn<-ns[,.(noshow=.N),by=.(match_id,teams,innings)]
 complete<-merge(ac,nsn,by=c("match_id","teams","innings"),all.x=TRUE);complete[is.na(noshow),noshow:=0L]
 incomplete<-complete[wickets==10 & batters+noshow<11]
 add(comp,"corrected_run_accounting_queries",ac[unresolved_score==TRUE,.N])
 add(comp,"corrected_bowler_credit_queries",wc[wickets!=credited,.N])
 add(comp,"corrected_allout_completeness_queries",nrow(incomplete))
 add(comp,"empty_zero_innings_records",ac[empty_zero_record==TRUE,.N],"No batting row; distinguish no-play/forfeiture from ordinary innings")
 query_ids<-unique(c(ac[unresolved_score==TRUE,match_id],wc[wickets!=credited,match_id],incomplete$match_id))
 pm[,source_accounting_query:=match_id%in%query_ids]
 pilot_ids<-pilot_queries[pilot_queries$comp==comp,match_id]
 pm[,external_source_disagreement:=match_id%in%pilot_ids]
 open_pilot_ids<-open_pilot_queries[open_pilot_queries$comp==comp,match_id]
 pm[,external_source_query:=match_id%in%open_pilot_ids]
 pm[,identity_registry_query:=if(file.exists(registry_path))player%in%registry_keys else NA]
 registry_appearances_path<-file.path(revision_dir,"audit/execution",paste0("registry_appearances_",comp,".csv"))
 pm[,identity_registry_linked:=FALSE]
 if(file.exists(registry_appearances_path)) {
  linked_keys<-unique(read_csv(registry_appearances_path)[!is.na(player),.(match_id,player)])
  pm[linked_keys,on=.(match_id,player),identity_registry_linked:=TRUE]
 }
 pm[,data_gate_passed:=FALSE]
 setorder(pm,player,start_date,match_id)
 pm[,match_sequence:=seq_len(.N),by=player]
 # Position uses all actual innings, not a mode of match-level modes.
 positions<-b[,.(actual_modal_position=mode_position(actual_position)),by=.(player=batter)]
 careers<-pm[,.(matches=.N,bat_runs=sum(bat_runs),innings_batted=sum(innings_batted),dismissals=sum(dismissals),
   wickets=sum(wickets),legal_balls=sum(legal_balls),runs_conceded=sum(runs_conceded),
   first_date=min(start_date),last_date=max(start_date),last_end=max(end_date),
   source_accounting_query=any(source_accounting_query),external_source_query=any(external_source_query),identity_registry_query=any(identity_registry_query),data_gate_passed=FALSE),by=player]
 careers<-merge(careers,positions,by="player",all.x=TRUE)
 for(v in c("bat_runs","innings_batted","dismissals","wickets","legal_balls","runs_conceded"))stopifnot(sum(careers[[v]])==sum(pm[[v]]))
 stopifnot(sum(pm$bat_runs)==sum(b$score),sum(pm$legal_balls)==sum(w$legal_balls),sum(pm$wickets)==sum(w$wickets))
 # Cross-national keys are review candidates, not automatically false matches: legitimate switches exist.
 if(comp%in%c("tests","odis","t20is")) {
   nc<-pm[,.(n_teams=uniqueN(team),teams=paste(sort(unique(team)),collapse=";"),first_date=min(start_date),last_date=max(start_date)),by=player][n_teams>1]
   if(nrow(nc))identities[[paste0(comp,"_nations")]]<-cbind(comp,issue="multiple_national_sides",nc)
 }
 players<-data.table(player=sort(unique(pm$player)))
 players[,player_id:=vapply(player,function(s)paste0("p_",digest::digest(s,algo="sha256",serialize=FALSE)),character(1))]
 players[,comp:=comp];crosswalk[[comp]]<-players
 pm<-merge(pm,players[,.(player,player_id)],by="player",all.x=TRUE)
 fwrite(pm,file.path(revision_dir,"data/corrected",paste0("player_match_",comp,".csv")))
 fwrite(careers,file.path(revision_dir,"data/corrected",paste0("careers_",comp,".csv")))
 saveRDS(pm,file.path(revision_dir,"data/corrected",paste0("player_match_",comp,".rds")),compress="gzip")
 cat(comp,"provisional corrected components conserved\n")
}
write_audit(rbindlist(checks),"rebuilt_component_checks")
write_audit(rbindlist(accounting),"corrected_innings_accounting")
write_audit(rbindlist(identities,fill=TRUE),"expanded_identity_queries")
fwrite(rbindlist(crosswalk),file.path(revision_dir,"data/corrected/player_id_crosswalk.csv"))
jsonlite::write_json(list(status="CLOSED",updated="2026-10-08",scope="all substantive analyses",
  reason="Identity collision/alias adjudication, Hampshire 2008 accounting, historical metadata and competition coverage checks remain open.",
  estimation_authorised=FALSE,paid_relabelling_authorised=FALSE,human_role_labels="declined by user; implementation validation only"),file.path(revision_dir,"config/data_gate.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Component rebuild complete; every output explicitly remains gated.\n")

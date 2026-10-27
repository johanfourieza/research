# Read-only data integrity checks for revision planning. No model fitting.
# All outputs are local audit evidence; these are not a public data release.
suppressPackageStartupMessages(library(data.table))
set.seed(2412)
args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
rev <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
root <- normalizePath(file.path(rev, ".."), winslash = "/")
out <- file.path(rev, "audit")
read <- function(f) fread(f, na.strings = c("", "NA"), showProgress = FALSE)
cfgs <- list(tests=c("international/test", "test"), odis=c("international/oneday", "oneday"),
 t20is=c("international/t20", "t20"), county=c("county", "countycricket", "countycricket_older", "countycricket_older1"),
 sheffield=c("sheffield", "sheffieldshield"))
metrics <- list(); missing <- list(); conservation <- list(); seasons <- list(); pilots <- list()
exceptions <- list(); semantics <- list(); position_checks <- list(); ball_types <- list()
add <- function(comp, check, count, note="") metrics[[length(metrics)+1L]] <<- data.table(comp, check, count, note)
for (ck in names(cfgs)) {
 cfg <- cfgs[[ck]]
 stack <- function(suffix) rbindlist(lapply(cfg[-1], function(p) {
  z <- read(file.path(root,"data/raw/daft",cfg[1],paste0(p,"_",suffix,".csv")))
  z[, vintage := p]; z
 }), use.names=TRUE, fill=TRUE)
 team <- stack("teams"); bat <- stack("batting"); bowl <- stack("bowling"); play <- stack("players"); ns <- stack("batting_noshow")
 team[, start_date := as.IDate(start_date)]
 team[, yr := as.integer(format(start_date, "%Y"))]
 team[, included := !is.na(start_date) & start_date <= as.IDate("2025-12-31")]
 if (ck == "county") team[, included := included & (competition %in% c("Division 1","Division 2","Division 3","Group 1","Group 2","Group 3","Bob Willis Trophy") | (competition == "Other bilateral" & yr %in% 1994:1999))]
 seasons[[ck]] <- unique(team[,.(match_id,yr,season_start_year,competition,included,vintage)])[,.(matches=.N),by=.(yr,season_start_year,competition,included,vintage)][,comp:=ck]
 m <- team[included==TRUE,.(start_date=start_date[1], yr=yr[1], competition=competition[1]),by=match_id]
 ids <- m$match_id
 team <- team[match_id %in% ids]; bat <- bat[match_id %in% ids]; bowl <- bowl[match_id %in% ids]; play <- play[match_id %in% ids]; ns <- ns[match_id %in% ids]
 add(ck,"retained_matches",nrow(m)); add(ck,"retained_players",uniqueN(play$player))
 for (nm in c("team","bat","bowl","play","ns")) {
  z <- get(nm)
  for (v in names(z)) if (anyNA(z[[v]])) missing[[length(missing)+1L]] <- data.table(comp=ck,table=nm,variable=v,n_missing=sum(is.na(z[[v]])),n=nrow(z))
 }
 add(ck,"duplicate_batting_innings_keys",sum(duplicated(bat,by=c("match_id","teams","innings","batter"))))
 add(ck,"duplicate_bowling_innings_keys",sum(duplicated(bowl,by=c("match_id","teams","innings","bowler"))))
 add(ck,"duplicate_XI_player_match_keys",sum(duplicated(play,by=c("match_id","player"))))
 add(ck,"duplicate_XI_slot_keys",sum(duplicated(play,by=c("match_id","teams","position"))))
 add(ck,"matches_with_conflicting_start_dates",team[,.(n=uniqueN(start_date)),by=match_id][n>1,.N])
 add(ck,"matches_with_multiple_vintages",team[,.(n=uniqueN(vintage)),by=match_id][n>1,.N])
 add(ck,"team_lookup_nonunique",unique(team[,.(match_id,teams,team)])[, .N,by=.(match_id,teams)][N>1,.N])
 add(ck,"player_keys_multiple_birth_dates",play[!is.na(birth_date),.(n=uniqueN(birth_date)),by=player][n>1,.N])
 add(ck,"invalid_out_codes",bat[!is.na(out)&!out%in%c(0,1),.N])
 add(ck,"invalid_bowling_components",bowl[overs<0 | extraballs<0 | extraballs>=balls_per_over | wickets<0 | wickets>10 | runs<0,.N])
 ball_types[[ck]] <- bowl[,.(n=.N),by=balls_per_over][,comp:=ck]
 xi <- unique(play[,.(match_id,player,teams)])
 orphan_bat <- unique(bat[,.(match_id,player=batter)])[!xi,on=.(match_id,player)]
 orphan_bowl <- unique(bowl[,.(match_id,player=bowler)])[!xi,on=.(match_id,player)]
 add(ck,"batting_player_match_not_in_XI",nrow(orphan_bat)); add(ck,"bowling_player_match_not_in_XI",nrow(orphan_bowl))
 if(nrow(orphan_bat)) exceptions[[paste0(ck,"_orphan_bat")]] <- cbind(comp=ck,issue="orphan_bat",orphan_bat)
 if(nrow(orphan_bowl)) exceptions[[paste0(ck,"_orphan_bowl")]] <- cbind(comp=ck,issue="orphan_bowl",orphan_bowl)
 # Direct totals independent of manuscript pipeline. Missing numeric inputs are counted above.
 car <- read(file.path(root,"data/processed",paste0("careers_",ck,".csv")))
 pm <- read(file.path(root,"data/processed",paste0("permatch_raw_",ck,".csv")))
 totals <- data.table(comp=ck,variable=c("batting_runs","bowler_wickets","legal_balls","conceded_runs","batting_innings","dismissals","appearances"),
  raw=c(sum(bat$score),sum(bowl$wickets),sum(bowl$overs*bowl$balls_per_over+bowl$extraballs),sum(bowl$runs),nrow(bat),sum(bat$out),uniqueN(play,by=c("player","match_id"))),
  career=c(sum(car$runs),sum(car$wickets),sum(car$balls),sum(car$runs_conceded),sum(car$innings_batted),sum(car$outs),sum(car$matches_played)),
  per_match=c(sum(pm$bat_runs),sum(pm$wk),sum(pm$balls_m),sum(pm$runs_m),sum(pm$inns_m),NA_real_,nrow(pm)))
 totals[,difference_career:=career-raw]; totals[,difference_per_match:=per_match-raw]; conservation[[ck]] <- totals
 # Missing balls-faced values should remain unknown, not a zero career total.
 bf <- bat[,.(known_balls=sum(ballsfaced,na.rm=TRUE),missing_balls=anyNA(ballsfaced)),by=.(player=batter)]
 bf <- merge(bf,car[,.(player,stored_balls=ballsfaced)],by="player")
 add(ck,"career_ballsfaced_zero_despite_positive_known_balls",bf[missing_balls & known_balls>0 & stored_balls==0,.N])
 # Verify old match-ID ordering rather than assume it is nonchronological.
 spine <- merge(unique(play[,.(player,match_id)]),m[,.(match_id,start_date,yr)],by="match_id")
 setorder(spine,player,yr,match_id); spine[,old_seq:=seq_len(.N),by=player]
 setorder(spine,player,start_date,match_id); spine[,date_seq:=seq_len(.N),by=player]
 add(ck,"old_sequence_differs_from_exact_date_order",spine[old_seq!=date_seq,.N])
 # Nominal XI slots versus actual batting-order slots (the latter are in batting.player).
 pos <- merge(bat[,.(match_id,teams,batter,innings,actual=player)], play[,.(match_id,teams,batter=player,nominal=position)],by=c("match_id","teams","batter"),all.x=TRUE)
 add(ck,"innings_actual_position_differs_from_nominal_XI",pos[!is.na(actual)&!is.na(nominal)&actual!=nominal,.N])
 mode1 <- function(x) {x<-x[!is.na(x)]; if(!length(x)) return(NA_integer_); u<-unique(x); as.integer(u[which.max(tabulate(match(x,u)))])}
 ap <- pos[,.(actual_mode=mode1(actual)),by=.(player=batter)]
 ap <- merge(ap,car[,.(player,modal_position,innings_batted)],by="player")
 ap[,comp:=ck]; position_checks[[ck]] <- ap
 add(ck,"top7_role_changes_with_actual_innings_position",ap[innings_batted>=20 & !is.na(actual_mode) & (actual_mode<=7)!=(modal_position<=7),.N])
 # Run/wicket accounting by batting team innings. A discrepancy is a query, not automatically a data error.
 bt <- bat[,.(bat_runs=sum(score),bat_outs=sum(out)),by=.(match_id,teams,innings)]
 ti <- merge(team[,.(match_id,teams,innings,runs,wickets,wides,noballs,byes,legbyes,penalties,team_batted)],bt,by=c("match_id","teams","innings"),all.x=TRUE)
 ti[,run_gap:=runs-(bat_runs+wides+noballs+byes+legbyes+penalties)]
 add(ck,"innings_team_score_discrepancies",ti[!is.na(run_gap)&run_gap!=0,.N])
 add(ck,"innings_team_dismissal_discrepancies",ti[!is.na(bat_outs)&wickets!=bat_outs,.N],"Review retired/absent cases against noshow table")
 bad <- ti[(!is.na(run_gap)&run_gap!=0)|(!is.na(bat_outs)&wickets!=bat_outs)]
 if(nrow(bad)) exceptions[[paste0(ck,"_score")]] <- cbind(comp=ck,issue="innings_accounting",bad)
 # Metadata labelling pilot, spread across eras and vintage boundaries.
 eligible <- merge(m,unique(play[,.(match_id,player,position,birth_date,fast,fast_medium,medium_fast,medium,slow_medium,seam,lob,off_break,leg_break,googly)]),by="match_id")
 eligible[,era:=(yr%/%25)*25]
 pilot <- eligible[, .SD[sample(.N,min(.N,3))],by=era]
 pilot[,comp:=ck]; pilots[[ck]] <- pilot
 # Check signed and clipped definitions without fitting a model.
 if(ck %in% c("odis","t20is")) {
  ana <- read(file.path(root,"data/analysis",paste0("analysis_",ck,".csv")))
  val <- read(file.path(root,"data/processed",paste0("permatch_",ck,".csv")))
  v <- val[,.(match_mean=mean(bowl_val)),by=player]
  v <- merge(v,ana[,.(player,bowl_value_map)],by="player")
  add(ck,"career_vs_match_clipped_bowling_value_mismatch",v[abs(match_mean-bowl_value_map)>1e-8,.N])
  add(ck,"zero_legal_ball_spells_with_conceded_runs",bowl[overs*balls_per_over+extraballs==0 & runs>0,.N])
 }
 cat(ck,"integrity checks complete\n")
}
write <- function(x,name) fwrite(rbindlist(x,use.names=TRUE,fill=TRUE),file.path(out,paste0(name,".csv")))
write(metrics,"integrity_metrics");write(missing,"missingness");write(conservation,"conservation")
write(seasons,"competition_year_coverage");write(pilots,"metadata_pilot_sample")
write(exceptions,"exceptions");write(position_checks,"position_comparison");write(ball_types,"balls_per_over")
capture.output(sessionInfo(),file=file.path(out,"r_session.txt"))
cat("Audit complete; no statistical models run.\n")

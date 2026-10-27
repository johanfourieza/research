# Stage A: immutable-source inventory and exhaustive semantic checks, no model fitting.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
dictionary <- list(); checks <- list(); issues <- list(); match_register <- list(); player_register <- list()
add <- function(comp, check, n, note="") checks[[length(checks)+1L]] <<- data.table(comp,check,n,note)
issue <- function(comp, type, z) {
  if (nrow(z)) issues[[length(issues)+1L]] <<- cbind(comp=comp, issue=type, z)
}
dd <- read_csv(file.path(book_data,"raw/daft/dd.csv"))
tab_map <- c(teams="teams", players="play", batting="bat", bowling="bowl", batting_noshow="bat_noshow")
for (comp in names(configs)) {
  x <- read_comp(comp)
  for (tab in names(x)) {
    z <- x[[tab]]
    for (v in setdiff(names(z),c("source_file","source_row","vintage"))) {
      desc <- dd[dataframe==tab_map[[tab]] & variable==v, description]
      dictionary[[length(dictionary)+1L]] <- data.table(comp,table=tab,column=v,
        storage_type=paste(class(z[[v]]),collapse="/"),source_files=paste(unique(z$source_file),collapse=";"),
        n=nrow(z),n_missing=sum(is.na(z[[v]])),vendor_description=if(length(desc))desc[1] else NA_character_)
    }
  }
  t <- x$teams
  t[, `:=`(start=as.IDate(start_date),end=as.IDate(end_date))]
  add(comp,"missing_start_date",t[is.na(start),.N])
  add(comp,"missing_end_date",t[is.na(end),.N])
  add(comp,"end_before_start",t[!is.na(start)&!is.na(end)&end<start,.N])
  issue(comp,"invalid_dates",t[is.na(start)|is.na(end)|end<start,.(match_id,teams,innings,start,end)])
  t[, date_eligible:=!is.na(start)&start<=as.IDate("2025-12-31")]
  t[, competition_eligible:=TRUE]
  if(comp=="county") t[, competition_eligible:=competition%in%c("Division 1","Division 2","Division 3","Group 1","Group 2","Group 3","Bob Willis Trophy") |
    (competition=="Other bilateral" & as.integer(format(start,"%Y"))%in%1994:1999)]
  # Membership is provisional until the independent competition register is checked.
  mr <- unique(t[,.(match_id,start,end,competition,season_start_year,season_end_year,date_eligible,competition_eligible,vintage)])
  assert_unique(mr,"match_id",paste(comp,"match dates/membership"))
  mr[,comp:=comp]; match_register[[comp]] <- mr
  ids <- mr[date_eligible & competition_eligible,match_id]
  for(tab in names(x)) x[[tab]] <- x[[tab]][match_id%in%ids]
  t <- x$teams; p <- x$players; b <- x$batting; w <- x$bowling; ns <- x$batting_noshow
  assert_unique(p,c("match_id","player"),paste(comp,"line-up"))
  assert_unique(t,c("match_id","teams","innings"),paste(comp,"team innings"))
  assert_unique(b,c("match_id","teams","innings","batter"),paste(comp,"batting"))
  assert_unique(w,c("match_id","teams","innings","bowler"),paste(comp,"bowling"))
  # Team membership must agree, not just identity presence somewhere in the match.
  bp <- b[!p,on=.(match_id,teams,batter=player)]
  wp <- w[!p,on=.(match_id,teams,bowler=player)]
  add(comp,"batter_wrong_or_missing_team",nrow(bp)); add(comp,"bowler_wrong_or_missing_team",nrow(wp))
  issue(comp,"bat_team_mismatch",bp); issue(comp,"bowl_team_mismatch",wp)
  team_lookup <- unique(t[,.(match_id,teams,team)])
  assert_unique(team_lookup,c("match_id","teams"),paste(comp,"team identity"))
  add(comp,"matches_without_two_teams",team_lookup[, .N, by=match_id][N!=2,.N])
  # In the vendor, bowling.teams is the bowler's team; innings is the opposition's innings number.
  bat_credit <- b[out==1 & !is.na(bowler),.(credited=sum(!howout%in%c("run out","retired out","obstructing the field","handled the ball","timed out"))),
    by=.(match_id,teams=3L-teams,innings,bowler)]
  wc <- merge(w[,.(match_id,teams,innings,bowler,wickets)],bat_credit,by=c("match_id","teams","innings","bowler"),all=TRUE)
  wc[is.na(credited),credited:=0L]; wc[is.na(wickets),wickets:=0L]
  add(comp,"bowler_credit_vs_dismissals",wc[wickets!=credited,.N])
  issue(comp,"bowler_credit_mismatch",wc[wickets!=credited])
  # Check every played innings, including entire absent batting tables.
  bs <- b[,.(bat_rows=.N,bat_runs=sum(score),outs=sum(out),notouts=sum(out==0)),by=.(match_id,teams,innings)]
  nt <- ns[,.(noshow_rows=.N,noshow_text=paste(how,collapse=";")),by=.(match_id,teams,innings)]
  xi <- p[,.(xi_count=.N),by=.(match_id,teams)]
  ac <- merge(t,bs,by=c("match_id","teams","innings"),all.x=TRUE)
  ac <- merge(ac,nt,by=c("match_id","teams","innings"),all.x=TRUE)
  ac <- merge(ac,xi,by=c("match_id","teams"),all.x=TRUE)
  ac[is.na(noshow_rows),noshow_rows:=0L]
  ac[, run_gap:=runs-bat_runs-wides-noballs-byes-legbyes-penalties]
  add(comp,"played_innings_missing_batting",ac[team_batted==1 & is.na(bat_rows),.N])
  add(comp,"run_accounting_mismatch",ac[team_batted==1 & (is.na(run_gap)|run_gap!=0),.N])
  issue(comp,"run_accounting",ac[team_batted==1 & (is.na(run_gap)|run_gap!=0)])
  # Eleven is not an inflexible XI rule: flag twelve-player matches for separate replacement adjudication.
  incomplete <- ac[wickets==10 & bat_rows+noshow_rows<11]
  add(comp,"allout_fewer_than_eleven_bat_or_noshow",nrow(incomplete))
  issue(comp,"allout_completeness",incomplete)
  add(comp,"team_lineups_over_eleven",xi[xi_count>11,.N]); issue(comp,"replacement_lineup",xi[xi_count>11])
  overlap <- merge(b[,.(match_id,teams,innings,batter)],ns[,.(match_id,teams,innings,batter,how)],by=c("match_id","teams","innings","batter"))
  add(comp,"batting_and_noshow_overlap",nrow(overlap));issue(comp,"batting_noshow_overlap",overlap)
  # Separate structural metadata categories rather than silently defaulting to R or pace.
  meta <- unique(p[,setdiff(names(p),c("match_id","teams","position","source_file","source_row","vintage")),with=FALSE])
  meta[, `:=`(birth=as.IDate(birth_date),death=as.IDate(death_date))]
  meta[, hand:=fcase(rh_bat==1 & lh_bat==0,"right",rh_bat==0 & lh_bat==1,"left",rh_bat==0 & lh_bat==0,"unknown",default="conflicting")]
  meta[, pace_flag:=rowSums(.SD,na.rm=FALSE)>0,.SDcols=c("fast","fast_medium","medium_fast","medium","slow_medium","seam")]
  meta[, spin_flag:=rowSums(.SD,na.rm=FALSE)>0,.SDcols=c("off_break","leg_break","googly")]
  meta[, style:=fcase(pace_flag & spin_flag,"mixed",pace_flag & !spin_flag,"pace",!pace_flag & spin_flag,"spin",default="unknown")]
  meta[death_date=="9999-12-31",death:=as.IDate(NA)]
  issue(comp,"metadata_changes",meta[player%in%meta[,.N,by=player][N>1,player]])
  firsts <- merge(p[,.(match_id,teams,player)],team_lookup,by=c("match_id","teams"))
  firsts <- merge(firsts,mr[,.(match_id,start,end)],by="match_id")
  setorder(firsts,player,start,match_id)
  firsts <- firsts[,.(first_date=start[1],first_match=match_id[1],first_team=team[1],last_date=max(start),matches=.N),by=player]
  firsts[,comp:=comp];player_register[[comp]] <- firsts
  saveRDS(x,file.path(revision_dir,"data/staging",paste0("source_",comp,".rds")),compress="gzip")
  fwrite(meta,file.path(revision_dir,"data/staging",paste0("metadata_",comp,".csv")))
  cat(comp,"staged and checked\n")
}
write_audit(rbindlist(dictionary,fill=TRUE),"raw_field_inventory")
write_audit(rbindlist(checks),"stage_a_checks")
write_audit(rbindlist(issues,fill=TRUE),"stage_a_issues")
write_audit(rbindlist(match_register,fill=TRUE),"match_membership_register")
pr <- rbindlist(player_register)
write_audit(pr,"player_competition_register")
tests <- pr[comp=="tests"]
controls <- list()
for(dom in c("county","sheffield")) {
  nation <- if(dom=="county")"England" else "Australia"
  cutoff <- as.IDate(if(dom=="county")"1946-01-01" else "1892-01-01")
  z <- merge(tests[first_team==nation & first_date>=cutoff],pr[comp==dom],by="player",all.x=TRUE,suffixes=c("_test","_domestic"))
  z[,linked:=!is.na(first_date_domestic)]
  z[,domestic_before_test:=linked & first_date_domestic<first_date_test]
  z[,domestic_comp:=dom];controls[[dom]]<-z
}
write_audit(rbindlist(controls),"test_debut_linkage_controls")
cat("Stage A diagnostics complete; no substantive analysis.\n")

source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
z<-rbindlist(lapply(c("batting","bowling"),function(tab) {
 q<-read_csv(file.path(revision_dir,"audit/execution",paste0("corrected_cricsheet_",tab,"_comparison.csv")))[status!="agree"]
 q[,table:=tab];q
}),fill=TRUE)
z[,`:=`(decision="open",reason="Requires further identity or source adjudication",evidence=NA_character_)]
ov<-read_csv(file.path(revision_dir,"audit/execution/cricsheet_miscounted_over_adjudications.csv"))
for(i in seq_len(nrow(z))) {
 r<-z[i]
 if(r$table=="bowling" && !is.na(r$name)) {
  a<-ov[cricsheet_id==r$cricsheet_id & bowler==r$name]
  if(nrow(a) && r$balls_cs+sum(a$scorecard_adjustment)==r$balls_daft && r$conceded_cs==r$conceded_daft && r$wickets_cs==r$wickets_daft)
   z[i,`:=`(decision="retain_scorecard_convention",reason="Actual legal deliveries differ from scorecard balls exactly by documented miscounted overs",evidence=paste0("all_json.zip:",r$cricsheet_id,".json; innings.miscounted_overs"))]
 }
}
z[comp=="sheffield" & match_id=="20250208_02337_Gabba",`:=`(decision="retain_daft_confirmed_scorecard",reason="Queensland club archive supports DAFT batter scores and bowling figures; Cricsheet allocation differs",evidence="https://www.cricketarchive.co.uk/Queensland/Scorecards/1356/1356169.html")]
z[comp=="sheffield" & match_id=="20241101_02325_Bellrve" & grepl("^Doran,",player),`:=`(decision="retain_daft_confirmed_scorecard",reason="WACA match report independently states Doran 16 from 145 balls; Cricsheet gives 12",evidence="https://www.wacricket.com.au/news/4162560/couch-hat-trick-seals-remarkable-win-over-tigers")]
write_audit(z,"cricsheet_pilot_adjudications")
print(z[,.(records=.N),by=.(table,decision)])
tm<-read_csv(file.path(revision_dir,"audit/execution/corrected_cricsheet_team_comparison.csv"))
stopifnot(all(tm$runs_cs==tm$runs_daft),all(tm$wickets_cs==tm$wickets_daft))
cat(nrow(tm),"team innings agree exactly on runs and wickets. Remaining open comparison rows:",z[decision=="open",.N],"\n")

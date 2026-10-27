# Additional deterministic identity screen. Genuine changes of national side are
# possible; same-day appearances for different national sides need record review.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
setDTthreads(1)
z<-rbindlist(lapply(c("tests","odis","t20is"),function(co) {
 x<-readRDS(file.path(revision_dir,"data/corrected",paste0("source_",co,".rds")))
 tm<-unique(x$teams[,.(match_id,teams,team,start_date=as.IDate(start_date),end_date=as.IDate(end_date))])
 y<-merge(x$players[,.(match_id,teams,player)],tm,by=c("match_id","teams"));y[,comp:=co];y
}))
q<-z[,.(national_sides=uniqueN(team),teams=paste(sort(unique(team)),collapse=";"),matches=paste(match_id,collapse=";"),comps=paste(comp,collapse=";")),by=.(player,start_date)][national_sides>1]
write_audit(q,"same_day_national_identity_queries")
# A short interval is a priority signal, not an asserted eligibility-rule violation.
multi<-z[,uniqueN(team),by=player][V1>1,player];near<-list()
for(who in multi) {
 p<-z[player==who];setorder(p,start_date,match_id)
 for(j in seq_len(nrow(p)-1L)) {
  k<-j+1L
  if(p$team[j]!=p$team[k] && as.integer(p$start_date[k]-p$start_date[j])<=7L)
   near[[length(near)+1L]]<-data.table(player=who,team_a=p$team[j],team_b=p$team[k],date_a=p$start_date[j],date_b=p$start_date[k],match_a=p$match_id[j],match_b=p$match_id[k],days=as.integer(p$start_date[k]-p$start_date[j]))
 }
}
write_audit(rbindlist(near),"short_interval_national_identity_queries")
cat(nrow(q),"same-day national identity queries;",length(near),"adjacent different-team appearances within seven days. These are queries, not automatic errors.\n")

source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
setDTthreads(1)
rows<-list()
for(id in c(64013,573007,1153693,1272379,1233450,1444511)) {
 card<-jsonlite::fromJSON(file.path(revision_dir,"sources/cricsheet",paste0(id,".json")),simplifyVector=FALSE)
 for(j in seq_along(card$innings)) {
  inn<-card$innings[[j]]
  if(length(inn$miscounted_overs))for(no in names(inn$miscounted_overs)) {
   ov<-Filter(function(z)z$over==as.integer(no),inn$overs)[[1]]
   bowler<-unique(vapply(ov$deliveries,function(d)d$bowler,character(1)))
   legal<-sum(vapply(ov$deliveries,function(d)is.null(d$extras$wides)&is.null(d$extras$noballs),logical(1)))
   rows[[length(rows)+1L]]<-data.table(cricsheet_id=id,absolute_innings=j,team=inn$team,over=as.integer(no),bowler=paste(bowler,collapse=";"),legal_deliveries=legal,registered_count=inn$miscounted_overs[[no]]$balls,scorecard_adjustment=6L-legal)
  }
 }
}
write_audit(rbindlist(rows),"cricsheet_miscounted_over_adjudications")
print(rbindlist(rows))

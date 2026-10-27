# Independent score reconstruction from the already-held Cricsheet delivery archive.
# A seeded 12-match sample per competition, plus three archive-linked known anomalies.
# A fourth requested target (Malaysia/Singapore 2023) lacks a unique archive link and is checked externally.
# These checks validate counting fields, not bowling style or human role labels.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
suppressPackageStartupMessages(library(jsonlite))
corrected<-"--corrected"%in%commandArgs(TRUE)
input_folder<-if(corrected)"data/corrected"else"data/staging"
audit_prefix<-if(corrected)"corrected_"else""
write_audit_original<-write_audit
write_audit<-function(x,name)write_audit_original(x,paste0(audit_prefix,name))
canon <- function(s) {
  s <- sub(" \\{b.*", "",s)
  comma <- grepl(",",s)
  s[comma] <- sub("^([^,]+), (.*)$","\\2 \\1",s[comma])
  toupper(gsub("[^a-zA-Z0-9]","",iconv(s,to="ASCII//TRANSLIT")))
}
manifest <- read_csv(file.path(book_data,"raw/cricsheet/MANIFEST_matches.csv"))
manifest <- manifest[gender=="male" & date<=as.IDate("2025-12-31")]
types <- c(tests="Test",odis="ODI",t20is="T20",county="CCH",sheffield="SSH")
archive <- file.path(book_data,"raw/cricsheet/all_json.zip")
source_dir <- file.path(revision_dir,"sources/cricsheet")
dir.create(source_dir,recursive=TRUE,showWarnings=FALSE)
crosswalk_file<-file.path(revision_dir,"config/cricsheet_pilot_crosswalk.csv")
crosswalk<-if(file.exists(crosswalk_file))read_csv(crosswalk_file)else data.table()
resolve_names<-function(z,players,co,mid) {
 z<-merge(z,players,by=c("teams","canonical"),all.x=TRUE)
 if(nrow(crosswalk))for(k in which(is.na(z$player))) {
  found<-crosswalk[comp==co & match_id==mid & teams==z$teams[k] & cricsheet_name==z$name[k] & !is.na(player),player]
  if(length(found)==1 && found%in%players$player)z$player[k]<-found
 }
 z
}
frames<-list(); coverage<-list(); results<-list(); aliases<-list(); batting<-list(); bowling<-list(); ambiguous<-list()
zero <- function(x) if(is.null(x))0 else x
for(comp in names(types)) {
  x <- readRDS(file.path(revision_dir,input_folder,paste0("source_",comp,".rds")))
  tm <- unique(x$teams[,.(match_id,date=as.IDate(start_date),teams,team)])
  mm <- tm[,.(pair=paste(sort(canon(team)),collapse="|")),by=.(match_id,date)]
  cs <- copy(manifest[match_type==types[[comp]]])
  if(comp=="county") cs<-rbind(cs,manifest[match_type=="BWT"])
  cs[,pair:=mapply(function(a,b)paste(sort(c(canon(a),canon(b))),collapse="|"),team1,team2)]
  setnames(cs,"match_id","cricsheet_id")
  linked <- merge(mm,cs,by=c("date","pair"))
  bad_daft <- linked[,.N,by=match_id][N>1,match_id]
  bad_cs <- linked[,.N,by=cricsheet_id][N>1,cricsheet_id]
  ambiguous[[comp]] <- linked[match_id%in%bad_daft | cricsheet_id%in%bad_cs][,comp:=comp]
  linked <- linked[!match_id%in%bad_daft & !cricsheet_id%in%bad_cs]
  assert_unique(linked,"match_id",paste(comp,"Cricsheet link"))
  assert_unique(linked,"cricsheet_id",paste(comp,"DAFT link"))
  coverage[[comp]]<-data.table(comp,daft_matches=nrow(mm),cricsheet_matches=nrow(cs),linked=nrow(linked))
  setorder(linked,date,match_id)
  linked[,stratum:=pmin(4L,ceiling(seq_len(.N)/.N*4))]
  chosen<-linked[, .SD[sample.int(.N,min(3L,.N))],by=stratum]
  chosen[,selection:="seeded_temporal_quartile"]
  forced<-linked[match_id%in%c("20250402_04862_Hamiltn","20160716_03634_Sthprt","20211021_01337_Almra","20230928_02257_Hngzh") & !match_id%in%chosen$match_id]
  forced[,selection:="targeted_known_anomaly"]
  chosen<-rbind(chosen,forced,fill=TRUE);chosen[,comp:=comp]
  frames[[comp]]<-chosen
  for(j in seq_len(nrow(chosen))) {
    mid<-chosen$match_id[j];cid<-chosen$cricsheet_id[j]
    filename<-paste0(cid,".json")
    if(!file.exists(file.path(source_dir,filename))) unzip(archive,files=filename,exdir=source_dir)
    card<-fromJSON(file.path(source_dir,filename),simplifyVector=FALSE)
    players<-x$players[match_id==mid,.(player,teams)]
    players[,canonical:=canon(player)]
    assert_unique(players,c("teams","canonical"),paste(comp,mid,"canonical identity"))
    inncounts<-setNames(integer(length(card$info$teams)),unlist(card$info$teams))
    for(inn in card$innings) {
      if(isTRUE(inn$super_over)) next
      inncounts[inn$team]<-inncounts[inn$team]+1L
      innings<-unname(inncounts[inn$team])
      teamno<-tm[match_id==mid & canon(team)==canon(inn$team),teams]
      stopifnot(length(teamno)==1)
      deliveries<-list(); dismissals<-list(); appearances<-character()
      for(ov in inn$overs) for(d in ov$deliveries) {
        appearances<-unique(c(appearances,d$batter,d$non_striker))
        deliveries[[length(deliveries)+1L]]<-data.table(batter=d$batter,bowler=d$bowler,
          runs=d$runs$batter,conceded=d$runs$total-zero(d$extras$byes)-zero(d$extras$legbyes)-zero(d$extras$penalty),
          legal=as.integer(zero(d$extras$wides)==0 & zero(d$extras$noballs)==0),
          wides=zero(d$extras$wides),noballs=zero(d$extras$noballs),byes=zero(d$extras$byes),legbyes=zero(d$extras$legbyes),
          total=d$runs$total)
        if(length(d$wickets)) for(k in d$wickets) dismissals[[length(dismissals)+1L]]<-data.table(player=k$player_out,
          bowler=d$bowler,kind=k$kind,out=as.integer(!k$kind%in%c("retired hurt","obstructing the field")))
      }
      if(!length(deliveries)) next
      d<-rbindlist(deliveries)
      wk<-if(length(dismissals))rbindlist(dismissals) else data.table(player=character(),bowler=character(),kind=character(),out=integer())
      # Obstructing the field IS a batter dismissal, but is not credited to the bowler.
      wk[kind=="obstructing the field",out:=1L]
      cb<-merge(data.table(name=appearances),d[,.(score_cs=sum(runs)),by=.(name=batter)],by="name",all.x=TRUE)
      cb[is.na(score_cs),score_cs:=0]
      cb<-merge(cb,wk[,.(out_cs=sum(out)),by=.(name=player)],by="name",all.x=TRUE)
      cb[is.na(out_cs),out_cs:=0L]
      cb[,`:=`(canonical=canon(name),teams=teamno)]
      cb<-resolve_names(cb,players,comp,mid)
      aliases[[length(aliases)+1L]]<-cb[is.na(player),.(comp,match_id=mid,cricsheet_id=cid,teams,name,canonical)]
      # Explicit local variables avoid data.table's column-name masking.
      inn_no<-innings
      rawb<-x$batting[match_id==mid & teams==teamno & innings==inn_no,.(player=batter,score_daft=score,out_daft=out)]
      cmp<-merge(cb,rawb,by="player",all=TRUE)
      cmp[,`:=`(comp=comp,match_id=mid,cricsheet_id=cid,innings=inn_no)]
      cmp[,status:=fcase(is.na(player),"identity_unresolved",is.na(score_daft),"missing_daft_batting",is.na(score_cs),"missing_cricsheet_batting",score_cs!=score_daft | out_cs!=out_daft,"value_mismatch",default="agree")]
      batting[[length(batting)+1L]]<-cmp
      cw<-d[,.(balls_cs=sum(legal),conceded_cs=sum(conceded)),by=.(name=bowler)]
      credit<-wk[!kind%in%c("run out","retired hurt","obstructing the field","retired out","timed out"),.(wickets_cs=.N),by=.(name=bowler)]
      cw<-merge(cw,credit,by="name",all.x=TRUE);cw[is.na(wickets_cs),wickets_cs:=0L]
      cw[,`:=`(canonical=canon(name),teams=3L-teamno)]
      cw<-resolve_names(cw,players,comp,mid)
      aliases[[length(aliases)+1L]]<-cw[is.na(player),.(comp,match_id=mid,cricsheet_id=cid,teams,name,canonical)]
      raww<-x$bowling[match_id==mid & teams==3L-teamno & innings==inn_no,.(player=bowler,balls_daft=overs*balls_per_over+extraballs,conceded_daft=runs,wickets_daft=wickets)]
      cmp<-merge(cw,raww,by="player",all=TRUE)
      cmp[,`:=`(comp=comp,match_id=mid,cricsheet_id=cid,innings=inn_no)]
      cmp[,status:=fcase(is.na(player),"identity_unresolved",is.na(balls_daft),"missing_daft_bowling",is.na(balls_cs),"missing_cricsheet_bowling",balls_cs!=balls_daft | conceded_cs!=conceded_daft | wickets_cs!=wickets_daft,"value_mismatch",default="agree")]
      bowling[[length(bowling)+1L]]<-cmp
      tr<-x$teams[match_id==mid & teams==teamno & innings==inn_no]
      results[[length(results)+1L]]<-data.table(comp,match_id=mid,cricsheet_id=cid,teams=teamno,innings=inn_no,
        runs_cs=sum(d$total)+zero(inn$penalty_runs$pre)+zero(inn$penalty_runs$post),runs_daft=tr$runs,
        wickets_cs=sum(wk$out),wickets_daft=tr$wickets,source_member=paste0("all_json.zip:",cid,".json"))
    }
    cat(comp,mid,"independent card checked\n")
  }
}
write_audit(rbindlist(frames,fill=TRUE),"cricsheet_pilot_frame")
write_audit(rbindlist(coverage),"cricsheet_linkage_coverage")
write_audit(rbindlist(batting,fill=TRUE),"cricsheet_batting_comparison")
write_audit(rbindlist(bowling,fill=TRUE),"cricsheet_bowling_comparison")
write_audit(rbindlist(results,fill=TRUE),"cricsheet_team_comparison")
write_audit(rbindlist(aliases,fill=TRUE),"cricsheet_name_queries")
write_audit(rbindlist(ambiguous,fill=TRUE),"cricsheet_ambiguous_match_links")
cat("Cricsheet count-field pilot complete; no substantive models.\n")

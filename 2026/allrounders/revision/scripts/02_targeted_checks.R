# Independent component/metadata audit; reads immutable source, fits no models.
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(FALSE); f <- sub("^--file=","",a[grepl("^--file=",a)])
rev <- normalizePath(file.path(dirname(f),".."),winslash="/"); root<-dirname(rev)
read <- function(p) fread(p,na.strings=c("","NA"),showProgress=FALSE)
cfgs<-list(tests=c("international/test","test"),odis=c("international/oneday","oneday"),t20is=c("international/t20","t20"),county=c("county","countycricket","countycricket_older","countycricket_older1"),sheffield=c("sheffield","sheffieldshield"))
rows<-list(); targets<-list(); coverage<-list(); sentinels<-list(); linkage<-list()
for(ck in names(cfgs)) {
 cfg<-cfgs[[ck]]
 stack<-function(s) rbindlist(lapply(cfg[-1],function(p)read(file.path(root,"data/raw/daft",cfg[1],paste0(p,"_",s,".csv")))),fill=TRUE)
 t<-stack("teams");p<-stack("players");b<-stack("batting");w<-stack("bowling")
 t[,date:=as.IDate(start_date)]; t<-t[date<=as.IDate("2025-12-31")]
 if(ck=="county")t<-t[competition%in%c("Division 1","Division 2","Division 3","Group 1","Group 2","Group 3","Bob Willis Trophy")|(competition=="Other bilateral" & as.integer(format(date,"%Y"))%in%1994:1999)]
 ids<-unique(t$match_id);p<-p[match_id%in%ids];b<-b[match_id%in%ids];w<-w[match_id%in%ids]
 car<-read(file.path(root,"data/processed",paste0("careers_",ck,".csv")))
 pm<-read(file.path(root,"data/processed",paste0("permatch_raw_",ck,".csv")))
 br<-b[,.(runs=sum(score),innings_batted=.N,outs=sum(out)),by=.(player=batter)]
 wr<-w[,.(wickets=sum(wickets),balls=sum(overs*balls_per_over+extraballs),runs_conceded=sum(runs)),by=.(player=bowler)]
 cr<-merge(unique(p[,.(player)]),br,by="player",all.x=TRUE);cr<-merge(cr,wr,by="player",all.x=TRUE)
 for(v in setdiff(names(cr),"player")) set(cr,which(is.na(cr[[v]])),v,0)
 cmp<-merge(cr,car,by="player",suffixes=c("_raw","_stored"))
 for(v in c("runs","innings_batted","outs","wickets","balls","runs_conceded"))rows[[length(rows)+1L]]<-data.table(comp=ck,check=paste0("career_",v),mismatch=sum(cmp[[paste0(v,"_raw")]]!=cmp[[paste0(v,"_stored")]]),n=nrow(cmp))
 # Match-ID sequence verified independently in audit 01.
 spine<-unique(p[,.(player,match_id)]);spine<-merge(spine,unique(t[,.(match_id,date)]),by="match_id")
 setorder(spine,player,date,match_id);spine[,seq:=seq_len(.N),by=player]
 bm<-b[,.(bat_runs=sum(score),inns_m=.N),by=.(player=batter,match_id)]
 wm<-w[,.(wk=sum(wickets),balls_m=sum(overs*balls_per_over+extraballs),runs_m=sum(runs)),by=.(player=bowler,match_id)]
 z<-merge(spine,bm,by=c("player","match_id"),all.x=TRUE);z<-merge(z,wm,by=c("player","match_id"),all.x=TRUE)
 for(v in c("bat_runs","inns_m","wk","balls_m","runs_m"))set(z,which(is.na(z[[v]])),v,0)
 cmp<-merge(z,pm,by=c("player","seq"),suffixes=c("_raw","_stored"))
 for(v in c("bat_runs","inns_m","wk","balls_m","runs_m"))rows[[length(rows)+1L]]<-data.table(comp=ck,check=paste0("player_match_",v),mismatch=sum(cmp[[paste0(v,"_raw")]]!=cmp[[paste0(v,"_stored")]]),n=nrow(cmp))
 sentinels[[ck]]<-b[,.(comp=ck,n=.N,positive_runs_zero_balls=sum(score>0 & ballsfaced==0),zero_balls=sum(ballsfaced==0))]
 # Birthdate-embedded key must be reconciled with the separate date field.
 pk<-unique(p[,.(player,birth_date)])
 pk[,embedded:=sub(".*\\{b([^}]+)\\}.*","\\1",player)]
 pk[,parsed:=suppressWarnings(as.IDate(embedded))]
 pk[,comp:=ck]; linkage[[ck]]<-pk
 # Exact start/end dates and named anomaly rows retained locally for adjudication.
 badids<-c("20250402_04862_Hamiltn","19650728_01799_Worcest","19720503_03552_Hove","20080530_01110_RoseBwl","20160716_03634_Sthprt")
 if(any(ids%in%badids)) {
  fwrite(t[match_id%in%badids],file.path(rev,"audit",paste0("exception_teams_",ck,".csv")))
  fwrite(b[match_id%in%badids],file.path(rev,"audit",paste0("exception_batting_",ck,".csv")))
  fwrite(w[match_id%in%badids],file.path(rev,"audit",paste0("exception_bowling_",ck,".csv")))
 }
 # Contradictory handedness and mixed/no style are flags for adjudication, not forced labels.
 rows[[length(rows)+1L]]<-data.table(comp=ck,check="contradictory_batting_hands_rows",mismatch=p[rh_bat==1 & lh_bat==1,.N],n=nrow(p))
 rows[[length(rows)+1L]]<-data.table(comp=ck,check="unknown_batting_hands_rows",mismatch=p[rh_bat==0 & lh_bat==0,.N],n=nrow(p))
 rows[[length(rows)+1L]]<-data.table(comp=ck,check="different_embedded_birthdate_keys",mismatch=pk[!is.na(parsed)&!is.na(birth_date)&parsed!=as.IDate(birth_date),.N],n=nrow(pk))
 targets[[ck]]<-unique(p[grepl("Kallis,|Shakib Al Hasan|Sobers,|Warne,|Anderson, JM|Muralitharan,|Procter,|Faulkner,|Rhodes, W|Botham,|Imran Khan|Haris Rauf|Naseem Shah",player),.SD,.SDcols=setdiff(names(p),c("match_id","teams","position"))])[,comp:=ck]
 cat(ck,"component checks complete\n")
}
fwrite(rbindlist(rows),file.path(rev,"audit/component_checks.csv"))
fwrite(rbindlist(sentinels),file.path(rev,"audit/balls_faced_sentinels.csv"))
fwrite(rbindlist(targets,fill=TRUE),file.path(rev,"audit/targeted_metadata.csv"))
fwrite(rbindlist(linkage,fill=TRUE),file.path(rev,"audit/player_key_birthdate_check.csv"))
# Read original/submitted paragraphs and identify only changed substantive lines.
old<-readLines(file.path(root,"submission/Fourie_allrounders.tex"),warn=FALSE)
new<-readLines(file.path(root,"jsa_submission/manuscript.tex"),warn=FALSE)
writeLines(new[nchar(new)>150 & !new%in%old],file.path(rev,"source_text/journal_changed_paragraphs.txt"))
cat("No statistical models run.\n")

# Versioned correction overlay. No correction is inferred solely from a balancing gap.
# Unknown auxiliary fields in inserted historical rows remain NA, never invented zeros.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
ledger<-list();totals<-list()
record<-function(id,comp,table,match_id,field,old,new,source,reason,n=1L) {
 ledger[[length(ledger)+1L]]<<-data.table(correction_id=id,comp,table,match_id,field,old=as.character(old),new=as.character(new),
 source,reason,affected_rows=n,reviewer="Codex source adjudication",date="2026-10-08")
}
source_url<-list(
 buss="https://i.imgci.com/db/ARCHIVE/1960S/1965/ENG_LOCAL/CC/WORCS_SUSSEX_CC_28-30JUL1965.html",
 prideaux="https://i.imgci.com/db/ARCHIVE/1970S/1972/ENG_LOCAL/CC/SUSSEX_ESSEX_CC_03-05MAY1972.html",
 cornford="https://i.imgci.com/db/ARCHIVE/1940S/1947/ENG_LOCAL/CC/SUSSEX_KENT_CC_09-12AUG1947.html",
 buck="https://www.cricketarchive.co.uk/Lancashire/Scorecards/898/898198.html",
 sufiyan="https://www.moneycontrol.com/sports/cricket/cricket-live-score/new-zealand-vs-pakistan/6611/nzpk04022025248581.html;Cricsheet:1443555",
 ridgway="https://i.imgci.com/db/ARCHIVE/1950S/1959/ENG_LOCAL/CC/NOTTS_KENT_CC_29-31JUL1959.html",
 inchmore="https://i.imgci.com/db/ARCHIVE/1970S/1979/ENG_LOCAL/CC/SURREY_WORCS_CC_25-27JUL1979.html",
 param="https://www.mykhel.com/cricket/malaysia-vs-singapore-2023-match-3-scorecard-58666/",
 abbas="https://www.sporty.co.nz/cricketwellington/squads/firebirds-player-profiles/muhammad-abbas;https://archive.nzc.nz/Players/2260/2260088/2260088.html",
 procter="https://cricket.co.za/wp-content/uploads/2024/09/12439-CSA-IR-FA.pdf",
 sobers="https://www.icc-cricket.com/hall-of-fame/hall-of-famers/hall-of-famer-garfield-sobers")
for(comp in names(configs)) {
 x<-readRDS(file.path(revision_dir,"data/staging",paste0("source_",comp,".rds")))
 before<-c(bat_rows=nrow(x$batting),bat_runs=sum(x$batting$score),bowl_rows=nrow(x$bowling),
   balls=sum(x$bowling$overs*x$bowling$balls_per_over+x$bowling$extraballs),conceded=sum(x$bowling$runs),wickets=sum(x$bowling$wickets),players=uniqueN(x$players$player))
 insert_bat<-function(id,mid,team,inn,pattern,pos,score,ref,extra=list()) {
   who<-unique(x$players[match_id==mid & teams==team & grepl(pattern,player),player])
   stopifnot(length(who)==1)
   present<-x$batting[match_id==mid & teams==team & innings==inn & batter==who]
   if(nrow(present)) {print(present);stop("Insertion conflicts with existing row: ",id)}
   row<-x$batting[NA_integer_]
   values<-c(list(match_id=mid,teams=team,innings=inn,player=pos,batter=who,howout="not out",score=score,out=0L,
     source_file=paste0("revision_overlay:",id),source_row=NA_integer_,vintage="verified_overlay"),extra)
   for(v in names(values))set(row,j=v,value=values[[v]])
   x$batting<<-rbind(x$batting,row,fill=TRUE)
   record(id,comp,"batting",mid,"row",NA,paste0(who,";position=",pos,";score=",score,";out=0"),ref,"Independently verified missing not-out innings")
 }
 replace_field<-function(id,tab,idx,field,expected,value,ref,reason) {
   stopifnot(length(idx)>0,all(x[[tab]][[field]][idx]==expected))
   mids<-paste(unique(x[[tab]]$match_id[idx]),collapse=";")
   record(id,comp,tab,mids,field,expected,value,ref,reason,length(idx))
   set(x[[tab]],i=idx,j=field,value=value)
 }
 if(comp=="county") {
   ref<-"https://cricketarchive.com/Middlesex/Scorecards/70/70420.html"
   idx<-which(x$batting$match_id=="20000821_01513_Bristol" & x$batting$innings==2 & grepl("^Johnson, RL ",x$batting$batter))
   replace_field("C014","batting",idx,"bowler","Ball, MCJ {b1970-4-26}","Smith, AM {b1967-10-1}",ref,"Johnson dismissed by Smith; independently verified dismissal card")
   idx<-which(x$batting$match_id=="20000821_01513_Bristol" & x$batting$innings==2 & grepl("^Fraser, ARC ",x$batting$batter))
   replace_field("C014","batting",idx,"bowler","Lewis, J {b1975-8-26}","Ball, MCJ {b1970-4-26}",ref,"Fraser dismissed by Ball; paired error concealed by unchanged Ball wicket total")
   insert_bat("C001","19650728_01799_Worcest",1L,2L,"^Buss, A ",10L,10L,source_url$buss)
   idx<-which(x$batting$match_id=="19720503_03552_Hove" & x$batting$teams==1 & x$batting$innings==2 & x$batting$player==4)
   replace_field("C002","batting",idx,"batter","Prideaux, RM {b1939-7-31}","Parks, JM {b1931-10-21}",source_url$prideaux,"Existing 7-not-out row belongs to Parks; independently verified alongside missing Prideaux innings")
   insert_bat("C002","19720503_03552_Hove",1L,2L,"^Prideaux, RM ",3L,14L,source_url$prideaux)
   insert_bat("C003","19470809_00522_Hstngs",2L,2L,"^Cornford, JH ",11L,0L,source_url$cornford)
   insert_bat("C004","20160716_03634_Sthprt",1L,2L,"^Buck, NL ",10L,27L,source_url$buck,list(fours=2L,sixes=2L,ballsfaced=46L,time=53L))
   mid<-"20160716_03634_Sthprt";who<-x$players[match_id==mid & grepl("^Buck, NL ",player),player]
   stopifnot(length(who)==1,nrow(x$bowling[match_id==mid & bowler==who & innings==2])==0)
   row<-x$bowling[NA_integer_]
   vals<-list(match_id=mid,balls_per_over=6L,teams=1L,innings=2L,player=2L,bowler=who,overs=4L,maidens=0L,runs=26L,wickets=0L,
     extraballs=0L,noballs=0L,wides=0L,hat_trick=0L,source_file="revision_overlay:C005",source_row=NA_integer_,vintage="verified_overlay")
   for(v in names(vals))set(row,j=v,value=vals[[v]])
   x$bowling<-rbind(x$bowling,row,fill=TRUE)
   record("C005",comp,"bowling",mid,"row",NA,paste0(who,";overs=4;balls=24;runs=26;wickets=0"),source_url$buck,"Replacement player's entire bowling spell omitted")
   idx<-which(x$batting$match_id=="19590729_04166_Wrksp" & x$batting$innings==2 & grepl("^Winfield, HM ",x$batting$batter))
   replace_field("C006","batting",idx,"bowler","Brown, A {b1935-10-17}","Ridgway, F {b1923-8-10}",source_url$ridgway,"Dismissal attribution conflicts with independently verified bowling card")
   idx<-which(x$batting$match_id=="19790725_01015_Guildfd" & x$batting$innings==2 & grepl("^Howarth, GP ",x$batting$batter))
   replace_field("C007","batting",idx,"bowler","Humphries, DJ {b1953-8-6}","Inchmore, JD {b1949-2-22}",source_url$inchmore,"Wicketkeeper was incorrectly named as dismissing bowler")
 }
 if(comp=="odis") {
   insert_bat("C008","20250402_04862_Hamiltn",2L,1L,"^Sufiyan Muqeem ",12L,13L,source_url$sufiyan)
   # Scope identity repair to line-ups actually representing New Zealand. Do not globally rename the Pakistani player.
   nt<-unique(x$teams[team=="New Zealand",.(match_id,teams)])
   old<-"Mohammad Abbas {b1990-3-10}";new<-"Abbas, MA {b2003-11-29}"
   affected<-merge(x$players[player==old],nt,by=c("match_id","teams"))$match_id
   stopifnot(length(affected)>0)
   identity_fields<-c("player","batter","bowler","fielder","captain","wicketkeeper","player1_of_match","player2_of_match","player3_of_match","player4_of_match")
   for(tab in names(x))for(v in intersect(identity_fields,names(x[[tab]]))) {
     if(!is.character(x[[tab]][[v]]))next
     idx<-which(x[[tab]]$match_id%in%affected & x[[tab]][[v]]==old)
     if(length(idx))replace_field("C009",tab,idx,v,old,new,source_url$abbas,"Split New Zealand all-rounder from Pakistani bowler; match/team-scoped identity correction")
   }
   idx<-which(x$players$match_id%in%affected & x$players$player==new)
   set(x$players,i=idx,j="birth_date",value=as.IDate("2003-11-29"))
   # Birth/style are identity-specific; copying the Pakistani player's metadata would preserve the collision.
   vals<-list(rh_bat=1L,lh_bat=0L,rh_bowl=0L,lh_bowl=1L,fast=0L,fast_medium=1L,medium_fast=0L,medium=0L,slow_medium=0L,lob=0L,seam=0L,off_break=0L,leg_break=0L,googly=0L)
   record("C009",comp,"players",paste(affected,collapse=";"),"birth_date","1990-03-10","2003-11-29",source_url$abbas,"Correct date for distinct New Zealand identity",length(idx))
   for(v in names(vals)) {
     oldv<-paste(unique(x$players[[v]][idx]),collapse=";")
     if(!all(x$players[[v]][idx]==vals[[v]]))record("C009",comp,"players",paste(affected,collapse=";"),v,oldv,vals[[v]],source_url$abbas,"Metadata belong to corrected New Zealand identity",length(idx))
     set(x$players,i=idx,j=v,value=vals[[v]])
   }
 }
 if(comp=="t20is") {
   insert_bat("C013","20211021_01337_Almra",1L,1L,"^Dernbach, JW ",11L,0L,"Cricsheet archive member 1282748.json; registry 7be8a0ed; Italy innings")
   idx<-which(x$batting$match_id=="20230928_02257_Hngzh" & x$batting$bowler=="Param, NMS {b1995-10-21}")
   replace_field("C010","batting",idx,"bowler","Param, NMS {b1995-10-21}","Param, AE {b1990-7-19}",source_url$param,"Three dismissals credited to Navin instead of Anish in batting table; bowling table already correct")
 }
 # Keep repertoire classification distinct from a fabricated match-level delivery style.
 x$players[,style_override:=NA_character_]
 idx<-which(grepl("^Sobers, GS ",x$players$player))
 if(length(idx)) {
   set(x$players,i=idx,j="style_override",value="mixed")
   record("C011",comp,"players","all Sobers appearances","style_override",NA,"mixed",source_url$sobers,"Career repertoire override only; does not assert style used in each match",length(idx))
 }
 idx<-which(grepl("^Procter, MJ ",x$players$player) & as.character(x$players$death_date)=="2023-02-17")
 if(length(idx)) {
   set(x$players,i=idx,j="death_date",value=as.IDate("2024-02-17"))
   record("C012",comp,"players","all Mike Procter appearances","death_date","2023-02-17","2024-02-17",source_url$procter,"Death year independently verified against CSA and ICC",length(idx))
 }
 assert_unique(x$players,c("match_id","player"),paste(comp,"corrected line-up"))
 assert_unique(x$batting,c("match_id","teams","innings","batter"),paste(comp,"corrected batting"))
 assert_unique(x$bowling,c("match_id","teams","innings","bowler"),paste(comp,"corrected bowling"))
 after<-c(bat_rows=nrow(x$batting),bat_runs=sum(x$batting$score),bowl_rows=nrow(x$bowling),
   balls=sum(x$bowling$overs*x$bowling$balls_per_over+x$bowling$extraballs),conceded=sum(x$bowling$runs),wickets=sum(x$bowling$wickets),players=uniqueN(x$players$player))
 totals[[comp]]<-data.table(comp,field=names(before),original=unname(before),corrected=unname(after),delta=unname(after-before))
 saveRDS(x,file.path(revision_dir,"data/corrected",paste0("source_",comp,".rds")),compress="gzip")
 cat(comp,"verified correction overlay applied\n")
}
write_audit(rbindlist(ledger,fill=TRUE),"correction_ledger_v3")
write_audit(rbindlist(totals),"correction_totals_v3")
cat("Corrected source copies are provisional; the analysis gate is still closed.\n")

# Broaden the identity audit to EVERY uniquely linked match in the local Cricsheet archive.
# Registry identifiers provide an independent identity signal, not certified DOB/style metadata.
# Conservative within-team name links only; collisions are queries, never automatic global renames.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
setDTthreads(1) # Many small joins; parallel dispatch costs more than these joins.
canon<-function(s) {
 s<-gsub(" *\\([^)]*\\)","",sub(" \\{b.*","",s))
 comma<-grepl(",",s);s[comma]<-sub("^([^,]+), (.*)$","\\2 \\1",s[comma])
 toupper(gsub("[^a-zA-Z0-9]","",iconv(s,to="ASCII//TRANSLIT")))
}
manifest<-read_csv(file.path(book_data,"raw/cricsheet/MANIFEST_matches.csv"))[gender=="male" & date<=as.IDate("2025-12-31")]
archive<-file.path(book_data,"raw/cricsheet/all_json.zip")
read_info<-function(id) {
 con<-unz(archive,paste0(id,".json"),open="r");on.exit(close(con))
 lines<-character()
 repeat {
  more<-readLines(con,n=256,warn=FALSE);lines<-c(lines,more)
  end<-grep('^  "innings":',lines)
  if(length(end))break
  if(!length(more))stop("Missing innings boundary: ",id)
 }
 prefix<-lines[seq_len(end[1]-1L)];prefix[length(prefix)]<-sub(",$","",prefix[length(prefix)])
 tryCatch(jsonlite::fromJSON(paste(c(prefix,"}"),collapse="\n"),simplifyVector=FALSE)$info,error=function(e) {
  cat("Header layout fallback for Cricsheet",id,"\n")
  # Some Windows unz text connections truncate despite a valid complete archive member.
  # Extract and parse the physical file before attributing a failure to the source.
  dest<-file.path(revision_dir,"sources/cricsheet")
  unzip(archive,files=paste0(id,".json"),exdir=dest)
  jsonlite::fromJSON(file.path(dest,paste0(id,".json")),simplifyVector=FALSE)$info
 })
}
types<-c(tests="Test",odis="ODI",t20is="T20",county="CCH",sheffield="SSH")
allrows<-list();coverage<-list()
for(co in names(types)) {
 x<-readRDS(file.path(revision_dir,"data/corrected",paste0("source_",co,".rds")))
 tm<-unique(x$teams[,.(match_id,date=as.IDate(start_date),teams,team)])
 mm<-tm[,.(pair=paste(sort(canon(team)),collapse="|")),by=.(match_id,date)]
 cs<-copy(manifest[match_type%in%c(types[[co]],if(co=="county")"BWT")])
 cs[,pair:=mapply(function(a,b)paste(sort(c(canon(a),canon(b))),collapse="|"),team1,team2)]
 setnames(cs,"match_id","cricsheet_id");linked<-merge(mm,cs,by=c("date","pair"))
 bad<-linked[,.N,by=match_id][N>1,match_id];badcs<-linked[,.N,by=cricsheet_id][N>1,cricsheet_id]
 linked<-linked[!match_id%in%bad & !cricsheet_id%in%badcs];setorder(linked,date,match_id)
 cache<-file.path(revision_dir,"audit/execution",paste0("registry_appearances_",co,".csv"))
 src<-file.path(revision_dir,"data/corrected",paste0("source_",co,".rds"))
 if("--resume"%in%commandArgs(TRUE) && file.exists(cache) && file.info(cache)$mtime>file.info(src)$mtime) {
  z<-read_csv(cache)
  stopifnot(setequal(unique(z$match_id),linked$match_id))
  allrows[[co]]<-z
  coverage[[co]]<-data.table(comp=co,source_matches=nrow(mm),linked_matches=nrow(linked),registry_appearances=nrow(z),name_linked=sum(!is.na(z$player)),unresolved_names=sum(is.na(z$player)))
  cat(co,"resumed completed competition cache\n");next
 }
 players<-x$players[,.(match_id,teams,player)];players[,canonical:=canon(player)]
 rows<-vector("list",nrow(linked))
 checkpoint<-file.path(revision_dir,"data/staging",paste0("registry_checkpoint_",co,".rds"))
 completed<-0L
 if("--resume"%in%commandArgs(TRUE) && file.exists(checkpoint) && file.info(checkpoint)$mtime>file.info(src)$mtime) {
  cp<-readRDS(checkpoint);stopifnot(identical(cp$match_ids,linked$match_id))
  completed<-cp$completed;rows[seq_len(completed)]<-cp$rows
  cat(co,"resumed",completed,"match headers\n")
 }
 for(j in seq.int(completed+1L,nrow(linked))) {
  mid<-linked$match_id[j];cid<-linked$cricsheet_id[j];info<-read_info(cid)
  cards<-rbindlist(lapply(names(info$players),function(team_name) {
   ns<-unlist(info$players[[team_name]])
   data.table(team=team_name,name=ns,registry_id=vapply(ns,function(nm)info$registry$people[[nm]],character(1)))
  }))
  team_map<-tm[match_id==mid]
  cards[,teams:=team_map$teams[match(canon(team),canon(team_map$team))]]
  stopifnot(!anyNA(cards$teams))
  cards[,canonical:=canon(name)];p<-players[match_id==mid]
  # Exact canonical matches must be unique within a team on both sides.
  ambp<-p[,.N,by=.(teams,canonical)][N>1];ambc<-cards[,.N,by=.(teams,canonical)][N>1]
  p<-p[!ambp,on=.(teams,canonical)];exact<-merge(cards,p[,.(teams,canonical,player)],by=c("teams","canonical"),all.x=TRUE)
  if(nrow(ambc))exact[ambc,on=.(teams,canonical),player:=NA_character_]
  exact[,method:=ifelse(is.na(player),"unresolved","exact_canonical_within_team")]
  for(k in which(is.na(exact$player))) {
   nm<-exact$name[k];surname<-sub("^[^ ]+ ","",nm);first<-substr(nm,1,1)
   candidates<-p[teams==exact$teams[k] & grepl(",",player)]
   candidates<-candidates[canon(sub(",.*","",player))==canon(surname) & substr(sub("^[^,]+, *","",player),1,1)==first]
   if(nrow(candidates)==1 && !candidates$player%in%exact$player) {
    exact$player[k]<-candidates$player;exact$method[k]<-"unique_surname_first_initial_within_team"
   }
  }
  exact[,`:=`(comp=co,match_id=mid,cricsheet_id=cid,date=linked$date[j])];rows[[j]]<-exact
  if(j%%250==0) {
   saveRDS(list(completed=j,match_ids=linked$match_id,rows=rows[seq_len(j)]),checkpoint)
   cat(co,j,"of",nrow(linked),"registry cards checked\n")
  }
 }
 z<-rbindlist(rows);allrows[[co]]<-z
 write_audit(z,paste0("registry_appearances_",co))
 coverage[[co]]<-data.table(comp=co,source_matches=nrow(mm),linked_matches=nrow(linked),registry_appearances=nrow(z),name_linked=sum(!is.na(z$player)),unresolved_names=sum(is.na(z$player)))
 cat(co,"identity registry complete\n")
}
z<-rbindlist(allrows)
edges<-z[!is.na(player),.(appearances=.N,first_date=min(date),last_date=max(date),methods=paste(sort(unique(method)),collapse=";")),by=.(comp,player,registry_id,name,team)]
multi<-edges[,.(n_registry=uniqueN(registry_id),registry_ids=paste(sort(unique(registry_id)),collapse=";"),teams=paste(sort(unique(team)),collapse=";"),appearances=sum(appearances)),by=player][n_registry>1]
splits<-edges[,.(n_source_keys=uniqueN(player),source_keys=paste(sort(unique(player)),collapse=";"),appearances=sum(appearances)),by=registry_id][n_source_keys>1]
write_audit(edges,"registry_identity_edges");write_audit(multi,"registry_collision_queries");write_audit(splits,"registry_split_queries")
write_audit(rbindlist(coverage),"registry_identity_coverage")
write_audit(z[is.na(player)],"registry_unresolved_names")
cat(nrow(multi),"source keys map to multiple registry identities;",nrow(splits),"registry identities map to multiple source keys. No automatic corrections applied.\n")

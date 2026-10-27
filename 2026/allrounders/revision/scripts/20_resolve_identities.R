# Additional evidence-based, team-scoped repairs. Run after 09, before 10.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'revision_common.R'))
setDTthreads(2)
rules<-rbindlist(list(
 data.table(id='I008',old='Robinson, RT {b1940-9-15}',new='Robinson, RT {b1958-11-21}',team='Nottinghamshire',url='https://www.trentbridge.co.uk/trentbridge/history/players/tim-robinson.html'),
 data.table(id='I001',old='Pretorius, D {b1977-12-6}',new='Pretorius, D {b1989-3-29}',team='Northamptonshire',url='https://nccc.co.uk/news/wakely-pretorius-and-sanderson-extend-dominance/'),
 data.table(id='I002',old='Saini, N {b1988-10-28}',new='Saini, N {b1992-11-23}',team=c('Kent','Worcestershire'),url='https://www.kentcricket.co.uk/profile/navdeep-saini/'),
 data.table(id='I003',old='Patel, AR {b1994-7-31}',new='Patel, AR {b1994-1-20}',team='Durham',url='https://www.durhamcricket.co.uk/news-and-media/lees-patel-set-for-durham-debuts/'),
 data.table(id='I004',old='Sandhu, GS {b1992-3-24}',new='Sandhu, GS {b1993-6-14}',team='Sussex',url='https://sussexcricket.co.uk/news/durham-vs-sussex;https://www.cricket.com.au/players/CA%3A1048/gurinder-sandhu'),
 data.table(id='I005a',old='Haider Ali {b0-0-0}',new='Haider Ali {b2000-10-2}',team=c('Pakistan','Derbyshire'),url='https://www.pcb.com.pk/player/haider-ali-46727.html'),
 data.table(id='I005b',old='Haider Ali {b0-0-0}',new='Haider Ali {b1994-7-5}',team='United Arab Emirates',url='https://www.pcb.com.pk/player-detail.php?action=view_profile&player_id=39799;https://www.icc-cricket.com/rankings/70196/haider-ali'),
 data.table(id='I006',old='Yasir Ali {b1996-3-6}',new='Yasir Ali (Sp) {b1991-12-9}',team='Spain',url='https://www.cricketspain.es/es-es/team/yasir-ali'),
 data.table(id='I007',old='Usman Khan {b1995-5-10}',new='Usman Khan (Fr) {b1990-10-6}',team='France',url='https://www.pcb.com.pk/player/usman-khan-47987.html;https://www.francecricket.com/wp-content/uploads/EDF-MAS-2021.pdf;https://www.icc-cricket.com/rankings/86975/usman-khan')
))
fwrite(rules,file.path(revision_dir,'config/verified_identity_rules.csv'))
allsource<-lapply(names(configs),function(comp)readRDS(file.path(revision_dir,'data/corrected',paste0('source_',comp,'.rds'))));names(allsource)<-names(configs)
metadata<-rbindlist(lapply(allsource,function(x)x$players),fill=TRUE)
fields<-c('birth_date','death_date','rh_bat','lh_bat','rh_bowl','lh_bowl','fast','fast_medium','medium_fast','medium','slow_medium','seam','lob','off_break','leg_break','googly','style_override')
ledger<-list();changes<-list()
for(comp in names(configs)) {
 x<-allsource[[comp]]
 for(i in seq_len(nrow(rules))) {
  rule<-rules[i];lineup<-merge(x$players[player==rule$old],unique(x$teams[team==rule$team,.(match_id,teams)]),by=c('match_id','teams'))
  mids<-unique(lineup$match_id);if(!length(mids))next
  stopifnot(all(x$players[match_id%in%mids & player==rule$old,.N,by=match_id]$N==1))
  for(tab in names(x))for(v in intersect(c('player','batter','bowler','fielder','captain','wicketkeeper','player1_of_match','player2_of_match','player3_of_match','player4_of_match'),names(x[[tab]]))) {
   if(!is.character(x[[tab]][[v]]))next
   idx<-which(x[[tab]]$match_id%in%mids & x[[tab]][[v]]==rule$old)
   if(length(idx)){set(x[[tab]],idx,v,rule$new);ledger[[length(ledger)+1L]]<-data.table(id=rule$id,comp,table=tab,field=v,n=length(idx),old=rule$old,new=rule$new,team=rule$team,url=rule$url)}
  }
  idx<-which(x$players$match_id%in%mids & x$players$player==rule$new)
  donor<-metadata[player==rule$new]
  for(v in intersect(fields,names(x$players))) {
   # Copy identity-specific existing metadata only where unanimous; unused in models.
   vals<-if(nrow(donor))unique(donor[[v]])else NULL
   val<-if(length(vals)==1)vals else NA
   set(x$players,idx,v,val)
  }
  if(rule$id=='I005a')set(x$players,idx,'birth_date',as.IDate('2000-10-02'))
  changes[[length(changes)+1L]]<-data.table(comp,match_id=mids,old=rule$old,new=rule$new,id=rule$id)
 }
 stopifnot(!anyDuplicated(x$players,by=c('match_id','player')))
 saveRDS(x,file.path(revision_dir,'data/corrected',paste0('source_',comp,'.rds')))
}
expected_counts<-c(I001=1L,I002=3L,I003=4L,I004=4L,I005a=47L,I005b=16L,I006=16L,I007=12L,I008=180L)
observed_counts<-rbindlist(changes)[,.N,by=id]
stopifnot(setequal(observed_counts$id,names(expected_counts)),all(observed_counts$N==expected_counts[observed_counts$id]))
write_audit(rbindlist(ledger),'identity_correction_ledger')
write_audit(rbindlist(changes),'identity_corrected_appearances')
# Clear only source-key families whose complete observed team scopes were reviewed.
fwrite(data.table(player=unique(c(rules$old,rules$new))),file.path(revision_dir,'config/cleared_identity_keys.csv'))
cat('Applied',uniqueN(rbindlist(ledger)$id),'scoped identity rules\n')

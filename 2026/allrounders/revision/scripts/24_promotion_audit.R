source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('domestic-to-Test observed transitions')
dd<-rbindlist(lapply(c('county','sheffield'),function(co){z<-readRDS(file.path(revision_dir,'data/analysis',paste0('player_match_',co,'.rds')));z[,comp:=co];z}),fill=TRUE)
setorder(dd,player,start_date,comp,match_id)
entry<-dd[,.(entry=min(start_date),entry_comp=comp[1],entry_team=team[1],last_end=max(end_date),tie=uniqueN(comp[start_date==min(start_date)])>1),by=player]
tt<-readRDS(file.path(revision_dir,'data/analysis/player_match_tests.rds'))[,.(test=min(start_date)),by=player]
entry<-merge(entry,tt,by='player',all.x=TRUE)
entry[,limit:=pmin(entry+1826L,cutoff)]
meta<-rbindlist(lapply(c('county','sheffield'),function(co)readRDS(file.path(revision_dir,'data/corrected',paste0('source_',co,'.rds')))$players))
dm<-unique(meta[,.(player,death=as.IDate(death_date))])[!is.na(death)&death<as.IDate('2026-01-01')]
entry<-merge(entry,dm,by='player',all.x=TRUE,allow.cartesian=TRUE)
write_audit(entry[!is.na(death)&death>=entry & death<=limit & (is.na(test)|death<test)],'promotion_death_queries')
write_audit(entry[tie==TRUE],'promotion_entry_ties')
write_audit(entry[entry_comp=='county'&entry<as.IDate('1950-01-01')|entry_comp=='sheffield'&entry<as.IDate('1897-01-01')],'promotion_boundary_entries')
cat('Death and entry-date audit completed\n')

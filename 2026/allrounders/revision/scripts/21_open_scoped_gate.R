# A gate is an explicit permission for a bounded, checked dataset, not a claim
# that every historic source field has been certified. No substantive estimation.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'revision_common.R'))
setDTthreads(2)
original<-read_csv(file.path(revision_dir,'audit/baseline_manifest.csv'))
original<-original[grepl('^data[/\\\\]raw[/\\\\]daft[/\\\\]',path)]
stopifnot(nrow(original)==35)
current_hash<-vapply(file.path(project_dir,gsub('\\\\','/',original$path)),function(f)toupper(digest::digest(file=f,algo='sha256')),character(1))
stopifnot(all(current_hash==original$sha256))
dir.create(file.path(revision_dir,'data/analysis'),recursive=TRUE,showWarnings=FALSE)
queries<-unique(c(read_csv(file.path(revision_dir,'audit/execution/registry_collision_queries.csv'))$player,
 unlist(strsplit(read_csv(file.path(revision_dir,'audit/execution/registry_split_queries.csv'))$source_keys,';',fixed=TRUE)),
 read_csv(file.path(revision_dir,'audit/execution/same_day_national_identity_queries.csv'))$player))
cleared<-utils::read.csv(file.path(revision_dir,'config/cleared_identity_keys.csv'),stringsAsFactors=FALSE)$player
excluded<-setdiff(queries,cleared)
fwrite(data.table(player=excluded,decision='Exclude all appearances in all competitions',reason='Unresolved identity or registry-link query; registry equivalence is not proof'),file.path(revision_dir,'audit/execution/identity_quarantine.csv'))
coverage<-list();badmatches<-list();allpm<-list()
county_clubs<-c('Derbyshire','Durham','Essex','Glamorgan','Gloucestershire','Hampshire','Kent','Lancashire','Leicestershire','Middlesex','Northamptonshire','Nottinghamshire','Somerset','Surrey','Sussex','Warwickshire','Worcestershire','Yorkshire')
for(comp in names(configs)) {
 p<-readRDS(file.path(revision_dir,'data/corrected',paste0('player_match_',comp,'.rds')))
 wrong_competition<-if(comp=='county')unique(p[!team%in%county_clubs,match_id])else character()
 if(comp=='county')write_audit(unique(p[match_id%in%wrong_competition,.(match_id,start_date,team,competition)]),'county_noncompetition_fixtures')
 p[,identity_registry_query:=player%in%excluded]
 p[,exclusion_reason:=fcase(match_id%in%wrong_competition,'Non-county opposition; outside competition',identity_registry_query,'unresolved identity',source_accounting_query,'unresolved match accounting',external_source_query,'unresolved external source',default='accepted')]
 badmatches[[comp]]<-unique(p[source_accounting_query==TRUE,.(comp=comp,match_id,player)])
 coverage[[comp]]<-cbind(comp=comp,p[,.(player_matches=.N,players=uniqueN(player),matches=uniqueN(match_id)),by=exclusion_reason])
 p[,data_gate_passed:=exclusion_reason=='accepted']
 # Keep unknown keeper nominations unknown, rather than silently coding nonkeepers.
 p[wicketkeeper_nomination_query==TRUE,wicketkeeper_nominated:=NA]
 # Unverified biographical fields are deliberately not available to estimation.
 p[,c('birth','death','age_at_start','birth_date','death_date','batting_hand','bowling_style','pace','spin'):=NULL]
 p<-p[data_gate_passed==TRUE]
 p[,match_sequence:=seq_len(.N),by=player]
 stopifnot(!anyNA(p[,.(bat_runs,wickets,scorecard_balls,runs_conceded,start_date,end_date)]),all(p$end_date>=p$start_date),!anyDuplicated(p,by=c('match_id','player')))
 saveRDS(p,file.path(revision_dir,'data/analysis',paste0('player_match_',comp,'.rds')))
 allpm[[comp]]<-p[,.(comp=comp,player,team,start_date,match_id)]
}
z<-rbindlist(allpm)[comp%in%c('tests','odis','t20is')]
# No remaining impossible same-day country appearances. Legitimate national switches are retained.
same<-z[,.(nteam=uniqueN(team)),by=.(player,start_date)][nteam>1]
stopifnot(nrow(same)==0)
checks<-read_csv(file.path(revision_dir,'audit/execution/rebuilt_component_checks.csv'))
stopifnot(checks[check%in%c('batting_missing_lineup','bowling_missing_lineup','corrected_bowler_credit_queries','corrected_allout_completeness_queries'),sum(n)]==0)
pilot<-read_csv(file.path(revision_dir,'audit/execution/implementation_pilot_results.csv'))
write_audit(rbindlist(coverage),'analysis_gate_coverage')
write_audit(rbindlist(badmatches),'match_accounting_quarantine')
jsonlite::write_json(list(status='OPEN',updated='2026-10-08',scope=c('verified scorecard output','workload and composition','output uncertainty','chronologically held-out prediction','domestic-to-Test observed transitions'),
 estimation_authorised=TRUE,excluded_scopes=c('age-adjusted inference','bowling-style inference','national-eligibility selector models','causal roster value'),
 excluded_identity_keys=length(excluded),identity_quarantine='audit/execution/identity_quarantine.csv',match_quarantine='audit/execution/match_accounting_quarantine.csv',
 reason='Confirmed errors repaired; unresolved source identities, nine non-county fixtures and one accounting-query match excluded; unverified biography removed from estimation inputs. Gate certifies stated checks and bounds, not flawless historical data.',
 paid_relabelling_authorised=FALSE,human_role_labels='declined; role pilot verifies implementation only',reviewer='Codex source and code audit'),file.path(revision_dir,'config/data_gate.json'),pretty=TRUE,auto_unbox=TRUE)
cat('OPEN for stated scorecard analyses;',length(excluded),'identity keys excluded\n')

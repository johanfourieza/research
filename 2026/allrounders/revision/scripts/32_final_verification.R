source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('verified scorecard output')
checks<-list()
check<-function(name,ok,detail='') {
 checks[[length(checks)+1L]]<<-data.table(check=name,passed=isTRUE(ok),detail=as.character(detail))
 if(!isTRUE(ok))stop('FINAL CHECK FAILED: ',name,': ',detail)
}
manifest<-read_csv(file.path(revision_dir,'audit/baseline_manifest.csv'))
licensed_replication<-'--licensed-replication'%in%commandArgs(TRUE)
if(licensed_replication) {
 manifest<-manifest[grepl('^data[/\\\\]raw[/\\\\]daft[/\\\\]',path)]
 stopifnot(nrow(manifest)==35)
 cat('Explicit licensed-replication mode: checking 35 source hashes; author-workspace nondata preservation check is not attempted.\n')
}
actual<-vapply(file.path(project_dir,gsub('\\\\','/',manifest$path)),function(f)toupper(digest::digest(file=f,algo='sha256')),character(1))
check(if(licensed_replication)'Licensed source hashes match'else'Original files unchanged',all(actual==manifest$sha256),paste(nrow(manifest),'SHA-256 hashes'))
pilot<-read_csv(file.path(revision_dir,'audit/execution/implementation_pilot_results.csv'))
check('Independent implementation pilot',nrow(pilot)==480 && all(pilot$agree),'60 careers, eight fields, deterministic role agreement; no human labels')
identity_changes<-read_csv(file.path(revision_dir,'audit/execution/identity_corrected_appearances.csv'))[,.N,by=id]
identity_contract<-c(I001=1L,I002=3L,I003=4L,I004=4L,I005a=47L,I005b=16L,I006=16L,I007=12L,I008=180L)
check('Scoped identity correction ledger contract',setequal(identity_changes$id,names(identity_contract)) && all(identity_changes$N==identity_contract[identity_changes$id]),'Nine rules; Robinson scope contains 180 Nottinghamshire appearances')
quarantine<-utils::read.csv(file.path(revision_dir,'audit/execution/identity_quarantine.csv'))$player
totals<-list();link<-list()
for(co in names(configs)) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',co,'.rds')));p<-a$pm
 x<-readRDS(file.path(revision_dir,'data/corrected',paste0('source_',co,'.rds')))
 check(paste(co,'unique accepted appearances'),!anyDuplicated(p,by=c('match_id','player')) && all(p$data_gate_passed))
 check(paste(co,'identity exclusion'),!any(p$player%in%quarantine))
 keys<-p[,.(match_id,player)]
 b<-x$batting[keys,on=.(match_id,batter=player),nomatch=0]
 w<-x$bowling[keys,on=.(match_id,bowler=player),nomatch=0]
 vals<-c(runs=sum(b$score)-sum(p$bat_runs),innings=nrow(b)-sum(p$innings_batted),dismissals=sum(b$out)-sum(p$dismissals),wickets=sum(w$wickets)-sum(p$wickets),balls=sum(w$overs*w$balls_per_over+w$extraballs)-sum(p$scorecard_balls),conceded=sum(w$runs)-sum(p$runs_conceded))
 check(paste(co,'source-to-analysis conservation'),all(vals==0),paste(names(vals),vals,collapse='; '))
 check(paste(co,'career conservation'),all(vapply(c('bat_runs','innings_batted','dismissals','wickets','scorecard_balls','runs_conceded'),function(v)sum(a$career[[v]])==sum(p[[v]]),logical(1))))
 f<-readRDS(file.path(revision_dir,'data/analysis',paste0('forecast_',co,'.rds')))
 check(paste(co,'chronological embargo'),max(f[partition=='training',horizon])<min(f[partition=='test',landmark]))
 check(paste(co,'past-only currency'),all(f$reference_last_date<f$landmark))
 # Independently reaggregate the bounded future, including end-date rather than start-date inclusion.
 ff<-merge(p,f[,.(player,landmark,horizon)],by='player')[start_date>landmark & end_date<=horizon,.(future_matches=.N,future_runs=sum(bat_runs),future_wickets=sum(wickets)),by=player]
 ff<-merge(f[,.(player,future_matches,future_runs,future_wickets)],ff,by='player',all.x=TRUE,suffixes=c('_saved','_check'))
 for(v in c('future_matches','future_runs','future_wickets')) {
  expected<-ff[[paste0(v,'_check')]];expected[is.na(expected)]<-0
  check(paste(co,'future window',v),all(ff[[paste0(v,'_saved')]]==expected))
 }
 totals[[co]]<-data.table(comp=co,matches=uniqueN(p$match_id),players=uniqueN(p$player),appearances=nrow(p))
 link[[co]]<-p[,.(first_date=min(start_date),first_team=team[which.min(start_date)]),by=player]
}
sm<-rbindlist(totals)
check('Final sample totals',sum(sm$appearances)==613532 && sum(sm$players)==17507 && sum(sm$matches)==27952)
read_result<-function(n)read_csv(file.path(results_dir,paste0(n,'.csv')))
r<-read_result('reml_independent_checks');check('REML versus lme4',all(r$max_error<r$tolerance))
r<-read_result('copula_independent_checks');check('Copula versus mvtnorm',max(abs(r$ours-r$reference))<1e-7)
r<-read_result('share_rca_prediction_equivalence');check('Share and RCA prediction equivalence',max(r$max_share_rca_prediction_difference)<1e-6)
r<-read_result('eb_fits');check('All baseline REML fits converge',all(r$convergence==0))
for(co in c('tests','county','sheffield')) {
 bfit<-r[comp==co & discipline=='bowl',.(mu,sigma2,tau2)]
 nfit<-r[comp==co & discipline=='native',.(mu,sigma2,tau2)]
 check(paste(co,'wicket-price cancellation'),nrow(bfit)==1 && nrow(nfit)==1 && max(abs(unlist(bfit)-unlist(nfit)))<1e-8)
}
r<-read_result('dual_bootstrap');check('All 1,000 joint resamples converge',nrow(r)==1000 && all(r$converged))
r<-read_result('dual_counts');check('Dependence bounds contain independence',all(r$frechet_certain<=r$independent & r$independent<=r$frechet_possible & r$expected_lower<=r$expected_independent & r$expected_independent<=r$expected_upper))
r<-read_result('threshold_grid');check('Complete threshold grid',nrow(r)==1080 && all(r[, .N, by=comp]$N==216))
r<-read_result('bowling_margin_decomposition');check('Three-margin accounting identity',max(abs(r$wickets_per_match-r$bowling_participation*r$balls_conditional*r$wickets_per_six_balls/6))<1e-10)
r<-read_result('prospective_output_gaps');check('Holm ten-contrast family',max(abs(r$p_holm_primary-p.adjust(r$p,'holm',n=10)))<1e-12)
r<-read_result('promotion_cox');check('Holm three-hazard family',max(abs(r$p_holm-p.adjust(r$p,'holm')))<1e-12 && all(is.finite(r$se_jackknife)))
s<-read_result('promotion_adjustment_sequence')
check('Adjustment sequence uses same entrant sample',nrow(s)==3 && all(s$players==5221) && all(s$events==463) && all(s$clusters==24) && all(is.finite(s$lower+s$upper)))
check('Adjustment sequence final row equals primary',abs(s[specification=='Add prior output rates',HR]-r[scope=='pooled',HR])<1e-12)
pr<-readRDS(file.path(revision_dir,'data/analysis/promotion.rds'));rr<-pr$risk;ee<-pr$entry
check('Transition cohort and events',nrow(ee)==5221 && sum(ee$status==1)==463 && sum(rr$event)==463 && !anyDuplicated(ee$player))
check('Past-only transition covariates',all(is.na(rr$last_completed)|rr$last_completed<rr$calendar_start))
check('Completion updates precede next-day risk sets',all(abs(rr[tstart>0,as.numeric(calendar_start-last_completed)]-.5)<1e-12))
ld<-read_result('promotion_landmark_differences')
check('Landmark bootstrap precision',ld[comp=='county',evaluated_resamples]==1000000 && ld[comp=='sheffield',evaluated_resamples]==462 && all(ld$valid_probability>0.999999))
check('Positive disjoint risk intervals',all(rr$tstop>rr$tstart) && all(rr[,all(tstart[-1]==head(tstop,-1)),by=player]$V1))
check('One terminal event per person',all(rr[,sum(event),by=player]$V1<=1))
check('Gate excludes unsupported inference',inherits(try(require_data_gate('age-adjusted inference'),silent=TRUE),'try-error'))
for(f in list.files(file.path(revision_dir,'scripts'),pattern='[.]R$',full.names=TRUE))parse(f)
check('All revision R scripts parse',TRUE)
write_audit(rbindlist(checks),'final_verification')
capture.output(sessionInfo(),file=file.path(revision_dir,'audit/execution/final_session_info.txt'))
jsonlite::write_json(list(status='PASS',date=as.character(Sys.Date()),checks=length(checks),original_files=nrow(manifest),scope='Audited and bounded scorecard-output analyses',source_limit='Quarantine and documented source limits remain; no claim of flawless historical coverage'),file.path(revision_dir,'audit/execution/final_verification.json'),pretty=TRUE,auto_unbox=TRUE)
cat(length(checks),'final verification checks passed\n')

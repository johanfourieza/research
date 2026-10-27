# Signed local audit checkpoint, preservation checks and machine-readable status.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
setDTthreads(1)
required<-c("registry_collision_queries","registry_split_queries","registry_identity_coverage","implementation_pilot_results","additive_identity_checks","correction_ledger_v3","rebuilt_component_checks")
stopifnot(all(file.exists(file.path(revision_dir,"audit/execution",paste0(required,".csv")))))
read_audit<-function(n)read_csv(file.path(revision_dir,"audit/execution",paste0(n,".csv")))
baseline<-read_csv(file.path(revision_dir,"audit/baseline_manifest.csv"))
baseline[,current_sha256:=vapply(path,function(p)toupper(digest::digest(file=file.path(project_dir,p),algo="sha256")),character(1))]
baseline[,unchanged:=toupper(sha256)==current_sha256]
write_audit(baseline,"original_preservation_check");stopifnot(all(baseline$unchanged))
issues<-list()
add<-function(id,kind,scope,status,action,evidence)issues[[length(issues)+1L]]<<-data.table(issue_id=id,kind,scope,status,action,evidence,reviewer="Codex model-performed audit",date=as.character(Sys.Date()))
corrections<-read_audit("correction_ledger_v3")
for(id in unique(corrections$correction_id)) {
 q<-corrections[correction_id==id]
 add(id,"verified source correction",paste(unique(q$comp),collapse=";"),"applied; aggregate conservation checked",paste(unique(q$reason),collapse=" | "),paste(unique(q$source),collapse=" | "))
}
collisions<-read_audit("registry_collision_queries");splits<-read_audit("registry_split_queries")
for(i in seq_len(nrow(collisions)))add(sprintf("ID%03d",i),"source key linked to multiple registry identities",collisions$player[i],"OPEN; quarantine affected careers","Review match/team identity assignments and biographical metadata before scoped split","registry_identity_edges.csv; registry_appearances_*.csv")
for(i in seq_len(nrow(splits)))add(sprintf("AL%03d",i),"registry identity linked to multiple source keys",splits$registry_id[i],"OPEN; quarantine affected careers","Review possible aliases, wrong person assignments and registry errors before merging","registry_identity_edges.csv; registry_appearances_*.csv")
add("SRC001","unresolved four-run accounting difference","county 20080530_01110_RoseBwl; Hampshire innings 2","OPEN; quarantine entire match and affected career inputs","Resolve 33 total against 29 batter runs and zero recorded extras; do not impute four extras","corrected_innings_accounting.csv")
add("META001","biographical/style validation incomplete","all formats; age/style regressions and first-Test transition","OPEN; analyses blocked","Complete stratified independent historical metadata checks; resolve identity-specific birth, death, hand and repertoire","metadata_pilot_sample.csv is a sample frame, not completed validation; identity_adjudication_notes.md")
add("META002","missing nominated wicketkeeper identity","three source match-team nominations literally recorded as unknown; two T20I and one county","OPEN; affected keeper indicators flagged","Preserve unknown nomination; resolve from a scorecard before using an actual-keeper indicator. A missing nomination is not evidence that no player kept wicket.","expanded_identity_queries.csv; rebuilt_component_checks.csv")
add("COV001","historical and boundary competition coverage","county vintage boundaries; 2020/2021 stages; historical Shield","OPEN; affected coverage claims blocked","Complete boundary and no-play/forfeiture validation; 1994-99 inclusion is already reconciled separately","county_membership_reconciliation.csv; empty_innings_slot_checks.csv")
add("LINK001","domestic-to-Test identity and risk timing","promotion analysis","OPEN; event-history estimation blocked","Adjudicate four unlinked early Australian debut controls; review all registry collision/split keys and overlap timing","test_debut_linkage_controls.csv; expanded_identity_queries.csv; same_day_national_identity_queries.csv")
add("ROLE001","human role labels declined","external role-classifier accuracy","CLOSED by explicit user instruction","Use independent implementation pilot only; make no human-label accuracy claim","planning/execution_decisions.md; implementation_pilot_results.csv")
add("BALL001","scorecard overs versus actual legal deliveries","exposure definition in every format","RESOLVED definition; document in manuscript","Use scorecard-derived balls consistently; retain actual delivery counts separately in modern extensions","cricsheet_miscounted_over_adjudications.csv; planning/execution_decisions.md")
add("PILOT001","modern independent-source count pilot","63 matches; all five competitions","RECONCILED within sampled scope","Retain adjudicated source differences; no inference of universal accuracy or independent human roles","cricsheet_pilot_adjudications.csv; corrected_cricsheet_*_comparison.csv")
register<-rbindlist(issues);write_audit(register,"signed_issue_register")
gate<-list(status="CLOSED",updated=as.character(Sys.Date()),scope="all substantive analyses",
 reason="Identity collision/alias adjudication, Hampshire 2008 accounting, historical metadata and competition coverage checks remain open.",
 estimation_authorised=FALSE,paid_relabelling_authorised=FALSE,human_role_labels="declined by user; implementation validation only",
 signed_issue_register="audit/execution/signed_issue_register.csv",reviewer="Codex model-performed audit")
jsonlite::write_json(gate,file.path(revision_dir,"config/data_gate.json"),pretty=TRUE,auto_unbox=TRUE)
results<-read_audit("implementation_pilot_results");stopifnot(all(results$agree))
checks<-read_audit("rebuilt_component_checks")
coverage<-read_audit("registry_identity_coverage")
summary<-list(updated=as.character(Sys.time()),status="Stage A execution checkpoint; data gate CLOSED",
 original_files_preserved=nrow(baseline),correction_cases=uniqueN(corrections$correction_id),
 source_pilot_matches=nrow(read_audit("corrected_cricsheet_pilot_frame")),source_pilot_team_innings=nrow(read_audit("corrected_cricsheet_team_comparison")),
 role_implementation_careers=uniqueN(results,by=c("comp","player")),implementation_field_checks=nrow(results),
 registry_linked_matches=sum(coverage$linked_matches),registry_appearances=sum(coverage$registry_appearances),registry_name_links=sum(coverage$name_linked),
 registry_collision_source_keys=nrow(collisions),registry_split_identities=nrow(splits),
 run_accounting_queries=sum(checks[check=="corrected_run_accounting_queries",n]),
 bowling_credit_queries=sum(checks[check=="corrected_bowler_credit_queries",n]),allout_completeness_queries=sum(checks[check=="corrected_allout_completeness_queries",n]),
 human_role_labels=0L,paid_relabelling_calls=0L,substantive_models_estimated=0L)
jsonlite::write_json(summary,file.path(revision_dir,"audit/execution/checkpoint_summary.json"),pretty=TRUE,auto_unbox=TRUE)
capture.output(sessionInfo(),file=file.path(revision_dir,"audit/execution/session_info.txt"))
# Confirm that the guard refuses estimation with the current gate.
blocked<-tryCatch({require_data_gate();FALSE},error=function(e)grepl("Data gate CLOSED",conditionMessage(e)))
stopifnot(blocked)
# Check the final artifacts, including safety flags and the identity-specific metadata refinement.
for(co in names(configs)) {
 pm<-readRDS(file.path(revision_dir,"data/corrected",paste0("player_match_",co,".rds")))
 stopifnot(all(!pm$data_gate_passed),!anyNA(pm$identity_registry_query),all(pm$scorecard_balls==pm$legal_balls))
 if(co=="odis") {
  a<-pm[player=="Abbas, MA {b2003-11-29}"]
  stopifnot(nrow(a)==3,all(a$fast_medium==1L),all(a$medium==0L),all(a$batting_hand=="right"),all(a$bowling_style=="pace"))
 }
}
code_files<-list.files(file.path(revision_dir,"scripts"),full.names=TRUE,pattern="\\.R$")
invisible(lapply(code_files,function(p)parse(file=p)))
cfg_files<-list.files(file.path(revision_dir,"config"),full.names=TRUE)
manifest<-data.table(path=c(code_files,cfg_files))
manifest[,sha256:=vapply(path,function(p)digest::digest(file=p,algo="sha256"),character(1))]
manifest[,path:=substring(path,nchar(revision_dir)+2L)];write_audit(manifest,"execution_code_config_manifest")
external_inputs<-data.table(path=c(file.path(book_data,"raw/cricsheet/all_json.zip"),file.path(book_data,"raw/cricsheet/MANIFEST_matches.csv")))
external_inputs[,sha256:=vapply(path,function(p)digest::digest(file=p,algo="sha256"),character(1))]
write_audit(external_inputs,"independent_archive_manifest")
cached<-list.files(file.path(revision_dir,"sources"),recursive=TRUE,full.names=TRUE)
cache_manifest<-data.table(path=cached[!file.info(cached)$isdir])
cache_manifest[,sha256:=vapply(path,function(p)digest::digest(file=p,algo="sha256"),character(1))]
cache_manifest[,path:=substring(path,nchar(revision_dir)+2L)];write_audit(cache_manifest,"source_cache_manifest")
cat(jsonlite::toJSON(summary,pretty=TRUE,auto_unbox=TRUE),"\n")

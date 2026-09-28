# =============================================================================
#  05_identity_scenarios.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Mutually exclusive assignments of the surname-only 1714 widow entry
#  and the unresolved Lombart widow (Table S2). Changes no confirmed link.
#
#  INPUTS   cohort from 04; widow candidate register
#  OUTPUTS  data/identity_scenarios.csv
#
#  This is the script that produced the released aggregates. It needs the
#  restricted individual-level sources and decision registers, which are not
#  redistributed, so it cannot run from this package. File paths refer to the
#  author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

# Mutually exclusive assignments of one surname-only widow, added in the final source review.
# This supplements the legacy personwise ambiguity output; it changes no confirmed link.
source('R/helpers/current_reporting.R')
A<-readRDS(file.path(CURRENT_OUT,'current_cohort.rds'))
C<-fread('R/decisions/additional_widow_candidates.csv')
stopifnot(all(C$baseline_hhobs%in%A$hhobs),uniqueN(C$widow_hhobs)==1L,
          !any(A[hhobs%in%C$baseline_hhobs,any_record]))
rows<-rbindlist(lapply(c(0L,C$baseline_hhobs),function(romond){
 rbindlist(lapply(c(FALSE,TRUE),function(lombart){
  ids<-c(romond,if(lombart)5862L else 0L)
  d<-A[,.(n=.N,events=sum(any_record|hhobs%in%ids)),by=group]
  ratio<-d[group=='low',events/n]/d[group=='high',events/n]
  data.table(romond_assignment=romond,lombart_added=lombart,total=sum(d$events),
    low_events=d[group=='low',events],middle_events=d[group=='middle',events],
    high_events=d[group=='high',events],ratio=ratio,equality_multiple=1/ratio)
 }))
}))
current_write(rows,'identity_scenarios')
writeLines(c(current_macro('IdentityRatioLo',current_fmt(min(rows$ratio),3)),
 current_macro('IdentityRatioHi',current_fmt(max(rows$ratio),3)),
 current_macro('IdentityThresholdLo',current_fmt(min(rows$equality_multiple),2)),
 current_macro('IdentityThresholdHi',current_fmt(max(rows$equality_multiple),2))),
 file.path(CURRENT_GEN,'identity_macros.tex'))
writeLines(vapply(seq_len(nrow(rows)),function(i){d<-rows[i]
 label<-if(d$romond_assignment==0)'Unassigned' else if(d$romond_assignment==5688)'Older Michiel' else 'Younger Michiel'
 paste0(label,if(d$lombart_added)' + Lombart' else '', ' & ',d$low_events,'/243 & ',
 d$middle_events,'/60 & ',d$high_events,'/45 & ',current_fmt(d$ratio,3),' & ',
 current_fmt(d$equality_multiple,2),' \\\\')},character(1)),
 file.path(CURRENT_GEN,'identity_rows.tex'))
cat('Mutually exclusive identity scenarios generated.\n');print(rows)

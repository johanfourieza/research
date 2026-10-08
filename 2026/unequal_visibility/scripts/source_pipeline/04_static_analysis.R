# =============================================================================
#  04_static_analysis.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  The core analysis. Applies the reviewed baseline and death-link
#  decisions (inputs, never inferred from fuzzy matches), forms the cohort and
#  resource groups, and computes Tables 1-2, the wife-named comparison, the
#  recording model, the resource index and the source-rule checks.
#
#  INPUTS   linked panel; decision registers for baseline rows, MOOC8 and
#  Stellenbosch-compilation death links, document and widow screening; the
#  MOOC8 document inventory and the Stellenbosch compilation schedule register
#  OUTPUTS  data/cohort_by_resource_group.csv; data/death_records_by_group.csv;
#  data/resource_index_death_records.csv; data/source_rule_death_records.csv;
#  data/denominator_sensitivity.csv; the cohort file exported as
#  data/microdata/cohort_1712.csv
#
#  This is the script that produced the released files. It needs restricted
#  inputs that are not redistributed (the full linked tax-roll panel, the SAF
#  genealogy and the source transcriptions), so it cannot run from this
#  package. The decision registers it reads are released in data/microdata/.
#  File paths refer to the author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

# Source-adjudicated static analysis. Decisions are inputs, never inferred from
# fuzzy matches or the ordering of names in a probate header.
source('R/helpers/current_reporting.R')
source('R/helpers/current_names.R');check_current_names()
B<-fread('R/decisions/baseline_rows.csv',encoding='UTF-8')
L<-fread('R/decisions/death_links.csv',encoding='UTF-8',
         colClasses=c(source_file='character',div_id='character',date_value='character'))
L<-rbindlist(list(L,fread('R/decisions/stellenbosch_death_links.csv',encoding='UTF-8',
         colClasses=c(source_file='character',div_id='character',date_value='character'))),use.names=TRUE)
P<-fread(PANEL_GZ,select=c('year','hhobs','names_men','names_women','widow',
  'individual_id','slave_men','slave_women','cattle_cows','cattle_work','horses','sheep'),
  encoding='Latin-1',showProgress=FALSE)
stopifnot(!anyDuplicated(B$hhobs),!anyDuplicated(P$hhobs),
  all(B$hhobs%in%P[year==1712,hhobs]),all(L$baseline_hhobs%in%B$hhobs),
  all(L$decision%in%c('accept','ambiguous','reject')),all(nzchar(L$evidence)))
ar<-merge(P[year==1712],B[,.(hhobs,decision,duplicate_set,raw_excel_row)],by='hhobs')
stopifnot(nrow(ar)==nrow(B))
# Source rule: a blank is zero only if the variable is recorded in the census.
# Coverage is checked by district against the raw workbook in the cohort audit.
assets<-c('slave_men','slave_women','cattle_cows','cattle_work','horses','sheep')
stopifnot(all(vapply(assets,function(v)any(!is.na(ar[[v]])),logical(1))))
ar[,slaves:=adult_slaves(.SD)];ar[,wealth:=wealth_index(.SD)]
ar[,group:=factor(fifelse(slaves==0,'low',fifelse(slaves<=4,'middle','high')),
                  levels=c('low','middle','high'))]
ar[,wife_recorded:=!is.na(names_women)&nzchar(trimws(names_women))]
ar[,saf:=!is.na(individual_id)&nzchar(as.character(individual_id))]
nm<-current_names(ar$names_men);ar[,name_key:=nm$key]
# Check source existence, year, named baseline and decision completeness.
inv<-readRDS(PROBATE_RDS)
inv<-rbindlist(list(inv,fread(STELLENBOSCH_REGISTER,colClasses=c(date_value='character'))),fill=TRUE)
inv[,document_key:=paste(source_file,div_id,date_value,sep='|')]
L[,source_year:=as.integer(substr(date_value,1,4))]
L[,document_key:=fifelse(channel=='widow',paste0('roll|',widow_hhobs),
                        paste(source_file,div_id,date_value,sep='|'))]
stopifnot(all(L$source_year%in%c(1713L,1714L)),
 all(L[channel!='widow',document_key]%in%inv$document_key),
 all(L[channel=='widow',widow_hhobs]%in%P[year%in%1713:1714&widow==1,hhobs]),
 all(L[decision=='accept',event_interval]=='after1712_by1714'))
accepted<-L[decision=='accept']
# A widow entry cannot be assigned to more than one man. Probate documents can
# explicitly name several deceased people; each person is separately adjudicated.
stopifnot(!any(accepted[channel=='widow',.(n=uniqueN(baseline_hhobs)),by=widow_hhobs]$n>1))
make_flags<-function(a,links){
 a<-copy(a)
 a[,probate:=hhobs%in%links[channel%in%c('probate','probate_incidental'),baseline_hhobs]]
 a[,widow_record:=hhobs%in%links[channel=='widow',baseline_hhobs]]
 a[,any_record:=probate|widow_record]
 a[]
}
ar<-make_flags(ar,accepted)
A<-ar[decision=='include']
stopifnot(!anyNA(A$name_key),all(A$any_record==(A$probate|A$widow_record)))
# Same full name can belong to distinct men: the two Ary van Wyk entries have
# different named wives. Unresolved same-name rows were excluded by the register.
same_name<-A[,.(n=.N,wives=uniqueN(names_women),all_wives=all(wife_recorded)),by=name_key][n>1]
stopifnot(all(same_name$wives==same_name$n),all(same_name$all_wives))
rates<-function(a,bycol='group')rbindlist(lapply(c('probate','widow_record','any_record','saf'),function(ch){
 r<-a[,.(n=.N,events=sum(get(ch))),by=bycol]
 r[,channel:=ch];r[,rate:=events/n];ci<-current_wilson(r$events,r$n)
 r[,`:=`(lo=ci$lo,hi=ci$hi)];r
}))
cap<-rates(A);setorder(cap,channel,group)
ratio<-function(c,channel_name){
 x<-c[channel==channel_name];l<-x[as.character(group)=='low'];h<-x[as.character(group)=='high']
 rr<-l$rate/h$rate
 positive<-l$events>0&&h$events>0
 se<-if(positive)sqrt(1/l$events-1/l$n+1/h$events-1/h$n) else NA_real_
 data.table(channel=channel_name,low_rate=l$rate,high_rate=h$rate,ratio=rr,
  log_se=se,ratio_lo=if(positive)exp(log(rr)-qnorm(.975)*se) else NA_real_,
  ratio_hi=if(positive)exp(log(rr)+qnorm(.975)*se) else NA_real_,
  interval_method=if(positive)'log-binomial delta method' else 'not computed: zero event cell',
  unrestricted_lo=l$rate,signed_lo=rr,upper=1/h$rate,
  rich_capture_multiple=if(rr>0)1/rr else NA_real_,at_half_capture=rr/.5,at_quarter_capture=rr/.25)
}
rr<-rbindlist(lapply(c('probate','widow_record','any_record'),function(ch)ratio(cap,ch)))
desc<-A[,.(n=.N,share=.N/nrow(A),slaves=mean(slaves),wealth=mean(wealth),
  cattle=mean(z(cattle_cows)+z(cattle_work)),sheep=mean(z(sheep)),
  wife_recorded=sum(wife_recorded)),by=group]
setorder(desc,group)
stopifnot(sum(desc$n)==nrow(A),all(cap$events<=cap$n))
# Results must not depend on source-row order.
revA<-make_flags(A[.N:1],accepted[.N:1])
stopifnot(identical(A[order(hhobs),any_record],revA[order(hhobs),any_record]))
current_write(A,'cohort');saveRDS(A,file.path(CURRENT_OUT,'current_cohort.rds'))
current_write(ar,'all_baseline_entries');current_write(L,'death_decisions')
current_write(cap,'capture');current_write(rr,'ratios');current_write(desc,'descriptives')
current_write(L[,.(links=.N,people=uniqueN(baseline_hhobs)),by=.(decision,channel)],'link_accounting')
# Ambiguous links are potential extra observations, not confirmed deaths.
possible<-make_flags(A,L[decision%in%c('accept','ambiguous')])
amb<-A[,.(n=.N,confirmed=sum(any_record)),by=group]
amb[possible[,.(possible=sum(any_record)),by=group],on='group',possible:=i.possible]
amb[,`:=`(recorded_rate_lo=confirmed/n,recorded_rate_hi=possible/n)]
amb_rr<-data.table(lower=amb[group=='low',recorded_rate_lo]/amb[group=='high',recorded_rate_hi],
 upper=amb[group=='low',recorded_rate_hi]/amb[group=='high',recorded_rate_lo])
current_write(amb,'ambiguity');current_write(amb_rr,'ambiguity_ratio')
# Re-admit the two unresolved repeated-name sets with explicit denominator weights.
# No member has an accepted or ambiguous death, so coarsening affects exposure only.
dup<-ar[decision=='exclude_identity_unresolved']
stopifnot(!any(dup$any_record),!any(dup$hhobs%in%L[decision=='ambiguous',baseline_hhobs]))
denom<-rbindlist(lapply(c('primary','one_person_per_unresolved_pair','two_people_per_unresolved_pair'),function(s){
 d<-A[,.(n=.N,events=sum(any_record)),by=group]
 if(s!='primary'){
   extra<-if(s=='one_person_per_unresolved_pair')uniqueN(dup$duplicate_set) else nrow(dup)
   stopifnot(all(dup$group=='low'));d[group=='low',n:=n+extra]
 }
 data.table(spec=s,n=sum(d$n),low_n=d[group=='low',n],
 ratio=(d[group=='low',events/n])/(d[group=='high',events/n]))
}))
current_write(denom,'denominator_sensitivity')
timing<-rbindlist(lapply(c('through1713','through1714','estate_only'),function(s){
 links<-if(s=='through1713')accepted[source_year==1713] else if(s=='estate_only')accepted[channel!='probate_incidental'] else accepted
 r<-rates(make_flags(A,links));x<-r[channel=='any_record']
 x[,spec:=s];x
}))
current_write(timing,'ascertainment')
wife<-rates(A[wife_recorded==TRUE]);current_write(wife,'wife_recorded_capture')
# Ties remain together: empirical tertile cut points define resource groups.
cuts<-quantile(A$wealth,c(1/3,2/3),names=FALSE,type=7)
wealthA<-copy(A);wealthA[,group:=factor(fifelse(wealth<=cuts[1],'low',
  fifelse(wealth<=cuts[2],'middle','high')),levels=c('low','middle','high'))]
wealthcap<-rates(wealthA);current_write(wealthcap,'wealth_capture')
current_write(data.table(first_cut=cuts[1],second_cut=cuts[2]),'wealth_cutpoints')
current_write(rbindlist(lapply(c('probate','widow_record','any_record'),function(ch)ratio(wealthcap,ch))),'wealth_ratios')
# Figures show confirmed record rates. The capture restriction is assumed, not estimated.
cl<-cap[channel%in%c('probate','widow_record','saf')]
cl[,channel:=factor(channel,levels=c('probate','widow_record','saf'),
  labels=c('MOOC8 death confirmation','Widow entry','Genealogical presence'))]
cl[,group:=factor(group,levels=c('low','middle','high'),labels=c('0 recorded','1-4','5+'))]
p<-ggplot(cl,aes(group,rate,colour=channel,group=channel))+
 geom_point(position=position_dodge(width=.4),size=2.3)+
 geom_errorbar(aes(ymin=lo,ymax=hi),position=position_dodge(width=.4),width=.12)+
 scale_colour_manual(values=current_palette[c(1,2,4)])+
 scale_y_continuous(labels=scales::label_percent(accuracy=1))+
 labs(x='Adult slaves recorded in 1712',y='Share with a linked record',colour=NULL)+current_theme()
current_save_plot(p,'current_capture')
curve<-rbindlist(lapply(rr$channel,function(ch){
 ratio_value<-rr[channel==ch,ratio]
 data.table(channel=ch,kappa=seq(.15,1,length.out=300),theta=ratio_value/seq(.15,1,length.out=300))
}))
curve[,channel:=factor(channel,levels=c('probate','widow_record','any_record'),
 labels=c('MOOC8 death confirmation','Widow entry','Either death record'))]
p<-ggplot(curve,aes(kappa,theta,colour=channel))+
 geom_hline(yintercept=1,linetype=2,colour='#888888')+
 geom_line(data=curve[channel!='Either death record'],linewidth=.6,alpha=.65)+
 geom_line(data=curve[channel=='Either death record'],linewidth=1.2)+
 geom_point(data=data.table(kappa=rr[channel=='any_record',ratio],theta=1,
                           channel='Either death record'),size=2.7)+
 annotate('label',x=.58,y=1.32,
          label=paste0('Either record: equal mortality at ',current_fmt(rr[channel=='any_record',ratio],3)),
          size=3,linewidth=0,fill='white',colour=current_palette[3])+
 scale_colour_manual(values=current_palette[1:3])+
 scale_x_reverse(breaks=c(1,.75,.5,.25))+
 labs(x=expression(paste('Assumed capture ratio ',C[L]/C[H])),
      y=expression(paste('Implied mortality ratio ',M[L]/M[H])),colour=NULL)+current_theme()
current_save_plot(p,'current_source_sensitivity')
# Main-text figure isolates the combined record and its equality threshold.
# All coordinates use the same audited ratios as the source-specific curves.
union_ratio<-rr[channel=='any_record',ratio]
union_curve<-curve[channel=='Either death record']
p<-ggplot(union_curve,aes(kappa,theta))+
 geom_hline(yintercept=1,linetype=2,colour='#888888')+
 geom_vline(xintercept=union_ratio,linetype=3,colour='#888888')+
 geom_line(linewidth=1.2,colour=current_palette[1])+
 geom_point(data=data.table(kappa=c(1,union_ratio),theta=c(union_ratio,1)),
            size=2.8,colour=current_palette[1])+
 annotate('text',x=.98,y=.51,hjust=0,
          label=paste0('Equal recovery\nMortality ratio = ',current_fmt(union_ratio,3)),
          size=3.5,colour=current_palette[1])+
 annotate('label',x=.57,y=1.35,
          label=paste0('Equal mortality\nRecovery ratio = ',current_fmt(union_ratio,3),
                       '\nHigher-resource advantage = ',
                       current_fmt(1/union_ratio,1),' times'),
          size=3.5,linewidth=0,fill='white',colour=current_palette[1])+
 scale_x_reverse(breaks=c(1,.75,.5,union_ratio,.15),
                 labels=c('1.00','0.75','0.50',current_fmt(union_ratio,3),'0.15'))+
 scale_y_continuous(limits=c(0,2.05),breaks=c(0,.5,1,1.5,2))+
 labs(x='Assumed relative probability of recovering a death (low / high)',
      y='Implied mortality ratio (low / high)')+current_theme()
current_save_plot(p,'current_capture_sensitivity')
current_write(curve,'capture_sensitivity')
# Tables and every repeated empirical figure in the manuscript are generated.
labels<-c(low='0 recorded',middle='1--4',high='5+')
writeLines(vapply(seq_len(nrow(desc)),function(i){d<-desc[i]
 paste0(labels[as.character(d$group)],' & ',d$n,' & ',current_fmt(100*d$share),' & ',
 current_fmt(d$slaves),' & ',current_fmt(d$cattle),' & ',current_fmt(d$sheep,0),' & ',d$wife_recorded,' \\\\')
},character(1)),file.path(CURRENT_GEN,'descriptive_rows.tex'))
writeLines(vapply(c('low','middle','high'),function(g){
 vals<-vapply(c('probate','widow_record','any_record','saf'),function(ch){
 d<-cap[group==g&channel==ch]
 paste0('\\shortstack{',d$events,'/',d$n,'\\\\',current_fmt(100*d$rate),' [',current_fmt(100*d$lo),', ',current_fmt(100*d$hi),']}')
 },character(1));paste0(labels[g],' & ',paste(vals,collapse=' & '),' \\\\ \\addlinespace[0.3em]')
},character(1)),file.path(CURRENT_GEN,'capture_rows.tex'))
m<-c(current_macro('CohortN',nrow(A)),current_macro('ConfirmedN',sum(A$any_record)),
 current_macro('RawEntries',nrow(ar)),current_macro('ExcludedEntries',nrow(ar)-nrow(A)),
 current_macro('AmbiguousPeople',sum(possible$any_record)-sum(A$any_record)),
 current_macro('UnionRatio',current_fmt(rr[channel=='any_record',ratio],2)),
 current_macro('UnionRatioThree',current_fmt(rr[channel=='any_record',ratio],3)),
 current_macro('UnionRatioLo',current_fmt(rr[channel=='any_record',ratio_lo],2)),
 current_macro('UnionRatioHi',current_fmt(rr[channel=='any_record',ratio_hi],2)),
 current_macro('UnionUpper',current_fmt(rr[channel=='any_record',upper],2)),
 current_macro('CaptureEqualityMultiple',current_fmt(rr[channel=='any_record',rich_capture_multiple],1)),
 current_macro('HalfCaptureRatio',current_fmt(rr[channel=='any_record',at_half_capture],2)),
 current_macro('QuarterCaptureRatio',current_fmt(rr[channel=='any_record',at_quarter_capture],2)),
 current_macro('AmbiguityRatioLo',current_fmt(amb_rr$lower,2)),
 current_macro('AmbiguityRatioHi',current_fmt(amb_rr$upper,2)))
for(g in c('low','middle','high')){
 title<-paste0(toupper(substr(g,1,1)),substring(g,2));d<-desc[group==g]
 m<-c(m,current_macro(paste0(title,'N'),d$n),current_macro(paste0(title,'Share'),current_fmt(100*d$share)))
 for(ch in c('probate','widow_record','any_record','saf')){
  ct<-c(probate='Probate',widow_record='Widow',any_record='Union',saf='SAF')[[ch]]
  d<-cap[group==g&channel==ch]
  m<-c(m,current_macro(paste0(title,ct,'Events'),d$events),
       current_macro(paste0(title,ct,'Rate'),current_fmt(100*d$rate)))
 }
}
writeLines(m,file.path(CURRENT_GEN,'static_macros.tex'))
checks<-c('All baseline rows unique and linked to source IDs.',
 'Accepted death links cite existing documents or widow rows and an adjudicated interval.',
 'No widow is accepted for more than one baseline man.',
 'No name-key deduplication; retained same-name entries have distinct named wives.',
 'Counts, union, denominators and reversed-row-order invariance verified.',
 'Same-source ambiguity is kept separate from accepted events.',
 'Wilson bands and ratio intervals are model-based conditional on the register.')
writeLines(checks,file.path(CURRENT_OUT,'current_static_checks.txt'))
cat('Current static analysis complete.\n');print(cap);print(rr)

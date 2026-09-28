# =============================================================================
#  03_event_series.R  (source pipeline; reference only)
#  Paper: The Unequal Visibility of Epidemic Death: Smallpox at the Cape, 1713
#  Author: Johan Fourie
#
#  WHAT THIS SCRIPT DOES
#  Builds the two unlinked administrative series (Figure 1).
#
#  INPUTS   mooc8_inventories.rds; linked opgaaf panel
#  OUTPUTS  data/probate_documents_by_year.csv; data/widow_entries_by_year.csv
#
#  This is the script that produced the released aggregates. It needs the
#  restricted individual-level sources and decision registers, which are not
#  redistributed, so it cannot run from this package. File paths refer to the
#  author's working layout. See scripts/source_pipeline/README.md.
# =============================================================================

# Rebuild the two unlinked descriptive series from source-derived inputs.
source('R/helpers/current_reporting.R')
inv<-readRDS(PROBATE_RDS)
inv[,year:=suppressWarnings(as.integer(substr(date_value,1,4)))]
docs<-inv[!is.na(year),.(documents=.N),by=year][order(year)]
P<-fread(PANEL_GZ,select=c('year','hhobs','names_men','widow'),
         encoding='Latin-1',showProgress=FALSE)
P[,widow_entry:=!is.na(widow)&widow==1]
P[,named_entry:=!is.na(names_men)&nzchar(trimws(names_men))]
series<-P[year>=1705&year<=1725,.(entries=sum(named_entry|widow_entry),
  widows=sum(widow_entry)),by=year][order(year)]
series[,share:=widows/entries]
base<-series[year%in%1708:1712,sum(widows)/sum(entries)]
neighbours<-docs[year%in%c(1710:1712,1714:1716)]
stopifnot(nrow(neighbours)==6L)
stats<-data.table(documents_1713=docs[year==1713,documents],
  neighbouring_mean=mean(neighbours$documents),
  document_multiple=docs[year==1713,documents]/mean(neighbours$documents),
  widows_1713=series[year==1713,widows],entries_1713=series[year==1713,entries],
  widow_share_1713=series[year==1713,share],widow_baseline_share=base,
  baseline_widows=series[year%in%1708:1712,sum(widows)],
  baseline_entries=series[year%in%1708:1712,sum(entries)])
current_write(docs,'probate_documents');current_write(series,'widow_series')
current_write(stats,'event_summary')
plotdata<-rbindlist(list(docs[year>=1705&year<=1722,.(year,y=documents,
  series='(a) Cape MOOC8 probate documents (count)')],
  series[year>=1705&year<=1722,.(year,y=100*share,
  series='(b) District widow entries (% of enumerated entries)')]))
p<-ggplot(plotdata,aes(year,y))+geom_vline(xintercept=1713,linetype=2,colour='#888888')+
  geom_line(colour=current_palette[1],linewidth=.55)+
  geom_point(colour=current_palette[1],size=1.8)+facet_wrap(~series,ncol=1,scales='free_y')+
  scale_x_continuous(breaks=c(1705,1708,1711,1713,1716,1719,1722))+
  labs(x='Year',y=NULL)+current_theme()
current_save_plot(p,'current_event',8,7)
writeLines(c(current_macro('EventDocuments',stats$documents_1713),
 current_macro('EventMultiple',current_fmt(stats$document_multiple)),
 current_macro('EventNeighbourMean',current_fmt(stats$neighbouring_mean)),
 current_macro('EventWidows',stats$widows_1713),
 current_macro('EventEntries',stats$entries_1713),
 current_macro('EventWidowShare',current_fmt(100*stats$widow_share_1713)),
 current_macro('BaselineWidowShare',current_fmt(100*stats$widow_baseline_share))),
 file.path(CURRENT_GEN,'event_macros.tex'))
cat('Current source event series complete.\n');print(stats)

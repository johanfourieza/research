# Document checks only; this script does not change data or analyses.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'revision_common.R'))
dir.create(file.path(revision_dir,'review/pdf_pages'),showWarnings=FALSE,recursive=TRUE)
out<-list()
for(doc in c('manuscript','appendix','response_to_referees')) {
 path<-file.path(revision_dir,'paper',paste0(doc,'.pdf'))
 tx<-pdftools::pdf_text(path)
 writeLines(tx,file.path(revision_dir,'review',paste0(doc,'_final_text.txt')))
 words<-sum(lengths(strsplit(trimws(tx),'[[:space:]]+')))
 pages<-unique(c(1,ceiling(length(tx)/2),length(tx)))
 out[[doc]]<-data.table(document=doc,pages=length(tx),words_including_exhibits=words,unresolved_double_question=any(grepl('??',tx,fixed=TRUE)))
 for(pg in pages)pdftools::pdf_convert(path,format='png',pages=pg,dpi=100,filenames=file.path(revision_dir,'review/pdf_pages',paste0(doc,'_',pg,'.png')),verbose=FALSE)
}
write_audit(rbindlist(out),'document_checks')
print(rbindlist(out))

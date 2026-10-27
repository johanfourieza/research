# Independently reconcile the disputed 1994-99 county membership using public
# Cricinfo championship scorecard indices. Cache source pages for reproducibility.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
suppressPackageStartupMessages(library(xml2))
cache <- file.path(revision_dir,"sources/county_indices")
dir.create(cache,recursive=TRUE,showWarnings=FALSE)
requests<-list();links<-list()
read_index <- function(url) {
  filename <- paste0(gsub("[^a-zA-Z0-9]","_",sub("https://i.imgci.com/","",url)),".html")
  dest<-file.path(cache,filename)
  ok<-file.exists(dest)
  if(!ok) ok<-tryCatch({download.file(url,dest,quiet=TRUE,mode="wb",method="libcurl");TRUE},error=function(e)FALSE)
  requests[[length(requests)+1L]]<<-data.table(url,local_file=filename,downloaded=ok)
  if(!ok)return(data.table(url=character(),text=character(),index_url=character()))
  doc<-read_html(dest)
  nodes<-xml_find_all(doc,"//a[@href]")
  data.table(url=url_absolute(xml_attr(nodes,"href"),url),text=trimws(xml_text(nodes)),index_url=url)
}
for(yr in 1994:1999) {
  base<-paste0("https://i.imgci.com/db/ARCHIVE/",yr,"/ENG_LOCAL/CC/")
  first<-read_index(base)
  all<-first
  rounds<-unique(first[grepl(paste0("/",yr,"/ENG_LOCAL/CC/"),url) & grepl("/R[0-9]+/?$",url),url])
  for(u in rounds)all<-rbind(all,read_index(u),fill=TRUE)
  all[,year:=yr];links[[as.character(yr)]]<-all
  cat(yr,"index inspected; round pages",length(rounds),"\n")
}
all<-unique(rbindlist(links))
score<-all[grepl("_CC_",url,ignore.case=TRUE) & grepl("[0-9]{4}\\.html$",url,ignore.case=TRUE)]
score[,filename:=basename(url)]
score<-unique(score,by="filename")
score[,pair_string:=sub("_CC_.*","",filename)]
aliases<-c(DERBY="Derbyshire",DERBYS="Derbyshire",DERBYSHIRE="Derbyshire",DURHAM="Durham",ESSEX="Essex",
  GLAM="Glamorgan",GLAMORGAN="Glamorgan",GLOUCS="Gloucestershire",GLOUCESTERSHIRE="Gloucestershire",HANTS="Hampshire",HAMPSHIRE="Hampshire",
  KENT="Kent",LANCS="Lancashire",LANCASHIRE="Lancashire",LEICS="Leicestershire",LEICESTERSHIRE="Leicestershire",MIDDX="Middlesex",MIDDLESEX="Middlesex",
  NORTHANTS="Northamptonshire",NORTHAMPTONSHIRE="Northamptonshire",NOTTS="Nottinghamshire",NOTTINGHAMSHIRE="Nottinghamshire",
  SOMERSET="Somerset",SURREY="Surrey",SUSSEX="Sussex",WARWICKS="Warwickshire",WARWICKSHIRE="Warwickshire",WORCS="Worcestershire",WORCESTERSHIRE="Worcestershire",YORKS="Yorkshire",YORKSHIRE="Yorkshire")
score[,team1:=unname(aliases[sub("_.*","",pair_string)])]
score[,team2:=unname(aliases[sub("^[^_]+_","",pair_string)])]
date_part<-sub(".*_CC_","",score$filename)
date_parts<-regmatches(date_part,regexec("^([0-9]{1,2})(?:-[0-9]{1,2})?([A-Z]{3})(?:-[0-9]{1,2}[A-Z]{3})?([0-9]{4})",date_part,perl=TRUE))
score[,start:=as.IDate(vapply(date_parts,function(z){
  if(length(z)!=4)return(NA_character_)
  month<-match(z[3],toupper(month.abb))
  if(is.na(month))return(NA_character_)
  sprintf("%s-%02d-%02d",z[4],month,as.integer(z[2]))
},character(1)))]
score[,pair:=mapply(function(a,b)paste(sort(c(a,b)),collapse="|"),team1,team2)]
write_audit(all,"county_index_links")
write_audit(rbindlist(requests),"county_index_downloads")
write_audit(score,"county_independent_scorecard_register")
x<-readRDS(file.path(revision_dir,"data/staging/source_county.rds"))
tm<-unique(x$teams[,.(match_id,start=as.IDate(start_date),team,competition)])
tm<-tm[start>=as.IDate("1994-01-01") & start<=as.IDate("1999-12-31")]
dm<-tm[,.(pair=paste(sort(team),collapse="|"),competition=competition[1]),by=.(match_id,start)]
matched<-merge(dm,score[,.(start,pair,url,filename)],by=c("start","pair"),all=TRUE)
matched[,status:=fcase(is.na(match_id),"independent_only",is.na(url),"daft_only",default="matched")]
write_audit(matched,"county_membership_reconciliation")
print(matched[,.N,by=status])
cat("Independent register comparison complete; unmatched records remain queries.\n")

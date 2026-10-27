# Restricted mortality field review for the five-year transition analysis.
# Birth dates, age and style remain excluded. Some corroborating biographies may
# share upstream sources: this is source corroboration, not a new vital register.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
d<-read_csv(file.path(revision_dir,'audit/execution/promotion_death_queries.csv'))
refs<-c(
 'Allin'='https://www.devoncricket.co.uk/page.php?Id=1020;https://www.espn.com.sg/cricket/story/_/id/17112185/Message',
 'Dolding'='https://acscricket.com/wp-content/uploads/Sundry_Extras_Fourth_Edition.pdf',
 'Edmunds'='https://www.cricketarchive.com/Archive/Players/29/29231/29231.html',
 'Harding'='boundary-entry exclusion; death unused in primary cohort',
 'Hiscock'='boundary-entry exclusion; death unused in primary cohort',
 'Hobden'='https://sussexcricket.co.uk/news/remembering-matthew-hobden',
 'Hyett'='https://adb.anu.edu.au/biography/hyett-francis-william-frank-6783',
 'Lugton'='https://www.awm.gov.au/collection/R1638140',
 'Maynard'='https://www.icc-cricket.com/news/ecb-and-surrey-confirm-the-death-of-tom-maynard',
 'Melville'='https://eastburwoodfc.com.au/seniors/awards/;https://en.wikipedia.org/wiki/Paul_Melville',
 'Mirza'='https://cricketarchive.com/Archive/Players/4/4541/4541.html',
 "O'Keeffe"='https://www.cricketarchive.co.uk/Archive/Players/6/6041/6041.html',
 'Peters'='https://en.wikipedia.org/wiki/Arthur_Peters_(Australian_cricketer)',
 'Richter'='https://en.wikipedia.org/wiki/Arthur_Richter',
 'Ridings'='https://www.awm.gov.au/collection/C251188',
 'Roberts'='https://en.wikipedia.org/wiki/Paul_Roberts_(cricketer)',
 'Schneider'='https://www.redlegsmuseum.com.au/ON_FIELD/PLAYERS/SCHNEIDERKarl.aspx',
 'Souter'='https://en.wikipedia.org/wiki/Vernon_Souter',
 'Squires'='boundary-entry exclusion; death unused in primary cohort',
 'Such'='https://trove.nla.gov.au/newspaper/article/185457355;https://en.wikipedia.org/wiki/Bruce_Suche',
 'Thorn'='https://libraries.darebin.vic.gov.au/Darebinheritage/biographies/sports-people/frankthorn',
 'Tozer'='https://www.khs.org.au/pdfs/news_nov09.pdf;https://www.sydneyuniversitycricket.com.au/current-news/2020/12/21/claude-tozer-the-centenary-of-his-tragic-death')
d[,surname:=sub(',.*','',player)];d[,source:=unname(refs[surname])];stopifnot(!anyNA(d$source))
d[,`:=`(source_death=as.IDate(death),analysis_death=as.IDate(death),decision='Retain source date; external biographical corroboration')]
d[surname=='Allin',`:=`(analysis_death=as.IDate('2016-01-04'),decision='Correct 6 January to 4 January; report of coroner inquest')]
d[surname%in%c('Harding','Hiscock','Squires'),decision:='Outside primary entry cohort; no mortality-date assertion']
d[,source_strength:=fifelse(grepl('wikipedia',source),'Secondary corroboration; timing sensitivity required','Institutional, contemporary report, or specialist archive')]
fwrite(d,file.path(revision_dir,'config/promotion_mortality.csv'))
write_audit(d,'mortality_adjudication')
cat('Reviewed 22 candidate mortality records, 19 inside primary entry periods; one date corrected\n')

# Conservative name reconciliation within a uniquely linked match and team.
# This links records for the count-field pilot; it does not certify birthdates.
source(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])), "revision_common.R"))
q<-read_csv(file.path(revision_dir,"audit/execution/cricsheet_name_queries.csv"))
clean<-function(z) toupper(gsub("[^A-Za-z]","",iconv(z,to="ASCII//TRANSLIT")))
bare<-function(z)trimws(gsub(" *\\([^)]*\\)","",sub(" \\{b.*","",z)))
# Explicit variants checked against the two team lists. Different identities remain open.
aliases<-c("Sameer Patel"="Patel, SAS", "Sarfaraj Patel"="Patel, SAH", "H Singh"="Harmanjot Singh", "Z Khan"="Zaheer Khan", "DPMD Jayawardene"="Jayawardena, DPMD", "HAPW Jayawardene"="Jayawardane, HAPW",
 "S Randiv"="Kaluhalamulla, HKSR", "Mehedi Hasan Miraz"="Mehidy Hasan Miraz", "KS Bharat"="Srikar Bharat, K", "Washington Sundar"="Washington Sundar, MS",
 "Yousuf Youhana"="Mohammad Yousuf", "Najeeb Amar"="Najeeb Aamer", "Yasir Ali Chowdhury"="Yasir Ali",
 "Simi Singh"="Simranjit Singh", "Abdullah Shafique"="Abdullah Shafiq", "Faheem Ashraf"="Fahim Ashraf",
 "Mohammad Wasim"="Mohammad Wasim jr", "N Dickwella"="Dickwella, DPDN", "Mudassir Iqbal"="Iqbal Mudassir",
 "Anjar Tadarus"="Tadarus, MA", "MV Matsau"="Matsau, MJ", "S Sesazi"="Ssesazi, S", "S Dube"="Dubey, ShR",
 "R Sandaruwan"="de Silva, BRS", "Ahmer Bin Nisar"="Ahmer Bin Nasir", "Haider Butt"="Haider Ali Butt",
 "Imran Ali Butt"="Imran Ali", "Ubaid Minhas"="Ubaid Murtaza", "Abdul Hashim"="Abdul Hashmi",
 "B Kumar"="Bhuvneshwar Kumar", "A Dananjaya"="Perera, MKPAD", "PWH de Silva"="Hasaranga, W",
 "B Akhilesh Reddy"="Bodugum, AR", "H Lin Aung"="Htet Lin Aung", "Thuya Aung"="Thuya Aung, Y")
rows<-list()
for(co in names(configs)) {
 x<-readRDS(file.path(revision_dir,"data/staging",paste0("source_",co,".rds")))
 for(j in which(q$comp==co)) {
  r<-q[j];p<-x$players[match_id==r$match_id & teams==r$teams,.(player)]
  p[,bare:=bare(player)]; nm<-bare(r$name)
  # Preserve obvious name qualifiers in original identifiers, remove only for matching.
  pick<-which(clean(p$bare)==clean(nm));method<-"same_name_after_qualifier_removal"
  if(!length(pick) && r$name%in%names(aliases)){pick<-which(p$bare==aliases[[r$name]]);method<-"explicit_variant_checked_in_team_lists"}
  if(!length(pick)) {
   # One initial plus the complete surname must identify exactly one same-team player.
   surname<-sub("^[^ ]+ ","",nm);initial<-substr(nm,1,1)
   pick<-which(grepl(",",p$bare) & clean(sub(",.*","",p$bare))==clean(surname) &
     substr(sub("^[^,]+, *","",p$bare),1,1)==initial)
   method<-"unique_complete_surname_and_first_initial_within_team"
  }
  # Full given-name variant: Dhruv Jurel versus Jurel, DC; no nearest-string rule.
  if(!length(pick) && grepl(" ",nm)) {
   surname<-sub(".* ","",nm);initial<-substr(nm,1,1)
   pick<-which(grepl(",",p$bare) & clean(sub(",.*","",p$bare))==clean(surname) &
     substr(sub("^[^,]+, *","",p$bare),1,1)==initial)
   method<-"unique_surname_and_given_initial_within_team"
  }
  # Never collapse the New Zealand Abbas into the Pakistani bowler.
  # Lesotho Patels were resolved from full-name/DOB profiles, not from their matching scores:
  # https://www.moneycontrol.com/sports/cricket/player-profile-sarfaraj-patel-88788.html
  # https://www.cricbuzz.com/profiles/34886/samir-patel
  if(r$name=="MA Abbas")pick<-integer()
  rows[[length(rows)+1L]]<-data.table(comp=co,match_id=r$match_id,teams=r$teams,cricsheet_name=r$name,
    player=if(length(pick)==1)p$player[pick]else NA_character_,method=method,
    status=if(length(pick)==1)"linked_for_count_pilot"else"requires_identity_adjudication")
 }
}
z<-unique(rbindlist(rows))
dest<-file.path(revision_dir,"config/cricsheet_pilot_crosswalk.csv")
if(file.exists(dest))z<-unique(rbind(read_csv(dest),z,fill=TRUE),by=c("comp","match_id","teams","cricsheet_name"),fromLast=TRUE)
fwrite(z,dest)
print(z[is.na(player)])

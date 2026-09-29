# Prepare historical household records and retain an explicit audit trail.
# Run with working directory HM_revision/. Raw files are never altered.
# Blank entries are assigned zero ONLY in source fields with evidence of
# coverage. This is an assumption about reporting, not archival verification.
# Missing entire fields, ambiguous entries and invalid totals remain missing.
suppressPackageStartupMessages({library(tidyverse); library(readxl); library(here)})
dir.create(here("data", "analysis"), recursive=TRUE, showWarnings=FALSE)
dir.create(here("output", "tables"), recursive=TRUE, showWarnings=FALSE)
dir.create(here("docs", "execution"), recursive=TRUE, showWarnings=FALSE)
out_csv <- function(x, name) write_csv(x, here("output", "tables", paste0(name, ".csv")), na="")
source_files <- list.files(here("data", "raw"), full.names=TRUE)
hashes_before <- vapply(source_files, digest::digest, character(1), file=TRUE, algo="sha256")
cat("Reading source panel as text to avoid guessing identifiers or discarding annotations.\n")
src <- read_csv(here("data", "raw", "stellenbosch_temp.csv"), col_types=cols(.default=col_character()))
stopifnot(nrow(problems(src)) == 0L)
econ <- c("slave_men","slave_women","slave_sons","slave_daughters","slave_children",
          "khoe_men","khoe_women","khoe_sons","khoe_daughters","servants",
          "cattle_bull","cattle_breeding","cattle_work","cattle_cows","cattle_heifers","cattle_calves",
          "horses","horse_riding","horse_breeding","horse_work","horse_foals",
          "sheep","sheep_breeding","sheep_wether","sheep_wool","goats","pigs",
          "wheat_vol","barley_vol","rye_vol","oat_vol","vines","wine","brandy",
          "wagons","carts","rifle","swords","pistols")
stopifnot(all(econ %in% names(src)))
numeric_cell <- function(x) suppressWarnings(parse_double(x, na=c("", "NA")))
raw <- src %>% transmute(hhobs=hhobs, hhid=hhid, year=as.integer(year),
                        source_row=row_number()+1L, across(all_of(econ), numeric_cell))
stopifnot(!anyNA(raw$year), !anyNA(raw$hhobs), !anyDuplicated(raw$hhobs),
          !anyDuplicated(raw[c("hhid","year")]))
raw <- raw %>% mutate(regime=case_when(year<=1714~"A", year<=1795~"B",year<=1803~"C",
                                      year<=1829~"D", year<=1834~"E", TRUE~"F"),
                      decade=10L*(year%/%10L),
                      hhid=if_else(is.na(hhid),paste0("unlinked_",hhobs),hhid))
long <- raw %>% select(hhobs,hhid,year,source_row,all_of(econ)) %>%
  pivot_longer(all_of(econ),names_to="variable",values_to="value")
invalid <- map_dfr(econ,function(v) {
  bad <- !is.na(src[[v]]) & is.na(raw[[v]])
  tibble(hhobs=raw$hhobs[bad],year=raw$year[bad],source_row=raw$source_row[bad],
         variable=v,source_value=src[[v]][bad],decision="ambiguous_numeric_entry_retained_missing")
})
out_csv(invalid,"source_parse_flags")
availability <- long %>% group_by(year,variable) %>% summarise(n=n(),
  n_observed=sum(!is.na(value)), n_zero=sum(value==0,na.rm=TRUE), n_positive=sum(value>0,na.rm=TRUE),
  n_blank=sum(is.na(value)), available=n_observed>0, .groups="drop") %>%
  mutate(category=case_when(str_detect(variable,"slave|khoe|servant")~"Labour",
    str_detect(variable,"cattle|horse|sheep|goat|pig")~"Livestock",
    str_detect(variable,"rifle|sword|pistol")~"Weapons",variable%in%c("wagons","carts")~"Transport",TRUE~"Crops"),
    coverage_evidence="at least one numeric entry in the transcribed field-year",
    blank_rule="conditional zero if field-year observed; ambiguous numeric entries stay missing")
out_csv(availability,"source_coverage_panel")
saveRDS(availability,here("data","analysis","var_availability.rds"))

# Keep a missingness copy. Positive-count selection later uses the harmonised
# measures, whereas blank diagnostics refer to the actual source components.
clean <- raw
for(v in econ) {
  covered <- availability$year[availability$variable==v & availability$available]
  genuine_blank <- is.na(src[[v]])
  clean[[v]][genuine_blank & raw$year %in% covered] <- 0
}
sum_components <- function(df, columns) rowSums(as.matrix(df[columns]),na.rm=FALSE)
clean$slaves_total <- NA_real_
eligible_slave_period <- clean$year>=1738 & clean$year<=1825
clean$slaves_total[eligible_slave_period] <- sum_components(clean,c("slave_men","slave_women","slave_sons","slave_daughters"))[eligible_slave_period]
clean$cattle_total <- clean$horses_total <- clean$sheep_total <- NA_real_
earlier <- clean$year>=1738 & clean$year<=1803
later <- clean$year>=1804 & clean$year<=1825
clean$cattle_total[earlier] <- clean$cattle_bull[earlier]
clean$horses_total[earlier] <- clean$horses[earlier]
clean$sheep_total[earlier] <- clean$sheep[earlier]
clean$cattle_total[later] <- sum_components(clean,c("cattle_work","cattle_breeding"))[later]
clean$horses_total[later] <- sum_components(clean,c("horse_riding","horse_breeding"))[later]
# Include wool-bearing sheep: the old pipeline omitted a recorded category.
# If the category has no observed entry that year, its value is unknown and
# the total is not admitted into the comparable analysis.
clean$sheep_total[later] <- sum_components(clean,c("sheep_breeding","sheep_wether","sheep_wool"))[later]
clean$grain_other <- sum_components(clean,c("barley_vol","rye_vol"))
clean$khoe_total <- sum_components(clean,c("khoe_men","khoe_women","khoe_sons","khoe_daughters"))
clean$weapons_total <- sum_components(clean,c("rifle","swords","pistols"))
clean$has_slaves <- if_else(is.na(clean$slaves_total),NA_integer_,as.integer(clean$slaves_total>0))
vars_D <- c("slave_men","slave_women","khoe_total","cattle_total","horses_total","sheep_total",
            "vines","wine","brandy","wheat_vol","grain_other","wagons","goats")
vars_bridge <- c("slaves_total","cattle_total","horses_total","sheep_total","vines","wine","wheat_vol","grain_other")
vars_B <- vars_bridge
vars_colony <- c("total_slaves","total_khoe","horses","cattle","sheep","goats","wheat_reaped","grain_other_reaped","wine","brandy")
components_D <- c("slave_men","slave_women","khoe_men","khoe_women","khoe_sons","khoe_daughters",
                   "cattle_work","cattle_breeding","horse_riding","horse_breeding","sheep_breeding","sheep_wether","sheep_wool",
                   "vines","wine","brandy","wheat_vol","barley_vol","rye_vol","wagons","goats")
clean$n_observed_components <- rowSums(!is.na(raw[components_D]))
clean$n_blank_active <- rowSums(is.na(raw[components_D]))
clean$any_blank_active <- clean$n_blank_active>0
clean$all_active_observed <- clean$n_blank_active==0
components_bridge_common<-c("slave_men","slave_women","slave_sons","slave_daughters","vines","wine","wheat_vol","barley_vol","rye_vol")
components_bridge_early<-c(components_bridge_common,"cattle_bull","horses","sheep")
components_bridge_late<-c(components_bridge_common,"cattle_work","cattle_breeding","horse_riding","horse_breeding","sheep_breeding","sheep_wether","sheep_wool")
clean$n_observed_components_bridge<-ifelse(clean$year<1804,
  rowSums(!is.na(raw[components_bridge_early])),rowSums(!is.na(raw[components_bridge_late])))

# Flags are sensitivity criteria, not assertions that an archival entry is
# wrong. No unresolved source quantity is silently overwritten.
ceilings <- c(slaves_total=200,slave_men=150,slave_women=150,khoe_total=100,
              cattle_total=1500,horses_total=500,sheep_total=15000,vines=500000,
              wine=500,brandy=200,wheat_vol=3000,grain_other=3000,wagons=100,goats=5000)
flag_long <- clean %>% select(hhobs,hhid,year,source_row,all_of(names(ceilings))) %>%
  pivot_longer(all_of(names(ceilings)),names_to="variable",values_to="value") %>%
  mutate(ceiling=unname(ceilings[variable])) %>% filter(!is.na(value) & (value<0 | value>ceiling)) %>%
  mutate(decision="retained if nonnegative; exclusion assessed as sensitivity",verification="original archival scan unavailable")
out_csv(flag_long,"source_value_flags_panel")
clean$any_outlier <- clean$hhobs %in% flag_long$hhobs
eligible_matrix <- function(df,vars) {
  m<-as.matrix(df[vars]); complete.cases(m) & rowSums(!is.finite(m)|m<0,na.rm=TRUE)==0
}
base_D <- clean$year>=1804 & clean$year<=1829
base_B <- clean$year>=1738 & clean$year<=1795
base_bridge <- clean$year>=1738 & clean$year<=1825
mask_D <- base_D & eligible_matrix(clean,vars_D) & !is.na(clean$slaves_total)
mask_B <- base_B & eligible_matrix(clean,vars_B)
mask_bridge <- base_bridge & eligible_matrix(clean,vars_bridge)
make_sample <- function(mask,vars) {
  d<-clean[mask,]
  if(identical(vars,vars_bridge))d$n_observed_components<-d$n_observed_components_bridge
  d %>% mutate(n_nonzero=rowSums(across(all_of(vars),~.x>0)),old_filter=n_nonzero>=3)
}
wide_D<-mask_D;wide_B<-mask_B;wide_bridge<-mask_bridge
saveRDS(make_sample(wide_D,vars_D),here("data","analysis","regime_D_blank_assumption.rds"))
saveRDS(make_sample(wide_B,vars_B),here("data","analysis","regime_B_blank_assumption.rds"))
saveRDS(make_sample(wide_bridge,vars_bridge),here("data","analysis","bridge_blank_assumption.rds"))
# An entirely blank economic return is not evidence of zero ownership.
# Literal recorded zeros still count as observations; no positive-asset rule.
mask_D<-wide_D & clean$n_observed_components>0
mask_B<-wide_B & clean$n_observed_components_bridge>0
mask_bridge<-wide_bridge & clean$n_observed_components_bridge>0
D <- make_sample(mask_D,vars_D); B <- make_sample(mask_B,vars_B); bridge<-make_sample(mask_bridge,vars_bridge)
stopifnot(nrow(D)>1000,nrow(B)>1000,nrow(bridge)>1000,
          all(vapply(D[vars_D],function(x)all(is.finite(x)&x>=0),logical(1))))
sample_register <- clean %>% transmute(hhobs,hhid,year,source_row,regime,
  eligible_D=mask_D,eligible_B=mask_B,eligible_bridge=mask_bridge,
  eligible_D_if_allblank_zero=wide_D,eligible_B_if_allblank_zero=wide_B,eligible_bridge_if_allblank_zero=wide_bridge,
  observed_D=clean$n_observed_components,observed_bridge=clean$n_observed_components_bridge,
  reason_D=case_when(!base_D~"outside nominal Regime D",mask_D~"included",
                    wide_D & clean$n_observed_components==0~"entirely blank active economic return",
                    TRUE~"incomplete component coverage, invalid value or noncomparable template"))
out_csv(sample_register,"sample_membership")
sample_flow <- bind_rows(
  tibble(sample="Panel source",stage="all source rows",n=nrow(clean),households=n_distinct(clean$hhid)),
  tibble(sample="Regime D",stage="nominal period 1804-1829",n=sum(base_D),households=n_distinct(clean$hhid[base_D])),
  tibble(sample="Regime D",stage="comparable columns before wholly blank returns excluded",n=sum(wide_D),households=n_distinct(clean$hhid[wide_D])),
  tibble(sample="Regime D",stage="comparable complete variables; no asset-count filter",n=nrow(D),households=n_distinct(D$hhid)),
  tibble(sample="Regime D",stage="old three-positive-variable rule within revised sample",n=sum(D$old_filter),households=n_distinct(D$hhid[D$old_filter])),
  tibble(sample="Regime B",stage="comparable complete variables",n=nrow(B),households=n_distinct(B$hhid)),
  tibble(sample="Bridge",stage="comparable complete variables 1738-1825",n=nrow(bridge),households=n_distinct(bridge$hhid)))
out_csv(sample_flow,"sample_flow")
out_csv(sample_register %>% group_by(year,regime) %>% summarise(source_n=n(),D_n=sum(eligible_D),bridge_n=sum(eligible_bridge),.groups="drop"),"analysis_coverage_by_year")
out_csv(D %>% mutate(group=if_else(old_filter,"at least three positive variables","fewer than three positive variables")) %>%
  group_by(group) %>% summarise(n=n(),across(all_of(vars_D),mean),.groups="drop"),"selection_profiles")
saveRDS(clean,here("data","analysis","opgaaf_clean.rds"))
saveRDS(D,here("data","analysis","regime_D_biplot.rds"));saveRDS(B,here("data","analysis","regime_B_biplot.rds"))
saveRDS(bridge,here("data","analysis","bridge_biplot.rds"))
cat("Panel samples: D",nrow(D),"B",nrow(B),"bridge",nrow(bridge),"\nD years:",paste(sort(unique(D$year)),collapse=", "),"\n")

# Positional maps are checked against the supplied workbook header rows.
# Save every map and original Excel row so exclusions can be independently
# reviewed. Wolgevende (wool-bearing) sheep are included, not discarded.
fields <- c("settler_men","settler_women","slaves_men","slaves_women","slaves_sons","slaves_daughters",
            "khoe_men","khoe_women","khoe_sons","khoe_daughters","horses_saddle","horses_breeding",
            "cattle_oxen","cattle_breeding","sheep_wethers","sheep_breeding","sheep_wool",
            "goats","wheat_reaped","barley_reaped","rye_reaped","oats_reaped","wine","brandy")
maps <- list(
  Cape=c(4,5,13,14,23,24,7,8,17,18,25,26,27,28,29,30,31,33,39,40,41,42,45,46),
  Stellenbosch=c(5,6,21,23,22,24,17,19,18,20,27,28,29,30,31,32,33,35,41,42,44,43,47,48),
  `Graaff-Reinet`=c(5,6,18,20,19,21,10,12,11,13,26,27,28,29,30,31,32,34,40,41,42,43,45,46),
  Swellendam=c(4,5,17,19,18,20,9,11,10,12,25,26,27,28,29,30,31,33,39,40,41,42,44,45),
  Albany=c(6,7,19,21,20,22,11,13,12,14,27,28,29,30,31,32,33,35,41,42,43,44,46,47),
  Beaufort=c(6,7,19,21,20,22,11,13,12,14,27,28,29,30,31,32,33,35,41,42,43,44,46,47),
  George=c(6,7,19,21,20,22,11,13,12,14,27,28,29,30,31,32,33,35,41,42,43,44,46,47),
  Uitenhage=c(6,7,19,21,20,22,11,13,12,14,27,28,29,30,31,32,33,35,41,42,43,44,46,47),
  Clanwilliam=c(6,7,20,22,21,23,16,18,17,19,24,25,26,27,28,29,30,31,37,38,39,40,42,43),
  Cradock=c(6,7,16,18,17,19,12,14,13,15,20,21,22,23,24,25,26,27,33,34,36,35,38,39),
  Worcester=c(7,8,21,23,22,24,17,19,18,20,25,26,27,28,29,30,31,33,39,40,41,42,44,45))
sheets <- c(Cape="Cape district 1825",Stellenbosch="Stellenbosch 1825",`Graaff-Reinet`="Graaff-Reinet 1825",
  Swellendam="Swellendam 1825",Albany="Albany 1825",Beaufort="Beaufort 1825",George="George 1825",
  Uitenhage="Uitenhage 1825",Clanwilliam="Clanwilliam 1824",Cradock="Cradock 1823",Worcester="Worcester 1824")
skips<-c(Cape=3,Stellenbosch=3,`Graaff-Reinet`=4,Swellendam=2,Albany=3,Beaufort=2,George=2,Uitenhage=2,Clanwilliam=2,Cradock=2,Worcester=2)
colony_parse <- function(x) vapply(x,function(v) {
  if(is.na(v)||!nzchar(trimws(v)))return(NA_real_)
  lines<-trimws(strsplit(v,"\n",fixed=TRUE)[[1]])
  # Exact vulgar fractions and scientific notation are quantities, not errors.
  # Use Unicode escapes so parsing is independent of the Windows code page.
  fractions<-setNames(c(1/4,1/2,3/4,1/7,1/9,1/10,1/3,2/3,1/5,2/5,3/5,4/5,1/6,5/6,1/8,3/8,5/8,7/8),
    c("\u00bc","\u00bd","\u00be","\u2150","\u2151","\u2152","\u2153","\u2154","\u2155","\u2156","\u2157","\u2158","\u2159","\u215a","\u215b","\u215c","\u215d","\u215e"))
  parsed<-vapply(lines,function(line) {
    line<-sub("^\u215f", "1/", line)
    if(grepl("^[0-9]+ sixteenths?$",line))return(as.numeric(sub(" .*","",line))/16)
    if(grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$",line))return(as.numeric(line))
    last<-substr(line,nchar(line),nchar(line)); first<-trimws(substr(line,1,nchar(line)-1))
    if(last%in%names(fractions)&&(!nzchar(first)||grepl("^[+-]?[0-9]+$",first))) {
      whole<-if(nzchar(first))as.numeric(first) else 0
      return(whole+ifelse(startsWith(first,"-"),-1,1)*fractions[[last]])
    }
    # A pure ASCII fraction or mixed number is also unambiguous.
    if(grepl("^([0-9]+ +)?[0-9]+/[0-9]+$",line)) {
      bits<-strsplit(line," +")[[1]]; frac<-as.numeric(strsplit(tail(bits,1),"/",fixed=TRUE)[[1]])
      if(frac[2]>0)return(if(length(bits)==2)as.numeric(bits[1])+frac[1]/frac[2] else frac[1]/frac[2])
    }
    NA_real_
  },numeric(1))
  candidates<-unique(parsed[is.finite(parsed)])
  if(length(candidates)==1L)candidates else NA_real_
},numeric(1))
stopifnot(isTRUE(all.equal(unname(colony_parse(c("100\u00bd","\u215b","6.25E-2","1 1/4","Burnt"))),c(100.5,.125,.0625,1.25,NA_real_))))
excel_coverage<-excel_flags<-excel_maps<-excel_exclusions<-list()
colony_list<-imap(maps,function(map,district) {
  sheet<-sheets[[district]];skip<-skips[[district]]
  full<-suppressMessages(read_excel(here("data","raw","1825 series.xlsx"),sheet=sheet,col_names=FALSE,col_types="text"))
  df<-full[-seq_len(skip),,drop=FALSE];names(df)<-paste0("c",seq_len(ncol(df)))
  txt<-df[,map];names(txt)<-fields
  num<-as_tibble(lapply(txt,colony_parse))
  excel_maps[[district]]<<-tibble(district=district,sheet=sheet,variable=fields,column=map,
    header=vapply(map,function(j)paste(na.omit(full[[j]][seq_len(skip)]),collapse=" / "),character(1)))
  excel_coverage[[district]]<<-tibble(district=district,sheet=sheet,year=as.integer(str_extract(sheet,"[0-9]{4}")),variable=fields,
    n=nrow(txt),n_blank=vapply(txt,function(x)sum(is.na(x)),integer(1)),
    n_zero=vapply(num,function(x)sum(x==0,na.rm=TRUE),integer(1)),
    n_positive=vapply(num,function(x)sum(x>0,na.rm=TRUE),integer(1)),
    coverage_evidence="mapped column in inspected workbook header",blank_rule="blank assigned zero, conditional reporting assumption")
  ambiguous<-imap_dfr(txt,function(x,v) {
    bad<-!is.na(x)&is.na(num[[v]])
    tibble(district=district,sheet=sheet,source_row=which(bad)+skip,variable=v,source_value=x[bad])
  })
  excel_flags[[district]]<<-ambiguous
  economic_observed<-rowSums(!is.na(num[setdiff(fields,c("settler_men","settler_women","oats_reaped"))]))
  for(v in fields)num[[v]][is.na(txt[[v]])]<-0
  head<-num$settler_men==1 | num$settler_women==1
  head[is.na(head)]<-FALSE
  if(district=="Swellendam")head<-head & !is.na(colony_parse(df$c1))
  if(district%in%c("Stellenbosch","Graaff-Reinet","Swellendam"))head<-head & !is.na(df$c2)
  num<-num %>% mutate(district=district,source_sheet=sheet,source_row=row_number()+skip,
    year=as.integer(str_extract(sheet,"[0-9]{4}")),record_nr=paste0(district,"_",source_row),
    n_blank_components=rowSums(is.na(txt)),n_observed_components=economic_observed,
    any_ambiguous=source_row%in%ambiguous$source_row)
  excel_exclusions[[district]]<<-num %>% transmute(district,source_sheet,source_row,year,
    included_head_marker=head,reason=if_else(head,"head marker accepted","no accepted household head marker"))
  num[head,]
})
colony_all<-bind_rows(colony_list) %>% mutate(
  total_slaves=slaves_men+slaves_women+slaves_sons+slaves_daughters,
  total_khoe=khoe_men+khoe_women+khoe_sons+khoe_daughters,
  horses=horses_saddle+horses_breeding,cattle=cattle_oxen+cattle_breeding,
  sheep=sheep_wethers+sheep_breeding+sheep_wool,grain_other_reaped=barley_reaped+rye_reaped,
  has_slaves=as.integer(total_slaves>0),hhobs=record_nr,hhid=record_nr)
colony_limits<-c(total_slaves=200,total_khoe=100,horses=300,cattle=1000,sheep=10000,goats=5000,wheat_reaped=2000,grain_other_reaped=2000,wine=500,brandy=200)
colony_flags<-colony_all %>% select(record_nr,district,source_sheet,source_row,year,all_of(vars_colony)) %>%
  pivot_longer(all_of(vars_colony),names_to="variable",values_to="value") %>%
  mutate(ceiling=unname(colony_limits[variable])) %>% filter(!is.na(value)&(value<0|value>ceiling)) %>%
  mutate(decision="retained if nonnegative; exclusion sensitivity",verification="workbook entry checked; archival scan unavailable")
colony_all$any_outlier<-colony_all$record_nr%in%colony_flags$record_nr
colony_wide<-colony_all[eligible_matrix(colony_all,vars_colony),]
saveRDS(colony_wide,here("data","analysis","colony_blank_assumption.rds"))
colony_1825<-colony_wide[colony_wide$n_observed_components>0,]
out_csv(bind_rows(excel_maps),"source_column_map_colony")
out_csv(bind_rows(excel_coverage),"source_coverage_colony")
out_csv(bind_rows(excel_flags),"source_parse_flags_colony")
out_csv(bind_rows(excel_exclusions),"source_head_selection_colony")
out_csv(colony_flags,"source_value_flags_colony")
out_csv(colony_all %>% transmute(record_nr,district,source_sheet,source_row,year,any_outlier,any_ambiguous,
  n_observed_components,eligible_if_allblank_zero=record_nr%in%colony_wide$record_nr,
  eligible=record_nr%in%colony_1825$record_nr,
  reason=case_when(eligible~"included",eligible_if_allblank_zero & n_observed_components==0~"entirely blank active economic return",
                  TRUE~"incomplete or invalid active quantity")),"sample_membership_colony")
saveRDS(colony_1825,here("data","analysis","colony_1825.rds"))
sample_flow<-bind_rows(sample_flow,tibble(sample="Colony",stage="accepted head markers",n=nrow(colony_all),households=nrow(colony_all)),
  tibble(sample="Colony",stage="comparable columns before wholly blank returns excluded",n=nrow(colony_wide),households=nrow(colony_wide)),
  tibble(sample="Colony",stage="complete nonnegative active quantities; no ceiling exclusions",n=nrow(colony_1825),households=nrow(colony_1825)))
out_csv(sample_flow,"sample_flow")

definitions<-tibble(variable=c(vars_D,"slaves_total"),
  description=c("Recorded adult male enslaved persons","Recorded adult female enslaved persons","Recorded Khoekhoe labourers",
    "Cattle: work and breeding","Horses: riding and breeding","Sheep: breeding, wethers and wool-bearing",
    "Vines","Wine output","Brandy output","Wheat harvest","Barley and rye harvest","Wagons","Goats","Enslaved persons: men, women, sons and daughters"),
  unit=c(rep("count",7),"leaguers","leaguers","muids","muids","count","count","count"),
  rule=c("slave_men","slave_women","khoe_men+khoe_women+khoe_sons+khoe_daughters","cattle_work+cattle_breeding",
    "horse_riding+horse_breeding","sheep_breeding+sheep_wether+sheep_wool","vines","wine","brandy","wheat_vol",
    "barley_vol+rye_vol; oats excluded throughout","wagons","goats","slave_men+slave_women+slave_sons+slave_daughters"))
out_csv(definitions,"variable_definitions_revised")
config<-list(vars_D=vars_D,vars_B=vars_B,vars_bridge=vars_bridge,vars_colony=vars_colony,
  sample_flow=sample_flow,definitions=definitions,ceilings=ceilings,colony_ceilings=colony_limits,
  source_assumption="Within economically observed returns, on-covered-field blanks are treated as zero conditionally; wholly blank returns, structural absence and ambiguous entries are excluded. Broad blank-as-zero samples are sensitivity only.",
  D_years=sort(unique(D$year)),bridge_years=sort(unique(bridge$year)),
  restricted_sources=basename(source_files),retired_inputs=c("spouse_temp.csv","1830s series.xlsx"),
  source_flags=flag_long,colony_flags=colony_flags)
saveRDS(config,here("data","analysis","analysis_config.rds"))
hashes_after<-vapply(source_files,digest::digest,character(1),file=TRUE,algo="sha256")
stopifnot(identical(hashes_before,hashes_after))
out_csv(tibble(file=basename(source_files),sha256=hashes_after),"raw_input_hashes")
cat("Colony",nrow(colony_1825),"records; flagged",sum(colony_1825$any_outlier),"retained.\n")
print(sample_flow)
cat("01_clean.R complete; source hashes unchanged.\n")

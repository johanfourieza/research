suppressPackageStartupMessages({library(dplyr);library(jsonlite);library(nnet)})
z <- readRDS('output/tables/final_analysis_state.rds')
cp <- readRDS('output/tables/linkage_review_checkpoint.rds')
d <- z$analysis_dataset_main; b <- z$best_matches; v <- z$vt_adults
write.csv(b,'output/tables/final_pairs_complete.csv',row.names=FALSE,fileEncoding='UTF-8')
for (nm in c('quartile_rates','comp_desc','year_stats','leader_stats','dest_stats','desc_emancipation',
             'dest_ci','dest_anova_results','dest_means')) {
  if (nm %in% names(z)) write.csv(z[[nm]],paste0('output/tables/paper_',nm,'.csv'),row.names=FALSE)
}
# Recompute descriptive linkage diagnostics on final identities, not proposals.
# The declared-origin table uses all records with a primary candidate.
eligible <- v %>% filter(row_id %in% cp$candidates$row_id) %>%
  mutate(matched=row_id %in% b$row_id,
    origin=case_when(census_districts %in% c('Somerset_multi','Cradock') ~ 'Somerset',
      census_districts == 'Colesberg_multi' ~ 'Graaff-Reinet',
      census_districts == 'Clanwilliam' ~ 'Worcester', TRUE ~ census_districts))
origin_flow <- v %>% mutate(matched=row_id %in% b$row_id,
  origin=case_when(census_districts %in% c('Somerset_multi','Cradock') ~ 'Somerset',
    census_districts == 'Colesberg_multi' ~ 'Graaff-Reinet',
    census_districts == 'Clanwilliam' ~ 'Worcester', TRUE ~ census_districts))
rates <- origin_flow %>% group_by(origin) %>% summarise(records=n(),matched=sum(matched),rate=100*matched/records,.groups='drop')
write.csv(rates,'output/tables/final_match_rates_by_origin.csv',row.names=FALSE)
write.csv(eligible %>% count(matched,leader_std) %>% group_by(matched) %>% mutate(pct=100*n/sum(n)),
  'output/tables/matched_vs_unmatched_leaders.csv',row.names=FALSE)
write.csv(eligible %>% count(matched,origin) %>% group_by(matched) %>% mutate(pct=100*n/sum(n)),
  'output/tables/final_matched_unmatched_origins.csv',row.names=FALSE)
freq <- cp$candidates %>% select(row_id,surname_freq) %>% distinct(row_id,.keep_all=TRUE)
eligible <- eligible %>% left_join(freq,by='row_id') %>% mutate(
  has_wife=(!is.na(wife_name) & trimws(wife_name)!='') | (!is.na(wife_surname) & trimws(wife_surname)!=''))
match_summary <- eligible %>% group_by(matched) %>% summarise(n=n(),wife_pct=100*mean(has_wife),surname_freq=mean(surname_freq),.groups='drop')
write.csv(match_summary,'output/tables/final_matched_unmatched_summary.csv',row.names=FALSE)
write.csv(eligible %>% count(matched,origin) %>% group_by(matched) %>% mutate(pct=100*n/sum(n)),
  'output/tables/matched_vs_unmatched_districts.csv',row.names=FALSE)
match_tests <- data.frame(variable=c('Wife name available','Surname frequency'),
  p=c(chisq.test(table(eligible$matched,eligible$has_wife))$p.value,
      t.test(surname_freq~matched,data=eligible)$p.value))
write.csv(match_tests,'output/tables/matched_vs_unmatched.csv',row.names=FALSE)
vars <- c('wealth_index','household_size','settler_children','total_slaves','cattle','sheep','horses','total_khoe','wine','wheat_sown','wheat_reaped')
counts <- bind_rows(lapply(vars,function(nm) {
  ok <- complete.cases(d[,c(nm,'is_voortrekker','district')]);
  data.frame(variable=nm,n=sum(ok),n_vt=sum(d$is_voortrekker[ok]),n_control=sum(!d$is_voortrekker[ok]))
}))
write.csv(counts,'output/tables/outcome_sample_counts.csv',row.names=FALSE)
summary <- list(n_households=nrow(d),n_vt=sum(d$is_voortrekker),n_control=sum(!d$is_voortrekker),
  n_pairs=nrow(b),n_candidates=n_distinct(cp$candidates$row_id),n_name_eligible=nrow(v),
  n_primary_linked=sum(eligible$matched),n_cross_only=sum(!b$row_id %in% cp$candidates$row_id),
  n_candidate_unknown_birth=sum(is.na(cp$vt_adults$birth_yr[match(unique(cp$candidates$row_id),cp$vt_adults$row_id)])),
  n_name_unknown_birth=sum(is.na(cp$vt_adults$birth_yr)),
  n_ambiguous_households=n_distinct(b$census_id[b$identity_ambiguous | b$n_vt_per_census>1]),
  n_unique_person_households=sum(b$n_vt_per_census==1 & !b$identity_ambiguous),
  n_male_control=sum(!d$is_voortrekker & d$settler_men>=1),
  n_female_control=sum(!d$is_voortrekker & d$settler_men<1),
  vt_one_male_pct=100*mean(d$settler_men[d$is_voortrekker]==1),
  control_one_male_pct=100*mean(d$settler_men[!d$is_voortrekker]==1),
  timing_n=nrow(z$timing_analysis),
  comp_complete=nrow(z$owners_with_valuation),comp_vt=sum(z$owners_with_valuation$is_voortrekker),
  quality=as.list(table(b$match_quality)))
write_json(summary,'output/tables/paper_summary.json',pretty=TRUE,auto_unbox=TRUE)
if ('mlogit_model' %in% names(z)) {
  mlogit_data <- z$matched_with_trek %>% filter(destination_std %in% z$dest_means$destination_std) %>%
    mutate(destination_std=factor(destination_std),wealth_scaled=as.numeric(scale(wealth_index)))
  mm <- summary(z$mlogit_model); cm <- mm$coefficients; sm <- mm$standard.errors
  a <- as.data.frame(as.table(cm));names(a)<-c('destination','term','coef')
  a$se <- as.vector(sm);a$p <- 2*pnorm(-abs(a$coef/a$se))
  write.csv(a,'output/tables/destination_multinomial_coefficients.csv',row.names=FALSE)
}
cat('Paper evidence exported; final match rate denominator',nrow(eligible),'\n')

stats <- bind_rows(lapply(c('wt_comp_rate','wt_loss_pct','chisq_result','trend_cor','fisher_q4q1'),function(nm) {
 o<-z[[nm]];data.frame(test=nm,p=o$p.value,statistic=if(length(o$statistic)) unname(o$statistic) else NA_real_,estimate=if(length(o$estimate)) unname(o$estimate) else NA_real_)
}))
write.csv(stats,'output/tables/paper_compensation_tests.csv',row.names=FALSE)
write.csv(data.frame(term=c('loss_m3','slaves_m3','loss_m5'),ame_pp=100*c(z$ame_loss_pct_3,z$ame_nslaves_3,z$ame_loss_pct_5)), 'output/tables/paper_compensation_ames.csv',row.names=FALSE)

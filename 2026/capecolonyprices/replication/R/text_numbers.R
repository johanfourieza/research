# Additional numeric claims stated in the main text and appendix.
source(here::here('replication','R','helpers.R'))
a <- v2_read('annual_inflation_1700_1834.csv')
v <- v2_read('annual_variant_conditional_summaries.csv')
h <- v2_read('historical_functionals_all.csv')
claims <- list()
add <- function(claim,value,unit) claims[[length(claims)+1L]] <<- tibble(claim,value,unit)
early <- h |> filter(variant_id!='primary',functional=='ratio_1818_1824_to1795')
add('alternatives_1818_1824_to1795_min',min(early$point),'ratio')
add('alternatives_1818_1824_to1795_max',max(early$point),'ratio')
add('alternatives_1818_1824_to1795_below_three',sum(early$point<3),'models')
paths <- v |> group_by(variant_id) |> summarise(peak=year[which.max(index_point)],
 relative1834_1822=index_point[year==1834]/index_point[year==1822],.groups='drop')
add('all_alternative_point_peaks_1822',sum(paths$peak==1822),'models')
add('alternatives_1834_to1822_min',min(paths$relative1834_1822),'ratio')
add('alternatives_1834_to1822_max',max(paths$relative1834_1822),'ratio')
for(id in c('official_sources_excluded','wheat1787_excluded')) add(paste0(id,'_inflation1787'),v$inflation_pct[v$variant_id==id&v$year==1787],'percent')
money <- v2_read('nominal_silver_comparator.csv')
for(sc in unique(money$monetary_scenario)) {
 z <- filter(money,monetary_scenario==sc)
 add(paste0(sc,'_fraction_post1795_log_inflation_removed_1822'),1-log(z$index_silver[z$year==1822]/z$index_silver[z$year==1795])/log(a$index_point[a$year==1822]/a$index_point[a$year==1795]),'share')
 add(paste0(sc,'_silver_peak_year'),z$year[which.max(z$index_silver)],'year')
}
write_csv(bind_rows(claims),file.path(v2_out,'additional_text_numbers.csv'))

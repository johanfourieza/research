# CSV is the distribution format. Reconstitute matrices only as local intermediates.
# Reuse public alternative summaries rather than distributing duplicate copies.
analysis <- 'replication/work/analysis'
for(pair in list(c('alternative_annual_series.csv','annual_variant_conditional_summaries.csv'),
 c('alternative_model_settings.csv','v2_variant_settings.csv'))) {
 dest <- file.path(analysis,pair[2])
 if(!file.exists(dest)) stopifnot(file.copy(file.path('data/series',pair[1]),dest))
}
if(!file.exists(file.path(analysis,'annual_structural_variants.csv'))) {
 z <- readr::read_csv(file.path(analysis,'annual_variant_conditional_summaries.csv'),show_col_types=FALSE)
 write_csv(dplyr::transmute(z,variant_id,year,index=index_point,inflation_pct),file.path(analysis,'annual_structural_variants.csv'))
}
restore <- function(input,output,seed) {
 d <- readr::read_csv(input,show_col_types=FALSE) |> dplyr::arrange(draw_id,year)
 years <- sort(unique(d$year));ids <- sort(unique(d$draw_id))
 stopifnot(nrow(d)==length(years)*length(ids),!anyDuplicated(d[c('year','draw_id')]))
 z <- list(year=as.integer(years),draw_id=as.integer(ids),index=matrix(d$index,nrow=length(years)),seed=seed,
 scope='conditional_on_fitted_variance_parameters_units_source_prior_and_model')
 saveRDS(z,output)
}
if(!file.exists('data/series/annual_index_joint_draws.rds')) restore('data/series/annual_index_joint_draws.csv.gz','data/series/annual_index_joint_draws.rds',20261109L)
if(!file.exists('replication/work/analysis/variants/offsets_constant_joint.rds')) restore('data/series/offsets_constant_joint_draws.csv.gz','replication/work/analysis/variants/offsets_constant_joint.rds',20261112L)

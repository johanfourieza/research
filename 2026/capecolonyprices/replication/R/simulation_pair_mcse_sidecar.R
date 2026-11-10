suppressPackageStartupMessages({library(tidyverse);library(here);library(digest)})
out<-here::here("replication","work","analysis","simulation","revised_primary1795_addon")
p<-read_csv(file.path(out,'paired_replicate_functional_differences.csv'),show_col_types=FALSE)
# The frozen summarizer's sequential summarise masks its input coverage_difference
# with the scalar mean before computing sd(). Recalculate this MCSE from the saved
# replicate differences. No fit, generator, mean, or other statistic is changed.
mc<-p |> group_by(scenario,functional) |> summarise(coverage_difference_mcse=sd(coverage_difference)/sqrt(n()),.groups='drop')
summary<-read_csv(file.path(out,'paired_stress_functional_summary.csv'),show_col_types=FALSE)
corrected<-summary |> select(-coverage_difference_mcse) |> left_join(mc,by=c('scenario','functional'))
stopifnot(all(is.finite(corrected$coverage_difference_mcse[corrected$n>1])))
write_csv(corrected,file.path(out,'paired_stress_functional_summary.csv'))
write_csv(mc,file.path(out,'paired_coverage_mcse_verified.csv'))
write_lines(c('Summary-only correction: run after simulation_addon_summarize.R.',
 'The frozen summarizer used the same name for the replicate coverage difference and its group mean. dplyr sequential summarise then computed sd() of a scalar, returning NA. This sidecar recomputes the MCSE directly from paired_replicate_functional_differences.csv and joins it onto the summary. Means, all other metrics, fitting code, checkpoints and original frozen scripts are unchanged.',
 paste('Sidecar SHA256:',digest(file=here::here("replication","R","simulation_pair_mcse_sidecar.R"),algo='sha256'))),file.path(out,'paired_mcse_correction_note.txt'))
cat('Paired coverage MCSE verified for',nrow(mc),'functional/scenario groups.\n')

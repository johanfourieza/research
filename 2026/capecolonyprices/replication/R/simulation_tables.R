source(here::here("replication","R","helpers.R"))
base <- file.path(v2_out,"simulation","revised_primary1795")
addon <- file.path(v2_out,"simulation","revised_primary1795_addon")
read <- function(dir,name) read_csv(file.path(dir,name),show_col_types=FALSE,num_threads=1)
stopifnot(sum(read(base,"progress.csv")$successful)==1020L,
          sum(read(addon,"progress.csv")$successful)==200L)
tab <- file.path(v2_root,"generated_tables")
fmt <- function(x,d=3) formatC(x,format="f",digits=d)
write_rows <- function(x,name) writeLines(apply(as.data.frame(x),1,function(z)
 paste0(paste(z,collapse=" & ")," \\\\")),file.path(tab,name))
fit_label <- function(x) ifelse(x=="primary","Re-estimated variances","Generating variances")
groups <- c("all","post1795","goods_0")
b <- read(base,"basket_recovery_summary.csv") |>
 filter(scenario=="S0_prior_calibration",group%in%groups,
        metric%in%c("pointwise_coverage","log_rmse")) |>
 select(fit_variant,group,metric,mean,mcse,n) |>
 pivot_wider(names_from=metric,values_from=c(mean,mcse,n)) |>
 arrange(match(group,groups),match(fit_variant,c("primary","true_parameter_plugin")))
stopifnot(all(b$n_pointwise_coverage==200L),all(b$n_log_rmse==200L))
write_csv(b,file.path(v2_out,"paper_table_simulation_calibration.csv"))
write_rows(b |> transmute(group=paste0("\\simgrouplabel{",group,"}"),fit=fit_label(fit_variant),
 coverage=fmt(mean_pointwise_coverage),mcse=fmt(mcse_pointwise_coverage),
 rmse=fmt(mean_log_rmse),rmse_mcse=fmt(mcse_log_rmse)),"simulation_calibration_rows.tex")
functionals <- c("ratio_1770s_to1700","ratio_1787_to1785","ratio_1822_to1795","ratio_1818_1824_to1795", "ratio_1820_1824_to1770s","ratio_1834_to1795","ratio_1824_to1822")
f <- read(base,"functional_recovery_summary.csv") |>
 filter(scenario=="S0_prior_calibration",functional%in%functionals) |>
 arrange(match(functional,functionals),match(fit_variant,c("primary","true_parameter_plugin")))
stopifnot(nrow(f)==14L,all(f$n==200L),all(f$missing_or_failed==0L))
write_csv(f,file.path(v2_out,"paper_table_simulation_functionals.csv"))
write_rows(f |> transmute(functional=paste0("\\functionallabel{",functional,"}"),
 fit=fit_label(fit_variant),bias=fmt(mean_log_error),bias_mcse=fmt(log_error_mcse),
 coverage=fmt(coverage),coverage_mcse=fmt(coverage_mcse)),"simulation_functional_rows.tex")
p <- read(addon,"paired_stress_functional_summary.csv") |>
 filter(functional%in%c("ratio_1820_1824_to1770s","ratio_1822_to1795","ratio_1824_to1822")) |>
 arrange(functional,scenario)
stopifnot(nrow(p)==9L,all(p$n==200L),all(is.finite(p$coverage_difference_mcse)))
write_csv(p,file.path(v2_out,"paper_table_simulation_paired_stresses.csv"))
write_rows(p |> transmute(functional=paste0("\\functionallabel{",functional,"}"),
 scenario=paste0("\\simlabel{",scenario,"}"),bias=fmt(mean_paired_log_error),bias_mcse=fmt(log_error_mcse),
 clean_rmse=fmt(rmse_clean),stress_rmse=fmt(rmse_stress),delta_coverage=fmt(coverage_difference),
 coverage_mcse=fmt(coverage_difference_mcse)),"simulation_paired_stress_rows.tex")
clean <- read(addon,"replicate_ratios.csv") |>
 group_by(functional) |> summarise(n=n(),mean_log_error=mean(log_error),log_error_mcse=sd(log_error)/sqrt(n()),
 log_rmse=sqrt(mean(log_error^2)),coverage=mean(covered),coverage_mcse=sqrt(coverage*(1-coverage)/n()),.groups="drop")
write_csv(clean,file.path(v2_out,"paper_table_simulation_clean_reference.csv"))
comparison <- f |> select(functional,fit_variant,coverage,coverage_mcse) |>
 pivot_wider(names_from=fit_variant,values_from=c(coverage,coverage_mcse)) |>
 left_join(clean,by="functional") |> arrange(match(functional,functionals))
stopifnot(nrow(comparison)==7L,all(comparison$n==200L))
write_csv(comparison,file.path(v2_out,"paper_table_simulation_truth_comparison.csv"))
write_rows(comparison |> transmute(functional=paste0("\\functionallabel{",functional,"}"),
 prior_cov=fmt(coverage_primary),prior_se=fmt(coverage_mcse_primary),
 plugin_cov=fmt(coverage_true_parameter_plugin),plugin_se=fmt(coverage_mcse_true_parameter_plugin),
 fixed_bias=fmt(mean_log_error),fixed_bias_se=fmt(log_error_mcse),fixed_cov=fmt(coverage),fixed_se=fmt(coverage_mcse)),
 "simulation_truth_comparison_rows.tex")
pk <- read(base,"peak_recovery_summary.csv")
cp <- read(addon,"replicate_peak.csv") |> summarise(scenario="S4_fixed_reference",fit_variant="primary",n=n(),
 covered=sum(covered),mean_error_years=mean(error_years),error_mcse=sd(error_years)/sqrt(n()),
 mean_absolute_error=mean(abs(error_years)),mean_interval_years=mean(interval_years),
 coverage=covered/n,coverage_mcse=sqrt(coverage*(1-coverage)/n))
pk <- bind_rows(pk,cp) |> arrange(scenario,fit_variant)
stopifnot(nrow(pk)==6L,all(pk$n==200L))
write_csv(pk,file.path(v2_out,"paper_table_simulation_peak.csv"))
write_rows(pk |> transmute(scenario=paste0("\\simlabel{",scenario,"}"),fit=fit_label(fit_variant),
 coverage=fmt(coverage),mcse=fmt(coverage_mcse),mean_error=fmt(mean_error_years,2),
 mean_width=fmt(mean_interval_years,2)),"simulation_peak_rows.tex")
cat("Final simulation numeric tables generated; author supplies definitions and scope.\n")

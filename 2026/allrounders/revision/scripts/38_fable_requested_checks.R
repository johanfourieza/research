# Additional descriptive adjustment sequence requested by the independent final review.
source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
require_data_gate('domestic-to-Test observed transitions')
suppressPackageStartupMessages(library(survival))
d<-readRDS(file.path(revision_dir,'data/analysis/promotion.rds'))$risk
forms<-list('Role and entry era'=Surv(tstart,tstop,event)~role+factor(entry_decade)+strata(entry_comp),'Add prior appearances'=Surv(tstart,tstop,event)~role+factor(entry_decade)+strata(entry_comp)+splines::ns(log1p(past_matches),knots=log1p(c(5,30)),Boundary.knots=c(0,log1p(200))))
res<-list()
teams<-sort(unique(d$entry_cluster));G<-length(teams);term<-'roleAll-round role'
for(nm in names(forms)) {
 form<-forms[[nm]];fit<-coxph(form,data=d,ties='efron');beta<-unname(coef(fit)[term])
 loo<-vapply(teams,function(g)unname(coef(coxph(form,data=d[entry_cluster!=g],ties='efron'))[term]),numeric(1))
 stopifnot(all(is.finite(loo)))
 se<-sqrt((G-1)/G*sum((loo-mean(loo))^2))
 res[[nm]]<-data.table(specification=nm,players=uniqueN(d$player),events=sum(d$event),HR=exp(beta),lower=exp(beta-qt(.975,G-1)*se),upper=exp(beta+qt(.975,G-1)*se),clusters=G)
 cat(nm,'comparison completed\n')
}
pc<-read_csv(file.path(results_dir,'promotion_cox.csv'))[scope=='pooled']
res[['Full history adjustment']]<-pc[,.(specification='Add prior output rates',players,events,HR,lower,upper,clusters)]
rr<-rbindlist(res);put(rr,'promotion_adjustment_sequence')
fmt<-function(x,d=2)formatC(x,format='f',digits=d,big.mark=',')
tex_table(rr[,.(Specification=specification,Entrants=players,Events=events,`Hazard ratio`=fmt(HR),`95% interval`=paste0('[',fmt(lower),', ',fmt(upper),']'))],'adjustment_sequence','Conditioning changes the transition-role association','tab:adjustment','Same 5,221 entrant histories and 463 first-Test events. Every model includes current role, entry-decade indicators and entry-competition baseline strata. Successive rows add the fixed splines in completed appearances and then prior runs and wickets per appearance. Intervals use the same 24-entry-team delete-one jackknife and t(23) reference. The sequence is a descriptive specification check requested at final review; it is not an additional primary hypothesis family or a causal mediation decomposition.')
base<-read_csv(file.path(results_dir,'dual_counts.csv'))[floor_quantile==.5,.(comp,Baseline=independent)]
role<-read_csv(file.path(results_dir,'role_prior_sensitivity.csv'))[,.(comp,`Role priors`=independent,eligible,covered)]
native<-read_csv(file.path(results_dir,'native_wicket_sensitivity.csv'))[floor_quantile==.5,.(comp,`Native wickets`=independent)]
z<-Reduce(function(x,y)merge(x,y,by='comp'),list(base,role,native));z<-z[match(names(configs),comp)]
tex_table(z[,.(Format=unname(comp_labels[comp]),Baseline,`Role priors`,`Prior coverage`=paste0(covered,'/',eligible),`Native wickets`)],'material_sensitivity','Benchmark counts under alternative priors and bowling currencies','tab:material','Median specialist floors, baseline exposure rules and joint probability threshold 0.5 under independence. Role-specific priors are fitted separately where at least 30 prior players are available; prior coverage reports evaluable players with estimable role priors. Three ODI and five T20 evaluable players lack such priors and are not classified in that column. The native-wicket column rebuilds the bowling floor and prior in era-normalised wickets; long-format standardisation already cancels the wicket price, so those counts coincide exactly.')
z<-read_csv(file.path(results_dir,'promotion_era_risks.csv'))
z[,Role:=fcase(role=='All-round role','Dual',role=='Batting role','Batting',role=='Bowling role','Bowling',default='Other')]
z<-z[order(entry_comp,era,Role)]
tex_table(z[,.(Competition=unname(comp_labels[entry_comp]),`Entry era`=era,Role,Players=n,Events=events,`Risk (%)`=fmt(100*risk,1))],'entry_era','Landmark transitions by entry era and role','tab:era','Exploratory complete-horizon subset of the two-year landmark population. Risk is the observed fraction making a first Test appearance between years two and five, with death preventing subsequent transition. Recent entrants without a complete administrative five-year horizon are excluded from this display only. The primary Cox and Aalen--Johansen analyses retain them with censoring. Small cells, especially three recent Shield dual-role players, cannot support stable subgroup conclusions.')
writeLines(paste0('\\newcommand{\\RoleOnlyHR}{',fmt(rr[specification=='Role and entry era',HR]),'}'),file.path(revision_dir,'paper/additional_macros.tex'))
cat('Fable-requested disclosure exhibits completed\n')

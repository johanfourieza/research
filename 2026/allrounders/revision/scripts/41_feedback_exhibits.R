source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
# Appendix tables for the robustness checks in 39_feedback_checks.R.
read_result<-function(n)read_csv(file.path(results_dir,paste0(n,'.csv')))
fmt<-function(x,d=0)formatC(x,format='f',digits=d,big.mark=',')
fci<-function(lo,hi,d=2)paste0('[',fmt(lo,d),', ',fmt(hi,d),']')
label<-function(x)unname(comp_labels[x])

# A1: wides and no-balls in limited-overs bowling figures.
ex<-read_result('feedback_extras_convention');oc<-read_result('feedback_odi_extras_harmonised')
tex_table(ex[,.(Format=label(comp),Decade=paste0(decade,'s'),Innings=fmt(innings),`Charged to bowler`=fmt(only_charging),`Not charged`=fmt(only_not_charging),Neither=fmt(neither))],
 'extras','Wides and no-balls in bowlers\' conceded runs','tab:extras',
 paste0('Each limited-overs innings is classified by whether the bowlers\' conceded runs equal the team total less byes, leg-byes and penalties (wides and no-balls charged to bowlers) or that total less wides and no-balls as well (not charged). Innings consistent with both, because they contain no wides or no-balls, are omitted from the last three columns. Reallocating the uncharged wides and no-balls of ',fmt(oc$innings_reallocated[2]),' ODI innings (',fmt(oc$runs_reallocated[2]),' runs) to their bowlers in proportion to balls bowled, and rebuilding all ODI reference values, leaves the ODI joint-benchmark count at ',oc$independent[2],' under independence (',oc$all_couplings[2],' under every coupling).'),
 align='llrrrr')

# A4: classification sensitivities.
cs<-read_result('feedback_classification_sensitivity')
cs[,cell:=paste0(independent,' (',fmt(dual_role),')')]
w<-dcast(cs,variant~comp,value.var='cell');ord<-c('Baseline','Participation intensity','Ball intensity','Posterior-mean floors','Player-specific variance, nu=5','Player-specific variance, nu=20')
w<-w[match(ord,variant)];w[,variant:=sub('nu=','$\\\\nu$=',variant)]
setnames(w,c('variant',names(configs)),c('Specification',label(names(configs))))
tex_table(w[,c('Specification',label(names(configs))),with=FALSE],'classification_sensitivity','Joint-benchmark counts under alternative workload rules, floors and variances','tab:classification_sensitivity',
 'Entries are the number of evaluable players whose joint probability of exceeding both median specialist floors is at least one half under posterior independence; dual-role counts are in parentheses. Participation intensity also requires an innings at seven or higher in at least half of all appearances for a batting role, and bowling in at least half of all appearances for a bowling role. Ball intensity requires the ball minimum to be reached at an average of at least one-thirtieth of the minimum per appearance (66.7 balls in the long formats). Both rebuild the specialist pools and floors. Posterior-mean floors take the median of the specialists\' shrunken rather than observed career means. Player-specific variance replaces the common residual variance with each player\'s own variance shrunk towards it with $\\nu$ prior degrees of freedom.',
 align='lrrrrr')
pe<-read_result('feedback_probability_examples')[comp=='tests']
pe[,name:=fcase(grepl('^Shakib',player),'Shakib Al Hasan',grepl('^Botham',player),'Ian Botham',grepl('^Cairns',player),'Chris Cairns',default=NA_character_)]
pe<-pe[!is.na(name)];pe[,cell:=paste0(fmt(joint_independent,2),' [',fmt(joint_lower,2),', ',fmt(joint_upper,2),']')]
pw<-dcast(pe,variant~name,value.var='cell');pw<-pw[match(ord,variant)];pw[,variant:=sub('nu=','$\\\\nu$=',variant)]
setnames(pw,'variant','Specification')
tex_table(pw[,.(Specification,`Shakib Al Hasan`,`Chris Cairns`,`Ian Botham`)],'test_candidates','Joint probabilities of the leading Test candidates','tab:test_candidates',
 'Joint probability of exceeding both median specialist floors under posterior independence, with the Fr\\\'echet bounds in brackets. Specifications as in Table~\\ref{tab:classification_sensitivity}.',align='lrrr')

# A5: forecasts with and without exposure terms, two chronological splits.
fx<-read_result('feedback_forecast_exposure')
fs<-fx[,.(role=rmse[model=='workload' & !exposure],best=min(rmse[!model%in%c('workload','exposure_only') & !exposure]),xonly=rmse[model=='exposure_only' & exposure],rolex=rmse[model=='workload' & exposure],bestx=min(rmse[!model%in%c('workload','exposure_only') & exposure]),n_train=n_train[1],n_test=n_test[1]),by=.(split,split_date,comp,outcome)]
fs[,ord:=match(comp,names(configs))];setorder(fs,-split,ord,outcome)
tex_table(fs[,.(Split=format(as.Date(split_date),'%Y'),Competition=label(comp),Outcome=ifelse(outcome=='future_bat_annual','Runs/year','Wickets/year'),Role=fmt(role,1),`Best other`=fmt(best,1),`X only`=fmt(xonly,1),`Role + X`=fmt(rolex,1),`Best other + X`=fmt(bestx,1))],
 'forecast_exposure','Forecast error with and without exposure predictors','tab:forecast_exposure',
 'Root mean squared errors on the later cohort. Best other is the lowest error among the average-gap, share, RCA and separate-input models. X only is a model with the exposure terms alone. Other columns marked X add them to the stated predictors. The exposure terms are log days from debut to the 30th appearance, appearances in the preceding 365 days, and early runs and wickets per year (denominator at least one year). The baseline split matches Table~3 of the main text; the alternative split moves the training/test boundary to 2000. T20Is began too recently for an earlier split.',
 align='lllrrrrr')

# A6: per-ball wicket rates with team-cluster bootstrap intervals.
wr<-read_result('feedback_wicket_ratio_intervals')
wr<-dcast(wr,comp+dual_n+clusters~measure,value.var=c('estimate','lower','upper','without_heaviest','valid_replicates'));wr<-wr[match(names(configs),comp)]
tex_table(wr[,.(Format=label(comp),`Dual n`=dual_n,Teams=clusters,Participation=paste(fmt(estimate_participation,2),fci(lower_participation,upper_participation)),`Balls/bowled match`=paste(fmt(estimate_balls_conditional,2),fci(lower_balls_conditional,upper_balls_conditional)),`Wickets/ball`=paste(fmt(estimate_wickets_per_ball,2),fci(lower_wickets_per_ball,upper_wickets_per_ball)),`Excl. heaviest`=fmt(without_heaviest_wickets_per_ball,2))],
 'wicket_ratios','Dual-role relative to bowling-specialist bowling margins','tab:wicket_ratios',
 paste0('Ratios of pooled dual-role to pooled bowling-role margins in the held-out cohorts of Table~\\ref{tab:margins}. Brackets give 2.5th and 97.5th percentiles of 2,000 bootstrap resamples of entry teams. The last column recomputes the wicket-per-ball ratio without the dual-role player with the most subsequent balls. The T20I dual-role group has three players, and ',fmt(wr[comp=='t20is',valid_replicates_wickets_per_ball]),' of the 2,000 T20I resamples contain both groups.'),
 align='lrrrrrr')

# A7: covariate overlap in the transition risk sets.
ov<-read_result('feedback_transition_overlap');sr<-read_result('feedback_transition_support_refit')
ov<-ov[match(c('county','sheffield'),entry_comp)]
tt<-data.table(Measure=c('Dual-role person-years','Dual-role Test debuts','Share of dual-role time in batting support','Median completed matches, dual / batting','Median runs per match, dual / batting','Median wickets per match, dual / batting'),
 County=c(fmt(ov$dual_person_years[1]),ov$dual_events[1],fmt(ov$dual_share_time_in_batting_support[1],2),paste(fmt(ov$dual_median_matches[1]),fmt(ov$batting_median_matches[1]),sep=' / '),paste(fmt(ov$dual_median_bat_rate[1],1),fmt(ov$batting_median_bat_rate[1],1),sep=' / '),paste(fmt(ov$dual_median_wicket_rate[1],2),fmt(ov$batting_median_wicket_rate[1],2),sep=' / ')),
 Shield=c(fmt(ov$dual_person_years[2]),ov$dual_events[2],fmt(ov$dual_share_time_in_batting_support[2],2),paste(fmt(ov$dual_median_matches[2]),fmt(ov$batting_median_matches[2]),sep=' / '),paste(fmt(ov$dual_median_bat_rate[2],1),fmt(ov$batting_median_bat_rate[2],1),sep=' / '),paste(fmt(ov$dual_median_wicket_rate[2],2),fmt(ov$batting_median_wicket_rate[2],2),sep=' / ')))
tex_table(tt,
 'transition_overlap','Common support of dual-role and batting-role histories','tab:transition_overlap',
 paste0('Risk-set intervals of the transition model. Batting support is the box spanned by the 5th and 95th percentiles of completed matches, runs per match and wickets per match over batting-role intervals; the third row gives the fraction of dual-role person-time inside it. Medians are over risk-set intervals. Refitting the fully adjusted pooled model after removing dual- and batting-role intervals outside the support gives a role hazard ratio of ',fmt(sr$HR[2],2),' (model-based 95\\% interval ',fmt(sr$lower_model[2],2),'--',fmt(sr$upper_model[2],2),'; ',sr$events[2],' events, of which ',sr$dual_events[2],' dual-role against ',sr$dual_events[1],' in the full sample), compared with ',fmt(sr$HR[1],2),' in the full sample. Unlike the jackknife intervals of the main text, this interval is model-based. The box is built from marginal percentiles and pools intervals over time, so it is a coarse measure of overlap rather than a test of joint support within risk sets.'),
 align='lrr')
cat('Feedback appendix tables complete\n')

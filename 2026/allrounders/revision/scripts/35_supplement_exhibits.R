source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
read_result<-function(n)read_csv(file.path(results_dir,paste0(n,'.csv')))
fmt<-function(x,d=1)formatC(x,format='f',digits=d,big.mark=',')
z<-read_result('alternative_definition_outputs')
z[,Definition:=fcase(definition=='actual_output_role','Workload',definition=='traditional_label','Average gap',default='Sadekar')]
z<-dcast(z,comp+Definition+labelled~discipline,value.var=c('difference','reference'))
tex_table(z[,.(Format=unname(comp_labels[comp]),Definition,`Dual n`=labelled,`Bat ref.`=reference_bat,`Run gap`=fmt(difference_bat,2),`Bowl ref.`=reference_bowl,`Wicket gap`=fmt(difference_bowl,2))],'alternative_definitions','Subsequent native output under alternative early labels','tab:alternatives','Unadjusted equal-player mean differences: labelled dual-role group minus the relevant workload-specialist reference, removing overlapping labels from that reference. First 30 appearances determine labels; later matches completed within five calendar years determine outcomes. The average-gap rule is batting average above bowling average when both exist. The Sadekar participation rule is implemented in ODIs only. These descriptive contrasts use different selected groups and are not causal costs or significance tests.')
z<-read_result('output_distributions')[comp%in%c('odis','t20is') & variable=='bowling_volume']
tex_table(z[,.(Format=unname(comp_labels[comp]),Players=n,Mean=fmt(mean),SD=fmt(sd),Minimum=fmt(min),`25th`=fmt(q25),Median=fmt(median),`75th`=fmt(q75),Maximum=fmt(max))],'signed_distribution','Signed career containment before clipping','tab:signed','Career sums of match-specific containment in runs at the baseline 75th-percentile economy reference. These are realised totals with unequal exposure, not ability estimates. Negative values are retained in the modelling response; only the separate composition basket clips a career total at zero.')
zz<-list()
for(co in c('odis','t20is')) {
 a<-readRDS(file.path(revision_dir,'data/analysis',paste0('analysis_',co,'.rds')))
 c<-a$career;p<-a$pm
 rz<-p[,.(zero_ball_concession=any(zero_ball_concessions,na.rm=TRUE),all_balls_zero=sum(scorecard_balls)==0,has_bowling=any(bowling_row_present)),by=player]
 zz[[co]]<-data.table(Format=unname(comp_labels[co]),Players=nrow(c),`No bowling`=sum(!rz$has_bowling),Nonpositive=sum(c$bowling_volume<=0 & c$bowl_matches>0),`0-ball debit`=sum(rz$zero_ball_concession),`0-ball only`=sum(rz$has_bowling & rz$all_balls_zero))
}
tex_table(rbindlist(zz),'zero_mass','Sources of the zero bowling-composition basket','tab:zeros','No bowling and bowled/nonpositive are disjoint sources of zero clipped bowling volume. Zero-ball-concession columns are separate diagnostics, may overlap other columns and need not imply a zero career basket. Unknown or undefined zero-total shares remain missing.')
cat('Supplementary definition and containment exhibits generated\n')

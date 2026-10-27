source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'revision_common.R'))
setDTthreads(2)
for(d in c('results','paper','paper/tables','paper/figures','supplement'))dir.create(file.path(revision_dir,d),recursive=TRUE,showWarnings=FALSE)
results_dir<-file.path(revision_dir,'results')
put<-function(x,name)fwrite(x,file.path(results_dir,paste0(name,'.csv')))
comp_labels<-c(tests='Tests',odis='ODIs',t20is='T20Is',county='County first-class',sheffield='Sheffield Shield')
ball_min<-c(tests=2000,odis=1000,t20is=500,county=2000,sheffield=2000)
ball_step<-c(tests=500,odis=250,t20is=125,county=500,sheffield=500)
cutoff<-as.IDate('2025-12-31')
roles<-function(c,imin=20,bmin=2000,position='actual_modal_position') {
 a<-!is.na(c[[position]]) & c[[position]]<=7 & c$innings_batted>=imin
 b<-c$scorecard_balls>=bmin
 factor(ifelse(a & b,'All-round role',ifelse(a,'Batting role',ifelse(b,'Bowling role','Insufficient/other'))),levels=c('Batting role','All-round role','Bowling role','Insufficient/other'))
}
career_summary<-function(p,b) {
 setorder(p,player,start_date,match_id)
 c<-p[,.(matches=.N,bat_runs=sum(bat_runs),innings_batted=sum(innings_batted),dismissals=sum(dismissals),wickets=sum(wickets),scorecard_balls=sum(scorecard_balls),runs_conceded=sum(runs_conceded),
   bowl_matches=sum(bowling_row_present),keeper_matches=sum(wicketkeeper_nominated,na.rm=TRUE),keeper_unknown=sum(is.na(wicketkeeper_nominated)),
   first_date=min(start_date),last_date=max(start_date),first_team=team[1],nominal_modal_position=mode_position(nominal_position)),by=player]
 bp<-b[,.(actual_modal_position=mode_position(actual_position),top7_innings=sum(actual_position<=7)),by=player]
 c<-merge(c,bp,by='player',all.x=TRUE);c[is.na(top7_innings),top7_innings:=0L]
 c[,`:=`(bat_per_match=bat_runs/matches,wickets_per_match=wickets/matches,bat_average=fifelse(dismissals>0,bat_runs/dismissals,NA_real_),bowling_average=fifelse(wickets>0,runs_conceded/wickets,NA_real_),entry_decade=10L*(as.integer(format(first_date,'%Y'))%/%10L))]
 c[,traditional_positive:=!is.na(bat_average)&!is.na(bowling_average)&bat_average>bowling_average]
 c
}
# Exact sufficient-statistic REML for y_pm = mu + u_p + e_pm. Equivalent to
# a Gaussian random-intercept fit, without storing a sparse design per bootstrap.
fit_reml<-function(s,weights=rep(1,nrow(s))) {
 ok<-weights>0 & s$n>0;s<-s[ok];weights<-weights[ok]
 n<-s$n;m<-s$m;ss<-pmax(0,s$ss);N<-sum(weights*n);P<-sum(weights)
 init_s<-max(sum(weights*ss)/max(1,N-P),1e-5);init_t<-max(weighted.mean((m-weighted.mean(m,weights))^2,weights)-init_s*weighted.mean(1/n,weights),1e-4)
 objective<-function(theta,ret=FALSE) {
  sigma2<-exp(theta[1]);tau2<-exp(theta[2]);v<-sigma2+n*tau2;w<-weights*n/v;mu<-sum(w*m)/sum(w)
  val<-.5*(sum(weights*((n-1)*log(sigma2)+log(v)+ss/sigma2+n*(m-mu)^2/v))+log(sum(w)))
  if(ret)return(list(mu=mu,sigma2=sigma2,tau2=tau2,objective=val))
  val
 }
 gradient<-function(theta) {
  sigma2<-exp(theta[1]);tau2<-exp(theta[2]);v<-sigma2+n*tau2;w<-weights*n/v;A<-sum(w);mu<-sum(w*m)/A;r2<-(m-mu)^2
  gs<-.5*(sum(weights*((n-1)/sigma2+1/v-ss/sigma2^2-n*r2/v^2))-sum(weights*n/v^2)/A)
  gt<-.5*(sum(weights*(n/v-n^2*r2/v^2))-sum(weights*n^2/v^2)/A)
  c(gs*sigma2,gt*tau2)
 }
 op<-optim(log(c(init_s,init_t)),objective,gradient,method='L-BFGS-B',lower=log(c(1e-8,1e-8)),upper=log(c(1e8,1e8)),control=list(factr=1e3,pgtol=1e-6))
 if(op$convergence!=0) {
  alt<-nlminb(op$par,objective,gradient,lower=log(c(1e-8,1e-8)),upper=log(c(1e8,1e8)),control=list(rel.tol=1e-10,iter.max=1000,eval.max=2000))
  if(alt$convergence==0)op<-list(par=alt$par,convergence=0L)
 }
 if(op$convergence!=0)stop('REML optimiser failed: no posterior is emitted')
 z<-objective(op$par,TRUE);z$convergence<-op$convergence;z
}
posterior<-function(s,fit) {
 v<-1/(1/fit$tau2+s$n/fit$sigma2)
 data.table(mean=v*(fit$mu/fit$tau2+s$n*s$m/fit$sigma2),sd=sqrt(v))
}
suff<-function(p,column) p[,.(n=.N,m=mean(get(column)),ss=sum((get(column)-mean(get(column)))^2)),by=player]
# Plackett's identity integrated by 48-point Gauss-Legendre quadrature.
nn<-48L;j<-seq_len(nn-1);jj<-matrix(0,nn,nn);jj[cbind(j,j+1)]<-jj[cbind(j+1,j)]<-j/sqrt(4*j^2-1)
ev<-eigen(jj,symmetric=TRUE);quad_x<-ev$values;quad_w<-2*ev$vectors[1,]^2
copula_joint<-function(p,q,rho) {
 if(rho==0)return(p*q)
 x<-qnorm(pmin(1-1e-12,pmax(1e-12,p)));y<-qnorm(pmin(1-1e-12,pmax(1e-12,q)))
 ans<-p*q
 for(k in seq_along(quad_x)) {
  r<-rho*(quad_x[k]+1)/2
  ans<-ans+rho/2*quad_w[k]*exp(-(x*x-2*r*x*y+y*y)/(2*(1-r*r)))/(2*pi*sqrt(1-r*r))
 }
 pmin(pmin(p,q),pmax(pmax(0,p+q-1),ans))
}
safe_q<-function(x,q=.5)if(length(x)>=30)as.numeric(quantile(x,q,na.rm=TRUE,names=FALSE))else NA_real_
tex_escape<-function(s){s<-gsub('&','\\&',s,fixed=TRUE);s<-gsub('%','\\%',s,fixed=TRUE);s<-gsub('_','\\_',s,fixed=TRUE);s}
tex_table<-function(x,file,caption,label,notes,align=NULL) {
 if(is.null(align))align<-paste0('l',paste(rep('r',ncol(x)-1),collapse=''))
 rows<-apply(as.data.frame(x),1,function(z)paste(tex_escape(as.character(z)),collapse=' & '))
 lines<-c('\\begin{table}[htbp]','\\centering',paste0('\\caption{',caption,'}\\label{',label,'}'),'\\begin{threeparttable}','\\fontsize{10}{12}\\selectfont\\setlength{\\tabcolsep}{3.5pt}',paste0('\\begin{tabular}{',align,'}'),'\\toprule',paste0(paste(tex_escape(names(x)),collapse=' & '),' \\\\'),'\\midrule',paste0(rows,' \\\\'),'\\bottomrule','\\end{tabular}',paste0('\\begin{tablenotes}\\fontsize{10}{12}\\selectfont\\item ',notes,'\\end{tablenotes}'),'\\end{threeparttable}','\\end{table}')
 writeLines(lines,file.path(revision_dir,'paper/tables',paste0(file,'.tex')))
}

# Figures and manuscript tables for the revised Historical Methods paper.
# Run from HM_revision. All estimates come from 02_analysis.R; this script
# never refits PCA, transforms a different sample, or reuses an old figure.
suppressPackageStartupMessages({library(ggplot2); library(ggrepel); library(patchwork)})
stopifnot(requireNamespace("gifski", quietly=TRUE))
if (!file.exists("data/analysis/revision_results.rds")) stop("Run the revised analysis first")
results <- readRDS("data/analysis/revision_results.rds")
figdir <- "output/figures"; tabdir <- "output/tables"
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
dir.create(tabdir, recursive=TRUE, showWarnings=FALSE)
palette <- c("#5C2346", "#3D8EB9", "#6B8E5E", "#D4A03E", "#A34466", "#45808B", "#8B6B3D", "#667788", "#975A38", "#426D47", "#82719D", "#2D677B")
labels <- c(slaves_total="Enslaved persons", slaves="Enslaved persons", slave_men="Enslaved men", slave_women="Enslaved women", slave_children="Enslaved children", servants="Servants", khoe_total="Khoekhoe labour", cattle_total="Cattle", cattle="Cattle", horses_total="Horses", horses="Horses", sheep_total="Sheep", sheep="Sheep", vines="Vines", wine="Wine", brandy="Brandy", wheat_vol="Wheat", goats="Goats", wheat_sown="Wheat sown", wheat_reaped="Wheat reaped", barley_sown="Barley sown", barley_reaped="Barley reaped", grain_sown="Grain sown", grain_reaped="Grain reaped", grain_other="Other grain", wagons="Wagons", total_tax="Tax", wheat="Wheat", barley="Barley")
nice <- function(x) { x<-as.character(x); z<-unname(labels[x]); z[is.na(z)]<-gsub("_", " ", x[is.na(z)]); z }
labels<-c(labels,total_slaves="Enslaved persons",total_khoe="Khoekhoe labour",grain_other_reaped="Other grain")
spec_labels<-c(levels="Levels",log1p="log(1+x)",asinh="asinh(x)",log_c001="log(x+0.01)",log_c01="log(x+0.1)",log_c1="log(x+1)",covariance_log1p="log(1+x), covariance PCA",units_naive_log1p="log(1+x), converted units",units_equivalent_log="Log, units and offset converted",units_naive_asinh="asinh(x), converted units",equal_household="Equal household weights",equal_year="Equal year weights",old_three_positive="At least three positive quantities",omit_top_one_percent_distance="Exclude largest 1% of distances",omit_flagged_outliers="Exclude flagged source entries")
nice_spec<-function(x) { z<-unname(spec_labels[x]);z[is.na(z)]<-gsub("_"," ",x[is.na(z)]);z }
# log(x + 1) duplicates the baseline log(1 + x). Keep both numerical records
# for verification, but show this transformation only once in paper exhibits.
paper_specs <- function(d) {
  if("specification" %in% names(d)) d <- d[d$specification != "log_c1",,drop=FALSE]
  d
}
paper_theme <- function() theme_minimal(base_size=10, base_family="sans") + theme(
  plot.title=element_blank(), plot.subtitle=element_blank(), plot.caption=element_blank(),
  strip.text=element_blank(), strip.background=element_blank(),
  axis.text=element_text(size=9, colour="#333333"), axis.title=element_text(size=10),
  legend.text=element_text(size=9), legend.title=element_text(size=9),
  panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="#EEEEEE", linewidth=.25),
  plot.margin=margin(9,12,9,9), legend.position="bottom", legend.box="vertical")
theme_set(paper_theme())
inventory <- list(); captions <- list(); calibrations <- list()
save_figure <- function(p, stem, width=6.5, height=5.5, description="") {
  stopifnot(is.character(stem), length(stem)==1L)
  for(ext in c("pdf","png")) {
    dest<-file.path(figdir,paste0(stem,".",ext))
    ggsave(dest,p,width=width,height=height,dpi=300,bg="white",limitsize=FALSE)
    if(!file.exists(dest)||file.info(dest)$size<100) stop("Empty figure: ",dest)
    inventory[[length(inventory)+1L]] <<- data.frame(path=dest,type=ext,width_in=width,height_in=height,dpi=if(ext=="png")300 else NA)
  }
  captions[[stem]] <<- description
  message("Rendered ",stem)
}
score_df <- function(model) { data.frame(PC1=model$scores[,1],PC2=model$scores[,2]) }
share <- function(model) { if(!is.null(model$variance)) model$variance else model$eigenvalues/sum(model$eigenvalues) }
limits <- function(model) {
  lim<-range(model$scores[,1:2],finite=TRUE); span<-diff(lim)
  c(lim[1]-.08*span,lim[2]+.13*span)
}
# In a row-principal biplot, score s predicts z_j = s'v_j. A tick at
# source value x therefore sits at v_j * ((log(1+x)-center_j)/scale_j) /
# (v_j'v_j). Orthogonal projection onto this calibrated line recovers the
# rank-two prediction. Axis lengths have no arbitrary enlargement constant.
axis_geometry <- function(model, lim, short=FALSE) {
  v<-model$rotation[,1:2,drop=FALSE]; vars<-rownames(v)
  if(is.null(vars)) vars<-model$variables
  segments<-ticks<-namesdf<-list()
  for(j in seq_along(vars)) {
    norm2<-sum(v[j,]^2); if(norm2<1e-10) next
    raw<-model$data[[vars[j]]]; raw<-raw[is.finite(raw)&raw>=0]
    if(!length(raw)) stop("No source values for calibrated axis: ",vars[j])
    positive<-raw[raw>0]
    candidates<-sort(unique(c(0, if(length(positive)) signif(as.numeric(quantile(positive,.9,names=FALSE)),2) else 1)))
    cen<-model$center[j]; scl<-model$scale[j]
    q<-(log1p(candidates)-cen)/scl
    tx<-q*v[j,1]/norm2; ty<-q*v[j,2]/norm2
    valid<-tx>lim[1]+.04*diff(lim)&tx<lim[2]-.04*diff(lim)&ty>lim[1]+.04*diff(lim)&ty<lim[2]-.04*diff(lim)
    if(!any(valid)) next
    tt<-data.frame(variable=vars[j], raw_value=candidates[valid],standardized_value=q[valid],PC1=tx[valid],PC2=ty[valid],vx=v[j,1],vy=v[j,2])
    stopifnot(max(abs(tt$PC1*v[j,1]+tt$PC2*v[j,2]-tt$standardized_value))<1e-10)
    # Variable axes are calibrated lines, not loading arrows. Extend the
    # positive ray to a common circular display boundary so that labels can
    # sit outside the dense cloud. Tick coordinates retain exact calibration.
    low<-min(c(0,tt$standardized_value))
    high<-max(max(c(0,tt$standardized_value))*1.1,.70*min(abs(lim))*sqrt(norm2))
    segments[[j]]<-data.frame(variable=vars[j],x=low*v[j,1]/norm2,y=low*v[j,2]/norm2,xend=high*v[j,1]/norm2,yend=high*v[j,2]/norm2)
    namesdf[[j]]<-data.frame(variable=vars[j],PC1=high*v[j,1]/norm2,PC2=high*v[j,2]/norm2,label=nice(vars[j]))
    # A compact panel keeps the high-value tick; the full-size instructional
    # figure retains every source-unit tick that lies inside its frame.
    if(short) tt<-tt[which.max(tt$raw_value),,drop=FALSE]
    eps<-.007*diff(lim); vv<-sqrt(norm2)
    tt$x<-tt$PC1-eps*v[j,2]/vv; tt$xend<-tt$PC1+eps*v[j,2]/vv
    tt$y<-tt$PC2+eps*v[j,1]/vv; tt$yend<-tt$PC2-eps*v[j,1]/vv
    tt$label<-format(tt$raw_value,trim=TRUE,scientific=FALSE,big.mark=",")
    ticks[[j]]<-tt
  }
  list(segments=do.call(rbind,segments),ticks=do.call(rbind,ticks),labels=do.call(rbind,namesdf))
}
biplot_plot <- function(model, rows=seq_len(nrow(model$scores)), lim=limits(model), letter=NULL, short=FALSE, hulls=FALSE, time=NULL) {
  d<-score_df(model)[rows,,drop=FALSE]; ax<-axis_geometry(model,lim,short)
  colours<-setNames(rep(palette,length.out=length(model$variables)),model$variables)
  p<-ggplot(d,aes(PC1,PC2)) + geom_hline(yintercept=0,colour="#BBBBBB",linewidth=.25) +
    geom_vline(xintercept=0,colour="#BBBBBB",linewidth=.25) +
    geom_point(colour="#515151",alpha=.075,size=.45,shape=16)
  if(hulls && nrow(d)>3) {
    h<-d[chull(d$PC1,d$PC2),,drop=FALSE]
    rad<-sqrt((d$PC1-mean(d$PC1))^2+(d$PC2-mean(d$PC2))^2)
    dc<-d[rad<=quantile(rad,.9,names=FALSE),,drop=FALSE]
    hc<-dc[chull(dc$PC1,dc$PC2),,drop=FALSE]
    p<-p+geom_polygon(data=h,fill=NA,colour="#777777",linewidth=.35,linetype=2)+
      geom_polygon(data=hc,fill=NA,colour="#333333",linewidth=.55)
  }
  ticktext<-ax$ticks[,c("variable","PC1","PC2","label")]
  alltext<-rbind(ticktext,ax$labels[,c("variable","PC1","PC2","label")])
  p<-p+geom_segment(data=ax$segments,aes(x=x,y=y,xend=xend,yend=yend,colour=variable),inherit.aes=FALSE,linewidth=.45)+
    geom_segment(data=ax$ticks,aes(x=x,y=y,xend=xend,yend=yend,colour=variable),inherit.aes=FALSE,linewidth=.5)+
    geom_text_repel(data=alltext,aes(PC1,PC2,label=label,colour=variable),inherit.aes=FALSE,size=3.25,seed=19,box.padding=.25,point.padding=.10,min.segment.length=0,max.overlaps=Inf,segment.size=.25,max.iter=10000,max.time=Inf)+
    scale_colour_manual(values=colours,guide="none")+
    coord_equal(xlim=lim,ylim=lim,clip="off")+
    labs(x=sprintf("PC1 (%.1f%%)",100*share(model)[1]),y=sprintf("PC2 (%.1f%%)",100*share(model)[2]))
  if(!is.null(letter)) p<-p+annotate("text",x=lim[1],y=lim[2],label=letter,hjust=0,vjust=1,size=3.5,fontface="bold")
  if(!is.null(time)) p<-p+annotate("text",x=lim[2],y=lim[1],label=time,hjust=1,vjust=0,size=3.5)
  p
}

# Availability of the constructed quantities uses the cleaner's component
# definitions and missingness rules. The complete underlying raw-field audit
# remains in source_coverage_panel.csv. Missing source years have no tile.
panel_data<-readRDS("data/analysis/opgaaf_clean.rds")
keep<-unique(c(results$pca$D$variables,results$pca$B$variables,results$pca$bridge$variables))
stopifnot(all(keep%in%names(panel_data)))
years<-sort(unique(panel_data$year))
av<-do.call(rbind,lapply(keep,function(v)data.frame(year=years,variable=v,
  available=vapply(years,function(y)any(!is.na(panel_data[[v]][panel_data$year==y])),logical(1)))))
write.csv(av,file.path(tabdir,"display_quantity_availability.csv"),row.names=FALSE)
av$variable<-factor(av$variable,levels=rev(keep))
av$status<-factor(ifelse(av$available,"Quantity available","Quantity unavailable"),levels=c("Quantity available","Quantity unavailable"))
p<-ggplot(av,aes(year,variable,fill=status))+geom_tile(width=.85,height=.85)+
  scale_fill_manual(values=c("#5C2346","#E4E4E4"),name=NULL)+scale_y_discrete(labels=nice)+
  labs(x="Year",y=NULL)+theme(panel.grid=element_blank())
save_figure(p,"fig1_availability",6.5,max(4,.23*length(keep)+1.2),"Availability of the constructed analysis quantities by year. Colour indicates at least one nonmissing constructed value after the declared component and blank rules. Blanks in covered fields are conditionally treated as zero; missing component fields remain unavailable. Coverage is inferred from numeric source entries and is not archival verification of form design. Years absent from the source have no tile.")

save_figure(biplot_plot(results$pca$D),"fig2_D_biplot",6.5,6.5,"Regime D household-years. Standardised log(1+x), row-principal scores and calibrated variable axes. Numeric axis ticks are quantities in source units; perpendicular projection gives rank-two approximations. The origin is the vector of transformed means. All eligible observations are drawn. Entirely unrecorded asset vectors are excluded; recorded zeros are retained under the declared component and blank rules.")
calibrations$D<-axis_geometry(results$pca$D,limits(results$pca$D))$ticks

# Same scores, axes, limits and scale in every temporal panel and GIF frame.
bridge<-results$pca$bridge; yr<-bridge$data$year; stopifnot(length(yr)==nrow(bridge$scores))
decade<-10*floor(yr/10); decs<-sort(unique(decade)); lim<-limits(bridge)
period<-ifelse(yr<1800,"B","D"); selected<-c("B","D")
panels<-lapply(seq_along(selected),function(i) biplot_plot(bridge,which(period==selected[i]),lim,letters[i],short=TRUE,hulls=TRUE))
panel_descriptions<-vapply(seq_along(selected),function(i)paste0(letters[i],": ",min(yr[period==selected[i]]),"-",max(yr[period==selected[i]]),"; n = ",format(sum(period==selected[i]),big.mark=",",trim=TRUE)),character(1))
save_figure(wrap_plots(panels,ncol=2),"fig3_temporal",6.5,3.8,paste0("Common pooled PCA frame. Panels ",paste(panel_descriptions,collapse="; "),". All observations are shown. Dashed outline: full convex hull. Solid outline: hull of the 90% nearest observations to the panel centroid by Euclidean score distance. These are descriptive coverage regions, not confidence regions. Axes and limits are identical across panels and animation frames. Only eligible observed years enter each period; gaps are listed in animation_frames.csv."))
frame_dir<-file.path(figdir,"temporal_frames")
dir.create(frame_dir,showWarnings=FALSE)
frame_paths<-character(length(decs))
for(i in seq_along(decs)) {
  frame_paths[i]<-file.path(frame_dir,sprintf("frame_%03d.png",i))
  p<-biplot_plot(bridge,which(decade==decs[i]),lim,short=FALSE,hulls=TRUE,time=paste0(decs[i],"-",decs[i]+9))
  ggsave(frame_paths[i],p,width=6.5,height=6.5,dpi=120,bg="white")
}
gifpath<-file.path(figdir,"fig3_temporal.gif")
gifski::gifski(frame_paths,gif_file=gifpath,width=780,height=780,delay=1.2,loop=TRUE,progress=FALSE)
stopifnot(file.exists(gifpath),file.info(gifpath)$size>100)
inventory[[length(inventory)+1L]]<-data.frame(path=gifpath,type="gif",width_in=6.5,height_in=6.5,dpi=120)
write.csv(data.frame(frame=seq_along(decs),start_year=decs,end_year=decs+9,observed_years=vapply(decs,function(d)paste(sort(unique(yr[decade==d])),collapse=";"),character(1)),n=as.integer(table(factor(decade,levels=decs)))),file.path(tabdir,"animation_frames.csv"),row.names=FALSE)
saveRDS(list(model="bridge",decades=decs,static_panels=selected,limits=lim,scores=bridge$scores,variables=bridge$variables,rotation=bridge$rotation,center=bridge$center,scale=bridge$scale),file.path(figdir,"temporal_plot_spec.rds"))

# Compare coverage using the exact equal-size samples already drawn and saved
# by 02. Areas are in squared PC-score units, in the unchanged pooled frame.
# Each central region uses its own cloud's centroid and includes ties at the
# 90th percentile of radial distance. This is one declared sample per decade,
# not a Monte Carlo distribution or a confidence region.
hull_area<-function(x) {
  x<-as.matrix(x);stopifnot(ncol(x)==2L,all(is.finite(x)))
  if(nrow(x)<3L)return(0)
  h<-x[chull(x[,1],x[,2]),,drop=FALSE]; if(nrow(h)<3L)return(0)
  next_row<-c(seq.int(2L,nrow(h)),1L)
  abs(sum(h[,1]*h[next_row,2]-h[next_row,1]*h[,2]))/2
}
central_rows<-function(x) {
  radius<-sqrt(rowSums(sweep(x,2,colMeans(x),"-")^2))
  which(radius<=quantile(radius,.9,names=FALSE))
}
unit_square<-rbind(c(0,0),c(1,0),c(1,1),c(0,1))
stopifnot(abs(hull_area(unit_square)-1)<1e-12,abs(hull_area(2*unit_square)-4)<1e-12,
  hull_area(rbind(c(0,0),c(1,1),c(2,2)))==0)
stopifnot(setequal(names(results$temporal_samples),as.character(decs)))
hull_sensitivity<-do.call(rbind,lapply(names(results$temporal_samples),function(nm) {
  full_rows<-which(decade==as.numeric(nm)); sampled_rows<-results$temporal_samples[[nm]]
  stopifnot(!anyDuplicated(sampled_rows),all(sampled_rows%in%full_rows))
  full<-bridge$scores[full_rows,1:2,drop=FALSE]; sampled<-bridge$scores[sampled_rows,1:2,drop=FALSE]
  a_full<-hull_area(full);a_sample<-hull_area(sampled)
  a_full_central<-hull_area(full[central_rows(full),,drop=FALSE])
  a_sample_central<-hull_area(sampled[central_rows(sampled),,drop=FALSE])
  stopifnot(a_sample<=a_full+1e-8,a_full_central<=a_full+1e-8,a_sample_central<=a_sample+1e-8)
  data.frame(decade=as.numeric(nm),full_n=nrow(full),sample_n=nrow(sampled),
    full_hull_area=a_full,sample_hull_area=a_sample,
    full_central90_area=a_full_central,sample_central90_area=a_sample_central,
    sampled_hull_share=if(a_full>0)a_sample/a_full else NA_real_,
    sampled_central90_ratio=if(a_full_central>0)a_sample_central/a_full_central else NA_real_)
}))
write.csv(hull_sensitivity,file.path(tabdir,"temporal_hull_sensitivity.csv"),row.names=FALSE)

# Tables are generated from the final result objects. Formatting introduces no
# hand-maintained numerical cells. Full diagnostics also remain available as CSV.
tex_escape <- function(x) {
  x<-as.character(x); x<-gsub("\\", "BACKSLASHPLACEHOLDER",x,fixed=TRUE)
  for(ch in c("&","%","$","#","_","{","}")) x<-gsub(ch,paste0("\\",ch),x,fixed=TRUE)
  x<-gsub("BACKSLASHPLACEHOLDER","\\textbackslash{}",x,fixed=TRUE)
  x<-gsub("~","\\textasciitilde{}",x,fixed=TRUE); x<-gsub("^","\\textasciicircum{}",x,fixed=TRUE); x
}
write_tex_table <- function(d,name,alignment=NULL) {
  if(is.matrix(d)) d<-data.frame(variable=rownames(d),d,check.names=FALSE)
  if(!is.data.frame(d)||!ncol(d)) return(invisible(NULL))
  if(any(vapply(d,is.list,logical(1)))) d<-d[,!vapply(d,is.list,logical(1)),drop=FALSE]
  if(!ncol(d))return(invisible(NULL))
  out<-lapply(seq_along(d),function(j) {
    x<-d[[j]]; digits<-if(grepl("%",names(d)[j],fixed=TRUE))1 else 3
    if(is.numeric(x)) { z<-ifelse(is.na(x),"--",ifelse(abs(x-round(x))<1e-10,format(round(x),trim=TRUE,scientific=FALSE,big.mark=","),formatC(x,digits=digits,format="f"))); z[is.infinite(x)]<-"--"; z }
    else { z<-as.character(x);z[is.na(z)]<-"--";z }
  })
  out<-as.data.frame(out,stringsAsFactors=FALSE)
  header<-tex_escape(gsub("_"," ",names(d),fixed=TRUE))
  if(is.null(alignment))alignment<-paste0(ifelse(vapply(d,is.numeric,logical(1)),"r","l"),collapse="")
  lines<-c(paste0("% Generated from final analysis/preparation objects: ",name),paste0("\\begin{tabular}{",alignment,"}"),"\\toprule",paste0(paste(header,collapse=" & ")," \\\\"),"\\midrule")
  if(nrow(out)) lines<-c(lines,apply(out,1,function(x)paste0(paste(tex_escape(x),collapse=" & ")," \\\\")))
  lines<-c(lines,"\\bottomrule","\\end{tabular}")
  writeLines(lines,file.path(tabdir,paste0("table_",name,".tex")),useBytes=TRUE)
}
tables<-results$tables
for(nm in names(tables)) write_tex_table(tables[[nm]],nm)
for(nm in c("transformations","weighting","bridge_weighting","sample_sensitivity","domain_sensitivity")) {
  d<-tables[[nm]]; if(is.null(d))next
  write_tex_table(d,paste0(nm,"_full"))
  if(nm=="transformations") d <- paper_specs(d)
  if(all(c("specification","fit_2D","max_angle")%in%names(d)))
    write_tex_table(data.frame(Specification=nice_spec(d$specification),N=d$n,`Two PCs (%)`=100*d$fit_2D,`Maximum angle`=d$max_angle,check.names=FALSE),nm,"p{7cm}rrr")
}
summaries<-function(d,columns)do.call(rbind,lapply(columns,function(v)data.frame(Statistic=gsub("_"," ",v),Median=median(d[[v]],na.rm=TRUE),`2.5 percentile`=unname(quantile(d[[v]],.025,na.rm=TRUE)),`97.5 percentile`=unname(quantile(d[[v]],.975,na.rm=TRUE)),check.names=FALSE)))
write_tex_table(summaries(tables$boot_subspace,c("max_angle","rms_angle","eigen_ratio_12","eigen_ratio_23")),"boot_subspace")
write_tex_table(summaries(tables$cluster_stability,c("k","r","ari","pair_jaccard","mean_best_group_jaccard")),"cluster_stability")
cs<-tables$cluster_search; cs<-cs[cs$r==results$clusters$r,c("k","silhouette","smallest_group","selected")]
names(cs)<-c("Groups","Silhouette","Smallest group share","Selected")
write_tex_table(cs,"cluster_search")
bs<-tables$boot_angles
write_tex_table(data.frame(Variable=nice(bs$variable),Reference=bs$angle_reference,`Aligned lower`=bs$aligned_lower,`Aligned upper`=bs$aligned_upper,check.names=FALSE),"boot_angles")
nl<-tables$nonlinear
write_tex_table(data.frame(Method=nl$method,Parameter=nl$parameter,Seed=nl$seed,`Neighbour overlap`=nl$neighbor_overlap,`Profile RMSE`=nl$profile_rmse,check.names=FALSE),"nonlinear")
vf<-tables$variable_fit; vf<-vf[vf$specification=="D",,drop=FALSE]
write_tex_table(data.frame(Variable=nice(vf$variable),`PC1 (%)`=100*vf$predictivity_PC1,`PC2 (%)`=100*vf$predictivity_PC2,`Two PCs (%)`=100*vf$predictivity_2D,RMSE=vf$reconstruction_rmse,check.names=FALSE),"variable_fit")
zp<-tables$zero_positive
write_tex_table(data.frame(Variable=nice(zp$variable),`Zero (%)`=100*zp$zero_share,Minimum=zp$positive_min,Median=zp$positive_median,P90=zp$positive_p90,Maximum=zp$maximum,check.names=FALSE),"zero_positive")
tg<-tables$theil_grouping; tg<-tg[tg$grouping%in%c("clusters","focal_asset_excluded","year","slaveholding"),,drop=FALSE]
write_tex_table(data.frame(Asset=nice(tg$variable),Grouping=gsub("_"," ",tg$grouping),T=tg$theil_total,`Within (%)`=100*tg$within_share,check.names=FALSE),"theil_grouping")
cd<-tables$cluster_dimensions
write_tex_table(data.frame(Dimensions=cd$r,Groups=cd$k,`Variance (%)`=100*cd$cumulative_variance,`ARI to full`=cd$ari_to_full,check.names=FALSE),"cluster_dimensions")
cp<-tables$cluster_profiles
profile_vars<-c("n","share",results$pca$D$variables)
cp_print<-data.frame(Quantity=c("Records","Share (%)",nice(results$pca$D$variables)),check.names=FALSE)
for(i in seq_len(nrow(cp))) { vals<-as.numeric(cp[i,profile_vars]);vals[2]<-100*vals[2];cp_print[[paste("Profile",cp$cluster[i])]]<-vals }
write_tex_table(cp_print,"cluster_profiles")
co<-tables$correlations
pairs<-list(c("slave_men","slave_women"),c("slave_men","cattle_total"),c("slave_men","wine"),c("slave_men","khoe_total"),c("cattle_total","horses_total"),c("cattle_total","sheep_total"),c("cattle_total","wine"),c("vines","wine"),c("vines","brandy"),c("wine","brandy"),c("wheat_vol","grain_other"),c("sheep_total","goats"))
pair_keys<-vapply(pairs,function(z)paste(sort(z),collapse="|"),character(1))
co<-co[vapply(seq_len(nrow(co)),function(i)paste(sort(c(co$variable1[i],co$variable2[i])),collapse="|")%in%pair_keys,logical(1)),]
write_tex_table(data.frame(`First quantity`=nice(co$variable1),`Second quantity`=nice(co$variable2),Correlation=co$correlation,check.names=FALSE),"correlations")
tm<-tables$temporal_fit
write_tex_table(data.frame(Decade=tm$period,Records=tm$n,Households=tm$households,`Fixed fit (%)`=100*tm$fit_fixed_2D,`Separate fit (%)`=100*tm$fit_separate_2D,`Maximum angle`=tm$max_angle,check.names=FALSE),"temporal_fit")
hs<-hull_sensitivity
write_tex_table(data.frame(Decade=hs$decade,N=hs$full_n,`Sample N`=hs$sample_n,`Full hull`=hs$full_hull_area,`Sample hull`=hs$sample_hull_area,`Full central`=hs$full_central90_area,`Sample central`=hs$sample_central90_area,check.names=FALSE),"temporal_hull_sensitivity")
coverage<-do.call(rbind,lapply(names(results$pca),function(nm) {m<-results$pca[[nm]];data.frame(Sample=nm,Variables=length(m$variables),Records=nrow(m$data),First=min(m$data$year),Last=max(m$data$year),`Observed years`=length(unique(m$data$year)),check.names=FALSE)}))
write_tex_table(coverage,"coverage")
ct<-tables$colony_districts
write_tex_table(data.frame(District=ct$district,N=ct$n,Year=ifelse(ct$year_min==ct$year_max,as.character(ct$year_min),paste0(ct$year_min,"-",ct$year_max)),`Mean PC1`=ct$PC1_mean,`SD PC1`=ct$PC1_sd,`Mean PC2`=ct$PC2_mean,`SD PC2`=ct$PC2_sd,check.names=FALSE),"colony_districts")
cw<-tables$colony_within_district
write_tex_table(data.frame(Records=cw$n,`Within district (%)`=100*cw$within_variance_share,`Between districts (%)`=100*cw$between_variance_share,check.names=FALSE),"colony_within")
ba<-tables$blank_assumption
write_tex_table(data.frame(Sample=ba$specification,`Primary N`=ba$primary_n,`Broad N`=ba$n,`Primary fit (%)`=100*tables$fit$fit_2D[match(ba$specification,tables$fit$specification)],`Broad fit (%)`=100*ba$fit_2D,check.names=FALSE),"blank_assumption")
bc<-tables$blank_cluster_sensitivity
write_tex_table(data.frame(`Primary N`=bc$primary_n,`Broad N`=bc$broad_n,`Primary groups`=bc$primary_k,`Broad groups`=bc$broad_k,`Common-record ARI`=bc$ari_common,check.names=FALSE),"blank_cluster_sensitivity")
ri<-tables$reporting_intensity
write_tex_table(data.frame(`Primary records`=ri$n,`Source components`=ri$components,`Complete records`=ri$fully_observed_n,`Active quantities`=length(results$pca$D$variables),check.names=FALSE),"strict_observed_diagnostic")
ca<-tables$calibration; ca<-ca[ca$example==2,,drop=FALSE]
write_tex_table(data.frame(Variable=nice(ca$variable),Recorded=ca$recorded,z=ca$standardised,`Predicted z`=ca$reconstructed_standardised,`Predicted quantity`=ca$reconstructed_source,check.names=FALSE),"calibration")
fit<-tables$fit
write_tex_table(fit,"fit_full")
write_tex_table(data.frame(Sample=fit$specification,N=fit$n,`PC1 (%)`=100*fit$pc1,`PC2 (%)`=100*fit$pc2,`Two PCs (%)`=100*fit$fit_2D,`Median row fit (%)`=100*fit$household_fit_median,check.names=FALSE),"fit")
config<-readRDS("data/analysis/analysis_config.rds")
if(!is.null(config$definitions)) {
  d<-config$definitions
  write_tex_table(data.frame(Variable=nice(d$variable),Definition=d$description,Unit=d$unit,check.names=FALSE),"dictionary","lp{8cm}l")
}
# Preparation metadata are generated by the cleaner and have their own CSVs.
for(nm in c("sample_flow","coverage","dictionary","source_error_register")) {
  choices<-c(file.path(tabdir,paste0(nm,".csv")),file.path("data/analysis",paste0(nm,".csv")))
  path<-choices[file.exists(choices)][1]
  if(!is.na(path)) write_tex_table(read.csv(path,check.names=FALSE),nm,if(nm=="sample_flow")"lp{7cm}rr" else NULL)
}

# Adapt only column naming, never estimates: each displayed value refers back
# to a named numerical column in the analysis tables.
find_col <- function(d, candidates, required=TRUE) {
  ans<-intersect(candidates,names(d)); if(length(ans)) return(ans[1])
  if(required) stop("Missing figure column: ",paste(candidates,collapse=" / "),"; have ",paste(names(d),collapse=", "))
  NULL
}
tf<-paper_specs(tables$transformations)
if(!is.null(tf)&&nrow(tf)) {
  sc<-find_col(tf,c("specification","spec","transformation","method","name"))
  fc<-find_col(tf,c("fit_2D","variance_2D","variance_2d","fit2","fit2d","variance_explained_2D","fit_2d","variance_share_2D"))
  dd<-data.frame(specification=nice_spec(tf[[sc]]),fit=tf[[fc]])
  if(max(dd$fit,na.rm=TRUE)<=1)dd$fit<-100*dd$fit
  dd$specification<-factor(dd$specification,levels=rev(unique(dd$specification)))
  p<-ggplot(dd,aes(fit,specification))+geom_point(size=2,colour=palette[1])+labs(x="Variance represented by two PCs (%)",y=NULL)
  save_figure(p,"fig4_transformations",6.5,max(3.4,.22*nrow(dd)+1),"Two-dimensional fit under alternative transformations and weighting of variables on the same eligible Regime D sample. A higher share does not by itself establish a more informative representation; see the reconstruction and subspace diagnostics.")
}
th<-tables$theil
if(!is.null(th)&&nrow(th)) {
  write_tex_table(th,"theil_full")
  write_tex_table(data.frame(Asset=nice(th$variable),N=th$n,T=th$theil_total,Within=th$theil_within,Between=th$theil_between,`Within (%)`=100*th$within_share,check.names=FALSE),"theil")
  ac<-find_col(th,c("asset","variable")); wc<-find_col(th,c("theil_within","within","W","within_component")); bc<-find_col(th,c("theil_between","between","B","between_component"))
  # Main figure uses the declared baseline partition only when the table also
  # includes alternatives. The full table retains every decomposition.
  spec<-find_col(th,c("partition","specification","grouping"),FALSE)
  if(!is.null(spec)) { vals<-unique(th[[spec]]); base<-vals[grepl("baseline|cluster|kmeans",vals,ignore.case=TRUE)][1]; if(is.na(base))base<-vals[1]; th<-th[th[[spec]]==base,,drop=FALSE] }
  dd<-rbind(data.frame(asset=nice(th[[ac]]),component="Within groups",T=th[[wc]]),data.frame(asset=nice(th[[ac]]),component="Between groups",T=th[[bc]]))
  p<-ggplot(dd,aes(asset,T,fill=component))+geom_col(width=.65)+scale_fill_manual(values=c("Within groups"=palette[1],"Between groups"=palette[2]),name=NULL)+labs(x=NULL,y="Theil T")+coord_flip()
  save_figure(p,"fig5_distribution",6.5,4,"Theil T decomposition of unshifted source quantities in eligible Regime D household-years. Observed zeros are retained. Within and between components sum to total inequality. Groups are estimated from the same asset data, so this is descriptive accounting conditional on the partition.")
}
# The colony cross-section contains a handful of extreme returns. A frame
# stretched to reach them would shrink the dense cloud of households into a
# corner, so the display is cropped to a fixed square. Records outside the
# square are not hidden: each is marked by an arrow at the frame edge that
# points towards it and prints its coordinates.
if(!is.null(results$pca$colony)) {
  colony<-results$pca$colony
  colony_lim<-c(-5,10)
  cs<-score_df(colony)
  inside<-cs$PC1>=colony_lim[1]&cs$PC1<=colony_lim[2]&cs$PC2>=colony_lim[1]&cs$PC2<=colony_lim[2]
  # Clamp each outside record to just within the frame to place its arrow.
  pad<-.03*diff(colony_lim)
  off<-cs[!inside,,drop=FALSE]
  off$x<-pmin(pmax(off$PC1,colony_lim[1]+pad),colony_lim[2]-pad)
  off$y<-pmin(pmax(off$PC2,colony_lim[1]+pad),colony_lim[2]-pad)
  off$label<-sprintf("(%.1f, %.1f)",off$PC1,off$PC2)
  p<-biplot_plot(colony,rows=which(inside),lim=colony_lim)
  if(nrow(off)) p<-p+
    geom_segment(data=off,aes(x=x-.6*sign(PC1-x),y=y-.6*sign(PC2-y),xend=x,yend=y),inherit.aes=FALSE,
      arrow=arrow(length=unit(.07,"inches"),type="closed"),colour="#515151",linewidth=.35)+
    geom_text(data=off,aes(x=x-.6*sign(PC1-x),y=y-.6*sign(PC2-y),label=label),inherit.aes=FALSE,
      hjust=1.1,vjust=.5,size=2.8,colour="#515151")
  save_figure(p,"figA1_colony_biplot",6.5,6.5,sprintf("Colony records, 1823-1825. Standardised log(1+x), row-principal scores and calibrated source-unit axes. Records are not a simultaneous census; source rows and unresolved extremes are documented separately. %d of %d records lie outside the displayed frame; arrows mark their direction and coordinates.",nrow(off),nrow(cs)))
}
if(!is.null(results$cva)) {
  cv<-results$cva; dd<-as.data.frame(cv$scores)
  stopifnot(all(c("CV1","CV2","group")%in%names(dd)))
  observed_means<-aggregate(cbind(CV1,CV2)~group,dd,mean)
  if(!is.null(cv$means)) { expected<-cv$means[match(observed_means$group,cv$means$group),];stopifnot(max(abs(as.matrix(observed_means[,2:3])-as.matrix(expected[,1:2])))<1e-10) }
  p<-ggplot(dd,aes(CV1,CV2,colour=group))+geom_point(size=.5,alpha=.12)+scale_colour_manual(values=palette,name=NULL)+coord_equal()+labs(x="Canonical discriminant",y="Additional display dimension")
  if(!is.null(cv$means))p<-p+geom_point(data=cv$means,size=3,shape=4,stroke=1)
  save_figure(p,"figA2_cva",6.5,5.5,"Two-group canonical display for households with and without enslaved persons. The defining labour quantities are excluded from active variables. The first direction maximises between-group relative to within-group variation; the second follows the sample-optimal two-group display criterion and is not a second discriminant. Crosses indicate group means; the point cloud describes observations rather than confidence regions.")
}
if(!is.null(results$nonlinear$embeddings)) {
  em<-results$nonlinear$embeddings
  # A four-panel visual accompanies the complete parameter and seed grid in
  # the generated diagnostic table. No selection is based on separation.
  selected_names<-c("PCA2",grep("^tsne_15_",names(em),value=TRUE)[1],grep("^umap_15_",names(em),value=TRUE)[1],grep("^umap_40_",names(em),value=TRUE)[1])
  em<-em[selected_names[!is.na(selected_names)]]; plots<-list()
  for(i in seq_along(em)) {
    e<-em[[i]]; if(is.list(e)&&!is.data.frame(e)) e<-e$coordinates
    if(is.null(e)||ncol(e)<2)next
    d<-data.frame(x=e[,1],y=e[,2])
    plots[[length(plots)+1L]]<-ggplot(d,aes(x,y))+geom_point(size=.4,alpha=.2,colour=palette[1])+coord_equal(clip="off")+labs(x="Embedding coordinate 1",y="Embedding coordinate 2")+annotate("text",x=-Inf,y=Inf,label=letters[i],hjust=-.5,vjust=-.5,size=3.5)
  }
  if(length(plots))save_figure(wrap_plots(plots,ncol=2),"figA3_nonlinear",6.5,3.3*ceiling(length(plots)/2),paste0("Fixed-sample nonlinear diagnostics. Panels correspond to ",paste(names(em),collapse="; "),". Separation in these embeddings does not validate discrete household groups; neighbourhood retention is reported separately."))
}

macros<-c(DRecords=nrow(results$pca$D$scores),DHouseholds=length(unique(na.omit(results$pca$D$data$hhid))),DFitTwo=100*sum(share(results$pca$D)[1:2]),BridgeRecords=nrow(bridge$scores),BridgeHouseholds=length(unique(na.omit(bridge$data$hhid))),BridgeFitTwo=100*sum(share(bridge)[1:2]),ColonyRecords=nrow(results$pca$colony$scores),ColonyFitTwo=100*sum(share(results$pca$colony)[1:2]),ClusterK=results$clusters$k,ClusterDimensions=results$clusters$r)
lookup<-function(d,key,value,column) { z<-d[d[[key]]==value,column];stopifnot(length(z)==1L,is.finite(z));unname(z) }
macros<-c(macros,DPCOne=100*share(results$pca$D)[1],DPCTwo=100*share(results$pca$D)[2],
  DFitKhoe=100*lookup(vf,"variable","khoe_total","predictivity_2D"),
  DFitWine=100*lookup(vf,"variable","wine","predictivity_2D"),DFitVines=100*lookup(vf,"variable","vines","predictivity_2D"),
  LevelsFitTwo=100*lookup(tables$transformations,"specification","levels","fit_2D"),AsinhFitTwo=100*lookup(tables$transformations,"specification","asinh","fit_2D"),
  SmallOffsetFitTwo=100*lookup(tables$transformations,"specification","log_c001","fit_2D"),
  OldFilterRecords=lookup(tables$sample_sensitivity,"specification","old_three_positive","n"),
  OldFilterFitTwo=100*lookup(tables$sample_sensitivity,"specification","old_three_positive","fit_2D"),
  WithinSlaves=100*lookup(tables$theil,"variable","slaves_total","within_share"),
  WithinCattle=100*lookup(tables$theil,"variable","cattle_total","within_share"),WithinHorses=100*lookup(tables$theil,"variable","horses_total","within_share"),
  WithinVines=100*lookup(tables$theil,"variable","vines","within_share"),WithinWine=100*lookup(tables$theil,"variable","wine","within_share"),
  ExclWithinVines=100*lookup(tg[tg$grouping=="focal_asset_excluded",],"variable","vines","within_share"),
  ExclWithinWine=100*lookup(tg[tg$grouping=="focal_asset_excluded",],"variable","wine","within_share"),
  ClusterARI=lookup(tables$cluster_dimensions,"r",2L,"ari_to_full"),ClusterBootThree=sum(tables$cluster_stability$k==3),
  ColonyWithinDistrictShare=100*tables$colony_within_district$within_variance_share,
  BroadDRecords=lookup(tables$blank_assumption,"specification","D","n"),
  BroadDFitTwo=100*lookup(tables$blank_assumption,"specification","D","fit_2D"),
  DCompleteComponentRecords=tables$reporting_intensity$fully_observed_n,
  ClusterBootRuns=nrow(tables$cluster_stability),ClusterMeanARI=mean(tables$cluster_stability$ari),
  PCABootRuns=nrow(tables$boot_subspace),
  BootMedianAngle=median(tables$boot_subspace$max_angle),
  BootUpperAngle=unname(quantile(tables$boot_subspace$max_angle,.975)),
  AsinhMaxAngle=lookup(tables$transformations,"specification","asinh","max_angle"),
  LevelsMaxAngle=lookup(tables$transformations,"specification","levels","max_angle"))
macro_lines<-vapply(names(macros),function(nm) {
  value<-if(grepl("ARI$",nm))sprintf("%.3f",macros[[nm]]) else if(grepl("Angle$",nm))sprintf("%.2f",macros[[nm]]) else if(grepl("Fit|Within|^DPC",nm))sprintf("%.1f",macros[[nm]]) else format(round(macros[[nm]]),trim=TRUE,scientific=FALSE,big.mark=",")
  paste0("\\newcommand{\\",nm,"}{",value,"}")
},character(1))
# These prose counts remain derived from the results, including their spelling.
count_words <- function(n) {
  stopifnot(length(n)==1L,is.finite(n),n==round(n),n>=0)
  if(n<=12) c("zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve")[n+1L]
  else format(n,trim=TRUE,scientific=FALSE,big.mark=",")
}
word_lines <- vapply(c("ClusterK","DCompleteComponentRecords"),function(nm)
  paste0("\\newcommand{\\",nm,"Words}{",count_words(macros[[nm]]),"}"),character(1))
writeLines(c("% Generated by 03_figures.R from saved final analysis.",macro_lines,word_lines),file.path(tabdir,"manuscript_values.tex"))
write.csv(data.frame(macro=names(macros),value=unname(macros),source="revision_results.rds"),file.path(tabdir,"manuscript_values.csv"),row.names=FALSE)
write.csv(do.call(rbind,inventory),file.path(tabdir,"figure_inventory.csv"),row.names=FALSE)
write.csv(calibrations$D,file.path(tabdir,"display_axis_calibration.csv"),row.names=FALSE)
writeLines(unlist(lapply(names(captions),function(nm)c(paste0("## ",nm),"",captions[[nm]],""))),file.path(figdir,"figure_captions.md"))
saveRDS(list(captions=captions,calibrations=calibrations,temporal_hull_sensitivity=hull_sensitivity,theme=list(width=6.5,essential_type_pt=9,png_dpi=300,no_titles=TRUE)),file.path(figdir,"plot_metadata.rds"))
message("03_figures.R complete: all figures and tables rendered from saved final objects.")

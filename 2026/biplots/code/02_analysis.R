# Descriptive analysis of Cape household records. Run from HM_revision.
# The revised pipeline contains no event study or compositional application.
if (dir.exists("library")) .libPaths(c(normalizePath("library"), .libPaths()))
options(stringsAsFactors=FALSE)
seed <- 20260908L

# Population-normalised weights change the eigenvalue divisor, not ordinary
# PCA directions, correlations or variance shares. Save the entire convention.
fit_pca <- function(raw, vars=colnames(raw), transform="log1p", offset=1,
                    standardise=TRUE, weights=NULL, unit=NULL) {
  x <- as.matrix(raw[,vars,drop=FALSE]); storage.mode(x) <- "double"
  stopifnot(nrow(x)>ncol(x),all(is.finite(x)),all(x>=0))
  if(!is.null(unit)) x <- sweep(x,2,unit,"*")
  y <- switch(transform,log1p=log1p(x),levels=x,asinh=asinh(x),
              log=log(sweep(x,2,rep(offset,length.out=ncol(x)),"+")),stop("Unknown transformation"))
  w <- if(is.null(weights))rep(1/nrow(y),nrow(y))else weights/sum(weights)
  stopifnot(length(w)==nrow(y),all(is.finite(w)),all(w>0))
  center <- colSums(y*w); z <- sweep(y,2,center,"-")
  scales <- if(standardise)sqrt(colSums(z^2*w))else rep(1,ncol(z))
  stopifnot(all(is.finite(scales)),all(scales>1e-12))
  names(center) <- names(scales) <- vars
  z <- sweep(z,2,scales,"/")
  eg <- eigen(crossprod(z,z*w),symmetric=TRUE); vv <- eg$vectors
  for(j in seq_len(ncol(vv))) vv[,j] <- vv[,j]*sign(vv[which.max(abs(vv[,j])),j])
  dimnames(vv) <- list(vars,paste0("PC",seq_along(vars)))
  scores <- z%*%vv; err <- max(abs(z-scores%*%t(vv)))/max(1,max(abs(z)))
  stopifnot(err<1e-10,min(eg$values)>-1e-10)
  eig <- pmax(0,eg$values); rec <- scores[,1:2,drop=FALSE]%*%t(vv[,1:2,drop=FALSE])
  den <- colSums(z^2*w); hden <- rowSums(z^2)
  f1 <- vv[,1]^2*eig[1]/den; f2 <- vv[,2]^2*eig[2]/den
  list(scores=scores,rotation=vv,eigenvalues=eig,sdev=sqrt(eig),variance=eig/sum(eig),
    center=center,scale=scales,variables=vars,transform=transform,offset=offset,
    standardise=standardise,unit=unit,transformed=y,matrix=z,weights=w,
    variable_fit=data.frame(variable=vars,predictivity_PC1=f1,predictivity_PC2=f2,
      predictivity_2D=f1+f2,reconstruction_rmse=sqrt(colSums((z-rec)^2*w))),
    household_fit=ifelse(hden>1e-20,rowSums(scores[,1:2,drop=FALSE]^2)/hden,NA_real_),
    reconstruction_error=err,
    coordinate_convention="row-principal: scores=ZV=UD; variable coordinates=V; population variance divisor")
}
project_pca <- function(m,raw) {
  x <- as.matrix(raw[,m$variables,drop=FALSE])
  if(!is.null(m$unit))x <- sweep(x,2,m$unit,"*")
  y <- switch(m$transform,log1p=log1p(x),levels=x,asinh=asinh(x),
              log=log(sweep(x,2,rep(m$offset,length.out=ncol(x)),"+")))
  sweep(sweep(y,2,m$center,"-"),2,m$scale,"/")%*%m$rotation
}
subspace <- function(a,b,r=2L) {
  cs <- pmin(1,pmax(0,svd(crossprod(a[,seq_len(r),drop=FALSE],b[,seq_len(r),drop=FALSE]),nu=0,nv=0)$d))
  cs[abs(cs-1)<1e-14] <- 1
  aa <- acos(cs)*180/pi
  c(max_angle=max(aa),rms_angle=sqrt(mean(aa^2)),projection_distance=sqrt(2*sum(1-cs^2)))
}
adjusted_rand <- function(a,b) {
  tt <- table(a,b); ch <- function(x)x*(x-1)/2
  aa <- sum(ch(rowSums(tt))); bb <- sum(ch(colSums(tt))); ex <- aa*bb/ch(sum(tt)); den <- (aa+bb)/2-ex
  if(abs(den)<1e-12)return(as.numeric(all(rowSums(tt>0)==1L)&&all(colSums(tt>0)==1L)))
  (sum(ch(tt))-ex)/den
}
assign_centers <- function(x,centers) {
  dd <- outer(rowSums(x*x),rowSums(centers*centers),"+")-2*tcrossprod(x,centers)
  max.col(-dd,ties.method="first")
}
# Selection uses a bounded observation sample and reports that criterion.
# Selected centres initialise a final fit on every observation. Projected
# silhouette values never select dimensionality; full rank is the reference.
select_clusters <- function(scores,r=ncol(scores),search_n=1500L,silhouette_n=750L,
                            starts=10L,random_seed=seed) {
  set.seed(random_seed); x <- scores[,seq_len(r),drop=FALSE]
  ix <- sort(sample.int(nrow(x),min(search_n,nrow(x))))
  si <- sort(sample.int(length(ix),min(silhouette_n,length(ix))))
  dd <- stats::dist(x[ix[si],,drop=FALSE]); fits <- rows <- vector("list",7L)
  for(k in 2:8) {
    fit <- kmeans(x[ix,,drop=FALSE],centers=k,nstart=starts,iter.max=200,algorithm="Lloyd")
    cl <- fit$cluster[si]
    sil <- if(length(unique(cl))>1)mean(cluster::silhouette(cl,dd)[,3])else -1
    rows[[k-1]] <- data.frame(r=r,k=k,silhouette=sil,search_n=length(ix),silhouette_n=length(si),
      nstart=starts,smallest_group=min(tabulate(fit$cluster,nbins=k))/length(ix),within_ss=fit$tot.withinss)
    fits[[k-1]] <- fit
  }
  grid <- do.call(rbind,rows)
  chosen <- which(grid$silhouette>=max(grid$silhouette)-1e-10)[1]
  fit <- kmeans(x,centers=fits[[chosen]]$centers,iter.max=300,algorithm="Lloyd")
  stopifnot(is.null(fit$ifault)||fit$ifault==0)
  grid$selected <- seq_len(nrow(grid))==chosen
  list(k=grid$k[chosen],r=r,assignment=fit$cluster,centers=fit$centers,grid=grid,
       search_indices=ix,silhouette_indices=ix[si],fit=fit)
}
theil_value <- function(x) {
  stopifnot(length(x)>0,all(is.finite(x)),all(x>=0))
  mu <- mean(x); if(mu==0)return(NA_real_)
  z <- x[x>0]/mu; sum(z*log(z))/length(x)
}
theil_decompose <- function(x,group) {
  stopifnot(length(x)==length(group),!anyNA(x),!anyNA(group),all(x>=0))
  mu <- mean(x); n <- length(x); tt <- theil_value(x)
  if(mu==0)return(data.frame(n=n,zero_share=1,theil_total=NA_real_,theil_within=NA_real_,
    theil_between=NA_real_,within_share=NA_real_,between_share=NA_real_,additivity_error=NA_real_))
  parts <- lapply(split(x,group),function(z) {
    mg <- mean(z); if(mg==0)return(c(within=0,between=0))
    mass <- length(z)/n*mg/mu
    c(within=mass*theil_value(z),between=mass*log(mg/mu))
  })
  pp <- rowSums(do.call(cbind,parts)); ee <- tt-sum(pp); stopifnot(abs(ee)<1e-10)
  data.frame(n=n,zero_share=mean(x==0),theil_total=tt,theil_within=pp[1],theil_between=pp[2],
    within_share=if(tt>0)pp[1]/tt else NA_real_,between_share=if(tt>0)pp[2]/tt else NA_real_,
    additivity_error=ee,row.names=NULL)
}
theil_checks <- function() {
  stopifnot(abs(theil_value(c(0,2))-log(2))<1e-12,theil_value(rep(7,9))==0,is.na(theil_value(c(0,0))))
  x <- c(0,0,1,5,8,22); g <- c(1,1,2,2,3,3)
  a <- theil_decompose(x,g); b <- theil_decompose(x*123.5,g)
  stopifnot(max(abs(a[,4:6]-b[,4:6]))<1e-12); invisible(TRUE)
}
hh_indices <- function(ids) {stopifnot(!anyNA(ids)); split(seq_along(ids),as.character(ids))}
draw_histories <- function(hh)unlist(hh[sample.int(length(hh),length(hh),replace=TRUE)],use.names=FALSE)
fit_row <- function(name,m)data.frame(specification=name,n=nrow(m$scores),p=length(m$variables),
  pc1=m$variance[1],pc2=m$variance[2],fit_2D=sum(m$variance[1:2]),residual_2D=1-sum(m$variance[1:2]),
  household_fit_p10=unname(quantile(m$household_fit,.1,na.rm=TRUE)),
  household_fit_median=median(m$household_fit,na.rm=TRUE),
  household_fit_p90=unname(quantile(m$household_fit,.9,na.rm=TRUE)),reconstruction_error=m$reconstruction_error)

# Everything below this marker belongs to the production build.
required <- c("cluster","biplotEZ","FNN","Rtsne","uwot")
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop("Missing required packages: ",paste(missing,collapse=", "))
theil_checks(); set.seed(seed)
config <- readRDS("data/analysis/analysis_config.rds")
datasets <- list(D=readRDS("data/analysis/regime_D_biplot.rds"),B=readRDS("data/analysis/regime_B_biplot.rds"),
  bridge=readRDS("data/analysis/bridge_biplot.rds"),colony=readRDS("data/analysis/colony_1825.rds"))
varsets <- list(D=config$vars_D,B=config$vars_B,bridge=config$vars_bridge,colony=config$vars_colony)
stopifnot(all(lengths(varsets)>=2L))
dir.create("output/tables",recursive=TRUE,showWarnings=FALSE)
tables <- pca <- list()
for(nm in names(datasets)) {
  cat("PCA:",nm,"; records",nrow(datasets[[nm]]),"\n")
  dat <- datasets[[nm]]
  stopifnot(nrow(dat)>0,all(complete.cases(dat[,varsets[[nm]],drop=FALSE])))
  pca[[nm]] <- fit_pca(dat,varsets[[nm]]); pca[[nm]]$data <- dat
}
D <- datasets$D; vars <- varsets$D; baseline <- pca$D
tables$fit <- do.call(rbind,lapply(names(pca),function(nm)fit_row(nm,pca[[nm]])))
tables$variable_fit <- do.call(rbind,lapply(names(pca),function(nm)cbind(specification=nm,pca[[nm]]$variable_fit)))
tables$eigenvalues <- do.call(rbind,lapply(names(pca),function(nm)data.frame(specification=nm,
  component=seq_along(pca[[nm]]$variance),eigenvalue=pca[[nm]]$eigenvalues,
  share=pca[[nm]]$variance,cumulative=cumsum(pca[[nm]]$variance))))
tables$zero_positive <- do.call(rbind,lapply(vars,function(v) {
  xx <- D[[v]]; pos <- xx[xx>0]
  data.frame(variable=v,zero_share=mean(xx==0),positive_min=min(pos),positive_p10=unname(quantile(pos,.1)),
    positive_median=median(pos),positive_p90=unname(quantile(pos,.9)),maximum=max(xx))
}))

# Wholly blank economic returns are incomplete in the primary analysis.
# These broad samples show the separate, conditional assumption that every
# blank represents zero. They never silently replace the primary population.
blank_files <- c(D="regime_D_blank_assumption.rds",B="regime_B_blank_assumption.rds",
  bridge="bridge_blank_assumption.rds",colony="colony_blank_assumption.rds")
blank_data <- lapply(blank_files,function(f)readRDS(file.path("data/analysis",f)))
blank_models <- lapply(names(blank_data),function(nm)fit_pca(blank_data[[nm]],varsets[[nm]]))
names(blank_models) <- names(blank_data)
tables$blank_assumption <- do.call(rbind,lapply(names(blank_data),function(nm) {
  mm <- blank_models[[nm]]
  cbind(fit_row(nm,mm),primary_n=nrow(datasets[[nm]]),
    added_wholly_blank_n=nrow(blank_data[[nm]])-nrow(datasets[[nm]]),
    as.data.frame(as.list(subspace(pca[[nm]]$rotation,mm$rotation))),
    interpretation="conditional inclusion of wholly blank economic returns as zero holdings")
}))

cat("Transformation, units and covariance sensitivity\n")
specs <- list(levels=list(transform="levels"),log1p=list(transform="log1p"),asinh=list(transform="asinh"),
  log_c001=list(transform="log",offset=.01),log_c01=list(transform="log",offset=.1),
  log_c1=list(transform="log",offset=1),covariance_log1p=list(transform="log1p",standardise=FALSE))
# A count of vines in thousands is a transparent unit change. Equivalent
# offsets must also change by .001: log(.001*x+.001)=log(x+1)+log(.001).
unit_var <- intersect(c("vines","sheep_total","cattle_total"),vars)[1]
stopifnot(!is.na(unit_var)); uu <- rep(1,length(vars)); uu[match(unit_var,vars)] <- .001
specs$units_naive_log1p <- list(transform="log1p",unit=uu)
specs$units_equivalent_log <- list(transform="log",unit=uu,offset=uu)
specs$units_naive_asinh <- list(transform="asinh",unit=uu)
spec_models <- lapply(specs,function(s)do.call(fit_pca,c(list(raw=D,vars=vars),s)))
tables$transformations <- do.call(rbind,lapply(names(spec_models),function(nm) {
  mm <- spec_models[[nm]]
  cbind(fit_row(nm,mm),as.data.frame(as.list(subspace(baseline$rotation,mm$rotation))),
    score_distance_rank_correlation=NA_real_)
}))
set.seed(seed+1L); pair_i <- sample.int(nrow(D),4000L,replace=TRUE); pair_j <- sample.int(nrow(D),4000L,replace=TRUE)
base_dist <- sqrt(rowSums((baseline$matrix[pair_i,]-baseline$matrix[pair_j,])^2))
full_pc_dist <- sqrt(rowSums((baseline$scores[pair_i,]-baseline$scores[pair_j,])^2))
full_pc_distance_error <- max(abs(base_dist-full_pc_dist))/max(1,max(base_dist))
stopifnot(full_pc_distance_error<1e-10)
for(i in seq_along(spec_models)) {
  zz <- spec_models[[i]]$matrix; dd <- sqrt(rowSums((zz[pair_i,]-zz[pair_j,])^2))
  tables$transformations$score_distance_rank_correlation[i] <- cor(base_dist,dd,method="spearman")
}
eqerr <- max(abs(baseline$matrix-spec_models$units_equivalent_log$matrix)); stopifnot(eqerr<1e-10)
tables$unit_equivalence <- data.frame(variable=unit_var,multiplier=.001,offset_original=1,
  offset_converted=.001,standardised_matrix_error=eqerr)
tables$transform_variable_fit <- do.call(rbind,lapply(names(spec_models),function(nm)
  cbind(specification=nm,spec_models[[nm]]$variable_fit)))
tables$transform_correlations <- do.call(rbind,lapply(names(spec_models),function(nm) {
  cc <- cor(spec_models[[nm]]$transformed); ij <- which(upper.tri(cc),arr.ind=TRUE)
  data.frame(specification=nm,variable1=rownames(cc)[ij[,1]],variable2=colnames(cc)[ij[,2]],correlation=cc[ij])
}))
tables$correlations <- subset(tables$transform_correlations,specification=="log1p")

cat("Weighting, selection, reporting intensity and influential observations\n")
wh <- 1/as.numeric(table(D$hhid)[as.character(D$hhid)])
wy <- 1/as.numeric(table(D$year)[as.character(D$year)])
weighted_models <- list(equal_household=fit_pca(D,vars,weights=wh),equal_year=fit_pca(D,vars,weights=wy))
tables$weighting <- do.call(rbind,lapply(names(weighted_models),function(nm)
  cbind(fit_row(nm,weighted_models[[nm]]),as.data.frame(as.list(subspace(baseline$rotation,weighted_models[[nm]]$rotation))))))
old <- rowSums(D[,vars,drop=FALSE]>0)>=3L
tail <- rowSums(baseline$matrix^2)>unname(quantile(rowSums(baseline$matrix^2),.99))
sample_specs <- list(old_three_positive=which(old),omit_top_one_percent_distance=which(!tail))
if("n_observed_components"%in%names(D)) {
  counts <- D$n_observed_components
  sample_specs$at_least_one_observed_component <- which(counts>0)
  sample_specs$reporting_intensity_upper_half <- which(counts>=unname(quantile(counts,.5)))
  sample_specs$reporting_intensity_upper_quartile <- which(counts>=unname(quantile(counts,.75)))
  tables$reporting_intensity <- data.frame(n=nrow(D),components=21L,
    all_active_blank_n=sum(counts==0),fully_observed_n=sum(!D$any_blank_active),
    all_zero_active_n=sum(rowSums(D[,vars,drop=FALSE])==0),
    observed_components_min=min(counts),observed_components_median=median(counts),
    observed_components_q75=unname(quantile(counts,.75)),observed_components_max=max(counts),
    interpretation="reporting-intensity restrictions change the population; they cannot validate the blank-as-zero assumption")
}
if("any_outlier"%in%names(D)&&any(D$any_outlier))sample_specs$omit_flagged_outliers <- which(!D$any_outlier)
if("any_blank_active"%in%names(D)&&sum(!D$any_blank_active)>ncol(baseline$matrix))
  sample_specs$no_blank_active_fields <- which(!D$any_blank_active)
tables$sample_sensitivity <- do.call(rbind,lapply(names(sample_specs),function(nm) {
  ix <- sample_specs[[nm]]; xx <- as.matrix(D[ix,vars,drop=FALSE])
  if(any(apply(xx,2,sd)==0))return(data.frame(specification=nm,n=length(ix),p=ncol(xx),
    pc1=NA,pc2=NA,fit_2D=NA,residual_2D=NA,household_fit_p10=NA,household_fit_median=NA,
    household_fit_p90=NA,reconstruction_error=NA,max_angle=NA,rms_angle=NA,projection_distance=NA,
    status="not estimable: a variable has zero variance in this subset"))
  mm <- fit_pca(D[ix,,drop=FALSE],vars)
  cbind(fit_row(nm,mm),as.data.frame(as.list(subspace(baseline$rotation,mm$rotation))),status="estimated")
}))
tables$omitted_profiles <- do.call(rbind,lapply(vars,function(v)data.frame(variable=v,
  eligible_mean=mean(D[[v]]),old_filter_mean=mean(D[[v]][old]),
  excluded_mean=if(any(!old))mean(D[[v]][!old])else NA_real_,
  excluded_zero_share=if(any(!old))mean(D[[v]][!old]==0)else NA_real_)))
tables$influential <- data.frame(row_index=which(tail),
  hhobs=if("hhobs"%in%names(D))as.character(D$hhobs[tail])else as.character(which(tail)),
  year=D$year[tail],standardised_distance=sqrt(rowSums(baseline$matrix[tail,,drop=FALSE]^2)))
domain_vars <- list(omit_wine_brandy=setdiff(vars,c("wine","brandy")))
if(all(c("slave_men","slave_women","slaves_total")%in%names(D)))
  domain_vars$combined_slave_count <- c(setdiff(vars,c("slave_men","slave_women")),"slaves_total")
tables$domain_sensitivity <- do.call(rbind,lapply(names(domain_vars),function(nm) {
  mm <- fit_pca(D,domain_vars[[nm]])
  cbind(fit_row(nm,mm),radial_rank_correlation=cor(rowSums(baseline$scores[,1:2]^2),
    rowSums(mm$scores[,1:2]^2),method="spearman"),variables=paste(domain_vars[[nm]],collapse=";"))
}))
tables$period_omission <- do.call(rbind,lapply(sort(unique(D$year)),function(y) {
  mm <- fit_pca(D[D$year!=y,,drop=FALSE],vars)
  cbind(omitted_year=y,fit_row("leave_one_year_out",mm),as.data.frame(as.list(subspace(baseline$rotation,mm$rotation))))
}))

cat("500 household bootstrap PCA repetitions\n")
hh <- hh_indices(D$hhid); nb <- 500L
angles <- matrix(NA_real_,nb,length(vars)); axis_angles <- angles; subs <- matrix(NA_real_,nb,5L)
base_ang <- atan2(baseline$rotation[,2],baseline$rotation[,1])*180/pi
set.seed(seed+2L)
for(b in seq_len(nb)) {
  ix <- draw_histories(hh); mm <- fit_pca(D[ix,,drop=FALSE],vars); vv <- mm$rotation[,1:2,drop=FALSE]
  for(j in 1:2)if(sum(vv[,j]*baseline$rotation[,j])<0)vv[,j] <- -vv[,j]
  aa <- atan2(vv[,2],vv[,1])*180/pi
  axis_angles[b,] <- base_ang+((aa-base_ang+180)%%360)-180
  # Procrustes aligns the plane when nearby eigenvalues permit PC rotation.
  sv <- svd(crossprod(vv,baseline$rotation[,1:2,drop=FALSE])); va <- vv%*%sv$u%*%t(sv$v)
  aa <- atan2(va[,2],va[,1])*180/pi
  angles[b,] <- base_ang+((aa-base_ang+180)%%360)-180
  subs[b,] <- c(subspace(baseline$rotation,mm$rotation),
    mm$eigenvalues[1]/mm$eigenvalues[2],mm$eigenvalues[2]/mm$eigenvalues[3])
  if(b%%100L==0)cat("PCA bootstrap",b,"/",nb,"\n")
}
tables$boot_angles <- data.frame(variable=vars,angle_reference=base_ang,
  aligned_lower=apply(angles,2,quantile,.025),aligned_upper=apply(angles,2,quantile,.975),
  axis_lower=apply(axis_angles,2,quantile,.025),axis_upper=apply(axis_angles,2,quantile,.975),
  resamples=nb,unit="household history",coordinates="row-principal V")
colnames(subs) <- c("max_angle","rms_angle","projection_distance","eigen_ratio_12","eigen_ratio_23")
tables$boot_subspace <- data.frame(repetition=seq_len(nb),subs)

cat("Fixed temporal frame diagnostics\n")
bm <- pca$bridge; bd <- datasets$bridge
period <- if("decade"%in%names(bd))bd$decade else floor(bd$year/10)*10
bm$data$period <- period; pca$bridge <- bm; groups <- split(seq_len(nrow(bd)),period)
equal_n <- min(500L,min(lengths(groups))); set.seed(seed+3L)
tables$temporal_fit <- do.call(rbind,lapply(names(groups),function(nm) {
  ii <- groups[[nm]]; den <- sum(bm$matrix[ii,,drop=FALSE]^2)
  sm <- fit_pca(bd[ii,,drop=FALSE],varsets$bridge)
  data.frame(period=nm,n=length(ii),households=length(unique(bd$hhid[ii])),
    fit_fixed_2D=sum(bm$scores[ii,1:2,drop=FALSE]^2)/den,fit_separate_2D=sum(sm$variance[1:2]),
    max_angle=subspace(bm$rotation,sm$rotation)["max_angle"],equal_subsample_n=equal_n)
}))
temporal_samples <- lapply(groups,function(ii)sort(sample(ii,equal_n)))
tables$bridge_weighting <- do.call(rbind,lapply(c("household","year"),function(nm) {
  field <- if(nm=="household")bd$hhid else bd$year; ww <- 1/as.numeric(table(field)[as.character(field)])
  mm <- fit_pca(bd,varsets$bridge,weights=ww)
  cbind(fit_row(paste0("equal_",nm),mm),as.data.frame(as.list(subspace(bm$rotation,mm$rotation))))
}))

cat("Clustering dimensions 2 through full rank; k 2 through 8\n")
rank <- sum(baseline$eigenvalues>max(baseline$eigenvalues)*1e-10); stopifnot(rank>=2L)
searches <- lapply(2:rank,function(r)select_clusters(baseline$scores,r=r,random_seed=seed+4L))
clusters <- searches[[length(searches)]]
tables$cluster_search <- do.call(rbind,lapply(searches,`[[`,"grid"))
tables$cluster_dimensions <- do.call(rbind,lapply(searches,function(m)data.frame(r=m$r,k=m$k,
  cumulative_variance=sum(baseline$variance[seq_len(m$r)]),ari_to_full=adjusted_rand(clusters$assignment,m$assignment),
  retained_rule="full rank reference; projected fits sensitivity only")))
profile_rows <- function(m)do.call(rbind,lapply(sort(unique(m$assignment)),function(g)
  data.frame(r=m$r,k=m$k,cluster=g,n=sum(m$assignment==g),share=mean(m$assignment==g),
    as.list(colMeans(D[m$assignment==g,vars,drop=FALSE])),check.names=FALSE)))
tables$cluster_profiles <- profile_rows(clusters)
tables$cluster_dimension_profiles <- do.call(rbind,lapply(searches,profile_rows))

cat("200 household bootstraps of preprocessing, PCA and k selection\n")
nc <- 200L; stab <- group_stab <- vector("list",nc)
for(b in seq_len(nc)) {
  set.seed(seed+10000L+b); ix <- draw_histories(hh); mm <- fit_pca(D[ix,,drop=FALSE],vars)
  rr <- sum(mm$eigenvalues>max(mm$eigenvalues)*1e-10)
  cc <- select_clusters(mm$scores,r=rr,random_seed=seed+20000L+b)
  common <- sort(unique(ix))
  pred <- assign_centers(project_pca(mm,D[common,,drop=FALSE])[,seq_len(rr),drop=FALSE],cc$centers)
  ref <- clusters$assignment[common]; tab <- table(ref,pred); a <- rowSums(tab); z <- colSums(tab)
  jac <- tab/(outer(a,z,"+")-tab)
  group_stab[[b]] <- data.frame(repetition=b,reference_cluster=as.integer(rownames(tab)),
    best_jaccard=apply(jac,1,max),reference_n=as.numeric(a),
    matched_bootstrap_cluster=as.integer(colnames(tab)[max.col(jac,ties.method="first")]))
  choose2 <- function(x)x*(x-1)/2
  joint <- sum(choose2(tab)); union <- sum(choose2(a))+sum(choose2(z))-joint
  stab[[b]] <- data.frame(repetition=b,r=rr,k=cc$k,n_boot=length(ix),n_common=length(common),
    ari=adjusted_rand(ref,pred),pair_jaccard=if(union>0)joint/union else NA_real_,
    mean_best_group_jaccard=mean(apply(jac,1,max)))
  if(b%%25L==0)cat("Clustering bootstrap",b,"/",nc,"\n")
}
tables$cluster_stability <- do.call(rbind,stab)
tables$cluster_group_stability <- do.call(rbind,group_stab)

cat("Unshifted Theil decomposition and grouping sensitivity\n")
ineq_vars <- intersect(c("slaves_total","cattle_total","horses_total","vines","wine"),names(D))
partition <- list(clusters=clusters$assignment,year=D$year)
if("slaves_total"%in%names(D))partition$slaveholding <- as.integer(D$slaves_total>0)
if("district"%in%names(D)&&length(unique(D$district))>1)partition$district <- D$district
tables$theil <- do.call(rbind,lapply(ineq_vars,function(v)
  cbind(variable=v,grouping="clusters",theil_decompose(D[[v]],clusters$assignment))))
tables$theil_grouping <- do.call(rbind,lapply(names(partition),function(nm)do.call(rbind,lapply(ineq_vars,function(v)
  cbind(variable=v,grouping=nm,theil_decompose(D[[v]],partition[[nm]]))))))
excluded_assignments <- list()
for(v in ineq_vars) {
  omit <- if(v=="slaves_total")c("slaves_total","slave_men","slave_women")else v
  mm <- fit_pca(D,setdiff(vars,omit)); cc <- select_clusters(mm$scores,random_seed=seed+30000L+match(v,ineq_vars))
  excluded_assignments[[v]] <- cc$assignment
  tables$theil_grouping <- rbind(tables$theil_grouping,
    cbind(variable=v,grouping="focal_asset_excluded",theil_decompose(D[[v]],cc$assignment)))
}
blank_clusters <- select_clusters(blank_models$D$scores,random_seed=seed+31000L)
matched <- match(D$hhobs,blank_data$D$hhobs); stopifnot(!anyNA(matched))
tables$blank_cluster_sensitivity <- data.frame(primary_n=nrow(D),broad_n=nrow(blank_data$D),
  primary_k=clusters$k,broad_k=blank_clusters$k,
  ari_common=adjusted_rand(clusters$assignment,blank_clusters$assignment[matched]))
tables$theil_blank_assumption <- do.call(rbind,lapply(ineq_vars,function(v)
  cbind(variable=v,grouping="all_blank_as_zero_clusters",
    theil_decompose(blank_data$D[[v]],blank_clusters$assignment))))

cat("Bounded nonlinear neighbourhood investigation\n")
set.seed(seed+5L); ni <- sort(sample.int(nrow(D),min(2000L,nrow(D))))
nx <- baseline$matrix[ni,,drop=FALSE]; ref_nn <- FNN::get.knn(nx,k=15L)$nn.index
neighbor_diagnostic <- function(embedding) {
  nn <- FNN::get.knn(embedding,k=15L)$nn.index
  overlap <- vapply(seq_len(nrow(nx)),function(i)length(intersect(nn[i,],ref_nn[i,]))/15,numeric(1))
  # Original-space characteristic prediction by embedding neighbours.
  neighbor_mean <- t(vapply(seq_len(nrow(nx)),function(i)colMeans(nx[nn[i,],,drop=FALSE]),numeric(ncol(nx))))
  byvar <- sqrt(colMeans((nx-neighbor_mean)^2))
  same <- mean(vapply(seq_len(nrow(nx)),function(i)
    mean(clusters$assignment[ni[nn[i,]]]==clusters$assignment[ni[i]]),numeric(1)))
  list(overlap=mean(overlap),profile_rmse=sqrt(mean(byvar^2)),byvar=byvar,same_cluster_neighbor_share=same)
}
nonlinear <- list(sample_indices=ni,embeddings=list(),parameters=list()); nl_rows <- nl_vars <- list()
record_embedding <- function(name,emb,method,parameter,rs) {
  dg <- neighbor_diagnostic(emb)
  nonlinear$embeddings[[name]] <<- emb
  nonlinear$parameters[[name]] <<- list(method=method,parameter=parameter,seed=rs)
  nl_rows[[name]] <<- data.frame(specification=name,method=method,parameter=parameter,seed=rs,
    n=nrow(nx),neighbors_evaluated=15,neighbor_overlap=dg$overlap,profile_rmse=dg$profile_rmse,
    same_cluster_neighbor_share=dg$same_cluster_neighbor_share)
  nl_vars[[name]] <<- data.frame(specification=name,variable=vars,neighbor_profile_rmse=dg$byvar)
}
record_embedding("PCA2",baseline$scores[ni,1:2],"PCA",2L,seed)
for(rs in c(seed+6L,seed+7L)) {
  for(pp in c(15L,40L)) {
    set.seed(rs)
    fit <- Rtsne::Rtsne(nx,dims=2,perplexity=pp,check_duplicates=FALSE,pca=FALSE,
      max_iter=1000L,theta=.5,num_threads=1L,verbose=FALSE)
    record_embedding(paste("tsne",pp,rs,sep="_"),fit$Y,"t-SNE",pp,rs)
  }
  for(nn in c(15L,40L)) {
    set.seed(rs)
    emb <- uwot::umap(nx,n_neighbors=nn,min_dist=.1,n_components=2,n_epochs=300L,
      n_threads=1L,n_sgd_threads=1L,verbose=FALSE,ret_model=FALSE)
    record_embedding(paste("umap",nn,rs,sep="_"),emb,"UMAP",nn,rs)
  }
}
initial_nl <- do.call(rbind,nl_rows)
# A diagnostic trigger, not a significance test: only extend the investigation
# if every t-SNE/UMAP parameter/seed fit improves both original-neighbour
# preservation and original-profile prediction relative to two-PC projection.
# This evaluates a further descriptive map; it does not establish historical
# nonlinearity or natural types. Bandwidths are multiples of median distance.
kernel_trigger <- all(initial_nl$neighbor_overlap[-1]>initial_nl$neighbor_overlap[1]) &&
  all(initial_nl$profile_rmse[-1]<initial_nl$profile_rmse[1])
tables$kernel_decision <- data.frame(triggered=kernel_trigger,
  rule="all eight t-SNE/UMAP fits improve neighbour overlap and profile RMSE against PCA2",
  interpretation="bounded descriptive sensitivity; not evidence of natural classes")
if(kernel_trigger) {
  cat("Consistent nonlinear diagnostic gains: three RBF kernel PCA bandwidths\n")
  d2 <- outer(rowSums(nx^2),rowSums(nx^2),"+")-2*tcrossprod(nx)
  d2[d2<0] <- 0
  med <- sqrt(median(d2[upper.tri(d2)&d2>0]))
  for(mult in c(.5,1,2)) {
    bandwidth <- med*mult; ker <- exp(-d2/(2*bandwidth^2))
    ker <- sweep(sweep(ker,1,rowMeans(ker),"-"),2,colMeans(ker),"-")+mean(ker)
    eg <- eigen(ker,symmetric=TRUE)
    stopifnot(eg$values[2]>0)
    emb <- sweep(eg$vectors[,1:2,drop=FALSE],2,sqrt(eg$values[1:2]),"*")
    record_embedding(paste0("kernel_rbf_",mult),emb,"Kernel PCA (RBF)",mult,seed)
    nonlinear$parameters[[paste0("kernel_rbf_",mult)]]$bandwidth <- bandwidth
  }
}
tables$nonlinear <- do.call(rbind,nl_rows); tables$nonlinear_profiles <- do.call(rbind,nl_vars)

cat("Slaveholding CVA with the sample-optimal additional dimension\n")
cva_vars <- setdiff(vars,c("slave_men","slave_women","slaves_total"))
slave_total <- if("slaves_total"%in%names(D))D$slaves_total else D$slave_men+D$slave_women
cva_group <- factor(ifelse(slave_total>0,"Slaveholding","No recorded slaves")); stopifnot(nlevels(cva_group)==2L)
cva_input <- as.data.frame(log1p(D[,cva_vars,drop=FALSE])); cva_warnings <- character()
cv <- withCallingHandlers(biplotEZ::CVA(biplotEZ::biplot(cva_input,classes=cva_group),
  weightedCVA="weighted",low.dim="sample.opt",dim.biplot=2),warning=function(w) {
    if(grepl("dimension of the canonical space < dim.biplot",conditionMessage(w),fixed=TRUE)) {
      cva_warnings <<- c(cva_warnings,conditionMessage(w)); invokeRestart("muffleWarning")
    }
  })
cva <- list(scores=data.frame(CV1=cv$Z[,1],CV2=cv$Z[,2],group=cva_group),
  loadings=data.frame(CV1=cv$ax.one.unit[,1],CV2=cv$ax.one.unit[,2],variable=cva_vars),
  means=data.frame(CV1=cv$Zmeans[,1],CV2=cv$Zmeans[,2],group=cv$g.names),variables=cva_vars,
  method="biplotEZ weighted CVA; low.dim=sample.opt; two groups, one discriminant dimension",
  W=cv$Wmat,B=cv$Bmat,Mr=cv$Mr,low_dim=cv$low.dim,warnings=cva_warnings,
  package_version=as.character(packageVersion("biplotEZ")))
stopifnot(all(is.finite(as.matrix(cva$scores[,1:2]))),qr(cva$B,tol=1e-7)$rank==1L)
tables$cva_group_means <- do.call(rbind,lapply(levels(cva_group),function(g)
  data.frame(group=g,n=sum(cva_group==g),as.list(colMeans(D[cva_group==g,cva_vars,drop=FALSE])))))
tables$cva_method <- data.frame(method="weighted",low_dim=cv$low.dim,between_rank=qr(cva$B,tol=1e-7)$rank,
  groups=2,package=as.character(packageVersion("biplotEZ")),expected_warning=paste(cva_warnings,collapse="; "))

cat("Colony district distributions and extreme-value sensitivity\n")
cd <- datasets$colony; cm <- pca$colony
tables$colony_districts <- do.call(rbind,lapply(sort(unique(cd$district)),function(g) {
  ii <- cd$district==g
  data.frame(district=g,n=sum(ii),year_min=min(cd$year[ii]),year_max=max(cd$year[ii]),
    PC1_mean=mean(cm$scores[ii,1]),PC1_sd=sd(cm$scores[ii,1]),PC2_mean=mean(cm$scores[ii,2]),PC2_sd=sd(cm$scores[ii,2]))
}))
district_center <- vapply(seq_len(ncol(cm$matrix)),function(j)ave(cm$matrix[,j],cd$district,FUN=mean),numeric(nrow(cd)))
tables$colony_within_district <- data.frame(within_variance_share=sum((cm$matrix-district_center)^2)/sum(cm$matrix^2),
  between_variance_share=sum(district_center^2)/sum(cm$matrix^2),n=nrow(cd))
extreme <- if("any_outlier"%in%names(cd))cd$any_outlier else rep(FALSE,nrow(cd))
for(v in intersect(c("horses","cattle","sheep","wheat_reaped"),names(cd))) {
  ceiling <- switch(v,horses=1000,cattle=5000,sheep=20000,wheat_reaped=10000)
  extreme <- extreme|cd[[v]]>ceiling
}
if(any(extreme)&&sum(!extreme)>ncol(cm$matrix)) {
  em <- fit_pca(cd[!extreme,,drop=FALSE],varsets$colony)
  tables$colony_extremes <- cbind(fit_row("exclude_unverified_ceiling_flags",em),excluded_n=sum(extreme),
    as.data.frame(as.list(subspace(cm$rotation,em$rotation))))
} else tables$colony_extremes <- data.frame(specification="no_eligible_ceiling_flags",excluded_n=sum(extreme))

# Examples span low, middle and high distance from the transformed centre.
ord <- order(rowSums(baseline$matrix^2)); example <- ord[pmax(1,round(c(.1,.5,.9)*length(ord)))]
zhat <- baseline$scores[example,1:2,drop=FALSE]%*%t(baseline$rotation[,1:2,drop=FALSE])
pred_transformed <- sweep(sweep(zhat,2,baseline$scale,"*"),2,baseline$center,"+")
tables$calibration <- do.call(rbind,lapply(seq_along(example),function(i)
  data.frame(example=i,row_index=example[i],variable=vars,recorded=as.numeric(D[example[i],vars]),
    transformed=baseline$transformed[example[i],],standardised=baseline$matrix[example[i],],
    reconstructed_standardised=zhat[i,],reconstructed_transformed=pred_transformed[i,],
    reconstructed_source=expm1(pred_transformed[i,]),PC1=baseline$scores[example[i],1],PC2=baseline$scores[example[i],2])))
tables$results_ledger <- data.frame(key=c("D_n","D_households","D_fit_2D","D_variables","cluster_k","cluster_r",
  "cluster_bootstrap_n","pca_bootstrap_n","theil_max_additivity_error","bridge_n","bridge_fit_2D",
  "colony_n","colony_fit_2D","colony_within_district_share","unit_equivalence_error","full_pc_distance_error"),
  value=c(nrow(D),length(unique(D$hhid)),sum(baseline$variance[1:2]),length(vars),clusters$k,clusters$r,nc,nb,
    max(abs(tables$theil_grouping$additivity_error),na.rm=TRUE),nrow(bd),sum(bm$variance[1:2]),nrow(cd),
    sum(cm$variance[1:2]),tables$colony_within_district$within_variance_share,eqerr,full_pc_distance_error),source="revision_results.rds")
for(nm in names(tables)) {
  rownames(tables[[nm]]) <- NULL
  write.csv(tables[[nm]],file.path("output/tables",paste0(nm,".csv")),row.names=FALSE,na="")
}
compact <- function(mm)mm[c("rotation","center","scale","eigenvalues","variance","variables","transform",
  "offset","unit","standardise","variable_fit")]
results <- list(pca=pca,tables=tables,clusters=clusters,cva=cva,nonlinear=nonlinear,
  transformation_models=lapply(spec_models,compact),weighted_models=lapply(weighted_models,compact),
  blank_assumption_models=lapply(blank_models,compact),
  temporal_samples=temporal_samples,bootstrap=list(angles=angles,axis_angles=axis_angles,subspace=subs),
  excluded_asset_assignments=excluded_assignments,config=config,
  design=list(seed=seed,loading_resamples=nb,clustering_resamples=nc,
    clustering_rank="full numerical rank; projected dimensions sensitivity only",
    clustering_selection="k=2:8; 1500 search observations; 750 silhouette observations; 10 starts; Lloyd; smallest k within 1e-10; full-data final refit",
    bootstrap_unit="household histories; scaling, PCA, numerical rank and k selection refit",
    baseline="standardised log1p in recorded units; at least one observed active economic component; covered partial blanks conditionally zero; wholly blank returns excluded",
    nonlinear="2000 fixed observations; 15-neighbour overlap; t-SNE perplexity15/40,1000 iterations,theta.5; UMAP neighbours15/40,min_dist.1,300epochs; two seeds; single thread; conditional RBF kernel PCA median-distance bandwidth times0.5/1/2",
    theil="unshifted nonnegative holdings; zeros retained; all-zero T undefined; missing disallowed",
    temporal_region="full hull; central90percent closest to period centroid; equal-size sample diagnostic",
    weights="household-years; equal-household and equal-year sensitivity; population-normalised covariance"))
saveRDS(results,"data/analysis/revision_results.rds",compress="gzip")
cat("02_analysis.R complete; analytical identities passed.\n")

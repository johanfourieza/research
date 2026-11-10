# Reusable annual nominal-price model. Gaussian common-plus-relative random walks.
# Static source/regime offsets have an explicit N(0, offset_sd^2) prior. Their
# levels in disconnected source periods are assumptions, not identified estimates.
suppressPackageStartupMessages({library(tidyverse);library(here);library(KFAS)})

prepare_price_panel <- function(ledger, broad_grain=FALSE, official=TRUE, fixed_offsets=FALSE,
                               purchases=FALSE,pooled_roles=FALSE,flat_discount=FALSE) {
  z <- ledger |> filter(included | (broad_grain & variant_use=="broad_grain_variant"))
  if(!official) z <- z |> filter(!str_detect(source_role,"VOC|government|official_statistics"))
  if(fixed_offsets) z$model_source_id <- paste(z$source_id,z$source_role,sep=" :: role=")
  if(pooled_roles) z$model_source_id <- str_remove(z$model_source_id," :: role=.*$")
  # A purchase and resale quote from the same company account are not independent
  # market observations. Retain sales in the primary fit; both remain in the ledger.
  if(!purchases) z <- z |> filter(!source_role%in%c("VOC_purchase","government_purchase"))
  panel <- z |> group_by(commodity,model_source_id,year) |>
    summarise(log_price=mean(log(price_rd_standard)),precision_units=sum(coalesce(n_documents,1)),
      n_documents=sum(n_documents,na.rm=TRUE),
      n_lots=sum(n_lots,na.rm=TRUE),source_role=first(source_role),
      conversion_assumption=any(conversion_assumption),n_input_rows=n(),.groups="drop") |>
    mutate(noise_role=case_when(str_detect(source_role,"auction")~"auction",
      str_detect(source_role,"VOC|government|official_statistics")~"official",TRUE~"other"))
  if(flat_discount) panel <- panel |> group_by(model_source_id) |> arrange(year,.by_group=TRUE) |>
    mutate(run=cumsum(replace_na(year-lag(year)!=1|abs(log_price-lag(log_price))>1e-10,TRUE))) |>
    group_by(model_source_id,run) |> mutate(precision_units=if_else(noise_role=="official",precision_units/n(),precision_units)) |>
    ungroup() |> select(-run)
  panel |> arrange(commodity,model_source_id,year)
}

fit_price_panel <- function(panel,offset_sd=.5,holdout=NULL,independent=FALSE,
                            wheat_specific=TRUE,role_noise=TRUE,robust_auction=FALSE,transitory_wheat=TRUE,
                            shared_offset_sd=0,shared_offset_mean=0,shared_start=1795L,shared_loading_override=NULL,
                            official_ar_sd=0,official_ar_rho=.8,fixed_parameters=NULL,
                            inits=log(c(.05,.08,.2,.45,.15,.3,.2)),maxit=150) {
  years <- 1700:1834
  commodities <- sort(unique(panel$commodity)); K <- length(commodities)
  channels <- panel |> group_by(model_source_id,commodity) |>
    summarise(source_role=first(source_role),noise_role=first(noise_role),first_year=min(year),last_year=max(year),.groups="drop") |> arrange(commodity,model_source_id)
  stopifnot(!anyDuplicated(channels$model_source_id),length(inits)==7)
  J <- nrow(channels)
  ar_channels <- if(official_ar_sd>0) which(channels$noise_role=="official") else integer()
  A <- length(ar_channels); W <- K+1L; D <- W+A; M <- D+J
  stopifnot(official_ar_sd>=0,abs(official_ar_rho)<1,shared_offset_sd>=0)
  Y <- matrix(NA_real_,length(years),J,dimnames=list(years,channels$model_source_id))
  count <- matrix(1,length(years),J)
  ii <- cbind(match(panel$year,years),match(panel$model_source_id,channels$model_source_id))
  stopifnot(!anyDuplicated(paste(ii[,1],ii[,2])))
  Y[ii] <- panel$log_price;count[ii] <- pmin(5,panel$precision_units)
  stopifnot(all(colSums(!is.na(Y))>0),all(count>0))
  if(!is.null(holdout)) Y[cbind(match(holdout$year,years),match(holdout$model_source_id,channels$model_source_id))] <- NA_real_
  Z <- matrix(0,J,M)
  Z[cbind(seq_len(J),match(channels$commodity,commodities))] <- 1
  Z[channels$commodity=="Wheat",W] <- 1
  if(A) Z[cbind(ar_channels,W+seq_len(A))] <- 1
  Z[cbind(seq_len(J),D+seq_len(J))] <- 1
  TT <- diag(M);TT[W,W] <- 0
  if(A) TT[cbind(W+seq_len(A),W+seq_len(A))] <- official_ar_rho
  RR <- rbind(diag(D),matrix(0,J,D))
  QQ <- diag(D);PP <- diag(c(rep(0,K),.04,rep(official_ar_sd^2,A),rep(offset_sd^2,J)))
  PI <- diag(c(rep(1,K),rep(0,J+1L+A)))
  # One common wedge is allowed among sources first observed after the transition.
  # Stable channels spanning it remain outside this loading.
  shared_loading <- as.numeric(channels$first_year>=shared_start)
  if(!is.null(shared_loading_override)) {
    stopifnot(!is.null(names(shared_loading_override)),all(channels$model_source_id%in%names(shared_loading_override)))
    shared_loading <- unname(shared_loading_override[channels$model_source_id])
    stopifnot(all(is.finite(shared_loading)))
  }
  offset_pos <- D+seq_len(J)
  PP[offset_pos,offset_pos] <- diag(offset_sd^2,J)+shared_offset_sd^2*tcrossprod(shared_loading)
  initial_mean <- rep(0,M);initial_mean[offset_pos] <- shared_offset_mean*shared_loading
  HH <- array(0,c(J,J,length(years)))
  for(t in seq_along(years)) diag(HH[,,t]) <- 1/count[t,]
  model <- SSModel(Y ~ -1 + SSMcustom(Z=Z,T=TT,R=RR,Q=QQ,P1=PP,P1inf=PI,
    a1=initial_mean,n=length(years),state_names=c(commodities,"wheat_transitory",if(A) paste0("official_ar_",ar_channels) else character(),paste0("offset_",seq_len(J)))),H=HH)
  role_index <- match(channels$noise_role,c("auction","official","other"))+3L
  wheat <- match("Wheat",commodities)
  variance_inflation <- matrix(1,length(years),J)
  update <- function(pars,model) {
    s <- exp(pars)
    model$Q[,,1] <- 0
    model$Q[seq_len(K),seq_len(K),1] <- (if(independent) 0 else s[1]^2)*matrix(1,K,K)+diag(s[2]^2,K)
    if(wheat_specific&&!is.na(wheat)) model$Q[wheat,wheat,1] <- model$Q[wheat,wheat,1]+s[3]^2
    model$Q[W,W,1] <- if(transitory_wheat) s[7]^2 else 0
    model$P1[W,W] <- if(transitory_wheat) s[7]^2 else 0
    if(A) model$Q[cbind(W+seq_len(A),W+seq_len(A),rep(1L,A))] <- official_ar_sd^2*(1-official_ar_rho^2)
    for(t in seq_along(years)) diag(model$H[,,t]) <- s[if(role_noise) role_index else rep(6L,J)]^2/count[t,]*variance_inflation[t,]
    model
  }
  optimise <- function(model,start) {
    if(!is.null(fixed_parameters)) {
      fitted <- update(fixed_parameters,model)
      return(list(model=fitted,optim.out=list(par=fixed_parameters,value=-as.numeric(logLik(fitted)),convergence=0L,message="parameters_fixed_by_design")))
    }
    fitSSM(model,inits=start,updatefn=update,method="L-BFGS-B",
      lower=log(c(.001,.005,.005,.01,.01,.01,.005)),upper=log(c(.8,.8,1.5,1.5,1.5,1.5,1.5)),control=list(maxit=maxit))
  }
  opt <- optimise(model,inits)
  smooth <- KFS(opt$model,filtering="state",smoothing="state")
  if(robust_auction) for(iter in 1:3) {
    residual <- Y-smooth$alphahat%*%t(Z)
    sigma <- exp(opt$optim.out$par[4])/sqrt(count)
    infl <- matrix(pmin(25,pmax(1,(residual/(2.5*sigma))^2)),nrow=length(years),ncol=J)
    infl[is.na(infl)] <- 1
    infl[,channels$noise_role!="auction"] <- 1
    variance_inflation <- infl
    opt <- optimise(opt$model,opt$optim.out$par)
    smooth <- KFS(opt$model,filtering="state",smoothing="state")
  }
  price_map <- matrix(0,K,M);price_map[,seq_len(K)] <- diag(K)
  if(!is.na(wheat)) price_map[wheat,W] <- 1
  list(model=opt$model,smooth=smooth,optim=opt$optim.out,years=years,commodities=commodities,
       channels=channels,panel=panel,offset_sd=offset_sd,independent=independent,count=count,K=K,
       wheat_specific=wheat_specific,role_noise=role_noise,robust_auction=robust_auction,variance_inflation=variance_inflation,
       transitory_wheat=transitory_wheat,n_dynamic=D,price_map=price_map,
       shared_offset_sd=shared_offset_sd,shared_offset_mean=shared_offset_mean,shared_loading=shared_loading,
       official_ar_sd=official_ar_sd,official_ar_rho=official_ar_rho,ar_channels=ar_channels,
       fixed_parameters=!is.null(fixed_parameters))
}

price_fit_locations <- function(fit) fit$smooth$alphahat%*%t(fit$price_map)

summarise_price_fit <- function(fit) {
  K <- fit$K; mu <- price_fit_locations(fit)
  se <- sapply(seq_len(K),function(k) sapply(seq_along(fit$years),function(t) {
    l <- fit$price_map[k,];sqrt(max(0,as.numeric(t(l)%*%fit$smooth$V[,,t]%*%l)))
  }))
  expand_grid(commodity=fit$commodities,year=fit$years) |>
    mutate(log_price=as.vector(mu),log_price_se=as.vector(se),price_median=exp(log_price),
           price_q05=exp(log_price-qnorm(.95)*log_price_se),price_q95=exp(log_price+qnorm(.95)*log_price_se))
}

draw_price_fit <- function(fit,nsim=1000,seed=20261108) {
  set.seed(seed)
  z <- simulateSSM(fit$model,type="states",nsim=nsim,conditional=TRUE)
  answer <- array(NA_real_,c(length(fit$years),fit$K,nsim))
  for(i in seq_len(nsim)) answer[,,i] <- z[,,i]%*%t(fit$price_map)
  answer
}

if(sys.nframe()==0L) {
  out <- here::here("replication","work","analysis")
  ledger <- read_csv(file.path(out,"nominal_observation_ledger.csv"),show_col_types=FALSE,guess_max=Inf)
  panel <- prepare_price_panel(ledger)
  fit <- fit_price_panel(panel)
  saveRDS(fit,file.path(out,"annual_state_model.rds"))
  write_csv(panel,file.path(out,"annual_model_panel.csv"))
  write_csv(summarise_price_fit(fit),file.path(out,"elementary_annual_state.csv"))
  off <- fit$channels |> mutate(offset=as.numeric(fit$smooth$alphahat[1,fit$n_dynamic+seq_len(n())]),
    offset_sd=sqrt(pmax(0,diag(fit$smooth$V[,,1])[fit$n_dynamic+seq_len(n())])),prior_sd=fit$offset_sd)
  write_csv(off,file.path(out,"source_offset_estimates.csv"))
  pars <- tibble(parameter=c("common_innovation_sd","commodity_innovation_sd","wheat_extra_innovation_sd","auction_observation_sd","official_observation_sd","other_observation_sd","wheat_transitory_sd"),
     estimate=exp(fit$optim$par),convergence=fit$optim$convergence,negative_log_likelihood=fit$optim$value)
  write_csv(pars,file.path(out,"state_model_parameters.csv"))
  print(pars)
  # Conditional simulation intervals exclude parameter and conversion uncertainty.
  draws <- draw_price_fit(fit,1000)
  saveRDS(list(log_price=draws,year=fit$years,commodity=fit$commodities,seed=20261108),
    file.path(out,"elementary_annual_draws.rds"))
  stopifnot(fit$optim$convergence==0,all(is.finite(draws)),dim(draws)[1]==135)
}

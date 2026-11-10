fill_one<-function(z) {
 y<-z$mean_price_rd_per_head;observed<-!is.na(y)
 runs<-rle(!observed);end<-cumsum(runs$lengths);start<-end-runs$lengths+1
 gap_weights<-tibble(k=runs$lengths,start=start,end=end,missing=runs$values) |>
  filter(missing,start>1,end<length(y),k<=5) |> count(k,name="gaps") |> mutate(weight=k*gaps)
 cv<-map_dfr(1:5,function(k)map_dfr(seq.int(2,length(y)-k),function(i){
  ix<-i:(i+k-1);ends<-c(i-1,i+k);if(anyNA(y[c(ix,ends)]))return(tibble())
  w<-seq_len(k)/(k+1)
  tibble(k=k,start_year=z$year[i],year=z$year[ix],actual=y[ix],linear=(1-w)*y[ends[1]]+w*y[ends[2]],log_linear=exp((1-w)*log(y[ends[1]])+w*log(y[ends[2]])))
 })) |> pivot_longer(c(linear,log_linear),names_to="method",values_to="prediction")
 score<-cv |> group_by(k,method) |> summarise(n=n(),rmse_log=sqrt(mean(log(prediction/actual)^2)),.groups="drop")
 select_score<-score |> inner_join(gap_weights,by="k") |> group_by(method) |> summarise(gap_weighted_rmse_log=sqrt(weighted.mean(rmse_log^2,weight)),.groups="drop") |> arrange(gap_weighted_rmse_log)
 chosen<-if(nrow(select_score))select_score$method[1] else "log_linear"
 write_csv(cv,file.path(out,paste0(z$category[1],"_imputation_holdouts.csv")))
 write_csv(score,file.path(out,paste0(z$category[1],"_imputation_validation.csv")))
 write_csv(select_score,file.path(out,paste0(z$category[1],"_imputation_selection.csv")))
 z$price_observed_rd<-y;z$price_completed_rd<-y;z$alternative_interpolation_rd<-y
 z$is_imputed<-FALSE;z$gap_length<-0L;z$left_anchor_year<-NA_integer_;z$right_anchor_year<-NA_integer_
 z$method<-if_else(observed,"observed_lot_mean","unfilled_no_extrapolation")
 for(i in which(!observed)) {
  l<-which(observed & seq_along(y)<i);r<-which(observed & seq_along(y)>i)
  if(!length(l)||!length(r))next
  l<-max(l);r<-min(r);gap<-r-l-1;w<-(i-l)/(r-l)
  z$gap_length[i]<-gap;z$left_anchor_year[i]<-z$year[l];z$right_anchor_year[i]<-z$year[r]
  if(gap>5){z$method[i]<-"unfilled_gap_exceeds_five_years";next}
  lin<-(1-w)*y[l]+w*y[r];lg<-exp((1-w)*log(y[l])+w*log(y[r]))
  z$price_completed_rd[i]<-if(chosen=="linear")lin else lg
  z$alternative_interpolation_rd[i]<-if(chosen=="linear")lg else lin
  z$is_imputed[i]<-TRUE;z$method[i]<-paste0(chosen,"_interpolation")
 }
 stopifnot(identical(z$price_completed_rd[observed],y[observed]))
 z
}

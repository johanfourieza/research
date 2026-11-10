source(here::here("replication","R","helpers.R"))
ledger <- v2_read("nominal_observation_ledger.csv")
fit <- readRDS(file.path(v2_out,"annual_state_model.rds"))
kept <- ledger |> filter(included,!source_role%in%c("VOC_purchase","government_purchase"))
base <- kept |> group_by(source_id) |> summarise(base_has_pre1795=any(year<1795),base_has_post1795=any(year>=1795),.groups="drop")
map <- kept |> group_by(model_source_id,source_id,source_role) |>
 summarise(first_year=min(year),last_year=max(year),n_inputs=n(),.groups="drop") |>
 left_join(base,by="source_id") |>
 mutate(family=case_when(str_starts(source_id,"project_auction")~"project_auction",
  str_starts(source_id,"native_MOOC")~"MOOC_transcribed",str_detect(source_id,"Neumark")~"Neumark",
  str_detect(source_id,"Theal")~"Theal",source_role=="newspaper"~"newspaper",
  str_detect(source_id,"Blue Books|BlueBook")~"Blue_Books",str_detect(source_id,"Cape Archives|Cape Archive")~"Cape_Archives",
  str_detect(source_id,"VOC")~"VOC",TRUE~"other_source_column"),
  stable_1795_bridge=first_year<1795&last_year>=1795,
  post_continuation=first_year>=1795&base_has_pre1795,
  g_all=as.numeric(first_year>=1795&!stable_1795_bridge),
  g_new=as.numeric(g_all==1&!post_continuation),
  classification_basis=case_when(stable_1795_bridge~"same_model_channel_observed_before_and_after1795",
   post_continuation~"post1795_channel_from_source_column_also_observed_before1795",
   first_year>=1795~"post1795_source_column_no_pre1795_row_in_retained_ledger",
   TRUE~"pre1795_channel"),
  scope="compiled_source_column_and_role_not_verified_archival_document_identity") |> arrange(family,source_id,first_year,source_role)
stopifnot(!anyDuplicated(map$model_source_id),setequal(map$model_source_id,fit$channels$model_source_id),all(map$g_new<=map$g_all))
v2_write(map,"channel_family_loading_map.csv")
v2_write(map |> count(family,g_all,g_new,post_continuation,name="channels"),"channel_loading_counts.csv")
print(map |> filter(g_all==1) |> select(model_source_id,family,g_all,g_new,post_continuation),n=Inf,width=180)

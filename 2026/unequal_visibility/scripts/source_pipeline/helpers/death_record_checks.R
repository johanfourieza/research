# Source and identity exclusions established in the 8 September 2026 audit.
# These repairs are not a claim that all remaining candidate links are adjudicated.
# See revision/audit_2026-09-08/results/probate_source_excerpts.txt and AUDIT.md.
head_death_inventories <- function(inv) {
  # The first named person is the surviving husband in these three documents.
  exclude <- (inv$source_file == 'MOOC8_3.01-107_v3.1.00.xml' &
              inv$div_id == 'MOOC8/3.3' & inv$date_value == '17141231') |
    (inv$source_file == 'MOOC8_2.01-122_v3.1.00.xml' &
     inv$div_id %in% c('MOOC8/2.111 1/2','MOOC8/2.107') &
     inv$date_value %in% c('17140217','17130815'))
  inv[!exclude]
}

death_record_match <- function(sur, key, candidate_sur, candidate_key, channel) {
  threshold <- if (channel == 'probate') .15 else .12
  accepted <- stringdist(sur,candidate_sur,method='jw',p=.1)<=.15 &
    stringdist(key,candidate_key,method='jw',p=.1)<=threshold
  # Claas is explicitly Isaac's brother in MOOC8/3.50. The widow records name
  # Gerrit Cloete and Maurits van Staden, not Gerrit Coetzee and Matthys van Staden.
  forbidden <- (key=='claas elbertsz' & candidate_key=='isaac elbertsz') |
    (key=='gerrit coetzee' & candidate_key=='gerrit cloete') |
    (key=='matthys van staden' & candidate_key=='maurits van staden')
  accepted & !forbidden
}

checked_death_flags <- function(ar, probate, widows) {
  probate <- head_death_inventories(probate)
  ar[, probate_flag := vapply(seq_len(.N), function(i)
    any(death_record_match(ar$h_sur[i], ar$h_key[i], probate$i_sur,
                           probate$i_key, 'probate')), logical(1))]
  ar[, widow_flag := vapply(seq_len(.N), function(i)
    any(death_record_match(ar$h_sur[i], ar$h_key[i], widows$wof_sur,
                           widows$wof_key, 'widow')), logical(1))]
  ar[, any_flag := probate_flag | widow_flag]
  ar[]
}

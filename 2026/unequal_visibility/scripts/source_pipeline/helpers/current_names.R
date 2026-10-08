# Current name parser. Historical helper remains unchanged for archived analyses.
# Requires R/00_setup.R (standardize_names and data.table).
current_names <- function(x) {
  raw <- as.character(x)
  clean <- tolower(trimws(gsub('[[:space:]]+', ' ', raw)))
  # Observed transcription split, hhobs 5717 in the 1712 roll; do not apply
  # a general initial-expansion rule to unrelated names.
  clean[!is.na(clean)&clean=='c asper gerritsz'] <- 'casper gerritsz'
  qualifier <- rep('',length(clean))
  # A terminal comma may introduce a generational/patronymic qualifier rather
  # than separate a surname from given names. Preserve that information.
  suffix_pattern <- '[,.]\\s*(de jonge|de oude|junior|senior|[[:alpha:]]+szoon|[[:alpha:]]+zoon|[[:alpha:]]+soon)\\s*$'
  has_suffix <- !is.na(clean) & grepl(suffix_pattern,clean)
  qualifier[has_suffix] <- trimws(sub('^.*[,.]','',clean[has_suffix]))
  clean[has_suffix] <- trimws(sub(suffix_pattern,'',clean[has_suffix]))
  clean <- gsub('[.:;?]', '', clean)
  qualifier[qualifier=='junior'] <- 'de jonge'
  qualifier[qualifier=='senior'] <- 'de oude'
  parsed <- standardize_names(clean)
  first <- parsed$firstname_std
  surname <- parsed$surname_std
  valid <- !is.na(raw)&nzchar(trimws(raw))&!is.na(first)&!is.na(surname)
  core <- trimws(paste(first,surname))
  core[!valid] <- NA_character_
  key <- ifelse(nzchar(qualifier),paste(core,qualifier,sep=' | '),core)
  ans <- data.table(raw=raw,first=first,first_token=sub('\\s.*$','',first),
             surname=surname,qualifier=qualifier,core_key=core,valid=valid)
  ans[,key:=key]
  ans
}

current_qualifier_compatible <- function(a,b) {
  # An absent qualifier is unspecified; two explicit unequal qualifiers conflict.
  !is.na(a)&!is.na(b)&(a==''|b==''|a==b)
}

current_name_match <- function(a,b,token_threshold=.10,key_threshold=.12) {
  # a is one row; b is any number of rows from current_names().
  stopifnot(nrow(a)==1L)
  ans <- a$valid & b$valid &
    stringdist::stringdist(surname_token(a$surname),surname_token(b$surname),method='jw',p=.1)<=token_threshold &
    stringdist::stringdist(a$core_key,b$core_key,method='jw',p=.1)<=key_threshold &
    current_qualifier_compatible(a$qualifier,b$qualifier)
  ans[is.na(ans)] <- FALSE
  ans
}

check_current_names <- function() {
  n<-current_names(c('Jacob Pinar, de jonge','Jacob Pinar','Gidion Malherbe, junior',
    'Dirk Coetzee, Janszoon','Kruijsman, Arnoldus','Hans Jurgen Potgieter',
    'Hans Harmen Potgieter','Pieter Lombart, de oude','Pietersz:, Andries',NA,''))
  stopifnot(n$key[1]=='jacob pinar | de jonge',n$key[2]=='jacob pinar',
    n$key[3]=='gidion malherbe | de jonge',n$key[4]=='dirk coetzee | janszoon',
    n$key[5]=='arnoldus kruijsman',n$key[6]!=n$key[7],
    n$key[8]=='pieter lombart | de oude',n$surname[9]=='pietersen',
    !n$valid[10],!n$valid[11],
    !current_qualifier_compatible('de jonge','de oude'))
  invisible(TRUE)
}

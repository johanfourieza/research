# Convert the editable Markdown manuscript to the existing journal layout.
# Source this file from HM_revision, then call render_manuscript_markdown().
# Pandoc handles Markdown prose and citations; raw LaTeX blocks retain the
# original mathematics, figure layout and generated table calls.

render_manuscript_markdown <- function(markdown, layout, output) {
  pandoc <- Sys.which('pandoc')
  if(!nzchar(pandoc))stop('Pandoc is required to build the Markdown manuscript.')
  stopifnot(file.exists(markdown),file.exists(layout))
  z <- paste(readLines(markdown,encoding='UTF-8',warn=FALSE),collapse='\n')
  z <- gsub('(?s)<!--.*?-->','',z,perl=TRUE)
  template <- readLines(layout,encoding='UTF-8',warn=FALSE)
  marker <- which(template=='%% MANUSCRIPT_BODY %%')
  stopifnot(length(marker)==1L)
  # Recognise only declared result tokens, so typos cannot silently print.
  definitions <- readLines('output/tables/manuscript_values.tex',warn=FALSE)
  definitions <- definitions[startsWith(definitions,'\\newcommand{')]
  names <- sub(r"(^\\newcommand\{\\([^}]+)\}.*)",'\\1',definitions)
  matches <- function(pattern,s)regmatches(s,gregexpr(pattern,s,perl=TRUE))[[1]]
  tokens <- unique(matches(r"(\{\{[A-Za-z][A-Za-z0-9]*\}\})",z))
  for(token in tokens) {
    key <- substring(token,3,nchar(token)-2L)
    if(!key%in%names)stop('Unknown numerical result token: ',token)
    z <- gsub(token,paste0('\\',key,'{}'),z,fixed=TRUE)
  }
  # Protect inline math and TeX references from cosmetic rewriting by Pandoc.
  # The raw blocks are passed through separately and stay byte-for-byte intact.
  saved <- character()
  protect <- function(pattern,s) {
    hits <- gregexpr(pattern,s,perl=TRUE)
    values <- regmatches(s,hits)[[1]]
    if(!length(values))return(s)
    keys <- sprintf('HMKEEP%06dTOKEN',length(saved)+seq_along(values))
    saved <<- c(saved,setNames(values,keys))
    regmatches(s,hits) <- list(keys)
    s
  }
  z <- protect(r"((?ms)^```\{=(?:latex|tex)\}[^\n]*\n.*?^```[ \t]*$)",z)
  z <- protect(r"((?<![\\$])\$(?!\$)(?:\\.|[^$\n])*\$)",z)
  z <- protect(r"(\\(?:ref|eqref)\{[^}]+\})",z)
  for(key in names)z <- protect(paste0('\\\\',key,'\\{\\}'),z)
  dir.create('.build/markdown',recursive=TRUE,showWarnings=FALSE)
  input <- tempfile('body_',tmpdir='.build/markdown',fileext='.md')
  body_file <- tempfile('body_',tmpdir='.build/markdown',fileext='.tex')
  on.exit(unlink(c(input,body_file)),add=TRUE)
  writeLines(z,input,useBytes=TRUE)
  status <- system2(pandoc,c('--from=markdown-smart','--to=latex','--natbib',
    '--wrap=none',shQuote(input),'--output',shQuote(body_file)),
    stdout='.build/markdown/pandoc.out.log',stderr='.build/markdown/pandoc.err.log')
  if(status!=0L)stop('Markdown conversion failed; see .build/markdown/pandoc.err.log')
  body <- paste(readLines(body_file,encoding='UTF-8',warn=FALSE),collapse='\n')
  # Block placeholders were plain paragraphs; restore their raw contents.
  for(key in names(saved)) {
    value <- saved[[key]]
    if(startsWith(value,'```')) {
      value <- sub(r"(^```\{=(?:latex|tex)\}[^\n]*\n)",'',value,perl=TRUE)
      value <- sub(r"(\n```[ \t]*$)",'',value,perl=TRUE)
    }
    body <- gsub(key,value,body,fixed=TRUE)
  }
  if(grepl('HMKEEP[0-9]+TOKEN',body))stop('Unresolved conversion placeholder.')
  # Display mathematics continues its surrounding paragraph in this manuscript.
  body <- gsub('\n\n\\\\begin\\{equation\\}','\n\\\\begin{equation}',body)
  body <- gsub('\\\\end\\{equation\\}\n\n','\\\\end{equation}\n',body)
  # Retain the compact citation syntax expected by the existing audit.
  hits <- gregexpr(r"(\\cite[pt]?\{[^}]+\})",body,perl=TRUE)
  cites <- regmatches(body,hits)[[1]]
  regmatches(body,hits) <- list(gsub(',[[:space:]]+',',',cites))
  # Ordinary Markdown links are handled by hyperref; lists use tightlist.
  body <- strsplit(body,'\n',fixed=TRUE)[[1]]
  result <- c(template[seq_len(marker-1L)],body,template[seq.int(marker+1L,length(template))])
  writeLines(result,output,useBytes=TRUE)
  invisible(output)
}

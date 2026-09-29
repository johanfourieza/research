# Fresh-process replication. --verify performs two clean analytical builds.
# --analysis-only omits documents; --documents-only compiles existing outputs.
args<-commandArgs(trailingOnly=TRUE)
script<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
root<-normalizePath(file.path(dirname(script),".."),winslash="/");setwd(root)
# A declared version selects Markdown when present; TeX is then generated.
active_file<-"paper/active_manuscript.txt"
active_stem<-if(file.exists(active_file))trimws(readLines(active_file,warn=FALSE)[1])else "manuscript"
stopifnot(length(active_stem)==1L,!is.na(active_stem),grepl("^manuscript(_v[0-9]+)?$",active_stem))
active_source<-file.path("paper",paste0(active_stem,".tex"))
markdown_source<-file.path("paper",paste0(active_stem,".md"))
markdown_layout<-"paper/manuscript_layout.tex"
markdown_builder<-"code/manuscript_markdown.R"
if(file.exists(markdown_source)) {
  source(markdown_builder)
  render_manuscript_markdown(markdown_source,markdown_layout,active_source)
}
editing_source<-if(file.exists(markdown_source))markdown_source else active_source
stopifnot(file.exists(active_source))
if(active_stem!="manuscript")stopifnot(file.copy(active_source,"paper/manuscript.tex",overwrite=TRUE))
if(dir.exists("library")) .libPaths(c(normalizePath("library"),.libPaths()))
Sys.setenv(R_LIBS=paste(.libPaths(),collapse=.Platform$path.sep),LC_ALL="C",OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1")
required<-c("tidyverse","readxl","here","digest","cluster","biplotEZ","FNN","Rtsne","uwot","ggplot2","ggrepel","patchwork","gifski","pdftools")
missing<-required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop("Missing dependencies: ",paste(missing,collapse=", "))
run_id<-format(Sys.time(),"%Y%m%d_%H%M%S");run_dir<-file.path(root,"docs/execution",run_id)
dir.create(run_dir,recursive=TRUE);dir.create(".build",showWarnings=FALSE)
rscript<-file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
sha<-function(f)vapply(f,digest::digest,character(1),file=TRUE,algo="sha256")
raw_files<-list.files("data/raw",full.names=TRUE);raw_hashes<-sha(raw_files)
production_scripts<-file.path("code",c("01_clean.R","02_analysis.R","03_figures.R"))
code_hashes<-sha(production_scripts)
frozen_code<-file.path(root,".build/frozen_code",run_id)
dir.create(frozen_code,recursive=TRUE)
stopifnot(all(file.copy(production_scripts,frozen_code)))
write.csv(data.frame(file=production_scripts,sha256=code_hashes),file.path(run_dir,"executed_code_manifest.csv"),row.names=FALSE)
timings<-list()
command<-function(exe,args,log,wd=root) {
  old<-getwd();on.exit(setwd(old));setwd(wd)
  status<-system2(exe,args,stdout=log,stderr=log)
  if(status!=0L)stop("Command failed (",status,"): ",log)
}
stage<-function(script,label) {
  cat("Running",label,"\n");t0<-Sys.time()
  command(rscript,c("--vanilla",shQuote(script)),file.path(run_dir,paste0(label,".log")))
  timings[[length(timings)+1L]]<<-data.frame(stage=label,seconds=as.numeric(difftime(Sys.time(),t0,units="secs")))
}
preserve<-function(label) {
  backup<-file.path(root,".build/previous",paste0(run_id,"_",label));dir.create(backup,recursive=TRUE)
  for(rel in c("data/analysis","output")) {
    target<-file.path(root,rel)
    if(dir.exists(target)) {
      target<-normalizePath(target,winslash="/",mustWork=TRUE)
      stopifnot(startsWith(tolower(target),paste0(tolower(root),"/")))
      destination<-file.path(backup,gsub("/","_",rel,fixed=TRUE))
      # Dropbox or a previewer can lock a directory even after all R devices
      # close. Preserve each generated file, verify its copy, then remove only
      # those files. No source directory is ever a candidate for this operation.
      if(!suppressWarnings(file.rename(target,destination))) {
        files<-list.files(target,recursive=TRUE,full.names=TRUE,all.files=TRUE)
        files<-files[!dir.exists(files)]
        for(f in files) {
          resolved<-normalizePath(f,winslash="/",mustWork=TRUE)
          stopifnot(startsWith(tolower(resolved),paste0(tolower(target),"/")))
          to<-file.path(destination,substring(resolved,nchar(target)+2))
          dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE)
          stopifnot(file.copy(resolved,to),identical(unname(sha(resolved)),unname(sha(to))))
          removed<-FALSE
          for(attempt in 1:10) {
            removed<-suppressWarnings(file.remove(resolved))
            if(removed)break
            Sys.sleep(.2)
          }
          if(!removed)stop("Generated file remains locked: ",resolved)
        }
      }
    }
    dir.create(file.path(root,rel),recursive=TRUE,showWarnings=FALSE)
  }
}
check<-function() {
  stopifnot(identical(raw_hashes,sha(raw_files)))
  ledger<-read.csv("output/tables/results_ledger.csv")
  stopifnot(all(is.finite(ledger$value)))
  inv<-read.csv("output/tables/figure_inventory.csv")
  figs<-list.files("output/figures",pattern="\\.(pdf|png)$",full.names=TRUE)
  stopifnot(nrow(inv)>0,length(figs)>=10,all(file.info(figs)$size>100),
    length(list.files("output/figures",pattern="\\.gif$"))==1,
    isTRUE(readRDS("output/figures/plot_metadata.rds")$theme$no_titles))
  th<-read.csv("output/tables/theil_grouping.csv")
  stopifnot(max(abs(th$additivity_error),na.rm=TRUE)<1e-10)
}
build<-function(label) {
  preserve(label)
  for(s in c("01_clean.R","02_analysis.R","03_figures.R"))stage(file.path(frozen_code,s),paste0(label,"_",s))
  check()
}
comparison<-NULL
if(!"--documents-only"%in%args) {
  build("first")
  if("--verify"%in%args) {
    ref<-file.path(root,".build/verification",run_id);dir.create(ref,recursive=TRUE)
    candidates<-c(list.files("output",pattern="\\.(csv|rds)$",full.names=TRUE,recursive=TRUE),list.files("data/analysis",pattern="\\.rds$",full.names=TRUE))
    for(f in candidates) {
      dest<-file.path(ref,f);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
      stopifnot(file.copy(f,dest))
    }
    build("second")
    second<-c(list.files("output",pattern="\\.(csv|rds)$",full.names=TRUE,recursive=TRUE),list.files("data/analysis",pattern="\\.rds$",full.names=TRUE))
    stopifnot(identical(candidates,second))
    comparison<-do.call(rbind,lapply(candidates,function(f) {
      read<-if(grepl("\\.csv$",f))function(p)read.csv(p,check.names=FALSE,stringsAsFactors=FALSE) else readRDS
      result<-all.equal(read(file.path(ref,f)),read(f),tolerance=1e-8,check.attributes=TRUE)
      data.frame(file=f,equal=isTRUE(result),sha256_equal=identical(unname(sha(file.path(ref,f))),unname(sha(f))),
        detail=if(isTRUE(result))"equal within 1e-8" else paste(result,collapse="; "))
    }))
    write.csv(comparison,file.path(run_dir,"replication_comparison.csv"),row.names=FALSE)
    if(!all(comparison$equal))stop("Clean repeat differs; inspect replication_comparison.csv")
    file.copy(file.path(run_dir,"replication_comparison.csv"),"docs/execution/replication_comparison.csv",overwrite=TRUE)
    cat(nrow(comparison),"CSV/RDS outputs agree across two clean builds.\n")
  }
} else check()
coherence_script<-"docs/verification/check_manuscript.R"
if(file.exists(coherence_script)&&file.exists(active_file))stage(coherence_script,"manuscript_coherence")
ip<-installed.packages(fields=c("Repository","RemoteType","RemoteHost","RemoteRepo","RemoteUsername","RemoteRef","RemoteSha"))
ip<-ip[!duplicated(ip[,"Package"]),,drop=FALSE]
write.csv(ip[,intersect(c("Package","Version","Built","Repository","RemoteType","RemoteHost","RemoteRepo","RemoteUsername","RemoteRef","RemoteSha"),colnames(ip)),drop=FALSE],"docs/execution/dependency_manifest.csv",row.names=FALSE,na="")
writeLines(capture.output(sessionInfo()),"docs/execution/session_info.txt")
configure_tex<-function() {
  if(.Platform$OS.type!="windows")return(invisible(NULL))
  exe<-Sys.which("pdflatex");if(!nzchar(exe))stop("pdflatex missing")
  installed<-normalizePath(file.path(dirname(exe),"../../.."),winslash="/")
  if(!dir.exists(file.path(installed,"miktex")))return(invisible(NULL))
  local<-file.path(root,".build/tex_environment")
  for(d in c("config","data","install/miktex/config"))dir.create(file.path(local,d),recursive=TRUE,showWarnings=FALSE)
  for(f in list.files(file.path(installed,"miktex/config"),pattern="^(package-manifests.ini|packages.ini|setup-.*\\.log)$",full.names=TRUE))
    if(!file.exists(file.path(local,"install/miktex/config",basename(f))))file.copy(f,file.path(local,"install/miktex/config",basename(f)))
  roots<-c(installed,file.path(Sys.getenv("APPDATA"),"MiKTeX"),file.path(Sys.getenv("LOCALAPPDATA"),"MiKTeX"));roots<-roots[dir.exists(roots)]
  Sys.setenv(MIKTEX_USERCONFIG=file.path(local,"config"),MIKTEX_USERDATA=file.path(local,"data"),MIKTEX_USERINSTALL=file.path(local,"install"),
    MIKTEX_USERROOTS=paste(roots,collapse=";"),MIKTEX_CORE_NOREGISTRY="true",MIKTEX_MPM_AUTOINSTALL="0")
  command(Sys.which("initexmf"),"--update-fndb",file.path(run_dir,"tex_initialisation.log"))
}
compile<-function(stem,folder,bib=FALSE) {
  folder<-normalizePath(folder,winslash="/")
  texargs<-c("-interaction=nonstopmode","-halt-on-error",shQuote(paste0(stem,".tex")))
  command(Sys.which("pdflatex"),texargs,file.path(run_dir,paste0(stem,"_tex1.log")),folder)
  if(bib)command(Sys.which("biber"),shQuote(stem),file.path(run_dir,paste0(stem,"_biber.log")),folder)
  for(i in 2:3)command(Sys.which("pdflatex"),texargs,file.path(run_dir,paste0(stem,"_tex",i,".log")),folder)
  log<-readLines(file.path(folder,paste0(stem,".log")),warn=FALSE)
  bad<-grep("undefined references|Citation .*undefined|Reference .*undefined|multiply defined",log,value=TRUE)
  if(length(bad))stop("Unresolved references in ",stem,": ",paste(bad,collapse="; "))
  stopifnot(file.info(file.path(folder,paste0(stem,".pdf")))$size>1000)
}
if(!"--analysis-only"%in%args) {
  configure_tex();compile("manuscript","paper",TRUE);compile("response_to_referees","referees")
  if(active_stem!="manuscript")stopifnot(file.copy("paper/manuscript.pdf",file.path("paper",paste0(active_stem,".pdf")),overwrite=TRUE))
  anonymise<-function(lines) {
    first<-grep("^\\\\author\\{",lines)[1];last<-grep("^\\\\date\\{",lines)[1]
    stopifnot(is.finite(first),is.finite(last),last>first)
    c(lines[seq_len(first-1)],"\\author{}",lines[seq.int(last,length(lines))])
  }
  clean_source<-readLines("paper/manuscript.tex",warn=FALSE,encoding="UTF-8")
  anonymous_source<-anonymise(clean_source)
  writeLines(anonymous_source,"paper/manuscript_anonymous.tex",useBytes=TRUE)
  compile("manuscript_anonymous","paper",TRUE)
  # The title page carries everything the anonymous manuscript must omit:
  # authors, affiliations, funding and declarations. The abstract, keywords
  # and JEL codes are copied from the manuscript so the two cannot diverge.
  abs_from<-grep("^\\\\begin\\{abstract\\}",clean_source)[1]
  jel_line<-grep("^\\\\noindent\\\\textbf\\{JEL codes:\\}",clean_source)[1]
  stopifnot(is.finite(abs_from),is.finite(jel_line),jel_line>abs_from)
  # The public replication package (code, aggregate results, documentation).
  # The URL identifies the authors, so it appears only on the title page.
  ai_statement<-"The authors used generative AI tools at all stages of this research, including data management, coding, hypothesis testing, and drafting and reviewing the manuscript. These tools were Claude Code (Anthropic; Claude Opus and Fable models) and OpenAI Codex (Sol and Astra models). Refine.ink provided an additional AI-assisted review of the manuscript. The authors reviewed and verified all AI-generated output and take full responsibility for the content of the final version."
  data_url<-"https://github.com/johanfourieza/research/tree/main/2026/biplots"
  data_statement<-function(url)paste0("The code, aggregate results and documentation that support the findings of this study are openly available at ",url,". Access to the underlying transcriptions requires permission from their custodians.")
  statements<-c("\\section*{Funding}",
    "This work was supported by the Riksbankens Jubileumsfond under the Cape of Good Hope Panel grant (M20-0041).",
    "\\section*{Disclosure statement}","The authors report there are no competing interests to declare.",
    "\\section*{Data availability statement}",
    data_statement(paste0("\\url{",data_url,"}")),
    "\\section*{Declaration of generative AI use}",
    ai_statement)
  title_source<-c(clean_source[seq_len(grep("^\\\\begin\\{document\\}",clean_source)[1]-1)],
    "\\begin{document}","\\maketitle",clean_source[abs_from:jel_line],statements,"\\end{document}")
  writeLines(title_source,"paper/title_page.tex",useBytes=TRUE);compile("title_page","paper")
  # Plain-text title page for the submission portal. The abstract is one
  # line with no internal line breaks; result macros are replaced by values.
  vals<-readLines("output/tables/manuscript_values.tex",warn=FALSE,encoding="UTF-8")
  vals<-regmatches(vals,regexec("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}\\{(.*)\\}$",vals))
  vals<-vals[lengths(vals)==3]
  plain<-function(x) {
    for(v in vals)x<-gsub(paste0("\\",v[2],"{}"),v[3],x,fixed=TRUE)
    x<-gsub("---","—",x,fixed=TRUE);x<-gsub("--","–",x,fixed=TRUE)
    x<-gsub("\\%","%",x,fixed=TRUE);x<-gsub("\\\\textit\\{([^}]*)\\}","\\1",x)
    x<-gsub("\\\\noindent\\\\textbf\\{([^}]*)\\}","\\1",x)
    trimws(gsub("\\s+"," ",x))
  }
  abstract_txt<-plain(paste(clean_source[(abs_from+1):(grep("^\\\\end\\{abstract\\}",clean_source)[1]-1)],collapse=" "))
  stopifnot(!grepl("\\\\",abstract_txt))
  title_txt<-c("Title: Biplots for historical household data: Evidence from Cape tax records","",
    "Authors:",
    "Johan Fourie (corresponding author), LEAP, Department of Economics, Stellenbosch University, johanf@sun.ac.za",
    "Sugnet Lubbe, MuViSU, Department of Statistics and Actuarial Science, Stellenbosch University; NITheCS",
    "Johané Nienkemper-Swanepoel, MuViSU, Department of Statistics and Actuarial Science, Stellenbosch University",
    "Dieter von Fintel, LEAP, Department of Economics, Stellenbosch University","",
    "Abstract:",abstract_txt,"",
    plain(grep("^\\\\noindent\\\\textbf\\{Keywords:\\}",clean_source,value=TRUE)),
    plain(clean_source[jel_line]),"",
    "Funding: This work was supported by the Riksbankens Jubileumsfond under the Cape of Good Hope Panel grant (M20-0041).",
    "Disclosure statement: The authors report there are no competing interests to declare.",
    paste("Data availability:",data_statement(data_url)),
    paste("Declaration of generative AI use:",ai_statement))
  con<-file("paper/title_page.txt",open="w",encoding="UTF-8");writeLines(title_txt,con);close(con)
  # Manuscript with author details for the journal: the clean manuscript plus
  # the funding and data-availability statements, which identify the authors
  # and are therefore withheld from the anonymous version.
  bib_at<-grep("\\\\printbibliography",clean_source)[1]
  stopifnot(is.finite(bib_at))
  author_source<-append(clean_source,c("\\section*{Funding}",statements[2],"",
    "\\section*{Data availability statement}",data_statement(paste0("\\url{",data_url,"}")),""),after=bib_at-1L)
  writeLines(author_source,"paper/manuscript_with_authors.tex",useBytes=TRUE)
  compile("manuscript_with_authors","paper",TRUE)
  perl<-Sys.which("perl")
  if(!nzchar(perl)&&file.exists("C:/Program Files/Git/usr/bin/perl.exe"))perl<-"C:/Program Files/Git/usr/bin/perl.exe"
  latexroot<-normalizePath(file.path(dirname(Sys.which("pdflatex")),"../../.."),winslash="/")
  ld<-file.path(latexroot,"scripts/latexdiff/latexdiff-so")
  if(!file.exists(ld))ld<-Sys.which("latexdiff")
  old<-file.path(root,"../archive/2026-06_HM_submission_R1/manuscript.tex")
  if(!file.exists(old))old<-file.path(root,"paper/submitted_manuscript.tex")
  if(!file.exists(old)||!nzchar(perl)||!file.exists(ld))stop("Marked manuscript requires submitted source and latexdiff/Perl")
  lines<-readLines(old,warn=FALSE,encoding="UTF-8")
  baseline<-file.path(root,"docs/baseline_before_execution_20260908/output")
  lines<-gsub("\\graphicspath{{./}}",paste0("\\graphicspath{{",baseline,"/figures/}}"),lines,fixed=TRUE)
  lines<-gsub("\\def\\input@path{{./}}",paste0("\\def\\input@path{{",baseline,"/tables/}}"),lines,fixed=TRUE)
  lines<-gsub("(\\\\includegraphics(\\[[^]]*\\])?\\{)([^}]+)(\\})",paste0("\\1",baseline,"/figures/\\3\\4"),lines,perl=TRUE)
  lines<-gsub("(\\\\input\\{)([^}]+)(\\})",paste0("\\1",baseline,"/tables/\\2\\3"),lines,perl=TRUE)
  oldnorm<-file.path(root,".build/submitted_paths.tex");writeLines(lines,oldnorm,useBytes=TRUE)
  old_bib_file<-file.path(dirname(old),"references.bib")
  if(!file.exists(old_bib_file))old_bib_file<-file.path(root,"paper/submitted_references.bib")
  stopifnot(file.copy(old_bib_file,file.path(root,".build/references.bib"),overwrite=TRUE))
  command(Sys.which("pdflatex"),c("-interaction=nonstopmode","-halt-on-error","submitted_paths.tex"),
    file.path(run_dir,"submitted_reference_labels.log"),file.path(root,".build"))
  oldaux<-readLines(file.path(root,".build/submitted_paths.aux"),warn=FALSE)
  oldaux<-oldaux[startsWith(oldaux,"\\newlabel{")]
  oldkeys<-sub("^\\\\newlabel\\{([^}]+)\\}.*","\\1",oldaux)
  oldnumbers<-sub("^\\\\newlabel\\{[^}]+\\}\\{\\{([^}]*)\\}.*","\\1",oldaux)
  # Deleted references retain submitted numbers without depending on labels
  # deliberately removed from the revised document and its external tables.
  for(i in seq_along(oldkeys)) {
    lines<-gsub(paste0("\\ref{",oldkeys[i],"}"),oldnumbers[i],lines,fixed=TRUE)
    lines<-gsub(paste0("\\eqref{",oldkeys[i],"}"),paste0("(",oldnumbers[i],")"),lines,fixed=TRUE)
  }
  writeLines(lines,oldnorm,useBytes=TRUE)
  # Deleted citations in the marked version still need their original entries.
  # Preserve revised entries for shared keys and add only retired cited keys.
  bib_entries<-function(f) {
    z<-readLines(f,warn=FALSE,encoding="UTF-8");starts<-grep("^@",z)
    if(!length(starts))return(list())
    ends<-c(starts[-1]-1,length(z))
    entries<-Map(function(a,b)z[a:b],starts,ends)
    names(entries)<-vapply(entries,function(e)sub("^@[^{]+\\{[[:space:]]*([^,]+),.*","\\1",e[1]),character(1))
    entries
  }
  current_bib<-bib_entries("paper/references.bib")
  old_bib_file<-file.path(dirname(old),"references.bib")
  if(!file.exists(old_bib_file))old_bib_file<-file.path(root,"paper/submitted_references.bib")
  old_bib<-bib_entries(old_bib_file)
  writeLines(unlist(c(current_bib,old_bib[setdiff(names(old_bib),names(current_bib))]),use.names=FALSE),"paper/references_marked.bib",useBytes=TRUE)
  use_marked_bib<-function(f) {
    z<-readLines(f,warn=FALSE,encoding="UTF-8")
    z<-gsub("\\addbibresource{references.bib}","\\addbibresource{references_marked.bib}",z,fixed=TRUE)
    z<-gsub("\\emergencystretch\\DIFadd{=2em}","\\emergencystretch=2em",z,fixed=TRUE)
    # CFONT's font-size switches are invalid inside deleted display equations.
    # Colour-only markup works in both text and maths and retains deletions.
    z[grepl("\\providecommand{\\DIFaddtex}",z,fixed=TRUE)]<-"\\providecommand{\\DIFaddtex}[1]{{\\protect\\color{blue} #1}}"
    z[grepl("\\providecommand{\\DIFdeltex}",z,fixed=TRUE)]<-"\\providecommand{\\DIFdeltex}[1]{{\\protect\\color{red} #1}}"
    in_math<-FALSE
    for(i in seq_along(z)) {
      if(grepl("\\\\begin\\{(displaymath|equation[*]?|align[*]?|gather[*]?|multline[*]?)\\}",z[i]))in_math<-TRUE
      if(in_math&&!nzchar(trimws(z[i])))z[i]<-"% Avoid a paragraph token in displayed mathematics."
      if(grepl("\\\\end\\{(displaymath|equation[*]?|align[*]?|gather[*]?|multline[*]?)\\}",z[i]))in_math<-FALSE
    }
    at<-grep("^\\\\begin\\{document\\}",z)[1]
    z<-append(z,"\\hypersetup{hypertexnames=false}",after=at-1L)
    at<-grep("^\\\\maketitle",z)[1]
    if(is.finite(at))z<-append(z,"\\par\\noindent{\\color{blue}Blue text is added.} {\\color{red}Red text is deleted.}\\par\\medskip",after=at)
    # Sync clients may briefly lock a freshly generated latexdiff file.
    written <- FALSE
    for(attempt in 1:10) {
      written <- tryCatch({writeLines(z,f,useBytes=TRUE);TRUE},error=function(e)FALSE)
      if(written)break
      Sys.sleep(.3)
    }
    if(!written)stop("Cannot update marked source: ",f)
  }
  status<-system2(perl,c(shQuote(ld),"--type=CFONT","--encoding=utf8","--disable-citation-markup",shQuote(oldnorm),shQuote(file.path(root,"paper/manuscript.tex"))),
    stdout="paper/manuscript_marked.tex",stderr=file.path(run_dir,"latexdiff.log"))
  if(status!=0L)stop("latexdiff failed")
  use_marked_bib("paper/manuscript_marked.tex")
  compile("manuscript_marked","paper",TRUE)
  # The submitted author version names the grant and the authors' repository.
  # Removing the author block alone would leave both visible as deleted text
  # in the anonymous marked copy, so the baseline uses the wording of the
  # anonymous version actually submitted for review.
  submitted_anon<-anonymise(lines)
  funding_at<-grep("^This work was supported by the Riksbankens Jubileumsfond",submitted_anon)
  data_at<-grep("^The data and all code that support the findings of this study are openly available at",submitted_anon)
  stopifnot(length(funding_at)==1L,length(data_at)==1L)
  submitted_anon[funding_at]<-"[Funding information removed for anonymous review.]"
  submitted_anon[data_at]<-"The data and all code that support the findings of this study will be made openly available in a public repository upon publication. The repository link is withheld here because it identifies the authors, and will be provided for the non-anonymous version of record."
  writeLines(submitted_anon,file.path(root,".build/submitted_anonymous.tex"),useBytes=TRUE)
  status<-system2(perl,c(shQuote(ld),"--type=CFONT","--encoding=utf8","--disable-citation-markup",shQuote(file.path(root,".build/submitted_anonymous.tex")),shQuote(file.path(root,"paper/manuscript_anonymous.tex"))),
    stdout="paper/manuscript_marked_anonymous.tex",stderr=file.path(run_dir,"latexdiff_anonymous.log"))
  if(status!=0L)stop("Anonymous latexdiff failed")
  use_marked_bib("paper/manuscript_marked_anonymous.tex")
  compile("manuscript_marked_anonymous","paper",TRUE)
  # Compare the active revision with its predecessor without rebuilding the
  # predecessor's PDF. Historical PDFs and sources remain preserved.
  version_number <- suppressWarnings(as.integer(sub("^manuscript_v","",active_stem)))
  previous_source <- if(is.finite(version_number))file.path("paper",paste0("manuscript_v",version_number-1L,".tex"))else ""
  changes_stem <- paste0(active_stem,"_changes")
  if(nzchar(previous_source)&&file.exists(previous_source)) {
    status<-system2(perl,c(shQuote(ld),"--type=CFONT","--encoding=utf8","--disable-citation-markup",
      shQuote(file.path(root,previous_source)),shQuote(file.path(root,active_source))),
      stdout=file.path("paper",paste0(changes_stem,".tex")),stderr=file.path(run_dir,paste0("latexdiff_",active_stem,".log")))
    if(status!=0L)stop(active_stem," comparison failed")
    use_marked_bib(file.path("paper",paste0(changes_stem,".tex")))
    compile(changes_stem,"paper",TRUE)
  }
  for(f in c("paper/manuscript.pdf","paper/manuscript_marked.pdf","referees/response_to_referees.pdf"))
    writeLines(pdftools::pdf_text(f),file.path("docs/execution",paste0(tools::file_path_sans_ext(basename(f)),"_text.txt")),useBytes=TRUE)
  document_files<-c("paper/manuscript.pdf","paper/manuscript_anonymous.pdf","paper/manuscript_marked.pdf",
    "paper/manuscript_marked_anonymous.pdf","paper/title_page.pdf","referees/response_to_referees.pdf")
  if(active_stem!="manuscript")document_files<-c(document_files,file.path("paper",paste0(active_stem,".pdf")))
  if(file.exists(file.path("paper",paste0(changes_stem,".pdf"))))document_files<-c(document_files,file.path("paper",paste0(changes_stem,".pdf")))
  document_check<-do.call(rbind,lapply(document_files,function(f) {
    info<-pdftools::pdf_info(f);txt<-pdftools::pdf_text(f)
    stopifnot(info$pages==length(txt),info$pages>0,!isTRUE(info$encrypted))
    if(grepl("anonymous",f))stopifnot(!grepl("@sun.ac.za|Johan Fourie|Sugnet Lubbe|Dieter von Fintel",txt[1]))
    data.frame(file=f,pages=info$pages,bytes=file.info(f)$size,readable=TRUE,sha256=sha(f))
  }))
  write.csv(document_check,"docs/execution/document_integrity.csv",row.names=FALSE)
}
write.csv(if(length(timings))do.call(rbind,timings)else data.frame(stage=character(),seconds=numeric()),file.path(run_dir,"stage_times.csv"),row.names=FALSE)
stopifnot(identical(raw_hashes,sha(raw_files)))
stopifnot(identical(code_hashes,sha(production_scripts)))
inputs<-unique(c(raw_files,list.files("code",pattern="\\.R$",full.names=TRUE),coherence_script,active_file,editing_source,markdown_layout,active_source,"paper/manuscript.tex","paper/references.bib","referees/response_to_referees.tex"));inputs<-inputs[file.exists(inputs)]
write.csv(data.frame(file=inputs,sha256=sha(inputs)),"docs/execution/input_manifest.csv",row.names=FALSE)
outputs<-c(list.files("output",recursive=TRUE,full.names=TRUE),list.files("data/analysis",full.names=TRUE),list.files("paper",pattern="\\.(pdf|tex|bib|md)$",full.names=TRUE),"referees/response_to_referees.pdf")
outputs<-outputs[file.exists(outputs)&!dir.exists(outputs)]
write.csv(data.frame(file=outputs,bytes=file.info(outputs)$size,sha256=sha(outputs)),"docs/execution/output_manifest.csv",row.names=FALSE)
writeLines(c(paste("Run:",run_id),paste("UTC completion:",format(Sys.time(),tz="UTC",usetz=TRUE)),"Raw source hashes unchanged.",
  paste("Active manuscript:",active_source),
  paste("Editing source:",editing_source),
  if(!is.null(comparison))paste(nrow(comparison),"CSV/RDS outputs agree across two clean builds at tolerance 1e-8.")else "No repeat comparison in this invocation; prior comparison retained separately.",
  if("--analysis-only"%in%args)"Analytical outputs only in this invocation."else "Clean/marked manuscripts and referee response compiled; references checked."),"docs/execution/latest_run.txt")
if(!"--analysis-only"%in%args) {
  builder<-file.path(root,"../release/scripts/build_release.R")
  if(file.exists(builder))stage(builder,"local_release")
}
cat("Build complete. See docs/execution/latest_run.txt and manifests.\n")

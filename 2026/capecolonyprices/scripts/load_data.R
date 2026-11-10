# Run from the package root; includes compressed joint draws and every frozen CSV.
stopifnot(file.exists('.here'))
suppressPackageStartupMessages(library(readr))
files <- c(list.files('data',pattern='[.]csv([.]gz)?$',recursive=TRUE,full.names=TRUE),
 list.files('replication/results',pattern='[.]csv$',recursive=TRUE,full.names=TRUE),
 list.files('docs',pattern='[.]csv$',recursive=TRUE,full.names=TRUE))
data <- setNames(lapply(files,function(f) {
 z <- read_csv(f,show_col_types=FALSE,guess_max=Inf,na='',num_threads=1)
 if(nrow(problems(z))) stop('CSV parse problems: ',f)
 cat(f,': ',nrow(z),' rows, ',ncol(z),' columns\n',sep='')
 z
}),sub('[.]csv([.]gz)?$','',files))
annual <- data[['data/series/annual_inflation_1700_1834']]
joint_draws <- data[['data/series/annual_index_joint_draws']]

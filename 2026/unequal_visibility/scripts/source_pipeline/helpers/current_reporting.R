# Shared reporting functions for the reviewed analysis.
source('R/00_setup.R')
suppressPackageStartupMessages(library(ggplot2))
CURRENT_OUT <- file.path(PROJ,'revision/output')
CURRENT_FIG <- file.path(PROJ,'revision/figures')
CURRENT_GEN <- file.path(PROJ,'revision/generated')
for(d in c(CURRENT_OUT,CURRENT_FIG,CURRENT_GEN))dir.create(d,recursive=TRUE,showWarnings=FALSE)
current_write <- function(x,name) fwrite(x,file.path(CURRENT_OUT,paste0('current_',name,'.csv')))
current_theme <- function() theme_minimal(base_size=11,base_family='sans')+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
        axis.title=element_text(colour='#333333'),legend.position='bottom',
        plot.background=element_rect(fill='white',colour=NA),
        strip.text=element_text(face='plain',hjust=0),plot.margin=margin(10,15,10,10))
current_palette <- c('#5C2346','#3D8EB9','#6B8E5E','#D4A03E')
current_save_plot <- function(p,name,width=8,height=5){
  ggsave(file.path(CURRENT_FIG,paste0(name,'.png')),p,
         width=width,height=height,dpi=350,bg='white')
  pdf_device<-if(capabilities('cairo'))grDevices::cairo_pdf else grDevices::pdf
  ggsave(file.path(CURRENT_FIG,paste0(name,'.pdf')),p,device=pdf_device,
         width=width,height=height,bg='white')
}
current_wilson <- function(x,n,z=qnorm(.975)){
  stopifnot(all(n>0),all(x>=0),all(x<=n))
  p<-x/n; den<-1+z*z/n
  centre<-(p+z*z/(2*n))/den
  half<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/den
  data.table(lo=pmax(0,centre-half),hi=pmin(1,centre+half))
}
current_macro <- function(name,value)paste0('\\newcommand{\\',name,'}{',value,'}')
current_fmt <- function(x,d=1)formatC(x,format='f',digits=d)

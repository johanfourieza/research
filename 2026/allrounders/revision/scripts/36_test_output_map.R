source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
suppressPackageStartupMessages({library(ggplot2);library(ggrepel)})
# Figure 2: Test players' shrunken mean outputs against the median specialist
# floors, with five well-known all-rounders labelled. Moved here from
# 31_make_exhibits.R so the figure can be redrawn without rebuilding the tables.
set.seed(2412)
read_result<-function(n)read_csv(file.path(results_dir,paste0(n,'.csv')))
theme_set(theme_minimal(base_size=16)+theme(text=element_text(family='Times New Roman',face='plain',colour='#333333'),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position='bottom',legend.title=element_blank(),strip.text=element_text(face='plain'),plot.title=element_text(face='plain')))
saveplot<-function(plot,name,width=9,height=5){ggsave(file.path(revision_dir,'paper/figures',paste0(name,'.pdf')),plot,width=width,height=height,device=cairo_pdf);ggsave(file.path(revision_dir,'paper/figures',paste0(name,'.png')),plot,width=width,height=height,dpi=300)}

d<-read_result('dual_counts')
floors<-d[comp=='tests'&floor_quantile==.5]
dd<-readRDS(file.path(revision_dir,'data/analysis/dual_tests.rds'))$career[evaluable==TRUE]

# Labelled players: DAFT name pattern -> display name. Each pattern must match
# exactly one evaluable Test player, so a changed identity key fails loudly.
labelled<-c('^Shakib Al Hasan '='Shakib Al Hasan','^Kallis, JH '='Jacques Kallis','^Botham, IT '='Ian Botham','^Sobers, GS '='Garfield Sobers','^Imran Khan '='Imran Khan')
lab<-rbindlist(lapply(names(labelled),function(pat){x<-dd[grepl(pat,player)];stopifnot(nrow(x)==1);x[,.(player,label=labelled[[pat]],post_bat,post_bowl)]}))
nudges<-data.table(label=c('Shakib Al Hasan','Jacques Kallis','Ian Botham','Garfield Sobers','Imran Khan'),nudge_x=c(.35,0,.3,0,-.25),nudge_y=c(.5,.45,.75,.45,.45))
lab<-merge(lab,nudges,by='label',sort=FALSE)

plot<-ggplot(dd,aes(post_bat,post_bowl,colour=role))+
 geom_hline(yintercept=floors$bowl_floor,linetype='dashed',colour='#777777')+
 geom_vline(xintercept=floors$bat_floor,linetype='dashed',colour='#777777')+
 geom_point(alpha=.6,size=1.4)+
 geom_point(data=lab,aes(post_bat,post_bowl),inherit.aes=FALSE,shape=21,size=3,stroke=.8,colour='#222222',fill=NA)+
 geom_label_repel(data=lab,aes(post_bat,post_bowl,label=label),inherit.aes=FALSE,size=4.2,family='Times New Roman',colour='#222222',fill='white',label.size=0,label.padding=.15,
  nudge_x=lab$nudge_x,nudge_y=lab$nudge_y,direction='both',force=.5,min.segment.length=0,segment.colour='#555555',segment.size=.3,seed=2412,max.overlaps=Inf)+
 scale_colour_manual(values=c('Batting role'='#3D8EB9','All-round role'='#5C2346','Bowling role'='#D4A03E','Insufficient/other'='#AAAAAA'))+
 labs(x='Shrunken batting output / era mean',y='Shrunken bowling output / era mean')
saveplot(plot,'test_output_map',7.5,5.5)
fwrite(lab,file.path(results_dir,'test_output_map_labels.csv'))

source(file.path(dirname(sub('^--file=', '', commandArgs(FALSE)[grepl('^--file=', commandArgs(FALSE))])), 'analysis_common.R'))
suppressPackageStartupMessages(library(ggplot2))
require_data_gate('workload and composition')
set.seed(2412)
# Common-population exhibit (feedback overall-2): the career batting share of every
# evaluable player, by workload role and competition. Players whose joint posterior
# probability of exceeding both median specialist floors is at least one half
# (posterior independence) are drawn as filled black points.
theme_set(theme_minimal(base_size=15)+theme(text=element_text(family='Times New Roman',face='plain',colour='#333333'),panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),legend.position='bottom',legend.title=element_blank(),strip.text=element_text(face='plain'),plot.title=element_text(face='plain')))
saveplot<-function(plot,name,width=9,height=5){ggsave(file.path(revision_dir,'paper/figures',paste0(name,'.pdf')),plot,width=width,height=height,device=cairo_pdf);ggsave(file.path(revision_dir,'paper/figures',paste0(name,'.png')),plot,width=width,height=height,dpi=300)}

dd<-rbindlist(lapply(names(configs),function(comp){
 c<-readRDS(file.path(revision_dir,'data/analysis',paste0('dual_',comp,'.rds')))$career[evaluable==TRUE & !is.na(batting_share)]
 c[,.(comp,role,batting_share,pool_batting_share,joint=prob_independent>=.5)]
}))
dd[,competition:=factor(unname(comp_labels[comp]),levels=unname(comp_labels[names(configs)]))]
dd[,role_label:=factor(fcase(role=='Batting role','Batting',role=='All-round role','Dual',role=='Bowling role','Bowling',default='Other'),levels=rev(c('Batting','Dual','Bowling','Other')))]
pools<-unique(dd[,.(competition,pool_batting_share)])

plot<-ggplot(dd,aes(batting_share,role_label))+
 geom_vline(data=pools,aes(xintercept=pool_batting_share),linetype='dashed',colour='#777777')+
 geom_jitter(data=dd[joint==FALSE],aes(colour=role_label),height=.28,width=0,alpha=.35,size=.9)+
 geom_jitter(data=dd[joint==TRUE],height=.2,width=0,shape=21,fill='#222222',colour='white',size=2.4,stroke=.4)+
 facet_wrap(~competition,ncol=1,strip.position='top')+
 scale_colour_manual(values=c('Batting'='#3D8EB9','Dual'='#5C2346','Bowling'='#D4A03E','Other'='#AAAAAA'),guide='none')+
 scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))+
 labs(x='Career batting share of output',y=NULL)
saveplot(plot,'share_by_role',7.5,9)

# Summary used in the text: where the joint-benchmark players sit.
summ<-dd[,.(players=.N,median_share=median(batting_share),joint_players=sum(joint),joint_min_share=if(any(joint))min(batting_share[joint])else NA_real_,joint_max_share=if(any(joint))max(batting_share[joint])else NA_real_),by=.(comp,role)]
put(summ[order(comp,role)],'feedback_share_by_role')
cat('Batting-share exhibit complete\n')

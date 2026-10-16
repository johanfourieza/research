# LEAP figure theme (Laboratory for the Economics of Africa's Past) used for Figure 1.
# save_leap_fig() writes PDF, SVG and PNG versions of a plot.
LEAP_COLORS <- c(plum='#5C2346', blue='#3D8EB9', sage='#6B8E5E',
                 gold='#D4A03E', rose='#A34466', teal='#45808B',
                 earth='#8B6B3D', mint='#97C5B0')
LEAP_NONSIG_COLOR <- '#AAAAAA'

theme_leap <- function(base_size=10) {
  theme_minimal(base_size=base_size, base_family='sans') + theme(
    text=element_text(family='sans'),
    plot.title=element_blank(), plot.subtitle=element_blank(), plot.caption=element_blank(),
    axis.title=element_text(size=base_size, colour='#4A4A4A'),
    axis.text=element_text(size=base_size-1, colour='#5A5A5A'),
    legend.text=element_text(size=base_size-1),
    axis.line.x.bottom=element_line(colour='#4A4A4A', linewidth=.8),
    axis.line.y.left=element_line(colour='#4A4A4A', linewidth=.8),
    panel.border=element_blank(),
    panel.grid.major.y=element_line(colour='#E0E0E0', linewidth=.5),
    panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(),
    axis.ticks=element_line(colour='#4A4A4A', linewidth=.6),
    axis.ticks.length=grid::unit(3,'pt'),
    legend.position='bottom', legend.title=element_blank(),
    legend.background=element_blank(), legend.key=element_blank(),
    plot.background=element_rect(fill='white', colour=NA),
    panel.background=element_rect(fill='white', colour=NA),
    plot.margin=margin(10,10,10,10),
    strip.text=element_text(size=base_size, face='plain', colour='#2D2D2D'))
}

save_leap_fig <- function(fig_path, plot, width=10, height=6, dpi=600) {
  for (ext in c('pdf','svg','png')) {
    ggsave(sub('\\.[^.]+$', paste0('.',ext), fig_path), plot,
           width=width, height=height, dpi=dpi, bg='white',
           device=if(ext=='pdf') cairo_pdf else if(ext=='svg') svglite::svglite else 'png')
  }
}

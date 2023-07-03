library(ggplot2)
#Boxplot 2014
p1 <- ggplot(data=main_data_2019,aes(x = spendvtax,y=immigrate_policy,colour=pol_side2)) + geom_point()
p1 <- p1 + scale_colour_manual(values = c('red3','olivedrab3', 'mediumblue', 'yellow2'))
p1 <- p1 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p1 <- p1 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2019 Chapel Hill expert survey',color='Political sides')
p1 <- p1 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p1 <- p1 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p1 <- p1 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

library(tiff)
set.seed(1234)
tiff("PopulistParties.tiff", width = 7, height = 4, units = 'in', res = 500)
p1
dev.off()
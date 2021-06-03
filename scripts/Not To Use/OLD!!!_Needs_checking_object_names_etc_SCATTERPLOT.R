if (require(ggplot2)==FALSE){
  install.packages('ggplot2')
} else {
  library(ggplot2)
}

library(ggplot2)
##########Spendtax and Immigartion policy plot Lef vs. Right
#####Creating the basic scatter plot
p1 <- ggplot(data=data,aes(x = spendvtax,y=immigrate_policy,colour=lr)) + geom_point()
p1 <- p1 + scale_colour_manual(values = c('red3','mediumblue'))
p1 <- p1 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p1 <- p1 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p1 <- p1 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p1 <- p1 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p1 <- p1 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Spendtax and Immigartion policy plot Populist vs. Not populist
#####Creating the basic scatter plot
p2 <- ggplot(data=data,aes(x = spendvtax,y=immigrate_policy,colour=pop_par)) + geom_point()
p2 <- p2 + scale_colour_manual(values = c('grey35','green2'))
p2 <- p2 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p2 <- p2 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Populist party')
p2 <- p2 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p2 <- p2 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p2 <- p2 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Spendtax and Immigartion policy plot Left vs. Right vs. Populist
#####Creating the basic scatter plot
p3 <- ggplot(data=data,aes(x = spendvtax,y=immigrate_policy,colour=pop_lr_gen2)) + geom_point()
p3 <- p3 + scale_colour_manual(values = c('red3','forestgreen','mediumblue'))
p3 <- p3 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p3 <- p3 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p3 <- p3 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p3 <- p3 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p3 <- p3 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))





##########Redistribution and Immigartion policy plot Lef vs. Right
#####Creating the basic scatter plot
p4 <- ggplot(data=data,aes(x = spendvtax,y=redistribution,colour=lr)) + geom_point()
p4 <- p4 + scale_colour_manual(values = c('red3','mediumblue'))
p4 <- p4 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p4 <- p4 + labs(x='Redistribution of wealth',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p4 <- p4 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p4 <- p4 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p4 <- p4 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Redistribution and Immigartion policy plot Populist vs. Not populist
#####Creating the basic scatter plot
p5 <- ggplot(data=data,aes(x = spendvtax,y=redistribution,colour=pop_par)) + geom_point()
p5 <- p5 + scale_colour_manual(values = c('grey35','green2'))
p5 <- p5 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p5 <- p5 + labs(x='Redistribution of wealth',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Populist party')
p5 <- p5 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p5 <- p5 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p5 <- p5 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Redistribution and Immigartion policy plot Left vs. Right vs. Populist
#####Creating the basic scatter plot
p6 <- ggplot(data=data,aes(x = spendvtax,y=redistribution,colour=pop_lr_gen2)) + geom_point()
p6 <- p6 + scale_colour_manual(values = c('red3','forestgreen','mediumblue'))
p6 <- p6 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p6 <- p6 + labs(x='Redistribution of wealth',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Populist party')
p6 <- p6 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p6 <- p6 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p6 <- p6 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))





##########Econ_interven and Immigartion policy plot Lef vs. Right
#####Creating the basic scatter plot
p7 <- ggplot(data=data,aes(x = spendvtax,y=econ_interven,colour=lr)) + geom_point()
p7 <- p7 + scale_colour_manual(values = c('red3','mediumblue'))
p7 <- p7 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p7 <- p7 + labs(x='State intervention in the economy',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p7 <- p7 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p7 <- p7 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p7 <- p7 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Econ_interven and Immigartion policy plot Populist vs. Not populist
#####Creating the basic scatter plot
p8 <- ggplot(data=data,aes(x = spendvtax,y=econ_interven,colour=pop_par)) + geom_point()
p8 <- p8 + scale_colour_manual(values = c('grey35','green2'))
p8 <- p8 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p8 <- p8 + labs(x='State intervention in the economy',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Populist party')
p8 <- p8 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p8 <- p8 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p8 <- p8 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

##########Econ_interven and Immigartion policy plot Left vs. Right vs. Populist
#####Creating the basic scatter plot
p9 <- ggplot(data=data,aes(x = spendvtax,y=econ_interven,colour=pop_lr_gen2)) + geom_point()
p9 <- p9 + scale_colour_manual(values = c('red3','forestgreen','mediumblue'))
p9 <- p9 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p9 <- p9 + labs(x='State intervention in the economy',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p9 <- p9 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p9 <- p9 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p9 <- p9 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

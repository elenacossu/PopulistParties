if (require(ggplot2)==FALSE){
  install.packages('ggplot2')
} else {
  library(ggplot2)
}

library(ggplot2)
##########Graphs for all the scale variables
#####creating the database
gradat1<-cbind(data[,c(44,49,39)],rep('Anti-elite rhetoric',268))
gradat2<-cbind(data[,c(44,49,28)],rep('Civil liberties vs. law and order',268))
gradat3<-cbind(data[,c(44,49,40)],rep('Reducing political corruption',268))
gradat4<-cbind(data[,c(44,49,27)],rep('State intervention in the economy',268))
gradat5<-cbind(data[,c(44,49,37)],rep('Ethnic minorities',268))
gradat6<-cbind(data[,c(44,49,31)],rep('Immigration policy',268))
gradat7<-cbind(data[,c(44,49,26)],rep('Redistribution of wealth',268))
gradat8<-cbind(data[,c(44,49,35)],rep('Political decentralization to regions',268))
gradat9<-cbind(data[,c(44,49,30)],rep('Religious principles in politics',268))
gradat10<-cbind(data[,c(44,49,29)],rep('Social lifestyle',268))
gradat11<-cbind(data[,c(44,49,24)],rep('Improving public services vs. reducing taxes',268))
gradat12<-cbind(data[,c(44,49,33)],rep('Urban vs. rural interests',268))
gradat13<-cbind(data[,c(44,49,15)],rep('EU Authority over economic & budgetary policies',268))
gradat14<-cbind(data[,c(44,49,12)],rep('EU Cohesion',268))
gradat15<-cbind(data[,c(44,49,11)],rep('EU Internal market',268))

names(gradat1)[1]<-'co_pa_code'
names(gradat2)[1]<-'co_pa_code'
names(gradat3)[1]<-'co_pa_code'
names(gradat4)[1]<-'co_pa_code'
names(gradat5)[1]<-'co_pa_code'
names(gradat6)[1]<-'co_pa_code'
names(gradat7)[1]<-'co_pa_code'
names(gradat8)[1]<-'co_pa_code'
names(gradat9)[1]<-'co_pa_code'
names(gradat10)[1]<-'co_pa_code'
names(gradat11)[1]<-'co_pa_code'
names(gradat12)[1]<-'co_pa_code'
names(gradat13)[1]<-'co_pa_code'
names(gradat14)[1]<-'co_pa_code'
names(gradat15)[1]<-'co_pa_code'
names(gradat1)[2]<-'pol_side'
names(gradat2)[2]<-'pol_side'
names(gradat3)[2]<-'pol_side'
names(gradat4)[2]<-'pol_side'
names(gradat5)[2]<-'pol_side'
names(gradat6)[2]<-'pol_side'
names(gradat7)[2]<-'pol_side'
names(gradat8)[2]<-'pol_side'
names(gradat9)[2]<-'pol_side'
names(gradat10)[2]<-'pol_side'
names(gradat11)[2]<-'pol_side'
names(gradat12)[2]<-'pol_side'
names(gradat13)[2]<-'pol_side'
names(gradat14)[2]<-'pol_side'
names(gradat15)[2]<-'pol_side'
names(gradat1)[3]<-'variable_value'
names(gradat2)[3]<-'variable_value'
names(gradat3)[3]<-'variable_value'
names(gradat4)[3]<-'variable_value'
names(gradat5)[3]<-'variable_value'
names(gradat6)[3]<-'variable_value'
names(gradat7)[3]<-'variable_value'
names(gradat8)[3]<-'variable_value'
names(gradat9)[3]<-'variable_value'
names(gradat10)[3]<-'variable_value'
names(gradat11)[3]<-'variable_value'
names(gradat12)[3]<-'variable_value'
names(gradat13)[3]<-'variable_value'
names(gradat14)[3]<-'variable_value'
names(gradat15)[3]<-'variable_value'
names(gradat1)[4]<-'variable_name'
names(gradat2)[4]<-'variable_name'
names(gradat3)[4]<-'variable_name'
names(gradat4)[4]<-'variable_name'
names(gradat5)[4]<-'variable_name'
names(gradat6)[4]<-'variable_name'
names(gradat7)[4]<-'variable_name'
names(gradat8)[4]<-'variable_name'
names(gradat9)[4]<-'variable_name'
names(gradat10)[4]<-'variable_name'
names(gradat11)[4]<-'variable_name'
names(gradat12)[4]<-'variable_name'
names(gradat13)[4]<-'variable_name'
names(gradat14)[4]<-'variable_name'
names(gradat15)[4]<-'variable_name'

graph_dataset_1<-rbind(gradat1,gradat2,gradat3,gradat4,gradat5,gradat6,gradat7,gradat8,gradat9,gradat10,gradat11,gradat12)
graph_dataset_2<-rbind(gradat13,gradat14,gradat15)
graph_dataset_1<-graph_dataset_1[c(1,2,4,3)]
graph_dataset_2<-graph_dataset_2[c(1,2,4,3)]

#Making the proper dataset for the radar plot
graph_dataset_1<-graph_dataset_1[order(graph_dataset_1$co_pa_code,graph_dataset_1$pol_side,graph_dataset_1$variable_name),]
radar_data1<-aggregate(graph_dataset_1$variable_value,by=list(graph_dataset_1$variable_name,graph_dataset_1$pol_side),FUN=mean,na.rm=TRUE)
radar_data1<-cbind(radar_data1[,c(2,1,3)],rep(seq(1:12),3))
political_side<-as.factor(radar_data1$Group.2)
variable_name<-as.factor(radar_data1$Group.1)
variable_value<-c(radar_data1$x)
variable_position<-rep(seq(1:12),3)
radar_dataset_1<-data.frame(political_side,variable_name,variable_value,variable_position)
names(radar_dataset_1)<-c('Political sides','Attribute','Scale value (mean)','Variable position (for radar plot)')


#####Creating boxplots
#Breaking the labels to fit the graph
levels(graph_dataset_2$variable_name)<-c(paste('EU Authority over','\n','economic & budgetary','\n','policies'),paste('EU','\n','Cohesion'),paste('EU','\n','Internal market'))
###EU boxplot
desc_plot_eu <- ggplot(data=graph_dataset_2,aes(x=graph_dataset_2$variable_name,y=graph_dataset_2$variable_value,fill=graph_dataset_2$pol_side))+geom_boxplot(notch=TRUE)
desc_plot_eu <- desc_plot_eu + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2'))
desc_plot_eu <- desc_plot_eu + scale_y_continuous(limits=c(1,7),breaks=1:7)
desc_plot_eu <- desc_plot_eu + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_eu <- desc_plot_eu + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_eu <- desc_plot_eu + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_eu <- desc_plot_eu + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))


#Breaking the labels to fit the graph
levels(graph_dataset_1$variable_name)<-gsub(' ','\n',levels(graph_dataset_1$variable_name))
###Other variables
desc_plot_other <- ggplot(data=graph_dataset_1,aes(x=graph_dataset_1$variable_name,y=graph_dataset_1$variable_value,fill=graph_dataset_1$pol_side))+geom_boxplot(notch=TRUE)
desc_plot_other <- desc_plot_other + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2'))
desc_plot_other <- desc_plot_other + scale_y_continuous(limits=c(0,10),breaks=0:10)
desc_plot_other <- desc_plot_other + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_other <- desc_plot_other + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_other <- desc_plot_other + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_other <- desc_plot_other + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))


###########Radar plot
levels(radar_dataset_1$Attribute)<-c(paste('Anti-elite','\n','rhetoric'),paste('Civil liberties vs.','\n','law and order'),paste('Reducing','\n','political','\n','corruption'),paste('State','\n','intervention','\n','in the','\n','economy'),paste('Ethnic','\n','minorities'),paste('Immigration','\n','policy'),paste('Redistribution','\n','of wealth'),paste('Political','\n','decentralization','\n','to regions'),paste('Religious','\n','principles','\n','in politics'),paste('Social','\n','lifestyle'),paste('Improving','\n','public services vs.','\n','reducing taxes'),paste('Urban vs. rural','\n','interests'))

radar_plot1 <- ggplot(data=radar_dataset_1,aes(y=radar_dataset_1$`Scale value (mean)`,x=reorder(radar_dataset_1$Attribute,radar_dataset_1$`Variable position (for radar plot)`),group=radar_dataset_1$`Political sides`,colour=radar_dataset_1$`Political sides`))
radar_plot1 <- radar_plot1 + geom_point(size=3.2)+geom_path()+geom_polygon(size=1.1,fill=NA) + coord_polar(clip='off')
radar_plot1 <- radar_plot1 + scale_colour_manual(values = c('firebrick1','springgreen3','royalblue2'))
radar_plot1 <- radar_plot1 + scale_y_continuous(limits=c(0,10),breaks=0:10) + theme(panel.grid=element_line(colour='grey70'))
radar_plot1 <- radar_plot1 + theme(axis.title.x=element_blank(),axis.text.x=element_text(face='bold',size=9,colour='black'))
radar_plot1 <- radar_plot1 + theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
radar_plot1 <- radar_plot1 + theme(legend.title=element_blank(),legend.position='top',legend.direction='horizontal',legend.text=element_text(face='bold',size=9))
radar_plot1 <- radar_plot1 + labs(caption='Source: 2014 Chapel Hill expert survey') + theme(plot.caption=element_text(face='italic',size=8))
#If the grey background is okay, then leave the line below as a comment. In the small window the labels are going out from the square, but if you zoom, it's fine
#radar_plot1 <- radar_plot1 + theme(panel.background=element_blank())

remove(radar_data1,gradat1,gradat2,gradat3,gradat4,gradat5,gradat6,gradat7,gradat8,gradat9,gradat10,gradat11,gradat12,gradat13,gradat14,gradat15)
# all variables test
test2014 <- main_data_2014 %>% 
  dplyr::select(pol_side2,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
                lrgen,
                lrecon,
                lrecon_salience,
                galtan,
                galtan_salience,
                spendvtax,
                deregulation,
                redistribution,
                econ_interven,
                civlib_laworder,
                sociallifestyle,
                religious_principle,
                immigrate_policy,
                multiculturalism,
                urban_rural,
                regions,
                ethnic_minorities,
                nationalism,
                antielite_salience,
                corrupt_salience) %>% 
  dplyr::group_by(pol_side2) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean')


p_test2014 <- ggplot2::ggplot(test2014, ggplot2::aes(x = attribute, y = mean, fill = pol_side2)) + 
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual('legend', values = c('Left' = 'red3', 'Center' = 'yellow3', 'Right' = 'mediumblue', 'Populist' = 'olivedrab3'))

p_test2014

#Scatterplot
library(ggplot2)
p1 <- ggplot(data=main_data_2014,aes(x = spendvtax,y=immigrate_policy,colour=pol_side2)) + geom_point()
p1 <- p1 + scale_colour_manual(values = c('red3','olivedrab3', 'mediumblue', 'yellow2'))
p1 <- p1 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p1 <- p1 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2014 Chapel Hill expert survey',color='Political sides')
p1 <- p1 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p1 <- p1 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p1 <- p1 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

p1

# Boxplot 1-7
boxplot_data_2014 <- main_data_2014 %>%
  dplyr::select(party_name, pol_side2, eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets) %>%
  tidyr::pivot_longer(cols = eu_position:eu_budgets, names_to = "variables", values_to = "scale_values")

desc_plot_eu <- ggplot(data=boxplot_data_2014,
                       aes(x=boxplot_data_2014$variables,
                           y=boxplot_data_2014$scale_values,
                           fill=boxplot_data_2014$pol_side2)) + geom_boxplot(notch=TRUE)
desc_plot_eu <- desc_plot_eu + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2', "yellow3"))
desc_plot_eu <- desc_plot_eu + scale_y_continuous(limits=c(1,7),breaks=1:7)
desc_plot_eu <- desc_plot_eu + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_eu <- desc_plot_eu + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_eu <- desc_plot_eu + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_eu <- desc_plot_eu + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))

# Boxplot 1-10
boxplot_data_2014_2 <- main_data_2014 %>%
  dplyr::select(party_name, pol_side2, eu_salience, eu_dissent, lrgen,
                lrecon,
                lrecon_salience,
                galtan,
                galtan_salience,
                spendvtax,
                deregulation,
                redistribution,
                econ_interven,
                civlib_laworder,
                sociallifestyle,
                religious_principle,
                immigrate_policy,
                multiculturalism,
                urban_rural,
                regions,
                ethnic_minorities,
                nationalism,
                antielite_salience,
                corrupt_salience) %>%
  tidyr::pivot_longer(cols = eu_salience:corrupt_salience, names_to = "variables", values_to = "scale_values")

desc_plot_other <- ggplot(data=boxplot_data_2014_2,
                       aes(x=boxplot_data_2014_2$variables,
                           y=boxplot_data_2014_2$scale_values,
                           fill=boxplot_data_2014_2$pol_side2)) + geom_boxplot(notch=TRUE)
desc_plot_other <- desc_plot_other + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2', "yellow3"))
desc_plot_other <- desc_plot_other + scale_y_continuous(limits=c(1,10),breaks=1:10)
desc_plot_other <- desc_plot_other + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_other <- desc_plot_other + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_other <- desc_plot_other + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_other <- desc_plot_other + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))

# Radar Graph


#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

# test all variables 2019
test2019 <- main_data_2019 %>% 
  dplyr::select(pol_side2,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
                lrgen,
                lrecon,
                lrecon_salience,
                galtan,
                galtan_salience,
                spendvtax,
                deregulation,
                redistribution,
                econ_interven,
                civlib_laworder,
                sociallifestyle,
                religious_principles,
                immigrate_policy,
                multiculturalism,
                urban_rural,
                regions,
                ethnic_minorities,
                nationalism,
                antielite_salience,
                corrupt_salience) %>% 
  dplyr::group_by(pol_side2) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean')


p_test2019 <- ggplot2::ggplot(test2019, ggplot2::aes(x = attribute, y = mean, fill = pol_side2)) + 
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual('legend', values = c('Left' = 'red3', 'Center' = 'yellow3', 'Right' = 'mediumblue', 'Populist' = 'olivedrab3'))

p_test2019

#Scatterplot 2019
p2 <- ggplot(data=main_data_2019,aes(x = spendvtax,y=immigrate_policy,colour=pol_side2)) + geom_point()
p2 <- p2 + scale_colour_manual(values = c('red3','olivedrab3', 'mediumblue', 'yellow2'))
p2 <- p2 + scale_x_continuous(limits=c(0,10),breaks=0:10) + scale_y_continuous(limits=c(0,10),breaks=0:10)
p2 <- p2 + labs(x='Improving public services vs. reducing taxes',y='Immigration policy',caption='Source: 2019 Chapel Hill expert survey',color='Political sides')
p2 <- p2 + geom_hline(yintercept=5, linetype='dashed', color = 'black') + geom_vline(xintercept=5, linetype='dashed', color = 'black')
p2 <- p2 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_text(face='bold',size=10),legend.text=element_text(face='bold',size=10))
p2 <- p2 + theme(axis.title.x = element_text(face='bold',size=12),axis.title.y=element_text(face='bold',size=12),plot.caption=element_text(face='italic',size=8))

p2

# Boxplot 2019 1-7
boxplot_data_2019 <- main_data_2019 %>%
  dplyr::select(party, pol_side2, eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets) %>%
  tidyr::pivot_longer(cols = eu_position:eu_budgets, names_to = "variables", values_to = "scale_values")

desc_plot_eu_2019 <- ggplot(data=boxplot_data_2019,
                       aes(x=boxplot_data_2019$variables,
                           y=boxplot_data_2019$scale_values,
                           fill=boxplot_data_2019$pol_side2)) + geom_boxplot(notch=TRUE)
desc_plot_eu_2019 <- desc_plot_eu_2019 + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2', "yellow3"))
desc_plot_eu_2019 <- desc_plot_eu_2019 + scale_y_continuous(limits=c(1,7),breaks=1:7)
desc_plot_eu_2019 <- desc_plot_eu_2019 + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_eu_2019 <- desc_plot_eu_2019 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_eu_2019 <- desc_plot_eu_2019 + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_eu_2019 <- desc_plot_eu_2019 + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))

# Boxplot 1-10
boxplot_data_2019_2 <- main_data_2019 %>%
  dplyr::select(party, pol_side2, eu_salience, eu_dissent, lrgen,
                lrecon,
                lrecon_salience,
                galtan,
                galtan_salience,
                spendvtax,
                deregulation,
                redistribution,
                econ_interven,
                civlib_laworder,
                sociallifestyle,
                religious_principles,
                immigrate_policy,
                multiculturalism,
                urban_rural,
                regions,
                ethnic_minorities,
                nationalism,
                antielite_salience,
                corrupt_salience) %>%
  tidyr::pivot_longer(cols = eu_salience:corrupt_salience, names_to = "variables", values_to = "scale_values")

desc_plot_other_2019 <- ggplot(data=boxplot_data_2019_2,
                          aes(x=boxplot_data_2019_2$variables,
                              y=boxplot_data_2019_2$scale_values,
                              fill=boxplot_data_2019_2$pol_side2)) + geom_boxplot(notch=TRUE)
desc_plot_other_2019 <- desc_plot_other_2019 + scale_fill_manual(values=c('firebrick1','springgreen3','royalblue2', "yellow3"))
desc_plot_other_2019 <- desc_plot_other_2019 + scale_y_continuous(limits=c(1,10),breaks=1:10)
desc_plot_other_2019 <- desc_plot_other_2019 + labs(x=element_blank(),y='Evaluation scale',caption='Source: 2014 Chapel Hill expert survey')
desc_plot_other_2019 <- desc_plot_other_2019 + theme(legend.position='top',legend.direction='horizontal',legend.title=element_blank(),legend.text=element_text(face='bold',size=10))
desc_plot_other_2019 <- desc_plot_other_2019 + theme(axis.text.x=element_text(color='black',size=10,face=c('bold.italic')),plot.caption=element_text(face='italic',size=8))
desc_plot_other_2019 <- desc_plot_other_2019 + theme(axis.text.y=element_text(color='black',size=9),axis.title.y=element_text(face='bold',size=11))

#Radar Graph 2019

## October 2020
## Notes for populist parties parties paper

##Objectives of the papers:
#First set of objectives:
# To look at mean and median of populist parties in 2019. 
# To look as well at centrist, left, and right parties so that I can map the political spectrum now that we've a lot of populist parties
# For this reason the log model and the kivad diagrams are also perfect top see where overall all the factions stand
# Second set of objectives: 
# once we show that all factions have something that unite them, I'd like to see if these commonalities are the same all over Europe or if there are differences
# Possible areas of comparison and analysis:
# PIIGS (Portugal, Ireland, Italy, Greece, Spain) 
# Western Europe: UK, France, Belgium, Holland, tiny States, Germany, Switzerland, Austria
# Northern Europe: Sweden, Norway, Denmark, Finland, Iceland
# Eastern Europe: former Soviet bloc countries
# exclude Turkey

##Questions:
#Generic:
# C2019 data. Change? Comparison?
# Europe vs European Union?
# Turkey, Greece and Cyprus as EE?
# Would it be stupid to delete and reinstall RStudio?

#By Script:
# 002: descriptive means with centrist faction as well?
# 002: can't find function write.xlsx
# 004: Why correlation matrix?
# 004: Error -> rcorr must have >4 observations
# 004: Error o Java runtime present when use library(xlsx). Tried to install Java with install.packages('rJava') 
# Error for library(rJava): No Java runtime present, try --request to install
# possible solution here https://github.com/vvo/selenium-standalone/issues/140 (but Oracle website impossible to access)
# 004: what is train?

##SOLVED:
##By Script:
## 002: aggregating the scale values by the political side groups to see the mean?
# you push together the database on a lower level. you put the average of the category to the observation
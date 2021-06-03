library(dplyr)

raw_data_2014 <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/CHES_2014_Party_dataset.csv', stringsAsFactors = FALSE)

populist_input <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/Populist_Parties_Input.csv', sep = ';', stringsAsFactors = FALSE)

country_regions2014 <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/Country_Regions_Input.csv', sep = ';', stringsAsFactors = FALSE)

main_data_2014 <- raw_data_2014 %>% 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::select(cname, party_name, party_id, lrgen,                                                             #Selecting the variables, which are needed for the analysis
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
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::rename(country = cname,                                                                                #Renaming variables for easier understanding
                party_code = party_id) %>% 
  dplyr::filter(country != 'tur') %>% 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::mutate(pol_side1 = factor(dplyr::case_when(lrgen <= 4 ~ 1,                                             #Creating a factor variable with political sides, based on the 'lrgen' variable
                                                   lrgen > 4 & lrgen < 6 ~ 2, 
                                                   lrgen >= 6 ~ 3), 
                                  labels = c('Left', 'Center', 'Right'))) %>% 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::left_join(., populist_input, by = 'party_code') %>%                                                    #Joining the populist dataset to the analysis dataset
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::mutate(populist = factor(populist, labels = c('Not Populist', 'Populist'))) %>%                        #Creating a factor variable from the joint populist variable
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::left_join(., country_regions2014, by = 'country') %>%                                                      #Joining the region dataset to the analysis dataset 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::mutate(region1 = factor(region1, labels = c('Western Europe', 'Eastern Europe',                        #Creating a factor variable from the 4-way region variable
                                                     'Northern Europe', 'Southern Europe')),
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                region2 = factor(region2, labels = c('Western Europe', 'Eastern Europe'))) %>%                  #Creating a factor variable from the 2-way region variable
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dplyr::mutate(pol_side2 = factor(dplyr::case_when(populist == 'Populist' ~ 4,                                 #Creating a 4-way political side variable using the political side and the populist variable
                                                    populist == 'Not Populist' & pol_side1 == 'Left' ~ 1,
                                                    populist == 'Not Populist' & pol_side1 == 'Center' ~ 2,
                                                    populist == 'Not Populist' & pol_side1 == 'Right' ~ 3),
                                   labels = c('Left', 'Center', 'Right', 'Populist')))


###Checking missing values
missing_values_2014 <- main_data_2014 %>% 
  as.data.frame %>% 
  dplyr::summarise_all(~sum(is.na(.))) %>% 
  tidyr::pivot_longer(dplyr::everything(), names_to = 'variables', values_to = 'missing_values')

missing_values_2014


###A party's name was NA and R translated it into 'NA' value, inputting the proper character NA
main_data_2014[which(main_data_2014$party_code == 2406), "party"] <- "NA"


###Checking the populist parties and political sides
table(main_data_2014$populist)
table(main_data_2014$pol_side1, main_data_2014$populist)
table(main_data_2014$pol_side2, main_data_2014$populist)

main_data_2014 %>% 
  dplyr::filter(populist == 'Populist' & pol_side1 == 'Center') %>% 
  dplyr::select(country, party_name)

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

raw_data_2019 <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/CHES_2019_Party_dataset.csv', stringsAsFactors = FALSE)

populist_input <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/Populist_Parties_Input.csv', sep = ';', stringsAsFactors = FALSE)

country_regions2019 <- read.csv('/Users/Elena/Documents/GitHub/PopulistParty/files/Country_Regions_Input2019.csv', sep = ';', stringsAsFactors = FALSE)

main_data_2019 <- raw_data_2019 %>% 
  dplyr::select(country, party, party_id, lrgen,                                                             #Selecting the variables, which are needed for the analysis
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
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::rename(country = country,                                                                                #Renaming variables for easier understanding
              party_code = party_id) %>% 
  dplyr::filter(country != 'tur' & country != 'ice') %>% 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::mutate(pol_side1 = factor(dplyr::case_when(lrgen <= 4 ~ 1,                                             #Creating a factor variable with political sides, based on the 'lrgen' variable
                                                  lrgen > 4 & lrgen < 6 ~ 2, 
                                                  lrgen >= 6 ~ 3), 
                                 labels = c('Left', 'Center', 'Right'))) %>% 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::left_join(., populist_input, by = 'party_code') %>%                                                    #Joining the populist dataset to the analysis dataset
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::mutate(populist = factor(populist, labels = c('Not Populist', 'Populist'))) %>%                        #Creating a factor variable from the joint populist variable
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::left_join(., country_regions2019, by = 'country') %>%                                                      #Joining the region dataset to the analysis dataset 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::mutate(region1 = factor(region1, labels = c('Western Europe', 'Eastern Europe',                        #Creating a factor variable from the 4-way region variable
                                                   'Northern Europe', 'Southern Europe')),
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
              region2 = factor(region2, labels = c('Western Europe', 'Eastern Europe'))) %>%                  #Creating a factor variable from the 2-way region variable
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dplyr::mutate(pol_side2 = factor(dplyr::case_when(populist == 'Populist' ~ 4,                                 #Creating a 4-way political side variable using the political side and the populist variable
                                                  populist == 'Not Populist' & pol_side1 == 'Left' ~ 1,
                                                  populist == 'Not Populist' & pol_side1 == 'Center' ~ 2,
                                                  populist == 'Not Populist' & pol_side1 == 'Right' ~ 3),
                                 labels = c('Left', 'Center', 'Right', 'Populist')))

###Checking missing values
missing_values_2019 <- main_data_2019 %>% 
  as.data.frame %>% 
  dplyr::summarise_all(~sum(is.na(.))) %>% 
  tidyr::pivot_longer(dplyr::everything(), names_to = 'variables', values_to = 'missing_values')

missing_values_2019

###A party's name was NA and R translated it into 'NA' value, inputting the proper character NA
main_data_2014[which(main_data_2014$party_code == 2406), "party"] <- "NA"

###Checking the populist parties and political sides
table(main_data_2019$populist)
table(main_data_2019$pol_side1, main_data_2019$populist)
table(main_data_2019$pol_side2, main_data_2019$populist)

main_data_2019 %>% 
  dplyr::filter(populist == 'Populist' & pol_side1 == 'Center') %>% 
  dplyr::select(country, party)



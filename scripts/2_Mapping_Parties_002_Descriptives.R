###Creating the means for each attribute, breakdown by political side ('pol_side2')
mean_sides_2014 <- main_data_2014 %>% 
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
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = pol_side2, values_from = mean)

###Creating the means for each attribute, breakdown by 4-way regions ('region1')
mean_region1_2014 <- main_data_2014 %>% 
  dplyr::select(region1,
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
  dplyr::group_by(region1) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-region1, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = region1, values_from = mean)

###Creating the means for each attribute, breakdown by 2-way regions ('region2')
mean_region2_2014 <- main_data_2014 %>% 
  dplyr::select(region2,
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
  dplyr::group_by(region2) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-region2, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = region2, values_from = mean)


#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

###Creating the means for each attribute, breakdown by political side ('pol_side2')
mean_sides_2019 <- main_data_2019 %>% 
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
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = pol_side2, values_from = mean)

###Creating the means for each attribute, breakdown by 4-way regions ('region1')
mean_region1_2019 <- main_data_2019 %>% 
  dplyr::select(region1,
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
  dplyr::group_by(region1) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-region1, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = region1, values_from = mean)

###Creating the means for each attribute, breakdown by 2-way regions ('region2')
mean_region2_2019 <- main_data_2019 %>% 
  dplyr::select(region2,
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
  dplyr::group_by(region2) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-region2, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = region2, values_from = mean)


# ----------------------------------------------------------------------------------------------------------

table2019 <- with(main_data_2019, table(pol_side1, pol_side2))
table2014 <- with(main_data_2014, table(pol_side1, pol_side2))


segg <- main_data_2014 %>%
  dplyr::select(country, party_code, party_name, pol_side2, region1) %>%
  dplyr::select(party_code) %>%
  dplyr::bind_rows(., main_data_2019 %>%
                     dplyr::select(party_code)) %>%
  dplyr::arrange(party_code) %>%
  dplyr::distinct() %>%
  dplyr::left_join(., main_data_2014 %>%
                     dplyr::select(country, party_code, party_name, pol_side2, region1), 
                   by = 'party_code') %>%
  dplyr::rename(country_2014 = country,
                party_name_2014 = party_name,
                pol_side2_2014 = pol_side2,
                region1_2014 = region1) %>%
  dplyr::left_join(., main_data_2019 %>%
                     dplyr::select(country, party_code, party, pol_side2, region1), 
                   by = 'party_code') %>%
  dplyr::rename(country_2019 = country,
                party_name_2019 = party,
                pol_side2_2019 = pol_side2,
                region1_2019 = region1)

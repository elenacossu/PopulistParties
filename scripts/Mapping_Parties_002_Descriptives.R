###Creating the means for each attribute, breakdown by political side ('pol_side2')
mean_sides_2014 <- main_data_2014 %>% 
  dplyr::filter(country != 'tur') %>% 
  dplyr::select(pol_side2,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
                eu_ep,
                eu_turkey,
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
  dplyr::filter(country != 'tur') %>% 
  dplyr::select(region1,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
                eu_ep,
                eu_turkey,
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
  dplyr::filter(country != 'tur') %>% 
  dplyr::select(region2,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
                eu_ep,
                eu_turkey,
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


# ----------------------------------------------------------------------------------------------------------
# Only use this part if you want to change the files. The results with the means are already in the folder!
write.csv(mean_sides_2014, 'files/mean_sides_2014.csv')
write.csv(mean_region1_2014, 'files/mean_region1_2014.csv')
write.csv(mean_region2_2014, 'files/mean_region2_2014.csv')
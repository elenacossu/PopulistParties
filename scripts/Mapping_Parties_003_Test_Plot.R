test <- main_data_2014 %>% 
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
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean')


p_test <- ggplot2::ggplot(test, ggplot2::aes(x = attribute, y = mean, fill = pol_side2)) + 
  ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual('legend', values = c('Left' = 'red3', 'Center' = 'yellow3', 'Right' = 'mediumblue', 'Populist' = 'olivedrab3'))

p_test
eu_position
eu_salience
eu_dissent
eu_intmark
eu_cohesion

region1
pol_side2

test <- main_data_2014 %>% 
  dplyr::select(region1,
                pol_side2,
                eu_position,
                eu_salience,
                eu_dissent,
                eu_intmark,
                eu_cohesion,
                eu_foreign,
                eu_budgets,
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
                corrupt_salience)

test_manova <- manova(cbind(eu_position,
                            eu_salience,
                            eu_dissent,
                            eu_intmark,
                            eu_cohesion,
                            eu_foreign,
                            eu_budgets,
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
                            corrupt_salience) ~ pol_side2,
                      data = test)

test_results <- summary.aov(test_manova)

test_results[[1]][[4]][1]



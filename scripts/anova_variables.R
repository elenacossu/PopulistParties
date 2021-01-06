library(tidyverse)
library(ggpubr)
library(rstatix)
library(purrr)

# ANOVA and Post-hoc eu_position political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_position ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_position ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_position region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_position ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_position ~ region1)

pwc

# ANOVA and Post-hoc eu_salience political sides
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_salience ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_salience ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_salience region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_salience ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_salience ~ region1)

pwc

# ANOVA and Post-hoc eu_dissent political sides
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_dissent ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_dissent ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_dissent region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_dissent ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_dissent ~ region1)

pwc

# ANOVA and Post-hoc eu_intmark political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_intmark ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_intmark ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_intmark region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_intmark ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_intmark ~ region1)

pwc

# ANOVA and Post-hoc eu_cohesion political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_cohesion ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_cohesion ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_cohesion region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_cohesion ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_cohesion ~ region1)

pwc

# ANOVA and Post-hoc eu_foreign political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_foreign ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_foreign ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_foreign region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_foreign ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_foreign ~ region1)

pwc

# ANOVA and Post-hoc eu_budgets political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_budgets ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_budgets ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_budgets region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_budgets ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_budgets ~ region1)

pwc

# ANOVA and Post-hoc eu_ep political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_ep ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_ep ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_ep region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_ep ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_ep ~ region1)

pwc

# ANOVA and Post-hoc eu_turkey political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_turkey ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_turkey ~ pol_side2)

pwc

# ANOVA and Post-hoc eu_turkey region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(eu_turkey ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(eu_turkey ~ region1)

pwc

# ANOVA and Post-hoc lrecon political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(lrecon ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(lrecon ~ pol_side2)

pwc

# ANOVA and Post-hoc lrecon region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(lrecon ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(lrecon ~ region1)

pwc

# ANOVA and Post-hoc lrecon_salience political sides
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(lrecon_salience ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(lrecon_salience ~ pol_side2)

pwc

# ANOVA and Post-hoc lrecon_salience region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(lrecon_salience ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(lrecon_salience ~ region1)

pwc

# ANOVA and Post-hoc galtan political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(galtan ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(galtan ~ pol_side2)

pwc

# ANOVA and Post-hoc galtan region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(galtan ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(galtan ~ region1)

pwc

# ANOVA and Post-hoc galtan_salience political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(galtan_salience ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(galtan_salience ~ pol_side2)

pwc

# ANOVA and Post-hoc galtan_salience region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(galtan_salience ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(galtan_salience ~ region1)

pwc

# ANOVA and Post-hoc spendvtax political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(spendvtax ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(spendvtax ~ pol_side2)

pwc

# ANOVA and Post-hoc spendvtax region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(spendvtax ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(spendvtax ~ region1)

pwc

# ANOVA and Post-hoc deregulation political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(deregulation ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(deregulation ~ pol_side2)

pwc

# ANOVA and Post-hoc deregulation region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(deregulation ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(deregulation ~ region1)

pwc

# ANOVA and Post-hoc econ_interven political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(econ_interven ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(econ_interven ~ pol_side2)

pwc

# ANOVA and Post-hoc econ_interven region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(econ_interven ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(econ_interven ~ region1)

pwc

# ANOVA and Post-hoc civlib_laworder political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(civlib_laworder ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(civlib_laworder ~ pol_side2)

pwc

# ANOVA and Post-hoc civlib_laworder region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(civlib_laworder ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(civlib_laworder ~ region1)

pwc

# ANOVA and Post-hoc sociallifestyle political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(sociallifestyle ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(sociallifestyle ~ pol_side2)

pwc

# ANOVA and Post-hoc sociallifestyle region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(sociallifestyle ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(sociallifestyle ~ region1)

pwc

# ANOVA and Post-hoc religious_principle political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(religious_principle ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(religious_principle ~ pol_side2)

pwc

# ANOVA and Post-hoc religious_principle region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(religious_principle ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(religious_principle ~ region1)

pwc

# ANOVA and Post-hoc immigrate_policy political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(immigrate_policy ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(immigrate_policy ~ pol_side2)

pwc

# ANOVA and Post-hoc immigrate_policy region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(immigrate_policy ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(immigrate_policy ~ region1)

pwc

# ANOVA and Post-hoc multiculturalism political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(multiculturalism ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(multiculturalism ~ pol_side2)

pwc

# ANOVA and Post-hoc multiculturalism region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(multiculturalism ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(multiculturalism ~ region1)

pwc

# ANOVA and Post-hoc urban_rural political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(urban_rural ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(urban_rural ~ pol_side2)

pwc

# ANOVA and Post-hoc urban_rural region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(urban_rural ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(urban_rural ~ region1)

pwc

# ANOVA and Post-hoc regions political sides
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(regions ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(regions ~ pol_side2)

pwc

# ANOVA and Post-hoc regions region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(regions ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(regions ~ region1)

pwc

# ANOVA and Post-hoc ethnic_minorities political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(ethnic_minorities ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(ethnic_minorities ~ pol_side2)

pwc

# ANOVA and Post-hoc ethnic_minorities region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(ethnic_minorities ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(ethnic_minorities ~ region1)

pwc

# ANOVA and Post-hoc nationalism political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(nationalism ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(nationalism ~ pol_side2)

pwc

# ANOVA and Post-hoc nationalism region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(nationalism ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(nationalism ~ region1)

pwc

# ANOVA and Post-hoc antielite_salience political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(antielite_salience ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(antielite_salience ~ pol_side2)

pwc

# ANOVA and Post-hoc antielite_salience region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(antielite_salience ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(antielite_salience ~ region1)

pwc

# ANOVA and Post-hoc corrupt_salience political sides
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(corrupt_salience ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(corrupt_salience ~ pol_side2)

pwc

# ANOVA and Post-hoc corrupt_salience region - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(corrupt_salience ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(corrupt_salience ~ region1)

pwc

# ANOVA and Post-hoc redistributon political sides - SIGNIFICANT
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(redistribution ~ pol_side2)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(redistribution ~ pol_side2)

pwc

main_data_2014 %>% 
  +   group_by(pol_side2) %>%
  +   dplyr::summarize(Mean = mean(redistribution, na.rm=TRUE), 
                       +                    median = median(redistribution, na.rm = TRUE))


# ANOVA and Post-hoc redistributon region
res.aov <- main_data_2014 %>% 
  rstatix::anova_test(redistribution ~ region1)

res.aov

pwc <- main_data_2014 %>% 
  rstatix::tukey_hsd(redistribution ~ region1)

pwc
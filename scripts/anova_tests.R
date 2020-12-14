library(tidyverse)
library(ggpubr)
library(rstatix)
library(purrr)

analysis_data <- main_data_2014 %>% 
  dplyr::filter(country != 'tur') %>% 
  dplyr::select(-c(country, party_name, lrgen, eu_benefit)) %>% 
  dplyr::select(party_code, pol_side1, pol_side2, populist, region1, region2, 
                eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets, eu_ep, eu_turkey,   #1-7 scale variables first
                dplyr::everything())                                                              #0-10 scale variables second



test_data <- analysis_data %>% 
  dplyr::select(-c(party_code, pol_side1, pol_side2, populist, region2,
                   eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets, eu_ep, eu_turkey))

# Checking the outliers
test_data %>% 
  dplyr::group_by(region1) %>% 
  rstatix::identify_outliers(redistribution)

ggboxplot(test_data, x = "region1", y = "redistribution")

# Checking normality
model <- lm(redistribution ~ region1, data = test_data)

rstatix::shapiro_test(residuals(model))

test_data %>% 
  dplyr::group_by(region1) %>% 
  rstatix::shapiro_test(redistribution)

# Homogenity of variances
test_data %>% 
  rstatix::levene_test(redistribution ~ region1)

# ANOVA
res.aov <- test_data %>% 
  rstatix::anova_test(redistribution ~ region1)

# Post-hoc test
pwc <- test_data %>% 
  rstatix::tukey_hsd(redistribution ~ region1)

pwc
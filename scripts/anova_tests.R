library(tidyverse)
library(ggpubr)
library(rstatix)
library(purrr)

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
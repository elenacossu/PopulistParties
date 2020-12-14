library(tidyverse)
library(ggpubr)
library(rstatix)
library(purrr)

analysis_data <- main_data %>% 
  dplyr::filter(country != 'tur') %>% 
  dplyr::select(-c(country, party_name, lrgen, eu_benefit)) %>% 
  dplyr::select(party_code, pol_side1, pol_side2, populist, region1, region2, 
                eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets, eu_ep, eu_turkey,   #1-7 scale variables first
                dplyr::everything())                                                              #0-10 scale variables second



test_data <- analysis_data %>% 
  dplyr::select(-c(party_code, pol_side1, pol_side2, populist, region2,
                   eu_position, eu_intmark, eu_cohesion, eu_foreign, eu_budgets, eu_ep, eu_turkey))


test_fun <- function(data, var, subse){
  
  data %>% 
    dplyr::group_by(as.name(subse)) %>% 
    rstatix::shapiro_test(as.name(var))
}



test_fun2 <- function(var) {
  var %>% 
    paste(., '~ region1') %>% 
    as.formula(.) %>% 
    lm(., data = test_data) %>% 
    residuals(.) %>% 
    rstatix::shapiro_test(.) %>% 
    dplyr::mutate(variable = var,
                  statistic = round(statistic, 3),
                  p.value = round(p.value, 3))
}

test_varlist <- purrr::set_names(names(test_data)[-1])

models <- test_varlist %>% 
  purrr::map(test_fun2)


x <- 'spendvtax'

test_data %>% dplyr::group_by(region1) %>% rstatix::shapiro_test(x)

var <- 'spendvtax'

hello <- as.name(var)

test_data %>% 
  dplyr::group_by(region1) %>% 
  rstatix::shapiro_test(hello)


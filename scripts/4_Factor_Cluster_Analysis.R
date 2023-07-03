library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ggrepel)

# dataset for analysis
used_parties_2019 <- main_data_2019 %>%
  select(party_code,
         eu_position,
         eu_salience,
         eu_dissent,
         eu_intmark,
         eu_cohesion,
         eu_foreign,
         eu_budgets,
         galtan,
         galtan_salience,
         lrecon,
         lrecon_salience,
         spendvtax,
         deregulation,
         redistribution,
         econ_interven,
         civlib_laworder,
         sociallifestyle,
         immigrate_policy,
         multiculturalism,
         urban_rural,
         regions,
         ethnic_minorities,
         nationalism,
         antielite_salience,
         corrupt_salience) %>%
  dplyr::mutate(na_ans = rowSums(is.na(.))) %>%
  dplyr::filter(na_ans == 0) %>%
  dplyr::select(-na_ans) %>%
  dplyr::select(party_code) %>%
  dplyr::pull()

# dataset for analysis
main_data_2019_clean <- main_data_2019 %>%
  select(party_code,
         eu_position,
         eu_salience,
         eu_dissent,
         eu_intmark,
         eu_cohesion,
         eu_foreign,
         eu_budgets,
         galtan,
         galtan_salience,
         lrecon,
         lrecon_salience,
         spendvtax,
         deregulation,
         redistribution,
         econ_interven,
         civlib_laworder,
         sociallifestyle,
         immigrate_policy,
         multiculturalism,
         urban_rural,
         regions,
         ethnic_minorities,
         nationalism,
         antielite_salience,
         corrupt_salience) %>%
  dplyr::mutate(na_ans = rowSums(is.na(.))) %>%
  dplyr::filter(na_ans == 0) %>%
  dplyr::select(-na_ans)

#creating correlation matrix

main_data_2019_cor <- cor(main_data_2019_clean %>%
                            dplyr::select(-party_code) %>%
                            as.matrix(), 
                          use = "complete.obs") 

corrplot::corrplot(main_data_2019_cor, type = "upper")

#factr analysis

cortest.bartlett(main_data_2019_cor, nrow(main_data_2019_clean))

KMO(main_data_2019_clean %>%
      as.matrix())

main_data_2019_screeplot <- psych::fa.parallel(main_data_2019_cor, 
                                               n.obs = nrow(main_data_2019_clean))

main_data_2019_pca <- psych::principal(main_data_2019_cor, 
                                       nfactors = 5, 
                                       rotate = "varimax",
                                       scores = TRUE)

print(main_data_2019_pca$loadings, cutoff = 0.39, sort = TRUE)

main_2019_pcascores <- psych::factor.scores(main_data_2019_clean %>% dplyr::select(-party_code),
                                            f = main_data_2019_pca$loadings,
                                            method = "Bartlett")

#cluster analysis

set.seed(1234)
clust_analysis_2019 <- kmeans(main_2019_pcascores$scores, 
                              centers = 4, 
                              iter.max = 50, 
                              nstart = 1)

# saving cluster membership

main_data_2019_with_clust <- main_data_2019 %>%
  dplyr::filter(party_code %in% used_parties_2019) %>%
  dplyr::bind_cols(., clust_analysis_2019$cluster) %>%
  dplyr::rename(clusters = `...36`) %>%
  dplyr::mutate(clusters = as.factor(clusters))

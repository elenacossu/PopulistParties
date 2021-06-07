library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ggrepel)

# dataset for analysis
main_data_2019_clean <- main_data_2019 %>%
  select(eu_position,
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

main_data_2019_matrix <- main_data_2019_clean %>% 
  select_if(is.numeric) %>%  
  as.matrix()

KMO(main_data_2019_matrix)

#creating correlation matrix

main_data_2019_cor <- cor(main_data_2019_clean %>%
                            as.matrix(), 
                          use = "complete.obs") 

eigen(main_data_2019_cor)

corrplot::corrplot(main_data_2019_cor, type = "upper")

nrow(main_data_2019)

cortest.bartlett(main_data_2019_cor, 262)

#factr analysis

main_data_2019_screeplot <- psych::fa.parallel(main_data_2019_cor, 
                                               n.obs = nrow(main_data_2019_clean))

main_data_2019_pca <- psych::principal(main_data_2019_cor, 
                                       nfactors = 5, 
                                       rotate = "varimax",
                                       scores = TRUE)

print(main_data_2019_pca$loadings, cutoff = 0.39, sort = TRUE) #%>%
  #write.csv(., "/Users/Elena/Desktop/pca_loadings.csv")

test <- psych::fa(main_data_2019_cor, 
                  nfactors = 5,
                  rotate = "varimax",
                  scores = "Bartlett")

print(test$loadings, cutoff = 0.2, sort = TRUE)

test_pcascores <- psych::factor.scores(main_data_2019_clean, 
                                       f = main_data_2019_pca$loadings,
                                       method = "Bartlett")

scores_df <- test_pcascores$scores %>%
  as.data.frame()

clust_df_2019 <- main_data_2019_clean %>%
  dplyr::bind_cols(., scores_df)

#cluster analysis

set.seed(1234)

clust_analysis_2019 <- kmeans(scores_df, 4, iter.max = 10, nstart = 1)

#----

set.seed(1234)

main_data_2019_comps <- tibble(pca_1 = main_data_2019_pca$ind$coord[,1],  
                    pca_2 = main_data_2019_pca$ind$coord[,2],
                    pca_3 = main_data_2019_pca$ind$coord[,3],
                    pca_4 = main_data_2019_pca$ind$coord[,4])

fviz_pca_ind(cee_pca, 
             title = "Clustered Parties - PCA", 
             habillage=clusters_as_factor)

fviz_pca_biplot(main_data_2019_pca,  geom = "text")

main_data_2019 <- main_data_2019 %>%
  mutate(cluster=clusters_as_factor)

ggplot(main_data_2019, aes(cluster, pol_side2, color=cluster)) +
  geom_point() +
  geom_text_repel(aes(label=countrycode), show.legend=FALSE) +
  labs(x="Cluster", y="Pol Side")


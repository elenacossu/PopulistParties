analysis_data_2019 <- main_data_2019_with_clust %>%
  dplyr::select(party_code,
                pol_side2,
                clusters,
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
                corrupt_salience)

# total index means

index_means_total <- analysis_data_2019 %>%
  dplyr::select(-party_code) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = 'attribute', values_to = 'mean') %>% 
  dplyr::rename(Total = mean)

# index means with political sides with total

pol_sides_means <- analysis_data_2019 %>%
  dplyr::select(-party_code) %>%
  dplyr::group_by(pol_side2) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-pol_side2, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = pol_side2, values_from = mean) %>% 
  dplyr::left_join(., index_means_total, by = "attribute")


# index means wth clusters

clusters_means <- analysis_data_2019 %>%
  dplyr::select(-party_code) %>%
  dplyr::group_by(clusters) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  tidyr::pivot_longer(-clusters, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = clusters, values_from = mean) %>% 
  dplyr::left_join(., index_means_total, by = "attribute") %>% 
  dplyr::rename(Cluster_1 = `1`, 
                Cluster_2 = `2`, 
                Cluster_3 = `3`, 
                Cluster_4 = `4`)


# anova using pol sides

pol_sides_manova <- manova(cbind(eu_position,
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
                                 corrupt_salience) ~ pol_side2,
  data = analysis_data_2019)

pol_sides_manova_results <- summary.aov(pol_sides_manova)

pol_side_manova_table <- data.frame(index = as.character(),
                                   f_value = as.numeric(),
                                   p_value = as.numeric(),
                                   stringsAsFactors = FALSE)

for (i in 1:length(pol_sides_manova_results)) {
  pol_side_manova_table[i,2] <- pol_sides_manova_results[[i]][[4]][1]
  pol_side_manova_table[i,3] <- pol_sides_manova_results[[i]][[5]][1]
  pol_side_manova_table[i,1] <- names(pol_sides_manova_results)[i]
}

pol_side_manova_table <- pol_side_manova_table %>% 
  dplyr::mutate(index = stringr::str_replace(index, ' Response ', '')) %>% 
  dplyr::mutate(stars = dplyr::case_when(p_value <= 0.001 ~ "***",
                                        p_value <= 0.01 ~ "**", 
                                        p_value <= 0.05 ~ "*",
                                        TRUE ~ ""))


# anova using clusters

clusters_manova <- manova(cbind(eu_position,
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
                                 corrupt_salience) ~ clusters,
                           data = analysis_data_2019)


clusters_manova_results <- summary.aov(clusters_manova)


clusters_manova_table <- data.frame(index = as.character(),
                                    f_value = as.numeric(),
                                    p_value = as.numeric(),
                                    stringsAsFactors = FALSE)

for (i in 1:length(clusters_manova_results)) {
  clusters_manova_table[i,2] <- clusters_manova_results[[i]][[4]][1]
  clusters_manova_table[i,3] <- clusters_manova_results[[i]][[5]][1]
  clusters_manova_table[i,1] <- names(clusters_manova_results)[i]
}

clusters_manova_table <- clusters_manova_table %>% 
  dplyr::mutate(index = stringr::str_replace(index, ' Response ', '')) %>% 
  dplyr::mutate(stars = dplyr::case_when(p_value <= 0.001 ~ "***",
                                         p_value <= 0.01 ~ "**", 
                                         p_value <= 0.05 ~ "*",
                                         TRUE ~ ""))

# Joining pol side means and anova results
pol_side_desc_and_manova_results <- pol_sides_means %>% 
  dplyr::left_join(., pol_side_manova_table, by = c("attribute" = "index"))


# Joining cluster means and anova results
clusters_desc_and_manova_results <- clusters_means %>% 
  dplyr::left_join(., clusters_manova_table, by = c("attribute" = "index"))
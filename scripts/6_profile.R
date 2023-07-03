profile_data_2019 <- analysis_data_2019 %>%
  left_join(., 
            main_data_2019  %>%
              dplyr::select(party_code, lrgen, region1), 
            by = "party_code")

profile_data_2019 %>% 
  dplyr::select(clusters, lrgen)  %>%
  dplyr::group_by(clusters) %>%
  dplyr::summarise(mean = mean(lrgen))

profile_data_2019 %>% 
  dplyr::select(-party_code, -pol_side2, -region1)  %>%
  dplyr::group_by(clusters) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(-clusters, names_to = 'attribute', values_to = 'mean') %>% 
  tidyr::pivot_wider(names_from = clusters, values_from = mean) %>%
  View()

clusters_means <- analysis_data_2019 %>%
  dplyr::select(-party_code) %>%
  dplyr::group_by(clusters) %>% 
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)
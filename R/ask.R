

dfs %>%
  filter(str_detect(text2, 'teach')) %>%
  filter(str_detect(text2, 'nothing')) %>% View()

library(tidyverse)

read_csv("hate_crime.csv") %>%
  filter(data_year == "2021") ->
  data

data


library(tidyverse)

d <- read_csv2("data/field_data/ITV_grids_heights.csv")

# First Tomst id's and 

d %>% slice(1:2) %>% select(plot, !starts_with("X")) -> d1
names(d1)
d1 %>% pivot_longer(cols = a1:f25) %>% 
  pivot_wider(id_cols = name, names_from = plot, values_from = value) %>% 
  rename(plot = name) -> d1

d1 %>% mutate(TMS = ifelse(TMS == 94217915, 94217015, TMS)) -> d1

write_csv(d1 %>% select(-medianH), "data/tomst_ids.csv")

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

# Species data
d %>% slice(3:nrow(d)) -> d2

# Change names
names(d2)[-1] <- ifelse(grepl("X", names(d2)[-1]), paste0(lag(names(d2)[-1]),"_flowers"), paste0(names(d2)[-1],"_height"))

d2 %>% slice(2:nrow(d2)) %>%
  group_by(plot) %>% 
  mutate(individual = seq_along(plot)) %>% 
  relocate(plot, individual) %>% 
  mutate(across(a1_height:f25_flowers, as.numeric)) %>% 
  pivot_longer(cols = a1_height:f25_flowers) %>% 
  rename(species = plot) %>% 
  mutate(plot = unlist(lapply(name, function(x) strsplit(x, "_")[[1]][1])),
         var = unlist(lapply(name, function(x) strsplit(x, "_")[[1]][2]))) %>% 
  select(plot, species, individual, var, value) %>% 
  filter(!is.na(value)) -> d2

d1 %>% select(-TMS) %>% 
  rename(value = medianH) %>% 
  mutate(var = "vegetation_height",
         species = "ALL",
         value = as.numeric(value)) %>% 
  bind_rows(., d2) %>% 
  arrange(plot, species, individual) %>% 
  select(plot, species, individual, var, value) -> d2

write_csv(d2, "data/heights.csv")

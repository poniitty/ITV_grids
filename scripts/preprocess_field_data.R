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

d2 %>% filter(species == "ALL") %>% 
  rename(veg_median_h = value) %>% 
  select(plot, veg_median_h) %>% 
  mutate(plot = toupper(plot)) %>% 
  write_csv("data/median_heights.csv")

d2 %>% filter(species != "ALL") %>% 
  pivot_wider(id_cols = plot:individual, names_from = var, values_from = value) %>% 
  mutate(plot = toupper(plot)) %>% 
  write_csv("data/heights.csv")

# Combine heights with leaf traits

d3 <- read_csv("data/ITV_leaf_traits.csv") %>% 
  mutate(leaf_area = leaf_area*100,
         total.leaf.area = total.leaf.area*100,
         sd_la_filt = sd_la_filt*100,
         SLA = SLA/10) # To the same units as in TRY data

d2 %>% filter(var != "vegetation_height") %>% 
  mutate(plot = toupper(plot)) %>% 
  pivot_wider(id_cols = plot:individual, names_from = var, values_from = value) %>% 
  group_by(plot, species) %>% 
  summarise(height_sd = sd(height),
            height = mean(height),
            flowers = mean(flowers)) -> d2

d4 <- full_join(d3 %>% rename(plot = site) %>% select(-study_design,-year), 
                d2 %>% rename(abbr = species)) %>% 
  relocate(height, leaf_area, SLA, LDMC, .after = n_leaf) %>% 
  filter(!is.na(species))

d4 %>% group_by(species) %>% 
  select(height:LDMC) %>% 
  summarize(h_la = cor(height, leaf_area, use = "pairwise.complete.obs"),
            sla_ldmc = cor(SLA, LDMC, use = "pairwise.complete.obs"),
            h_sla = cor(height, SLA, use = "pairwise.complete.obs"),
            la_ldmc = cor(leaf_area, LDMC, use = "pairwise.complete.obs"))

write_csv(d4, "output/all_traits.csv")

######################################################################################
# LA

d <- read_csv("../Fennoscandia_plant_traits/trait_data/Kilpis_all_leaf_areas.csv") %>% 
  filter(substr(site, 2, 2) %in% (0:9)) %>% 
  rename(leaf_area = Area,
         plot = site) %>% 
  mutate(leaf_area = leaf_area*100)

write_csv(d, "data/leaf_areas.csv")




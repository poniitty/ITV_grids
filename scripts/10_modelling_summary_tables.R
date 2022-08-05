###########################################################################
# Summary tables for the modelling results


library(tidyverse)
library(bayesplot)
library(tidybayes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(Cairo)

r2s <- read_csv("output/r2s.csv") %>% 
  mutate(trait = factor(trait, 
                        levels = c("height","LA",
                                   "LDMC","SLA"), 
                        labels = c("Height","Leaf area",
                                   "LDMC","SLA"))) %>% 
  mutate(species = factor(species, 
                          levels = c("BISVIV","SOLVIR","BETNAN",
                                     "VACMYR","VACULI","VACVIT"), 
                          labels = c("Bistorta vivipara", "Solidago virgaurea", "Betula nana",
                                     "Vaccinium myrtillus","Vaccinium uliginosum","Vaccinium vitis-idaea")))
slp <- read_csv("output/slopes.csv") %>% 
  mutate(.variable = factor(.variable, 
                            levels = c("b_moist_mean_7_8","b_snow_depth","b_scd",
                                       "b_T3_mean_7_8","b_T1_mean_7_8"), 
                            labels = c("Soil moisture", "Snow depth", "Snow melt.",
                                       "Air temp.","Soil temp."))) %>% 
  mutate(trait = factor(trait, 
                        levels = c("height","LA",
                                   "LDMC","SLA"), 
                        labels = c("Plant height","Leaf area",
                                   "LDMC","SLA"))) %>% 
  mutate(species = factor(species, 
                          levels = c("BISVIV","SOLVIR","BETNAN",
                                     "VACMYR","VACULI","VACVIT"), 
                          labels = c("Bistorta vivipara","Solidago virgaurea",
                                     "Betula nana","Vaccinium myrtillus",
                                     "Vaccinium uliginosum","Vaccinium vitis-idaea")))

# TABLE OF POSTERIOR SAMPLES

slp <- read_csv("output/slopes.csv") %>% 
  mutate(.variable = factor(.variable, 
                            levels = c("b_moist_mean_7_8","b_snow_depth","b_scd",
                                       "b_T3_mean_7_8","b_T1_mean_7_8"), 
                            labels = c("Soil moisture", "Snow depth", "Snow melting day",
                                       "Air temperature","Soil temperature"))) %>% 
  mutate(trait = factor(trait, 
                        levels = c("height","LA",
                                   "LDMC","SLA"), 
                        labels = c("Plant height","Leaf area",
                                   "LDMC","SLA"))) %>% 
  mutate(species = factor(species, 
                          levels = c("BISVIV","SOLVIR","BETNAN",
                                     "VACMYR","VACULI","VACVIT"), 
                          labels = c("Bistorta vivipara","Solidago virgaurea",
                                     "Betula nana","Vaccinium myrtillus",
                                     "Vaccinium uliginosum","Vaccinium vitis-idaea")))


sgnfs <- slp %>% 
  group_by(trait, species, .variable) %>% 
  summarise(l95 = median(.value)) %>% 
  mutate(sgnf = ifelse(l95 > 0, 1, 0),
         sgnf = ifelse(u95 < 0, -1, sgnf))

slp %>% 
  group_by(.variable, trait, species) %>% 
  summarise(Median = median(.value),
            `Lower_95%` = quantile(.value, probs = 0.025),
            `Upper_95%` = quantile(.value, probs = 0.975),
            lower = mean(.value < 0),
            higher = mean(.value > 0)) %>% 
  rowwise() %>% 
  mutate(pers = abs(lower-higher)) %>% 
  ungroup() %>% 
  select(-lower, -higher) %>% 
  mutate(Parameter = "β") %>% 
  relocate(Parameter, .after = species) %>% 
  relocate(trait) %>% 
  rename(Predictor = .variable,
         Trait = trait,
         Species = species) %>% 
  arrange(Trait, Predictor, Species) %>% 
  mutate(across(Median:pers, ~round(.x, 4))) %>% 
  write_csv("output/Model_parameters_summary_table.csv")

# Table
r2s %>% 
  filter(type == "fit_total" | type == "fit_fixed") %>% 
  relocate(trait, species, type, n_obs) %>% 
  rename(Trait = trait,
         Species = species,
         Type = type) %>% 
  arrange(Trait, Species, Type) %>% 
  mutate(across(Estimate:Q97.5, ~round(.x, 3))) %>% 
  write_csv("output/R2_table.csv")

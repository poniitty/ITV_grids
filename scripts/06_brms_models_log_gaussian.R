###########################################################################
# Gaussian trait models


library(brms)
library(tidyverse)
library(performance, lib.loc = "/projappl/project_2003061/Rpackages/")
library(bayesplot)
library(tidybayes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(posterior)

setwd("/projappl/project_2003061/repos/ITV_grids")

if(!dir.exists("./models")){
  dir.create("./models")
}

########################################################
# GAUSSIAN MODELS, RESPONSES LOG-SCALED

warmup <- 4000
iter   <- 8000 
chains <- 4
inits  <- "random"
seed   <- 123
cores  <- 4
thin <- 2
adapt_delta <- 0.99
max_treedepth <- 20
logs <- c("height","leaf_area","LDMC","SLA")
mod_family <- "gaussian"


ed <- read_csv("output/thermal_variables.csv")%>% 
  # mutate(snow_depth = log(snow_depth+1)) %>% 
  rowwise() %>% 
  mutate(moist_mean_7_8 = mean(c(moist_mean_7, moist_mean_8)),
         T3_mean_7_8 = mean (c(T3_mean_7,T3_mean_8)),
         T1_mean_7_8 = mean (c(T1_mean_7,T1_mean_8))) %>% 
  relocate(c(moist_mean_7_8, T3_mean_7_8, T1_mean_7_8), .after = snow_depth) %>% 
  ungroup() %>% 
  mutate(across(snow_depth:moist_absmin_9, ~as.numeric(scale(.x))))

rs <- read_csv("output/rs_variables.csv") %>% 
  mutate(across(scd:ndvi, ~as.numeric(scale(.x))))

# HEIGHT

include_flowers <- c("BISVIV","SOLVIR")

d <- read_csv("data/heights.csv") %>% 
  rename(id = plot)

if("height" %in% logs){
  d <- d %>% 
    mutate(height = log(height))
  is_log <- "log"
} else {
  is_log <- "natural"
}

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>% 
  mutate(flowers = factor(flowers, levels = c("0","1"), labels = c("no", "yes"))) %>% 
  select(id:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  # i <- "BETNAN"
  
  # if(i %in% c("BETNAN","VACMYR","VACULI")){
  #   mod_family <- "skew_normal"
  # }
  # if(i %in% c("BISVIV")){
  #   mod_family <- "student"
  # }
  
  if(i %in% include_flowers){
    m1 <- brm(height ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  } else {
    m1 <- brm(height ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + (1|grid/id), 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  }
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste("./models/height",i,mod_family,is_log,".rds", sep = "_"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# LEAF AREA

d <- read_csv("data/leaf_areas.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr) %>% 
  filter(leaf_area > 0.1) # To get rid of two clearly erroneous values

if("leaf_area" %in% logs){
  d <- d  %>% 
    mutate(leaf_area = log(leaf_area))
  is_log <- "log"
} else {
  is_log <- "natural"
}

hf <- read_csv("data/heights.csv") %>% 
  rename(id = plot) %>% 
  group_by(id, species) %>% 
  summarise(flowers = mean(flowers))

d <- left_join(d, hf) %>% 
  left_join(., ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>%
  select(id:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  
  # if(i %in% c("BISVIV","SOLVIR","VACMYR")){
  #   mod_family <- "skew_normal"
  # }
  # if(i %in% c("BETNAN")){
  #   mod_family <- "student"
  # }
  
  m1 <- brm(leaf_area ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + (1|grid/id), 
            data   = d %>% filter(species == i),
            family = mod_family,
            warmup = warmup,
            iter   = iter,
            chains = chains,
            inits  = inits,
            thin = thin,
            seed  = seed,
            cores = cores,
            # sample_prior = TRUE,
            refresh = 0,
            silent = 2,
            control = list(adapt_delta = adapt_delta,
                           max_treedepth = max_treedepth))
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste("./models/LA",i,mod_family,is_log,".rds", sep = "_"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# SPECIFIC LEAF AREA

d <- read_csv("output/all_traits.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr)

if("SLA" %in% logs){
  d <- d %>% 
    mutate(SLA = log(SLA))
  is_log <- "log"
} else {
  is_log <- "natural"
}

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>%
  select(grid, id:LDMC, flowers, snow_depth:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  
  # if(i %in% c("SOLVIR","VACMYR","BETNAN")){
  #   mod_family <- "student"
  # }
  
  if(i == "BISVIV"){
    m1 <- brm(SLA | weights(n_inds) ~ snow_depth + moist_mean_7_8 + T3_mean_7_8, 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  } else {
    m1 <- brm(SLA | weights(n_inds) ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + (1|grid), 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  }
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste("./models/SLA",i,mod_family,is_log,".rds", sep = "_"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}


# LEAF DRY MATTER CONTENT

d <- read_csv("output/all_traits.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr)

if("LDMC" %in% logs){
  d <- d %>% 
    mutate(LDMC = log(LDMC))
  is_log <- "log"
} else {
  is_log <- "natural"
}

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>%
  select(grid, id:LDMC, flowers, snow_depth:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  
  # if(i %in% c("SOLVIR","VACMYR")){
  #   mod_family <- "student"
  # }
  
  if(i == "BISVIV"){
    m1 <- brm(LDMC | weights(n_inds) ~ snow_depth + moist_mean_7_8 + T3_mean_7_8, 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  } else {
    m1 <- brm(LDMC | weights(n_inds) ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + (1|grid), 
              data   = d %>% filter(species == i),
              family = mod_family,
              warmup = warmup, 
              iter   = iter, 
              chains = chains, 
              inits  = inits,
              thin = thin,
              seed  = seed,
              cores = cores,
              # sample_prior = TRUE,
              refresh = 0,
              silent = 2,
              control = list(adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth))
  }
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste("./models/LDMC",i,mod_family,is_log,".rds", sep = "_"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}
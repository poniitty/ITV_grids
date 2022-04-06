# install.packages("brms")
library(brms)
library(tidyverse)
library(performance)
library(bayesplot)

if(!dir.exists("./models")){
  dir.create("./models")
}

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

d <- read_csv("data/heights.csv") %>% 
  rename(id = plot)

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>% 
  mutate(flowers = factor(flowers, levels = c("0","1"), labels = c("no", "yes"))) %>% 
  select(id:T1_mean_7_8, scd)

# HEIGHT

for(i in unique(d$species)){
  print(i)
  m1 <- brm(height | trunc(lb = 0) ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
            data   = d %>% filter(species == i),
            # family = "skew_normal",
            warmup = 1000, 
            iter   = 2000, 
            chains = 2, 
            inits  = "random",
            seed  = 123,
            cores = 1,
            sample_prior = TRUE,
            refresh = 0,
            silent = 2)
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste0("./models/height_",i,".rds"))
  
}

for(i in unique(d$species)){
  print(i)
  
  m1 <- readRDS(paste0("./models/height_",i,".rds"))
  
  # print(summary(m1))
  
  # mcmc_plot(m1, type = "trace")
  # mcmc_plot(m1, type = "hist")
  # mcmc_plot(m1, type = "intervals", variable = c("^b_scd",
  #                                                "^b_snow_depth",
  #                                                "^b_moist_mean_7_8",
  #                                                "^b_T3_mean_7_8",
  #                                                "^b_T1_mean_7_8",
  #                                                "^b_flowers"), regex = TRUE)
  # 
  # brms::pp_check(m1)
  # # plot(conditional_effects(m1, points = T, re_formula = NA), ask = FALSE)
  # 
  # print(bayes_R2(m1, re.form = NULL))
  # print(bayes_R2(m1, re.form = NA))
  # 
  print(loo_R2(m1, re.form = NULL, cores = 1))
  print(loo_R2(m1, re.form = NA, cores = 1))
  # 
  # r2_bayes(m1)
  # icc(m1)
  # model_performance(m1)
  
}





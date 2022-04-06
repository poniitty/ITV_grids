# install.packages("brms")
library(brms)
library(tidyverse)
library(performance)

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

# Height
d <- read_csv("data/heights.csv") %>% 
  rename(id = plot)

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>% 
  mutate(flowers = factor(flowers, levels = c("0","1"), labels = c("no", "yes"))) %>% 
  select(id:T1_mean_7_8, scd)

priors <- c(set_prior("normal(0,2)", class = "b", coef = "scd"),
            set_prior("normal(0,2)", class = "b", coef = "snow_depth"),
            set_prior("normal(0,2)", class = "b", coef = "moist_mean_7_8"),
            set_prior("normal(0,2)", class = "b", coef = "T3_mean_7_8"),
            set_prior("normal(0,2)", class = "b", coef = "T1_mean_7_8" ))

# VACMYR
m1 <- brm(height ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
          data   = d %>% filter(species == "VACMYR"),
          prior = priors,
          family = "skew_normal",
          warmup = 1000, 
          iter   = 2000, 
          chains = 2, 
          inits  = "random",
          seed  = 123,
          cores = 2,
          sample_prior = TRUE)

summary(m1)
plot(hypothesis(m1, "T3_mean_7_8 > 0"))

mcmc_plot(m1, type = "trace")
mcmc_plot(m1, type = "hist")
mcmc_plot(m1, type = "intervals", variable = c("^b_scd",
                                               "^b_snow_depth",
                                               "^b_moist_mean_7_8",
                                               "^b_T3_mean_7_8",
                                               "^b_T1_mean_7_8",
                                               "^b_flowers"), regex = TRUE)

brms::pp_check(m1, cores = 1)
# plot(conditional_effects(m1, points = T, re_formula = NA), ask = FALSE)

bayes_R2(m1, re.form = NULL)
bayes_R2(m1, re.form = NA)

loo_R2(m1, re.form = NULL, cores = 1)
loo_R2(m1, re.form = NA, cores = 1)

r2_bayes(m1)
icc(m1)
model_performance(m1)

# SOLVIR
m1 <- brm(height ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
          data   = d %>% filter(species == "SOLVIR"),
          prior = priors,
          family = "skew_normal",
          warmup = 1000, 
          iter   = 2000, 
          chains = 2, 
          inits  = "random",
          seed  = 123,
          cores = 2,
          sample_prior = TRUE)

summary(m1)
plot(hypothesis(m1, "T3_mean_7_8 > 0"))

mcmc_plot(m1, type = "trace")
mcmc_plot(m1, type = "hist")
mcmc_plot(m1, type = "intervals", variable = c("^b_scd",
                                               "^b_snow_depth",
                                               "^b_moist_mean_7_8",
                                               "^b_T3_mean_7_8",
                                               "^b_T1_mean_7_8",
                                               "^b_flowers"), regex = TRUE)

brms::pp_check(m1)
# plot(conditional_effects(m1, points = T, re_formula = NA), ask = FALSE)

bayes_R2(m1, re.form = NULL)
bayes_R2(m1, re.form = NA)

loo_R2(m1, re.form = NULL, cores = 1)
loo_R2(m1, re.form = NA, cores = 1)

r2_bayes(m1)
icc(m1)
model_performance(m1)


# install.packages("tidybayes", lib = "/projappl/project_2003061/Rpackages/")
library(brms)
library(tidyverse)
library(performance, lib.loc = "/projappl/project_2003061/Rpackages/")
library(bayesplot)
library(tidybayes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(posterior)

if(!dir.exists("./models")){
  dir.create("./models")
}

warmup <- 5000
iter   <- 10000 
chains <- 4
inits  <- "random"
seed   <- 123
cores  <- 4
adapt_delta <- 0.99
max_treedepth <- 20

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

d <- read_csv("data/heights.csv") %>% 
  rename(id = plot)

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>% 
  mutate(flowers = factor(flowers, levels = c("0","1"), labels = c("no", "yes"))) %>% 
  select(id:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  # i <- "BETNAN"
  m1 <- brm(height ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
            data   = d %>% filter(species == i),
            family = "gamma",
            warmup = warmup, 
            iter   = iter, 
            chains = chains, 
            inits  = inits,
            seed  = seed,
            cores = cores,
            # sample_prior = TRUE,
            refresh = 0,
            silent = 2,
            control = list(adapt_delta = adapt_delta,
                           max_treedepth = max_treedepth))
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste0("./models/height_",i,".rds"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# LEAF AREA

d <- read_csv("data/leaf_areas.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr) %>% 
  filter(leaf_area > 0.1) # To get rid of two clearly erroneous values

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
  
  m1 <- brm(leaf_area ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid/id), 
            data   = d %>% filter(species == i),
            family = "gamma",
            warmup = warmup, 
            iter   = iter, 
            chains = chains, 
            inits  = inits,
            seed  = seed,
            cores = cores,
            # sample_prior = TRUE,
            refresh = 0,
            silent = 2,
            control = list(adapt_delta = adapt_delta,
                           max_treedepth = max_treedepth))
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste0("./models/LA_",i,".rds"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# SPECIFIC LEAF AREA

d <- read_csv("output/all_traits.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr)

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>%
  select(grid, id:LDMC, flowers, snow_depth:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  
  m1 <- brm(SLA | weights(n_inds) ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid), 
            data   = d %>% filter(species == i),
            family = "gamma",
            warmup = warmup, 
            iter   = iter, 
            chains = chains, 
            inits  = inits,
            seed  = seed,
            cores = cores,
            # sample_prior = TRUE,
            refresh = 0,
            silent = 2,
            control = list(adapt_delta = adapt_delta,
                           max_treedepth = max_treedepth))
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste0("./models/SLA_",i,".rds"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}


# LEAF DRY MATTER CONTENT

d <- read_csv("output/all_traits.csv") %>% 
  rename(id = plot) %>% 
  select(-species) %>% 
  rename(species = abbr)

d <- left_join(d, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species)) %>%
  select(grid, id:LDMC, flowers, snow_depth:T1_mean_7_8, scd)

for(i in unique(d$species)){
  print(i)
  
  m1 <- brm(LDMC | weights(n_inds) ~ scd + snow_depth + moist_mean_7_8 + T3_mean_7_8 + T1_mean_7_8 + flowers + (1|grid), 
            data   = d %>% filter(species == i),
            family = "gamma",
            warmup = warmup, 
            iter   = iter, 
            chains = chains, 
            inits  = inits,
            seed  = seed,
            cores = cores,
            # sample_prior = TRUE,
            refresh = 0,
            silent = 2,
            control = list(adapt_delta = adapt_delta,
                           max_treedepth = max_treedepth))
  
  # print(brms::pp_check(m1))
  
  saveRDS(object = m1, paste0("./models/LDMC_",i,".rds"))
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

############################################
# Gather results

files <- list.files("./models", pattern = ".rds$")

slopes <- tibble()
r2s <- tibble()
pdf("visuals/BRMS_diagnostics_GAMMA.pdf", 12, 10)
for(i in files){
  print(i)
  
  spec <- gsub(".rds","",str_split(i, "_")[[1]][2])
  trait <- str_split(i, "_")[[1]][1]
  
  m1 <- readRDS(paste0("./models/",i))
  
  slopes <- m1 %>%
    gather_draws(b_scd, b_snow_depth, b_moist_mean_7_8,
                 b_T3_mean_7_8,b_T1_mean_7_8) %>%
    mutate(trait = trait,
           species = spec) %>% 
    bind_rows(slopes, .)
  
  print(mcmc_plot(m1, type = "trace") + ggtitle(paste0(trait, " ", spec)))
  print(mcmc_plot(m1, type = "hist") + ggtitle(paste0(trait, " ", spec)))
  
  rhat_vals <- bayesplot::rhat(m1)
  print(mcmc_rhat(rhat_vals[1:10]) + theme_bw() + ggtitle(paste0(trait, " ", spec)))
  
  neff_vals <- neff_ratio(m1)
  print(mcmc_neff(neff_vals[1:10]) + theme_bw() + ggtitle(paste0(trait, " ", spec)))
  
  print(mcmc_acf(as_draws_df(m1) %>% select(starts_with("b_"))) + ggtitle(paste0(trait, " ", spec)))
  
  print(brms::pp_check(m1, ndraws = 100) + ggtitle(paste0(trait, " ", spec)))
  
  r2s <- bind_rows(r2s,
                   bind_rows(bayes_R2(m1, re.form = NULL) %>% as_tibble() %>% mutate(type = "fit_total"),
                             bayes_R2(m1, re.form = NA) %>% as_tibble() %>% mutate(type = "fit_fixed")) %>% 
                     mutate(species = spec, trait = trait,
                            n_obs = nrow(m1$data)),
                   bind_rows(loo_R2(m1, re.form = NULL) %>% as_tibble() %>% mutate(type = "loo_total"),
                             loo_R2(m1, re.form = NA) %>% as_tibble() %>% mutate(type = "loo_fixed")) %>% 
                     mutate(species = spec, trait = trait,
                            n_obs = nrow(m1$data)))
  
  m1 <- add_criterion(m1, c("loo"))
  
  print(tibble(pareto_k = m1$criteria$loo$diagnostics$pareto_k) %>% 
    ggplot(aes(x = pareto_k)) +
    geom_vline(xintercept = .5, linetype = 2) +
    stat_dots() + ggtitle(paste0(trait, " ", spec)))
  
}
dev.off()

write_csv(r2s, "output/r2s_GAMMA.csv")
write_csv(slopes, "output/slopes_GAMMA.csv")


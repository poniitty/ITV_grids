####################################################################3
# Gather results and plot model diagnostics


library(brms)
library(tidyverse)
library(performance, lib.loc = "/projappl/project_2003061/Rpackages/")
library(bayesplot)
library(tidybayes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(posterior)

res <- read_csv("output/model_weights.csv")

bests <- res %>% group_by(traits, species) %>% 
  slice_max(loo_weight) %>% as.data.frame()

files <- paste0(bests$mod, "_.rds")

slopes <- tibble()
r2s <- tibble()
pdf("visuals/BRMS_diagnostics.pdf", 12, 10)
for(i in files){
  print(i)
  
  spec <- gsub(".rds","",str_split(i, "_")[[1]][2])
  trait <- str_split(i, "_")[[1]][1]
  
  m1 <- readRDS(paste0("./models/",i))
  
  slopes <- m1 %>%
    gather_draws(`b_.*`, regex=TRUE) %>%
    filter(.variable %in% c("b_scd", "b_snow_depth", "b_moist_mean_7_8",
                            "b_T3_mean_7_8","b_T1_mean_7_8")) %>% 
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

write_csv(r2s, "output/r2s.csv")
write_csv(slopes, "output/slopes.csv")


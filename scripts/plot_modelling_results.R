library(brms)
library(tidyverse)
library(performance, lib.loc = "/projappl/project_2003061/Rpackages/")
library(bayesplot)
library(tidybayes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(posterior)

r2s <- read_csv("output/r2s_GAMMA.csv") %>% 
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
slp <- read_csv("output/slopes_GAMMA.csv") %>% 
  mutate(.variable = factor(.variable, 
                            levels = c("b_moist_mean_7_8","b_snow_depth","b_scd",
                                       "b_T3_mean_7_8","b_T1_mean_7_8"), 
                            labels = c("Soil moist.", "Snow depth", "Snow melt",
                                       "Air temp.","Soil temp."))) %>% 
  mutate(trait = factor(trait, 
                        levels = c("height","LA",
                                   "LDMC","SLA"), 
                        labels = c("Height","Leaf area",
                                   "LDMC","SLA"))) %>% 
  mutate(species = factor(species, 
                          levels = c("BISVIV","SOLVIR","BETNAN",
                                     "VACMYR","VACULI","VACVIT")))

# Effects / slope parameters

sgnfs <- slp %>% 
  group_by(trait, species, .variable) %>% 
  summarise(l95 = quantile(.value, probs = 0.025),
            u95 = quantile(.value, probs = 0.975)) %>% 
  mutate(sgnf = ifelse(l95 > 0, 1, 0),
         sgnf = ifelse(u95 < 0, -1, sgnf))

gg2 <- slp %>%
  left_join(., sgnfs) %>% 
  mutate(sgnf = factor(sgnf)) %>% 
  ggplot(aes(y = species, x = .value, color = sgnf)) +
  stat_pointinterval(point_size = 1.5) +
  scale_color_manual(values = c("blue","gray","red")) +
  geom_vline(xintercept = c(0), linetype = "dashed", color = "gray") +
  theme_ggdist() + ylab(NULL) + xlab("β") +
  theme(legend.position = "none") +
  facet_grid(.variable ~ trait, scales = "free") +
  scale_y_discrete(limits=rev) +
  theme(panel.spacing.y = unit(1, "lines"))

ggsave("visuals/slopes.pdf", plot = gg2, width = 20, height = 15, units = "cm")
ggsave("visuals/slopes.png", plot = gg2, width = 20, height = 15, units = "cm")

gg1 <- slp %>%
  left_join(., sgnfs) %>%  
  mutate(sgnf = factor(sgnf)) %>% 
  ggplot(aes(y = .variable, x = .value, color = sgnf)) +
  stat_pointinterval(point_size = 1.5) +
  scale_color_manual(values = c("blue","gray","red")) +
  geom_vline(xintercept = c(0), linetype = "dashed", color = "gray") +
  theme_ggdist() + ylab(NULL) + xlab("β") +
  theme(legend.position = "none") +
  facet_grid(species ~ trait, scales = "free") +
  scale_y_discrete(limits=rev) +
  theme(panel.spacing.y = unit(1, "lines"))

ggsave("visuals/slopes_by_species.pdf", plot = gg1, width = 20, height = 15, units = "cm")
ggsave("visuals/slopes_by_species.png", plot = gg1, width = 20, height = 15, units = "cm")

# R2

gg3 <- r2s %>% 
  filter(type == "fit_fixed") %>% 
  ggplot(aes(x = trait, y = Estimate)) +
  geom_hline(yintercept = c(0), color = "black") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "black", size = 0.2) +
  geom_crossbar(aes(ymin = Q2.5, ymax = Q97.5),
                fill = "gray90", fatten = 0, width = 0.2) + 
  geom_point(size = 2) +
  facet_wrap(vars(species)) +
  theme_ggdist() +
  geom_text(aes(label=n_obs, y = 1), vjust=0.15, size = 3) +
  ylab(bquote(R^2)) + xlab(NULL) +
  theme(strip.text = element_text(face = "italic"))

ggsave("visuals/R2s.pdf", plot = gg3, width = 20, height = 14, units = "cm")
ggsave("visuals/R2s.png", plot = gg3, width = 20, height = 14, units = "cm")

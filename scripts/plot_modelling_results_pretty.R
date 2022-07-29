# library(MetBrewer) # nice colour palettes
library(patchwork) # for nicer plots
#library(brms)
library(tidyverse)
#library(performance)
library(bayesplot)
library(tidybayes)
#library(posterior)

r2s <- read_csv("output/r2s.csv") %>% 
  mutate(trait = factor(trait, 
                        levels = c("height","LA",
                                   "LDMC","SLA"), 
                        labels = c("Plant height","Leaf area",
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
  scale_color_manual(values = c("deeppink","lightgrey","deeppink4")) +
  geom_vline(xintercept = c(0), linetype = "dashed", color = "grey") +
  theme_ggdist() + ylab(NULL) + xlab("β") +
  theme(legend.position = "none") +
  facet_grid(.variable ~ trait, scales = "free") +
  scale_y_discrete(limits=rev) +
  theme(panel.spacing.y = unit(1, "lines"),
        aspect.ratio = 1,
        axis.text.y = element_text(face = "italic"),
        strip.background = element_rect(fill = "ghostwhite"))

#ggsave("visuals/slopes.pdf", plot = gg2, width = 20, height = 15, units = "cm")
#ggsave("visuals/slopes.png", plot = gg2, width = 20, height = 15, units = "cm")

#save figure
dev.off()
pdf(file = "slopes.pdf", width = 7.48, height = 9.45)

gg2 + theme(plot.tag = element_text(size = 8))

dev.off()

gg1 <- slp %>%
  left_join(., sgnfs) %>%  
  mutate(sgnf = factor(sgnf)) %>% 
  ggplot(aes(y = .variable, x = .value, color = sgnf)) +
  stat_pointinterval(point_size = 1.5) +
  scale_color_manual(values = c("deeppink","lightgrey","deeppink4")) +
  geom_vline(xintercept = c(0), linetype = "dashed", color = "grey") +
  theme_ggdist() + ylab(NULL) + xlab("β") +
  theme(legend.position = "none") +
  facet_grid(species ~ trait, scales = "free") +
  scale_y_discrete(limits=rev) +
  theme(panel.spacing.y = unit(1, "lines"),
        aspect.ratio = 1,
        strip.text.y = element_text(face = "italic"),
        strip.background = element_rect(fill = "ghostwhite"))

#ggsave("visuals/slopes_by_species.pdf", plot = gg1, width = 20, height = 15, units = "cm")
#ggsave("visuals/slopes_by_species.png", plot = gg1, width = 20, height = 15, units = "cm")

#save figure
dev.off()
pdf(file="visuals/slopes_byspecies.pdf", width = 7.48, height = 9.45)

gg1 + theme(plot.tag = element_text(size = 8))

dev.off()

# R2
gg3 <- r2s %>% 
  filter(type == "fit_total" | type == "fit_fixed") %>% 
  ggplot(aes(x = trait, y = Estimate, group = type, fill = type)) +
  geom_hline(yintercept = c(0), color = "grey") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "grey", size = 0.2) +
  geom_crossbar(aes(ymin = Q2.5, ymax = Q97.5),
                colour=NA, fatten = 0, width = 0.2,
                position = position_dodge(width=0.35)) + 
  geom_point(size = 2, colour="black", position = position_dodge(width=0.35)) +
  scale_fill_manual(values = c("coral","mediumvioletred"),
                    labels = c("Fixed effects", "Fixed + random effects")) +
  facet_wrap(vars(species), nrow=2) +
  theme_ggdist() +
  geom_text(aes(label=n_obs, y = 1), vjust=1, size=3) +
  ylab(bquote(R^2)) + xlab(NULL) +
  theme(strip.text = element_text(face = "italic"),
        strip.background = element_rect(fill = "ghostwhite"),
        legend.position = "bottom",
        legend.title = element_blank())

#ggsave("visuals/R2s.pdf", plot = gg3, width = 20, height = 14, units = "cm")
#ggsave("visuals/R2s.png", plot = gg3, width = 20, height = 14, units = "cm")

#save figure
dev.off()
pdf(file="visuals/RS2.pdf", width = 7.48, height = 9.45)

gg3 + theme(plot.tag = element_text(size = 8))

dev.off()

gg1
gg2
gg3

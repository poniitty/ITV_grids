library(MetBrewer) # nice colour palettes. we use Hokusai3 for the species.
library(patchwork) # for nicer plots
library(tidyverse)
library(viridis)

# Read in local data

d <- read_csv("data/heights.csv") %>% 
  rename(value = height) %>% 
  mutate(trait = "height") %>% 
  select(-plot, -individual, -flowers)

d <- bind_rows(d,
               read_csv("data/leaf_areas.csv") %>% 
                 select(-species) %>% 
                 rename(species = abbr) %>% 
                 filter(leaf_area > 0.1) %>% 
                 select(-plot, -leaf_id) %>% 
                 rename(value = leaf_area) %>% 
                 mutate(trait = "leaf_area"))

d <- bind_rows(d,
               read_csv("output/all_traits.csv") %>% 
                 select(-species) %>% 
                 mutate(SLA = SLA/10) %>% 
                 rename(species = abbr) %>% 
                 select(species,SLA,LDMC) %>% 
                 pivot_longer(cols = SLA:LDMC, names_to = "trait"))

d$species <- recode_factor(d$species,
                        BISVIV = "Bistorta vivipara",
                        SOLVIR = "Solidago virgaurea",
                        BETNAN = "Betula nana",
                        VACMYR = "Vaccinium myrtillus",
                        VACULI = "Vaccinium uliginosum",
                        VACVIT = "Vaccinium vitis-idaea")

d <- d %>% 
  filter(complete.cases(.)) %>% 
  group_by(species, trait) %>% 
  mutate(l95 = quantile(value, probs = 0.025),
         u95 = quantile(value, probs = 0.975)) %>% 
  filter(value > l95 & value < u95) %>% 
  select(-l95, -u95)

d <- d %>% 
  mutate(scale = "Local")

# Read in Global data

dd <- read_csv("data/kilpis_ITV_species_traits.csv") %>% 
  filter(trait %in% c("height","LDMC","leaf_area","SLA")) %>% 
  mutate(canonical_species_name = ifelse(canonical_species_name == "Persicaria vivipara", "Bistorta vivipara", canonical_species_name)) %>% 
  rename(species = canonical_species_name) %>% 
  select(-canonical_name, -latitude, -longitude,-refid, -DatasetID,
         -DataID, -unit, -DataOrigin) %>% 
  mutate(value = ifelse(trait == "height", value*100, value)) %>% 
  mutate(value = ifelse(trait == "leaf_area", value/100, value))

dd <- dd %>% 
  mutate(species = factor(species, levels = c("Bistorta vivipara","Solidago virgaurea",
                                              "Betula nana","Vaccinium myrtillus",
                                              "Vaccinium uliginosum","Vaccinium vitis-idaea")))

dd <- dd %>% 
  group_by(species, trait) %>% 
  mutate(l95 = quantile(value, probs = 0.025),
         u95 = quantile(value, probs = 0.975)) %>% 
  filter(value > l95 & value < u95) %>% 
  select(-l95, -u95)

dd <- dd %>% 
  mutate(scale = "Global")

#############################################################################
# Combine dataset and plot

dd <- bind_rows(d, dd)

trait_names <- c(
  `height` = "Plant height (cm)",
  `leaf_area` = "Leaf area (cm^2)",
  `SLA` = "Specific leaf area (cm^2/g)",
  `LDMC` = "Leaf dry matter content (g/g)"
  
)

dd %>% 
  ggplot(aes(y = value, x = species, fill = species, colour = species,
             group = scale)) +
  geom_boxplot (notch=TRUE, notchwidth = 0.1, width=0.8, outlier.shape = NA, lwd=0.5, alpha=7/10) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2) +
  facet_wrap(vars(trait), scales = "free_y", labeller = as_labeller(trait_names)) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Trait value") +
  xlab(NULL) +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position="bottom",
    legend.text = element_text(face = "italic"))

perse <- dd %>% 
  group_by(trait) %>% 
  mutate(maxv = max(value)) %>% 
  group_by(species, trait, scale) %>% 
  summarise(range = max(value)-min(value),
            maxv = max(maxv)) %>% 
  pivot_wider(id_cols = c(species,trait,maxv), names_from = scale, values_from = range) %>% 
  mutate(diff = round(Local/Global*100)) %>% 
  mutate(diff = paste0(diff,"%")) %>% 
  mutate(inter = paste(species, "Global", sep = "_")) %>% 
  mutate(inter = factor(inter, levels = c("Bistorta vivipara_Global",
                                          "Solidago virgaurea_Global",
                                          "Betula nana_Global",
                                          "Vaccinium myrtillus_Global",
                                          "Vaccinium uliginosum_Global",
                                          "Vaccinium vitis-idaea_Global")))

geom_text(data = quants, aes(x = variable, y = quant, label = variable), size = 10)

hok2 <- unlist(lapply(as.character(met.brewer("Hokusai3", n = 6)), function(x){
  c("white", x)
}))

dd %>% 
  arrange(species, scale) %>% 
  mutate(inter = paste(species, scale, sep = "_")) %>% 
  mutate(inter = factor(inter, levels = c("Bistorta vivipara_Global","Bistorta vivipara_Local",
                                          "Solidago virgaurea_Global","Solidago virgaurea_Local",
                                          "Betula nana_Global","Betula nana_Local",
                                          "Vaccinium myrtillus_Global","Vaccinium myrtillus_Local",
                                          "Vaccinium uliginosum_Global","Vaccinium uliginosum_Local",
                                          "Vaccinium vitis-idaea_Global","Vaccinium vitis-idaea_Local"))) %>% 
  # left_join(., perse) %>% 
  ggplot(aes(y = value, x = inter, fill = inter, colour = species)) +
  geom_boxplot(notch=TRUE, notchwidth = 0.1, outlier.shape = NA, width=0.8, lwd=0.5, alpha=7/10) +
  geom_jitter(alpha = 0.3, width = 0.2, size = 2) +
  facet_wrap(vars(trait), scales = "free_y", labeller = as_labeller(trait_names)) +
  geom_text(data = perse, aes(label=diff, y = maxv, color = "black"), vjust=1, size=3, color = "black") +
  scale_fill_manual(values = hok2, guide = "none") +
  scale_colour_met_d("Hokusai3") +
  ylab("Trait value") +
  xlab(NULL) +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12),
    legend.position="bottom",
    legend.text = element_text(face = "italic"))


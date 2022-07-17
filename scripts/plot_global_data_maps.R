library(sf)
library(tidyverse)
library(cowplot)

d <- read_csv("data/kilpis_ITV_species_traits.csv")


d <- d %>% 
  drop_na(latitude, longitude) %>% 
  mutate(canonical_species_name = ifelse(canonical_species_name == "Persicaria vivipara","Bistorta vivipara",canonical_species_name))

unique(d$canonical_species_name)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify


g1 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Bistorta vivipara"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Bistorta vivipara", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

g2 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Solidago virgaurea"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Solidago virgaurea", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

g3 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Betula nana"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Betula nana", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

g4 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Vaccinium myrtillus"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Vaccinium myrtillus", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

g5 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Vaccinium uliginosum"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Vaccinium uliginosum", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

g6 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "gray70", colour = NA, size=0.5) + 
  geom_point(data = d %>% filter(canonical_species_name == "Vaccinium vitis-idaea"),
             aes(longitude, latitude), color = "blue",
             alpha = 0.7) +
  coord_map("mollweide", xlim=c(-180,180), ylim=c(20, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Vaccinium vitis-idaea", x="", y="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


pdf("visuals/global_trait_maps.pdf", 9, 13)
plot_grid(g1, g2, g3, g4, g5, g6,
          nrow = 6,
          align = "v")
dev.off()
pdf("visuals/global_trait_maps.pdf", 9, 13)
plot_grid(g1, g2, g3, g4, g5, g6,
          nrow = 6,
          align = "v")
dev.off()

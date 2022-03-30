library(MetBrewer) # nice colour palettes. we use Hokusai3 for the species.
library(patchwork) # for nicer plots
library(tidyverse)
library(viridis)

ed <- read_csv("output/thermal_variables.csv")
rs <- read_csv("output/rs_variables.csv")

rs <- rs %>% 
  arrange(grid,plot) %>% 
  mutate(col = rep(1:5, times = 30),
         row = rep(rep(1:5, each = 5), times = 6))

d <- left_join(rs, ed)

# plot
p_moi = d %>% select(grid, col, row, moist_mean) %>% 
  ggplot(aes(y = col, x = row, colour = moist_mean)) +
  geom_point(size =4) + 
  facet_wrap(vars(grid), nrow = 6) +
  scale_colour_met_c("Hokusai3") +
  ggtitle("Soil moisture") +
  ylab("") +
  xlab("") +
  labs(colour = "Soil moisture (VWC%)") +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom")

p_scd = d %>% select(grid, col, row, scd) %>% 
  ggplot(aes(y = col, x = row, colour = scd)) +
  geom_point(size =4) + 
  facet_wrap(vars(grid), nrow = 6) +
  scale_fill_met_c("Hokusai3") +
  scale_colour_met_c("Hokusai3") +
  ggtitle("Snow melting day") +
  ylab("") +
  xlab("") +
  labs(colour = "Snow melting day (DOY)") +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom")

p_airT = d %>% select(grid, col, row, T3_mean) %>% 
  ggplot(aes(y = col, x = row, colour = T3_mean)) +
  geom_point(size =4) + 
  facet_wrap(vars(grid), nrow = 6) +
  scale_fill_met_c("Hokusai3") +
  scale_colour_met_c("Hokusai3") +
  ggtitle("Air temperature") +
  ylab("") +
  xlab("") +
  labs(colour = "Temperature (°C)") +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom")

p_soiT = d %>% select(grid, col, row, T1_mean) %>% 
  ggplot(aes(y = col, x = row, colour = T1_mean)) +
  geom_point(size =4) + 
  facet_wrap(vars(grid), nrow = 6) +
  scale_fill_met_c("Hokusai3") +
  scale_colour_met_c("Hokusai3") +
  ggtitle("Soil temperature") +
  ylab("") +
  xlab("") +
  labs(colour = "Temperature (°C)") +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom")

# plot grid names only
p_names = ggplot() +
  annotate("text", x = 1, y =1.7, size = 3.5, fontface =2,
           label = "Forest, poor") +
  annotate("text", x = 1, y =1.6, size = 3.5, fontface =2,
           label = "Forest, rich") +
  annotate("text", x = 1, y =1.5, size = 3.5, fontface =2,
           label = "Ecotone, poor") +
  annotate("text", x = 1, y =1.4, size = 3.5, fontface =2,
           label = "Ecotone, rich") +
  annotate("text", x = 1, y =1.3, size = 3.5, fontface =2,
           label = "Tundra, poor") +
  annotate("text", x = 1, y =1.2, size = 3.5, fontface =2,
           label = "Tundra, rich") +
  theme_void()

#save figure
dev.off()
pdf(file="visuals/map_environment.pdf", width = 7.48, height = 9.45)

layout <- '
ABCDE
'

wrap_plots(A = p_names,
           B = p_moi,
           C = p_scd,
           D = p_airT,
           E = p_soiT,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()

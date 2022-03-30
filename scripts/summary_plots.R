library(MetBrewer) # nice colour palettes. we use Hokusai3 for the species.
library(patchwork) # for nicer plots
library(tidyverse)
library(viridis)

# check if colour palette is inclusive
MetBrewer::colorblind.friendly("Hokusai3") 

# read data
td <- read_csv("output/all_traits.csv") %>% 
  filter(!is.na(species)) %>% 
  mutate(grid = toupper(substr(plot, 1, 1)), 
         plot = parse_number(plot),
         id = paste0(grid, plot)) %>% 
  relocate(c(id,grid,plot)) %>% 
  arrange(grid, plot)

# rename and reorder species: first the forbs, then the shrubs
td$abbr<- recode_factor(td$abbr,
                        BISVIV = "Bistorta vivipara",
                        SOLVIR = "Solidago virgaurea",
                        BETNAN = "Betula nana",
                        VACMYR = "Vaccinium myrtillus",
                        VACULI = "Vaccinium uliginosum",
                        VACVIT = "Vaccinium vitis-idaea")

# give mroe informative trait names
trait_names <- c(
  `height` = "Plant height (cm)",
  `leaf_area` = "Leaf area (mm^2)",
  `SLA` = "Specific leaf area (mm^2/mg)",
  `LDMC` = "Leaf dry matter content (mg/mg)"
  
)

# plot
p1 = td %>% select(id, abbr, height:LDMC) %>% 
  pivot_longer(cols = height:LDMC, names_to = "trait") %>% 
  ggplot(aes(y = value, x = abbr, fill = abbr, colour = abbr)) +
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
    legend.position="bottom",
    legend.text = element_text(face = "italic"))

# save figure
dev.off()
pdf(file="visuals/ITV_summary.pdf", width = 7.48, height = 9.45)

p1 & theme(plot.tag = element_text(size = 8))

dev.off()


library(tidyverse)
library(viridis)

td <- read_csv("output/all_traits.csv") %>% 
  filter(!is.na(species)) %>% 
  mutate(grid = toupper(substr(plot, 1, 1)), 
         plot = parse_number(plot),
         id = paste0(grid, plot)) %>% 
  relocate(c(id,grid,plot)) %>% 
  arrange(grid, plot)

td %>% select(id, abbr, height:LDMC) %>% 
  pivot_longer(cols = height:LDMC, names_to = "trait") %>% 
  ggplot(aes(y = value, x = abbr, fill = abbr)) +
  geom_boxplot() +
  facet_wrap(vars(trait), scales = "free_y") +
  scale_fill_viridis_d() +
  ylab("Trait value") + xlab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

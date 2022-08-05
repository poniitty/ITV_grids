###########################################################################
# plot correlation matrices of trait and environmental data


library(MetBrewer) # nice colour palettes. we use Hokusai3 for the species.
library(patchwork) # for nicer plots
library(cowplot) # for nicer plots
library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)
library(MuMIn)
library(ggcorrplot) #correlogram
library(colormap) #generating color codes

td <- read_csv("output/all_traits.csv") %>% 
  filter(!is.na(species)) %>% 
  mutate(grid = toupper(substr(plot, 1, 1)), 
         plot = parse_number(plot),
         id = paste0(grid, plot)) %>% 
  relocate(c(id,grid,plot)) %>% 
  arrange(grid, plot)

ed <- read_csv("output/thermal_variables.csv")
rs <- read_csv("output/rs_variables.csv")

d <- left_join(td, ed) %>% 
  left_join(., rs) %>% 
  filter(!is.na(species))

# calculate mean values for relevant months
d %>% 
  rowwise() %>% 
  mutate(moist_mean_7_8 = mean(c(moist_mean_7, moist_mean_8)),
         T3_mean_7_8 = mean (c(T3_mean_7,T3_mean_8)),
         T1_mean_7_8 = mean (c(T1_mean_7,T1_mean_8))) %>% 
  relocate(c(moist_mean_7_8, T3_mean_7_8, T1_mean_7_8), .after = snow_depth) %>% 
  ungroup() -> d

# rename and reorder species: first the forbs, then the shrubs
d$abbr<- recode_factor(d$abbr,
                       BISVIV = "Bistorta vivipara",
                       SOLVIR = "Solidago virgaurea",
                       BETNAN = "Betula nana",
                       VACMYR = "Vaccinium myrtillus",
                       VACULI = "Vaccinium uliginosum",
                       VACVIT = "Vaccinium vitis-idaea")

names (d)
unique (d$species)

#subset by species
d %>%
  filter (abbr == "Bistorta vivipara") -> dat_bisviv

d %>%
  filter (abbr == "Solidago virgaurea") -> dat_solvir

d %>%
  filter (abbr == "Betula nana") -> dat_betnan

d %>%
  filter (abbr == "Vaccinium myrtillus") -> dat_vacmyr

d %>%
  filter (abbr == "Vaccinium uliginosum") -> dat_vaculi

d %>%
  filter (abbr == "Vaccinium vitis-idaea") -> dat_vacvit

# keep only the variables for correlation matrix
keep_bisviv = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

keep_solvir = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

keep_betnan = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

keep_vacmyr = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

keep_vaculi = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

keep_vacvit = c("moist_mean_7_8", "snow_depth", "scd", "T3_mean_7_8", "T1_mean_7_8",
                "height", "leaf_area", "LDMC", "SLA")

dat_bisviv = dat_bisviv[,keep_bisviv]
dat_solvir = dat_solvir[,keep_solvir]
dat_betnan = dat_betnan[,keep_betnan]
dat_vacmyr = dat_vacmyr[,keep_vacmyr]
dat_vaculi = dat_vaculi[,keep_vaculi]
dat_vacvit = dat_vacvit[,keep_vacvit]

# rename colunms
colnames(dat_bisviv) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

colnames(dat_solvir) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

colnames(dat_betnan) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

colnames(dat_vacmyr) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

colnames(dat_vaculi) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

colnames(dat_vacvit) = c("Soil moisture", "Snow depth", "Snow melt.", "Air temp.", "Soil temp.",
                         "Plant height", "Leaf area", "LDMC", "SLA")

# check data
head (dat_bisviv)
head (dat_solvir)
head (dat_betnan)
head (dat_vacmyr)
head (dat_vaculi)
head (dat_vacvit)

# compute a correlation matrix
corr_bisviv = round(cor(dat_bisviv, method="spearman", use = "complete.obs"), 2)
corr_solvir = round(cor(dat_solvir, method="spearman", use = "complete.obs"), 2)
corr_betnan = round(cor(dat_betnan, method="spearman", use = "complete.obs"), 2)
corr_vacmyr = round(cor(dat_vacmyr, method="spearman", use = "complete.obs"), 2)
corr_vaculi = round(cor(dat_vaculi, method="spearman", use = "complete.obs"), 2)
corr_vacvit = round(cor(dat_vacvit, method="spearman", use = "complete.obs"), 2)

# compute a matrix of correlation p-values
p.mat_bisviv <- cor_pmat(dat_bisviv, use = "complete.obs")
p.mat_solvir <- cor_pmat(dat_solvir, use = "complete.obs")
p.mat_betnan <- cor_pmat(dat_betnan, use = "complete.obs")
p.mat_vacmyr <- cor_pmat(dat_vacmyr, use = "complete.obs")
p.mat_vaculi <- cor_pmat(dat_vaculi, use = "complete.obs")
p.mat_vacvit <- cor_pmat(dat_vacvit, use = "complete.obs")

#check corour names
hok2 <- unlist(lapply(as.character(met.brewer("Hokusai3", n = 6)), function(x){
  c("white", x)
}))
hok2

# visualize the correlation matrix of bisviv
dev.off()
p_bisviv = ggcorrplot(corr_bisviv,
                 p.mat = p.mat_bisviv,
                 type="lower", hc.order = F,
                 outline.col = "white",
                 ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                          panel.background = element_blank(),
                                          axis.ticks.y=element_blank(),
                                          aspect.ratio = 1),
                 colors = c("grey50","white", "#d8d97a"),
                 insig = "blank",
                 lab = TRUE,
                 lab_col = c("black"),
                 lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_bisviv)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_bisviv)-0.5, colour="white", size=2) +
  ggtitle("Bistorta vivipara") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))

p_solvir = ggcorrplot(corr_solvir,
                      p.mat = p.mat_solvir,
                      type="lower", hc.order = F,
                      outline.col = "white",
                      ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                               panel.background = element_blank(),
                                               axis.ticks.y=element_blank(),
                                               aspect.ratio = 1),
                      colors = c("grey50","white", "#95c36e"),
                      insig = "blank",
                      lab = TRUE,
                      lab_col = c("black"),
                      lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_solvir)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_solvir)-0.5, colour="white", size=2) +
  ggtitle("Solidago virgaurea") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))

p_betnan = ggcorrplot(corr_betnan,
                      p.mat = p.mat_betnan,
                      type="lower", hc.order = F,
                      outline.col = "white",
                      ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                               panel.background = element_blank(),
                                               axis.ticks.y=element_blank(),
                                               aspect.ratio = 1),
                      colors = c("grey50","white", "#74c8c3"),
                      insig = "blank",
                      lab = TRUE,
                      lab_col = c("black"),
                      lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_betnan)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_betnan)-0.5, colour="white", size=2) +
  ggtitle("Betula nana") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))

p_vacmyr = ggcorrplot(corr_vacmyr,
                      p.mat = p.mat_vacmyr,
                      type="lower", hc.order = F,
                      outline.col = "white",
                      ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                               panel.background = element_blank(),
                                               axis.ticks.y=element_blank(),
                                               aspect.ratio = 1),
                      colors = c("grey50","white", "#5a97c1"),
                      insig = "blank",
                      lab = TRUE,
                      lab_col = c("black"),
                      lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_vacmyr)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_vacmyr)-0.5, colour="white", size=2) +
  ggtitle("Vaccinium myrtillus") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))

p_vaculi = ggcorrplot(corr_vaculi,
                      p.mat = p.mat_vaculi,
                      type="lower", hc.order = F,
                      outline.col = "white",
                      ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                               panel.background = element_blank(),
                                               axis.ticks.y=element_blank(),
                                               aspect.ratio = 1),
                      colors = c("grey50","white", "#295384"),
                      insig = "blank",
                      lab = TRUE,
                      lab_col = c("black"),
                      lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_vaculi)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_vaculi)-0.5, colour="white", size=2) +
  ggtitle("Vaccinium uliginosum") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))

p_vacvit = ggcorrplot(corr_vacvit,
                      p.mat = p.mat_vacvit,
                      type="lower", hc.order = F,
                      outline.col = "white",
                      ggtheme = ggplot2::theme(axis.ticks.x=element_blank(),
                                               panel.background = element_blank(),
                                               axis.ticks.y=element_blank(),
                                               aspect.ratio = 1),
                      colors = c("grey50","white", "#0a2e57"),
                      insig = "blank",
                      lab = TRUE,
                      lab_col = c("black"),
                      lab_size = 1.8)+
  geom_vline(xintercept=1:ncol(dat_vacvit)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(dat_vacvit)-0.5, colour="white", size=2) +
  ggtitle("Vaccinium vitis-idaea") +
  theme(plot.title = element_text(face = "italic"),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6))
        
# print pdf
layout <- '
AB
CD
EG
'

dev.off()
pdf(file="visuals/corr.pdf", width = 7.48, height = 9)

wrap_plots(A = p_bisviv,
           B = p_solvir,
           C = p_betnan,
           D = p_vacmyr,
           E = p_vaculi,
           G = p_vacvit, design = layout) +
  theme(plot.tag = element_text(size = ))

dev.off()

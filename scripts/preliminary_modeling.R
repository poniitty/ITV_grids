library(MetBrewer) # nice colour palettes. we use Hokusai3 for the species.
library(patchwork) # for nicer plots
library(cowplot) # for nicer plots
library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)
library(MuMIn)

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

# plot soil moisture (moist_mean_7_8) curves
p_moi_hei = d %>% 
  ggplot(aes(y = height, x = moist_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Plant height (cm)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_moi_lei = d %>% 
  ggplot(aes(y = leaf_area, x = moist_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Leaf area "(mm^2))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_moi_ldmc = d %>% 
  ggplot(aes(y = LDMC, x = moist_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Leaf dry matter content (mg/mg)") +
  xlab("Soil moisture (VWC%)") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_moi_sla = d %>% 
  ggplot(aes(y = SLA, x = moist_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Specific leaf area "(mm^2/mg))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_moi_sla_legend = d %>% 
  ggplot(aes(y = SLA, x = moist_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Specific leaf area (mm^2/mg)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position="bottom",
    legend.text = element_text(face = "italic")) +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

# now extract the legend
legend <- get_legend(p_moi_sla_legend)
plot(legend)

#save figure
dev.off()
pdf(file="visuals/response_curves_moi.pdf", width = 7.48, height = 9.45)

layout <- '
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
EEEE
'

wrap_plots(A = p_moi_hei,
           B = p_moi_lei,
           C = p_moi_ldmc,
           D = p_moi_sla,
           E = legend,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()

# plot snow depth (snow_depth) curves
p_sno_hei = d %>% 
  ggplot(aes(y = height, x = snow_depth, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Plant height (cm)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_sno_lei = d %>% 
  ggplot(aes(y = leaf_area, x = snow_depth, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Leaf area "(mm^2))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_sno_ldmc = d %>% 
  ggplot(aes(y = LDMC, x = snow_depth, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Leaf dry matter content (mg/mg)") +
  xlab("Snow depth (cm)") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_sno_sla = d %>% 
  ggplot(aes(y = SLA, x = snow_depth, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Specific leaf area "(mm^2/mg))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

#save figure
dev.off()
pdf(file="visuals/response_curves_sno.pdf", width = 7.48, height = 9.45)

layout <- '
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
EEEE
'

wrap_plots(A = p_sno_hei,
           B = p_sno_lei,
           C = p_sno_ldmc,
           D = p_sno_sla,
           E = legend,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()

# plot snow melting day (scd) curves
p_scd_hei = d %>% 
  ggplot(aes(y = height, x = scd, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Plant height (cm)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_scd_lei = d %>% 
  ggplot(aes(y = leaf_area, x = scd, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Leaf area "(mm^2))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_scd_ldmc = d %>% 
  ggplot(aes(y = LDMC, x = scd, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Leaf dry matter content (mg/mg)") +
  xlab("Snow melting day (DOY)") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_scd_sla = d %>% 
  ggplot(aes(y = SLA, x = scd, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Specific leaf area "(mm^2/mg))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

#save figure
dev.off()
pdf(file="visuals/response_curves_scd.pdf", width = 7.48, height = 9.45)

layout <- '
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
EEEE
'

wrap_plots(A = p_scd_hei,
           B = p_scd_lei,
           C = p_scd_ldmc,
           D = p_scd_sla,
           E = legend,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()

# plot air temperature (T3_mean) curves
p_airT_hei = d %>% 
  ggplot(aes(y = height, x = T3_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Plant height (cm)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_airT_lei = d %>% 
  ggplot(aes(y = leaf_area, x = T3_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Leaf area "(mm^2))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_airT_ldmc = d %>% 
  ggplot(aes(y = LDMC, x = T3_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Leaf dry matter content (mg/mg)") +
  xlab("Air temperature (°C)") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_airT_sla = d %>% 
  ggplot(aes(y = SLA, x = T3_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Specific leaf area "(mm^2/mg))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

#save figure
dev.off()
pdf(file="visuals/response_curves_airT.pdf", width = 7.48, height = 9.45)

layout <- '
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
EEEE
'

wrap_plots(A = p_airT_hei,
           B = p_airT_lei,
           C = p_airT_ldmc,
           D = p_airT_sla,
           E = legend,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()

# plot soil temperature (T1_mean) curves
p_soiT_hei = d %>% 
  ggplot(aes(y = height, x = T1_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Plant height (cm)") +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_soiT_lei = d %>% 
  ggplot(aes(y = leaf_area, x = T1_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Leaf area "(mm^2))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_soiT_ldmc = d %>% 
  ggplot(aes(y = LDMC, x = T1_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab("Leaf dry matter content (mg/mg)") +
  xlab("Soil temperature (°C)") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

p_soiT_sla = d %>% 
  ggplot(aes(y = SLA, x = T1_mean_7_8, colour = abbr, fill = abbr)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  scale_fill_met_d("Hokusai3") +
  scale_colour_met_d("Hokusai3") +
  ylab(bquote("Specific leaf area "(mm^2/mg))) +
  xlab("") +
  labs(fill = "Species", colour = "Species") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    strip.background = element_blank(),
    legend.position = "none") +
  facet_wrap(vars(abbr), nrow = 6, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

#save figure
dev.off()
pdf(file="visuals/response_curves_soiT.pdf", width = 7.48, height = 9.45)

layout <- '
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
ABCD
EEEE
'

wrap_plots(A = p_soiT_hei,
           B = p_soiT_lei,
           C = p_soiT_ldmc,
           D = p_soiT_sla,
           E = legend,
           design = layout) +
  theme(plot.tag = element_text(size = 8))

dev.off()






# moisture
d %>% ggplot(aes(x = moist_mean, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free_y") +
  theme_minimal() + xlab("Soil moisture")

d %>% ggplot(aes(x = moist_mean, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free_y") +
  theme_minimal() + xlab("Soil moisture")

# Soil temperature
d %>% ggplot(aes(x = T1_mean, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

d %>% ggplot(aes(x = T1_mean, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

# Air temperature
d %>% ggplot(aes(x = T3_mean, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

d %>% ggplot(aes(x = T3_mean, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

# Snow cover duration
d %>% ggplot(aes(x = scd, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

d %>% ggplot(aes(x = scd, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free_y")

# NDVI
d %>% ggplot(aes(x = ndvi, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

d %>% ggplot(aes(x = ndvi, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

# chm
d %>% ggplot(aes(x = chm, y = height))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")

d %>% ggplot(aes(x = chm, y = SLA))+
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  facet_wrap(vars(species), nrow = 3, scales = "free")


m <- lmer(SLA ~ ndvi + chm + scd + moist_mean + T1_mean + T3_mean + (1 | grid), data = d %>% filter(abbr == "BETNAN"))

m <- lm(SLA ~ ndvi + chm + scd + moist_mean + T1_mean + T3_mean + factor(grid), data = d %>% filter(abbr == "BETNAN"))
glance(m)
tidy(m)

r.squaredGLMM(m)


d <- d %>% mutate(across(T1_mean:ndvi, scale)) %>% 
  unnest(cols = c(T1_mean:ndvi)) %>% 
  group_by(species) %>% 
  mutate(across(height:LDMC, scale)) %>% 
  unnest(cols = c(height:LDMC)) %>% 
  ungroup()

# By species, SLA
d %>% 
  nest(data = -species) %>% 
  mutate(fit_linear = map(data, ~ lmer(SLA ~ ndvi + scd + moist_mean + T1_mean + T3_mean + (1 | grid), data = .))) -> fits

names(fits$fit_linear) <- fits$species

fits <- fits %>% 
  mutate(r2 = map(fit_linear, r.squaredGLMM)) %>% 
  mutate(r2 = map(r2, as_tibble))

map(fits$fit_linear, tidy) %>% bind_rows(.id = "species") %>% 
  mutate(trait = "SLA") -> ef_SLA

r2_SLA <- fits$r2 %>% bind_rows(.id = "species") %>% 
  mutate(trait = "SLA")

# By species, LDMC
d %>% 
  nest(data = -species) %>% 
  mutate(fit_linear = map(data, ~ lmer(LDMC ~ ndvi + scd + moist_mean + T1_mean + T3_mean + (1 | grid), data = .))) -> fits

names(fits$fit_linear) <- fits$species

fits <- fits %>% 
  mutate(r2 = map(fit_linear, r.squaredGLMM)) %>% 
  mutate(r2 = map(r2, as_tibble))

map(fits$fit_linear, tidy) %>% bind_rows(.id = "species") %>% 
  mutate(trait = "LDMC") -> ef_LDMC

r2_LDMC <- fits$r2 %>% bind_rows(.id = "species") %>% 
  mutate(trait = "LDMC")

# By species, height
d %>% 
  nest(data = -species) %>% 
  mutate(fit_linear = map(data, ~ lmer(height ~ ndvi + scd + moist_mean + T1_mean + T3_mean + (1 | grid), data = .))) -> fits

names(fits$fit_linear) <- fits$species

fits <- fits %>% 
  mutate(r2 = map(fit_linear, r.squaredGLMM)) %>% 
  mutate(r2 = map(r2, as_tibble))

map(fits$fit_linear, tidy) %>% bind_rows(.id = "species") %>% 
  mutate(trait = "height") -> ef_height

r2_height <- fits$r2 %>% bind_rows(.id = "species") %>% 
  mutate(trait = "height")


# By species, leaf_area
d %>% 
  nest(data = -species) %>% 
  mutate(fit_linear = map(data, ~ lmer(leaf_area ~ ndvi + scd + moist_mean + T1_mean + T3_mean + (1 | grid), data = .))) -> fits

names(fits$fit_linear) <- fits$species

fits <- fits %>% 
  mutate(r2 = map(fit_linear, r.squaredGLMM)) %>% 
  mutate(r2 = map(r2, as_tibble))

map(fits$fit_linear, tidy) %>% bind_rows(.id = "species") %>% 
  mutate(trait = "leaf_area") -> ef_leaf_area

r2_leaf_area <- fits$r2 %>% bind_rows(.id = "species") %>% 
  mutate(trait = "leaf_area")


# Bind together
r2s <- bind_rows(r2_SLA,
                 r2_LDMC,
                 r2_height,
                 r2_leaf_area)
r2s %>% group_by(species) %>% 
  summarise(R2m = mean(R2m),
            R2c = mean(R2c))
r2s %>% group_by(trait) %>% 
  summarise(R2m = mean(R2m),
            R2c = mean(R2c))


efs <- bind_rows(ef_SLA,
                 ef_LDMC,
                 ef_height,
                 ef_leaf_area)
efs %>% filter(effect == "fixed", term != "(Intercept)") %>% 
  group_by(term) %>% 
  summarise(vi = mean(abs(statistic)))
efs %>% filter(effect == "fixed", term != "(Intercept)") %>% 
  group_by(term, trait) %>% 
  summarise(vi = mean(statistic)) %>% slice(1)

d %>% select(species, height:LDMC) %>% 
  filter(complete.cases(.)) %>% 
  nest(data = -species) %>% 
  mutate(cor_map = map(data, cor)) -> cors
names(cors$cor_map) <- cors$species
cors$cor_map

d %>% select(species, c(ndvi,chm,scd,moist_mean,T1_mean,T3_mean)) %>% 
  filter(complete.cases(.)) %>% 
  nest(data = -species) %>% 
  mutate(cor_map = map(data, cor)) -> cors
names(cors$cor_map) <- cors$species
cors$cor_map

augment(fits$fit_linear[[1]])

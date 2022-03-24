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

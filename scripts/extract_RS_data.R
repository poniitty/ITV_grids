library(terra)
library(sf)
library(tidyverse)
library(exactextractr)

scd <- rast("E:/Planet_uusi/Kilpisjarvi/scd.tif")

p <- st_read("data/plot_coordinates.gpkg") %>% 
  st_zm()

scd <- crop(scd, p, snap = "out")

plot(scd)
plot(st_geometry(p), add = T)

p2 <- st_transform(p, crs = crs(scd))
p2 <- st_buffer(p2, dist = 2)
rr <- exact_extract(scd, p2)
p$scd <- unlist(lapply(rr, function(x) weighted.mean(x$value, x$coverage_fraction, na.rm = T)))

# Canopy height model

chm <- rast("E:/Planet_uusi/Kilpisjarvi/chm.tif")
chm <- crop(chm, p %>% st_transform(crs = crs(chm)), snap = "out")

plot(chm)
plot(st_geometry(p %>% st_transform(crs = crs(chm))), add = T)

p2 <- st_transform(p, crs = crs(chm))
p2 <- st_buffer(p2, dist = 2)
rr <- exact_extract(chm, p2)
p$chm <- unlist(lapply(rr, function(x) weighted.mean(x$value, x$coverage_fraction, na.rm = T)))

# NDVI

ndvi <- rast("E:/Planet_uusi/Kilpisjarvi/mosaiced/planet_2021-7-10_09-35-00_242b.tif")
ndvi <- crop(ndvi, p %>% st_transform(crs = crs(ndvi)), snap = "out")
ndvi <- (ndvi[[4]]-ndvi[[3]])/(ndvi[[4]]+ndvi[[3]])

plot(ndvi)
plot(st_geometry(p %>% st_transform(crs = crs(ndvi))), add = T)

p2 <- st_transform(p, crs = crs(ndvi))
p2 <- st_buffer(p2, dist = 2)
rr <- exact_extract(ndvi, p2)
p$ndvi <- unlist(lapply(rr, function(x) weighted.mean(x$value, x$coverage_fraction, na.rm = T)))

# To dataframe

df <- bind_cols(p %>% st_drop_geometry() %>% as.tibble(),
                p %>% st_transform(crs = 4326) %>% st_coordinates() %>% as.tibble()) %>% 
  mutate(grid = toupper(substr(Comment, 1, 1)), 
         plot = parse_number(Comment),
         id = paste0(grid, plot)) %>% 
  relocate(c(id,grid,plot), .after = Comment) %>% 
  relocate(X:Y, .after = plot) %>% 
  select(-Comment) %>% 
  arrange(grid, plot)

df %>% write_csv("output/rs_variables.csv")

library(brms)
library(tidyverse)

files <- list.files("./models", pattern = ".rds$")
files <- files[!grepl("_gamma_", files)]
files <- files[grepl("_log_", files)]

traits <- unique(unlist(lapply(files, function(x) str_split(x, "_")[[1]][1])))
species <- unique(unlist(lapply(files, function(x) str_split(x, "_")[[1]][2])))
# mod_family <- unique(unlist(lapply(files, function(x) str_split(x, "_")[[1]][3]))) %>% 
#   ifelse(. == "skew", "skew_normal", .)
# is_log <- unique(unlist(lapply(files, function(x) rev(str_split(x, "_")[[1]])[2])))

combs <- expand_grid(traits = traits,
                     species = species)

res <- tibble()
for(i in seq_len(nrow(combs))){
  print(i)
  sel_files <- files[grepl(paste0("^",combs$traits[i]), files) & grepl(combs$species[i], files)]
  
  if(length(sel_files) != 3){
    stop("WTF!!!")
  } else {
    
    mods <- lapply(sel_files, function(x) readRDS(paste0("models/",x)))
    names(mods) <- gsub("_.rds","",sel_files)
    
    mods <- lapply(mods, function(x){
      add_criterion(x, criterion = c("loo", "waic"))
    })
    
    # loow <- model_weights(mods[[1]],mods[[2]],mods[[3]],
    #                       mods[[4]],mods[[5]],mods[[6]], weights = "loo")
    # waicw <- model_weights(mods[[1]],mods[[2]],mods[[3]],
    #                        mods[[4]],mods[[5]],mods[[6]], weights = "waic")
    loow <- model_weights(mods[[1]],mods[[2]],mods[[3]], weights = "loo")
    waicw <- model_weights(mods[[1]],mods[[2]],mods[[3]], weights = "waic")
    
    loow <- loow %>% as.data.frame() %>% 
      rownames_to_column("id") %>% 
      mutate(id = parse_number(id)) %>% 
      rename(loo_weight = ".") %>% 
      mutate(loo_weight = round(loo_weight, 10))
    
    waicw <- waicw %>% as.data.frame() %>% 
      rownames_to_column("id") %>% 
      mutate(id = parse_number(id)) %>% 
      rename(waic_weight = ".") %>% 
      mutate(waic_weight = round(waic_weight, 10))
    
    mw <- full_join(loow, waicw)
    
    mw$mod <- names(mods)[mw$id]
    
    mw$traits <- unlist(lapply(mw$mod, function(x) str_split(x, "_")[[1]][1]))
    mw$species <- unlist(lapply(mw$mod, function(x) str_split(x, "_")[[1]][2]))
    mw$mod_family <- unlist(lapply(mw$mod, function(x) str_split(x, "_")[[1]][3])) %>% 
      ifelse(. == "skew", "skew_normal", .)
    mw$is_log <- unlist(lapply(mw$mod, function(x) rev(str_split(x, "_")[[1]])[1]))
    
    res <- bind_rows(res, mw)
    
  }
}

res %>% group_by(traits, species) %>% 
  slice_max(loo_weight) %>% as.data.frame()

res %>% group_by(traits, species) %>% 
  slice_max(waic_weight) %>% as.data.frame()

write_csv(res, "output/model_weights.csv")

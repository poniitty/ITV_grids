library(tidyverse)
library(missForest)
library(hms)
library(lubridate)

# Microclimate

d <- read_csv("data/tomst_data.csv") %>% 
  select(-moist_count) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) 

d %>% mutate(across(T1:moist, ~ifelse(probl != 0, NA, .x))) %>% 
  select(-probl, -T2) %>% 
  filter(plot != "F11") -> d

d %>% select(plot, datetime, T1) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = T1) -> T1
d %>% select(plot, datetime, T3) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = T3) -> T3
d %>% select(plot, datetime, moist) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = moist) -> moist

plots <- unique(d$plot)
dates <- as_date(as_date(min(T1$datetime)):(as_date(max(T1$datetime)) - days(14)))

reps <- 10

imput_df <- tibble()
for(i in 1:reps){
  
  plot_id <- sample(plots, 1)
  date_id <- sample(dates, 1)
  date_range <- as_date(date_id:(date_id + days(13)))
  
  temp1 <- T1
  temp1[as_date(temp1$datetime) %in% date_range, plot_id] <- NA
  temp2 <- T3
  temp2[as_date(temp2$datetime) %in% date_range, plot_id] <- NA
  temp3 <- moist
  temp3[as_date(temp3$datetime) %in% date_range, plot_id] <- NA
  
  impT1 <- missForest(as.data.frame(temp1) %>%
                        mutate(time = as.numeric(as_hms(datetime)),
                               datetime = as.numeric(datetime)) %>% 
                        relocate(time, .after = datetime))
  
  impT3 <- missForest(as.data.frame(temp2) %>%
                        mutate(time = as.numeric(as_hms(datetime)),
                               datetime = as.numeric(datetime)) %>% 
                        relocate(time, .after = datetime))
  
  impmo <- missForest(as.data.frame(temp3) %>%
                        mutate(time = as.numeric(as_hms(datetime)),
                               datetime = as.numeric(datetime)) %>% 
                        relocate(time, .after = datetime))
  
  imput_df <- bind_rows(imput_df,
                        tibble(imp_round = i,
                               plot = plot_id,
                               datetime = T1 %>% filter(as_date(datetime) %in% date_range) %>% pull(datetime),
                               T1 = T1 %>% filter(as_date(datetime) %in% date_range) %>% pull(plot_id),
                               T3 = T3 %>% filter(as_date(datetime) %in% date_range) %>% pull(plot_id),
                               moist = moist %>% filter(as_date(datetime) %in% date_range) %>% pull(plot_id),
                               T1_imp = impT1$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
                                 filter(as_date(datetime) %in% date_range) %>% pull(plot_id) %>% round(2),
                               T3_imp = impT3$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
                                 filter(as_date(datetime) %in% date_range) %>% pull(plot_id) %>% round(2),
                               moist_imp = impmo$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
                                 filter(as_date(datetime) %in% date_range) %>% pull(plot_id) %>% round(1)))

}

imput_df %>% mutate(T1_diff = abs(T1 - T1_imp),
                    T3_diff = abs(T3 - T3_imp),
                    moist_diff = abs(moist - moist_imp)) -> imput_df

summary(imput_df)

imput_df %>% mutate(date = as_date(datetime)) %>% 
  group_by(plot, date) %>% 
  summarise(across(T1:moist_imp, list(mean = mean, min = min, max = max))) %>% 
  mutate(T1_mean_diff = abs(T1_mean - T1_imp_mean),
         T1_min_diff = abs(T1_min - T1_imp_min),
         T1_max_diff = abs(T1_max - T1_imp_max),
         T3_mean_diff = abs(T3_mean - T3_imp_mean),
         T3_min_diff = abs(T3_min - T3_imp_min),
         T3_max_diff = abs(T3_max - T3_imp_max),
         moist_mean_diff = abs(moist_mean - moist_imp_mean),
         moist_min_diff = abs(moist_min - moist_imp_min),
         moist_max_diff = abs(moist_max - moist_imp_max)) -> daily

summary(daily)

imput_df %>% group_by(imp_round) %>% 
  summarise(across(T1:moist_imp, list(mean = mean, min = min, max = max))) %>% 
  mutate(T1_mean_diff = abs(T1_mean - T1_imp_mean),
         T1_min_diff = abs(T1_min - T1_imp_min),
         T1_max_diff = abs(T1_max - T1_imp_max),
         T3_mean_diff = abs(T3_mean - T3_imp_mean),
         T3_min_diff = abs(T3_min - T3_imp_min),
         T3_max_diff = abs(T3_max - T3_imp_max),
         moist_mean_diff = abs(moist_mean - moist_imp_mean),
         moist_min_diff = abs(moist_min - moist_imp_min),
         moist_max_diff = abs(moist_max - moist_imp_max)) -> roundly

summary(roundly)

####################################################################################
# FINAL IMPUTATION

# Microclimate

d <- read_csv("data/tomst_data.csv") %>% 
  select(-moist_count) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) 

d %>% mutate(across(T1:moist, ~ifelse(probl != 0, NA, .x))) %>% 
  select(-probl, -T2) -> d

d %>% select(plot, datetime, T1) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = T1) -> T1
d %>% select(plot, datetime, T3) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = T3) -> T3
d %>% select(plot, datetime, moist) %>% 
  pivot_wider(id_cols = datetime, names_from = plot, values_from = moist) -> moist

impT1 <- missForest(as.data.frame(T1) %>%
                      mutate(time = as.numeric(as_hms(datetime)),
                             datetime = as.numeric(datetime)) %>% 
                      relocate(time, .after = datetime))

impT3 <- missForest(as.data.frame(T3) %>%
                      mutate(time = as.numeric(as_hms(datetime)),
                             datetime = as.numeric(datetime)) %>% 
                      relocate(time, .after = datetime))

impmo <- missForest(as.data.frame(moist) %>%
                      mutate(time = as.numeric(as_hms(datetime)),
                             datetime = as.numeric(datetime)) %>% 
                      relocate(time, .after = datetime))

impT1$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
  select(-time) %>% 
  pivot_longer(cols = A1:F9, names_to = "plot", values_to = "T1") -> T1

impT3$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
  select(-time) %>% 
  pivot_longer(cols = A1:F9, names_to = "plot", values_to = "T3") -> T3

impmo$ximp %>% mutate(datetime = as_datetime(datetime, tz = "Etc/GMT-2")) %>% 
  select(-time) %>% 
  pivot_longer(cols = A1:F9, names_to = "plot", values_to = "moist") -> moist

full_join(T1, T3) %>% full_join(., moist) %>% 
  arrange(plot, datetime) -> all

write_csv(all, "data/tomst_data_imputed.csv")

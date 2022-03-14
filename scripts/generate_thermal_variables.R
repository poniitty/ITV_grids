library(tidyverse)
library(missForest)
library(lubridate)

# Microclimate

d <- read_csv("data/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(date = as_date(datetime))

d %>% group_by(plot, date) %>%
  summarise(across(T1:moist, list(mean = mean, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}")) %>% 
  ungroup() -> daily


# Summarizing variables over the whole period
daily %>% 
  group_by(plot) %>% 
  summarise(T1_mean = round(mean(T1_mean, na.rm = T),2),
            T1_absmax = round(max(T1_max, na.rm = T),2),
            T1_absmin = round(min(T1_min, na.rm = T),2),
            T1_meanmax = round(mean(T1_max, na.rm = T),2),
            T1_meanmin = round(mean(T1_min, na.rm = T),2),
            T3_mean = round(mean(T3_mean, na.rm = T),2),
            T3_absmax = round(max(T3_max, na.rm = T),2),
            T3_absmin = round(min(T3_min, na.rm = T),2),
            T3_meanmax = round(mean(T3_max, na.rm = T),2),
            T3_meanmin = round(mean(T3_min, na.rm = T),2),
            moist_sd = round(sd(moist_mean, na.rm = T),1),
            moist_cv = round(sd(moist_mean, na.rm = T)/mean(moist_mean, na.rm = T),2),
            moist_mean = round(mean(moist_mean, na.rm = T),1),
            moist_absmax = round(max(moist_max, na.rm = T),1),
            moist_absmin = round(min(moist_min, na.rm = T),1)) %>% 
  ungroup() -> dw

# Thermal sums
d %>% 
  group_by(plot) %>% 
  summarise(T1_TDD = round(sum(ifelse(T1 < 0, 0, T1), na.rm = T)),
            T1_GDD3 = round(sum(ifelse(T1 < 3, 0, T1), na.rm = T)),
            T1_GDD5 = round(sum(ifelse(T1 < 5, 0, T1), na.rm = T)),
            T1_FDD = round(sum(ifelse(T1 > 0, 0, T1), na.rm = T)),
            T3_TDD = round(sum(ifelse(T3 < 0, 0, T3), na.rm = T)),
            T3_GDD3 = round(sum(ifelse(T3 < 3, 0, T3), na.rm = T)),
            T3_GDD5 = round(sum(ifelse(T3 < 5, 0, T3), na.rm = T)),
            T3_FDD = round(sum(ifelse(T3 > 0, 0, T3), na.rm = T))) %>% 
  ungroup() -> dd

# Monthly aggregation
daily %>% 
  mutate(month = month(date)) %>% 
  group_by(plot, month) %>% 
  summarise(T1_mean = round(mean(T1_mean, na.rm = T),2),
            T1_absmax = round(max(T1_max, na.rm = T),2),
            T1_absmin = round(min(T1_min, na.rm = T),2),
            T1_meanmax = round(mean(T1_max, na.rm = T),2),
            T1_meanmin = round(mean(T1_min, na.rm = T),2),
            T3_mean = round(mean(T3_mean, na.rm = T),2),
            T3_absmax = round(max(T3_max, na.rm = T),2),
            T3_absmin = round(min(T3_min, na.rm = T),2),
            T3_meanmax = round(mean(T3_max, na.rm = T),2),
            T3_meanmin = round(mean(T3_min, na.rm = T),2),
            moist_sd = round(sd(moist_mean, na.rm = T),1),
            moist_cv = round(sd(moist_mean, na.rm = T)/mean(moist_mean, na.rm = T),2),
            moist_mean = round(mean(moist_mean, na.rm = T),1),
            moist_absmax = round(max(moist_max, na.rm = T),1),
            moist_absmin = round(min(moist_min, na.rm = T),1)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = plot, names_from = month, values_from = T1_mean:moist_absmin) -> dm

da <- full_join(dw, dd) %>% full_join(dm)
summary(da)

write_csv(da, "output/thermal_variables.csv")


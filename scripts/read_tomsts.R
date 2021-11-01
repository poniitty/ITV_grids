
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

# Set date limits to remove implausible dates
mind <- "2021-07-15"
maxd <- "2021-09-29"

#################################################################################3
# plot names x tomst ids

ids <- read_csv("data/tomst_ids.csv") %>% 
  rename(tomst_id = TMS)

# List logger data files to read
f <- list.files("data",pattern = "data_9", full.names = T, recursive = T)

fi <- data.frame(file = f)

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi <- full_join(fi, ids) %>% 
  mutate(plot = toupper(plot))

fi$file2 <- gsub("_..csv", "", fi$file)

fi <- fi[order(fi$plot),]


readdata <- function(i){
  nn <- sum(grepl(i, fi$file2))
  
  if(nn > 1){
    
    fi2 <- fi %>% filter(grepl(i, fi$file2))
    
    df2 <- data.frame()
    for(ii in fi2$file2){
      print(ii)
      d <- fread(ii)
      
      d %>% select(V2,V3,V4,V5,V6,V7) -> d
      
      d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
      
      df2 <- bind_rows(df2, d)
    }
    
    df2 %>% filter(!duplicated(.$V2, fromLast = T)) -> df2
    
    df2$plot <- fi[which(fi$file2 == ii),"plot"]
    
    df2 %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> df2
    
    df2 %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> df2
    
    
    return(df2)
    
  } else {
    
    print(i)
    d <- fread(fi$file[fi$file2 == i])
    
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$plot <- fi[which(fi$file2 == i),"plot"]
    
    d %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> d
    
    return(d)
    
  }
  
}


mylist <- lapply(fi$file2, readdata)
df <- rbindlist( mylist )

# Rename columns
df %>% rename(datetime = V2,
              zone = V3,
              T1 = V4,
              T2 = V5,
              T3 = V6,
              moist = V7) -> df

df %>% arrange(plot, datetime) -> df


plots <- unique(df$plot)

# Calculate different daily values for diagnostics
df %>% mutate(date = as_date(datetime)) %>% 
  group_by(plot,date) %>% 
  mutate(roll_diff_T1 = T1 - lead(T1),
         roll_diff_T2 = T2 - lead(T2),
         roll_diff_T3 = T3 - lead(T3)) %>%
  summarise(soil_sd = sd(T1),
            air_sd = sd(T3),
            soil_mean = mean(T1),
            air_mean = mean(T3),
            soil_min = min(T1),
            soil_max = max(T1),
            surf_max = max(T2),
            air_max = max(T3),
            mean_diff = mean(abs(T3-T1)),
            moist = min(moist),
            corr = cor(T1,T3, use = "pairwise.complete.obs"),
            max_diff_T1 = max(roll_diff_T1, na.rm = T),
            max_diff_T2 = max(roll_diff_T2, na.rm = T),
            max_diff_T3 = max(roll_diff_T3, na.rm = T)) %>% 
  mutate(sa_diff = air_mean-soil_mean,
         sa_max_diff = air_max-soil_max,
         ss_max_diff = surf_max-soil_max) %>% 
  mutate(sd_ratio = soil_sd/air_sd) %>% 
  mutate(sd_ratio = ifelse(soil_sd < 1, 0, sd_ratio)) %>% 
  mutate(corr = ifelse(is.na(corr), 0, corr)) %>% 
  as.data.frame() -> df2

# create column for error codes
df2 %>% mutate(probl = 0) -> df2

############################################################################
# PLOTTINGS
############################################################################

# Plot temperatures
pdf("visuals/Temperature_graphs.pdf", 12, 5)
for(i in plots){
  #i <- plots[3]
  print(i)
  df %>% filter(plot == i) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG
  print(GG)
  
}
dev.off()

###################################################################################################################

# Months to plot
times <- seq(floor_date(as_date(min(df2$date)), "month"),
             ceiling_date(as_date(max(df2$date)), "month") + months(1) - days(1),
             by = "month")

# Plot each plot month by month
for(plotid in plots){
  #plotid <- "AIL105"
  print(plotid)
  pdf(paste0("visuals/monthly_", plotid, ".pdf"), 10, 6)
  for (tt in 1:(length(times) - 1)) {
    
    df %>% filter(plot == plotid) %>%
      filter(datetime >= ymd(times[tt]),
             datetime < ymd(times[tt + 1])) -> dft
    
    if(nrow(dft %>% filter(complete.cases(.)) > 0)){
      dft %>%
        ggplot(aes_string(x = "datetime")) +
        geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
        geom_line(aes_string(y = "T2"), col = "brown1") +
        geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
        theme_minimal() +
        ylab("Temperature") + xlab("Date") +
        ggtitle(paste("plot: ", plotid, "; Time: ", times[tt])) +
        scale_x_datetime(date_minor_breaks = "1 day") -> GG1
      
      dft %>%
        ggplot(aes_string(x = "datetime")) +
        geom_line(aes_string(y = "moist"), col = "blue") +
        theme_minimal() +
        ylab("Moisture") + xlab("Date") +
        ggtitle(paste("plot: ", plotid, "; Time: ", times[tt])) +
        scale_x_datetime(date_minor_breaks = "1 day") -> GG2
      
      print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
    }
  }
  dev.off()
}  

#################################################################################
# Screening each plot for possible errors

df2 %>% 
  mutate(probl = ifelse(date <= "2021-07-13", 2, probl),
         probl = ifelse(date >= "2021-10-01", 2, probl)) %>% 
  mutate(probl = ifelse(date %in% c("2021-07-14","2021-07-15"), 1, probl),
         probl = ifelse(date %in% c("2021-09-29","2021-09-30"), 1, probl)) -> df2

# plot = F11
plotid <- "F11"

probls <- c(as_date(as_date("2021-09-11"):as_date("2021-09-29")))

df2 %>% mutate(probl = ifelse(plot == plotid &
                                date %in% probls,
                              1, probl)) -> df2
               

########################################################################
# FILL MISSING TIMESTAMPS WITH NA
df3 <- data.frame()
for(i in unique(df$plot)){
  #i <- 6
  
  df %>% filter(plot == i) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(i)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt),
                            plot = i)
    
    print(NROW(missingdf))
    
    temp %>% full_join(., missingdf) %>% 
      arrange(datetime) %>% 
      select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
  }
  
}

# #################################################################################
# # CORRECT BIASES BASED ON IN-THE-FRIDGE DATA
# 
# df3 %>% filter((datetime >= "2021-07-11 20:00:00" & datetime <= "2021-07-12 08:00:00") | 
#                  (datetime >= "2021-07-13 20:00:00" & datetime <= "2021-07-14 08:00:00")) %>% 
#   group_by(plot) %>% 
#   summarise(across(T1:T3, median)) -> means_corr
# 
# means_corr %>% summarise(across(T1:T3, median)) -> medians
# 
# means_corr %>% mutate(T1 = T1-medians$T1,
#                  T2 = T2-medians$T2,
#                  T3 = T3-medians$T3) %>% 
#   rename(T1_diff = T1,
#          T2_diff = T2,
#          T3_diff = T3) -> means_corr
# 
# head(means_corr, 20)
# 
# summary(means_corr)
# 
# df3 %>% full_join(means_corr) %>% 
#   mutate(T1 = T1-T1_diff,
#          T2 = T2-T2_diff,
#          T3 = T3-T3_diff) %>% 
#   select(!ends_with("diff")) -> df3

#################################################################################
# CORRECT BIASES BASED ON THE NOT-IN-FIELD DATA
#

dfc <- df3

# 
# diffs_all <- data.frame()
# for(i in plots){
#   # i <- "RA082"
#   office <- df2 %>% filter(plot == i) %>% 
#     filter(probl == 2) %>% pull(date)
#   office <- office[-which(office == max(office))]
#   
#   df %>% filter(plot == i) %>%
#     mutate(date = as_date(datetime)) %>% 
#     filter(date %in% office) %>% 
#     mutate(change1a = abs(T3 - lag(T3,1)),
#            change1b = abs(T3 - lag(T3,2)),
#            change1c = abs(T3 - lag(T3,3)),
#            change1d = abs(T3 - lag(T3,4)),
#            change1e = abs(T3 - lag(T3,5)),
#            change1f = abs(T3 - lag(T3,6)),
#            change1g = abs(T3 - lead(T3,1)),
#            change1h = abs(T3 - lead(T3,2)),
#            change1i = abs(T3 - lead(T3,3))) %>% 
#     rowwise() %>%
#     mutate(change1 = max(change1a, change1b, change1c,
#                          change1d, change1e, change1f,
#                          change1g, change1g, change1i, na.rm = T)) %>%
#     mutate(T3 = ifelse(change1 > 0.1250, NA, T3)) %>% 
#     filter(!is.na(T3)) %>% 
#     as.data.frame() %>% 
#     filter(complete.cases(.)) -> temp
#   
#   means <- c(T1 = mean(temp$T1),
#              T2 = mean(temp$T2),
#              T3 = mean(temp$T3))
#   
#   diffs <- round(means - median(means),4)
#   
#   dfc %>% mutate(T1 = ifelse(plot == i,T1 - diffs["T1"],T1),
#                  T2 = ifelse(plot == i,T2 - diffs["T2"],T2),
#                  T3 = ifelse(plot == i,T3 - diffs["T3"],T3)) -> dfc
#   
#   print(i)
#   print(diffs)
#   
#   diffs_all <- bind_rows(diffs_all,
#                          bind_cols(data.frame(plot = i), 
#                                    as.data.frame(t(as.data.frame(diffs)))))
#   
# }
# 
# fwrite(diffs_all, "output/Correction_temperatures.csv")

###################################################################################
# Delete erroneous data
#

dfc %>% mutate(date = as_date(datetime)) %>%
  left_join(., df2 %>% select(plot, date, probl)) %>% 
  filter(probl != 2) -> dfc

dfc %>% mutate(h = hour(datetime)) %>% 
  group_by(plot, date) %>% 
  summarise(nh = length(unique(h))) %>% 
  filter(nh != 24) -> missh

dfc %>% left_join(., missh) %>% 
  mutate(T1 = replace(T1, !is.na(nh), NA),
         T2 = replace(T2, !is.na(nh), NA),
         T3 = replace(T3, !is.na(nh), NA),
         moist = replace(moist, !is.na(nh), NA)) %>%
  select(-nh,-zone) -> dfc

# Remove implausible dates
dfc %>% filter(datetime > mind,
              datetime < maxd) -> dfc


####################################################################
# MASK IMPOSSIBLE VALUES

dfc %>% mutate(T1 = ifelse(T1 < (-50) | T1 > 50, NA, T1),
              T2 = ifelse(T2 < (-50) | T2 > 50, NA, T2),
              T3 = ifelse(T3 < (-50) | T3 > 50, NA, T3),
              moist = ifelse(moist < 200, NA, moist)) -> dfc


###############################################################################
# PLOT CORRECTED

pdf("visuals/Temperature_graphs_corrected.pdf", 10, 5)
for(i in plots){
  #i <- 104
  print(i)
  dfc %>% filter(plot == i) %>% 
    mutate(across(T1:T3, ~ifelse(probl == 1, NA, .x))) %>% 
    mutate(across(c(T1,T3), ~ifelse(probl == 4, NA, .x))) %>% 
    mutate(T3 = ifelse(probl == 3, NA, T3)) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG
  print(GG)
  
}
dev.off()

# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

# Calibrate the moisture values
dfc %>% rename(moist_count = moist) %>% 
  mutate(moist = round(cal_funNA(moist_count),1)) -> dfc

round2 <- function(x) round(x,2)

dfc %>% mutate(across(T1:T3, round2)) %>% 
  select(plot, datetime, T1, T2, T3, moist, moist_count, probl) -> dfc

fwrite(dfc, "data/tomst_data.csv")

####################################################################################
# Test if the correction temperatures correlate with the field measurements

# dfc %>% filter(date >= "2021-07-15" & date <= "2021-08-31") %>% 
#   group_by(plot) %>% 
#   summarise(across(T1:T3, mean)) %>% 
#   full_join(diffs_all %>% 
#                 rename(T1_diff = T1,
#                        T2_diff = T2,
#                        T3_diff = T3)) -> to_test
# 
# cor(to_test$T1, to_test$T1_diff)
# cor(to_test$T2, to_test$T2_diff)
# cor(to_test$T3, to_test$T3_diff)
# 
# summary(to_test)

# It seems that correcting the temperatures can be actually harmful, thus skipped

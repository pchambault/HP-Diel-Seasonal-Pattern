#########################################
######    Statistics for paper    #######
#########################################

library(tidyverse)



##################################################
# Graphical abstract:
# mean and maxdep in Jul and Dec for day vs night
##################################################

# load high resolution
#---------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS")  %>%
  mutate(hour  = substr(start_local, 12, 13),
         month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(month != "Jan") %>%  # remove because only available for one animal
  ungroup()
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))

# calculate daily depth per phase and ID
#----------------------------------------
daily_hr = dive %>%
  dplyr::select(id,date,day_night,maxdep) %>% 
  group_by(id, date, day_night) %>%
  summarise(mean_dep   = mean(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily_hr$month = factor(daily_hr$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

daily_hr %>% 
  filter(month == "Jul") %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep))
# day_night meandep maxdep
# day          25    164.
# night        39.8  137.

daily_hr %>% 
  filter(month == "Dec") %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep))
# day_night meandep maxdep
# day       80.3   213.
# night     90.7   251

kruskal.test(daily_hr$mean_dep, daily_hr$day_night) # p<0.001
daily_hr %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep),
            sddep   = sd(mean_dep))
# day_night meandep sddep
# day          53.4  30.8
# night        73.3  27.0
((73-53)/53) * 100 # 37% increase of meandep between day and night



#------------------------------
# low-resolution dataset
#------------------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive_lr <- readRDS("./RDATA/0c.depth_LR_1row_1dive_19ids_tzCorrected.RDS") %>%
  dplyr::select(id,date,posix_local,period,depth) %>% 
  mutate(month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(id == "22850" | id == "22854"| id == "27261"|
           id == "27261b"| id == "37228"| id == "37235b" |
           id == "93096"| id == "93102") %>%
  filter(month != "Jan")   # removed because only available for 1 id
dive_lr$month = factor(dive_lr$month, levels=c("Jul","Aug","Sep",
                                               "Oct","Nov","Dec"))
unique(dive_lr$id)


# calculate mean daily depth
#------------------------------
daily_lr = dive_lr %>%
  group_by(id, date, day_night) %>%
  summarise(mean_dep = mean(depth)) %>%
  mutate(month       = format(date, "%b")) %>%
  ungroup()

# proportion of increase between day and night
#-----------------------------------------------
daily_lr %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep),
            sddep   = sd(mean_dep))
# day_night   meandep sddep
# 1 day          36.5  22.4
# 2 night        48.3  25.6
((48.3-36.5)/48.3) * 100 # 24% increase of meandep between day and night


# average dive depth across month
#--------------------------------
daily2 = dive %>%
  group_by(date) %>%
  summarise(max_dep    = max(maxdep, na.rm=T),
            mean_dep   = mean(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily2$month = factor(daily2$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

daily2 %>% 
  group_by(month) %>%
  summarise(meandep = mean(mean_dep),
            sddep   = sd(mean_dep),
            maxdep  = mean(max_dep))
# month meandep sddep maxdep
# 1 Jul      30.0  7.77   215.
# 2 Aug      35.7  3.64   262.
# 3 Sep      52.4  8.65   269.
# 4 Oct      72.4  9.99   274.
# 5 Nov      82.4 10.5    274.
# 6 Dec      91.7 17.9    283.


# proportion of increase between day and night
# both high and low-resolution tags included
#-----------------------------------------------
daily = rbind(daily_hr, daily_lr)
daily %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep),
            sddep   = sd(mean_dep))
# day_night    meandep sddep
# 1 day          45.9  28.6
# 2 night        62.1  29.2
((62.1-45.9)/62.1) * 100 # 26% increase of meandep between day and night










#########################################
# Fig.1: min and max sunrise and sunset
#########################################

#---------------------
# load low res dataset
#---------------------
lr <- readRDS("./RDATA/0c.daylength_depth_LR_19ids_tzCorrected.RDS")
lr$month = as.numeric(substr(lr$date, 6, 7))
unique(lr$id) # 19 ids

# select individuals with long tracking dataset 
# at least 2 months, not only covering summer
#------------------------------------------------
lr %>%
  group_by(id) %>%
  summarise(start    = first(date),
            end      = last(date),
            duration = round(difftime(last(date), first(date), units="days")),
            ndives   = n()) 
lr = lr %>% 
  select(id,date,max_dep,mean_dep,month,daylength) %>%
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
unique(lr$id) # 8 ids

# load high res dataset
#----------------------
hr <- readRDS("./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS") %>%
  mutate(month = as.numeric(substr(date, 6, 7))) %>%
  filter(month != "Jan") %>%
  select(id,date,max_dep,mean_dep,month,daylength)
unique(hr$id)  # 5 ids
names(lr)
names(hr)
daily = rbind(lr, hr)
length(unique(daily$id))                # 13 ids
daily = daily[!is.na(daily$daylength),] # 1246 obs
daily = daily %>% mutate(month = format(date, "%b"))

mean(daily$max_dep)  # 236 m
sd(daily$max_dep)    # 75 m


daily %>% 
  filter(month == "Jul") %>%
  summarise(mean_daylentgh = mean(daylength),
            min_daylentgh = min(daylength),
            max_daylength = max(daylength))

daily %>% 
  filter(month == "Dec") %>%
  summarise(mean_daylentgh = mean(daylength),
            min_daylentgh  = min(daylength),
            max_daylength  = max(daylength))






###############################################
# proportion of deep dives (>100 m)
###############################################
nrow(daily[daily$max_dep>100,]) / nrow(daily) * 100 # 93%
nrow(dive[dive$maxdep>100,]) / nrow(dive) * 100     # 20%
nrow(dive[dive$maxdep>50,]) / nrow(dive) * 100      # 30%
  









#####################################
# stat test: depth vs months
#####################################
daily = daily %>% 
  mutate(month_num = as.numeric(substr(date, 6,7))) %>%
  filter(month != "Jan")
kruskal.test(mean_dep~month_num, daily)  # p<0.001, X2=148, df=5
kruskal.test(max_dep~month_num, daily)   # p<0.001, X2=30, df=5








#############################################
# daily mean of maxdep
#############################################
mean(daily$max_dep)  # 268 m
sd(daily$max_dep)    # 34 m
max(daily$max_dep)   # 361 m

mean(daily$mean_dep)  # 64 m
sd(daily$mean_dep)    # 23 m
max(daily$mean_dep)   # 135 m

nrow(daily[daily$mean_dep>100,]) / nrow(daily) * 100 # 4%
nrow(daily[daily$mean_dep>50,]) / nrow(daily) * 100  # 70%










#############################################
# Mean dive duration (n=17 porpoises)
#############################################

# low resolution dataset
#--------------------------
lr <- readRDS("./RDATA/0c.duration_Binned_LR_19ids_tzCorrected.RDS")
names(lr)
lr = lr %>%
  filter(id != "22849b", id != "22850b", id != "27262",
         id != "27262b", id != "93100") %>%
  select(id, duration, date) %>% rename(dur = duration) 
mean(lr$dur) # 2.12 min

daily_lr = lr %>%
  group_by(id, date) %>%
  summarise(mean_dur = mean(dur),
            sd_dur   = sd(dur)) %>%
  ungroup()
summary(daily_lr) # mean dur: 2.0 min, sd: 1.6

# high resolution dataset
#--------------------------
hr <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS") %>%
  select(id, dur, start) %>%
  mutate(dur = dur / 60)
mean(hr$dur) # 1.6 min
sd(hr$dur)   # 1.03
mean_dur = bind_rows(lr, hr)
mean(mean_dur$dur) # 1.7 min
sd(mean_dur$dur)   # 1.1 min
nrow(mean_dur[mean_dur$dur>4,]) / nrow(mean_dur) *100 # 3.2%

daily_hr = hr %>%
  group_by(id, start) %>%
  summarise(mean_dur = mean(dur),
            sd_dur   = sd(dur)) %>%
  ungroup()
summary(daily_hr$mean_dur)    # mean dur: 1.6 min

daily = bind_rows(daily_lr, daily_hr)
mean(daily$mean_dur) # 1.67 min
sd(daily$mean_dur)   # 1.02 min
nrow(daily[daily$mean_dur>4,]) / nrow(daily) *100 # 3%








#####################################
# Fig 2: n hourly dives
#####################################

# calculate hourly number of dives per depth class 
#----------------------------------------------------
hourly = dive %>%
  mutate(month = format(start_local, "%b"),
         hour  = as.numeric(format(start_local, "%H"))) %>%
  group_by(id, hour, date) %>%
  summarise(ntot = n_distinct(dive),
            dives0_20m     = n_distinct(dive[maxdep<=20]),
            dives20_50m    = n_distinct(dive[maxdep>20 & maxdep<=50]),
            dives50_100m   = n_distinct(dive[maxdep>50 & maxdep<=100]),
            dives_deep100m = n_distinct(dive[maxdep>100])) %>%
  ungroup() %>%
  mutate(month = format(date, "%b"))
hourly


# number of dives/hour (all depth class pooled)
#-----------------------------------------------
summary(hourly$ntot)          # mean: 17.4 dives/hour
sd(hourly$ntot)               # mean: 6.7 dives/hour
summary(hourly$ntot[hourly$month=="Jul"])     # mean: 14 dives/hour
summary(hourly$ntot[hourly$month=="Dec"])     # mean: 16 dives/hour

summary(hourly$dives0_20m)    # mean: 4 dives/hour
summary(hourly$dives50_100m)  # mean: 3.3 dives/hour
summary(hourly$dives_deep100m)# mean: 3.5 dives/hour

# average number of shallow dives/hour (<20m) across months
#----------------------------------------------------------
summary(hourly$dives0_20m[hourly$month=="Jul"])     # mean: 7.0 dives/hour
summary(hourly$dives0_20m[hourly$month=="Dec"])     # mean: 2 dives/hour

# average number of deep dives/hour (>100 m) across months
#----------------------------------------------------------
summary(hourly$dives_deep100m[hourly$month=="Jul"]) # mean: 0.4 dives/hour
summary(hourly$dives_deep100m[hourly$month=="Dec"]) # mean: 5.3 dives/hour



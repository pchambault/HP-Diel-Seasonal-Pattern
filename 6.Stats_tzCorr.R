#########################################
######  Statistics for paper   ##########
###### after time zone correction #######
#########################################

library(tidyverse)



##################################################
# graphical abstract:
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
  filter(month != "Jan") %>%
  ungroup()
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))

# calculate daily depth per phase and ID
#----------------------------------------
daily = dive %>%
  group_by(id, date, day_night) %>%
  summarise(max_dep    = max(maxdep, na.rm=T),
            mean_dep   = mean(maxdep, na.rm=T),
            median_dep = median(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily$month = factor(daily$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

daily %>% 
  filter(month == "Jul") %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep),
            maxdep = mean(max_dep))
# day_night meandep maxdep
# day          25.0   164.
# night        339.8  137.

daily %>% 
  filter(month == "Dec") %>%
  group_by(day_night) %>%
  summarise(meandep = mean(mean_dep),
            maxdep = mean(max_dep))
# day_night meandep maxdep
# day       80.3   213.
# night     90.7   251.


# average dive depth across month
#--------------------------------
daily = dive %>%
  group_by(date) %>%
  summarise(max_dep    = max(maxdep, na.rm=T),
            mean_dep   = mean(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily$month = factor(daily$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

daily %>% 
  group_by(month) %>%
  summarise(meandep = mean(mean_dep),
            sddep   = sd(mean_dep),
            maxdep  = mean(max_dep))
#   month meandep sddep maxdep
# 1 Jul      30.0  7.77   215.
# 2 Aug      35.7  3.64   262.
# 3 Sep      52.4  8.65   269.
# 4 Oct      72.4  9.99   274.
# 5 Nov      82.4 10.5    274.
# 6 Dec      91.7 17.9    283.













#########################################
# Fig.1: min and max sunrise and sunset
#########################################

#------------------
# load lr dataset
#------------------
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
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
unique(lr$id) # 8 ids

# load HR dataset
#--------------------
hr <- readRDS("./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS") %>%
  # select(-c(sunrise, sunset)) %>%
  mutate(month = as.numeric(substr(date, 6, 7))) %>%
  filter(month != "Jan")
unique(hr$id)  # 5 ids

names(lr)
names(hr)
daily = rbind(lr, hr)
length(unique(daily$id))            # 13 ids
daily = daily[!is.na(daily$daylength),] # 1246 obs
daily = daily %>% mutate(month = format(date, "%b"))

mean(daily$maxdep)  # 236 m
mean(daily$meandep) # 60 m


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












#####################################
# stat test: depth vs months
#####################################
daily = daily %>% 
  mutate(month_num = as.numeric(substr(date, 6,7))) %>%
  filter(month != "Jan")
kruskal.test(mean_dep~month_num, daily)  # p<0.001, X2=148, df=5
kruskal.test(max_dep~month_num, daily)   # p<0.001, X2=30, df=5








#############################################
# mean maxdep
#############################################
mean(daily$max_dep)  # 288 m
sd(daily$max_dep)    # 34 m
max(daily$max_dep)   # 361 m

mean(daily$mean_dep)  # 64 m
sd(daily$mean_dep)    # 23 m
max(daily$mean_dep)   # 135 m

nrow(daily[daily$mean_dep>100,]) / nrow(daily) * 100 # 4%
nrow(daily[daily$mean_dep>50,]) / nrow(daily) * 100  # 69%










#############################################
# mean dive duration (n=17 porpoises)
#############################################

# low resolution dataset
#--------------------------
lr <- readRDS("./RDATA/1c.duration_Binned_LR_19ids.RDS")
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
hr <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") %>%
  select(id, dur, start) %>%
  mutate(dur = dur / 60)
mean(hr$dur) # 1.67 min
sd(hr$dur)   # 1.03
mean_dur = bind_rows(lr, hr)
mean(mean_dur$dur) # 1.7 min
sd(mean_dur$dur)   # 1.1 min
nrow(mean_dur[mean_dur$dur>4,]) / nrow(mean_dur) *100 # 3.3%

daily_hr = hr %>%
  group_by(id, start) %>%
  summarise(mean_dur = mean(dur),
            sd_dur   = sd(dur)) %>%
  ungroup()
summary(daily_hr)    # mean dur: 1.6 min, sd: ?

daily = bind_rows(daily_lr, daily_hr)
mean(daily$mean_dur) # 1.67 min
sd(daily$mean_dur)   # 1.02 min
nrow(daily[daily$mean_dur>4,]) / nrow(daily) *100 # 3%








#####################################
# Fig 2: n hourly dives
#####################################
hourly <- readRDS("./RDATA/8b.DiveFrequency_hour_month_depthClass.RDS")
hourly

# number of dives/hour (all depth class pooled)
summary(hourly$ntot)          # mean: 17.4 dives/hour
sd(hourly$ntot)               # mean: 6.7 dives/hour
summary(hourly$ntot[hourly$month=="Jul"])     # mean: 14 dives/hour
summary(hourly$ntot[hourly$month=="Dec"])     # mean: 16 dives/hour

summary(hourly$dives0_20m)    # mean: 4 dives/hour
summary(hourly$dives50_100m)  # mean: 4 dives/hour
summary(hourly$dives_deep100m)# mean: 4 dives/hour

# average number of shallow dives/hour (<20m) across months
summary(hourly$dives0_20m[hourly$month=="Jul"])     # mean: 7 dives/hour
summary(hourly$dives0_20m[hourly$month=="Dec"])     # mean: 2 dives/hour

# average number of deep dives/hour (>100 m) across months
summary(hourly$dives_deep100m[hourly$month=="Jul"]) # mean: 0.4 dives/hour
summary(hourly$dives_deep100m[hourly$month=="Dec"]) # mean: 5 dives/hour










# #########################################
# # light regimes based on solar altitude
# #########################################
# solar <- readRDS("./RDATA/1c.solar_Altitude_depth_LR_19ids.RDS") %>%
#   mutate(month = format(date, "%b"),
#          twilight = case_when(alt_deg > 0 ~ "Day",
#                               alt_deg > (-6) & alt_deg < (0) ~ "Civil twilight",
#                               alt_deg > (-12) & alt_deg < (-6) ~ "Nautical twilight",
#                               alt_deg > (-18) & alt_deg < (-12) ~ "Astronomical twilight",
#                               alt_deg < (-18) ~ "Night"))
# table(solar$twilight)
# range(solar$alt_deg)
# jul = solar %>% filter(month == "Jul") 
# table(jul$twilight)
# 
# dec = solar %>% filter(month == "Dec") 
# table(dec$twilight)



#######################################################
############ FILTERING LOW RESOLUTION TDR  ############
############      HARBOUR PORPOISES        ############
#######################################################

library(tidyverse)
library(janitor)
library(ggplot2)
library(readr)
library(tidyquant)
library(gridExtra)
library(ncdf4)
library(raster)
library(viridis)
library(oce)
library(readxl)


####################################
# import binned data
####################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/")
lr <- read.csv("./DATA/HP_Low-Resolution_Dive_data/HP2017-Histos.csv") %>%
  as_tibble() %>%
  dplyr::select(-c(DeployID, Source, Time.Offset, 
                   BadTherm, DepthSensor)) %>%
  dplyr::rename(id  = Ptt, instr = Instr, dateTime = Date, lc = LocationQuality, 
         lon = Longitude, lat = Latitude, type = HistType, count = Count) %>%
  mutate(date = as.numeric(dateTime),
         date = excel_numeric_to_date(date)) %>%
  relocate(date, .before = dateTime)

names(lr)
length(unique(lr$id))  # 15 porpoises! same ids for some, should be 12
table(lr$id)
table(lr$instr)
# Mk10 Splash 
# 7657   5871 
summary(lr$NumBins) # 5 to 72
table(lr$type)

# convert date and times
#--------------------------
lr$dateTime = convert_to_datetime(
  lr$dateTime,
  character_fun=lubridate::ymd_hms, truncated=1, tz="UTC")
lr$dateTime
first(lr$dateTime)
last(lr$dateTime)

lr = lr %>% mutate(year = substr(dateTime, 1, 4)) %>% # "2012" "2013" "2014" "2011"
  relocate(year, .after = dateTime)
table(lr$year)
lr = lr %>% filter(year != "2011")  # wrong date: only 1 observation in 2011
lr$id = as.character(lr$id)






###################################
# 2012: change bins names 
###################################
detach("package:janitor", unload = TRUE)
# depth class in m: 0; 5; 10; 15; 20; 25; 30; 40; 50; 60; 70; 80; 90; >90
lr %>% filter(year == "2012") %>% summarise(n = n_distinct(id)) # 2 HPs
subset = lr %>% filter(id == "7617" | id == "7618")  
unique(subset$id) # 7617 7618

#---------------
## dive depth ##
#---------------
dive = subset %>% filter(type == "DiveDepth")
summary(dive)
names(dive)
n    = 12+max(dive$NumBins) # bins start after column 12
dive = dive[,c(1:n)] 
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                    "lc","lat","lon","numbins","sum","0m","5m","10m",
                    "15m","20m","25m","30m","40m","50m","60m","70m",
                    "80m","90m","100m") 
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 

# pivot table to get 1 column with the depth class
names(dive)
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), # from bin 0 m to bin 100 m
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class, # reorder factor
                           levels=c("0m","5m","10m","15m","20m","25m","30m",
                                    "40m","50m","60m","70m","80m","90m","100m"))

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Depth", y = "n dives") +
  facet_wrap(~id) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
pivot -> dep2012
table(dep2012$class)


#------------------
## dive duration ##
#------------------
# 30; 60; 90; 120; 180; 240; 300; 360; 420; 480; 540; 600; 1200; >1200 (sec)
dive = subset %>% filter(type == "DiveDuration")
summary(dive)
names(dive)
n = 12+max(dive$NumBins)
dive = dive[,c(1:n)] # retain only bins with no NA
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                    "lc","lat","lon","numbins","sum","0.5min","1min","1.30min",
                    "2min","3min","4min","5min","6min","7min","8min","9min",
                    "10min","20min","21min")
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 
names(dive)

# pivot table to get 1 column with the duration class
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), 
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class, 
                           levels=c("0.5min","1min","1.30min","2min","3min",
                                    "4min","5min","6min","7min","8min","9min",
                                    "10min","20min","21min")) 

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Duration", y = "n dives") +
  facet_wrap(~id, ncol=1) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
pivot -> dur2012
table(dur2012$class)






###################################
# 2013: change bins names 
###################################
# depth class in m: 5; 10; 25; 50; 100; 150; 200; 250; 300; 350; 400; 450; 500; >500
lr %>% filter(year == "2013") %>% 
  summarise(n = n_distinct(id)) # 10 HPs including the 2 from 2012 (not same config!)
subset = lr %>% filter(year == "2013")
subset = subset %>% filter(id != "7617", id != "7618") # exclude 2 ids from 2012 that lasted until 2013
unique(subset$id) # 22849 22850 22853 27261 27262 37227 37228 37235

#---------------
## dive depth ##
#---------------
dive = subset %>% filter(type == "DiveDepth")
summary(dive)
names(dive)
n = 12+max(dive$NumBins)
dive = dive[,c(1:n)] 
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                   "lc","lat","lon","numbins","sum","5m","10m",
                   "25m","50m","100m","200m","250m","300m","350m")
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 
names(dive)

# pivot table to get 1 column with the depth class
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), 
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class,  # reorder factors
                     levels=c("5m","10m","25m","50m","100m",
                              "200m","250m","300m","350m"))

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Depth", y = "n dives") +
  facet_wrap(~id) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
pivot -> dep2013
table(dep2013$class)


#------------------
## dive duration ##
#------------------
# 30secs; 1 min ; 1 min 30; 2 mins ; 3 mins ; 4 mins ; 5 mins ; 6 mins ; 7 mins ; 8 mins ; 9 mins ; 10 mins ; 11 mins ; >11 mins
dive = subset %>% filter(type == "DiveDuration")
summary(dive)
names(dive)
n = 12+max(dive$NumBins)
dive = dive[,c(1:n)] 
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                   "lc","lat","lon","numbins","sum",
                   "0.5min","1min","1.30min","2min","3min",
                   "4min","5min","6min","7min","8min","9min",
                   "10min","11min","12min")
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 
names(dive)

# pivot table to get 1 column with the duration class
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), 
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class, 
                     levels=c("0.5min","1min","1.30min","2min","3min",
                              "4min","5min","6min","7min","8min","9min",
                              "10min","11min","12min"))

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Duration", y = "n dives") +
  facet_wrap(~id, ncol=2) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
pivot -> dur2013
table(dur2013$class)





###################################
# 2014: change bins names 
###################################
# depth class in m: 1; 2; 10; 25; 50; 100; 150; 200; 250; 300; 350; 400; 450; >450
lr %>% filter(year == "2014") %>% 
  summarise(n = n_distinct(id)) # 10 HPs 
subset = lr %>% filter(year == "2014")
unique(subset$id)

# rename ids that are similar to those from 2013
#---------------------------------------------------
subset = subset %>%
  mutate(id = case_when(id == "22849" ~ "22849b",
                        id == "22850" ~ "22850b",
                        id == "22854" ~ "22854",
                        id == "24638" ~ "24638",
                        id == "27262" ~ "27262b",
                        id == "27261" ~ "27261b",
                        id == "37235" ~ "37235b",
                        id == "93096" ~ "93096",
                        id == "93100" ~ "93100",
                        id == "93102" ~ "93102"))
unique(subset$id)
subset %>% group_by(id) %>% summarise(tag = unique(instr))

#---------------
## dive depth ##
#---------------
dive = subset %>% filter(type == "DiveDepth")
summary(dive)
names(dive)
n = 12+max(dive$NumBins)
dive = dive[,c(1:n)] 
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                   "lc","lat","lon","numbins","sum","1m","2m","10m",
                   "25m","50m","100m","150m","200m","300m","350m","400m","450m")
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 
names(dive)

# pivot table to get 1 column with the depth class
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), 
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class, 
                     levels=c("1m","2m","10m","25m","50m","100m",
                              "150m","200m","300m","350m","400m","450m"))

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Depth", y = "n dives") +
  facet_wrap(~id, ncol=3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
table(pivot$id) # 24638: only 12 dives!
pivot -> dep2014
table(dep2014$class)


#-------------------
## dive duration ##
#------------------
# 30secs; 1 min ; 1 min 30secs; 2 mins ; 3 mins ; 4 mins ; 5 mins ; 6 mins ; 7 mins ; 8 mins ; 9 mins ; 10 mins ; 11 mins
dive = subset %>% filter(type == "DiveDuration")
summary(dive)
names(dive)
n = 12+max(dive$NumBins)
dive = dive[,c(1:n)] 
names(dive)
colnames(dive) = c("id","instr","type","date","dateTime","year","count",
                   "lc","lat","lon","numbins","sum",
                   "0.5min","1min","1.30min","2min","3min",
                   "4min","5min","6min","7min","8min","9min",
                   "10min","11min","12min")
dive = dive %>% dplyr::select(-c(numbins, sum, count)) 
names(dive)

# pivot table to get 1 column with the duration class
pivot = dive %>%
  pivot_longer(c(10:ncol(dive)), 
               names_to = "class", values_to = "n") 
pivot
str(pivot)
table(pivot$class)
pivot$class = factor(pivot$class, 
                     levels=c("0.5min","1min","1.30min","2min","3min",
                              "4min","5min","6min","7min","8min","9min",
                              "10min","11min","12min"))

ggplot(data = pivot, aes(x=class, y=n)) +
  geom_bar(stat="identity") +
  labs(x = "Duration", y = "n dives") +
  facet_wrap(~id, ncol=2) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))
pivot -> dur2014
table(dur2014$class)








#########################################
# combine datasets from the 3 years
#########################################
dep = rbind(dep2012, dep2013, dep2014)
dur = rbind(dur2012, dur2013, dur2014)
length(unique(dep$id)) # 20 animals (including the 5 ids from HR data)
length(unique(dur$id)) # 20 animals (including the 5 ids from HR data)
rm(dep2012, dep2013, dep2014, dive, dur2012, dur2013, dur2014, pivot)

# create new column to differentiate high and low resloution tags
#----------------------------------------------------------------
dep = dep %>%
  mutate(dataset = case_when(id == "93100"  ~ "high",
                             id == "22850b" ~ "high",
                             id == "22849b" ~ "high",
                             id == "27262"  ~ "high",
                             id == "27262b" ~ "high",
                             TRUE ~ "low"),
         year  = substr(date, 1, 4),
         month = format(dep$date, "%b"))
table(dep$dataset)

dur = dur %>%
  mutate(dataset = case_when(id == "93100"  ~ "high",
                             id == "22850b" ~ "high",
                             id == "22849b" ~ "high",
                             id == "27262"  ~ "high",
                             id == "27262b" ~ "high",
                             TRUE ~ "low"),
         year  = substr(date, 1, 4),
         month = format(dur$date, "%b"))

dep$month = format(dep$date, "%b")
unique(dep$month)
dur$month = format(dur$date, "%b")
unique(dur$month)
dep$year = substr(dep$date, 1, 4)
dur$year = substr(dur$date, 1, 4)







#################################################
# correct time zone for each coordinate
# for depth dataset
# in summer: UTC-2 West Greenland
# in winter: UTC-3 West Greenland
#################################################
library(lutz)
library(janitor)
system.time({  # 4 sec
  dep2 = dep %>%
    # find the time zone
    mutate(
      timezone = tz_lookup_coords(lat = lat,
                                  lon = lon,
                                  method = "accurate"),
      # transform date using the right time zone
      posix_local = map2(
        .x = dateTime,
        .y = timezone,
        .f = function(x, y) {
          with_tz(time = x, tzone = y)
        }
      )
    ) %>%
    # required because of the map2
    unnest(posix_local)
})
paste0(dep2$dateTime, "->", dep2$posix_local)
table(dep2$timezone)

## duration ##
#-------------
system.time({  # 4 sec
  dur2 = dur %>%
    # find the time zone
    mutate(
      timezone = tz_lookup_coords(lat = lat,
                                  lon = lon,
                                  method = "accurate"),
      # transform date using the right time zone
      posix_local = map2(
        .x = dateTime,
        .y = timezone,
        .f = function(x, y) {
          with_tz(time = x, tzone = y)
        }
      )
    ) %>%
    # required because of the map2
    unnest(posix_local)
})
paste0(dur2$dateTime, "->", dur2$posix_local)
table(dur2$timezone)






#############################################
# extract sunrise/sunset for depth
#############################################
dep2 = dep2 %>%
  mutate(date = as.Date(substr(posix_local, 1, 10))) %>%
  filter(!is.na(lon))

## tz= America/Godthab ##
#--------------------------
table(dep2$timezone)
time1 = dep2 %>%
  filter(timezone == "America/Godthab") 
time1 = time1 %>%
  mutate(sunrise = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                    tz   = "America/Godthab",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                   tz   = "America/Godthab",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                  tz   = "America/Godthab",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                     tz   = "America/Godthab",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',     # after sunset, before night
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn', # before sunrise
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+2 ##
#--------------------------
time2 = dep2 %>%
  filter(timezone == "Etc/GMT+2") 
time2 = time2 %>%
  mutate(sunrise = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+2",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+2",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+2",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+2",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+2",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+2",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',     
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn', 
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+3 ##
#-----------------
time3 = dep2 %>%
  filter(timezone == "Etc/GMT+3") 
time3 = time3 %>%
  mutate(sunrise = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+3",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+3",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+3",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+3",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+3",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+3",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',    
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+4 ##
#-----------------
time4 = dep2 %>%
  filter(timezone == "Etc/GMT+4") 
time4 = time4 %>%
  mutate(sunrise = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+4",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+4",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+4",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+4",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+4",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+4",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',    
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()

dep2 = rbind(time1, time2, time3, time4)
dep2 -> dep






#############################################
# extract sunrise/sunset for duration
#############################################
dur2 = dur2 %>%
  mutate(date = as.Date(substr(posix_local, 1, 10))) %>%
  filter(!is.na(lon))

## tz= America/Godthab ##
#--------------------------
table(dur2$timezone)
time1 = dur2 %>%
  filter(timezone == "America/Godthab") 
time1 = time1 %>%
  mutate(sunrise = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                    tz   = "America/Godthab",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                   tz   = "America/Godthab",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                  tz   = "America/Godthab",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                     tz   = "America/Godthab",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',     # after sunset, before night
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn', # before sunrise
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+2 ##
#--------------------------
time2 = dur2 %>%
  filter(timezone == "Etc/GMT+2") 
time2 = time2 %>%
  mutate(sunrise = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+2",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+2",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+2",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+2",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+2",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+2",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',     
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn', 
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+3 ##
#-----------------
time3 = dur2 %>%
  filter(timezone == "Etc/GMT+3") 
time3 = time3 %>%
  mutate(sunrise = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+3",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+3",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+3",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+3",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+3",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+3",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',    
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+4 ##
#-----------------
time4 = dur2 %>%
  filter(timezone == "Etc/GMT+4") 
time4 = time4 %>%
  mutate(sunrise = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                    tz   = "Etc/GMT+4",
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                   tz   = "Etc/GMT+4",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+4",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                 tz   = "Etc/GMT+4",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                  tz   = "Etc/GMT+4",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time4[,c("date","lon","lat")],
                                     tz   = "Etc/GMT+4",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',    
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()

dur2 = rbind(time1, time2, time3, time4)
dur2 -> dur
rm(dep2, dur2)

# remove empty classes
#-----------------------
dep  = dep %>% filter(n!=0)
dur  = dur %>% filter(n!=0)











#########################################
# reshape tibbles to get 1 row=1 dive
#########################################
dep$depth = as.numeric(gsub("([0-9]+).*$", "\\1", dep$class))
summary(dep$depth) # 0 to 450 m
dep2 = dep %>% uncount(n)
dep2 %>% dplyr::select(id, posix_local, depth, class)
dep  %>% dplyr::select(id, posix_local, depth, class, n) %>% slice_head()

dur$duration = as.numeric(gsub("([0-9]+).*$", "\\1", dur$class))
summary(dur$duration) # 0 to 12 min
dur2 = dur %>% uncount(n)

# remove id 24638 due to low number of dives (n=6 dives)
table(dep$id)
dur2 = dur2 %>% filter(id != "24638")
dur  = dur  %>% filter(id != "24638")
dep2 = dep2 %>% filter(id != "24638")
dep  = dep  %>% filter(id != "24638")
length(unique(dep$id))    # 19 ids
length(unique(dep2$id))   # 19 ids
length(unique(dur$id))    # 19 ids
length(unique(dur2$id))   # 19 ids




#########################
# save data
#########################
saveRDS(dep2, "./RDATA/0c.depth_LR_1row_1dive_19ids_tzCorrected.RDS")
saveRDS(dur2, "./RDATA/0c.duration_LR_1row_1dive_19ids_tzCorrected.RDS")
saveRDS(dep,  "./RDATA/0c.depth_Binned_LR_19ids_tzCorrected.RDS")
saveRDS(dur,  "./RDATA/0c.duration_Binned_LR_19ids_tzCorrected.RDS")









###########################################
# calculate daylength per day and ID
###########################################
# dep <- readRDS("./RDATA/0c.depth_Binned_LR_19ids_tzCorrected.RDS")
names(dep)
daylength = dep %>%
  dplyr::select(id, date, depth, sunrise, sunset) %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1) %>%
  filter(day_depart != 1) %>%  # remove first day because never complete (starts in the middle of day)
  group_by(id, date) %>%
  summarise(sunrise   = first(sunrise),
            sunset    = first(sunset),
            mean_dep   = mean(depth),
            max_dep    = max(depth)) %>%
  mutate(daylength = as.numeric(difftime(sunset, sunrise, units = "hour"))) %>%
  ungroup()
daylength
length(unique(daylength$id)) # 19 ids


# save data
#--------------
saveRDS(daylength, "./RDATA/0c.daylength_depth_LR_19ids_tzCorrected.RDS")






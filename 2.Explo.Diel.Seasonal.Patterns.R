################################################################
###########         SEASONAL AND DIEL PATTERNS   ###############
###########   HARBOUR PORPOISES - WEST GREENLAND ###############
###########     HIGH RESOLUTION DATASET (n=5)    ###############
################################################################

library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(viridis)
library(DT)
library(data.table)



#########################################
# load data
#########################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
hp <- readRDS("./RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")
names(hp)
hp %>%
  group_by(id) %>%
  summarise(start = first(date),
            end   = last(date))





#############################
# check coordinates
#############################
names(hp)
hp %>%
  group_by(id) %>%
  summarise(minlon = min(lon),
            minlat = min(lat),
            maxlon = max(lon),
            maxlat = max(lat))
coords = hp %>%
  group_by(id, date) %>%
  summarise(lon = mean(lon),
            lat = mean(lat),
            period = unique(period),
            meandep = mean(depth),
            maxdep = max(depth)) %>%
  ungroup()
coords$month = substr(coords$date, 6, 7)

north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Russia"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-40), ylim=c(60, 70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_point(data=coords, aes(x=lon, y=lat, colour=month), size=0.2) +
  facet_grid(month~id) +
  theme_tq()

hp %>% 
  filter(id == "27262b" & month == "09") %>%
  summarise(min_sunrise = min(sunrise),
            max_sunrise = max(sunrise),
            max_sunset = max(sunset),
            min_sunset = min(sunset),
            minlat = min(lat),
            maxlat = max(lat)) %>%
  ungroup()









#####################################################
# geom_tile: y=hour, x=days, fill=freq hourly ndive
#####################################################
dive <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") # 1 row=1 dive
names(dive)
dive = dive %>% 
  mutate(hour  = substr(start, 12, 13),
         date  = as.Date(start), 
         month = substr(start, 6, 7)) %>%
  ungroup()
str(dive)
dive = dive[order(dive$date),]

# proportions of dives/hour/day/id
#-----------------------------------
dataPlot = dive %>%
  dplyr::group_by(id, hour, date) %>%
  dplyr::summarize(ntot           = n(),
                   dives0_20m     = n_distinct(dive[maxdep<=20]),
                   dives20_50m    = n_distinct(dive[maxdep>20 & maxdep<=50]),
                   dives50_100m   = n_distinct(dive[maxdep>50 & maxdep<=100]),
                   dives100_200m  = n_distinct(dive[maxdep>100 & maxdep<=200]),
                   dives_deep200m = n_distinct(dive[maxdep>200])) %>%
  dplyr::ungroup()
# View(dataPlot)

dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
dataPlot = dataPlot %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01")
dataPlot = dataPlot[order(dataPlot$id, dataPlot$date),]
dataPlot$hour = as.numeric(dataPlot$hour)
dataPlot

# identify sunrise and sunset/day
#---------------------------------
start_end = hp %>%
  group_by(id, date) %>%
  summarize(sunrise = first(sunrise),
            sunset  = first(sunset)) %>%
  mutate(month = substr(date, 6, 7),
         date2 = as.Date(paste0("2022",substr(date,5,10)))) %>%
  filter(month!="01") %>%
  group_by(id, date2) %>%
  summarise(sunrise_mean = mean(sunrise),  # average over the different ids (years)
            sunset_mean  = mean(sunset)) %>%
  ungroup() 

sunrise = start_end %>%
  dplyr::rename("hour" = "sunrise_mean") 
sunrise$hour = as.numeric(substr(sunrise$hour, 12, 13))
sunrise = sunrise[order(sunrise$date2),]

sunset  = start_end %>%
  dplyr::rename("hour" = "sunset_mean") 
sunset$hour = as.numeric(substr(sunset$hour, 12, 13))
sunset = sunset[order(sunset$date2),]


# plot all dep classes
# facet / ID
#--------------------
ggplot(dataPlot, aes(x = date2, y = hour)) +
  geom_tile(aes(fill = ntot)) + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  geom_path(data=sunrise, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  geom_path(data=sunset, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  facet_grid(id ~ .) +
  theme(legend.position = c("bottom")) +
  theme_tq() +
  labs(x = "", y = "Hour (UTC-2)", fill = "n dive/hour") 


# pivot table to add dep_class column
#---------------------------------------
pivot = dataPlot %>%
  pivot_longer(c(dives0_20m, dives20_50m, 
                 dives50_100m, dives100_200m, dives_deep200m), 
               names_to = "depth_class", values_to = "prop") 
pivot

pivot = pivot %>%
  mutate(class = case_when(depth_class == "dives0_20m" ~ "0-20 m",
                           depth_class == "dives20_50m" ~ "20-50 m",
                           depth_class == "dives50_100m" ~ "50-100 m",
                           depth_class == "dives100_200m" ~ "100-200 m",
                           depth_class == "dives_deep200m" ~ "> 200 m"))

pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", "100-200 m", "> 200 m"))

# plot: facet / ID and depth class
#----------------------------------
ggplot(pivot, aes(x = date2, y = hour)) +
  geom_tile(aes(fill = prop)) + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  facet_grid(id ~ class) +
  geom_path(data=sunrise, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  geom_path(data=sunset, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  theme(legend.position = c("bottom")) +
  theme_tq() +
  labs(x = "", y = "Hour (UTC-2)", fill = "n dive/hour") 

ggplot(pivot[pivot$class=="0-20 m",], aes(x = date2, y = hour)) +
  geom_tile(aes(fill = prop)) + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  facet_grid(id ~ class) +
  geom_path(data=sunrise, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  geom_path(data=sunset, aes(x = date2, y = hour), 
            size=0.5, colour="blue") + 
  theme(legend.position = c("bottom")) +
  theme_tq() +
  labs(x = "", y = "Hour (UTC-2)", fill = "n dive/hour") 






###################################################
# months vs hour (fill=depth): per indiv 
# timestep: 1 hour
###################################################

# aggregate by date, id, hour
#----------------------------
dataPlot = hp %>% 
  setDT(.) %>% 
  .[,.(period = first(period)), 
    by=.(id, date, hour)]
dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
dataPlot = dataPlot %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01", day_depart != 1)
dataPlot = dataPlot[order(dataPlot$id, dataPlot$date2),]

# identify sunrise and sunset/day
#---------------------------------
start_end = hp %>%
  group_by(id, date) %>%
  summarize(sunrise = first(sunrise),
            sunset  = first(sunset)) %>%
  mutate(month = substr(date, 6, 7),
         date2 = as.Date(paste0("2022",substr(date,5,10)))) %>%
  filter(month!="01") %>%
  group_by(id, date2) %>%
  summarise(sunrise_mean = mean(sunrise),  # average over the different ids (years)
            sunset_mean  = mean(sunset)) %>%
  ungroup() 

sunrise = start_end %>%
  dplyr::rename("hour" = "sunrise_mean") 
sunrise$hour = as.numeric(substr(sunrise$hour, 12, 13))
sunrise = sunrise[order(sunrise$date2),]

sunset  = start_end %>%
  dplyr::rename("hour" = "sunset_mean") 
sunset$hour = as.numeric(substr(sunset$hour, 12, 13))
sunset = sunset[order(sunset$date2),]


# calculate median depth/hour/day/id
#--------------------------------------
dataPlot2 = hp %>% 
  setDT(.) %>% 
  .[,.(depth = median(depth)), 
    by=.(id, day_depart, date, hour)]
dataPlot2$date2 = as.Date(paste0("2022",substr(dataPlot2$date,5,10)))
dataPlot2 = dataPlot2 %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01")

# generate depth classes
#--------------------------
summary(dataPlot2$depth)
dataPlot2$class_dep = cut(dataPlot2$depth, 
                          breaks=c(0,20,50,108))
unique(dataPlot2$class_dep)
dataPlot2 = as_tibble(dataPlot2)

# individual plots
#------------------
ggplot(dataPlot2, aes(x = date2, y = as.numeric(hour))) +
  geom_tile(aes(fill = class_dep)) + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                            "12:00","16:00","20:00"),
                   breaks=c(0,4,8,12,16,20)) +
  scale_fill_brewer(palette = "Blues") + # BuPu
  facet_grid(id ~ .) +
  theme(legend.position = c("bottom")) +
  geom_path(data=sunrise, aes(x = date2, y = hour), 
             size=0.5, colour="blue") + 
  geom_path(data=sunset, aes(x = date2, y = hour), 
             size=0.5, colour="red") + 
  theme_tq() +
  labs(x = "", y = "Time of the day (UTC-2)", 
       fill = "Median depth (m)") 









# ##################################
# # plot with all ids combined
# ##################################
# 
# # median depth/date
# #--------------------
# dataPlot3 = hp %>% 
#   setDT(.) %>% 
#   .[,.(depth = median(depth)), 
#     by=.(date2, hour)]
# dataPlot3 = dataPlot3 %>% 
#   mutate(month = substr(date2, 6, 7)) %>%
#   filter(month!="01")
# 
# summary(dataPlot3$depth)
# dataPlot3$class_dep = cut(dataPlot3$depth, 
#                           breaks=c(0,20,50,110))
# unique(dataPlot3$class_dep)
# 
# # extract sunrise and sunset/day
# #---------------------------------
# start_end = dat %>%
#   mutate(sunrise_hour = as.numeric(substr(sunrise, 12, 13)),
#          sunset_hour  = as.numeric(substr(sunset, 12, 13))) %>%
#   group_by(date2) %>%
#   summarize(sunrise = mean(sunrise_hour),
#             sunset  = mean(sunset_hour)) %>%
#   mutate(month = substr(date2, 6, 7)) %>%
#   filter(month!="01") %>%
#   ungroup() 
# start_end
# 
# sunrise = start_end %>%
#   dplyr::rename("hour" = "sunrise") 
# # sunrise$hour = as.numeric(substr(sunrise$hour, 12, 13))
# # sunrise = sunrise[order(sunrise$date2),]
# 
# sunset  = start_end %>%
#   dplyr::rename("hour" = "sunset") 
# # sunset$hour = as.numeric(substr(sunset$hour, 12, 13))
# # sunset = sunset[order(sunset$date2),]
# 
# # combined plots
# #------------------
# ggplot(dataPlot3, aes(x = date2, y = hour)) +
#   geom_tile(aes(fill = class_dep)) + 
#   scale_y_continuous(labels=c("00:00","04:00","08:00",
#                               "12:00","16:00","20:00"),
#                      breaks=c(0,4,8,12,16,20)) +
#   scale_fill_brewer(palette = "Blues") + 
#   theme(legend.position = c("bottom")) +
#   geom_path(data=sunrise, aes(x = date2, y = hour), 
#             size=0.5, colour="blue") + 
#   geom_path(data=sunset, aes(x = date2, y = hour), 
#             size=0.5, colour="red") + 
#   theme_tq() +
#   labs(x = "", y = "Time of the day (UTC-2)", 
#        fill = "Median depth (m)") 












# ###################################################
# # months vs hour (fill=depth): all indiv combined
# # timestep: 15 min
# ###################################################
# 
# dataPlot = hp %>% 
#   setDT(.) %>% 
#   .[,.(period = first(period)), 
#     by=.(id, day_depart, date, halfhour)]
# dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
# dataPlot = dataPlot %>% 
#   mutate(month = substr(date2, 6, 7)) %>%
#   filter(month!="01", day_depart != 1)
# dataPlot = dataPlot[order(dataPlot$id, dataPlot$date),]
# dataPlot
# 
# dataPlot = dataPlot %>%
#   mutate(day_night = case_when(period == "day"  ~ "day",
#                                period == "dusk" ~ "night",
#                                period == "dawn" ~ "night",
#                                period == "night"~ "night"),
#          day_night = as.factor(day_night),
#          id        = as.factor(id),
#          month     = as.factor(month))
# 
# 
# # extract sunrise and sunset
# #-----------------------------
# dat$date2 = as.Date(paste0("2022",substr(dat$date,5,10)))
# start_end = dat %>%
#   group_by(date) %>%
#   summarize(sunrise = mean(sunrise),
#             sunset  = mean(sunset)) %>%
#   mutate(month = substr(date, 6, 7),
#          date2 = as.Date(paste0("2022",substr(date,5,10)))) %>%
#   filter(month!="01") %>%
#   group_by(date2) %>%
#   summarise(sunrise_mean = mean(sunrise),
#             sunset_mean  = mean(sunset)) %>%
#   ungroup() 
# 
# # round times every 15 mins
# start_end$sunrise_floor = round_date(start_end$sunrise_mean, "15 mins") 
# start_end$sunset_floor  = round_date(start_end$sunset_mean, "15 mins") 
# start_end
# 
# start = start_end %>%
#   select(-c(sunset_mean, sunset_floor)) %>%
#   dplyr::rename("timestep" = "sunrise_floor") 
# start$timestep = substr(start$timestep, 12, 16)
# start = start[order(start$date2),]
# 
# end  = start_end %>%
#   select(-c(sunrise_mean, sunrise_floor)) %>%
#   dplyr::rename("timestep" = "sunset_floor") 
# end$timestep = substr(end$timestep, 12,16)
# end = end[order(end$date2),]
# 
# 
# # check data gaps
# start$diff = c(NA,difftime(start$sunrise_mean[2:nrow(start)],
#                            start$sunrise_mean[1:nrow(start)-1], units="mins"))
# # View(start)
# end$diff = c(NA,difftime(end$sunset_mean[2:nrow(end)],
#                          end$sunset_mean[1:nrow(end)-1], units="mins"))
# # View(end)
# 
# 
# 
# # calculate mediandep/day/15min
# #----------------------------------
# dataPlot2 = dat %>% 
#   setDT(.) %>% 
#   .[,.(depth = median(depth)), 
#     by=.(date2, halfhour)]
# dataPlot2 = dataPlot2 %>% 
#   mutate(month = substr(date2, 6, 7)) %>%
#   filter(month!="01")
# dataPlot2
# dataPlot2$timestep = substr(dataPlot2$halfhour, 12,16)
# 
# # generate depth classes
# #--------------------------
# summary(dataPlot2$depth)
# dataPlot2$class_dep = cut(dataPlot2$depth, 
#                           breaks=c(0,20,50,100,170))
# unique(dataPlot2$class_dep)
# 
# 
# # plots
# #---------
# library(RColorBrewer)
# brewer.pal(n=4, "Blues")
# # display.brewer.pal(4, "Blues")
# 
# ggplot(dataPlot2, aes(x = date2, y = timestep)) +
#   geom_tile(aes(fill = class_dep)) + 
#   scale_y_discrete(breaks=c("00:00","04:00","08:00",
#                             "12:00","16:00","20:00")) +
#   # scale_fill_viridis_c(option = "magma", direction = -1) +
#   # scale_fill_distiller(palette = "PuBuGn", direction = 1) +
#   # scale_fill_distiller(palette = "BuPu", direction = 1) +
#   # scale_fill_brewer(palette = "Blues") + # BuPu
#   # scale_fill_brewer() +
#   scale_fill_manual(values = c("#EFF3FF", "#BDD7E7", 
#                                "#2171B5", "dodgerblue4")) +
#   geom_point(data=start, aes(x = date2, y = timestep),
#              size=0.5, colour="black") +
#   geom_point(data=end, aes(x = date2, y = timestep),
#              size=0.5, colour="red") +
#   theme_tq() +
#   theme(legend.position = c("right")) +
#   labs(x = "", y = "Hour (UTC-2)", fill = "Median \ndepth (m)") 









# ##########################################
# # calculate daily depth (max, mean, median)
# ##########################################
# # Summarize data per id: extract daily max depth
# dat = hp %>%
#   mutate(month = as.factor(substr(date, 6, 7)),
#          day_night = case_when(period == "day"   ~ "day",
#                                period == "dusk"  ~ "night",
#                                period == "dawn"  ~ "day",
#                                period == "night" ~ "night")) %>%
#   filter(month!="01") %>%
#   ungroup()
# 
# dep_daily = dat %>%
#   dplyr::group_by(id) %>%
#   dplyr::mutate(day = as.numeric(date - first(date)) + 1) %>%
#   dplyr::group_by(id, date, day_night) %>%
#   dplyr::summarise(maxdep    = max(depth, na.rm=T),
#                    mindep    = min(depth, na.rm=T),
#                    meandep   = mean(depth, na.rm=T),
#                    mediandep = median(depth, na.rm=T)) %>%
#   dplyr::ungroup()
# 
# dep_daily = dep_daily %>%
#   dplyr::group_by(id) %>%
#   dplyr::mutate(day   = as.numeric(date - first(date)) + 1,
#                 month = substr(date, 6, 7),
#                 month2 = case_when(month == "07" ~ "Jul",
#                                    month == "08" ~ "Aug",
#                                    month == "09" ~ "Sep",
#                                    month == "10" ~ "Oct",
#                                    month == "11" ~ "Nov",
#                                    month == "12" ~ "Dec")) %>%
#   dplyr::ungroup()
# dep_daily$month2 = factor(dep_daily$month2, 
#                           levels=c("Jul","Aug","Sep",
#                                    "Oct","Nov","Dec"))
# dep_daily
# 
# saveRDS(dep_daily, "./RDATA/4.dailyDepth_HR.RDS")





#########################################
# check data gaps
#########################################
# hp = hp[order(hp$id, hp$posix_local),]
data_gaps = hp %>%
  group_by(id) %>%
  mutate(diff_time = c(NA,difftime(posix_local[2:n()], 
                              posix_local[1:n()-1], units="secs"))) %>%
  ungroup()

data_gaps = data_gaps %>% 
  select(id, posix_local, diff_time, day_depart)
summary(data_gaps$diff_time)
data_gaps %>% 
  filter(diff_time>1) 
head(which(data_gaps$diff_time>1))
# data_gaps[77933:77936,]
data_gaps %>% 
  filter(diff_time>1000) 
head(which(data_gaps$diff_time>1000))
# data_gaps[1015797:1015799,]


stat = data_gaps %>% 
  group_by(id) %>%
  summarise(tot_tracking = sum(diff_time, na.rm=T),
            no_gaps = sum(diff_time[diff_time==1], na.rm=T),
            gaps = sum(diff_time[diff_time!=1], na.rm=T))
stat$no_gaps_PC = (stat$no_gaps / stat$tot_tracking) * 100
stat$gaps_PC = (stat$gaps / stat$tot_tracking) * 100
stat
# id     tot_tracking  no_gaps   gaps no_gaps_PC gaps_PC
# 22849b     12600877 12301269 299608       97.6    2.41
# 22850b     11991381 11015811 975570       91.8    8.16
# 27262       1370905  1209901 161004       88.3    11.7 
# 27262b     11923041 10928526 994515       91.6    8.37
# 93100      14279648 13604418 675230       95.2    4.75








#######################################################
# plot days since departure vs hours (z=depth)
#######################################################
hp = hp %>% 
  group_by(id) %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1)
summary(hp$day_depart)

hp %>%
  group_by(id) %>%
  summarise(start = first(day_depart),
            end   = last(day_depart))


# let's average `depth` by individuals, day since departure and hour
dataPlot = hp %>% 
  setDT(.) %>% 
  .[,.(depth = median(depth)), 
              by=.(id, day_depart, date, hour)]
summary(dataPlot$depth)

# display the result
ggplot(dataPlot, aes(x = day_depart, y = hour, fill = depth)) +
  geom_tile() + 
  facet_grid(id ~ .) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_tq() +
  labs(x = "Days since departure", 
       y = "Hour", 
       fill = "Median depth (m)") +
  theme(legend.position = c("bottom"))






############################################
# plot daylength vs month
############################################
# average `daylength` by individuals, day since departure and hour
dataPlot = hp %>% 
  setDT(.) %>% 
  .[,.(period = first(period)), 
    by=.(id, date, hour)]
dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
dataPlot = dataPlot %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01")

# display the result
ggplot(dataPlot, aes(x = date2, y = as.numeric(hour), fill = period)) +
  geom_tile() + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  facet_grid(id ~ .) +
  scale_x_date(date_labels="%b",date_breaks  ="month") +
  scale_fill_manual(values = c("#337882","#d7aa94","#2b4d68","#301748")) +
  theme_tq() +
  labs(x = "Time", y = "Hour", fill = "") +
  theme(legend.position = c("bottom"))


# day vs night
#--------------
dataPlot = dataPlot %>%
  mutate(day_night = case_when(period == "day" ~ "day",
                               period == "dusk" ~ "night",
                               period == "dawn" ~ "night",
                               period == "night" ~ "night"))

ggplot(dataPlot, aes(x = date2, y = as.numeric(hour), fill = day_night)) +
  geom_tile() + 
  facet_grid(id ~ .) +
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  scale_x_date(date_labels="%b",date_breaks  ="month") +
  scale_fill_manual(values = c("#d7aa94","#301748")) +
  theme_tq() +
  labs(x = "Time", y = "Hour", fill = "Day/night") +
  theme(legend.position = c("bottom"))

# hp %>% filter(month == "Oct") %>% 
#   select(c(id,date,sunrise,sunset,period)) %>% View()







################################################
# summarise daily mean(maxdep) and daylight
################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
dive <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")  %>%
  mutate(date = as.Date(substr(start, 1, 10)),
         hour = substr(start, 12, 13),
         month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  ungroup()
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec","Jan"))


daylight = dive %>%
  group_by(id, date) %>%
  summarise(daylight = as.numeric(difftime(sunset, sunrise, units = "hour")))
dive_select = dive %>% 
  select(id, period, month, date, maxdep, sunrise, sunset) 

daylight2 = daylight %>%
  left_join(dive_select, by = c("id", "date"))
daylight2

daylight3 = daylight2 %>%
  group_by(id, daylight, date) %>%
  summarise(mean_dep   = mean(maxdep),
            max_dep    = max(maxdep),
            median_dep = median(maxdep)) %>%
  ungroup
daylight3
saveRDS(daylight3, "./RDATA/4.daylight_HR.RDS")

# plots
#-------------
ggplot(daylight3, aes(y = date, x = daylight)) +
  geom_point(aes(colour=id), size=0.2) + 
  facet_grid(id ~ .) +
  scale_y_date(date_labels="%b",date_breaks  ="month") +
  theme_tq() +
  labs(y = "Time", x = "Daylight hours")

ggplot(daylight3, aes(y = mean_dep, x = daylight)) + 
  geom_point(aes(colour=id), size=0.2) + 
  facet_grid(id ~ ., scales="free_y") +
  geom_smooth(method="gam", aes(colour=id), 
              lwd=0.5, se=F, show.legend = T) +
  theme_tq() +
  labs(x = "Depth (m)", y = "Daylight hours")

ggplot(daylight3, aes(y = max_dep, x = daylight)) + 
  geom_point(aes(colour=id), size=0.2) + 
  facet_grid(id ~ ., scales="free_y") +
  geom_smooth(method="gam", aes(colour=id), 
              lwd=0.5, se=F, show.legend = T) +
  theme_tq() +
  labs(x = "Max depth (m)", y = "Daylight hours")

ggplot(daylight3, aes(y = median_dep, x = daylight)) + 
  geom_point(aes(colour=id), size=0.2) + 
  facet_grid(id ~ ., scales="free_y") +
  geom_smooth(method="gam", aes(colour=id), 
              lwd=0.5, se=F, show.legend = T) +
  theme_tq() +
  labs(x = "Median depth (m)", y = "Daylight hours")






#----------------------------------------
# histo dive depth vs period and month
#----------------------------------------
## Max depth
sum <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")
names(sum)
summary(sum$maxdep)
sum$month = substr(sum$start, 6, 7)
table(sum$month, sum$period)

ggplot(data = sum[sum$month!="01",], 
       aes(x=maxdep, fill=period, colour=period)) +
  geom_density(aes(y=..scaled..,alpha=.4)) +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  labs(y = "Dive probability", x = "(m)", 
       title = "Dive depth",fill="") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() +
  facet_grid(period~month) +
  theme(legend.position = "none",
        axis.text  = element_text(size=7, hjust=0.5),
        strip.text = element_text(colour='white',size=10,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")))







###############################
# plot 3D
###############################
library("plot3D")
# set x, y and z coordinates
names(hp)
dataPlot = hp %>% 
  setDT(.) %>% 
  .[,.(depth = median(depth)), 
    by=.(id, day_depart, date, hour)]
dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
dataPlot = dataPlot %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01")

names(dataPlot)
x <- dataPlot$hour
y <- dataPlot$day_depart
z <- -dataPlot$depth

scatter3D(x, y, z, clab = c("Depth (m)"), bty = "b2",#"f"
          pch=19, cex=0.6, ticktype = "detailed", col = viridis(64),
          ylab="Day after departure", zlab="Depth (m)", xlab="Hour of the day",
          theta=30, phi=0) # viewing direction (theta: azimuthal direction, phi: co-latitude)







##############################################
# density plot
##############################################

# max depth vs hour of the day
#----------------------------------
ndive_summary <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")
names(ndive_summary)
ndive_summary$hour = as.numeric(substr(as.character(ndive_summary$start), 
                                       12, 13))
ndive_summary$month = as.factor(substr(as.character(ndive_summary$start), 
                                       6, 7))

ggplot(ndive_summary[ndive_summary$month=="11",], aes(y = -maxdep, x = hour)) +
  geom_density_2d_filled(contour_var = "ndensity", 
                         bins = 8) + 
  scale_fill_brewer(palette = 3, direction = 1) + # 3,9,12
  facet_wrap(vars(id), ncol=5) +
  labs(x = "Hour of the day", y = "Dive depth (m)",
       title = "October", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.text = element_text(colour='white',size=10,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l



# max depth vs hours of the day/ID
#----------------------------------
ggplot(daylight3, aes(y=-maxdep, x=daylight)) +
  geom_density_2d_filled(contour_var = "ndensity", 
                         bins = 8) + 
  scale_fill_brewer(palette = 3, direction = 1) + # 3,9,12
  facet_wrap(vars(id), ncol=5) +
  labs(x = "Photoperiod (n hours/day)", y = "Dive depth (m)",
       title = "", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.text = element_text(colour='white',size=10,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l


# max depth vs photoperiod/month
#--------------------------------
ggplot(daylight3, aes(y=-maxdep, x=daylight)) +
  geom_density_2d_filled(contour_var = "ndensity", 
                         bins = 8) + 
  scale_fill_brewer(palette = 3, direction = 1) + # 3,9,12
  facet_wrap(vars(month), ncol=6) +
  labs(x = "Photoperiod (n hours/day)", y = "Dive depth (m)",
       title = "Months", fill="n dives \nprobability") +
  theme_tq() +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.text = element_text(colour='white',size=10,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l

















#################################
# extract coordinates from loc
#################################

# import locations for the 5 HP
#----------------------------------
loc <- readRDS("./RDATA/1.locations_filtered_5HP.rds") %>% 
  dplyr::select(id, posix_local, bathy, lon, lat) %>%
  rename(date = posix_local)
loc
names(ndive_summary)
ndive_summary = ndive_summary %>% 
  rename(date = start)

# convert into data tables
#-------------------------
diveDT = setDT(ndive_summary)
setkey(diveDT,id,date)
locDT = setDT(loc)
setkey(locDT,id,date)

# rolling join on date
dive2 = locDT[,.(date, id, lon, lat, bathy)] %>%
  .[diveDT, roll="nearest"] %>%
  .[] # to display tibble
names(dive2)
summary(dive2$lon)
summary(dive2$lat)
dive2 -> dive
rm(dive2)
dive = as_tibble(dive)
names(dive)
colnames(dive)[1] = "start"
dive$date = as.Date(dive$start) 

sun  = getSunlightTimes(data = dive, tz="America/Nuuk", 
                        keep=c("sunrise","sunset","dusk",
                               "dawn","night","nightEnd")) 
head(sun)
dive2 = cbind(dive, sun[,c("sunrise","sunset","dusk",
                       "dawn","night","nightEnd")]) %>%
  as_tibble()
names(dive2)
dive2 -> dive
rm(dive2)
dive$period = as.factor(dive$period)





###########################
# Hours after sunrise
###########################
dive = dive %>%
  mutate(hrs_aft_sunrise = as.numeric((start - sunrise) / 3600), # in hours
         hrs_aft_sunrise_round = round(hrs_aft_sunrise), 
         day_night = case_when(hrs_aft_sunrise_round >= 0 ~ 'day',
                               TRUE ~ "night"))
table(dive$day_night)
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
saveRDS(dive, "./RDATA/4.HP_diveSummary_Period.RDS")





# maxdep vs hours_after_sunrise
#----------------------------------
ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=maxdep)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Max depth (m)", x = "Hours after sunrise", 
       title= "", fill="") +
  scale_x_discrete(labels=c("-12","-8","-4","0","4","8","12","16"),
                     breaks=c("-12","-8","-4","0","4","8","12","16")) +
  scale_y_continuous(trans = "reverse", limits=c(260, 0)) +
  theme_tq()

ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=maxdep)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Max depth (m)", x = "Hours after sunrise", 
       title= "", fill="") +
  facet_wrap(~id, ncol = 5) +
  ylim(0,300) +
  scale_y_continuous(trans = "reverse") +
  theme_tq()


# dive duration vs hours_aft_sunrise
#-----------------------------------
ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=dur/60)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Dive duration (min)", x = "Hours after sunrise", 
       title= "", fill="") +
  scale_x_discrete(labels=c("-12","-8","-4","0","4","8","12","16"),
                   breaks=c("-12","-8","-4","0","4","8","12","16")) +
  ylim(0, 6.3) +
  theme_tq()

# ascent rate
#--------------
ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=asc_rate)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Ascent rate (m/s)", x = "Hours after sunrise", 
       title= "", fill="") +
  ylim(0, 0.8) +
  theme_tq()

# descent rate
#--------------
ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=des_rate)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Descent rate (m/s)", x = "Hours after sunrise", 
       title= "", fill="") +
  ylim(0, 1.5) +
  theme_tq()

# bottom time 
#--------------
ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=bot_dur)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Bottom time (sec)", x = "Hours after sunrise", 
       title= "", fill="") +
  ylim(0, 22) +
  theme_tq()

# n deep dives/day or hours
# proxy of foraging efficiency?
#--------------------------------
deep_dives = dive %>%
  mutate(hour = substr(start, 12, 13)) %>%
  filter(maxdep > 100) %>%
  group_by(id, date, hour) %>%
  summarise(ndeep_dives = n(),
            hrs_aft_sunrise_round = unique(hrs_aft_sunrise_round),
            day_night = first(day_night),
            month = unique(month)) %>%
  ungroup()
deep_dives

ggplot(deep_dives, aes(x=as.factor(hrs_aft_sunrise_round), y=ndeep_dives)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "N dives > 100 m", x = "Hours after sunrise", fill="") +
  facet_wrap(~month) +
  ylim(0,20) +
  theme_tq()





#########################################
# recovery times (after deep dive)
#########################################
dive = dive[order(dive$id, dive$start),]
dive %>% select(start, end, id) 

dive = dive %>%
  group_by(id) %>%
  mutate(start_next = lead(start)) %>% # select the next dive start
  ungroup()
tail(dive %>% select(start, end, start_next))
tail(dive %>% filter(id=="27262b") %>% select(start, end, start_next))

dive = dive %>%
  group_by(id) %>%
  mutate(recovery = as.numeric(start_next - start)) %>%
  ungroup()

summary(dive$recovery)
dive %>% select(id, start, end, start_next, recovery, maxdep)
dive[dive$recovery < 0,]  # refers to the alst row of each ID
nrow(dive[dive$recovery > 2000,]) / nrow(dive) * 100 # 6.4 %
hist(dive$recovery, xlim=c(0,2000), breaks=100)
dive %>% 
  select(id, dive, start, end, start_next, recovery, maxdep) %>% 
  filter(recovery > 1000) %>%
  View() %>%
  
  ggplot(dive, aes(x=recovery, y=maxdep)) +
  geom_point(size=0.1) +
  xlim(0,1000)

ggplot(dive, aes(x=as.factor(hrs_aft_sunrise_round), y=recovery)) +
  geom_boxplot(outlier.shape = NA, aes(fill=day_night)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Recovery (min)", x = "Hours after sunrise", 
       title= "", fill="") +
  scale_x_discrete(labels=c("-12","-8","-4","0","4","8","12","16"),
                   breaks=c("-12","-8","-4","0","4","8","12","16")) +
  ylim(0,600) +
  theme_tq()



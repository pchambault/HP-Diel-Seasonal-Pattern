################################################################
###########    MAX DEPTH and SEASONAL, DIEL PATTERNS  ##########
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
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/")
hp <- readRDS(".HP/RDATA/1.HP_2013-2014_filtered.RDS")
names(hp)

ndive_summary <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")
names(ndive_summary)
ndive_summary = ndive_summary %>% 
  mutate(month = substr(start, 6, 7))
str(ndive_summary)



##############################################
# proportion of deep dives across months
##############################################
names(ndive_summary)
prop = ndive_summary %>%
  group_by(id, month) %>%
  summarize(ntot = max(dive),
            dives0_20m   = n_distinct(dive[maxdep<=20]),
            dives20_50m  = n_distinct(dive[maxdep>20 & maxdep<=50]),
            dives50_100m = n_distinct(dive[maxdep>50 & maxdep<=100]),
            dives_deep100m = n_distinct(dive[maxdep>100])) %>%
  ungroup()
prop
prop = prop %>%
  mutate(dives0_20m     = (dives0_20m / ntot) * 100,
         dives20_50m    = (dives20_50m / ntot) * 100,
         dives50_100m   = (dives50_100m / ntot) * 100,
         dives_deep100m = (dives_deep100m / ntot) * 100)
prop


# geom_bar (position:fill): x:month, y=prop
# barplot: month vs dive prop (fill=depth_class)
#------------------------------------------------
pivot = prop %>%
  pivot_longer(c(dives0_20m, dives20_50m, 
                 dives50_100m, dives_deep100m), 
               names_to = "depth_class", values_to = "prop") %>%
  select(id,month,ntot,depth_class,prop) %>%
  filter(month!="01")
pivot

pivot = pivot %>%
  mutate(class = case_when(depth_class == "dives0_20m" ~ "0-20 m",
                           depth_class == "dives20_50m" ~ "20-50 m",
                           depth_class == "dives50_100m" ~ "50-100 m",
                           depth_class == "dives_deep100m" ~ "> 100 m"))

pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", "> 100 m"))
ggplot(pivot, aes(y=prop, x=month)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  facet_wrap(~id, ncol=5) +
  # scale_fill_brewer(palette = "PiYG") +
  scale_fill_brewer(palette = 1, direction = 1) +
  # scale_fill_brewer(palette = "BuPu") +
  labs(x = "Months", y = "Dive frequency (%)",fill = "") +
  theme_tq() +
  theme(axis.title = element_text(size=7, hjust=0.5),
        # legend.position = "bottom",
        axis.text  = element_text(size=6, hjust=0.5, angle=0),
        axis.line.y.left = element_line(size=0.2),
        axis.line.x.bottom = element_line(size=0.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.6,0.3,0.4,0.6),"cm"))








############################################
# n daily dives
############################################
dive <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")
names(dive)
dive = dive %>% 
  mutate(hour  = substr(start, 12, 13),
         date  = as.Date(start), #substr(start, 1, 10),
         month = substr(start, 6, 7)) %>%
  ungroup()
str(dive)
dive = dive[order(dive$date),]


# depth class
#----------------
summary(dive$maxdep)  # 5.5 to 362 m
dive$class_maxdep = cut(dive$maxdep, 
                        breaks=c(0,20,50,100,200,365))
unique(dive$class_maxdep)

# proportions of dives/class/day/id
#-----------------------------------
prop = dive %>%
  dplyr::group_by(id, date) %>%
  dplyr::summarize(ntot         = max(dive),
                   dives0_20m   = n_distinct(dive[maxdep<=20]),
                   dives20_50m  = n_distinct(dive[maxdep>20 & maxdep<=50]),
                   dives50_100m = n_distinct(dive[maxdep>50 & maxdep<=100]),
                   dives100_200m = n_distinct(dive[maxdep>100 & maxdep<=200]),
                   dives_deep200m = n_distinct(dive[maxdep>200])) %>%
  dplyr::ungroup()
prop

prop = prop %>%
  mutate(dives0_20m     = (dives0_20m / ntot) * 100,
         dives20_50m    = (dives20_50m / ntot) * 100,
         dives50_100m   = (dives50_100m / ntot) * 100,
         dives100_200m  = (dives100_200m / ntot) * 100,
         dives_deep200m = (dives_deep200m / ntot) * 100)

pivot = prop %>%
  pivot_longer(c(dives0_20m, dives20_50m, 
                 dives50_100m, dives100_200m, dives_deep200m), 
               names_to = "depth_class", values_to = "prop") %>%
  select(id,date,ntot,depth_class,prop)

pivot = pivot %>%
  mutate(class = case_when(depth_class == "dives0_20m" ~ "0-20 m",
                           depth_class == "dives20_50m" ~ "20-50 m",
                           depth_class == "dives50_100m" ~ "50-100 m",
                           depth_class == "dives100_200m" ~ "100-200 m",
                           depth_class == "dives_deep200m" ~ "> 200 m"))

pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", "100-200 m", "> 200 m"))

pivot$date2 = as.Date(paste0("2022",substr(pivot$date,5,10)))
pivot = pivot %>%
  group_by(id) %>%
  mutate(day_depart = as.numeric(date - first(date) + 1)) %>%
  ungroup()


# histogram: prop vs month, fill=class
pivot = pivot %>%
  mutate(month = substr(date, 6, 7)) %>%
  filter(month != "01")
ggplot(pivot, aes(y=prop, x=month)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  # facet_wrap(~id, ncol=5) + 
  scale_fill_brewer(palette = 1, direction = 1) +
  labs(x = "", y = "Dive frequency (%)", fill="Depth \nclass") +
  theme_tq() 

# histogram: prop vs id, fill=class
ggplot(pivot, aes(y=prop, x=id)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  scale_fill_brewer(palette = 1, direction = 1) +
  labs(x = "", y = "Dive frequency (%)", fill="Depth \nclass") +
  theme_tq() 

# histogram: prop vs time
ggplot(pivot, aes(y=prop, x=day_depart)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  facet_wrap(~id, ncol=5) + 
  scale_fill_brewer(palette = 1, direction = 1) +
  labs(x = "", y = "Dive frequency (%)") +
  theme_tq() +
  theme(axis.title = element_text(size=7, hjust=0.5),
        axis.text  = element_text(size=6, hjust=0.5, angle=0),
        axis.line.y.left = element_line(size=0.2),
        axis.line.x.bottom = element_line(size=0.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.6,0.3,0.4,0.6),"cm"))

# prop vs time, fill=class
ggplot(pivot, aes(y=prop, x=day_depart, colour=class)) +
  geom_line() +
  facet_grid(class~id) +
  labs(x = "", y = "Dive frequency (%)") +
  theme_tq()








############################################
# n hourly dives over time / ID
############################################

# n dives/hour/month/id
#-----------------------------------
prop = dive %>%
  dplyr::group_by(id, hour, date) %>%
  dplyr::summarize(ntot           = n_distinct(dive),
                   dives0_20m     = n_distinct(dive[maxdep<=20]),
                   dives20_50m    = n_distinct(dive[maxdep>20 & maxdep<=50]),
                   dives50_100m   = n_distinct(dive[maxdep>50 & maxdep<=100]),
                   dives100_200m  = n_distinct(dive[maxdep>100 & maxdep<=200]),
                   dives_deep200m = n_distinct(dive[maxdep>200])) %>%
  dplyr::ungroup()
prop

pivot = prop %>%
  pivot_longer(c(dives0_20m, dives20_50m, 
                 dives50_100m, dives100_200m, dives_deep200m), 
               names_to = "depth_class", values_to = "n") 

pivot = pivot %>%
  mutate(class = case_when(depth_class == "dives0_20m" ~ "0-20 m",
                           depth_class == "dives20_50m" ~ "20-50 m",
                           depth_class == "dives50_100m" ~ "50-100 m",
                           depth_class == "dives100_200m" ~ "100-200 m",
                           depth_class == "dives_deep200m" ~ "> 200 m"))
pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", "100-200 m", "> 200 m"))
pivot$month = substr(pivot$date, 6, 7)
pivot$month = factor(pivot$month,
                     levels=c("07","08","09","10","11","12","01"))
pivot = pivot %>% filter(month != "01")
pivot = pivot[,-c(4)]
pivot$date2 = as.Date(paste0("2022",substr(pivot$date,5,10)))
saveRDS(pivot, "./RDATA/5.HourlyDive_perdDay_depClass.RDS")


# ndive/hour vs days, fill=class
ggplot(pivot, aes(y=n, x=date2, colour=class)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  facet_wrap(~id, ncol=5) + 
  scale_colour_brewer(palette = 1, direction = 1) +
  scale_fill_brewer(palette = 1, direction = 1) +
  labs(x = "", y = "n hourly dives", fill="Depth \nclass",
       title="Hourly dive frequency") +
  theme_tq() 

# histogram: prop vs id, fill=class
ggplot(pivot, aes(y=n, x=month)) +
  geom_histogram(aes(fill=class), stat="identity",
                 position="fill") +
  facet_wrap(~id, ncol=5) + 
  scale_fill_brewer(palette = 1, direction = 1) +
  labs(x = "", y = "Dive frequency (%)", fill="Depth \nclass") +
  theme_tq() 










#####################################################
# n dives hourly over time and depth class
# geom_tile: y=hour, x=days, fill=freq hourly ndive
#####################################################
dive <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS") # 1 row=1 dive
names(dive)
dive = dive %>% 
  mutate(hour  = substr(start, 12, 13),
         date  = as.Date(start), #substr(start, 1, 10),
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

# identify sunrise and sunset/day
#---------------------------------
start_end = dat %>%
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

ggplot(pivot[pivot$class=="100-200 m",], aes(x = date2, y = hour)) +
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




############################################
# dive duration over time / ID
############################################

# n dives/hour/month/id
#-----------------------------------
prop = dive %>%
  dplyr::group_by(id, hour, date) %>%
  dplyr::summarize(dives0_20m     = mean(dur[maxdep<=20], na.rm=T)/60,
                   dives20_50m    = mean(dur[maxdep>20 & maxdep<=50], na.rm=T)/60,
                   dives50_100m   = mean(dur[maxdep>50 & maxdep<=100], na.rm=T)/60,
                   dives100_200m  = mean(dur[maxdep>100 & maxdep<=200], na.rm=T)/60,
                   dives_deep200m = mean(dur[maxdep>200], na.rm=T)/60) %>%
  dplyr::ungroup()
prop
summary(prop)

pivot = prop %>%
  pivot_longer(c(dives0_20m, dives20_50m, 
                 dives50_100m, dives100_200m, dives_deep200m), 
               names_to = "depth_class", values_to = "n") 

pivot = pivot %>%
  mutate(class = case_when(depth_class == "dives0_20m" ~ "0-20 m",
                           depth_class == "dives20_50m" ~ "20-50 m",
                           depth_class == "dives50_100m" ~ "50-100 m",
                           depth_class == "dives100_200m" ~ "100-200 m",
                           depth_class == "dives_deep200m" ~ "> 200 m"))
pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", "100-200 m", "> 200 m"))
pivot$month = substr(pivot$date, 6, 7)
pivot$month = factor(pivot$month,
                     levels=c("07","08","09","10","11","12","01"))
pivot = pivot %>% filter(month != "01")
pivot$date2 = as.Date(paste0("2022",substr(pivot$date,5,10)))
pivot$hour = as.numeric(pivot$hour)

# mean dive dur/hour vs days, fill=class
ggplot(pivot[pivot$class=="100-200 m" & !is.na(pivot$n),], 
       aes(x = date2, y = hour)) +
  geom_tile(aes(fill = n)) + 
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
  labs(x = "", y = "Hour (UTC-2)", fill = "Dive duration \n(min)") 









#-----------------------------------
# recovery times (after deep dive)
#-----------------------------------
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




###################################################
# plot dive depth vs duration: 
# to detect depth transition between day and night
###################################################
dive <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")
names(dive)
table(dive$id)
id = dive %>%
  filter(id == "22850b") %>%
  mutate(month = substr(date, 6, 7)) 
id$period = as.factor(id$period)
# View(id)

id2 = id %>% 
  filter(month =="09") %>%
  select(depth, depth_cor, posix_local, date, month, hour, sunrise, sunset, period, phase)
id2

ggplot(id2[25000:100000,], aes(x=posix_local, y=-depth, group=month)) +
  geom_path(aes(colour=period)) +
  scale_colour_manual(values = c("navajowhite3","dodgerblue3"))

dat = id2[25000:32000,]
ggplot(dat, aes(x=posix_local, y=-depth, group=month)) +
  geom_path(aes(colour=period)) +
  scale_colour_manual(values = c("navajowhite3","dodgerblue3")) +
  theme_tq() 


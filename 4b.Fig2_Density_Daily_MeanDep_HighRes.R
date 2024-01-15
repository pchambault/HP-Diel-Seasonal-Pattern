######################################################
##    FIG 2: plot geom_density meanDep vs months    ##
##         from high resolution dataset             ##
##  panel a: daily mean depth across months         ##
##  panel b: dive frequency across hours and months ##
######################################################

library(patchwork)
library(data.table)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyquant)
library(scales)
library(gridExtra)
library(lubridate)





#############################################
# plot a: density plot high-resolution tags 
#############################################

#----------------------------
# mean daylength per month
#----------------------------
hr <- readRDS("./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS") %>%
  mutate(month = format(date, "%b")) %>%
  filter(month != "Jan")
names(hr)
hr$month = factor(hr$month, levels=c("Jul","Aug","Sep",
                                     "Oct","Nov","Dec"))
hr %>%
  group_by(month) %>%
  summarise(mean_photo = round(mean(daylength)),
            min_photo  = round(min(daylength)),
            max_photo  = round(max(daylength)))
#   month mean_photo min_photo max_photo
# 1 Jul           19        18        20
# 2 Aug           17        15        19
# 3 Sep           13        11        15
# 4 Oct           10         8        11
# 5 Nov            7         5         8
# 6 Dec            5         4         6



#------------------------------------------------
# load high resolution: 1 row=1 dive
#------------------------------------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive_hr <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS")  %>%
  mutate(hour  = substr(start_local, 12, 13),
         month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(month != "Jan") %>%
  ungroup()
dive_hr$month = factor(dive_hr$month, levels=c("Jul","Aug","Sep",
                                               "Oct","Nov","Dec"))


#--------------------------------------------
# calculate mean daily depth per phase and ID
#--------------------------------------------
daily_hr = dive_hr %>%
  group_by(id, date, day_night) %>%
  summarise(mean_dep = mean(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily_hr$month = factor(daily_hr$month, levels=c("Jul","Aug","Sep",
                                                 "Oct","Nov","Dec"))

#-------------------------------------------------------------
# Kolmogorov-Smirnov test to compare day/night distributions
#-------------------------------------------------------------
## Jul ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Jul"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Jul"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Aug ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Aug"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Aug"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Sep ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Sep"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Sep"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Oct ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Oct"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Oct"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Nov ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Nov"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Nov"]
ks.test(day, night) # p<0.001: the 2 datasets do come from the same distribution

## Dec ##
day = daily_hr$mean_dep[daily_hr$day_night=="day" & daily_hr$month=="Dec"]
night = daily_hr$mean_dep[daily_hr$day_night=="night" & daily_hr$month=="Dec"]
ks.test(day, night) # p<0.05 (p=0.02): the 2 datasets do come from the same distribution



#--------------------------------------------------
## Compare distributions at night across months ##
#--------------------------------------------------
# library("ggpubr")
# ggboxplot(daily[daily$day_night=="night",], 
#           x = "month", y = "mean_dep", 
#           color = "month", 
#           ylab = "Hourly mean depth", xlab = "Months")

kruskal.test(mean_dep ~ month, 
             data = daily_hr[daily_hr$day_night=="night",]) # p<0.001
kruskal.test(mean_dep ~ month, 
             data = daily_hr[daily_hr$day_night=="day",])   # p<0.001



#---------------------------------
# get mean daily depth/month/phase
# to add text on plot
#---------------------------------
mean_hr = daily_hr %>%
  group_by(day_night, month) %>%
  summarise(val = mean(mean_dep)) %>%
  ungroup()
mean_hr

text_day_mean_hr = mean_hr %>% 
  filter(day_night == "day") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 180,
         y = c(0.03,0.02,0.015,0.01,0.0075,0.0075))
text_night_mean_hr = mean_hr %>% 
  filter(day_night == "night") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 195,
         y = c(0.03,0.02,0.015,0.01,0.0075,0.0075))


#-------------------------------------------------
# calculate t tests between day and night / month
#-------------------------------------------------
stat_hr = daily_hr %>%
  dplyr::select(-c(id,date)) %>%
  group_by(month) %>%
  kruskal_test(mean_dep ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         mean_dep  = "Mean depth",
         y = c(0.03,0.02,0.015,0.01,0.0075,0.0075)) %>%
  ungroup()
stat_hr




#------------------------
# plot a: mean depth
# high-resolution tags
#------------------------
# n daily mean depth per month and phase
n_day_hr = daily_hr %>%
  filter(day_night == "day") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.03,0.025,0.014,0.011,0.007,0.008),
         day_night = "day") %>%
  ungroup()
n_night_hr = daily_hr %>%
  filter(day_night == "night") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.03,0.025,0.014,0.011,0.007,0.008),
         day_night = "night") %>%
  ungroup()

a = ggplot(data = daily_hr,
           aes(x = mean_dep, fill = day_night, colour = day_night)) +
  geom_density(alpha=.6,lwd=0.2) +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(210, 0)) +
  scale_fill_manual(values = c("navajowhite1","#595b54")) +
  labs(y = "Density", x = "Daily mean depth (m)",
       title = "a) Dive depth (high-resolution tags)", fill="") +
  facet_grid(~month, scales="free") +
  theme_bw() +
  geom_text(data=n_night_hr,
            aes(x=13, y = y, label=n),
            size=2.5, colour = "#595b54") +
  geom_text(data=n_day_hr,
            aes(x=0, y = y, label=n),
            size=2.5, colour = "navajowhite3") +
  geom_text(data=stat_hr,
            aes(x=165, y = y,
                label=p.adj.signif),
            size=2.5, colour = "#595b54") +
  geom_text(data=text_day_mean_hr,
            aes(x=values, y = y, label=text),
            size=2.5, colour = "navajowhite3") +
  geom_text(data=text_night_mean_hr,
            aes(x=values, y = y, label=text),
            size=2.5, colour = "#595b54") +
  geom_vline(data=mean_hr, aes(xintercept=val, colour=day_night),
             lwd=0.2, linetype = "dashed") +
  scale_colour_manual(values = c("navajowhite2","#595b54"),
                      guide="none") +
  theme(legend.position = c(0.08,0.6),
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.4,0.2,0,0.1), "cm"), # t r b l
        panel.border = element_rect(linewidth = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing.y = unit(0.0, "lines"),
        panel.grid.minor = element_blank())











#######################################
# Panel b: low-resolution density plot
#######################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive_lr <- readRDS("./RDATA/0c.depth_LR_1row_1dive_19ids_tzCorrected.RDS") %>%
  dplyr::select(id,date,posix_local,sunrise,sunset,period,depth) %>% 
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
length(unique(dive_lr$id)) # 8
unique(dive_lr$id)


#------------------------------
# calculate mean daily depth
#------------------------------
daily_lr = dive_lr %>%
  group_by(id, date, day_night) %>%
  summarise(mean_dep = mean(depth)) %>%
  mutate(month       = format(date, "%b")) %>%
  ungroup()

daily_lr$month = factor(daily_lr$month, levels=c("Jul","Aug","Sep",
                                                 "Oct","Nov","Dec"))


# discard data in Dec due to very few dives
#-------------------------------------------
daily_lr$mean_dep[daily_lr$month == "Dec"] = NA


#---------------------------------
# get mean daily depth/month/phase
# to add text on plot
#---------------------------------
mean_lr = daily_lr %>%
  group_by(day_night, month) %>%
  summarise(val = mean(mean_dep))

text_day_mean_lr = mean_lr %>% 
  filter(day_night == "day") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 180)
text_night_mean_lr = mean_lr %>% 
  filter(day_night == "night") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 195)


#---------------------------------------------------
# calculate t tests between day and night / month
#---------------------------------------------------
stat_lr = daily_lr %>%
  filter(month != "Dec") %>%
  dplyr::select(-c(id,date)) %>%
  group_by(month) %>%
  kruskal_test(mean_dep ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         mean_dep  = "Mean depth")



#-------------------------------------------------------------
# Kolmogorov-Smirnov test to compare day/night distributions
#-------------------------------------------------------------
## Jul ##
day   = daily_lr$meandep[daily_lr$day_night=="day" & daily_lr$month=="Jul"]
night = daily_lr$meandep[daily_lr$day_night=="night" & daily_lr$month=="Jul"]
ks.test(day, night) # p=0.0014: the 2 datasets do NOT come from the same distribution

## Aug ##
day   = daily_lr$meandep[daily_lr$day_night=="day" & daily_lr$month=="Aug"]
night = daily_lr$meandep[daily_lr$day_night=="night" & daily_lr$month=="Aug"]
ks.test(day, night) # p<0.0001: the 2 datasets do NOT come from the same distribution

## Sep ## !!
day   = daily_lr$meandep[daily_lr$day_night=="day" & daily_lr$month=="Sep"]
night = daily_lr$meandep[daily_lr$day_night=="night" & daily_lr$month=="Sep"]
ks.test(day, night) # p=0.41: the 2 datasets DO come from the same distribution

## Oct ##
day = daily_lr$meandep[daily_lr$day_night=="day" & daily_lr$month=="Oct"]
night = daily_lr$meandep[daily_lr$day_night=="night" & daily_lr$month=="Oct"]
ks.test(day, night) # p<0.0001: the 2 datasets do NOT come from the same distribution

## Nov ##
day = daily_lr$meandep[daily_lr$day_night=="day" & daily_lr$month=="Nov"]
night = daily_lr$meandep[daily_lr$day_night=="night" & daily_lr$month=="Nov"]
ks.test(day, night) # p<0.005: the 2 datasets do NOT from the same distribution


#--------------
# scales free
#--------------
stat_lr$y            = c(0.05,0.04,0.017,0.015,0.0125) #0.05,0.03,0.02,0.015,0.0125
text_day_mean_lr$y   = c(0.05,0.04,0.017,0.015,0.0125,0)
text_night_mean_lr$y = c(0.05,0.04,0.017,0.015,0.0125,0)
text_day_mean_lr = text_day_mean_lr %>%
  filter(month != "Dec")
text_night_mean_lr = text_night_mean_lr %>%
  filter(month != "Dec")

# n daily mean depth per month and phase
#---------------------------------------
n_day_lr = daily_lr %>%
  filter(day_night == "day") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.05,0.04,0.02,0.015,0.0125,0),
         day_night = "day") %>%
  ungroup() %>%
  filter(month != "Dec")

n_night_lr = daily_lr %>%
  filter(day_night == "night") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.05,0.04,0.02,0.015,0.0125,0),
         day_night = "night") %>%
  ungroup() %>%
  filter(month != "Dec")


# plot panel b
#----------------
b = ggplot(data = daily_lr,
           aes(x=mean_dep, fill=day_night, colour=day_night)) +
  geom_density(alpha=.6,lwd=0.2) +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(210, 0)) +
  scale_fill_manual(values = c("navajowhite1","#595b54")) +
  labs(y = "Density", x = "Daily mean depth (m)",
       title = "b) Dive depth (low-resolution tags)", fill="") +
  facet_grid(~month, scales="free") +
  theme_bw() +
  geom_text(data=n_night_lr,
            aes(x=13, y = y, label=n),
            size=2.5, colour = "#595b54") +
  geom_text(data=n_day_lr,
            aes(x=0, y = y, label=n),
            size=2.5, colour = "navajowhite3") +
  geom_text(data=stat_lr,
            aes(x=165, y = y,
                label=p.adj.signif),
            size=2.5, colour = "#595b54") +
  geom_text(data=text_day_mean_lr,
            aes(x=values, y = y, label=text),
            size=2.5, colour = "navajowhite3") +
  geom_text(data=text_night_mean_lr,
            aes(x=values, y = y, label=text),
            size=2.5, colour = "#595b54") +
  geom_vline(data=mean_lr, aes(xintercept=val,
                               colour = day_night),
             lwd=0.2, linetype = "dashed") +
  scale_colour_manual(values = c("navajowhite2","#595b54"),
                      guide="none") +
  theme(legend.position = c(0.08,0.6),
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key   = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0,
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.4,0.2,0,0.1), "cm"), 
        panel.border = element_rect(linewidth = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing.y = unit(0.0, "lines"),
        panel.grid.minor = element_blank())













#################################
# Panel c: hourly dive frequency 
#################################

# calculate hourly number of dives per depth class 
#----------------------------------------------------
hourly = dive_hr %>%
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
summary(hourly$ntot)                          # mean: 17.4 dives/hour
sd(hourly$ntot)                               # mean: 6.7 dives/hour
summary(hourly$ntot[hourly$month=="Jul"])     # mean: 14 dives/hour
summary(hourly$ntot[hourly$month=="Dec"])     # mean: 16 dives/hour

summary(hourly$dives0_20m)                    # mean: 4 dives/hour
summary(hourly$dives50_100m)                  # mean: 3 dives/hour
summary(hourly$dives_deep100m)                # mean: 3 dives/hour

# average number of dives/hour for shallow dives across months
summary(hourly$dives0_20m[hourly$month=="Jul"])     # mean: 7 dives/hour
summary(hourly$dives0_20m[hourly$month=="Dec"])     # mean: 2 dives/hour

# average number of dives/hour for deep dives across months
summary(hourly$dives_deep100m[hourly$month=="Jul"]) # mean: 0.4 dives/hour
summary(hourly$dives_deep100m[hourly$month=="Dec"]) # mean: 5.4 dives/hour




# mean hourly number of dives per depth class and month 
#------------------------------------------------------
mean_hourly = hourly %>%
  group_by(hour, month) %>%
  summarise(tot_dive = mean(ntot),
            mean_dives0_20m     = mean(dives0_20m),
            mean_dives20_50m    = mean(dives20_50m),
            mean_dives50_100m   = mean(dives50_100m),
            mean_dives_deep100m = mean(dives_deep100m)) %>%
  ungroup()

pivot = mean_hourly %>%
  pivot_longer(c(mean_dives0_20m, mean_dives20_50m, 
                 mean_dives50_100m, mean_dives_deep100m), 
               names_to = "depth_class", values_to = "n") %>%
  dplyr::select(hour,month,depth_class,n) %>%
  filter(month != "Jan") %>%
  mutate(class = case_when(depth_class == "mean_dives0_20m" ~ "0-20 m",
                           depth_class == "mean_dives20_50m" ~ "20-50 m",
                           depth_class == "mean_dives50_100m" ~ "50-100 m",
                           depth_class == "mean_dives_deep100m" ~ ">100 m"))
pivot$class = factor(pivot$class,
                     levels=c("0-20 m","20-50 m",
                              "50-100 m", ">100 m"))
pivot$month = factor(pivot$month,
                     levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))


# extract sunset and sunrise daily and per hour
#----------------------------------------------
times <- dive_hr %>%
  group_by(id, date) %>%
  summarise(sunrise_mean = mean(sunrise),
            sunset_mean  = mean(sunset)) %>%
  mutate(sunrise_hour = as.numeric(format(round_date(sunrise_mean, unit="hours"),"%H")),
         sunset_hour  = as.numeric(format(round_date(sunset_mean, unit="hours"),"%H")),
         month        = format(date, "%b")) %>%
  ungroup() 

# mean sunrise and sunset times per month
times_month = times %>%
  group_by(month) %>%
  summarise(sunrise_round = round(mean(sunrise_hour)),
            sunset_round  = round(mean(sunset_hour)),
            sunrise       = mean(sunrise_hour),
            sunset        = mean(sunset_hour)) %>%
  ungroup() 
times_month
times_month$month = factor(times_month$month,
                           levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))



#--------------------------------------------------------
# create a tibble containing all hours for each month
# to plot day vs night at bottom of each panel
#--------------------------------------------------------
dates = seq(as.POSIXct("2014-07-01 00:00:00"), 
            as.POSIXct("2014-12-31 23:59:00"), 
            "hour") %>%
  as_tibble() %>%
  rename(dateTime = value) %>%
  mutate(hour  = as.numeric(format(dateTime, "%H")),
         date  = as.Date(dateTime),
         month = format(dateTime, "%b"),
         day   = format(dateTime, "%d")) %>%
  filter(day == 15) # take middle of the month to be representative
dates

times_month
dates = dates %>% 
  mutate(day_night = case_when(month == "Jul" & hour <=4   ~ "night",
                               month == "Jul" & hour >=21  ~ "night",
                               month == "Aug" & hour <=5   ~ "night",
                               month == "Aug" & hour >=22  ~ "night",
                               month == "Sep" & hour <=7   ~ "night",
                               month == "Sep" & hour >=20  ~ "night",
                               month == "Oct" & hour <=8   ~ "night",
                               month == "Oct" & hour >=18  ~ "night",
                               month == "Nov" & hour <=9   ~ "night",
                               month == "Nov" & hour >=16  ~ "night",
                               month == "Dec" & hour <=10  ~ "night",
                               month == "Dec" & hour >=15  ~ "night",
                               TRUE ~"day"))
dates
dates$month = factor(dates$month,
                     levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))




#---------------
# plot panel c
#---------------
c = ggplot(pivot, aes(y=n, x=hour)) +       
  geom_col(aes(fill=class, colour=class), position="stack") +
  scale_fill_brewer(palette = 1, direction = 1) +
  scale_colour_brewer(palette = 1, direction = 1) +
  scale_x_continuous(labels=c("00:00","05:00","10:00",
                              "15:00","20:00"),
                     breaks=c(0,5,10,15,20),
                     expand = c(0, 0)) +
  labs(x = "Hours of the day", y = "Mean number of dives",
       fill = "Depth class", title = "c) Dive frequency (high-resolution tags)") +
  scale_y_continuous(limits = c(-0.3, 22), expand = c(0, 0)) +
  facet_wrap(~month, ncol=6) +
  geom_point(data = dates[dates$day_night=="day",], 
             aes(x=hour, y=-0.2), colour="navajowhite2", size=2, shape=15)  +
  geom_point(data = dates[dates$day_night=="night",], 
             aes(x = hour, y=-0.2), colour="#595b54", size=2, shape=15) +
  theme_tq() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.box.margin = margin(-10,-10,-10,-10),# t r b l
        legend.key  = element_blank(),
        axis.text.x = element_text(size=7, hjust=1, vjust=1, angle=45),
        axis.text.y = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title = element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour = "white"),
        plot.margin  = unit(c(0,0.2,0.3,0.1), "cm"),         # t r b l
        panel.border = element_rect(linewidth = 0.2),
        panel.spacing.x  = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing.y  = unit(0.0, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = "none")












######################################
# export plot with the 3 panels
######################################
(a / b / c) + plot_layout(heights = c(1,1,1.4),
                          width   = c(1,1,1))
ggsave(filename=paste0("./PAPER/5.SciRep/3.Review_Dec2023/Fig.2.pdf"),
       grid.arrange(a,b,c,ncol=1),
       width=190, height=170, units="mm", dpi=400, family="ArialMT") # width=190, height=127














##########################################
# Provide magnitude to compare meandep 
# between July and Dec
##########################################
daily = dive %>%
  group_by(id, date) %>%
  summarise(mean_dep = mean(maxdep, na.rm=T),
            max_dep  = max(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily$month = factor(daily$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))
month = daily %>% 
  group_by(month) %>%
  summarise(meandep = mean(mean_dep),
            maxdep  = mean(max_dep))
month$meandep[month$month == "Dec"] / month$meandep[month$month == "Jul"] # 2.9
month$maxdep[month$month == "Dec"] / month$maxdep[month$month == "Jul"]   # 1.4




#######################################################
nrow(dive[dive$maxdep>100,]) / nrow(dive) * 100 # 20%
nrow(dive[dive$maxdep>50,]) / nrow(dive) * 100  # 39%
mean(dive$maxdep).  # 60 m


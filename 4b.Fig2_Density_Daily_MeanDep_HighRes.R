################################################
## FIG 2: plot geom_density meanDep vs months ##
##  panel a: daily mean depth across months   ##
##  panel b: dive frequency across hours and months
##         AFTER timezone correction     #######
################################################

library(patchwork)
library(data.table)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyquant)
library(scales)


#############################################
# mean daylength per month
#############################################
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







###############################################
# load high resolution
###############################################
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


#-----------------------------------------
# calculate daily depth per phase and ID
#----------------------------------------
daily = dive %>%
  group_by(id, date, day_night) %>%
  summarise(mean_dep = mean(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily$month = factor(daily$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

#-------------------------------------------------------
# Kolmogorov-Smirnov test to compare both distributions
#-------------------------------------------------------
## Jul ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Jul"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Jul"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Aug ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Aug"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Aug"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Sep ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Sep"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Sep"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Oct ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Oct"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Oct"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Nov ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Nov"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Nov"]
ks.test(day, night) # p<0.001: the 2 datasets do come from the same distribution

## Dec ##
day = daily$mean_dep[daily$day_night=="day" & daily$month=="Dec"]
night = daily$mean_dep[daily$day_night=="night" & daily$month=="Dec"]
ks.test(day, night) # p<0.05 (p=0.02): the 2 datasets DO NOT come from the same distribution



#--------------------------------------------------
## Compare distributions at night across months ##
#--------------------------------------------------
library("ggpubr")
ggboxplot(daily[daily$day_night=="night",], 
          x = "month", y = "mean_dep", 
          color = "month", 
          ylab = "Hourly mean depth", xlab = "Months")

kruskal.test(mean_dep ~ month, 
             data = daily[daily$day_night=="night",]) # p<0.001
kruskal.test(mean_dep ~ month, 
             data = daily[daily$day_night=="day",])   # p<0.001




#--------------
# pivot_longer
#--------------
pivot = daily %>%
  pivot_longer(c(mean_dep), 
               names_to = "metric", values_to = "values") 
pivot$month = factor(pivot$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))
pivot = pivot %>%
  mutate(metric2 = case_when(metric == "mean_dep" ~ "Mean depth"))




#---------------------------------
# get mean daily depth/month/phase
# to add text on plot
#---------------------------------
mean = pivot %>%
  group_by(day_night, month, metric2) %>%
  summarise(val = mean(values)) %>%
  ungroup()
mean

text_day_mean = mean %>% 
  filter(day_night == "day") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 180)
text_night_mean = mean %>% 
  filter(day_night == "night") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 195)





##################################################
# calculate t tests between day and night / month
##################################################
stat_meandep = pivot %>%
  dplyr::select(-c(id,date,metric2)) %>%
  group_by(month) %>%
  kruskal_test(values ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         metric2 = "Mean depth") %>%
  ungroup()
stat_meandep








###############################
# plot a: mean depth
###############################
text_day_meandep = text_day_mean[text_day_mean$metric2=="Mean depth",]
text_night_meandep = text_night_mean[text_night_mean$metric2=="Mean depth",]
mean_meandep = mean[mean$metric2 == "Mean depth",]

#--------------
# scales free
#--------------
stat_meandep$y = c(0.03,0.02,0.015,0.01,0.0075,0.0075)
text_day_meandep$y = c(0.03,0.02,0.015,0.01,0.0075,0.0075)
text_night_meandep$y = c(0.03,0.02,0.015,0.01,0.0075,0.0075)

# n daily mean depth per month and phase
n_day = pivot %>%
  filter(day_night == "day") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.03,0.025,0.014,0.011,0.007,0.008),
         day_night = "day") %>%
  ungroup()
n_night = pivot %>%
  filter(day_night == "night") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = c(0.03,0.025,0.014,0.011,0.007,0.008),
         day_night = "night") %>%
  ungroup()

a = ggplot(data = pivot,
           aes(x=values, fill=day_night, colour=day_night)) +
  geom_density(alpha=.6,lwd=0.2) +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(210, 0)) +
  scale_fill_manual(values = c("navajowhite1","#595b54")) +
  labs(y = "Density", x = "Daily mean depth (m)",
       title = "a) Dive depth", fill="") +
  facet_grid(~month, scales="free") +
  theme_bw() +
  geom_text(data=n_night,
            aes(x=9, y = y, label=n),
            size=2, colour = "#595b54") +
  geom_text(data=n_day,
            aes(x=0, y = y, label=n),
            size=2, colour = "navajowhite3") +
  geom_text(data=stat_meandep,
            aes(x=165, y = y,
                label=p.adj.signif),
            size=2, colour = "#595b54") +
  geom_text(data=text_day_meandep,
            aes(x=values, y = y, label=text),
            size=2, colour = "navajowhite3") +
  geom_text(data=text_night_meandep,
            aes(x=values, y = y, label=text),
            size=2, colour = "#595b54") +
  geom_vline(data = mean_meandep, aes(xintercept=val,
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












###############################
# b) hourly dive frequency 
###############################

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
summary(hourly$ntot)          # mean: 17.4 dives/hour
sd(hourly$ntot)               # mean: 6.7 dives/hour
summary(hourly$ntot[hourly$month=="Jul"])     # mean: 14 dives/hour
summary(hourly$ntot[hourly$month=="Dec"])     # mean: 16 dives/hour

summary(hourly$dives0_20m)    # mean: 4 dives/hour
summary(hourly$dives50_100m)  # mean: 3 dives/hour
summary(hourly$dives_deep100m)# mean: 3 dives/hour

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
library(lubridate)
times <- dive %>%
  group_by(id, date) %>%
  summarise(sunrise_mean = mean(sunrise),
            sunset_mean  = mean(sunset)) %>%
  mutate(sunrise_hour = as.numeric(format(round_date(sunrise_mean, unit="hours"),"%H")),
         sunset_hour  = as.numeric(format(round_date(sunset_mean, unit="hours"),"%H")),
         month = format(date, "%b")) %>%
  ungroup() 

# mean sunrise and sunset times per month
times_month = times %>%
  group_by(month) %>%
  summarise(sunrise_round = round(mean(sunrise_hour)),
            sunset_round  = round(mean(sunset_hour)),
            sunrise = mean(sunrise_hour),
            sunset  = mean(sunset_hour)) %>%
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
  filter(day == 15)
dates

times_month
dates = dates %>% 
  mutate(day_night = case_when(month == "Jul" & hour <=4  ~ "night",
                               month == "Jul" & hour >=21 ~ "night",
                               month == "Aug" & hour <=5   ~ "night",
                               month == "Aug" & hour >=22 ~ "night",
                               month == "Sep" & hour <=7   ~ "night",
                               month == "Sep" & hour >=20 ~ "night",
                               month == "Oct" & hour <=8   ~ "night",
                               month == "Oct" & hour >=18 ~ "night",
                               month == "Nov" & hour <=9   ~ "night",
                               month == "Nov" & hour >=16  ~ "night",
                               month == "Dec" & hour <=10 ~ "night",
                               month == "Dec" & hour >=15 ~ "night",
                               TRUE ~"day"))
dates
dates$month = factor(dates$month,
                     levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))



# plot panel b
#---------------
b = ggplot(pivot, aes(y=n, x=hour)) +       
  geom_col(aes(fill=class, colour=class), position="stack") +
  scale_fill_brewer(palette = 1, direction = 1) +
  scale_colour_brewer(palette = 1, direction = 1) +
  scale_x_continuous(labels=c("00:00","05:00","10:00",
                              "15:00","20:00"),
                     breaks=c(0,5,10,15,20),
                     expand = c(0, 0)) +
  labs(x = "Hours of the day", y = "Mean number of dives",
       fill = "Depth class", title = "b) Dive frequency") +
  scale_y_continuous(limits = c(-0.3, 22), expand = c(0, 0)) +
  facet_wrap(~month, ncol=6) +
  geom_point(data = dates[dates$day_night=="day",], 
             aes(x=hour, y=-0.2), colour="navajowhite2", size=2, shape=15)  +
  geom_point(data = dates[dates$day_night=="night",], 
             aes(x=hour, y=-0.2), colour="#595b54", size=2, shape=15) +
  theme_tq() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.box.margin = margin(-10,-10,-10,-10),# t r b l
        legend.key = element_blank(),
        axis.text.x = element_text(size=7, hjust=1, vjust=1, angle=45),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0,0.2,0.3,0.1), "cm"), # t r b l
        panel.border = element_rect(linewidth = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing.y = unit(0.0, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = FALSE)




# export plot with both panels
#--------------------------------
# grid.arrange(a,b,ncol=1)
library(gridExtra)
(a / b) + plot_layout(heights = c(1,1.4),
                      width   = c(1,1))
ggsave(filename=paste0("./PAPER/4.PRSB/Fig.2.pdf"),
       grid.arrange(a,b,ncol=1),
       width=190,height=127,units="mm",dpi=400,family="ArialMT")







#######################################################
nrow(dive[dive$maxdep>100,]) / nrow(dive) * 100 # 20%
nrow(dive[dive$maxdep>50,]) / nrow(dive) * 100  # 39%
mean(dive$maxdep)














##########################################
# meandep: 3 times deeper in December
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




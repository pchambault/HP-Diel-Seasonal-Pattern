##################################################
## SI FIG 1: plot geom_density meanDep vs months ##
##         low resolution datasets
##          after timezone correction     ########
##################################################

library(patchwork)
library(data.table)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gridExtra)



###############################################
# reshape low resolution dataset
###############################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive <- readRDS("./RDATA/0c.depth_Binned_LR_19ids_tzCorrected.RDS") %>%
  select(id,date,posix_local,sunrise,sunset,period,depth) %>%
  mutate(month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(id != "7618",id != "7617") %>%
  filter(month != "Jan") 
dive %>% select(posix_local,sunrise,sunset,day_night)
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))


#------------------------------
# calculate mean daily depth
#------------------------------
daily = dive %>%
  group_by(id, date, day_night) %>%
  summarise(meandep = mean(depth),
            maxdep  = max(depth)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily


#--------------
# pivot_longer
#--------------
pivot = daily %>%
  pivot_longer(c(maxdep, meandep), 
               names_to = "metric", values_to = "values") 
pivot
pivot$month = factor(pivot$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))
pivot = pivot %>%
  mutate(metric2 = case_when(metric == "meandep" ~ "Mean depth",
                             metric == "maxdep" ~ "Maximum depth"))
pivot$metric2 = factor(pivot$metric2, levels = c("Mean depth","Maximum depth"))




#---------------------------------
# get mean daily depth/month/phase
# to add text on plot
#---------------------------------
mean = pivot %>%
  group_by(day_night, month, metric2) %>%
  summarise(val = mean(values))
mean

text_day_mean = mean %>% 
  filter(day_night == "day" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 280)
text_night_mean = mean %>% 
  filter(day_night == "night" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 300)

text_day_max = mean %>% 
  filter(day_night == "day" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 470)
text_night_max = mean %>% 
  filter(day_night == "night" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 500)





##################################################
# calculate t tests between day and night / month
##################################################
stat_meandep = pivot %>%
  dplyr::select(-c(id,date,metric2)) %>%
  filter(metric == "meandep") %>%
  group_by(month) %>%
  kruskal_test(values ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         metric2 = "Mean depth")

stat_maxdep = pivot %>%
  dplyr::select(-c(id,date,metric2)) %>%
  filter(metric == "maxdep") %>%
  group_by(month) %>%
  kruskal_test(values ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         metric2 = "Maximum depth")

stat = bind_rows(stat_meandep, stat_maxdep)
stat$metric2 = factor(stat$metric2, levels = c("Mean depth",
                                               "Maximum depth"))
pivot$metric2 = factor(pivot$metric2, levels = c("Mean depth",
                                                 "Maximum depth"))





###############################
# plot a: mean depth
###############################
#--------------
# scales free
#--------------
text_day_meandep = text_day_mean[text_day_mean$metric2=="Mean depth",]
text_night_meandep = text_night_mean[text_night_mean$metric2=="Mean depth",]
mean_meandep = mean[mean$metric2 == "Mean depth",]

stat_meandep$y       = rep(0.004,6)
text_day_meandep$y   = rep(0.004,6)
text_night_meandep$y = rep(0.004,6)

# n daily mean depth per month and phase
n_day = pivot %>%
  filter(day_night == "day") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = rep(0.004,6),
         day_night = "day") %>%
  ungroup()
n_night = pivot %>%
  filter(day_night == "night") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = rep(0.004,6),
         day_night = "night") %>%
  ungroup()

library(scales)
a = ggplot(data = pivot,
       aes(x=values, fill=day_night, colour=day_night)) +
  geom_density(alpha=.6,lwd=0.2) +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(450, 0)) +
  scale_fill_manual(values = c("navajowhite1","#595b54")) +
  labs(y = "Density", x = "Daily mean depth (m)",
       title = "Dive depth", fill="") +
  # facet_grid(~month) +
  facet_grid(~month, scales="free") +
  theme_bw() +
  geom_text(data=n_night,
            aes(x=15, y = y, label=n),
            size=2, colour = "#595b54") +
  geom_text(data=n_day,
            aes(x=0, y = y, label=n),
            size=2, colour = "navajowhite3") +
  geom_text(data=stat_meandep,
            aes(x=360, y = y,
                label=p.adj.signif),
            size=2, colour = "#595b54") +
  geom_text(data=text_day_meandep,
            aes(x=390, y = y, label=text),
            size=2, colour = "navajowhite3") +
  geom_text(data=text_night_meandep,
            aes(x=420, y = y, label=text),
            size=2, colour = "#595b54") +
  geom_vline(data = mean_meandep, aes(xintercept=val,
                                      colour = day_night),
             lwd=0.2, linetype = "dashed") +
  scale_colour_manual(values = c("navajowhite2","#595b54"),
                      guide="none") +
  theme(legend.position = c(0.08,0.55),
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        # axis.text.x = element_text(size=7, hjust=0.5, angle=0),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
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


ggsave(filename=paste0("./FIGURES/PAPER/SI.Fig.1_density.DailyDepth.LR.tzCorr.pdf"),
       width=7,height=3,units="in",dpi=400,family="ArialMT")









# ###############################
# # b) hourly dive frequency 
# ###############################
# 
# # calculate hourly number of dives per depth class 
# #----------------------------------------------------
# hourly = dive %>%
#   mutate(month = format(posix_local, "%b"),
#          hour  = as.numeric(format(posix_local, "%H"))) %>%
#   group_by(id, hour, date) %>%
#   summarise(ntot           = n(),
#             dives0_20m     = n_distinct(depth<=20),
#             dives20_50m    = n_distinct(depth>20 & depth<=50),
#             dives50_100m   = n_distinct(depth>50 & depth<=100),
#             dives_deep100m = n_distinct(depth>100)) %>%
#   ungroup() %>%
#   mutate(month = format(date, "%b"))
# str(hourly)
# 
# 
# # mean hourly number of dives per depth class and month 
# #------------------------------------------------------
# mean_hourly = hourly %>%
#   group_by(hour, month) %>%
#   summarise(tot_dive = mean(ntot),
#             mean_dives0_20m     = mean(dives0_20m),
#             mean_dives20_50m    = mean(dives20_50m),
#             mean_dives50_100m   = mean(dives50_100m),
#             mean_dives_deep100m = mean(dives_deep100m)) %>%
#   ungroup()
# 
# pivot = mean_hourly %>%
#   pivot_longer(c(mean_dives0_20m, mean_dives20_50m, 
#                  mean_dives50_100m, mean_dives_deep100m), 
#                names_to = "depth_class", values_to = "n") %>%
#   dplyr::select(hour,month,depth_class,n) %>%
#   filter(month != "Jan") %>%
#   mutate(class = case_when(depth_class == "mean_dives0_20m" ~ "0-20 m",
#                            depth_class == "mean_dives20_50m" ~ "20-50 m",
#                            depth_class == "mean_dives50_100m" ~ "50-100 m",
#                            depth_class == "mean_dives_deep100m" ~ ">100 m"))
# pivot$class = factor(pivot$class,
#                      levels=c("0-20 m","20-50 m",
#                               "50-100 m", ">100 m"))
# pivot$month = factor(pivot$month,
#                      levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# # extract sunset and sunrise daily and per hour
# #----------------------------------------------
# library(lubridate)
# times <- dive %>%
#   group_by(id, date) %>%
#   summarise(sunrise = mean(sunrise),
#             sunset  = mean(sunset)) %>%
#   mutate(sunrise_hour = as.numeric(format(round_date(sunrise, unit="hours"),"%H")),
#          sunset_hour  = as.numeric(format(round_date(sunset, unit="hours"),"%H")),
#          month = format(date, "%b")) %>%
#   ungroup() 
# 
# # mean sunrise and sunset times per month
# times_month = times %>%
#   group_by(month) %>%
#   summarise(sunrise_round = round(mean(sunrise_hour)),
#             sunset_round  = round(mean(sunset_hour)),
#             sunrise = mean(sunrise_hour),
#             sunset  = mean(sunset_hour)) %>%
#   ungroup() 
# times_month
# times_month$month = factor(times_month$month,
#                            levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# 
# #--------------------------------------------------------
# # create a tibble containing all hours for each month
# # to plot day vs night at bottom of each panel
# #--------------------------------------------------------
# dates = seq(as.POSIXct("2014-07-01 00:00:00"), 
#             as.POSIXct("2014-12-31 23:59:00"), "hour") %>%
#   as_tibble() %>%
#   rename(dateTime = value) %>%
#   mutate(hour  = as.numeric(format(dateTime, "%H")),
#          date  = as.Date(dateTime),
#          month = format(dateTime, "%b"),
#          day   = format(dateTime, "%d")) %>%
#   filter(day == 15)
# dates
# 
# times_month
# dates = dates %>% 
#   mutate(day_night = case_when(month == "Jul" & hour <=4  ~ "night",
#                                month == "Jul" & hour >=21 ~ "night",
#                                month == "Aug" & hour <5   ~ "night",
#                                month == "Aug" & hour >=22 ~ "night",
#                                month == "Sep" & hour <7   ~ "night",
#                                month == "Sep" & hour >=20 ~ "night",
#                                month == "Oct" & hour <8   ~ "night",
#                                month == "Oct" & hour >=18 ~ "night",
#                                month == "Nov" & hour <9   ~ "night",
#                                month == "Nov" & hour >15  ~ "night",
#                                month == "Dec" & hour <=10 ~ "night",
#                                month == "Dec" & hour >=15 ~ "night",
#                                TRUE ~"day"))
# dates
# dates$month = factor(dates$month,
#                      levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# 
# # plot panel b
# #---------------
# b = ggplot(pivot, aes(y=n, x=hour)) +       
#   geom_col(aes(fill=class, colour=class), position="stack") +
#   scale_fill_brewer(palette = 1, direction = 1) +
#   scale_colour_brewer(palette = 1, direction = 1) +
#   scale_x_continuous(labels=c("00:00","05:00","10:00",
#                               "15:00","20:00"),
#                      breaks=c(0,5,10,15,20),
#                      expand = c(0, 0)) +
#   labs(x = "Hours of the day", y = "Mean number of dives",
#        fill = "Depth class", title = "b) Dive frequency") +
#   scale_y_continuous(limits = c(-0.3, 22), expand = c(0, 0)) +
#   facet_wrap(~month, ncol=6) +
#   geom_point(data = dates[dates$day_night=="day",], 
#              aes(x=hour, y=-0.2), colour="navajowhite2", size=2, shape=15)  +
#   geom_point(data = dates[dates$day_night=="night",], 
#              aes(x=hour, y=-0.2), colour="#595b54", size=2, shape=15) +
#   theme_tq() +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(0.8, 'cm'),     #change legend key size
#         legend.key.height = unit(0.3, 'cm'),   #change legend key height
#         legend.key.width = unit(0.3, 'cm'),    #change legend key width
#         legend.text = element_text(size=8),    #change legend text font size
#         legend.background = element_blank(),
#         legend.box.margin = margin(-10,-10,-10,-10),# t r b l
#         legend.key = element_blank(),
#         axis.text.x = element_text(size=7, hjust=1, vjust=1, angle=45),
#         axis.text.y  = element_text(size=7, hjust=0.5),
#         title = element_text(colour="black",size=10,face="bold"),
#         plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
#                                 colour="black"),
#         strip.text = element_text(size=9, colour = "white"),
#         strip.background = element_rect(fill = "steelblue4",
#                                         colour="white"),
#         plot.margin = unit(c(0,0.2,0.3,0.1), "cm"), # t r b l
#         panel.border = element_rect(linewidth = 0.2),
#         panel.spacing.x = unit(0.0, "lines"),
#         panel.grid.major = element_blank(),
#         panel.spacing.y = unit(0.0, "lines"),
#         panel.grid.minor = element_blank()) +
#   guides(color = FALSE)
# 
# 
# 
# 
# # export plot with both panels
# #--------------------------------
# (a / b) + plot_layout(heights = c(1,1.4),
#                       width   = c(1,1))
# ggsave(filename=paste0("./FIGURES/PAPER/SI.Fig.2_density.DailyDepth.LR.tzCorr.pdf"),
#        grid.arrange(a,b,ncol=1),
#        width=7,height=5,units="in",dpi=400,family="ArialMT")







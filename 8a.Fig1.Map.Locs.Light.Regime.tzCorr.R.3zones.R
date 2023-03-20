###############################################################
##### FIGURE 1: map of locations and daylength vs time ########
#####.          after time zone correction 
#####.        for 3 phases: day, twilight, night
###############################################################

library(tidyverse)
library(ggplot2)
library(raster)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(animation)
library(gridExtra)
library(patchwork)
library(suncalc)
library(oce)
library(rCAT)


###################################
# load locations datasets
###################################
loc <- readRDS("./RDATA/0a.locations_filtered_17HP_tzCorrected.rds") %>%
  mutate(month = format(date, "%b"),
         day_depart = as.numeric(date - first(date)) + 1) %>%
  filter(day_depart != 1)
loc$month = factor(loc$month, 
                   levels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
names(loc)
length(unique(loc$id))  # 17

# check locs on land
summary(loc$bathy_200)



# number of dives in total
#---------------------------
table <- readRDS("./RDATA/6.Table_summary_17ids_tzCorr.RDS")
str(table)
table$ndive = as.numeric(gsub("\\,", "", table$ndive))








#########################################
# n dives in July and December
#########################################

# depth: low resolution
#------------------------
dep_lr <- readRDS("./RDATA/0c.depth_Binned_LR_19ids_tzCorrected.RDS") %>%
  mutate(date  = as.Date(posix_local),
         hour  = substr(posix_local, 12, 13),
         month = format(date, "%b")) %>%
  dplyr::select(c(id,posix_local,date,hour,depth,month)) %>%
  filter(id != "7618",id != "7617") %>%
  mutate(dataset = "Low resolution")
dep_lr = dep_lr[order(dep_lr$date),]
dep_lr
unique(dep_lr$id)


# depth: high resolution
#-----------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dep_hr <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS")  %>%
  mutate(date  = as.Date(substr(start_local, 1, 10)),
         hour  = substr(start_local, 12, 13),
         month = format(date, "%b"),
         dataset = "Low resolution") %>%
  dplyr::rename(posix_local = start_local,
                depth = maxdep) %>%
  dplyr::select(c(id,posix_local,date,hour,depth,month,dataset)) %>%
  ungroup()
dep_hr = dep_hr[order(dep_hr$date),]
names(dep_hr)
names(dep_lr)

dep = rbind(dep_lr, dep_hr)
unique(dep$id) # 17 porpoises
table(dep$id)

# n dives in July and Dec
#--------------------------
ndive_jul = dep %>% filter(month == "Jul") %>% 
  summarise(ndive = n()) # 14410
ndive_jul = as.numeric(ndive_jul)

ndive_dec = dep %>% filter(month == "Dec") %>% 
  summarise(ndive = n()) # 21325
ndive_dec = as.numeric(ndive_dec)






####################################
# plot panel a: map of locations
####################################
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Canada"
                      | north_map$region=="Iceland",]

# import raster of reference to generate grid
#----------------------------------------------
bathy = raster("./ENV.DATA/Bathy/GEBCO_Arctic2.nc")
bathy[bathy>0] = NA
bathy = crop(bathy, extent(-100,20,50,90))
res(bathy)
bathy = aggregate(bathy, fact=4)
res(bathy)  # 0.03 decimal deg


# convert to tibble
#------------------------------
bathy_df = rasterToPoints(bathy) %>% as_tibble()
bathy_df = bathy_df %>% rename(lon = x, lat = y)
bathy_df$layer = abs(bathy_df$layer)

# plot panel a
#--------------
a = ggplot(shore, aes(long, lat)) +
  geom_contour_filled(data=bathy_df, alpha=0.7,
                      aes(lon,lat,z=layer), 
                      breaks=c(0,50,100,500,1000,2000,6000)) +
  scale_fill_brewer(palette="Blues") + 
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",lwd=0.1,colour="black") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc, aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, size=0.3, colour="red") +
  annotate("text", y = 79, x=-42, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = paste0(length(unique(loc$id))," individuals"), size=1) +
  annotate("text", y = 74, x=-40, 
           label = paste0(nrow(loc)," locations"), size=1) +
  annotate("text", y = 73, x=-40, 
           label = paste0(sum(table$ndive)," dives"), size=1) +
  geom_hline(yintercept = 66, colour = "blue", 
             size = 0.3, linetype=2) +
  labs(y = "Latitude (°N)", x = "Longitude (°W)", 
       fill="Bathymetry \n(m)", title = "a) All locations") +
  theme(axis.text.x  = element_text(size=6, vjust = 0.5, hjust=0.5,
                                    margin = margin(r = 0)),
        axis.text.y  = element_text(size=6, hjust=1, vjust = 1),
        axis.ticks   = element_blank(),
        axis.title   = element_text(size=6,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.14,"cm"),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.15,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", linewidth=0.1),
        plot.margin = unit(c(0.1,0.1,0.2,-0.2), "cm")) + # t r b l: top,right,bottom,left
  guides(fill = guide_legend(nrow = 3))








##################################################
# plot panel b: locs in July with day/night areas
##################################################

# convert to tibble
#------------------------------
bathy_df = rasterToPoints(bathy) %>% as_tibble()
bathy_df = bathy_df %>% 
  mutate(date = as.POSIXct("2014-07-15 23:54:00", # time at sunset
                           tz="America/Nuuk")) %>%
  rename(lon = x, lat = y)


# extract sun position
#----------------------
sunpos = sunAngle(as.POSIXct(bathy_df$date), 
                  bathy_df$lon, bathy_df$lat) %>%
  as_tibble()
sunpos
summary(sunpos$altitude) 

sunpos = sunpos %>%
  mutate(period  = case_when(altitude > 0 ~ 3,                       #Day
                             altitude > (-18) & altitude < (0) ~ 2,  #Twilight
                             altitude < (-18) ~ 1))                  #Night
summary(sunpos$altitude) # -19 to +19 degrees
table(sunpos$period) / nrow(sunpos) * 100
sunpos = sunpos %>% bind_cols(bathy_df)

# mean sunrise and sunset on 15 July from all locations
#----------------------------------------------------
sun = loc %>% 
  filter(date == as.Date("2014-07-16")) %>%  # 15/07 not available in dataset so took 16/07
  summarise(min_sunrise  = min(sunrise),
            mean_sunrise = mean(sunrise),
            max_sunrise  = max(sunrise),
            min_sunset   = max(sunset),
            mean_sunset  = mean(sunset),
            max_sunset   = min(sunset)) %>%
  mutate(daylength = as.numeric(difftime(mean_sunset, 
                                         mean_sunrise,
                                         units = "hours")))
data.frame(sun)


# plot
#---------
b = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",
               lwd=0.1,colour="black") +
  geom_contour_filled(data=sunpos, aes(lon,lat,z=altitude), alpha=0.8,
                      breaks=c(-18,0,max(sunpos$altitude))) +
  scale_fill_manual(values = c("#a2a38c","navajowhite1"),
                                labels=c("Twilight","Day")) +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Jul",], aes(lon,lat), 
             stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, 
             size=0.5, colour="red") +
  annotate("text", y = 79, x=-42, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 75, x=-40,
           label = "Mean daylength: 19h", size=1) +
  annotate("text", y = 74, x=-40,
           label = paste0("Mean sunrise: ",
                          substr(mean(sun$mean_sunrise),12,16)), size=1) +
  annotate("text", y = 73, x=-40,
           label = paste0("Mean sunset: ",
                          substr(mean(sun$mean_sunset),12,16)), size=1) +
  annotate("text", y = 72, x=-40,
           label = paste0(length(unique(loc$id[loc$month=="Jul"])),
                          " individuals"), size=1) +
  annotate("text", y = 71, x=-40, size=1,
           label = paste0(nrow(loc[loc$month=="Jul",])," locations")) +
  annotate("text", y = 70, x=-40.1, 
           label = paste0(ndive_jul," dives"), size=1) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "", x = "Longitude (°W)", title = "b) July", 
       fill = "Solar \nelevation") + #Light regime
  theme(axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5,
                                   margin = margin(r = 0)),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=6,face="bold", hjust=0.5),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.14,"cm"),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.15,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.1,0.2,0.1), "cm")) + # t r b l
  guides(fill = guide_legend(nrow = 2))








##################################################
# plot panel c: locs in Dec with day/night areas
##################################################
# https://en.tutiempo.net/day-night/?utc=20141201T1412
bathy_df = rasterToPoints(bathy) %>% as_tibble()
bathy_df = bathy_df %>% 
  mutate(date = as.POSIXct("2014-12-15 14:09:00", # at sunset
                           tz="America/Nuuk")) %>%
  rename(lon = x, lat = y)

# extract sun position
#----------------------
sunpos = sunAngle(as.POSIXct(bathy_df$date), 
                  bathy_df$lon, bathy_df$lat) %>%
  as_tibble() %>%
  mutate(period  = case_when(altitude > 0 ~ 3,                    #Day
                             altitude > (-18) & altitude < 0 ~ 2, #Twilight
                             altitude < (-18) ~ 1)) %>%           #Night 
  bind_cols(bathy_df)
summary(sunpos$altitude) # -24 to +16 degrees
table(sunpos$period) / nrow(sunpos) * 100


# sunrise and sunset in December
#-------------------------------
dates = seq.Date(from=as.Date("2014-12-01"),
                 to=as.Date("2014-12-31"),
                 by="1 day")
sun = getSunlightTimes(date=dates, 
                       lon=-52.54, lat=65.25,
                       keep=c("sunrise","sunset"),
                       tz="America/Nuuk") %>% as_tibble()
sun %>% summarise(min_sunrise  = min(sunrise),
                  mean_sunrise = mean(sunrise),
                  max_sunrise  = max(sunrise),
                  min_sunset   = max(sunset),
                  mean_sunset  = mean(sunset),
                  max_sunset   = min(sunset))

# plot
#--------
c = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",
               lwd=0.1, colour="black") +
  geom_contour_filled(data=sunpos, 
                      aes(lon,lat,z=altitude), alpha=0.8,
                      breaks=c(-25,-18,0,max(sunpos$altitude))) +
  scale_fill_manual(values = c("black","#a2a38c","navajowhite1"),
                              labels=c("Night","Twilight","Day")) +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Dec",], 
             aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, 
             size=0.5, colour="red") +
  annotate("text", y = 79, x=-43, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 75, x=-40,
           label = "Mean daylength: 5h", size=1) +
  annotate("text", y = 74, x=-40,
           label = paste0("Mean sunrise: ",substr(mean(sun$sunrise),12,16)), size=1) +
  annotate("text", y = 73, x=-40,
           label = paste0("Mean sunset: ",substr(mean(sun$sunset),12,16)), size=1) +
  annotate("text", y = 72, x=-40,
           label = paste0(length(unique(loc$id[loc$month=="Dec"])),
                          " individuals"), size=1) +
  annotate("text", y = 71, x=-40, size=1, 
           label = paste0(nrow(loc[loc$month=="Dec",])," locations")) +
  annotate("text", y = 70, x=-40.2, 
           label = paste0(ndive_dec," dives"), size=1) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "", x = "Longitude (°W)", title = "c) December", 
       fill = "Solar \nelevation") +
  theme(axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5,
                                   margin = margin(r = 0)),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=6,face="bold", hjust=0.5),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.14,"cm"),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.15,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,-0.2,0.2,0.1), "cm")) +  # top,right,bottom,left
  guides(fill = guide_legend(nrow = 3))







###############################
# export plot
###############################
(a | b | c) 
ggsave(filename=paste0("./FIGURES/PAPER/Fig.1_Map.Locs_3zones.pdf"),
       width=5,height=2.5,units="in",dpi=400,family="ArialMT")

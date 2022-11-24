#######################################################
###             DAY NIGHT WORLDMAP 
#######################################################


library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(animation)
source("terminator.R")
#https://stackoverflow.com/questions/48384058/world-map-showing-day-and-night-regions

time = as.POSIXct("2014-12-15 14:18:00 GMT",
                  tz="America/Nuuk")
time = as.POSIXct("2014-07-15 23:45:00 GMT",
                  tz="America/Nuuk")

ggplot2::ggplot() +
  borders("world", colour = "gray90", fill = "gray85") +
  geom_ribbon(data = terminator(time, -180, 190, 0.1), 
              aes(lat, ymax = lon), ymin = -90, alpha = 0.4) +
  # coord_equal(xlim = c(-80, -30), ylim = c(55, 80), expand = 0) +
  ggthemes::theme_map()







#####################################################
# map regions of day vs night using a grid
#####################################################

# import raster of reference to generate grid
#----------------------------------------------
bathy = raster("./ENV.DATA/GEBCO_Arctic2.nc")
bathy
# plot(bathy)
bathy = crop(bathy, extent(-100,0,50,90))
res(bathy)
bathy = aggregate(bathy, fact=4)
res(bathy)


# convert to tibble
#------------------------------
bathy_df = rasterToPoints(bathy) %>% as_tibble()
bathy_df = bathy_df %>% 
  mutate(date = as.Date("2014-07-15",tz="America/Nuuk")) %>%
  rename(lon = x, lat = y)


# extract sun position
#----------------------
sunpos = getSunlightPosition(data = bathy_df,
                       keep=c("altitude","azimuth")) %>%
  mutate(date_dec = as.POSIXct("2014-07-15 23:50:00",
                               tz="America/Nuuk")) %>% as_tibble()
sunpos
summary(sunpos$altitude)
# altitude in radians: 0 at the horizon and PI/2 (1.52 rad, 90 deg) at the zenith (straight over your head)
# 0: sunrise and sunset 
# 6-12 deg: 
# sunpos = sunpos %>%
#   mutate(alt_deg = rad2deg(altitude),
#          period = case_when(alt_deg > 0 ~ "day",
#                             alt_deg > (-12) & alt_deg > (-6) ~ "dawn",
#                             alt_deg >(-12) ~"night"))
# table(sunpos$period)

sunpos = sunpos %>%
  mutate(alt_deg = rad2deg(altitude),
         period  = case_when(alt_deg > 0 ~ 3,
                             alt_deg > (-12) & alt_deg > (-6) ~ 2,
                             alt_deg >(-12) ~ 1))


summary(sunpos$alt_deg) # -12.6 to 20.7 degrees
# 0-6 deg  = civil twilight
# 6-12 deg = nautical twilight
# 12-18 deg= astronomical twilight
# >0    deg=sunrise
ggplot(shore, aes(long, lat)) +
  geom_contour_filled(data=sunpos, aes(lon,lat,z=alt_deg), alpha=0.6,
                      breaks=c(min(sunpos$alt_deg),-12,-6,0,max(sunpos$alt_deg))) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  # scale_fill_brewer(palette="Blues", direction=-1) +
  scale_fill_manual(values = c("black","darkblue","dodgerblue4", "navajowhite1"))  +
  theme_tq()


ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,85)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_contour_filled(data=sunpos, aes(lon,lat,z=alt_deg), alpha=0.6,
                      breaks=c(min(sunpos$alt_deg),-12,-6,0,max(sunpos$alt_deg))) +
  scale_fill_manual(values = c("black","darkblue","dodgerblue4", "navajowhite1"),
                    labels=c("Night","Astronomical twilight","Nautical twilight","Day"))  +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Jul",], aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, size=0.5, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 59, x=-45,
           label = "Mean daylength: 20h30", size=1) +
  annotate("text", y = 58, x=-45,
           label = paste0("Mean sunrise: ",substr(mean(sun$sunrise),12,16)), size=1) +
  annotate("text", y = 57, x=-45,
           label = paste0("Mean sunset: ",substr(mean(sun$sunset),12,16)), size=1) +
  annotate("text", y = 56, x=-45,
           label = paste0(length(unique(loc$id[loc$month=="Jul"])),
                          " individuals"), size=1) +
  annotate("text", y = 55, x=-45, size=1,
           label = paste0(nrow(loc[loc$month=="Jul",])," locations")) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "", x = "Longitude (°W)", title = "b) July", fill = "Sun \naltitude") +
  theme(axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=6,face="bold", hjust=0.5),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.1,0.2,0.1), "cm")) + # t r b l: top,right,bottom,left
  guides(color = guide_legend(override.aes = list(size=0.3)))









dates = seq.Date(from=as.Date("2014-07-01"),
                 to=as.Date("2014-07-31"),
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
# min_sunrise         mean_sunrise        max_sunrise         min_sunset          mean_sunset         max_sunset         
# 2014-07-01 02:37:41 2014-07-16 03:28:52 2014-07-31 04:22:54 2014-07-31 22:52:36 2014-07-16 23:45:06 2014-07-02 00:32:38       

time = terminator(as.POSIXct(mean(sun$sunset),
                             tz="America/Nuuk"), 
                  -180, 180, 0.1)

b = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_ribbon(data = time, aes(lat, ymax = lon), 
              ymin = -90, alpha = 0.4, fill = "dodgerblue4") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Jul",], aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, size=0.5, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 59, x=-45,
           label = "Mean daylength: 20:30", size=1) +
  annotate("text", y = 58, x=-45,
           label = paste0("Mean sunrise: ",substr(mean(sun$sunrise),12,16)), size=1) +
  annotate("text", y = 57, x=-45,
           label = paste0("Mean sunset: ",substr(mean(sun$sunset),12,16)), size=1) +
  annotate("text", y = 56, x=-45,
           label = paste0(length(unique(loc$id[loc$month=="Jul"])),
                          " individuals"), size=1) +
  annotate("text", y = 55, x=-45, size=1,
           label = paste0(nrow(loc[loc$month=="Jul",])," locations")) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "", x = "Longitude (°W)", title = "b) July") +
  theme(axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=6,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.1,0.2,0.1), "cm")) + # t r b l: top,right,bottom,left
  guides(color = guide_legend(override.aes = list(size=0.3)))








##################################################
# plot panel c: locs in Dec with day/night areas
##################################################
dates = seq.Date(from=as.Date("2014-12-01"),
                 to=as.Date("2014-12-31"),
                 by="1 day")
sun = getSunlightTimes(date=dates, 
                       lon=-52.54, lat=65.25,
                       keep=c("sunrise","sunset"),
                       tz="America/Nuuk") %>% as_tibble()
sun %>% summarise(min_sunrise = min(sunrise),
                  mean_sunrise = mean(sunrise),
                  max_sunrise = max(sunrise),
                  min_sunset  = max(sunset),
                  mean_sunset = mean(sunset),
                  max_sunset  = min(sunset))
# min_sunrise        mean_sunrise         max_sunrise          min_sunset         mean_sunset
# 2014-12-01 10:05:28 2014-12-16 10:35:57 2014-12-31 10:44:19 2014-12-31 14:24:16 2014-12-16 14:18:34
# max_sunset
# 2014-12-01 14:35:56         

time = terminator(as.POSIXct(mean(sun$sunset),
                             tz="America/Nuuk"), 
                  -180, 180, 0.1)

c = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_ribbon(data = time, aes(lat, ymax = lon), 
              ymin = 90, alpha = 0.4, fill = "dodgerblue4") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Dec",], aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), stroke=0, size=0.5, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = "Greenland", size=2, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=1, colour="red") +
  annotate("text", y = 59, x=-45,
           label = "Mean daylength: 3h24", size=1) +
  annotate("text", y = 58, x=-45,
           label = paste0("Mean sunrise: ",substr(mean(sun$sunrise),12,16)), size=1) +
  annotate("text", y = 57, x=-45,
           label = paste0("Mean sunset: ",substr(mean(sun$sunset),12,16)), size=1) +
  annotate("text", y = 56, x=-45,
           label = paste0(length(unique(loc$id[loc$month=="Dec"])),
                          " individuals"), size=1) +
  annotate("text", y = 55, x=-45, size=1,
           label = paste0(nrow(loc[loc$month=="Dec",])," locations")) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "", x = "Longitude (°W)", title = "c) December") +
  theme(axis.text.x = element_text(size=6, hjust=1, vjust = 1),
        axis.text.y = element_blank(),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=6,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=7,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.1,0.2,0.1), "cm"))  # t r b l: top,right,bottom,left





###############################################################
# identify day and night for a specific date in December 2014
#------------------------------------------------------------
sun = getSunlightTimes(data = bathy_df,
                       keep=c("sunrise","sunset","dusk","dawn",
                              "sunriseEnd","sunsetStart","night","nightEnd"),
                       tz="America/Nuuk") %>%
  mutate(date_dec = as.POSIXct("2014-07-15 03:30:00",
                               tz="America/Nuuk"))
head(sun)

sun = as_tibble(sun)
sun = sun %>%
  mutate(period = case_when(date_dec %within% interval(sunrise, sunset) ~ 'day',
                            date_dec %within% interval(sunset, night) ~ 'dusk',
                            date_dec %within% interval(nightEnd, sunrise) ~ 'dawn',
                            TRUE ~ "night"))
summary(sun)
table(sun$period)
sun %>% filter(!is.na(dawn) & lat>56) %>% View()

ggplot(shore, aes(long, lat)) +
  geom_raster(data=sun, aes(lon,lat,fill=period), alpha=0.6) +
  # scale_fill_manual(values = c("lightsalmon","navajowhite1",
  #                              "#337882","dodgerblue3")) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1)


# sun = sun %>%
#   filter(!is.na(sunrise), !is.na(sunset)) %>%
#   filter(is.na(night), is.na(nightEnd)) %>%
#   mutate(period = case_when(date_dec %within% interval(sunrise, sunset) ~ 'day',
#                             date_dec %within% interval(sunset, night) ~ 'dusk',
#                             date_dec %within% interval(nightEnd, sunrise) ~ 'dawn',
#                             TRUE ~ "night"))


sun %>% 
  filter(period == "night") %>%
  group_by(lon) %>%
  summarise(maxlat = max(lat))




ggplot(shore, aes(long, lat)) +
  geom_raster(data=plot, aes(lon,lat,fill=period), alpha=0.6) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1)

coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_tile(data=plot, aes(lon,lat,fill=period), alpha=0.3) +
  # geom_contour_filled(data=plot, 
  #                     aes(lon,lat,z=period)) +
  # scale_fill_brewer(palette="BuPu") + 
  # geom_ribbon(data = sun[sun$period=="night",], aes(lat, ymax = lon), 
  #             ymin = -90, alpha = 0.4, fill = "dodgerblue4") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Dec",], aes(lon,lat), stroke=0, size=0.3) +
  theme_tq()












####################################################################################
# add terminator at a specific time to map of Greenland
# using a `coord_*` function to crop after drawing shaded region with `geom_ribbon`
####################################################################################
library(suncal)
#https://www.timeanddate.com/sun/greenland/maniitsoq

#---------------
# plot in July
#---------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(animation)
source("terminator.R")

dates = seq.Date(from=as.Date("2014-07-01"),
                 to=as.Date("2014-07-31"),
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
# min_sunrise         mean_sunrise        max_sunrise         min_sunset          mean_sunset         max_sunset         
# 2014-07-01 02:37:41 2014-07-16 03:28:52 2014-07-31 04:22:54 2014-07-31 22:52:36 2014-07-16 23:45:06 2014-07-02 00:32:38       

time = terminator(as.POSIXct(min(sun$sunset),
                             tz="America/Nuuk"), 
                  -180, 180, 0.1)

b = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_ribbon(data = time, aes(lat, ymax = lon), 
              ymin = 90, alpha = 0.4, fill = "dodgerblue4") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Jul",], aes(lon,lat), size=0.1) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), size=0.4, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = "Greenland", size=3, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=2, colour="red") +
  annotate("text", y = 58, x=-45,
           label = paste0("Mean sunrise: ",mean(sun$sunrise)), size=2) +
  annotate("text", y = 57, x=-45,
           label = paste0("Mean sunset: ",mean(sun$sunset)), size=2) +
  annotate("text", y = 56, x=-45,
           label = paste0(length(unique(loc$id[loc$month=="Jul"])),
                          " individuals"), size=2) +
  annotate("text", y = 55, x=-45,
           label = paste0(nrow(loc[loc$month=="Jul",])," locations"), size=2) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "Latitude (°N)", x = "Longitude (°W)", title = "b) July") +
  theme(axis.text.x = element_text(size=7, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size=7, hjust=1, vjust = 1),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=8,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=10,face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.2,0.2,0.1), "cm")) # t r b l: top,right,bottom,left





#---------------
# plot in December
#---------------
dates = seq.Date(from=as.Date("2014-12-01"),
                to=as.Date("2014-12-31"),
                by="1 day")
sun = getSunlightTimes(date=dates, 
                 lon=-52.54, lat=65.25,
                 keep=c("sunrise","sunset"),
                 tz="America/Nuuk") %>% as_tibble()
sun %>% summarise(min_sunrise = min(sunrise),
                  mean_sunrise = mean(sunrise),
                  max_sunrise = max(sunrise),
                  min_sunset  = max(sunset),
                  mean_sunset = mean(sunset),
                  max_sunset  = min(sunset))
# min_sunrise        mean_sunrise         max_sunrise          min_sunset         mean_sunset
# 2014-12-01 10:05:28 2014-12-16 10:35:57 2014-12-31 10:44:19 2014-12-31 14:24:16 2014-12-16 14:18:34
# max_sunset
# 2014-12-01 14:35:56         

time = terminator(as.POSIXct(min(sun$sunset),
                             tz="America/Nuuk"), 
                  -180, 180, 0.1)

c = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_ribbon(data = time, aes(lat, ymax = lon), 
              ymin = 90, alpha = 0.4, fill = "dodgerblue4") +
  scale_x_continuous(breaks=c(-60,-50,-40,-30), 
                     labels=c(-60,-50,-40,-30)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_point(data=loc[loc$month=="Dec",], aes(lon,lat), size=0.1) +
  theme_tq() +
  geom_point(aes(x=-52.54, y=65.25), size=0.4, colour="red") +
  annotate("text", y = 75, x=-40, 
           label = "Greenland", size=3, colour="black") +
  annotate("text", y = 65.35, x=-48, 
           label = "Maniitsoq", size=2, colour="red") +
  annotate("text", y = 58, x=-45,
           label = paste0("Mean sunrise: ",mean(sun$sunrise)), size=2) +
  annotate("text", y = 57, x=-45,
           label = paste0("Mean sunset: ",mean(sun$sunset)), size=2) +
  annotate("text", y = 56, x=-45,
           label = paste0(length(unique(loc$id[loc$month=="Dec"])),
                          " individuals"), size=2) +
  annotate("text", y = 55, x=-45,
           label = paste0(nrow(loc[loc$month=="Dec",])," locations"), size=2) +
  geom_hline(yintercept = 66, colour = "blue", 
             linewidth = 0.3, linetype=2) +
  labs(y = "Latitude (°N)", x = "Longitude (°W)", title = "c) December") +
  theme(axis.text.x = element_text(size=7, vjust = 0.5, hjust=0.5),
        axis.text.y = element_text(size=7, hjust=1, vjust = 1),
        axis.ticks  = element_blank(),
        axis.title  = element_text(size=8,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        title = element_text(colour="black",size=10,face="bold"),
        # plot.title=element_text(size=14, vjust=-0.5,hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.1,0.2,0.2,0.1), "cm")) # t r b l: top,right,bottom,left

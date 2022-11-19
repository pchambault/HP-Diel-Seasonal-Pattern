###############################################
######## sea temperature at HP depth ##########
###############################################

library(tidync)
library(raster)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(raster)
library(viridis)
library(maps)
library(fields)
library(gridExtra)
library(png)
library(cowplot)
library(tidyquant)
library(scales)


##################################
# import shorline
##################################
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Russia"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]




#############################################
# import raster of temp at 25 m
#############################################
# r = raster("temp25m_02102013-08012015_PHY-001-030.nc")
# nbands(r)
# plot(r, col=viridis(64), band=5)
# r@z[[1]]

setwd("./ENV.DATA/PHY-001-030")
df_nc <- tidync("temp25m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
df

# select cells around Maniitsoq
loc <- readRDS("./RDATA/1.locations_filtered_5HP.rds")
summary(loc$lon)
summary(loc$lat)
df_select = df %>%
  filter(longitude>(-61) & longitude<(-33)
         & latitude>60 & latitude<69)
gc()


# average daily across whole region
daily = df_select %>%
  group_by(time) %>%
  summarise(mean = mean(thetao),
            sd   = sd(thetao)) %>%
  ungroup() %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
daily
table(daily$year)

# average monthly across whole region
daily$year = as.factor(daily$year)
monthly = daily %>%
  group_by(month, year) %>%
  summarise(mean = mean(mean),
            sd   = mean(sd, na.rm=T)) %>%
  ungroup()
monthly

ggplot(monthly, aes(x=month, y=mean)) +
  geom_path(aes(group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, lwd=0.2) +
  theme_tq() +
  facet_wrap(~year, ncol=1) +
  labs(x = "Month", y = "Temp at 25 m (deg C)") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        panel.spacing = unit(0,'lines'),
        panel.grid.minor = element_blank())





#################################################
# extract temp at 25m at HP's loc
#################################################
hr <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")
str(hr)
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/PHY-001-030")
r = raster("temp25m_02102013-08012015_PHY-001-030.nc") 
hr$temp_25m = NA
hr$date_ch  = as.character(hr$date)

system.time({                 # 198 sec (3 min)
  for (i in 1:nbands(r)) {    # 464 dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/PHY-001-030")
    r0   = raster("temp25m_02102013-08012015_PHY-001-030.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    hr$temp_25m[hr$date_ch==date]=raster::extract(r0, hr[hr$date_ch==date,c("lon","lat")])
  }
})
summary(hr$temp_25m)  # 4 873 002 NA: why that many NAs?


# average daily across whole region
temp25m_daily = hr %>%
  group_by(id, date) %>%
  summarise(mean = mean(temp_25m, na.rm=T),
            sd   = sd(temp_25m, na.rm=T),
            depth = 25,
            lon  = mean(lon),
            lat  = mean(lat)) %>%
  ungroup()
summary(temp25m_daily)

ggplot(temp25m_daily[!is.na(temp25m_daily$mean),], aes(x=date, y=mean)) +
  geom_path(aes(group=1)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, lwd=0.2) +
  geom_point() +
  theme_tq() +
  facet_wrap(~id, ncol=1, scales="free_x") +
  labs(x = "Time", y = "Temp at 25 m (deg C)") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        panel.spacing = unit(0,'lines'),
        panel.grid.minor = element_blank())

# identify NA at locations 
#------------------------------
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-40), ylim=c(62,70)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  # geom_path(data=temp25m_daily, aes(x=lon, y=lat,group=id), lwd=0.3) +
  geom_point(data=temp25m_daily, aes(x=lon, y=lat), size=0.2) +
  geom_point(data=temp25m_daily[is.na(temp25m_daily$mean),], 
             aes(x=lon, y=lat), size=0.2, colour="red") +
  # geom_rect(data=rect, colour = "red", fill = NA,
  #           aes(xmin=long[1], xmax=long[2], ymin=lat[1], ymax=lat[2])) +
  labs(title="HP locations", x="",y="",colour="ID") +
  facet_wrap(~id) +
  theme_tq()






#################################################
# extract temp at 55m at HP's loc
#################################################
r = raster("temp55m_02102013-08012015_PHY-001-030.nc") 
hr$temp_55m = NA
system.time({                 # 198 sec (3 min)
  for (i in 1:nbands(r)) {    # 464 dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/PHY-001-030")
    r0   = raster("temp55m_02102013-08012015_PHY-001-030.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    hr$temp_55m[hr$date_ch==date]=raster::extract(r0, hr[hr$date_ch==date,c("lon","lat")])
  }
})
summary(hr$temp_55m)  # 4 873 002 NA

# average daily across whole region
temp55m_daily = hr %>%
  group_by(id, date) %>%
  summarise(mean = mean(temp_55m, na.rm=T),
            sd   = sd(temp_55m, na.rm=T),
            depth = 55,
            lon  = mean(lon),
            lat  = mean(lat)) %>%
  ungroup()
summary(temp55m_daily$mean)

ggplot(temp55m_daily[!is.na(temp55m_daily$mean),], aes(x=date, y=mean)) +
  geom_path(aes(group=1)) +
  geom_point() +
  theme_tq() +
  facet_wrap(~id, ncol=1, scales="free_x") +
  labs(x = "Time", y = "Temp at 55 m (deg C)") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        panel.spacing = unit(0,'lines'),
        panel.grid.minor = element_blank())

# identify NA at locations 
#------------------------------
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-55,-40), ylim=c(62,70)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_point(data=temp55m_daily, aes(x=lon, y=lat), size=0.2) +
  geom_point(data=temp55m_daily[is.na(temp55m_daily$mean),], 
             aes(x=lon, y=lat), size=0.2, colour="red") +
  labs(title="HP locations", x="",y="",colour="ID") +
  facet_wrap(~id) +
  theme_tq()

na = temp55m_daily %>% 
  filter(is.na(mean)) %>%
  View()





#################################################
# extract temp at 110m at HP's loc
#################################################
r = raster("temp110m_02102013-08012015_PHY-001-030.nc") 
hr$temp_110m = NA
system.time({                 # 212 sec (3 min)
  for (i in 1:nbands(r)) {    # 464 dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/PHY-001-030")
    r0   = raster("temp110m_02102013-08012015_PHY-001-030.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    hr$temp_110m[hr$date_ch==date]=raster::extract(r0, hr[hr$date_ch==date,c("lon","lat")])
  }
})
gc()
summary(hr$temp_110m)  # 30 425 746 NA

# average daily across whole region
temp110m_daily = hr %>%
  group_by(id, date) %>%
  summarise(mean = mean(temp_110m, na.rm=T),
            sd   = sd(temp_110m, na.rm=T),
            depth = 110,
            lon  = mean(lon),
            lat  = mean(lat)) %>%
  ungroup()
summary(temp110m_daily$mean)

ggplot(temp110m_daily[!is.na(temp110m_daily$mean),], aes(x=date, y=mean)) +
  geom_path(aes(group=1)) +
  geom_point() +
  theme_tq() +
  facet_wrap(~id, ncol=1, scales="free_x") +
  labs(x = "Time", y = "Temp at 110 m (deg C)") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        panel.spacing = unit(0,'lines'),
        panel.grid.minor = element_blank())

# identify NA at locations 
#------------------------------
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-55,-40), ylim=c(62,70)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_point(data=temp110m_daily, aes(x=lon, y=lat), size=0.2) +
  geom_point(data=temp110m_daily[is.na(temp110m_daily$mean),], 
             aes(x=lon, y=lat), size=0.2, colour="red") +
  labs(title="HP locations", x="",y="",colour="ID") +
  facet_wrap(~id) +
  theme_tq()

temp110m_daily %>% 
  filter(is.na(mean)) %>%
  View()



#################################################
# extract temp at 222m at HP's loc
#################################################
r = raster("temp222m_02102013-08012015_PHY-001-030.nc") 
hr$temp_222m = NA
system.time({                 # 212 sec (3 min)
  for (i in 1:nbands(r)) {    # 464 dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/PHY-001-030")
    r0   = raster("temp222m_02102013-08012015_PHY-001-030.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    hr$temp_222m[hr$date_ch==date]=raster::extract(r0, hr[hr$date_ch==date,c("lon","lat")])
  }
})
gc()
summary(hr$temp_222m)  # 43 688 674 NA

# average daily across whole region
temp222m_daily = hr %>%
  group_by(id, date) %>%
  summarise(mean = mean(temp_222m, na.rm=T),
            sd   = sd(temp_222m, na.rm=T),
            depth = 110,
            lon  = mean(lon),
            lat  = mean(lat)) %>%
  ungroup()
summary(temp222m_daily$mean)

ggplot(temp222m_daily[!is.na(temp222m_daily$mean),], aes(x=date, y=mean)) +
  geom_path(aes(group=1)) +
  geom_point() +
  theme_tq() +
  facet_wrap(~id, ncol=1, scales="free_x") +
  labs(x = "Time", y = "Temp at 222 m (deg C)") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1),
        panel.spacing = unit(0,'lines'),
        panel.grid.minor = element_blank())

# identify NA at locations 
#------------------------------
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-55,-40), ylim=c(62,70)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_point(data=temp222m_daily, aes(x=lon, y=lat), size=0.2) +
  geom_point(data=temp222m_daily[is.na(temp222m_daily$mean),], 
             aes(x=lon, y=lat), size=0.2, colour="red") +
  labs(title="HP locations", x="",y="",colour="ID") +
  facet_wrap(~id) +
  theme_tq()

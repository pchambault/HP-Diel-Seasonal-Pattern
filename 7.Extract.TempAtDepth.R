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
library(data.table)


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





##############################
# import HP locations
##############################
loc <- readRDS("./RDATA/1a.locations_filtered_17HP.rds")




#############################################
# import raster of temp at 25 m
#############################################
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

# select cells matching HP coordinates
summary(loc$lon)
summary(loc$lat)
df_25m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()









#############################################
# import raster of temp at 55 m
#############################################
setwd("./ENV.DATA/PHY-001-030")
df_nc <- tidync("temp55m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells matching HP coordinates
df_55m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()


#############################################
# import raster of temp at 110 m
#############################################
df_nc <- tidync("temp110m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_110m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()



#############################################
# import raster of temp at 155 m
#############################################
df_nc <- tidync("temp155m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_155m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()



#############################################
# import raster of temp at 222 m
#############################################
df_nc <- tidync("temp222m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_222m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()


#############################################
# import raster of temp at 318 m
#############################################
df_nc <- tidync("temp318m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_318m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()




#############################################
# import raster of temp at 380 m
#############################################
df_nc <- tidync("temp380m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_380m = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()




##############################
# aggregate depths
##############################
temp = bind_rows(df_25m, df_55m, df_110m, 
                 df_155m, df_222m, df_318m, df_380m) %>%
  as_tibble() %>%
  mutate(month = format(time, "%b"))
gc()
rm(df_25m, df_55m, df_110m, 
   df_155m, df_222m, df_318m, df_380m)
unique(temp$depth)
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
saveRDS(temp, "./RDATA/7.Env_Data/temp_25-388m_Oct2013-Aug2015.RDS")

daily = temp %>%
  filter(longitude>(-55) & longitude<(-50)
         & latitude>57 & latitude<65) %>%
  group_by(time, depth) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao),
            min_temp  = min(thetao),
            max_temp  = max(thetao)) %>%
  mutate(month = format(time, "%b")) %>%
  ungroup()
daily
saveRDS(daily, "./RDATA/7.Env_Data/temp_daily_25-388m_Oct2013-Aug2015.RDS")
daily$month = factor(daily$month, levels = month.abb)

ggplot(data=daily, aes(x=month, y=mean_temp)) +
  geom_boxplot() +
  facet_wrap(~as.factor(depth))

monthly = temp %>%
  filter(longitude>(-55) & longitude<(-50)
         & latitude>57 & latitude<65) %>%
  group_by(month, depth) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao, na.rm=T),
            min_temp  = min(thetao),
            max_temp  = max(thetao)) %>%
  ungroup()
monthly
monthly$month = factor(monthly$month, levels = month.abb)
saveRDS(monthly, "./RDATA/7.Env_Data/temp_monthly_25-388m_Oct2013-Aug2015.RDS")

ggplot(data=monthly, aes(x=mean_temp, y=-depth)) +
  geom_point() +
  geom_path() +
  facet_wrap(~month) + 
  theme_tq()

















#############################################
## Temp from 0 to 380 m 
#############################################
df_nc <- tidync("./ENV.DATA/PHY-001-030/temp0-380m_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 
df

# select cells matching HP coordinates
df_temp = df %>%
  mutate(month = format(time, "%b")) %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()
saveRDS(df_temp, "./RDATA/7.Env_Data/temp_0-380m_Jul2014-Aug2015.RDS")

# daily summary per depth 
#---------------------------
daily = df_temp %>%
  filter(longitude>(-55) & longitude<(-50)
         & latitude>57 & latitude<65) %>%
  group_by(time, depth) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao),
            min_temp  = min(thetao),
            max_temp  = max(thetao)) %>%
  mutate(month = format(time, "%b")) %>%
  ungroup()
daily
saveRDS(daily, "./RDATA/7.Env_Data/temp_daily_0-380m_Jul2014-Aug2015.RDS")
daily$month = factor(daily$month, levels = month.abb)

# monthly summary per depth 
#---------------------------
monthly = df_temp %>%
  filter(longitude>(-55) & longitude<(-50)
         & latitude>57 & latitude<65) %>%
  group_by(month, depth) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao, na.rm=T),
            min_temp  = min(thetao),
            max_temp  = max(thetao)) %>%
  ungroup()
monthly
monthly$month = factor(monthly$month, levels = c("Jul","Aug","Sep","Oct",
                                                 "Nov","Dec","Jan"))
saveRDS(monthly, "./RDATA/7.Env_Data/temp_monthly_0-380m_Jul2014-Aug2015.RDS")


# add HP mean depth / month
#----------------------------
# load lr dataset
lr <- readRDS("./RDATA/1c.daylength_depth_LR_19ids.RDS") %>%
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
lr$month = as.numeric(substr(lr$date, 6, 7))

# load HR dataset
hr <- readRDS("./RDATA/1b.daylength_depth_HR_5ids.RDS") %>%
  select(-c(sunrise, sunset)) %>%
  mutate(month = as.numeric(substr(date, 6, 7))) %>%
  filter(month != "Jan")

dat = rbind(lr, hr)
dat = dat[!is.na(dat$daylength),] # 1479 obs
dat = dat %>% mutate(month = format(date, "%b"))
mean = dat %>%
  group_by(month) %>%
  summarise(mean_dep   = mean(mean_dep),
            median_dep = median(mean_dep),
            max_dep    = mean(max_dep))
mean$month = factor(mean$month, levels = c("Jul","Aug","Sep","Oct",
                                           "Nov","Dec","Jan"))

# plot monthly temperate vs depth per month
# add mean HP depth per month
#---------------------------------------------
monthly = monthly %>% filter(month != "Jan") 
ggplot(data=monthly, aes(x=mean_temp, y=-depth)) +
  geom_point() +
  geom_path() +
  # geom_hline(data = mean, aes(yintercept=-mean_dep),
  #            lwd=0.2, linetype = "dashed", colour="blue") +
  facet_wrap(~month) + 
  labs(x="Temperature (deg C)",y="Depth (m)",
       title = "Monthly mean in 2014- 2015") +
  theme_tq()

rm(df_nc)





















#############################################
### MLD: import raster of MLD
#############################################
df_nc <- tidync("./ENV.DATA/PHY-001-030/mld_02102013-08012015_PHY-001-030.nc") 

# flatten the multidimensional array
df = df_nc %>% hyper_tibble() %>% setDT()

# convert time: hours since 1950-01-01 00:00:00
df = df[, time := as.Date(time/(24), origin=as.Date("1950-01-01"))]
df = df %>%
  mutate(month = substr(time, 6, 7),
         year  = lubridate::year(time)) 

# select cells mlatching HP coordinates
df_mld = df %>%
  filter(longitude>(-61) & longitude<(-44)
         & latitude>56 & latitude<70)
gc()
df_mld

monthly_mld = df_mld %>%
  filter(longitude>(-55) & longitude<(-50)
         & latitude>57 & latitude<65) %>%
  mutate(month = format(time, "%b")) %>%
  group_by(month) %>%
  summarise(mean_mld = mean(mlotst),
            sd_mld   = sd(mlotst, na.rm=T),
            min_mld  = min(mlotst),
            max_mld  = max(mlotst)) %>%
  ungroup()
monthly_mld
monthly_mld$month = factor(monthly_mld$month, levels = month.abb)

ggplot(data=monthly_mld, aes(y=-mean_mld, x=month)) +
  geom_point() +
  geom_line(aes(group=1)) +
  geom_line(data=monthly_mld, aes(y=-max_mld, x=month, group=1), colour="red") +
  # geom_line(data=monthly_mld, aes(y=-min_mld, x=month, group=1), colour="blue") +
  theme_tq()


ggplot(data=monthly, aes(x=mean_temp, y=-depth)) +
  geom_point() +
  geom_path() +
  geom_hline(data = monthly_mld, aes(yintercept=-mean_mld),
             lwd=0.2, linetype = "dashed", colour="blue") +
  facet_wrap(~month) + 
  theme_tq()

monthly %>%
  group_by(month) %>%
  summarize(depth_maxtemp = depth[max(mean_temp)])
















#################################################
# extract temp at 25m at HP's locations
#################################################
# hr <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")
# str(hr)
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
loc <- readRDS("./RDATA/1a.locations_filtered_17HP.rds")

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/ENV.DATA/PHY-001-030")
r = raster("temp25m_02102013-08012015_PHY-001-030.nc") 
hr$temp_25m = NA
hr$date_ch  = as.character(hr$date)

system.time({                 # 198 sec (3 min)
  for (i in 1:nbands(r)) {    # 464 dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/ENV.DATA/PHY-001-030")
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
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/ENV.DATA/PHY-001-030")
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
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/ENV.DATA/PHY-001-030")
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
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/ENV.DATA/PHY-001-030")
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

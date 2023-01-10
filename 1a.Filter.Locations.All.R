####################################################################
##########   FILTER LOCATION DATA FROM THE 5 HP   ##################
##########  TAGS RETRIEVED: HIGH-RESOLUTION DATA  ##################
####################################################################



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
library(suncalc)





###############################
# import dataset
###############################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
loc <- read_excel("./DATA/HP_Satellite_data/HP_satellite-tag_data.xlsx", 
                  col_types = c("text", "text", "text","text",
                                "text", "text", "numeric", "numeric")) %>%
  as_tibble() %>%
  select(-c(DeployID)) %>%
  rename(id  = Ptt, instr = Instr, dateTime = Date, 
         lc = Quality, lon = Longitude, lat = Latitude) 
loc
table(loc$id)

## PTT correspondence:
#----------------------
# SN 13A0471, PTT 27262, nloc: 251  (2013)
# SN 14A0375, PTT 22849b,nloc: 1714 (2014)
# SN 14A0376, PTT 22850b,nloc: 1732 (2014)
# SN 14A0380, PTT 27262b,nloc: 1576 (2014)
# SN 14A0384, PTT 93100, nloc: 790  (2014)

# select the 17 ids of interest
#-------------------------------
loc = loc %>% 
  filter(id == "27262" | id == "22849b" | id == "22850b"
         | id == "93100" | id == "27262b" 
         | id == "22853" | id == "22849" | id == "37235"
         | id == "27261" | id == "37227" | id == "37228"
         | id == "22850" | id == "27261b" | id == "37235b"
         | id == "93096" | id == "93102" | id == "22854") %>%
  mutate(month = substr(dateTime, 1, 2))
unique(loc$month)       # issue with dates !!

loc %>%
  group_by(id) %>%
  summarise(start       = first(dateTime),
            end         = last(dateTime)) 
#   id     year_deploy start               end                
# 1 22849b 2014        07/22/2014 15:16:19 12/15/2014 13:43:53
# 2 22850b 2014        07/17/2014 19:12:20 41651.564884259256 
# 3 27262  6619        41315.661956018521  10/18/2013 13:42:40   !!
# 4 27262b 2014        07/22/2014 15:26:13 11/18/2015 16:07:36
# 5 93100  2014        07/22/2014 14:05:17 12/25/2014 21:56:44


#----------------------
# correct wrong dates
#----------------------
unique(loc$month)
wrong_dates = loc %>%
  filter(month == "41" | month == "42")
unique(wrong_dates$id) 

# convert date and times from numeric
wrong_dates = wrong_dates %>%
  mutate(date  = as.numeric(dateTime),
         date  = excel_numeric_to_date(date))
wrong_dates
unique(wrong_dates$date)

# convert date and times from numeric to POSIX
wrong_dates$posix = convert_to_datetime(
  wrong_dates$dateTime,
  character_fun=lubridate::ymd_hms, truncated=1, tz="America/Nuuk")
wrong_dates
unique(wrong_dates$posix)

wrong_dates %>%
  group_by(id) %>%
  summarise(start = first(posix),  # ymd
            end   = last(posix))   # ymd

# convert dates into POSIX
wrong_dates$posix = as.POSIXct(strptime(wrong_dates$posix, # ymd
                                 format="%Y-%d-%m %H:%M:%S"), tz="America/Nuuk")

# convert date and times from initial dataset without wrong dates
loc2 = loc %>%
  filter(!(month == "41" | month == "42")) %>%
  mutate(date = substr(dateTime, 1, 10))
unique(loc2$date)

# convert dates from numeric to dates and times (ddtm)
loc2$posix = convert_to_datetime(
  loc2$dateTime,
  character_fun = lubridate::mdy_hms, truncated=1, tz="America/Nuuk") # ymd
summary(loc2$posix)

loc3 = rbind(loc2, wrong_dates)
loc3 = loc3 %>%
  mutate(month       = substr(posix, 6, 7),
         deploy_year = substr(posix, 1, 4)) %>%
  group_by(id) %>%
  arrange(posix) %>%
  ungroup()
unique(loc3$month)
unique(loc3$deploy_year) # 2013 to 2015

# summary table for each id
loc3 %>%
  group_by(id) %>%
  summarise(start  = first(posix),
            end    = last(posix),
            deploy_year = unique(deploy_year)) %>%
  mutate(duration = difftime(end, start, units = "days"))

loc <- loc3
rm(loc3, loc2, wrong_dates)

  
  


#-----------------------------
# remove coordinates outliers
#-----------------------------
summary(loc$lon) # -149 + 11.4
summary(loc$lat) # 47 to 69

# plot locations per ID
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.2) +
  # coord_map("azequidistant", xlim=c(-65,-40), ylim=c(55,75)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  facet_wrap(~id, ncol=6) +
  theme(legend.position = "none") 

loc = loc %>%
  filter(lon >(-60) & lon < (-40))
summary(loc$lon)
summary(loc$lat)
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  theme(legend.position = "none") 

ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=deploy_year), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  theme_tq() +
  facet_wrap(~deploy_year, ncol=3) +
  theme(legend.position = "none") 

rm(loc2) 



#-------------------
# add column hour
#-------------------
loc = loc %>%
  rename(posix_local = posix) %>%
  mutate(hour = as.numeric(substr(posix_local, 12, 13))) %>%
  ungroup()





#---------------------------------------------
# check seasonal variation
#---------------------------------------------
loc %>%
  group_by(id) %>%
  summarise(year_deploy = first(deploy_year),
            start       = first(posix),
            end         = last(posix)) %>%
  mutate(duration = difftime(end, start, units="days"))

  
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(month)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,0), ylim=c(55,78)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(.~month) + 
  theme_tq() +
  theme(legend.position = "none")

# nlocs/day/id
#---------------
loc = loc %>%
  group_by(id) %>%
  mutate(date = as.Date(posix),
         day  = as.numeric(date - first(date)) + 1) 

ndaily_loc = loc %>%
  group_by(id, date) %>%
  summarise(ndaily_loc = n()) %>%
  ungroup()
ndaily_loc

ndaily_loc %>%
  group_by(id) %>%
  summarise(mean = mean(ndaily_loc),
            sd   = sd(ndaily_loc),
            min  = min(ndaily_loc),
            max  = max(ndaily_loc)) %>%
  ungroup()
#    id      mean    sd   min   max
# 1 22849   15.8  3.82     9    21
# 2 22849b  22.9  6.55     8    34
# 3 22850   32.3  8.92    10    49
# 4 22850b  24.7  5.47    11    40
# 5 22853   19.2  5.11    11    29
# 6 22854   23.6  6.19     7    38
# 7 27261   26.4  9.10     1    41
# 8 27261b  21.9  6.02     3    33
# 9 27262   25.1  8.21    11    35
# 10 27262b  21.3  6.33     8    38
# 11 37227   15.2  4.39     4    25
# 12 37228   27.7  7.22     8    39
# 13 37235   27.3  6.22     9    39
# 14 37235b  22.0  6.17     5    35
# 15 93096   22.1  5.70     7    36
# 16 93100   14.4  7.53     1    31
# 17 93102   23.3  5.14     8    36


# nlocs/month/id
#---------------
nmonthly_loc = loc %>%
  group_by(id, month) %>%
  summarise(nmonthly_loc = n()) %>%
  ungroup()
nmonthly_loc
nmonthly_loc %>% filter(id == "27262") # only oct (only 15 locs!)

nmonthly_loc %>%
  group_by(id) %>%
  summarise(mean = mean(nmonthly_loc),
            sd   = sd(nmonthly_loc),
            min  = min(nmonthly_loc),
            max  = max(nmonthly_loc)) %>%
  ungroup()










#############################################
# identify day, dusk, dawn and night phases
#############################################
str(loc)
loc %>% group_by(deploy_year) %>% summarise(n = n_distinct(id))

#----------------------------------------------------
# extract dusk, dawn, day, night for each coordinate
#----------------------------------------------------
sun  = getSunlightTimes(data = loc, tz="America/Nuuk", 
                        keep = c("sunrise","sunset","dusk",
                                 "dawn","night","nightEnd")) 
head(sun)
sun %>%
  mutate(year = substr(date, 1, 4)) %>%
  filter(year == "2014") %>%  # focus on year 2014 to get an idea of sunrise and sunset extremes
  summarise(earliest_sunrise = min(sunrise),
            latest_sunrise   = max(sunrise),
            earliest_sunset  = max(sunset),
            latest_sunset    = min(sunset))

# add sun columns to loc dataset
#---------------------------------
loc2 = cbind(loc, sun[,c("sunrise","sunset","dusk",
                         "dawn","night","nightEnd")]) %>%
  as_tibble()
names(loc2)

# identify periods based on dusk, dawn, day and night hours
#------------------------------------------------------------
loc2 = loc2 %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',     # after sunset, before night
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn', # before sunrise
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night"))
table(loc2$period)
loc2 %>% dplyr::select(c(posix_local, sunrise, sunset, hour, period))
unique(loc2$period)
loc2 -> loc
rm(loc2)







############################
# remove duplicated rows
############################
nrow(loc[duplicated(loc$posix_local),])    # 5327
loc %>%
  group_by(id) %>%
  filter(duplicated(posix_local)) %>%
  summarise(n_duplicated = n()) %>%
  ungroup()

loc2 = loc %>%
  group_by(id) %>%
  filter(!(duplicated(posix_local))) %>%
  ungroup()
nrow(loc2[duplicated(loc2$posix_local),])  # 136
loc2 %>%
  group_by(id) %>%
  filter(duplicated(posix_local))          # 0 left
loc2 -> loc
rm(loc2)






######################################
# extract bathy at locations
######################################
bathy = raster("./ENV.DATA/GEBCO_Arctic_West.nc")
# plot(bathy, col=viridis(64))
bathy[bathy>0] = NA  # remove cells on land (above sea level)
system.time({ 
  loc$bathy = raster::extract(bathy, loc[,c("lon","lat")])
})
summary(loc$bathy) # mean: 420 m, NA: 190
loc$bathy = abs(loc$bathy)





#####################
# save dataset
#####################
loc = loc %>% ungroup()
saveRDS(loc, "./RDATA/1a.locations_filtered_17HP.rds")









# ######################################
# # extract env data at locations
# ######################################
# 
# # MLD #
# #------
# r = raster("./ENV.DATA/CMEMS/mld_02102013-08012015_PHY-001-030.nc")
# plot(r, col=viridis(64))
# loc$mld = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$mld) # 66 NA
# 
# ggplot(data = loc, aes(x=month, y=mld, fill=mld)) +
#   geom_boxplot() +
#   facet_grid(~id)
# 
# daily = loc %>%
#   group_by(id, date) %>%
#   summarise(mld = mean(mld, na.rm=T))
# 
# ggplot(data = daily, aes(x=date, y=mld, fill=mld)) +
#   geom_bar(stat="identity") +
#   scale_y_continuous(trans = "reverse") +
#   scico::scale_fill_scico(palette = "berlin", direction=-1) +
#   labs(x = "", y = "MLD at location (m)", 
#        title="Mean daily MLD") +
#   facet_wrap(~id, scales="free_x") +
#   theme_tq() 
# 
# 
# 
# # sea ice concentration #
# #------------------------
# library(oce)
# library(maps)
# r = raster("./ENV.DATA/CMEMS/sic_02102013-08012015_PHY-001-030.nc")
# nbands(r)
# plot(r, col=oceColorsGebco(64))
# map(add=T)
# loc$sic = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$sic) # no sic !
# 
# 
# # sst #
# #-------
# r = raster("./ENV.DATA/CMEMS/sst_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsFreesurface(64))
# map(add=T)
# loc$sst = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$sst) # 66 NA
# points(lat~lon, loc[is.na(loc$sst),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=sst, fill=sst)) +
#   geom_boxplot(aes(colour=sst)) +
#   facet_grid(~id)
# 
# # U #
# #-------
# r = raster("./ENV.DATA/CMEMS/u_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsVelocity(64))
# map(add=T)
# loc$u = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$u) # 66 NA
# points(lat~lon, loc[is.na(loc$u),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=u)) + # mainly currents flowing westward (offshore)
#   geom_boxplot() +
#   facet_grid(~id)
# 
# 
# # V #
# #-------
# r = raster("./ENV.DATA/CMEMS/v_02102013-08012015_PHY-001-030.nc")
# plot(r, col=oceColorsVelocity(64))
# map(add=T)
# loc$v = raster::extract(r, loc[,c("lon","lat")])
# summary(loc$v) # 66 NA
# points(lat~lon, loc[is.na(loc$v),], pch=19, cex=0.4, col="red")
# ggplot(data = loc, aes(x=month, y=v)) + 
#   geom_boxplot() +
#   facet_grid(~id)














##################################
# exploration
##################################

#------------------------------
# bathy over time per whale
#------------------------------
daily = loc %>%
  group_by(id, date) %>%
  summarise(bathy = mean(bathy, na.rm=T))

ggplot(data = daily[!is.na(daily$bathy),], 
       aes(x=date, y=bathy, fill=bathy)) +
  # geom_line(colour="red") +
  geom_bar(stat="identity") +
  scale_y_continuous(trans = "reverse") +
  scico::scale_fill_scico(palette = "berlin", direction=-1) +
  labs(x = "", y = "Bathymetry at location (m)", 
       title="Mean daily bathy") +
  facet_wrap(~id, scales="free_x") +
  theme_tq() +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=10,vjust=0.5,color="black"),
        axis.text.y  = element_text(size=10,color="black"),
        strip.text = element_text(colour='white',size=12,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        panel.background = element_blank(),
        text=element_text(size=10, family="serif"),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        axis.line.x.top = element_line(size = 3, color = "red"))


#-------------------------------
# tracks with bathy colorcoded
#-------------------------------
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Iceland",]

ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=bathy), size=0.1) +
  # scale_colour_brewer(palette = "PiYG") +
  # scale_colour_continuous(type = "viridis") +
  scico::scale_colour_scico(palette = "berlin", direction=-1) + #lajolla
  coord_map("azequidistant", xlim=c(-57,-40), ylim=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~id, ncol=3) + 
  labs(x="",y="") +
  theme_tq() +
  theme(legend.position = "bottom")


# plot tracking duration / ID
#------------------------------
loc %>%
  group_by(id) %>%
  summarise(start    = first(posix),
            end      = last(posix),
            dur_days = difftime(last(posix), first(posix), units="days"),
            dur_months = floor(as.numeric(difftime(last(posix), 
                                                   first(posix), 
                                                   units="days")/30))) %>%
  print(n=5)

a = ggplot(loc, aes(y=day, x=id, colour=id)) +
  geom_line(size=2) + 
  scale_y_continuous(name="Days", breaks=seq(0,max(loc$day),by=10)) +
  scale_color_tq() +
  labs(x="", y="Tracking duration (days)") +
  coord_flip() +
  theme_tq() +
  theme(legend.position="none")

b = ggplot(loc, aes(y=date, x=id, colour=id)) +
  geom_line(size=2) + 
  scale_color_tq() +
  labs(x="", y="Year of deployment") +
  coord_flip() +
  theme_tq() +
  theme(legend.position="none")

ggsave(paste0("./FIGURES/Tracks/TrackDuration_satTag_5HP.png"),
       grid.arrange(a, b, ncol=1),
       width=5,height=6,units="in",dpi=400)


# plot locations per individual
#--------------------------------
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,-20), ylim=c(60,72)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~id, ncol=2) + 
  theme_tq() +
  theme(legend.position = "none")
ggsave(paste0("./FIGURES/Tracks/Locations_indiv_5HP.png"),
       width=4.5,height=6,units="in",dpi=400)

# plot locations per month
#--------------------------------
ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=as.factor(id)), size=0.1) +
  coord_map("azequidistant", xlim=c(-60,-20), ylim=c(60,72)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_wrap(~month, ncol=2) + 
  theme_tq() 
ggsave(paste0("./FIGURES/Tracks/Locations_mois_5HP.png"),
       width=4.5,height=6,units="in",dpi=400)

# latitude according to months
#---------------------------------
ggplot(loc, aes(y=lat, x=month, colour=id)) +
  geom_boxplot() +
  theme_tq()
ggsave(paste0("./FIGURES/Tracks/Locations_mois_indiv.png"),
       width=5,height=4,units="in",dpi=400)

ggplot(loc, aes(y=lat, x=month)) +
  geom_boxplot() +
  theme_tq()
ggsave(paste0("./FIGURES/Tracks/Locations_mois.png"),
       width=5,height=4,units="in",dpi=400)






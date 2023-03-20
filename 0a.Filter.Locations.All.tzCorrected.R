####################################################################
##########   FILTER LOCATION DATA FROM THE 17 HP   #################
##########             LOW RESOLUTION FILES        #################
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

# concatenate all individuals
# reshape data
# remove duplicates
# check coordinate outliers
# remove locs on land





###############################
# import dataset
###############################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
loc <- read_excel("./DATA/HP_Satellite_data/HP_satellite-tag_data.xlsx", 
                  col_types = c("text", "text", "text","text",
                                "text", "text", "numeric", "numeric")) %>%
  as_tibble() %>%
  dplyr::select(-c(DeployID)) %>%
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
  character_fun=lubridate::ymd_hms, truncated=1, tz="UTC")
wrong_dates
unique(wrong_dates$posix)

wrong_dates %>%
  group_by(id) %>%
  summarise(start = first(posix),  # ymd
            end   = last(posix))   # ymd

# convert dates into POSIX
wrong_dates$posix = as.POSIXct(strptime(wrong_dates$posix, # ymd
                                 format="%Y-%d-%m %H:%M:%S"), 
                               tz="UTC")

# convert date and times from initial dataset without wrong dates
loc2 = loc %>%
  filter(!(month == "41" | month == "42")) %>%
  mutate(date = substr(dateTime, 1, 10))
unique(loc2$date)

# convert dates from numeric to dates and times (ddtm)
loc2$posix = convert_to_datetime(
  loc2$dateTime,
  character_fun = lubridate::mdy_hms, truncated=1, tz="UTC") 
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
summary(loc$lon) # -150 + 11.4
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
  geom_polygon(aes(group=group), fill="lightgrey",linewidth=1) +
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
# table(loc$timezone)









############################
# remove duplicated rows
############################
nrow(loc[duplicated(loc$posix),])    # 5327
loc %>%
  group_by(id) %>%
  filter(duplicated(posix)) %>%
  summarise(n_duplicated = n()) %>%
  ungroup()

loc2 = loc %>%
  group_by(id) %>%
  filter(!(duplicated(posix))) %>%
  ungroup()
nrow(loc2[duplicated(loc2$posix),])  # 136
loc2 %>%
  group_by(id) %>%
  filter(duplicated(posix))          # 0 left
loc2 -> loc
rm(loc2)






###############################################
# extract bathy from GEBCO (1 km) at locations
###############################################
bathy = raster("./ENV.DATA/Bathy/GEBCO_Arctic_West.nc")
bathy[bathy>0] = NA  # remove cells on land (above sea level)
system.time({ 
  loc$bathy = raster::extract(bathy, loc[,c("lon","lat")])
})
summary(loc$bathy) # mean: 420 m, NA: 190
loc$bathy = abs(loc$bathy)






#######################################################
# remove locs on land based on Greenland shpaefile
#######################################################
library(rgdal)
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/ClimateChange_PAPER3/ENV.DATA/Bathy_MP/Arc_Greenland/")
green = readOGR("greenland_wvs_polygon.shp", layer="greenland_wvs_polygon")
# plot(green,axes=T)
stere <- "+proj=stere +lat_0=90"
WGS84 = '+proj=longlat +datum=WGS84'

names(loc)
xy = SpatialPoints(loc[, c("lon", "lat")], proj4string=CRS(WGS84))
plot(xy, axes=T, pch=20, cex=0.2)
maps::map(add=T)

# remove locs on land
points_on_land = over(xy,green)                # extract land value
summary(points_on_land)
loc$land = points_on_land$GREENLAND1
unique(loc$land)
points(lat~lon, loc[!is.na(loc$land),],        # identify locs on land
       pch=19, col="green",cex=0.2)
nrow(loc[!is.na(loc$land),]) / nrow(loc) * 100 # 0.74%, 101 locs
loc2 = loc %>% filter(is.na(loc$land))
plot(lat~lon, loc2, pch=19, cex=0.2)
maps::map(add=T)
loc2 -> loc
rm(loc2)





##########################################################
# extract bathy from IBCAO (fine-scale bathy in Arctic)
##########################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
bathy = raster("./ENV.DATA/Bathy/IBCAO_v4_2_200m_ice.nc") # Polar Stereographic projection
bathy
plot(bathy, col=viridis(64), zlim=c(-5000,0))
crs(bathy) <- stere
bathy = crop(bathy, extent(-2910000,0,-2500000,0))
plot(bathy, col=viridis(64), zlim=c(-5000,0))


# convert locs from lon/lat to meters
#-------------------------------------
xy = SpatialPoints(loc[, c("lon", "lat")], proj4string=CRS(WGS84))
xy_m <- spTransform(xy, CRS(stere)) %>% 
  as_tibble() %>% 
  rename(lat_m = lat, lon_m = lon)
points(xy_m$lon_m, xy_m$lat_m, pch=20, cex=0.2)
loc = loc %>% bind_cols(xy_m)
names(loc)

# extract bathy (200 m resolution) from lon_m/lat_m
#---------------------------------------------------
loc$bathy_200 = raster::extract(bathy, loc[,c("lon_m","lat_m")])
summary(loc$bathy_200)  # 101 NA outside bathy range (southwest Greenland)
plot(bathy, col=viridis(64), zlim=c(-5000,0))
points(lat_m~lon_m, loc[is.na(loc$bathy_200),], pch=19, cex=0.2, col="red")

names(loc)
loc = loc %>% 
  dplyr::select(-c(dateTime, land, lon_m, lat_m, Type, instr))
loc = loc %>% ungroup()
length(unique(loc$id))
loc$bathy_200 = abs(loc$bathy_200)





#################################################
# correct time zone for each coordinate
# for location dataset
#################################################
library(lutz)
system.time({  # 1 sec
  loc = loc %>%
    # find the time zone
    mutate(
      timezone = tz_lookup_coords(lat = lat,
                                  lon = lon,
                                  method = "accurate"),
      # transform date using the right time zone
      posix_local = map2(
        .x = posix,
        .y = timezone,
        .f = function(x, y) {
          with_tz(time = x, tzone = y)
        }
      )
    ) %>%
    # required because of the map2
    unnest(posix_local)
})
paste0(loc$posix, "->", loc$posix_local)
table(loc$timezone)








#############################################
# identify day, dusk, dawn and night phases
#############################################
str(loc)
loc %>% group_by(deploy_year) %>% summarise(n = n_distinct(id))
loc = loc %>% mutate(date = as.Date(posix_local))

#----------------------------------------------------
# extract dusk, dawn, day, night for each coordinate
#----------------------------------------------------
sun = NULL
system.time({  # 68 sec
  for (i in 1:nrow(loc)) {
    sun_calc = getSunlightTimes(data = loc[i,c("date","lon","lat","timezone")],
                                tz   = loc$timezone[i],
                                keep = c("sunrise","sunset","dusk",
                                         "dawn","night","nightEnd"))
    sun = rbind(sun, sun_calc)
  }
})
head(sun)

sun %>%
  mutate(year = format(date, "%Y")) %>%
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
loc2 %>% dplyr::select(c(posix_local, sunrise, sunset, period))
loc2 -> loc
rm(loc2)






################################
# save dataset
################################
dim(loc) # 13 521
saveRDS(loc, "./RDATA/0a.locations_filtered_17HP_tzCorrected.rds")




##############################
# maps of locations
##############################
unique(loc$timezone)
unique(loc$month[loc$timezone=="America/Godthab"])
unique(loc$month[loc$timezone=="Etc/GMT+3"])
unique(loc$month[loc$timezone=="Etc/GMT+4"])

ggplot(shore, aes(long, lat)) +
  geom_point(data=loc, aes(lon,lat,colour=timezone), size=0.2) +
  geom_polygon(aes(group=group), fill="lightgrey",linewidth=1) +
  theme_tq() +
  facet_wrap(~timezone, ncol=3) +
  theme(legend.position = "none") 


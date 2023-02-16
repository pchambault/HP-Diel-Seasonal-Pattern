##############################################################
#####   IMPORT and CLEAN HP HR data from 2013-2015   #########
#####                n=5 MK10 tags
##############################################################

library(dplyr)
library(janitor)
library(ggplot2)
library(readr)
library(tidyquant)
library(diveMove)
library(suncalc)
library(gridExtra)
library(magrittr)
library(data.table)





##################################
# import data for the 5 porpoises
##################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern/DATA/HP_Dive_data_2013-2016/")
files = list.files(pattern=".csv")
files # 5 files, different ID names

hp = NULL
system.time({ # 77 sec
  for (i in 1:length(files)) {
    id = read_csv(files[[i]]) %>%
      as_tibble() %>%
      filter(!row_number() %in% c(1:3)) %>%  # remove 3 first rows containing headings
      mutate(id = substr(files[i], 1,7))     # add column id
    colnames(id)[1] = "dateTime"
    colnames(id)[2] = "depth"
    
    id = id %>%
      mutate(depth = as.numeric(depth),
             date  = as.numeric(dateTime),
             date  = excel_numeric_to_date(date))
    
    # convert date and times
    id$posix = convert_to_datetime(
      id$dateTime,
      character_fun=lubridate::ymd_hms, truncated=1, tz="UTC")
    
    hp = rbind(hp, id)
  }
})
hp
unique(hp$id)
str(hp)

# convert dates into POSIX
#--------------------------
gc()
system.time({  # 260 sec: 4 min!
  hp$posix = as.POSIXct(strptime(hp$posix,
                                 format="%Y-%m-%d %H:%M:%S"), tz="America/Nuuk") })
gc()

# summarise data per ID
#------------------------
hp %>%
  group_by(id) %>%
  summarise(start    = first(posix),
            end      = last(posix),
            duration = difftime(last(posix), first(posix), units="days"),
            nobs     = n(),
            ndepths  = sum(is.na(.)),
            meanDep  = mean(depth, na.rm=T),
            maxDep   = max(depth, na.rm=T))
# id      start               end                 duration       meanDep maxDep
# 13A0471 2013-10-02 15:51:00 2013-10-18 12:39:25  15.86696 days    38.2   336.
# 14A0375 2014-07-22 15:15:00 2014-12-15 11:29:37 145.84348 days    27.6 32728.
# 14A0376 2014-07-17 19:11:00 2014-12-03 14:07:21 138.78913 days    30.0 32728.
# 14A0380 2014-07-22 15:24:00 2014-12-07 15:21:21 137.99816 days    25.0 32727 
# 14A0384 2014-07-27 09:15:41 2015-01-08 15:49:49 165.27370 days    29.2   348 

# plot histo of depth per ID
#-----------------------------
ggplot(data = hp, aes(x=-depth)) +
  geom_histogram() +
  # geom_density(aes(y=..scaled..,alpha=.4)) +
  xlim(-400,0) +
  coord_flip() +
  labs(y = "dive frequency", x = "Depth (m)") +
  # scale_y_continuous(scales::percent) +
  facet_wrap(~id, scales="free")



# rename ID column to match PTT from location data
#--------------------------------------------------
# SN 13A0471, PTT 27262, nloc: 251  (2013)
# SN 14A0375, PTT 22849b,nloc: 1714 (2014)
# SN 14A0376, PTT 22850b,nloc: 1732 (2014)
# SN 14A0380, PTT 27262b,nloc: 1576 (2014)
# SN 14A0384, PTT 93100, nloc: 790  (2014)
hp = hp %>%
  mutate(id = case_when(id == "13A0471" ~ "27262",
                        id == "14A0375" ~ "22849b",
                        id == "14A0376" ~ "22850b",
                        id == "14A0380" ~ "27262b",
                        id == "14A0384" ~ "93100"))
unique(hp$id)
hp
hp = hp %>% rename(posix_local = posix) %>% dplyr::select(-c(dateTime))
str(hp)














######################################
# clean dataset: check depth outliers
######################################

# remove depth outliers
#--------------------------
summary(hp$depth)  # starts at -40m and goes up to 32727.5 m !
hp = hp %>% filter(depth < 600)

# summarize dataset to 1 maxDep per day
#--------------------------------------
maxdep_daily = hp %>%
  group_by(id) %>%
  mutate(day = as.numeric(date - first(date)) + 1) %>%
  group_by(id, date) %>%
  summarise(maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T)) %>%
  ungroup()
maxdep_daily 

# need zoc!
ggplot(data = maxdep_daily, aes(x=-mindep)) +
  geom_histogram() +
  labs(y = "dive frequency", x = "min depth (m)") +
  facet_wrap(~id, scales="free")

summary(hp$depth)  # min=-40 --> need ZOC
ggplot(data = maxdep_daily, aes(x=date, y=-maxdep)) +
  geom_bar(stat="identity") +
  labs(x = "Time", y = "Max depth (m)") +
  facet_wrap(~id, scales="free_x") +
  theme_tq()


# need to reset depth above 0 (some values below 0)
#---------------------------------------------------
# remove negative depth (above 3 m)
nrow(hp[hp$depth < (-1.5),]) / nrow(hp )*100  # 0.4%
hp = hp %>%
  filter(depth >= (-1.5)) %>%
  mutate(depth = depth + 1.5) 
summary(hp$depth)

# add column day and month
# day 1 = first day for each id
#--------------------------------
hp = hp %>%
  group_by(id) %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1,
         month = substr(date, 6, 7)) %>%
  ungroup()
unique(hp$month)
range(hp$day_depart)

# order dates by ID
#-----------------------
hp = hp[order(hp$id, hp$posix_local),]

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
saveRDS(hp, "./RDATA/1b.HP_dive_HR_Not_filtered.RDS")
gc()













###################################################################################################
# ZOC and dives identification
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent, 
# (DA) descent/ascent (occurring when no bottom phase can be detected) and (X) non-dive (surface),
###################################################################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
path = paste0("./Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/",
              "ANALYSES/HP-Diel-Seasonal-Pattern/RDATA/1b.HP_dive_HR_Not_filtered.rds")
str(hp)
offset    = 0
threshold = 5  # threshold depth below which an underwater phase should be considered a dive


system.time({  
  # 14700 for 4 ids, threshold=5 m, offset=0
  # 27876 sec for offset(zoc)=0 and threshold=2 and 3 ids
  for (i in 1:length(unique(hp$id))) { 
    id = hp %>% 
      filter(id == unique(id)[i]) %>%
      rename(time = posix_local)
    
    ptt = unique(id$id)
    id  = id %>%
      select(depth, time)
    id = id[order(id$time),]
    id = id[!duplicated(id$time),]
    
    tdr   = createTDR(id$time, id$depth, file=path, speed=F)
    calib = calibrateDepth(tdr, dive.thr=threshold, 
                           zoc.method="offset", offset=offset) 
    
    saveRDS(calib, paste0("./RDATA/calib_",threshold,"m_zoc",offset,"/","calib_",
                          ptt,"_dcalib_",threshold,"m_zoc",offset,".RDS"))
  }
})











#########################################################################
# add dive number and phases to the hp dataset
#########################################################################
# hp <- readRDS("./RDATA/1b.HP_dive_HR_Not_filtered.RDS")
offset    = 0
threshold = 5 # threshold depth below which an underwater phase should be considered a dive
zoc   = paste0("calib_",threshold,"m_zoc",offset)
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern",
             "/RDATA/",zoc))
files = list.files(pattern=paste0(zoc,".RDS"))
files 

dive = NULL
system.time({  # 60 sec
  for (i in 1:length(files)) {
    dat = readRDS(files[i])
    d   = getDAct(dat)                     # extract dive id and activity
    ptt = substr(files[i], 7, 12)          # extract ptt number
    ptt = sub("*_", "", ptt)               # retain all characters located before "_"
    
    # combine dive number to hp dataset
    dive2 = hp %>%
      filter(id == ptt)  %>%               # select the same id from whole dataset
      mutate(dive     = d[,1],             # extract dive number
             activity = d[,2],             # extract dive activity
             phase    = getDPhaseLab(dat)) # extract dive phase
    
    # save the 5 ids into 1 tibble
    dive = rbind(dive, dive2)
  }
})
gc()
unique(dive$id)
rm(dive2, ptt, d, dat, path)

# # check duplicated times
# i = "93100"
# id = dive %>%
#   filter(id == i)
# nrow(id[duplicated(id$posix_local),])










##############################################
# correct for remaining depth above 0
##############################################

#--------------------------------------------
# proportion of dive above depth threshold?
#--------------------------------------------
dive %>%
  group_by(id) %>%
  summarise(ndive  = max(dive),
            above  = length(dive[dive==0])/3600, # in hours
            below  = length(dive[dive>0])/3600,  # in hours
            trackdur = n()/3600) %>%             # in hours
  mutate(abovepc = (above / trackdur) * 100,     # in %
         belowpc = (below / trackdur) * 100)     # in %


#----------------------------------------
# still not proper zoc starting at 0 m !
#----------------------------------------
dat = dive[1:400,]
ggplot(dat, aes(x=posix_local, y=-depth, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)

# remove 1 meter from depth column to set it to the surface
dive2 = dive %>%
  mutate(depth_cor = depth-1) 
ggplot(dive2[1:400,], aes(x=posix_local, y=-depth_cor, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)
summary(dive2$depth_cor)  # -1 to 361 m

# set to zero depth<0
dive2$depth_cor[dive2$depth_cor<0] = 0
ggplot(dive2[1:400,], aes(x=posix_local, y=-depth_cor, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)
summary(dive2$depth_cor) # 0 to 361 m
dive2 -> dive
rm(dive2)
names(dive)

# replace depth by depth_cor
dive = dive %>%
  dplyr::select(-depth) %>%
  rename(depth = depth_cor)
















################################################
# check duplicated rows
# check coordinates: must have 1 per dive
# to avoid having several sunset/sunrise per dive
################################################
# 7 22850b 2014-07-17 19:24:32 2014-07-17 19:25:41
id = dive %>% filter(id == "22850b" & dive == 7) 
# unique(id$sunrise) # 2 different sunrises!
# unique(id$lon)     # 2 coordinates!
# unique(id$lat)
# library(maps)
# plot(lat~lon, id, pch=19, xlim=c(-58,-49), ylim=c(60,70), cex=0.2)
# map(add=T)


# extract first depth of each dive/ID
# to avoid having different coordinates for the same dive
#--------------------------------------------------------
first_dep = dive %>%
  dplyr::select(posix_local, id, dive) %>%
  filter(dive != 0) %>%   # remove surface times
  group_by(id, dive) %>%
  slice_head() %>%
  mutate(first_dep = "yes") %>%
  ungroup() 
first_dep








##########################################################
# Rolling join to extract coordinates from loc dataset
##########################################################

# import locations for the 5 HP
#----------------------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
loc <- readRDS("./RDATA/1a.locations_filtered_17HP.rds") %>% 
  dplyr::select(id, posix_local, bathy, lon, lat, bathy, bathy_200)  %>%
  # filter(unique(id) == unique(dive$id))
  filter(id == "27262" | id == "22849b" | id == "22850b" | 
           id == "27262b" | id == "93100") %>%
  rename(date = posix_local)
summary(loc$bathy_200)
unique(loc$id)
unique(first_dep$id)
names(loc)
names(first_dep)
first_dep = first_dep %>% rename(date = posix_local)


# convert into data tables
#-------------------------
diveDT = setDT(first_dep)
setkey(diveDT,id,date)
locDT  = setDT(loc)
setkey(locDT,id,date)

# rolling join on date
dive2 = locDT[,.(date, id, lon, lat, bathy_200)] %>%
  .[diveDT, roll="nearest"] %>%
  .[] # to display tibble
summary(dive2$lon)
summary(loc$lon)

summary(dive2$lat)
summary(loc$lat)

summary(dive2$bathy_200)
names(first_dep)


# check correspondence
# ok BUT for some dives, several coordinates 
# and therefore sunset/sunrise !!
#--------------------------------------------
dive2 %>% filter(id == "27262") %>% 
  dplyr::select(c(id, date, lon, lat))
id = loc %>% filter(id == "27262") %>% View()

dive2 -> first_dep
names(first_dep)
rm(dive2)
first_dep = as_tibble(first_dep)
names(first_dep)
first_dep = first_dep %>% rename(posix_local = date) 

# join coordinates from first_dep to dive dataset
#-------------------------------------------------
names(dive)
names(first_dep)
dive2 = dive %>%
  left_join(first_dep, by = c("id","posix_local","dive")) %>%
  dplyr::select(-c(lon.y, lat.y, first_dep.y)) %>%
  rename(first_dep = first_dep.x, lon = lon.x, lat = lat.x)
names(dive2)
dive2$first_dep[is.na(dive2$first_dep)] = "no"

 
# assign coordinates an bathy to whole dive for each dive
#--------------------------------------------------------
dive2 = dive2 %>%
  group_by(id, dive) %>%
  mutate(lon = first(lon),
         lat = first(lat),
         bathy_200 = first(bathy_200))
View(dive2)
summary(dive2$bathy_200)  # NA when no dive, mean=253 m
dive2 %>% filter(id == "22849b" & dive == 2) %>% View()
dive2 -> dive
rm(dive2)

dive %>% filter(id == "27262" & dive == 1658) %>% View()


# check coordinates
#-------------------------------
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Canada"
                      | north_map$region=="Iceland",]
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",lwd=0.1,colour="black") +
  geom_point(data=dive[dive$first_dep=="yes",], aes(lon,lat), stroke=0, size=0.3) +
  theme_tq() 

sub = dive %>% filter(dive!=0 & first_dep=="yes")
sub$bathy_class = cut(sub$bathy_200, breaks = c(0,50,150,500,2350))
table(sub$bathy_class)
ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",lwd=0.1,colour="black") +
  geom_point(data=sub, 
             aes(lon,lat, colour=bathy_class), stroke=0, size=0.3) +
  facet_wrap(~bathy_class) +
  theme_tq() 

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",lwd=0.1,colour="black") +
  geom_point(data=dive[dive$first_dep=="yes",], 
             aes(lon,lat,colour=id), stroke=0, size=0.3) +
  facet_wrap(~id, ncol=3) +
  theme_tq() 

















########################################
# identify sunrises and sunsets
########################################
names(dive)

# identify phases of the day based on coordinates and dates
#-----------------------------------------------------------
system.time({ # 104 sec
  sun  = getSunlightTimes(data = dive[,c("date","lon","lat")], 
                          tz   = "America/Nuuk",  
                          keep = c("sunrise","sunset","dusk",
                                   "dawn","night","nightEnd"))  })
head(sun)
gc()
rm(locDT, first_dep, diveDT, dat)

# add columns to hp dataset
#----------------------------
dive2 = cbind(dive, sun[,c("sunrise","sunset","dusk",
                       "dawn","night","nightEnd")]) %>%
  as_tibble()
names(dive2)


# identify dusk, dawn, day, night
#------------------------------------
dive2 = dive2 %>%
  mutate(period = case_when(posix_local %within% interval(sunset, night) ~ 'dusk',
                            posix_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            posix_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night"))
table(dive2$period)
unique(dive2$period)
dive2 -> dive
rm(dive2, sun, files, id, i)
dive = dive %>% mutate(hour = substr(posix_local, 12, 13))
gc()









###################################
# add solar elevation 
###################################
names(dive)

# identify light regimes based on coordinates and dates
#-----------------------------------------------------------
system.time({     # 6 sec
  sun  = getSunlightPosition(data = dive[,c("date","lon","lat")], # include date with times
                             keep = c("altitude"))  }) 
sun = sun %>% mutate(alt_deg = rad2deg(altitude)) %>% as_tibble()
gc()
head(sun)

# combine with dive dataset and convert radians to degrees
#----------------------------------------------------------
dive2 = bind_cols(dive, sun[,c("alt_deg")]) %>%
  mutate(twilight  = case_when(alt_deg > 0 ~ "Day",          
                               alt_deg > (-6) & alt_deg < (0) ~ "Civil twilight",  
                               alt_deg > (-12) & alt_deg < (-6) ~ "Nautical twilight", 
                               alt_deg > (-18) & alt_deg < (-12) ~ "Astronomical twilight",
                               alt_deg < (-18) ~ "Night"))                 
names(dive2)
table(dive2$twilight) / nrow(dive2) * 100
dive2 -> dive
gc()
rm(dive2)










###############################################################################
# summarise each dive / ID: 1 row=1 dive
# add columns asc_rate and desc_rate
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent
###############################################################################
names(dive)
dive = dive %>% rename(date2 = date, posix_local = date)

system.time({  # 50 sec
ndive_summary = dive %>%
  filter(dive != 0) %>%   # remove surface times
  group_by(dive, id) %>% 
  mutate(difftime = c(NA, diff(posix_local)),
         diffdep  = c(NA, abs(diff(depth)))) %>% 
  summarise(start     = first(posix_local),
            end       = last(posix_local),
            nobs      = n(),           # number of observations (depths) / dive
            lon       = unique(lon),
            lat       = unique(lat),
            bathy_200 = unique(bathy_200),
            solar_alt = mean(alt_deg),
            twilight  = first(twilight),
            maxdep    = max(depth),
            mindep    = min(depth),
            meandep   = mean(depth),
            period    = first(period),
            sunrise   = mean(sunrise), # for dives between 2 dates (around midnight)
            sunset    = mean(sunset),  # for dives between 2 dates (around midnight)
            bot_dur   = sum(difftime[phase == "B"], na.rm=T), # in sec
            asc_dur   = sum(difftime[phase == "A"], na.rm=T), # in sec
            des_dur   = sum(difftime[phase == "D"], na.rm=T), # in sec
            asc_rate  = mean(diffdep[phase == "A"]/difftime[phase == "A"], na.rm=T),     # in m/s
            des_rate  = mean(diffdep[phase == "D"]/difftime[phase == "D"], na.rm=T),
            bot_rate  = mean(diffdep[phase == "B"]/difftime[phase == "B"], na.rm=T)) %>% # in m/s 
  mutate(dur = as.numeric(difftime(end, start, units = "secs"))) %>% # in sec
  ungroup()
})
names(ndive_summary) 

# check duplicated dives
#-----------------------------
nrow(ndive_summary %>% distinct(id, start, .keep_all = TRUE)) # 393 483
ndive_summary %>% filter(duplicated(cbind(id, start))) # 0 
str(ndive_summary)
summary(ndive_summary)
hist(ndive_summary$bathy_200, xlab = "Bathy (m)s")

# check outliers (extreme duration)
#------------------------------------
hist((ndive_summary$dur)/60, xlab = "Dive duration (min)")
nrow(ndive_summary[ndive_summary$dur>(7*60),]) / nrow(ndive_summary) * 100 # 0.001%
ndive_summary = ndive_summary %>% filter(dur < (7*60))

# remove dives lasting 0 min
#---------------------------
ndive_summary %>% filter(dur == 0) %>% View()       # 16530 dives!
ndive_summary = ndive_summary %>% filter(!dur == 0) # 376 947 dives left

# remove very short dives < 20 sec
#---------------------------------
summary(ndive_summary$dur) # min: 1 sec!
ndive_summary %>%
  group_by(id) %>%
  summarise(ndives = n_distinct(dive))
ndive_summary %>%
  filter(dur < 20) %>%
  group_by(id) %>%
  summarise(ndives = n_distinct(dive))
ndive_summary = ndive_summary %>%
  filter(dur > 20) # retain dives longer than 20 sec
ndive_summary %>%
  group_by(id) %>%
  summarise(ndives = n_distinct(dive))
hist(ndive_summary$dur/60)

# save dataset
#--------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
saveRDS(ndive_summary, paste0("./RDATA/1b.diveSummary_5HP_calib_",
                              threshold,"m_zoc",offset,".RDS"))







#########################################################
# remove outlier dives from full dataset (1 row=1 depth)
# dives < 20 sec
#########################################################
names(dive)
dive2 = ndive_summary %>%
  select(id, dive) %>%
  left_join(dive, by = c("id","dive"))
dim(dive2) # 25 337 344 depths
setdiff(unique(dive2$dive), unique(ndive_summary$dive))
dive2 %>%
  group_by(id) %>%
  summarise(ndives = n_distinct(dive))
# id     ndives
# 22849b  58968
# 22850b  57428
# 27262    6357
# 27262b  61025
# 93100   66372

dive2 = dive2[order(dive2$id, dive2$posix_local),]
ggplot(dive2[dive2$id=="27262" & dive2$dive==1,],
       aes(x=posix_local, y=-depth)) +
  geom_point()

# remove first dive of each ID (can be incomplete)
dive2 = dive2 %>%
  group_by(id) %>%
  filter(dive != 1) %>%
  ungroup()
dim(dive2) # 25 337 216
dive2 %>%
  group_by(id) %>%
  summarise(ndives = n_distinct(dive))

dive2 -> dive
rm(dive2)

names(dive)
dive = dive %>% 
  select(-c(first_dep, dusk, dawn, night, nightEnd)) %>% 
  rename(date2 = date, date = posix_local)
saveRDS(dive, "./RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")










#############################################
# calculate daily depth (max, mean, median)
#############################################
# Summarize data per id: extract daily max depth
# "dawn" refers to morning, "dusk" refers to the evening twilight
dive = dive %>%
  mutate(month = format(date2, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(month!="Jan") %>% # remove few dives recorded in Jan for only 1 ID
  ungroup()
gc()

# calculate mean, max, median depth/day/ID
#-----------------------------------------
dep_daily = dive %>%
  group_by(id) %>%
  mutate(day = as.numeric(date2 - first(date2)) + 1) %>%
  group_by(id, date2, day_night) %>%
  summarise(maxdep    = max(depth, na.rm=T),
            mindep    = min(depth, na.rm=T),
            meandep   = mean(depth, na.rm=T),
            mediandep = median(depth, na.rm=T)) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(day   = as.numeric(date2 - first(date2)) + 1,
         month = format(date2, "%b")) %>%
  ungroup()
dep_daily

dep_daily$month = factor(dep_daily$month,
                         levels=c("Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))
saveRDS(dep_daily, "./RDATA/1b.dailyDepth_HR.RDS")










###########################################
# calculate daylength per day and ID
###########################################
names(dive)
daily = dive %>%
  filter(day_depart != 1) %>%  # remove first day because never complete (starts in the middle of day)
  group_by(id, date2) %>%
  summarise(maxdep     = max(depth),
            mediandep  = median(depth),
            meandep    = mean(depth),
            mean_bathy = mean(bathy_200),
            min_bathy  = min(bathy_200),
            max_bathy  = max(bathy_200))
daily

# calculate daylength per day and ID
#------------------------------------
daylength = dive %>%
  filter(day_depart != 1) %>%  # remove first day because never complete (starts in the middle of day)
  group_by(id, date2) %>%
  summarise(sunset  = mean(sunset),
            sunrise = mean(sunrise)) %>%
  mutate(daylength  = as.numeric(difftime(sunset, sunrise, units = "hour"))) %>%
  ungroup()
daylength
 
# add daylength to dive dataset
#-------------------------------
daylength2 = daylength %>%
  # rename(date = date2) %>%
  left_join(daily, by = c("id", "date2")) %>%
  rename(max_dep = maxdep, mean_dep = meandep, median_dep = mediandep)
daylength2
names(daylength2)
length(unique(daylength2$id)) # 5 ids
summary(daylength2)

# save data
#--------------
saveRDS(daylength2, "./RDATA/1b.daylength_depth_HR_5ids.RDS")















# ################
# # explo
# ################
# nrow(ndive_summary[ndive_summary$dur<60,]) / nrow(ndive_summary) * 100  # 32%
# ndive_summary %>% 
#   filter(dur>5) %>%
#   dplyr::select(dive, id, dur, maxdep, start, end,
#          bot_dur, asc_rate, des_rate, bot_rate)
# hist(ndive_summary$dur)
# summary(ndive_summary$dur[ndive_summary$maxdep>100]) # 184 sec
# 
# names(ndive_summary)
# nrow(ndive_summary[ndive_summary$maxdep>50,]) / nrow(ndive_summary) * 100  # 40%
# nrow(ndive_summary[ndive_summary$maxdep>100,]) / nrow(ndive_summary) * 100 # 21%
# 
# 
# 
# 
# #--------------------------------------
# # histograms of dive characteristics
# #--------------------------------------
# ggplot(data = ndive_summary, aes(x=dur/60, fill=period)) +
#   geom_density(aes(alpha=.4, colour=period)) +
#   labs(y = "Dive proba", x = "(min)", title = "Dive duration",fill="") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   theme_tq() +
#   facet_grid(period~id, scales="free_y") +
#   theme(legend.position = "none")
# 
# ggplot(data = ndive_summary, aes(x=-maxdep, fill=period)) +
#   # geom_histogram() +
#   geom_density(aes(alpha=.4, colour=period)) +
#   labs(y = "Dive proba", x = "(m)", title = "Max dive depth",fill="") +
#   coord_flip() +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   theme_tq() +
#   facet_grid(period~id, scales="free_y") +
#   theme(legend.position = "none")
# 
# ggplot(data = ndive_summary[!is.na(ndive_summary$des_rate),], 
#        aes(x=des_rate, fill=period)) +
#   geom_density(aes(alpha=.4, colour=period)) +
#   labs(y = "Dive proba", x = "(m/s)", title = "Descent rate",fill="") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   theme_tq() +
#   facet_grid(period~id, scales="free_y") +
#   # xlim(0,2.55) +
#   # ylim(0,5010) +
#   theme(legend.position = "none")
# 
# ggplot(data = ndive_summary[!is.na(ndive_summary$asc_rate),], 
#        aes(x=asc_rate, fill=period)) +
#   geom_density(aes(alpha=.4, colour=period)) +
#   labs(y = "Dive proba", x = "(m/s)", title = "Ascent rate",fill="") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   theme_tq() +
#   facet_grid(period~id, scales="free_y") +
#   theme(legend.position = "none")
# 
# ggplot(data = ndive_summary[!is.na(ndive_summary$bot_dur),], 
#        aes(x=bot_dur/60, fill=period)) +
#   geom_density(aes(alpha=.4, colour=period)) +
#   labs(y = "Dive porba", x = "(min)", title = "Bottom time",fill="") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   theme_tq() +
#   facet_grid(period~id, scales="free_y") +
#   theme(legend.position = "none")
# 
# # grid.arrange(a, b, c, d, e, ncol=2)
# 
# 
# 
# 
# 
# #################################
# # geom_density maxdep/dur
# #################################
# # all dives
# #----------------
# ggplot(ndive_summary, aes(y = -maxdep, x = dur/60)) +
#   geom_density_2d_filled(contour_var = "ndensity",
#                          aes(colour = dive)) +
#   # scale_colour_distiller(palette = "Blues", direction = 1) +
#   facet_wrap(vars(id)) +
#   labs(x = "Dive duration (min)", y = "Dive depth (m)",
#        title = "All dives", fill="n dives \nprobability") +
#   theme_tq() +
#   theme(panel.spacing = unit(0, "lines"),
#         panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
#         strip.background = element_rect(fill="steelblue4",size=0.2,
#                                         colour="white"),
#         strip.text = element_text(colour='white',size=10,face="bold",
#                                   margin=margin(0.1,0.1,0.1,0.1,"cm")),
#         axis.title = element_text(size=8, hjust=0.5),
#         legend.position = "bottom",
#         legend.key.size = unit(0.4,"line"),
#         legend.key.width = unit(0.2,"cm"),
#         legend.key.height = unit(0.2,"cm"),
#         legend.title = element_text(size=6),
#         legend.text = element_text(size=5),
#         legend.box.spacing = unit(0.1,'cm'),
#         legend.margin=margin(t=-0.0, unit='cm'),
#         legend.key = element_blank(),
#         axis.text  = element_text(size=7, hjust=0.5),
#         title = element_text(colour="black",size=10,face="bold"),
#         plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
# ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
#               threshold,"m_zoc",offset,"/MaxDep_Dur_AllDives_5HP_",
#               threshold,"m_zoc",offset,".png"),
#        width=4.5,height=5,units="in",dpi=400)
# 
# # deep dives > 50 m
# #-----------------------
# deep_dives = ndive_summary %>% filter(maxdep>=50)
# table(deep_dives$id)
# ggplot(deep_dives, aes(y = -maxdep, x = dur/60)) +
#   geom_density_2d_filled(contour_var = "ndensity",
#                          aes(colour = dive)) +
#   facet_wrap(vars(id)) +
#   ylim(-365,0) +
#   labs(x = "Dive duration (min)", y = "Dive depth (m)",
#        title = "Deep dives below 50 m", fill="n dives \nprobability") +
#   theme_tq() +
#   theme(panel.spacing = unit(0, "lines"),
#         panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
#         strip.background = element_rect(fill="steelblue4",size=0.2,
#                                         colour="white"),
#         strip.text = element_text(colour='white',size=10,face="bold",
#                                   margin=margin(0.1,0.1,0.1,0.1,"cm")),
#         axis.title = element_text(size=8, hjust=0.5),
#         legend.position = "bottom",
#         legend.key.size = unit(0.4,"line"),
#         legend.key.width = unit(0.2,"cm"),
#         legend.key.height = unit(0.2,"cm"),
#         legend.title = element_text(size=6),
#         legend.text = element_text(size=5),
#         legend.box.spacing = unit(0.1,'cm'),
#         legend.margin=margin(t=-0.0, unit='cm'),
#         legend.key = element_blank(),
#         axis.text  = element_text(size=7, hjust=0.5),
#         title = element_text(colour="black",size=10,face="bold"),
#         plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
# ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
#               threshold,"m_zoc",offset,"/MaxDep_Dur_Below50m_5HP_",
#               threshold,"m_zoc",offset,".png"),
#        width=4.5,height=5,units="in",dpi=400)
# 
# 
# # deep dives > 100 m
# #-----------------------
# deep_dives = ndive_summary %>% filter(maxdep>=100)
# table(deep_dives$id)
# ggplot(deep_dives, aes(y = -maxdep, x = dur/60)) +
#   geom_density_2d_filled(contour_var = "ndensity",
#                          aes(colour = dive)) +
#   facet_wrap(vars(id)) +
#   ylim(-365,0) +
#   labs(x = "Dive duration (min)", y = "Dive depth (m)",
#        title = "Deep dives below 100 m", fill="n dives \nprobability") +
#   theme_tq() +
#   theme(panel.spacing = unit(0, "lines"),
#         panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
#         strip.background = element_rect(fill="steelblue4",size=0.2,
#                                         colour="white"),
#         strip.text = element_text(colour='white',size=10,face="bold",
#                                   margin=margin(0.1,0.1,0.1,0.1,"cm")),
#         axis.title = element_text(size=8, hjust=0.5),
#         legend.position = "bottom",
#         legend.key.size = unit(0.4,"line"),
#         legend.key.width = unit(0.2,"cm"),
#         legend.key.height = unit(0.2,"cm"),
#         legend.title = element_text(size=6),
#         legend.text = element_text(size=5),
#         legend.box.spacing = unit(0.1,'cm'),
#         legend.margin=margin(t=-0.0, unit='cm'),
#         legend.key = element_blank(),
#         axis.text  = element_text(size=7, hjust=0.5),
#         title = element_text(colour="black",size=10,face="bold"),
#         plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
# ggsave(paste0("./FIGURES/Depth_Profiles/calib_",
#               threshold,"m_zoc",offset,"/MaxDep_Dur_Below100m_5HP_",
#               threshold,"m_zoc",offset,".png"),
#        width=4.5,height=5,units="in",dpi=400)
# 
# 
# 
# 
# 
# 
# # plot daily dive depth according to time of the day
# #--------------------------------------------------
# # Summarize data per id: extract daily max depth
# maxdep_daily = hp %>%
#   group_by(id, date, hour) %>%
#   summarise(period    = first(period),
#             maxdep    = max(depth, na.rm=T),
#             mindep    = min(depth, na.rm=T),
#             meandep   = mean(depth, na.rm=T),
#             mediandep = median(depth, na.rm=T)) %>%
#   ungroup()
# 
# ggplot(maxdep_daily, aes(x=as.factor(hour), y=-mediandep, colour=id)) +
#   geom_boxplot() +
#   facet_wrap(.~ id) +
#   theme_tq() +
#   theme(legend.position = "none") 
# 
# 
# 
# # histogram of daily depth per hour of the day
# #----------------------------------------------
# maxdep_hourly = hp %>%
#   group_by(id, date, hour) %>%
#   summarise(period    = first(period),
#             maxdep    = max(depth, na.rm=T),
#             mindep    = min(depth, na.rm=T),
#             meandep   = mean(depth, na.rm=T),
#             mediandep = median(depth, na.rm=T)) %>%
#   ungroup()
# maxdep_hourly$hour = as.factor(maxdep_hourly$hour)
# 
# ggplot(maxdep_hourly, aes(x=hour, y=maxdep, colour = period)) +
#   geom_point() +
#   scale_colour_brewer(palette = "Blues") +
#   scale_y_continuous(trans = "reverse") +
#   labs(x="Hours of the day", y="Daily max depth (m)",
#        title ="Diel pattern from max depth (daily)", colour="") +
#   theme_tq() +
#   theme(legend.position = "bottom")
# 
# 
# 
# 
# 
# 
# # density plot of max depth (day vs night)
# #---------------------------------------------
# ggplot(maxdep_hourly, aes(x=maxdep, fill = period, colour = period)) +
#   geom_density(aes(y=..scaled..,alpha=.4)) +
#   coord_flip() +
#   scale_x_continuous(trans = "reverse") +
#   scale_fill_brewer(palette = "Set2") +
#   scale_colour_brewer(palette = "Set2") +
#   facet_grid(id~period) +
#   labs(y="Dive density", x="Daily max depth (m)", 
#        title ="Diel pattern from max depth (daily)", fill="") +
#   theme_tq() +
#   theme(legend.position = "none")
# ggsave(paste0("./FIGURES/Dive_diel_pattern/Histo_DailyMaxDep_period.png"),
#        width=6,height=5,units="in",dpi=400)
# 
# ggplot(maxdep_hourly, aes(y = maxdep, x = as.numeric(hour), colour = id)) +
#   geom_smooth(method="gam", lwd = 0.5) +
#   scale_y_continuous(trans = "reverse") +
#   ylim(365, 0) +
#   facet_grid(.~id) +
#   labs(x="Hour", y="Daily max depth (m)", 
#        title ="Diel pattern from max depth (daily)", fill="") +
#   theme_tq()
# ggsave(paste0("./FIGURES/Dive_diel_pattern/geomSmooth_DailyMaxDep_hour.png"),
#        width=7,height=3,units="in",dpi=400)
# 
# ggplot(maxdep_hourly, aes(y = maxdep, x = hour, colour = id)) +
#   geom_boxplot() +
#   scale_y_continuous(trans = "reverse") +
#   ylim(365, 0) +
#   facet_wrap(.~id) +
#   labs(x="Hour", y="Daily max depth (m)", 
#        title ="Diel pattern from max depth (daily)", fill="") +
#   theme_tq()






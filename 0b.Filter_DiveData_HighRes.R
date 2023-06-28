##########################################################################
#####   IMPORT and CLEAN Harbour Porpoises data from 2013-2015   #########
#####                HIGH RESOLUTION DATA (1 Hz)                 #########
#####                      n=5 MK10 tags                         #########
##########################################################################

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
library(lutz)
library(purrr)
library(tidyr)


# concatenate the 5 files
# remove depth outliers
# identify dives > 5 m
# extract coordinates closest in time from location dataset




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
      mutate(date  = as.numeric(dateTime),
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
summary(hp$posix) # no NA so ok

# reformat depth column
#------------------------
hp = hp %>%
  mutate(depth2 = parse_number(depth)) 
summary(hp$depth2) 
hp = hp %>% 
  select(-depth) %>%
  rename(depth = depth2) 
gc()
summary(hp$posix)


# summarise data per ID
#------------------------
hp %>%
  group_by(id) %>%
  summarise(start    = first(posix),
            end      = last(posix),
            duration = difftime(last(posix), first(posix), units="days"),
            nobs     = n(),
            meanDep  = mean(depth),
            maxDep   = max(depth))
# id      start               end                 duration       meanDep maxDep
# 13A0471 2013-10-02 15:51:00 2013-10-18 12:39:25  15.86696 days    38.2   336.
# 14A0375 2014-07-22 15:15:00 2014-12-15 11:29:37 145.84348 days    27.6 32728.!!
# 14A0376 2014-07-17 19:11:00 2014-12-03 14:07:21 138.78913 days    30.0 32728.!!
# 14A0380 2014-07-22 15:24:00 2014-12-07 15:21:21 137.99816 days    25.0 32727 !!
# 14A0384 2014-07-27 09:15:41 2015-01-08 15:49:49 165.27370 days    29.2   348 
# extreme depths for some individuals!

# plot histo of depth per ID
#-----------------------------
ggplot(data = hp, aes(x=-depth)) +
  geom_histogram() +
  coord_flip() +
  labs(y = "dive frequency", x = "Depth (m)") +
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
hp = hp %>% dplyr::select(-c(dateTime))
str(hp)











######################################
# clean dataset: check depth outliers
######################################

# remove depth outliers
#--------------------------
summary(hp$depth)  # starts at -40m and goes up to 32727.5 m !
hp = hp %>% filter(depth < 600)

# summarize dataset to 1 maxDep per day for checking
#----------------------------------------------------
maxdep_daily = hp %>%
  group_by(id) %>%
  mutate(day = as.numeric(date - first(date)) + 1) %>%
  group_by(id, date) %>%
  summarise(maxdep    = max(depth),
            mindep    = min(depth),
            meandep   = mean(depth),
            mediandep = median(depth)) %>%
  ungroup()
maxdep_daily 

# need zoc!
ggplot(data = maxdep_daily, aes(x=-mindep)) +
  geom_histogram() +
  labs(y = "dive frequency", x = "min depth (m)") +
  facet_wrap(~id, scales="free")
summary(hp$depth)  # min=-40 --> need ZOC


# need to reset depth above 0 (some values below 0)
# remove negative depth (above 3 m)
#---------------------------------------------------
nrow(hp[hp$depth < (-1.5),]) / nrow(hp )*100  # 0.4%
hp = hp %>%
  filter(depth >= (-1.5)) %>%
  mutate(depth = depth + 1.5) 
summary(hp$depth)                             # 0 to 362 m

# add column day and month
# day 1 = first day for each id
#--------------------------------
hp = hp %>%
  group_by(id) %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1,
         month      = substr(date, 6, 7)) %>%
  ungroup()
unique(hp$month)
range(hp$day_depart)

# order dates by ID
#-----------------------
hp = hp[order(hp$id, hp$posix),]
gc()
saveRDS(hp, "./RDATA/0b.HP_dive_HR_Not_filtered.RDS")












###################################################################################################
# ZOC and dives identification
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent,
# (DA) descent/ascent (occurring when no bottom phase can be detected) and (X) non-dive (surface),
###################################################################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
path = paste0("./Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/",
              "ANALYSES/HP-Diel-Seasonal-Pattern/RDATA/0b.HP_dive_HR_Not_filtered.rds")
str(hp)
offset    = 0
threshold = 5  # threshold depth below which an underwater phase should be considered a dive

system.time({  # 13,643 sec for 5 ids (4h30) with threshold=5 m, offset=0
  for (i in 1:length(unique(hp$id))) {
    id = hp %>%
      filter(id == unique(id)[i]) %>%
      rename(time = posix)

    ptt = unique(id$id)
    id  = id %>% select(depth, time)
    id  = id[order(id$time),]
    id  = id[!duplicated(id$time),]

    tdr   = createTDR(id$time, id$depth, file=path, speed=F)
    calib = calibrateDepth(tdr, dive.thr=threshold,
                           zoc.method="offset", offset=offset)

    saveRDS(calib, paste0("./RDATA/calib_",threshold,"m_zoc",offset,"/","calib_",
                          ptt,"_dcalib_",threshold,"m_zoc",offset,".RDS"))
  }
})











#########################################################################
# Add dive number and phases to the dive dataset
#########################################################################
offset    = 0 # zero offset correction in meters
threshold = 5 # threshold depth below which an underwater phase should be considered a dive
zoc       = paste0("calib_",threshold,"m_zoc",offset)
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern",
             "/RDATA/",zoc))
files     = list.files(pattern=paste0(zoc,".RDS"))
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
summary(dive$posix)











##############################################
# correct for remaining depth above 0
##############################################

#-------------------------------------------------
# check proportion of dive above depth threshold?
#-------------------------------------------------
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
ggplot(dat, aes(x=posix, y=-depth, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)

# remove 1 meter from depth column to set it to the surface
dive2 = dive %>%
  mutate(depth_cor = depth-1) 
ggplot(dive2[1:400,], aes(x=posix, y=-depth_cor, colour=activity)) +
  geom_path(group="activity") +
  ylim(-30,0)
summary(dive2$depth_cor)  # -1 to 361 m

# set to zero depth<0
dive2$depth_cor[dive2$depth_cor<0] = 0
ggplot(dive2[1:400,], aes(x=posix, y=-depth_cor, colour=activity)) +
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
names(dive)
rm(hp)









##########################################################
# Rolling join to extract coordinates from loc dataset
##########################################################

# extract first depth of each dive/ID
# to avoid having different coordinates for the same dive
#--------------------------------------------------------
first_dep = dive %>%
  dplyr::select(posix, id, dive) %>%
  filter(dive != 0) %>%   # remove surface times
  group_by(id, dive) %>%
  slice_head() %>%        # retain the first depth of each dive
  mutate(first_dep = "yes") %>%
  rename(date = posix) %>%
  ungroup() 
first_dep

# import locations for the 5 HP
#----------------------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
loc <- readRDS("./RDATA/0a.locations_filtered_17HP_tzCorrected.rds") %>% 
  dplyr::select(id, posix, bathy, lon, lat)  %>%
  filter(id == "27262" | id == "22849b" | id == "22850b" | 
         id == "27262b" | id == "93100") %>%
  rename(date = posix) # use posix not posix_local which is in local timezone
unique(loc$id)
unique(first_dep$id)
names(loc)
names(first_dep)


# convert into data tables
#-------------------------
diveDT = setDT(first_dep)
setkey(diveDT,id,date)
locDT  = setDT(loc)
setkey(locDT,id,date)


# rolling join on date
#----------------------
dive2 = locDT[,.(date, id, lon, lat)] %>%
  .[diveDT, roll="nearest"] %>%
  .[] # to display tibble
summary(dive2$lon)
summary(loc$lon)
summary(dive2$lat)
summary(loc$lat)


# check correspondence
#--------------------------------------------
dive2 %>% filter(id == "27262") %>%
  dplyr::select(c(id, date, lon, lat))
id = loc %>% filter(id == "27262") %>% View()
dive2 -> first_dep
names(first_dep)
first_dep = as_tibble(first_dep) %>%
  rename(posix = date)
rm(dive2)

# join coordinates from first_dep to dive (entire dataset with all depths/dive)
#------------------------------------------------------------------------------
names(dive)
names(first_dep)
dive2 = dive %>%
  left_join(first_dep, by = c("id","posix","dive"))
str(dive2)
summary(dive2$lon)
unique(dive2$first_dep)
dive2$first_dep[is.na(dive2$first_dep)] = "no" # assign "no" when depth is not the first one/dive
table(dive2$first_dep)

 
# assign coordinates to all depths for each dive
#------------------------------------------------
dive2 = dive2 %>%
  group_by(id, dive) %>%
  mutate(lon = first(lon),
         lat = first(lat)) %>%
  ungroup()
summary(dive2$lon) # some NA when not a dive (when dive=0)
check = dive2 %>% filter(dive!=0)
summary(check$lon)
summary(check$lat)
dive2 -> dive
rm(dive2, first_dep)


# check coordinates
#--------------------
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

ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-60,-30), ylim=c(55,80)) +
  geom_polygon(aes(group=group), fill="white",lwd=0.1,colour="black") +
  geom_point(data=dive[dive$first_dep=="yes",], 
             aes(lon,lat,colour=id), stroke=0, size=0.3) +
  facet_wrap(~id, ncol=3) +
  theme_tq()  +
  theme(legend.position = "none")





############################
# save dataset
############################
str(dive)
saveRDS(dive, "./RDATA/0b.HP_dive_HR_filtered_tzNotCorrected.RDS")











###############################################################################
# summarise each dive / ID: 1 row=1 dive
# add columns asc_rate and desc_rate
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent
###############################################################################
# dive <- readRDS("./RDATA/0b.HP_dive_HR_filtered_tzNotCorrected.RDS")
str(dive)
system.time({   # 66 sec
  ndive_summary = dive %>%
    filter(dive != 0) %>%            # remove surface times
    group_by(dive, id) %>%
    mutate(difftime = c(NA, diff(posix)),
           diffdep  = c(NA, abs(diff(depth)))) %>%
    summarise(start     = first(posix),
              end       = last(posix),
              nobs      = n(),       # number of observations (depths) / dive
              lon       = unique(lon),
              lat       = unique(lat),
              maxdep    = max(depth),
              mindep    = min(depth),
              meandep   = mean(depth),
              bot_dur   = sum(difftime[phase == "B"], na.rm=T),        # in sec
              asc_dur   = sum(difftime[phase == "A"], na.rm=T),        # in sec
              des_dur   = sum(difftime[phase == "D"], na.rm=T),        # in sec
              asc_rate  = mean(diffdep[phase == "A"]/difftime[phase == "A"], na.rm=T),     # in m/s
              des_rate  = mean(diffdep[phase == "D"]/difftime[phase == "D"], na.rm=T),
              bot_rate  = mean(diffdep[phase == "B"]/difftime[phase == "B"], na.rm=T)) %>% # in m/s
    mutate(dur = as.numeric(difftime(end, start, units = "secs"))) %>% # in sec
    ungroup()
})
summary(ndive_summary$lon)
summary(ndive_summary$lat)
names(ndive_summary)

# check duplicated dives
#-----------------------------
nrow(ndive_summary %>% 
       distinct(id, start, .keep_all = TRUE))          # 393,483
ndive_summary %>% filter(duplicated(cbind(id, start))) # 0 rows 
str(ndive_summary)
summary(ndive_summary)

# check outliers (extreme duration)
#------------------------------------
hist((ndive_summary$dur)/60, xlab = "Dive duration (min)")
nrow(ndive_summary[ndive_summary$dur>(7*60),]) / nrow(ndive_summary) * 100 # 0.0005%
ndive_summary = ndive_summary %>% filter(dur < (7*60))

# remove dives lasting 0 min
#---------------------------
ndive_summary %>% filter(dur == 0) %>% View()       
ndive_summary = ndive_summary %>% filter(!dur == 0) # 376,951 dives left

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









###################################################
# Correct time zone according to lon/lat and date
# in summer: UTC-2 West Greenland
# in winter: UTC-3 West Greenland
###################################################
system.time({  # 9 sec 
  ndive_summary2 = ndive_summary %>%
    # find the time zone
    mutate(timezone = tz_lookup_coords(lat = lat,
                                       lon = lon,
                                       method = "accurate"),
           # transform start time using the right time zone
           start_local = map2(
             .x = start,
             .y = timezone,
             .f = function(x, y) {
               with_tz(time = x, tzone = y)
             }
           ),
           # transform end time using the right time zone
           end_local = map2(
             .x = end,
             .y = timezone,
             .f = function(x, y) {
               with_tz(time = x, tzone = y)
             }
           )
    ) %>%
    # required because of the map2
    unnest(start_local, end_local)
})
summary(ndive_summary2$lon)

# check new times
#-----------------
head(paste0(ndive_summary2$start, "->", ndive_summary2$start_local))
head(paste0(ndive_summary2$end,   "->", ndive_summary2$end_local))
table(ndive_summary2$timezone)






#############################################
# Extract sunrise/sunset at locs
#############################################
str(ndive_summary2)
ndive_summary2 = ndive_summary2 %>%
  mutate(date = as.Date(start_local))

## tz= America/Godthab ##
#--------------------------
table(ndive_summary2$timezone)
time1 = ndive_summary2 %>%
  filter(timezone == "America/Godthab") 
time1 = time1 %>%
  mutate(sunrise = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                    tz   = "America/Godthab",
                                    keep = "sunrise")$sunrise,
         sunset = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                   tz   = "America/Godthab",
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                 tz   = "America/Godthab",
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                  tz   = "America/Godthab",
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time1[,c("date","lon","lat")],
                                     tz   = "America/Godthab",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(start_local %within% interval(sunset, night) ~ 'dusk',     # after sunset, before night
                            start_local %within% interval(nightEnd, sunrise) ~ 'dawn', # before sunrise
                            start_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+3 ##
#------------------
time2 = ndive_summary2 %>%
  filter(timezone == "Etc/GMT+3") 
tz = unique(time2$timezone)
time2 = time2 %>%
  mutate(sunrise = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                    tz   = tz,
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                   tz   = tz,
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = tz,
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                 tz   = tz,
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                  tz   = tz,
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time2[,c("date","lon","lat")],
                                     tz   = "America/Godthab",
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(start_local %within% interval(sunset, night) ~ 'dusk',     
                            start_local %within% interval(nightEnd, sunrise) ~ 'dawn', 
                            start_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()


## tz=Etc/GMT+4 ##
#-----------------
time3 = ndive_summary2 %>%
  filter(timezone == "Etc/GMT+4") 
tz = unique(time3$timezone)
time3 = time3 %>%
  mutate(sunrise = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                    tz   = tz,
                                    keep="sunrise")$sunrise,
         sunset = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                   tz   = tz,
                                   keep="sunset")$sunset,
         dawn = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = tz,
                                 keep="dawn")$dawn,
         dusk = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                 tz   = tz,
                                 keep="dusk")$dusk,
         night = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                  tz   = tz,
                                  keep="night")$night,
         nightEnd = getSunlightTimes(data = time3[,c("date","lon","lat")],
                                     tz   = tz,
                                     keep="nightEnd")$nightEnd) %>%
  mutate(period = case_when(start_local %within% interval(sunset, night) ~ 'dusk',    
                            start_local %within% interval(nightEnd, sunrise) ~ 'dawn',
                            start_local %within% interval(sunrise, sunset) ~ 'day',
                            TRUE ~ "night")) %>%
  ungroup()

ndive_summary = rbind(time1, time2, time3)
table(ndive_summary$timezone)
# America/Godthab       Etc/GMT+3       Etc/GMT+4 
#          213054            1888           35212 
rm(ndive_summary2, time1, time2, time3)



##################
# save dataset
##################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
saveRDS(ndive_summary, paste0("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS"))
gc()





############################################
# Calculate daylength per day and per ID
# base don times of sunrise and sunset
############################################
names(ndive_summary)
daylength = ndive_summary %>%
  mutate(day_depart = as.numeric(date - first(date)) + 1) %>%
  filter(day_depart != 1) %>%  # remove first day because never complete
  group_by(id, date) %>%
  summarise(max_dep  = max(maxdep),
            mean_dep = mean(maxdep),
            mean_meandep = mean(meandep),
            sunset  = mean(sunset),
            sunrise = mean(sunrise)) %>%
  ungroup() %>%
  mutate(daylength  = as.numeric(difftime(sunset, sunrise, 
                                          units = "hour")))
daylength


# save data
#--------------
saveRDS(daylength, "./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS")




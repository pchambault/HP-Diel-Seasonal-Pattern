################################################
######### extract SEAPODYM at locations ########
################################################

library(tidync)
library(raster)





###############################
# import HR dataset
###############################
hr <- readRDS("./RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")
names(hr)





##############################
# extract values at HP locs
##############################

# summarise to 1 daily loc/id
#------------------------------
system.time({  # 1 sec
daily = hr %>%
  group_by(id, date) %>%
  dplyr::summarise(lon = first(lon),
            lat = first(lat),
            meandep = mean(depth),
            maxdep = max(depth),
            month  = first(month)) %>%
  mutate(date_ch = as.character(date)) %>%
  ungroup()
})
daily
dim(daily)                 # 609 rows
unique(daily$id)
length(unique(daily$date)) # 193 dates
summary(daily)
plot(lat~lon, daily, pch=20)

  
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/SEAPODYM")
r = raster("mnkc_hmlmeso_02102013-08012015_SEAPODYM.nc") 
hr$mnkc_hmlmeso = NA
hr$date_ch = as.character(hr$date)

system.time({   # 164 sec
  for (i in 1:nbands(r)) {    #  dates
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP/ENV.DATA/SEAPODYM")
    r0 = raster("mnkc_hmlmeso_02102013-08012015_SEAPODYM.nc",band=i) 
    date = as.character(substr(r0@z,1,10))
    
    # extract variable in each location for each date
    hr$mnkc_hmlmeso[hr$date_ch==date]=raster::extract(r0, hr[hr$date_ch==date,c("lon","lat")])
  }
})
summary(hr$mnkc_hmlmeso)  # 47 964 473 NA






# #----------------
# hr2 = daily %>%
#   left_join(df, by=c(date, lon, lat)) 
#   dplyr::select(-c(longitude, latitude))
  




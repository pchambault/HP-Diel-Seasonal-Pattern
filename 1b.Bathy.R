############################################################################
##### INVESTIGATE THE INFLUENCE OF BATHYMETRY on DIVING BEHAVIOUR  #########
#####             CLASSIFY DIVES AS PELAGIC vs BENTHIC DIVES       #########
############################################################################

library(ncdf4)
library(tidyverse)
library(tidyquant)
library(gridExtra)
library(raster)
library(viridis)
library(maps)
library(fields)
library(tidync)
library(data.table)




##################################
# import shoreline
##################################
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Iceland",]


##################################
# load bathy from GECBO
##################################
bathy = raster("./ENV.DATA/Bathy/GEBCO_Arctic_West.nc")
bathy[bathy>0] = NA  # remove cells on land (above sea level)




##########################################
# import dive data from the 5 HR tags
##########################################
dive <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS") %>%
  dplyr::select(id,dive,start,end,lon,lat,maxdep,period,date) %>%
  mutate(month = format(date, "%b")) %>%
  filter(month != "Jan") 
names(dive)
dive$bathy = raster::extract(bathy, dive[,c("lon","lat")])
summary(dive$bathy)   # -1150 to -1 m, 2052 NA (locs very close to shore)
dive$bathy = abs(dive$bathy)

plot(bathy, col = viridis(64), xlim = c(-65,-40), ylim=c(60,70))
points(lat~lon, dive[is.na(dive$bathy),], 
       pch = 20, cex= 0.2, col="red")

# identify dives with no associated bathy (value=NA)
#----------------------------------------------------
library(maps)
nrow(dive[is.na(dive$bathy),])                    # 2052 dives
nrow(dive[is.na(dive$bathy),]) / nrow(dive) * 100 # < 1%
summary(dive$diff)      # 2052 NA because no bathy under these locs
plot(lat~lon, dive, pch = 20)
points(lat~lon, dive[is.na(dive$bathy),], pch = 20, col = "red")





##############################################################
# categorize benthic vs pelagic dives
# according to Harcourt et al 2021, FMS
##############################################################
# three sources of error that contribute to this mismatch are:
# (1) errors in dive location estimates
# (2) errors in the tag???s depth sensors
# (3) errors in the bathymetry

# 1) difference between bathy and maxdep
# calculate diff between bathy and maxdep for the dives that exceeded bathy
#---------------------------------------------------------------------------
dive = dive %>%
  mutate(diff = bathy - maxdep)
summary(dive$diff[!is.na(dive$bathy)])                # -308 to 1144 m
nrow(dive[dive$diff<=(-20),]) / nrow(dive) * 100      # 17% of dives more than 20m deeper than bathy
nrow(dive[dive$diff<=0,]) / nrow(dive) * 100          # 25%
dive$month = factor(dive$month,
                    levels = c("Jul","Aug","Sep","Oct","Nov","Dec"))


# 2) 30 percentile of these differences when maxdep>bathy
# value that cuts off the first 30% of the data values 
# when it is sorted in ascending order.
#---------------------------------------------------------
dive %>% 
  filter(diff<(-20)) %>% 
  select(id, maxdep, bathy, diff)
summary(dive$diff[!is.na(dive$bathy)])
hist(dive$diff[!is.na(dive$bathy)])
quantile(dive$diff[dive$diff<0 & !is.na(dive$bathy)], probs = 0.3)  # -70.5

dive = dive %>%
  mutate(dive_type2 = case_when(diff > (-70) & diff<=70  ~ "benthic",
                                diff > 70 ~ "pelagic",
                                diff <= (-70)  ~ "unknown"))
table(dive$dive_type2) / nrow(dive) * 100 # 45% benthic, 47% pelagic, 7% unknown
ggplot(dive[!is.na(dive$bathy),], aes(x = diff)) +
  geom_bar(aes(fill = dive_type2)) +
  geom_vline(xintercept = 0)


# 3) plot maxdep vs bathy with dive category
#-------------------------------------------------------
ggplot(dive[!is.na(dive$bathy),], aes(x = bathy, y = maxdep)) +
  geom_point(size = 0.5, aes(colour = dive_type2)) +
  labs(x = "Ocean depth (m)", y = "Porpoises max depth (m)") +
  geom_abline(slope=1, linetype = "dashed", color = "Red")

# save data
#----------
saveRDS(dive, "./RDATA/1.diveSummary_5HP_calib_5m_zoc0_tzCorrected_diveType.RDS")





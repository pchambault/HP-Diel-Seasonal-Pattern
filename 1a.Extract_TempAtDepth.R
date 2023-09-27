##############################################################
########  EXTRACT SEA TEMPERATURE at PORPOISES DEPTH  ########
######## DATA FROM COPERNICUS from surface to 380 m   ########
###### PRODUCT: PHY-001-030, daily, RESOLUTION: 0.08 deg #####
##############################################################

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


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")



##############################
# import HP locations
##############################
loc <- readRDS("./RDATA/0a.locations_filtered_17HP_tzCorrected.rds")






#############################################
## Import Temp from 0 to 380 m in 1 netcdf ## 
##        from PHY-030 (0.08 deg)          ##
#############################################
df_nc <- tidync("./ENV.DATA/PHY-001-030/temp0-380m_2Oct2013-31Dec2014_PHY-001-030.nc") 

# flatten the multidimensional array
#------------------------------------
df = df_nc %>% hyper_tibble() %>% setDT()
gc()

# convert time: days since 1950-01-01 00:00:00
#----------------------------------------------
system.time({ df = df[, time := as.Date(time/24,  # 0.76 sec
                                        origin=as.Date("1950-01-01", 
                                                       tz = "UTC"))] })
system.time({ # 96 sec
  df = df %>%
    rename(lon = longitude, 
           lat = latitude) %>%
    mutate(month = format(time, "%b"),
           year  = lubridate::year(time)) 
})
df
gc()

# select cells matching HP coordinates
#---------------------------------------
range(loc$lon)
range(df$longitude)
range(loc$lat)
range(df$latitude)
system.time({ 
  df_temp = df %>%    # 6 sec
  filter(lon>(-60) & lon<(-44)
         & lat>60 & lat<70)
})
gc()
range(df_temp$lon)
range(df_temp$lat)


# retain only months during tracking
#-------------------------------------
df_temp = df_temp %>%
  filter(month != "Jan", month != "Feb", month != "Mar", 
         month != "Apr", month != "May", month != "Jun")
unique(df_temp$month)
df_temp$month = factor(df_temp$month, 
                       levels=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
gc()

# ensure time is in proper format and UTC 
# (otherwise does not find match in loc_dive data when knn!)
#------------------------------------------------------------
system.time({  # 7 min: takes a while...
  df_temp[, time := as.Date(format(time, "%Y-%m-%d"), tz = "UTC")]
})
gc()
oceano_data <- df_temp
oceano_data                  # columns: thetao,lon,lat,depth,time,month,year
unique(oceano_data$temp_dep) # 30 depths in total
# saveRDS(df_temp, "./RDATA/7.Env_Data/temp_0-380m_Oct2013-Dec2014_PHY030.RDS")
# gc()











###############################################################
# import summarized dive data form the 5 high resolution tags
# 1 row=1 dive
###############################################################
# loc_dive_data containing: date in POSIX, lon, lat ...
loc_dive_data <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS") %>%
  dplyr::select(id,start_local,lon,lat,maxdep) %>%
  mutate(date  = as.Date(start_local),
         month = format(date, "%b"),
         year  = format(date, "%Y"))  %>%
  filter(month != "Jan") %>%
  setDT() # 247,485 rows (dives)
unique(loc_dive_data$month)
range(loc_dive_data$date)








############################################################################
# k-mean kriging to extract temp at individual maxdep of each loc and time
############################################################################
# need to check difference between ind maxdep and temp_max
# to make sure knn handled well when no temp at certain depth from CMEMS!
system.time({   # 4h for 250,000 rows, 10 min for 10,000 rows
  temp_atMaxdep <- loc_dive_data[,{
    # number of row from oceanographic date to join
    k <- 1     # get the first neighbour
    # date "xxxx-xx-xx"
    gg <- as.Date(start_local)
    # k-nearest neighbor
    kn <- nabor::knn(
      oceano_data[time == gg, .(
        lon = longitude,
        lat = latitude,
        maxdep = temp_dep
      )],
      matrix(c(lon, lat, maxdep), ncol = 3),
      k  # retain the first nearest neighbour
    )
    # keep all columns from x
    c(
      .SD[rep(seq.int(.N), k)],
      # add columns found in oceanographic_data
      oceano_data[time == gg][
        as.vector(kn$nn.idx),
        .(
          thetao, temp_dep
        )
      ]
    )
  },
  by = .(start_local)
  ]
})
temp_atMaxdep = as_tibble(temp_atMaxdep)
temp_atMaxdep = temp_atMaxdep %>%
  mutate(diff_dep = temp_dep - maxdep)

saveRDS(temp_atMaxdep, "./RDATA/1.temp_at_IndMaxdep_5HP.RDS")


# depth difference between temp and ind maxdep
#----------------------------------------------
summary(temp_atMaxdep$diff_dep) # -25 + 25 m
hist(temp_atMaxdep$diff_dep)
nrow(temp_atMaxdep[temp_atMaxdep$diff_dep>=10 | temp_atMaxdep$diff_dep <= (-10),]) / nrow(temp_atMaxdep) * 100 # 4% of the dives


# plot temp profile using temp at maxdep
#-----------------------------------------
temp_atMaxdep$month = factor(temp_atMaxdep$month, 
                             levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(temp_atMaxdep, aes(y=thetao, x=maxdep)) +
  geom_point(aes(colour=month), size = 1, stroke = 0) +
  geom_smooth(method = "gam", colour = "black") +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(400, 0)) +
  labs(y = "Thetao (deg C)", x = "Depth (m)") +
  facet_wrap(~id, ncol = 5) +
  theme_tq() +
  theme(legend.position = "bottom")

# plot temp profile vs depth (smooth curve for the 5 HP)
#---------------------------------------------------------
ggplot(temp_atMaxdep, aes(y=thetao, x=maxdep)) +
  geom_point(aes(colour=id), size = 1, stroke = 0) +
  geom_smooth(method = "gam", colour = "black", fill = "brown1") +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(380, 0)) +
  labs(y = "Water temperature (deg C)", x = "Depth (m)") +
  facet_wrap(~month, ncol = 6) +
  theme_tq() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(0.0, "lines"))

# plot temp profile vs depth (1 smooth curve per 5 HP)
#-------------------------------------------------------
ggplot(temp_atMaxdep, aes(y=thetao, x=maxdep)) +
  geom_point(colour="black", size = 1, stroke = 0, alpha=0.3) +
  geom_smooth(method = "gam", aes(colour=id)) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(380, 0)) +
  labs(y = "Water temperature (deg C)", x = "Depth (m)") +
  facet_wrap(~month, ncol = 6) +
  theme_tq() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(0.0, "lines"))


# plot temp profile over time / id
#-----------------------------------
ggplot(temp_atMaxdep, aes(y=maxdep, x=start_local)) +
  geom_point(aes(colour=thetao), size = 0.5, stroke = 0) +
  scale_y_continuous(trans = "reverse", limits = c(380, 0)) +
  scale_colour_continuous(type = "viridis") +
  labs(x = "", y = "Depth (m)", 
       colour="Water \ntemperature \n(deg C)") +
  facet_wrap(~id, ncol = 5, scales = "free_x") +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90),
        panel.spacing.x = unit(0.0, "lines")) 









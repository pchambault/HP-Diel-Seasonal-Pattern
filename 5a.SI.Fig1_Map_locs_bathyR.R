############################################################################
#####         SI FIG 1: Individual maps of locs with Bathy         #########
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



###########################################
# import dive data
###########################################
dive <- readRDS("./RDATA/1.diveSummary_5HP_calib_5m_zoc0_tzCorrected_diveType.RDS") %>%
  select(id,start,bathy,lon,lat,maxdep)
dive

# generates classes for bathy
options(scipen=999)
summary(dive$bathy)
dive = dive %>%
  filter(!is.na(bathy))
dive$bathy_cut = cut(dive$bathy, dig.lab = 4,
                     breaks = c(0,50,200,500,1200))
unique(dive$bathy_cut)




#######################################
# plot maps of locations with bathy
#######################################

# facet by bathy classes
#---------------------------
ggplot(shore, aes(long, lat)) +
  geom_point(data=dive, aes(lon,lat,colour=id), size=0.1) +
  coord_map("azequidistant", xlim=c(-55,-40), ylim=c(60,72)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  scale_colour_brewer(palette = "Set2") +
  facet_wrap(~bathy_cut, ncol=4) + 
  labs(x="", y="", coloour = "ID") +
  theme_tq() +
  theme(panel.spacing.x  = unit(0.2, "lines"),
        panel.spacing.y  = unit(0.0, "lines")) + 
  guides(colour = guide_legend(nrow = 1, 
                               override.aes = list(size=4)))
ggsave(filename="./PAPER/5.SciRep/2.Review_Sep2023/SI/SI_Fig.1.png",
       width=200,height=120,units="mm",dpi=400) 




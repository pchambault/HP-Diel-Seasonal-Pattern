##############################################################
####   SI FIGURE 2-6: barplot bathy vs dive maxdep   #########
##############################################################

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



##########################################
# import dive data from the 5 HR tags
##########################################
dive <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0_tzCorrected_diveType.RDS") %>%
  dplyr::select(id,dive,start,end,maxdep,period,
                date,month,bathy,diff,dive_type2)
names(dive)

# some NA values for bathy when too close to shore and>0 values
# replace those by "unknown category"
#----------------------------------------------------------------
unique(dive$dive_type2)
dive$dive_type2[is.na(dive$bathy)] = "unknown"
unique(dive$dive_type2)

dive$month = factor(dive$month,
                    levels = c("Jul","Aug","Sep","Oct","Nov","Dec"))













#######################################################
# dive profile with dive type (benthic vs pelagic)
# facet per month for each id
#######################################################

# calculate proportions of pelagic dives/ID
#-------------------------------------------
prop = dive %>%
  group_by(id) %>%
  summarise(n_tot     = n_distinct(dive),
            n_pelagic = n_distinct(dive[dive_type2 == "pelagic"]),
            n_benthic = n_distinct(dive[dive_type2 == "benthic"]),
            n_unknown = n_distinct(dive[dive_type2 == "unknown"])) %>%
  ungroup() %>%
  mutate(prop_pelagic = (n_pelagic / n_tot) * 100,
         prop_benthic = (n_benthic / n_tot) * 100,
         prop_unknown = (n_unknown / n_tot) * 100,
         sum_dives = prop_pelagic + prop_benthic + prop_unknown)
prop
mean(prop$prop_pelagic) # 49%
sd(prop$prop_pelagic)   # 11%
mean(prop$prop_benthic) # 43%
sd(prop$prop_benthic)   # 8%

nrow(dive[dive$dive_type2=="pelagic",]) / nrow(dive) * 100 # 47%
nrow(dive[dive$dive_type2=="benthic",]) / nrow(dive) * 100 # 45%




## 27262 ##
#------------
unique(dive$id)
id = dive %>% 
  filter(id == "27262")

ggplot(id, aes(y = maxdep, x = start)) +
  geom_linerange(aes(ymin  = 0, ymax = maxdep, colour = dive_type2), 
                 linewidth = 0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values = c("aquamarine4","chocolate1","grey")) +
  theme_tq() +
  facet_wrap(~month, ncol=1, scales="free_x") +
  labs(y = "Maximum dive depth (m)", x = "Day of the month", 
       colour = "Dive type",
       title  = paste0(unique(id$id)),
       subtitle = paste0("(Pelagic: ",round(prop$prop_pelagic[prop$id==unique(id$id)]),"%",
                                                       "; Benthic: ",round(prop$prop_benthic[prop$id==unique(id$id)]),"%)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=100,units="mm",dpi=400)


## 93100 ##
#------------
id = dive %>% 
  filter(id == "93100")

ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour = dive_type2), 
                 linewidth=0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("3 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values=c("aquamarine4","chocolate1","grey")) +
  # geom_line(data=id, aes(x=start, y=bathy),colour="black",lwd=0.3) +
  theme_tq() +
  facet_wrap(~month, ncol=3, scales="free_x") +
  labs(y = "Maximum dive depth (m)", x = "Day of the month", 
       colour = "Dive type",
       title  = paste0(unique(id$id)),
       subtitle = paste0("(Pelagic: ",round(prop$prop_pelagic[prop$id==unique(id$id)]),"%",
                                                    "; Benthic: ",round(prop$prop_benthic[prop$id==unique(id$id)]),"%)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=200,units="mm",dpi=400)


## 22850b ##
#------------
id = dive %>% 
  filter(id == "22850b")

ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour = dive_type2), 
                 linewidth = 0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("2 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values=c("aquamarine4","chocolate1","grey")) +
  theme_tq() +
  facet_wrap(~month, ncol=3, scales="free_x") +
  labs(y = "Maximum dive depth (m)", x = "Day of the month",
       colour = "Dive type",
       title  = paste0(unique(id$id)),
       subtitle = paste0("(Pelagic: ",round(prop$prop_pelagic[prop$id==unique(id$id)]),"%",
                         "; Benthic: ",round(prop$prop_benthic[prop$id==unique(id$id)]),"%)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=200,units="mm",dpi=400)




## 27262b ##
#------------
id = dive %>% 
  filter(id == "27262b")

ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour = dive_type2), 
                 linewidth = 0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("3 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values=c("aquamarine4","chocolate1","grey")) +
  # geom_line(data=id, aes(x=start, y=bathy),colour="black",lwd=0.3) +
  theme_tq() +
  facet_wrap(~month, ncol=3, scales="free_x") +
  labs(y = "Maximum dive depth (m)", x = "Day of the month", 
       colour = "Dive type",
       title  = paste0(unique(id$id)),
       subtitle = paste0("(Pelagic: ",round(prop$prop_pelagic[prop$id==unique(id$id)]),"%",
                         "; Benthic: ",round(prop$prop_benthic[prop$id==unique(id$id)]),"%)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=200,units="mm",dpi=400)



## 22849b ##
#------------
id = dive %>% 
  filter(id == "22849b")
id = id[order(id$start),]

ggplot(id, aes(y = maxdep, x = start)) +
  geom_linerange(aes(ymin  = 0, ymax = maxdep, colour = dive_type2), 
                 linewidth = 0.1) +
  scale_y_continuous(trans  = "reverse", expand = c(0,0), 
                     limits = c(365,0),
                     breaks = c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("3 day"), 
                   date_labels = "%d", expand = c(0,0)) +
  scale_colour_manual(values = c("aquamarine4","chocolate1","grey")) +
  theme_tq() +
  facet_wrap(~month, ncol = 3, scales = "free_x") +
  labs(y = "Maximum dive depth (m)", x = "Day of the month", 
       colour = "Dive type",
       title  = paste0(unique(id$id)),
       subtitle = paste0("(Pelagic: ",round(prop$prop_pelagic[prop$id==unique(id$id)]),"%",
                    "; Benthic: ",round(prop$prop_benthic[prop$id==unique(id$id)]),"%)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=200,units="mm",dpi=400)





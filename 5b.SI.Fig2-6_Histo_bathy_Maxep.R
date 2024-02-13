##############################################################
####     SI FIGURE 3: barplot bathy vs dive maxdep   #########
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








##############################################################
# plot dive profile with bathy (raw data: 1 dive=1 barplot)
##############################################################

# histo of each maxdep vs bathy per ID
#--------------------------------------------------
ggplot(dive, aes(y = maxdep, x = start)) + 
  geom_linerange(aes(ymin = 0, ymax = maxdep),
                 linewidth = 0.1) +
  geom_line(aes(y = bathy, x=start),colour = "red",lwd = 0.3) +
  labs(x = "", y = "", fill = "") +
  facet_wrap(~id, ncol=5, scales = "free_x") +
  scale_y_reverse(limits = c(400,0)) +
  labs(x="", y="Depth (m)", 
       title="Maximum depth (black) and bathy (red) \nfor each dive") +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 90),
        panel.spacing.x  = unit(0.0, "lines"),
        legend.position = "none") 

# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/MaxDep_bathy_eachDive.pdf"),
#        width=120, height=80, units="mm", dpi=400,family="ArialMT")










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
#    id     n_tot n_pelagic n_benthic prop_pelagic prop_benthic
# 1 22849b 58969     36806     20545         62.4         34.8
# 2 22850b 57429     28114     24507         49.0         42.7
# 3 27262   6357      3618      2253         56.9         35.4
# 4 27262b 61026     26241     30033         43.0         49.2
# 5 93100  63704     21749     33760         34.1         53.0
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
  # geom_line(data=id, aes(x=start, y=bathy),colour="black",lwd=0.3) +
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
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
#                        "DiveProfile_bathy_diveType_",unique(id$id),".pdf"),
#        width=120,height=80,units="mm",dpi=400,family="ArialMT")


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
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
#                        "DiveProfile_bathy_diveType_",unique(id$id),".pdf"),
#        width=120,height=400,units="mm",dpi=400,family="ArialMT")


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
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
#                        "DiveProfile_bathy_diveType_",unique(id$id),".pdf"),
#        width=120,height=400,units="mm",dpi=400,family="ArialMT")




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
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
#                        "DiveProfile_bathy_diveType_",unique(id$id),".pdf"),
#        width=120,height=400,units="mm",dpi=400,family="ArialMT")


## 22849b ##
#------------
id = dive %>% 
  filter(id == "22849b")
id = id[order(id$start),]
# id$month = factor(id$month,
#                   levels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# text = data.frame(start  = c(as.POSIXct("2014-07-25 08:22:33"),
#                              as.POSIXct("2014-07-25 08:22:33")),
#                   maxdep = c(300, 340),
#                   lab = "text",
#                   month = c("Jul","Jul"),
#                   label  = c(paste0("Pelagic: ",
#                                  round(prop$prop_pelagic[prop$id==unique(id$id)]),"%"),
#                            paste0("Benthic: ",
#                                  round(prop$prop_benthic[prop$id==unique(id$id)]),"%")))

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
  # geom_text(data = text, label = text$label, size = 2) +
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

# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
#                        "DiveProfile_bathy_diveType_",unique(id$id),".pdf"),
#        width=120,height=400,units="mm",dpi=400,family="ArialMT")
ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/",
                       "DiveProfile_bathy_diveType2_",unique(id$id),".png"),
       width=200,height=200,units="mm",dpi=400)












# ##################################################
# # plot average bathy and maxdep monthly per ID
# ##################################################
# monthly = dive %>%
#   group_by(id, month) %>%
#   summarise(mean_bathy = mean(bathy, na.rm=T),
#             mean_dep   = mean(maxdep, na.rm=T),
#             max_bathy  = max(bathy, na.rm=T),
#             max_dep    = max(maxdep, na.rm=T),
#             diff_max   = max_bathy - max_dep,
#             diff_mean  = mean_bathy - mean_dep) %>%
#   filter(month != "Jan")
# monthly
# hist(monthly$diff)
# nrow(monthly[monthly$diff_max<0,])  # 7 rows
# nrow(monthly[monthly$diff_mean<0,]) # 4 rows
# 
# 
# # pivot table to get 1 column including 
# # max(bathy) and max(maxdep) monthly
# #----------------------------------------
# pivot_max = monthly %>%
#   pivot_longer(c(max_bathy,max_dep), 
#                names_to = "type", values_to = "max_value") %>%
#   dplyr::select(id,month,type,max_value,diff_max,diff_mean) %>%
#   mutate(type2 = case_when(type == "max_bathy" ~ "ocean depth",
#                            type == "max_dep" ~ "porpoise depth"))
# pivot_max
# pivot_max$month = factor(pivot_max$month,
#                          levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# # histo of monthly maxdep vs bathy per month and ID
# #--------------------------------------------------
# a = ggplot(pivot_max, aes(y=max_value, x=month)) +       
#   geom_col(aes(fill=type2), position="dodge") + 
#   scale_fill_manual(values=c("aquamarine4","chocolate1")) +
#   labs(x = "", y = "", fill = "") +
#   facet_wrap(~id, ncol=5) +
#   scale_y_reverse() +
#   labs(x="", y="Depth (m)", title="Monthly maximum values") +
#   theme_tq() +
#   theme(axis.text.x = element_text(angle = 90),
#         panel.spacing.x  = unit(0.0, "lines"),
#         legend.position = "none") 
# 
# # pivot table to get 1 column including mean(bathy) 
# # and mean(maxdep) monthly
# #-----------------------------------------------------
# pivot_mean = monthly %>%
#   pivot_longer(c(mean_bathy, mean_dep), 
#                names_to = "type", values_to = "mean_value") %>%
#   dplyr::select(id,month,type,mean_value,diff_max,diff_mean) %>%
#   mutate(type2 = case_when(type == "mean_bathy" ~ "ocean depth",
#                            type == "mean_dep" ~ "porpoise depth")) %>%
#   filter(month != "Jan")
# pivot_mean
# pivot_mean$month = factor(pivot_mean$month,
#                           levels=c("Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# # histo of monthly mean dep vs mean bathy per month and ID
# #---------------------------------------------------------
# b = ggplot(pivot_mean, aes(y=mean_value, x=month)) +       
#   geom_col(aes(fill=type2), position="dodge") + 
#   scale_fill_manual(values=c("aquamarine4","chocolate1")) +
#   labs(x = "", y = "", fill = "") +
#   facet_wrap(~id, ncol=5) +
#   scale_y_reverse() +
#   labs(x="", y="Depth (m)", title="Monthly mean values") +
#   theme_tq() +
#   theme(axis.text.x = element_text(angle = 90),
#         panel.spacing.x = unit(0.0, "lines")) 
# 
# # export plot
# #-------------
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/SI.Fig3.pdf"),
#        grid.arrange(a,b,ncol=1),
#        width=120,height=127,units="mm",dpi=400,family="ArialMT")
# 
# 
# 
# 
# ###################################################################
# # histo of the daily dive profiles with the bathymetry overlaid
# ###################################################################
# 
# # average bathy and maxdep daily per ID
# #------------------------------------------
# daily = dive %>%
#   filter(month != "Jan") %>%
#   group_by(id, date) %>%
#   summarise(mean_bathy = mean(bathy, na.rm=T),
#             mean_dep   = mean(maxdep, na.rm=T),
#             max_bathy  = max(bathy, na.rm=T),
#             max_dep    = max(maxdep, na.rm=T),
#             diff_max   = max_bathy - max_dep,
#             diff_mean  = mean_bathy - mean_dep) %>%
#   ungroup() %>%
#   mutate(month = format(date, "%b"))
# daily
# 
# ggplot(daily, aes(y=mean_dep, x=date)) +       
#   geom_col() + 
#   geom_line(aes(y=max_bathy, x=date),colour="red",lwd=0.3) +
#   labs(x = "", y = "", fill = "") +
#   facet_wrap(~id, ncol=5, scales="free_x") +
#   scale_y_reverse() +
#   labs(x="", y="Depth (m)", 
#        title="Daily mean depth (grey) \nand the associated max bathymetry (red)") +
#   theme_tq() +
#   theme(axis.text.x = element_text(angle = 90),
#         panel.spacing.x = unit(0.0, "lines")) 
# 
# ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/SI/Daily_meanDep_maxBathy.pdf"),
#        width=130,height=70,units="mm",dpi=400,family="ArialMT")













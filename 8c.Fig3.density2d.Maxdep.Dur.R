#####################################################
##1 FIG. 3: density 1d plots maxdep/dive duration ###
#####################################################

library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(DT)
library(data.table)
library(ggdensity)




#############################
# load HR dataset (n=5 HP)
#############################
sum <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") %>% 
  ungroup() %>%
  mutate(month = format(as.Date(start), "%b"),
         day_night = case_when(period == "day" ~ "Day",
                               period == "dawn" ~ "Night",
                               period == "dusk" ~ "Night",
                               period == "night" ~ "Night"),
         season = case_when(month == "Jul" | month == "Aug" | month == "Sep" ~"Summer",
                            month == "Oct" | month == "Nov" | month == "Dec" ~"Winter")) %>%
  filter(month != "Jan") # only available for 1 individual, so removed
sum$month = factor(sum$month, levels = c("Jun","Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))
table(sum$season)



################################################
# plot dives according to season and day night
################################################
# sum2 = sum %>% filter(maxdep>20) %>% filter(month == "Aug" | month == "Dec")
ggplot(sum, aes(y = -maxdep, x = dur/60)) +
  geom_hdr(aes(fill = after_stat(probs)),
           alpha = 1, xlim=c(0,6), ylim=c(-300,-20)) +
  # geom_density_2d_filled(contour_var = "ndensity", bins = 8) + 
  scale_x_continuous(breaks = c(1,3,5)) +
  scale_y_continuous(breaks = c(-20,-50,-100,-200,-300),
                     labels = c(20, 50, 100, 200, 300),
                     minor_breaks = c(-20,-50,-100,-200,-300)) +
  scale_fill_brewer(palette = 3, direction = 1) + 
  facet_grid(season ~ day_night) + 
  labs(x = "Dive duration (min)", y = "Maximum depth (m)",
       title = "Dives > 20 m", fill="Dive \nproportions") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        strip.text = element_text(colour='white',size=8,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "right",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l


ggsave(filename=paste0("./FIGURES/PAPER/Fig.3bis_MaxDep_Dur_geomHDR.pdf"),
       # grid.arrange(a, b, ncol = 1),
       width=4,height=5,units="in",dpi=400,family="ArialMT")











############################
# plot geom_hdr_points
############################
ggplot(sum, aes(y = -maxdep, x = dur/60)) +
  geom_hdr_points(size=1, alpha=0.4) +
  scale_x_continuous(breaks = c(1,3,5)) +
  scale_y_continuous(breaks = c(-20,-50,-100,-200,-300),
                     labels = c(20, 50, 100, 200, 300),
                     minor_breaks = c(-20,-50,-100,-200,-300)) +
  scale_fill_brewer(palette = 3, direction = 1) + 
  facet_grid(season ~ day_night) + 
  labs(x = "Dive duration (min)", y = "Maximum depth (m)",
       title = "Dives > 20 m", fill="Dive \nproportions") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.2),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        strip.text = element_text(colour='white',size=8,
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "right",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l

ggsave(filename=paste0("./FIGURES/PAPER/Fig.3_MaxDep_Dur_HDR-point.pdf"),
       width=4,height=5,units="in",dpi=400,family="ArialMT")








# #####################
# # plot diurnal dives
# #####################
# period = sum %>% filter(period =="day" & maxdep>20)
# a = ggplot(period, aes(y = -maxdep, x = dur/60)) +
#   # geom_density_2d_filled(contour_var = "ndensity", bins = 8) + 
#   geom_hdr(aes(fill = after_stat(probs)),
#            alpha = 1, xlim=c(0,6), ylim=c(-300,-20)) +
#   # geom_hdr(aes(fill = after_stat(probs)), alpha = 1) +
#   # scale_y_reverse(limits=c(300,0),expand=c(0,0)) +
#   scale_x_continuous(breaks = c(1,3,5)) +
#   scale_y_continuous(breaks = c(-20,-50,-100,-200,-300),
#                      labels = c(20, 50, 100, 200, 300),
#                      minor_breaks = c(-20,-50,-100,-200,-300)) +
#   scale_fill_brewer(palette = 3, direction = 1) + 
#   facet_wrap(vars(month), ncol=6) +
#   labs(x = "Dive duration (min)", y = "Maximum depth (m)",
#        title = "a) Diurnal dives (>20 m)", fill="Dive \nproportions") +
#   theme_tq() +
#   theme(panel.spacing = unit(0, "lines"),
#         panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
#         strip.background = element_rect(fill = "steelblue4",
#                                         colour="white"),
#         strip.text = element_text(colour='white',size=8,
#                                   margin=margin(0.1,0.1,0.1,0.1,"cm")),
#         axis.title = element_text(size=8, hjust=0.5),
#         legend.position = "right",
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
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
# 
# 
# 
# #####################
# # plot nocturnal dives
# #####################
# period = sum %>% filter(period =="night" & maxdep>20)
# b = ggplot(period, aes(y = -maxdep, x = dur/60)) +
#   # geom_density_2d_filled(contour_var = "ndensity", bins = 8) + 
#   # geom_hdr(aes(fill = after_stat(probs)), alpha = 1) +
#   geom_hdr(aes(fill = after_stat(probs)),
#            alpha = 1, xlim=c(0,6), ylim=c(-300,-20)) +
#   scale_x_continuous(breaks = c(1,3,5)) +
#   scale_y_continuous(breaks = c(-20,-50,-100,-200,-300),
#                      labels = c(20, 50, 100, 200, 300),
#                      minor_breaks = c(-20,-50,-100,-200,-300)) +
#   scale_fill_brewer(palette = 3, direction = 1) + 
#   facet_wrap(vars(month), ncol=6) +
#   labs(x = "Dive duration (min)", y = "Maximum depth (m)",
#        title = "b) Nocturnal dives (>20 m)", fill="Dive \nproportions") +
#   theme_tq() +
#   theme(panel.spacing = unit(0, "lines"),
#         panel.border = element_rect(color = "white", fill = NA, size = 0.2),
#         strip.background = element_rect(fill = "steelblue4",
#                                         colour="white"),
#         strip.text = element_text(colour='white',size=8,
#                                   margin=margin(0.1,0.1,0.1,0.1,"cm")),
#         axis.title = element_text(size=8, hjust=0.5),
#         legend.position = "right",
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
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.margin = unit(c(0.2,0.2,0.5,0.2),"cm"))  # t, r, b, l
# # grid.arrange(a, b, ncol = 1)
# 
# ggsave(filename=paste0("./FIGURES/PAPER/Fig.3_MaxDep_Dur_geomHDR.pdf"),
#        grid.arrange(a, b, ncol = 1),
#        width=5,height=5,units="in",dpi=400,family="ArialMT")


################################################################
### Fig. 3: GAM+geom_tile: mean of daily maxdep vs daylight  ###
################################################################

library(visreg)
library(mgcv)
library(patchwork)
library(data.table)
library(tidyquant)


###################################
# hours vs month, fill=day_night
###################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive <- readRDS("./RDATA/0b.diveSummary_5HP_calib_5m_zoc0_tzCorrected.RDS") %>%
  mutate(hour = as.numeric(substr(start_local, 12, 13)),
         month = format(date, "%b")) %>%
  filter(month != "Jan")
unique(dive$month)

# extract sunset and sunrise daily and per hour
#----------------------------------------------
dat <- dive %>%
  group_by(id, date, hour) %>%
  summarise(sunrise = mean(sunrise),
            sunset  = mean(sunset),
            lon     = mean(lon),
            lat     = mean(lat)) %>%
  ungroup()
dat    # 14,218 rows









#######################
# panel a: dive depth
#######################

# calculate hourly mean depth/day for ID 27262b
#-----------------------------------------------
dataPlot2 = dive %>% 
  filter(id == "27262b") %>%
  group_by(date, hour) %>%
  summarise(depth = mean(maxdep)) %>%
  ungroup()

# extract sunrise and sunset
#-------------------------------
dataPlot = dat %>%
  filter(id == "27262b") %>%
  group_by(date) %>%
  summarise(sunrise = mean(sunrise), # use mean when several locs/day, hours can differ slightly
            sunset  = mean(sunset))  # same

sunrise = dataPlot %>%
  mutate(hour = as.numeric(substr(sunrise, 12, 13)),
         date = as.Date(sunrise))%>%
  ungroup()

sunset = dataPlot %>%
  mutate(hour = as.numeric(substr(sunset, 12, 13)),
         date = as.Date(sunset)) %>%
  ungroup()


# plot panel a
#--------------
a = ggplot(dataPlot2, aes(x = date, y = hour)) +
  geom_tile(aes(fill = depth)) + 
  scale_x_date(date_labels="%b",date_breaks  ="month",expand = c(0, 0)) +
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00","23:00"),
                     breaks=c(0,4,8,12,16,20,23),
                     expand = c(0, 0)) +
  geom_line(data=sunrise, aes(x = date, y = hour), 
             size=0.5, colour="black") + 
  geom_line(data=sunset, aes(x = date, y = hour), 
             size=0.5, colour="black") + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_tq() +
  labs(x = "", y = "Hour of the day (GMT-2)", 
       fill = "Mean depth \n(m)", 
       title = "a) Dive depth (#27262b)") +
  annotate("text", y = 9, x=as.Date("2014-08-01"), 
           label = "Sunrise", size=2, colour="black") +
  geom_segment(aes(x = as.Date("2014-08-01"), y = 8,
                   xend = as.Date("2014-08-01"), yend = 5),
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate("text", y = 17, x=as.Date("2014-08-01"), 
           label = "Sunset", size=2, colour="black") +
  geom_segment(aes(x = as.Date("2014-08-01"), y = 18,
                   xend = as.Date("2014-08-01"), yend = 21),
               arrow = arrow(length = unit(0.1, "cm"))) +
  theme(legend.position = c("bottom"),
        legend.key.size = unit(1, 'cm'),         #change legend key size
        legend.key.height = unit(0.3, 'cm'),     #change legend key height
        legend.key.width = unit(0.5, 'cm'),      #change legend key width
        legend.title = element_text(size=8, 
                                    hjust = 0.5,
                                    vjust = 0.5),#change legend title font size
        legend.text = element_text(size=8),      #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t=-20),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=8,
                                   vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=8, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())









#########################
# panel b: dive duration
#########################

# calculate daily mean dive dur
#-------------------------------
dataPlot2 = dive %>% 
  filter(id == "27262b") %>%
  group_by(date, hour) %>%
  summarise(dur = mean(dur/60)) %>% # in minutes
  ungroup()

# plot panel b
#--------------------
b = ggplot(dataPlot2, aes(x = date, y = hour)) +
  geom_tile(aes(fill = dur)) + 
  scale_x_date(date_labels="%b",date_breaks  ="month",expand = c(0, 0)) +
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00","23:00"),
                     breaks=c(0,4,8,12,16,20,23),
                     expand = c(0, 0)) +
  geom_line(data=sunrise, aes(x = date, y = hour), 
            size=0.5, colour="black") + 
  geom_line(data=sunset, aes(x = date, y = hour), 
            size=0.5, colour="black") + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_tq() +
  annotate("text", y = 9, x=as.Date("2014-08-01"), 
           label = "Sunrise", size=2, colour="black") +
  geom_segment(aes(x = as.Date("2014-08-01"), y = 8,
                   xend = as.Date("2014-08-01"), yend = 5),
               arrow = arrow(length = unit(0.1, "cm"))) +
  annotate("text", y = 17, x=as.Date("2014-08-01"), 
           label = "Sunset", size=2, colour="black") +
  geom_segment(aes(x = as.Date("2014-08-01"), y = 18,
                   xend = as.Date("2014-08-01"), yend = 21),
               arrow = arrow(length = unit(0.1, "cm"))) +
  labs(x = "", y = "Hour of the day (GMT-2)", fill = "Mean duration \n(min)", 
       title = "b) Dive duration (#27262b)") +
  theme(legend.position = c("bottom"),
        legend.key.size = unit(1, 'cm'),          #change legend key size
        legend.key.height = unit(0.3, 'cm'),      #change legend key height
        legend.key.width = unit(0.5, 'cm'),       #change legend key width
        legend.title = element_text(size=8, 
                                    hjust = 0.5,
                                    vjust = 0.5), #change legend title font size
        legend.text = element_text(size=8),       #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t=-20),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=8,
                                   vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=8, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





################
# panel c: GAM
################

# import individual predictions
ind_pred <- readRDS("./RDATA/3.GAM/Indiv-GAM_daylength_mean_dep_slope_intercept_all_tzCorr.rds")
# import population predictions
pop_pred <- readRDS("./RDATA/3.GAM/Pop-GAM_daylength_mean_dep_slope_intercept_all_tzCorr.rds")
# import GAM output (summary to extract deviance)
m <- readRDS("./RDATA/3.GAM/GAM_output_daylength_mean_dep_slope_intercept_all_tzCorr.rds")

# plot
c = ggplot(pop_pred, aes(x = daylength, y = fit_pop)) +
  geom_ribbon(aes(ymin=fit_pop-fit_pop_se,ymax=fit_pop+fit_pop_se),
              alpha=0.3, fill="dimgrey") +
  geom_line(colour="black",lwd=0.7) +
  geom_line(data=ind_pred, aes(daylength, fit_ind, group=id), 
            colour="dimgrey", lwd=0.5, alpha=0.4) +
  ylim(0,120) +
  labs(y = "Daily mean depth (m)", x = "Daylength (hours)", 
       title="c) All individuals") +
  annotate("text", x = 8, y = 15, size = 3,
           label = paste0("Deviance: ",
                          round(as.numeric(summary.gam(m)[14])*100),"%")) +
  theme_tq() +
  theme(legend.key.size = unit(6,"cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.box.spacing = unit(0.1,'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background=element_rect(fill = NA),
        legend.margin=margin(t=-15),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,linewidth=0.2),
        axis.text.x  = element_text(angle = 0, size=8,
                                    vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=8, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, 
                                colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(lwd = 1) ) )




#############################
# export plot with 3 panels
#############################
((a / b) | c) + plot_layout(widths  = c(1.5, 1),
                            heights = c(1,2))
ggsave(filename=paste0("./PAPER/4.PRSB/Fig.3.pdf"),
       width=190,height=110,units="mm",
       dpi=400,family="ArialMT")















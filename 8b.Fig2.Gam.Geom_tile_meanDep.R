##################################################
### Fig. 2: GAM+geom_tile: meandep vs daylight ###
##################################################

library(visreg)
library(mgcv)
library(patchwork)
library(data.table)
library(tidyquant)


###################################
# hours vs month, fill=day_night
###################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
dive <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")
names(dive)
dive = dive %>%
  mutate(hour = as.numeric(substr(start, 12, 13)),
         date = as.Date(substr(start, 1, 10)))

# daily averaged
#----------------
dat <- dive %>%
  group_by(id, date, hour) %>%
  summarise(period  = first(period),
            sunrise = mean(sunrise),
            sunset  = mean(sunset)) %>%
  ungroup()
dat # 14 473 rows









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
        legend.key.size = unit(1, 'cm'),       #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.5, 'cm'),    #change legend key width
        legend.title = element_text(size=8, 
                                    hjust = 0.5,
                                    vjust = 0.5),   #change legend title font size
        legend.text = element_text(size=7),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t=-20),
        axis.title.y = element_text(size=7),
        axis.text.x = element_text(size=7,
                                   vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=7, hjust=0.5),
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
  summarise(dur = mean(dur/60)) %>%
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
        legend.key.size = unit(1, 'cm'),       #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.5, 'cm'),    #change legend key width
        legend.title = element_text(size=8, 
                                    hjust = 0.5,
                                    vjust = 0.5), #change legend title font size
        legend.text = element_text(size=7),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.margin = margin(t=-20),
        axis.title.y = element_text(size=7),
        axis.text.x = element_text(size=7,
                                   vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





################
# panel c: GAM
################
ind_pred <- readRDS("./RDATA/4.GAM/Indiv-GAM_daylength_meandep_slope_intercept_all.rds")
pop_pred <- readRDS("./RDATA/4.GAM/Pop-GAM_daylength_meandep_slope_intercept_all.rds")
m <- readRDS("./RDATA/4.GAM/GAM_output_daylength_meandep_slope_intercept_all.rds")

c = ggplot(pop_pred, aes(x = daylength, y = fit_pop)) +
  geom_ribbon(aes(ymin=fit_pop-fit_pop_se,ymax=fit_pop+fit_pop_se),
              alpha=0.3, fill="dimgrey") +
  geom_line(colour="black",lwd=0.7) +
  geom_line(data=ind_pred, aes(daylength, fit_ind, group=id), 
            colour="dimgrey", lwd=0.5, alpha=0.4) +
  labs(y = "Daily mean depth (m)", x = "Daylength (hours)", 
       title="c) All individuals") +
  annotate("text", x = 5, y = 15, size = 2,
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
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        axis.text.x = element_text(angle = 0, size=7,
                                   vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(lwd = 1) ) )



((a / b) | c) + plot_layout(widths  = c(1.5, 1),
                            heights = c(1,2))
ggsave(filename=paste0("./FIGURES/PAPER/Fig.2_geom-tile_GAM_meandep.pdf"),
       width=7,height=4,units="in",dpi=400,family="ArialMT")














# ##############################
# # plot smooth curves from GAM
# ##############################
# #------------
# # run GAM
# #------------
# m = gam(meandep ~ s(daylight, k=5) + #s(day_depart, k=5) +
#           s(id, bs = 're') +           # random intercept
#           s(daylight, id, bs = 're'),  # random slope
#         data = dat, method="REML") 
# summary(m) # mediandep: 60%, maxdep:30%, meandep: 68%
# plot(m,pages=1, shade=T)
# 
# 
# # extract GAM outupt 
# #-----------------------
# visreg(m, "daylight", gg=TRUE, type = "conditional")
# plot = visreg(m, type="conditional", plot = FALSE, gg=TRUE)
# 
# # The output from visreg is a list of the same length as the number of 'x' variables,
# # so we use ldply to pick the objects we want from the each list part and make a dataframe: 
# smooths <- ldply(plot, function(part)   
#   data.frame(Variable = part$meta$x, 
#              x=part$fit[[part$meta$x]], 
#              smooth=part$fit$visregFit, 
#              lower=part$fit$visregLwr, 
#              upper=part$fit$visregUpr))
# smooths = as_tibble(smooths)
# smooths
# # ggplot(ind_pred, aes(x = daylight, y = fit_ind)) +
# #   geom_line(aes(colour=id),lwd=1) +
# #   scale_colour_brewer(palette = "Paired") +
# #   scale_fill_brewer(palette = "Paired") +
# #   geom_hline(yintercept = 0, lty="dashed", colour = "black", lwd=0.2) +
# #   geom_ribbon(aes(ymin=fit_ind-se_ind, 
# #                   ymax=fit_ind+se_ind, fill=id), alpha=0.2) +
# #   labs(y = "Daily mean depth (m)", x = "Daylength (hours)", title="") +
# #   theme(legend.position = c(0.9,0.8),
# #         legend.key.size = unit(6,"cm"),
# #         legend.key.width = unit(0.5,"cm"),
# #         legend.key.height = unit(0.5,"cm"),
# #         legend.title = element_blank(),
# #         legend.text = element_text(size=8),
# #         legend.box.spacing = unit(0.1,'cm'),
# #         legend.margin=margin(t=-0.0, unit='cm'),
# #         legend.spacing.x = unit(0.1, 'cm'),
# #         legend.key = element_blank(),
# #         legend.background=element_rect(fill = NA),
# #         axis.title = element_text(size=9, hjust=0.5),
# #         panel.border = element_rect(colour="black",fill=NA,size=0.2),
# #         axis.text.y  = element_text(size=8, hjust=1),
# #         axis.text.x  = element_text(size=8, hjust=0.5),
# #         panel.background = element_blank(),
# #         panel.grid.major = element_blank()) +
# #   guides(color = guide_legend(override.aes = list(lwd = 1) ) )
# 

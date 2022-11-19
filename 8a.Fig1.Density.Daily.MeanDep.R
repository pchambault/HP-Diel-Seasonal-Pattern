################################################
## FIG 1: plot geom_density meanDep vs months ##
################################################

library(visreg)
library(plyr)
library(mgcv)
library(patchwork)
library(data.table)
library(tidyverse)


###############################################
# load high resolution
###############################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP")
dive <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")  %>%
  mutate(date = as.Date(substr(start, 1, 10)),
         hour = substr(start, 12, 13),
         month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  ungroup()
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec","Jan"))


# calculate daily depth per phase and ID
#----------------------------------------
daily = dive %>%
  group_by(id, date, day_night) %>%
  summarise(max_dep    = max(maxdep, na.rm=T),
            mean_dep   = mean(maxdep, na.rm=T),
            median_dep = median(maxdep, na.rm=T)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily$month = factor(daily$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec","Jan"))


#--------------
# pivot_longer
#--------------
pivot = daily %>%
  pivot_longer(c(max_dep, mean_dep), 
               names_to = "metric", values_to = "values") 
pivot
pivot$month = factor(pivot$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec","Jan"))
pivot = pivot %>%
  mutate(metric2 = case_when(metric == "mean_dep" ~ "Mean depth",
                             metric == "max_dep" ~ "Maximum depth"))
pivot$metric2 = factor(pivot$metric2, levels = c("Mean depth","Maximum depth"))




#---------------------------------
# get mean daily depth/month/phase
# to ass text on plot
#---------------------------------
mean = pivot %>%
  group_by(day_night, month, metric2) %>%
  summarise(val = mean(values))
mean

text_day_mean = mean %>% 
  filter(day_night == "day" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 300)
text_night_mean = mean %>% 
  filter(day_night == "night" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 325)

text_day_max = mean %>% 
  filter(day_night == "day" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 370)
text_night_max = mean %>% 
  filter(day_night == "night" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 395)



#------------------
# plot facet_grid
#------------------
ggplot(data = pivot, aes(x=values, fill=day_night, colour=day_night), 
       colour="gray47") +
  geom_density(aes(y=..scaled..),alpha=.6,lwd=0.2) + 
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(400, 0)) + 
  scale_y_continuous(breaks=c(0, 0.5, 1), labels=c(0,0.5,1)) +
  scale_fill_manual(values = c("navajowhite3","dodgerblue3")) +
  labs(y = "Dive probability", x = "Dive depth (m)", 
       title= "", fill="") +
  facet_grid(metric2~month) +
  theme_bw() +
  geom_text(data=text_day_mean, aes(x=values, y = 0.5, label=text), 
            size=2, colour = "navajowhite4") + 
  geom_text(data=text_night_mean, aes(x=values, y = 0.5, label=text), 
            size=2, colour = "dodgerblue4") + 
  
  geom_text(data=text_day_max, aes(x=values, y = 0.6, label=text), 
            size=2, colour = "navajowhite4") + 
  geom_text(data=text_night_max, aes(x=values, y = 0.6, label=text), 
            size=2, colour = "dodgerblue4") + 
  
  geom_vline(data = mean, aes(xintercept=val, colour = day_night),
             lwd=0.2, linetype = "dashed") +
  scale_colour_manual(values = c("navajowhite4","dodgerblue4"),
                      guide="none") +
  # geom_vline(xintercept = 0, colour="gray47", lwd=0.2) +
  theme(legend.position = c(0.07,0.8),
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=7, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, # t r b l
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave(filename=paste0("./FIGURES/PAPER/Fig.1_density.DailyDepth.HR.pdf"),
       width=7,height=4,units="in",dpi=400,family="ArialMT")





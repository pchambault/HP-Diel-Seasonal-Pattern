################################################################
##      SI FIG 1: geom_density plot of low resolution data   ###
##                   low resolution datasets            ########
################################################################

library(patchwork)
library(data.table)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gridExtra)



###############################################
# reshape low resolution dataset
###############################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dive <- readRDS("./RDATA/0c.depth_Binned_LR_19ids_tzCorrected.RDS") %>%
  select(id,date,posix_local,sunrise,sunset,period,depth) %>%
  mutate(month = format(date, "%b"),
         day_night = case_when(period == "day"   ~ "day",
                               period == "dusk"  ~ "night",
                               period == "dawn"  ~ "night",
                               period == "night" ~ "night")) %>%
  filter(id != "7618",id != "7617") %>% # removed because different depth bin settings
  filter(month != "Jan")                # removed because only available for one id
dive %>% select(posix_local,sunrise,sunset,day_night)
dive$month = factor(dive$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))


#------------------------------
# calculate mean daily depth
#------------------------------
daily = dive %>%
  group_by(id, date, day_night) %>%
  summarise(meandep = mean(depth),
            maxdep  = max(depth)) %>%
  mutate(month = format(date, "%b")) %>%
  ungroup()
daily


#--------------------------------------------------------------
# pivot_longer to have both maxdep and meandep in same column
#--------------------------------------------------------------
pivot = daily %>%
  pivot_longer(c(maxdep, meandep), 
               names_to = "metric", values_to = "values") 
pivot
pivot$month = factor(pivot$month, levels=c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))
pivot = pivot %>%
  mutate(metric2 = case_when(metric == "meandep" ~ "Mean depth",
                             metric == "maxdep" ~ "Maximum depth"))
pivot$metric2 = factor(pivot$metric2, levels = c("Mean depth","Maximum depth"))
pivot



#---------------------------------
# get mean daily depth/month/phase
# to add text on plot
#---------------------------------
mean = pivot %>%
  group_by(day_night, month, metric2) %>%
  summarise(val = mean(values))
mean

text_day_mean = mean %>% 
  filter(day_night == "day" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 280)
text_night_mean = mean %>% 
  filter(day_night == "night" & metric2 == "Mean depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 300)

text_day_max = mean %>% 
  filter(day_night == "day" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 470)
text_night_max = mean %>% 
  filter(day_night == "night" & metric2 == "Maximum depth") %>%
  mutate(text = paste0("mean = ",round(val, 1)," m"),
         values = 500)





##################################################
# calculate t tests between day and night / month
##################################################
stat_meandep = pivot %>%
  dplyr::select(-c(id,date,metric2)) %>%
  filter(metric == "meandep") %>%
  group_by(month) %>%
  kruskal_test(values ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         metric2 = "Mean depth")

stat_maxdep = pivot %>%
  dplyr::select(-c(id,date,metric2)) %>%
  filter(metric == "maxdep") %>%
  group_by(month) %>%
  kruskal_test(values ~ day_night) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  mutate(day_night = "day",
         metric2 = "Maximum depth")

stat = bind_rows(stat_meandep, stat_maxdep)
stat$metric2 = factor(stat$metric2, levels = c("Mean depth",
                                               "Maximum depth"))
pivot$metric2 = factor(pivot$metric2, levels = c("Mean depth",
                                                 "Maximum depth"))


#-------------------------------------------------------------
# Kolmogorov-Smirnov test to compare day/night distributions
#-------------------------------------------------------------
## Jul ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Jul"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Jul"]
ks.test(day, night) # p=0.3: the 2 datasets do come from the same distribution

## Aug ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Aug"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Aug"]
ks.test(day, night) # p=0.01: the 2 datasets do come from the same distribution

## Sep ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Sep"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Sep"]
ks.test(day, night) # p<0.0001: the 2 datasets do not come from the same distribution

## Oct ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Oct"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Oct"]
ks.test(day, night) # p=0.01: the 2 datasets do come from the same distribution

## Nov ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Nov"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Nov"]
ks.test(day, night) # p=0.1: the 2 datasets do from the same distribution

## Dec ##
day = daily$meandep[daily$day_night=="day" & daily$month=="Dec"]
night = daily$meandep[daily$day_night=="night" & daily$month=="Dec"]
ks.test(day, night) # p=0.5: the 2 datasets do from the same distribution







###############################
# plot mean depth
###############################

#--------------
# scales free
#--------------
text_day_meandep = text_day_mean[text_day_mean$metric2=="Mean depth",]
text_night_meandep = text_night_mean[text_night_mean$metric2=="Mean depth",]
mean_meandep = mean[mean$metric2 == "Mean depth",]

stat_meandep$y       = rep(0.004,6)
text_day_meandep$y   = rep(0.004,6)
text_night_meandep$y = rep(0.004,6)

# n daily mean depth per month and phase
#---------------------------------------
n_day = pivot %>%
  filter(day_night == "day") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = rep(0.004,6),
         day_night = "day") %>%
  ungroup()
n_night = pivot %>%
  filter(day_night == "night") %>%
  group_by(month) %>%
  summarise(n = paste0("n=",n())) %>%
  mutate(y = rep(0.004,6),
         day_night = "night") %>%
  ungroup()

library(scales)
a = ggplot(data = pivot,
       aes(x=values, fill=day_night, colour=day_night)) +
  geom_density(alpha=.6,lwd=0.2) +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(450, 0)) +
  scale_fill_manual(values = c("navajowhite1","#595b54")) +
  labs(y = "Density", x = "Daily mean depth (m)",
       title = "Dive depth", fill="") +
  facet_grid(~month, scales="free") +
  theme_bw() +
  geom_text(data=n_night,
            aes(x=15, y = y, label=n),
            size=2, colour = "#595b54") +
  geom_text(data=n_day,
            aes(x=0, y = y, label=n),
            size=2, colour = "navajowhite3") +
  geom_text(data=stat_meandep,
            aes(x=360, y = y,
                label=p.adj.signif),
            size=2, colour = "#595b54") +
  geom_text(data=text_day_meandep,
            aes(x=390, y = y, label=text),
            size=2, colour = "navajowhite3") +
  geom_text(data=text_night_meandep,
            aes(x=420, y = y, label=text),
            size=2, colour = "#595b54") +
  geom_vline(data = mean_meandep, aes(xintercept=val,
                                      colour = day_night),
             lwd=0.2, linetype = "dashed") +
  scale_colour_manual(values = c("navajowhite2","#595b54"),
                      guide="none") +
  theme(legend.position = c(0.08,0.55),
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0,
                                colour="black"),
        strip.text = element_text(size=9, colour = "white"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.4,0.2,0,0.1), "cm"), 
        panel.border = element_rect(linewidth = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing.y = unit(0.0, "lines"),
        panel.grid.minor = element_blank())


ggsave(filename=paste0("./PAPER/4.PRSB/SI.Fig.1_density.DailyDepth.LR.tzCorr.pdf"),
       width=7,height=3,units="in",dpi=400,family="ArialMT")







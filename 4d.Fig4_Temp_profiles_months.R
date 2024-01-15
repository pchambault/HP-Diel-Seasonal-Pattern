####################################################################
#######    FIG. 4: TEMPERATURE PROFILES ACROSS MONTHS ##############
####################################################################

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidyquant)
library(RColorBrewer) 
library(tidync)
library(data.table)



###############################################
# import temperature data at propoise's depth
###############################################
temp <- readRDS("./RDATA/1.temp_at_IndMaxdep_5HP.RDS")
names(temp)
temp$month = factor(temp$month, levels=c("Jul","Aug","Sep",
                                         "Oct","Nov","Dec"))
summary(temp$thetao).   # -1.1 to 8.7




############################################
# plot temperature profile across months
############################################
ggplot(temp, aes(y=thetao, x=maxdep)) +
  geom_point(colour="black", size = 1, stroke = 0, alpha=0.3) +
  geom_smooth(method = "gam", colour = "brown2", 
              fill = NA, linewidth = 0.6) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  scale_x_continuous(trans = "reverse", limits = c(380, 0)) +
  labs(y = "Water temperature (°C)", x = "Depth (m)") +
  facet_wrap(~month, ncol = 6) +
  theme_tq() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(0.0, "lines"))

ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/Fig.4.pdf"),
       width=190,height=110,units="mm",dpi=400,family="ArialMT")
ggsave(filename=paste0("./PAPER/5.SciRep/2.Review_Sep2023/Fig.4.png"),
       width=190,height=110,units="mm",dpi=400)






###############################################
# stats on temperature at depth
###############################################
summary(temp$thetao)  # -1.1 to 8.7 deg, mean=4.3
sd(temp$thetao)       # 1.8

temp %>%
  group_by(month) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao),
            min_temp  = min(thetao),
            max_temp  = max(thetao))

sum_temp = temp %>%
  group_by(id,month) %>%
  summarise(mean_temp = mean(thetao),
            sd_temp   = sd(thetao),
            min_temp  = min(thetao),
            max_temp  = max(thetao))
sum_temp




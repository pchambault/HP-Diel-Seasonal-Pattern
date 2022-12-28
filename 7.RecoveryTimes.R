##############################################
###### recovery times: post-dive interval ####
##############################################
# is there any post-dive recovery time that varies between deep and shallow dives


library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(data.table)




k = 5 # dive threshold

########################################
# load summary dataset (from 5 HR tags)
########################################
sum <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")
names(sum)

# reorder dataset
#--------------------
sum = sum[order(sum$start),]

# difference between end of dive and start of dive+1
#-----------------------------------------------------
sum2 = sum %>%
  group_by(id) %>%
  mutate(start_next = lead(start)) %>% # select the next dive start
  ungroup()
tail(sum2 %>% select(start, end, start_next))
tail(sum2 %>% filter(id=="27262b") %>% select(start, end, start_next))

sum2 = sum2 %>%
  group_by(id) %>%
  mutate(recovery = as.numeric(start_next - start),
         cut(maxdep, breaks = c(0, 10, 20, 50, 100, 365))) %>%
  ungroup()
summary(sum2$recovery) # max=13666 sec !! 5 NA=5 last dive of each ID


# identify classes of dive depth
#--------------------------------
summary(sum2$maxdep)
sum2$class = cut(sum2$maxdep, 
                 breaks = c(0, 10, 20, 50, 100, 365))
table(sum2$class)
summary(sum2$recovery)/60


# plot recovery vs depth class
#------------------------------
ggplot(sum2, aes(x = class, y = recovery/60, fill = class)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "PiYG") +
  labs(y = "Recovery time (min)", x = "Depth class (m)",
       title = paste0("Dive threshold at ",k,"m")) +
  ylim(0,10) +
  theme_tq() +
  theme(legend.position = "none")

# correlation between recovery and max depth
#--------------------------------------------
cor.test(sum2$maxdep, sum2$recovery) # 0.56











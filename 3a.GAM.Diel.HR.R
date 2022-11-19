################################
#### modelling  diel pattern ###
#### high resolution dataset ###
################################

library(mgcv)
library(ggcorrplot)
library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(viridis)
library(DT)
library(data.table)


#########################
# load dataset
#########################
data <- readRDS("./RDATA/1b.daylength_depth_HR_5ids.RDS")
data


  
############################################
# pairwise correlations
############################################
names(data)
cor_dat = data %>% 
  dplyr::select(-c(id, date, sunset, sunrise))
str(cor_dat)

# compute correlation
corr <- round(cor(cor_dat, use = "pairwise.complete.obs"), 1)

# replace NA value by 0
corr[is.na(corr)] <- 0

# compute p_values
corr_p <- cor_pmat(cor_dat)

# replace NA value by 0
corr_p[is.na(corr_p)] <- 1

cor.test(cor_dat$mean_dep, cor_dat$daylength)
cor.test(cor_dat$median_dep, cor_dat$daylength)

# display
ggcorrplot(
  corr,
  # p.mat = p.mat,
  hc.order = TRUE,
  method = "square",
  type = "lower",
  lab = TRUE,
  sig.level = 0.05
)







#################################
# GAM: meandep vs day length
#################################
data$id = as.factor(data$id)
system.time({ m = gam(mean_dep ~ s(daylength, k=5) +   #s(day_depart, k=5) +
                        s(id, bs = 're') +             # random intercept
                        s(daylength, id, bs = 're'),   # random slope
                      data = data, method="REML") })
summary(m)  # dev: 80%
plot(m,pages=1, shade=T)
as.numeric(performance::r2(m)) # 0.79
saveRDS(m, "./RDATA/4.GAM/GAM_HR_output_daylength_meandep_slope_intercept.rds")

par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")



# predictions at individual level
#----------------------------------
ind_pred_inter <- expand.grid(daylength = as.numeric(0:max(data$daylength)),
                              id        = unique(data$id))
head(ind_pred_inter)

# add group_to_compare column
ind_pred <- ind_pred_inter %>%
  # set as data.table
  setDT(.) %>% 
  # sort by group_to_compare, time and id
  .[order(daylength, id),] %>%
  # add individual prediction
  .[, fit_ind := predict.gam(m,
                             .SD,
                             type = "response")] %>%
  # add individual SE
  .[, se_ind := predict.gam(m,
                            .SD,
                            se.fit=TRUE,
                            type = "response")$se.fit] %>%
  # sort by group_to_compare, time and id
  .[order(daylength, id),] %>%
  # trick to avoid calling twice this object in the console for display
  .[]
ind_pred
saveRDS(ind_pred, "./RDATA/4.GAM/Indiv-GAM_HR_daylength_meandep_slope_intercept.rds")

ggplot(ind_pred, aes(x = daylength, y = fit_ind)) +
  geom_line(aes(colour=id),lwd=1) +
  geom_ribbon(aes(ymin=fit_ind-se_ind, 
                  ymax=fit_ind+se_ind, fill=id), alpha=0.2) +
  labs(y = "Mean depth (m)", x = "Day length (hours)", title="") +
  theme_tq()


#----------------------------------
# predictions at population level
#----------------------------------
pop_pred <- setDT(
  expand.grid(
    daylength = as.numeric(0:max(data$daylength)),
    id = "population_level",
    stringsAsFactors = FALSE
  )
) %>%
  # sort by group_to_compare, time and id
  .[order(daylength, id),] %>%
  # retrieve population prediction
  .[, fit_pop := predict.gam(m,
                             # select the first individual by group_to_compare
                             ind_pred[, .SD[id == first(id)]],
                             type = "response",
                             exclude = c("s(id)",
                                         "s(daylength,id)"))] %>%
  # add standard error at the population level
  .[, fit_pop_se := predict.gam(
    m,
    # select the first individual by group_to_compare
    ind_pred[, .SD[id == first(id)]],
    type = "response",
    exclude = c("s(id)",
                "s(daylength,id)"),
    se.fit = T
  )$se.fit] %>%
  # trick to avoid calling twice this object in the console for display
  .[]
pop_pred = as_tibble(pop_pred)
saveRDS(pop_pred, "./RDATA/4.GAM/Pop-GAM_HR_daylength_meandep_slope_intercept.rds")


# plot pop curve + SE
#----------------------
ggplot(pop_pred, aes(x = daylength, y = fit_pop)) +
  geom_ribbon(aes(ymin=fit_pop-fit_pop_se,ymax=fit_pop+fit_pop_se),
              alpha=0.3, fill="mediumseagreen") +
  geom_line(colour="mediumseagreen",lwd=1) +
  geom_line(data=ind_pred, aes(daylength, fit_ind, group=id), 
            colour="mediumseagreen", lwd=0.5, alpha=0.4) +
  labs(y = "Mean depth (m)", x = "Day length (hours)") +
  theme_tq()

# # density plots
# fig_dens <-
#   ggplot(daylight, aes(y = daylight)) +
#   geom_density(show.legend = F, col = "black", 
#                fill="deepskyblue3", alpha = 0.4) +
#   # coord_cartesian(ylim = driftrate_limits) +
#   # scale_fill_manual(values = colours) +
#   theme_void()
# 
# (gam_plot | fig_dens) + plot_layout(widths = c(5, 1))








# ########################################################
# # GAM: interaction day_depart * hour, y=maxdep
# ########################################################
# data$id = as.factor(data$id)
# system.time({ m = gam(depth ~ s(hour) + s(day_depart) + ti(day_depart, hour) +
#                         s(id, bs = 're'),           # random intercept
#                       data = data, method="REML") }) # 5 sec
# summary(m)  # dev= 32%
# plot(m, scheme = 1)
# plot(m, scheme = 2)
# plot(m, pages=1, shade=T)
# vis.gam(x = m,                # GAM object
#         view = c("day_depart", "hour"),   # variables
#         plot.type = "persp",
#         theta = -50)   
# 
# saveRDS(m, "./RDATA/4.GAM/GAM_output_interaction_hour-dayDepart_meandep.rds")
# 
# # check residuals:
# # better fit without interaction!
# #------------------------------------
# par(mfrow=c(2,2),mar=c(6,5,6,3))
# qq.gam(m,cex=1,pch=20)
# hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
# observed.y <- napredict(m$na.action, m$y)
# plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
#      ylab = "Response", main = "Response vs. Fitted Values")
# acf(resid(m),lag.max=200, main="Autocorrelation")
# 
# 
# #----------------
# # predictions
# #----------------
# newdat <- expand_grid(
#   hour = seq(from=min(data$hour), 
#              to=max(data$hour), by = 1),
#   day_depart = seq(from=min(data$day_depart), 
#                    to=max(data$day_depart), by = 1),
#   id = "27262b"
# )
# 
# pred <- predict(m, newdata = newdat, se.fit = TRUE, type="response") %>%  
#   as_tibble() %>% 
#   cbind(newdat) %>% 
#   as_tibble()
# pred
# summary(pred)
# 
# ggplot(pred, aes(x=day_depart, y=hour)) + 
#   geom_tile(aes(fill = fit)) +
#   geom_contour(aes(z = fit), colour = "white") +
#   # scale_fill_viridis() +
#   scale_fill_gradient(low="white", high="blue") +
#   # scale_fill_distiller(palette = "RdPu") +
#   labs(x = "Days since departure", 
#        y = "Time of the day (UTC-2)", 
#        fill = "Mean depth (m)") +
#   theme_tq() +
#   theme(legend.position = "bottom")
# 
# summary(pred$fit)
# pred$class_dep = cut(pred$fit, 
#                      breaks=c(0,10,20,50,110))
# unique(pred$class_dep)
# 
# ggplot(pred, aes(x = day_depart, y = hour)) +
#   geom_tile(aes(fill = class_dep)) + 
#   scale_y_continuous(labels=c("00:00","04:00","08:00",
#                               "12:00","16:00","20:00"),
#                      breaks=c(0,4,8,12,16,20)) +
#   scale_fill_brewer(palette = "Purples") + 
#   # scale_fill_brewer(palette = "Blues") + # BuPu
#   theme(legend.position = c("bottom")) +
#   theme_tq() +
#   labs(x = "Days since departure", 
#        y = "Time of the day (UTC-2)", 
#        fill = "Median depth (m)") 
# 
# # ggplot(pred, aes(x = day_depart, y = fit)) +
# #   geom_line(lwd=1) +
# #   # geom_ribbon(aes(ymin=fit-se.fit, 
# #   #                 ymax=fit+se.fit), alpha=0.2) +
# #   labs(y = "Mean depth", x = "Days since departure")
# # 
# # ggplot(pred, aes(x = hour, y = fit)) +
# #   geom_line(lwd=1) +
# #   labs(y = "Mean depth", x = "Hours")
# 
# 
# 
# 


########################################################
# GAMM: interaction month * hour, y=maxdep
########################################################
dataPlot2$id = as.factor(dataPlot2$id)
dataPlot2$month = as.factor(dataPlot2$month)
system.time({ m2 = gamm(depth ~ month + s(hour, by=month) + s(day_depart),
                        correlation=corAR1(),
                        random=list(id=~1), data = dataPlot2, method="REML") }) # 532 sec

summary(m2$gam)  # R2=35%
saveRDS(m2, "./RDATA/4.GAM/GAMM_output_interaction_hour-month_meandep.rds")

plot(m2$gam, pages=1, shade=T)
vis.gam(x = m,                # GAM object
        view = c("month", "hour"),   # variables
        plot.type = "persp",
        theta = -50)   

par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m2$gam,cex=1,pch=20)
hist(residuals(m2$gam), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m2$gam$na.action, m2$gam$y)
plot(fitted(m2$gam), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m2$gam),lag.max=200, main="Autocorrelation")


# predictions
#----------------
newdat <- expand_grid(
  hour = seq(from=min(dataPlot2$hour), 
             to=max(dataPlot2$hour), by = 1),
  month = c("07","08","09","10","11","12"),
  day_depart = seq(from=min(dataPlot2$day_depart), 
                   to=max(dataPlot2$day_depart), by = 1),
  id = "22850b"
)

pred <- predict(m2$gam, newdata = newdat, se.fit = TRUE, type="response") %>%  
  as_tibble() %>% 
  cbind(newdat) %>% 
  as_tibble()
pred
summary(pred)

ggplot(pred, aes(x=month, y=hour)) + 
  geom_tile(aes(fill = fit)) +
  geom_contour(aes(z = fit), colour = "white") +
  scale_fill_gradient(low="white", high="blue") +
  labs(x = "Days since departure", 
       y = "Time of the day (UTC-2)", 
       fill = "Mean depth (m)") +
  theme_tq() +
  theme(legend.position = "bottom")




summary(pred$fit)
pred$class_dep = cut(pred$fit, 
                     breaks=c(0,10,20,50,110))
unique(pred$class_dep)

ggplot(pred, aes(x = month, y = hour)) +
  geom_tile(aes(fill = class_dep)) + 
  scale_y_continuous(labels=c("00:00","04:00","08:00",
                              "12:00","16:00","20:00"),
                     breaks=c(0,4,8,12,16,20)) +
  scale_fill_brewer(palette = "Purples") + 
  # scale_fill_brewer(palette = "Blues") + # BuPu
  theme(legend.position = c("bottom")) +
  theme_tq() +
  labs(x = "Days since departure", 
       y = "Time of the day (UTC-2)", 
       fill = "Median depth (m)") 

# ggplot(pred, aes(x = day_depart, y = fit)) +
#   geom_line(lwd=1) +
#   # geom_ribbon(aes(ymin=fit-se.fit, 
#   #                 ymax=fit+se.fit), alpha=0.2) +
#   labs(y = "Mean depth", x = "Days since departure")
# 
# ggplot(pred, aes(x = hour, y = fit)) +
#   geom_line(lwd=1) +
#   labs(y = "Mean depth", x = "Hours")











#################################
# GAMM with gamm4
#################################
library(gamm4)
system.time({ m = gamm4(meandep ~ s(daylight), data = daylight, # 1 sec
                       random=~(1+daylight|id)) })
plot(m$gam,pages=1, shade=T)
summary(m$gam) # R2=0.42, ***

par(mfrow=c(2,2),mar=c(6,5,6,3)) # still very good fit!
qq.gam(m$gam,cex=1,pch=20)
hist(residuals(m$gam), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$gam$na.action, m$gam$y)
plot(fitted(m$gam), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m$gam),lag.max=200, main="Autocorrelation")



############
# GAMM
############
system.time({ m = gamm(meandep ~ s(daylight), data = daylight, # 0.75 sec
                       random=list(id=~1+daylight), correlation=corAR1()) })
                       # random=list(id=~1), correlation=corAR1()) })
plot(m$gam,pages=1, shade=T)
summary(m$gam) # R2= 0.42: similar to gamm from gamm4

par(mfrow=c(2,2),mar=c(6,5,6,3)) # still very good fit!
qq.gam(m$gam,cex=1,pch=20)
hist(residuals(m$gam), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$gam$na.action, m$gam$y)
plot(fitted(m$gam), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m$gam),lag.max=200, main="Autocorrelation")

# check autocor
#---------------
pacf(resid(m$lme, type = "normalized"))
acf(resid(m$gam),lag.max=200, main="Autocorrelation")
pacf(resid(m$lme, type = "normalized")[daylight$id == "22849b"])
pacf(resid(m$lme, type = "normalized")[daylight$id == "22850b"])
pacf(resid(m$lme, type = "normalized")[daylight$id == "27262"])
pacf(resid(m$lme, type = "normalized")[daylight$id == "27262b"])
pacf(resid(m$lme, type = "normalized")[daylight$id == "93100"])

# m2 <- gamm(meandep ~ s(daylight), data = daylight, 
#            correlation = corCAR1(value = 0.5, form = ~ daylight | id))
# summary(m2$lme)


system.time({ m = gamm(meandep ~ s(daylight) + s(day_depart) + period, data = daylight3,
                       random=list(id=~1)) }) # 0.14 sec
plot(m$gam,pages=1, shade=T)
summary(m$gam)

par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m$gam,cex=1,pch=20)
hist(residuals(m$gam), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$gam$na.action, m$gam$y)
plot(fitted(m$gam), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m$gam),lag.max=200, main="Autocorrelation")




######################
# lme
######################
library(lme4)
daylight3$id = as.factor(daylight3$id)
daylight3$period = as.factor(daylight3$period)
m <- lme(meandep ~ daylight + day_depart + period,
         random = ~1|id, data = daylight3)
summary(m)

par(mfrow=c(2,2),mar=c(6,5,6,3))
qqnorm(resid(m))
qqline(resid(m))
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")


library(sjPlot)
library(sjlabelled)
library(sjmisc)
plot_model(m, sort.est = TRUE)
plot_model(m, transform = NULL)
plot_model(m, show.values = TRUE, value.offset = .3)
plot_model(
  m, 
  colors = "Accent", 
  show.values = TRUE,
  value.offset = .4,
  value.size = 4,
  dot.size = 3,
  line.size = 1.5,
  vline.color = "blue",
  width = 1.5
)


#-----------------------
# lmer
#-----------------------
# library(effects)
library(lme4)
m <- lmer(meandep ~ daylight + day_depart + period + (1|id),
          data = daylight3)
summary(m)
plot_model(m, show.values = TRUE, value.offset = .3)
plot_model(m, type="eff", terms="daylight")
plot_model(m, type="eff", terms="day_depart")




# how many deep dives/day? increase through winter?
# density plot with period and month







###################################################
# months vs hour (fill=depth): all ids combined
###################################################

#---------------------
dataPlot = dat %>% 
  setDT(.) %>% 
  .[,.(period = first(period)), 
    by=.(id, day_depart, date, hour)]
dataPlot$date2 = as.Date(paste0("2022",substr(dataPlot$date,5,10)))
dataPlot = dataPlot %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01", day_depart != 1)
dataPlot = dataPlot[order(dataPlot$id, dataPlot$date),]
dataPlot

dataPlot = dataPlot %>%
  mutate(day_night = case_when(period == "day"  ~ "night",
                               period == "dusk" ~ "night",
                               period == "dawn" ~ "day",
                               period == "night"~ "night"),
         day_night = as.factor(day_night),
         id        = as.factor(id),
         month     = as.factor(month))


# identify day start and end
#-----------------------------
start_end = dataPlot %>%
  dplyr::group_by(id, date2, month) %>%
  dplyr::summarise(day_start = min(hour[day_night=="day"]),
                   day_end   = max(hour[day_night=="day"])) %>%
  ungroup()

start = start_end %>%
  dplyr::select(id, date2, day_start) %>%
  dplyr::rename("hour" = "day_start")
end = start_end %>%
  dplyr::select(id, date2, day_end) %>%
  dplyr::rename("hour" = "day_end")

# check data gaps
start = start %>%
  group_by(id, date2) %>%
  mutate(diff_time = c(NA, diff(hour)))
start


# calculate mediandep/id/day/hour
#--------------------------------
dataPlot2 = dat %>% 
  setDT(.) %>% 
  .[,.(depth = median(depth)), 
    by=.(id, day_depart, date, hour)]
dataPlot2$date2 = as.Date(paste0("2022",substr(dataPlot2$date,5,10)))
dataPlot2 = dataPlot2 %>% 
  mutate(month = substr(date2, 6, 7)) %>%
  filter(month!="01")
dataPlot2

dataPlot3 = dataPlot2 %>%
  dplyr::group_by(date2, hour) %>%
  dplyr::summarise(maxdep    = max(depth),
                   meandep   = mean(depth),
                   mediandep = median(depth)) %>%
  ungroup()
dataPlot3

# generate depth classes
#--------------------------
summary(dataPlot3$meandep)
dataPlot3$class_meandep = cut(dataPlot3$meandep, 
                              breaks=c(seq(0,80, by=20),110))
unique(dataPlot3$class_meandep)

dataPlot3$class_mediandep = cut(dataPlot3$mediandep, 
                                breaks=c(seq(0,80, by=20),110))
unique(dataPlot3$class_mediandep)

dataPlot3$class_maxdep = cut(dataPlot3$maxdep, 
                             breaks=c(seq(0,80, by=20),110))
unique(dataPlot3$class_maxdep)


# plots
#---------
ggplot(dataPlot3, aes(x = date2, y = hour)) +
  geom_tile(aes(fill = class_mediandep)) + 
  # scale_fill_viridis_c(option = "magma", direction = -1) +
  # scale_fill_distiller(palette = "PuBuGn", direction = 1) +
  # scale_fill_distiller(palette = "BuPu", direction = 1) +
  scale_fill_brewer(palette = "Blues") + # BuPu
  geom_point(data=start, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  geom_point(data=end, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  theme_tq() +
  theme(legend.position = c("right")) +
  labs(x = "", y = "Hour (UTC-2)", fill = "Median \ndepth (m)") 

ggplot(dataPlot3, aes(x = date2, y = hour)) +
  geom_tile(aes(fill = class_meandep)) + 
  scale_fill_brewer(palette = "Blues") + # BuPu
  geom_point(data=start, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  geom_point(data=end, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  theme_tq() +
  theme(legend.position = c("right")) +
  labs(x = "", y = "Hour (UTC-2)", fill = "Mean \ndepth (m)") 

ggplot(dataPlot3, aes(x = date2, y = hour)) +
  geom_tile(aes(fill = class_maxdep)) + 
  scale_fill_brewer(palette = "Blues") + # BuPu
  geom_point(data=start, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  geom_point(data=end, aes(x = date2, y = hour),
             size=0.5, colour="black") +
  theme_tq() +
  theme(legend.position = c("right")) +
  labs(x = "", y = "Hour (UTC-2)", fill = "Max \ndepth (m)") 









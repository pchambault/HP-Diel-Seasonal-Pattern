################################
#### modelling  diel pattern ###
####    LR + HR datasets     ###
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
library(GGally)



#########################
# load LR dataset
#########################
lr <- readRDS("./RDATA/1c.daylength_depth_LR_19ids.RDS")
lr$month = as.numeric(substr(lr$date, 6, 7))
unique(lr$id) # 19 ids
unique(lr$month)

# select individuals with long tracking dataset 
# at least 2 months, not only covering summer
#------------------------------------------------
lr %>%
  group_by(id) %>%
  summarise(start    = first(date),
            end      = last(date),
            duration = round(difftime(last(date), first(date), units="days")),
            ndives   = n()) 
lr = lr %>% 
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
unique(lr$id) # 8 ids
summary(lr)
unique(lr$month)




#########################
# load HR dataset
#########################
hr <- readRDS("./RDATA/1b.daylength_depth_HR_5ids.RDS") %>%
  dplyr::select(-c(sunrise, sunset)) %>%
  rename(date = date2) %>%
  mutate(month = as.numeric(substr(date, 6, 7))) %>%
  filter(month != "Jan")
unique(hr$id)  # 5 ids
names(lr)
names(hr)
hr = hr %>% relocate(date, .after="daylength")

dat = rbind(lr, hr)
length(unique(dat$id))            # 13 ids
summary(dat$daylength)
dat = dat[!is.na(dat$daylength),] # 1471 obs
table(dat$id)
summary(dat)





############################################
# pairwise correlations
############################################
names(dat)
cor_dat = dat %>% 
  dplyr::select(-c(id,date))
corr <- round(cor(cor_dat, use = "pairwise.complete.obs"), 1)
corr[is.na(corr)] <- 0
corr_p <- cor_pmat(cor_dat)
corr_p[is.na(corr_p)] <- 1

cor.test(dat$mean_dep, dat$daylength)   # -0.48
cor.test(dat$median_dep, dat$daylength) # -0.56
cor.test(dat$max_dep, dat$daylength)    # -0.31
cor.test(dat$mean_dep, dat$month)       # 0.48

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






###########################################
# pairwise colinearity
###########################################
# Create data 
data <- dat %>% dplyr::select(-c(id, date)) 

# Function to return points and geom_smooth
# allow for the method to be changed
my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 0.3) + 
    geom_smooth(method=method, ..., size = 0.4)
  p
}

# plot scatterplots, distribution and print correlation coefficient 
ggpairs(data, title="Pairwise correlations between variables", 
        lower = list(continuous = my_fn), 
        upper = list(continuous = wrap("cor", size = 2))) +
  theme_tq()








#################################
# GAM: depth vs day length
#################################
dat$id = as.factor(dat$id)
system.time({ m = gam(max_dep ~ s(daylength, k=4) + 
                        s(id, bs = 're') +            # random intercept
                        s(daylength, id, bs = 're'),  # random slope
                      data = dat, method="REML") })
summary(m)   # meandep: 64%, mediandep: 53%, maxdep: 40%
plot(m,pages=1, shade=T)
as.numeric(performance::r2(m)) 
# saveRDS(m, "./RDATA/3.GAM/GAM_output_daylength_meandep_slope_intercept_all.rds")
saveRDS(m, "./RDATA/3.GAM/GAM_output_daylength_maxdep_slope_intercept_all.rds")
# saveRDS(m, "./RDATA/3.GAM/GAM_output_daylength_mediandep_slope_intercept_all.rds")

# residuals: better fit with mediandep
# good fit but some spatial autocorrelation!
#---------------------------------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")



# predictions at individual level
#----------------------------------
ind_pred_inter <- expand.grid(daylength = as.numeric(0:max(dat$daylength,na.rm=T)),
                              id       = unique(dat$id))
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
# saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_daylength_meandep_slope_intercept_all.rds")
saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_daylength_maxdep_slope_intercept_all.rds")
# saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_daylength_mediandep_slope_intercept_all.rds")

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
    daylength = as.numeric(0:max(dat$daylength,na.rm=T)),
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
# saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_daylength_meandep_slope_intercept_All.rds")
saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_daylength_maxdep_slope_intercept_All.rds")
# saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_daylength_mediandep_slope_intercept_All.rds")


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











#########################
# GAMM
#########################
system.time({  # 4 sec
  m2 = gamm(mediandep~s(daylength,bs="ts"), 
            data = data.frame(dat),
            correlation=corAR1(form=~1|id), 
            method ="REML",
            random = list(id=~1, id=~0+daylength))
})
summary(m2$gam) # R2:0.47 for meandep, 0.13 for mediandep
plot(m2$gam, pages=1, shade=T)

# check residuals:
#------------------------------------
gam.check(m2$gam) #
## Raw residuals still show correlation, of course...
acf(residuals(m2$gam),main="raw residual ACF")
## But standardized are now fine
acf(residuals(m2$lme,type="normalized"),main="standardized residual ACF")

par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m2$gam,cex=1,pch=20)
hist(residuals(m2$gam), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m2$gam$na.action, m$y)
plot(fitted(m2$gam), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(residuals(m2$lme,type="normalized"),main="standardized residual ACF")




# ####################
# # lmm
# ####################
# library(sjPlot)
# library(sjlabelled)
# library(sjmisc)
# library(lme4)
# m = lmer(meandep ~ daylength + (1 | id), data = dat, REML = TRUE)
# m = lme(meandep ~ daylength, 
#         random = ~ 1|id, 
#         data = data.frame(dat),
#         method = "REML",
#         correlation = corAR1(form = ~ 1 | id))
# summary(m) # 
# 
# # diagnostic plot
# #----------------------------------------
# par(mfrow=c(2,2),mar=c(6,5,6,3))
# qqnorm(resid(m), pch=19)
# qqline(resid(m))
# hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
# observed.y <- napredict(m$na.action, m$y)
# plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
#      ylab = "Response", main = "Response vs. Fitted Values")
# acf(resid(m),lag.max=200, main="Autocorrelation")
# 
# # plot response curves
# #------------------------
# plot_model(m, sort.est = TRUE, show.values = TRUE)
# plot_model(m, type="pred", terms=c("daylength"))
# 
# 
# 
# 
# 
# 
# 
# #########################
# # GAMM
# #########################
# library(gamm4)
# system.time({  # 2 sec
#   m2 = gamm(meandep~s(daylength,bs="ts"), 
#             data = data.frame(data),
#             correlation=corAR1(form=~1|id), 
#             method ="REML",
#             random= list(id=~1, id=~0+daylength))
# })
# summary(m2$gam) # R2:0.66
# plot(m2$gam, pages=1, shade=T)
# 
# # check residuals:
# #------------------------------------
# gam.check(m2$gam) #
# ## Raw residuals still show correlation, of course...
# acf(residuals(m2$gam),main="raw residual ACF")
# ## But standardized are now fine
# acf(residuals(m2$lme,type="normalized"),main="standardized residual ACF")
# 
# par(mfrow=c(2,2),mar=c(6,5,6,3))
# qq.gam(m2$gam,cex=1,pch=20)
# hist(residuals(m2$gam), xlab="Residuals", main="Histogram of residuals")
# observed.y <- napredict(m2$gam$na.action, m$y)
# plot(fitted(m2$gam), observed.y, pch=20, xlab = "Fitted Values",
#      ylab = "Response", main = "Response vs. Fitted Values")
# acf(residuals(m2$lme,type="normalized"),main="standardized residual ACF")
# 
# 

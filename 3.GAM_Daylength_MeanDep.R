##########################################################
#### Modelling  diel pattern: mean_depth vs daylength  ####
####              LR + HR datasets                    ####
####         after timezone correction                ####
##########################################################

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


# changes made compared to previous version (before June 2023):
#--------------------------------------------------------------
# - retained only months from Jun to Dec for LR dataset
# - removed from HR dataset id 27262 that was tracked for less than 1 month
# - recalculated mean_dep for HR dataset: previously was mean(mean_dep) instead of mean(max_dep)
# - obtained higher deviance but weird residuals
# - faced some trouble with the GAMs using dplyr::summarize because the names 
#   of new column (meandep) was similar to the column itself so had to rename it (mean_dep)
# - now fixed after renaming columns when calculating the daily values for daylength object in script 0b




#########################
# load LR dataset
#########################
lr <- readRDS("./RDATA/0c.daylength_depth_LR_19ids_tzCorrected.RDS") %>%
  mutate(month = format(date, "%b")) 
unique(lr$id) # 19 ids
unique(lr$month)

# select individuals with long tracking dataset 
# at least 1 month, not only covering summer
#------------------------------------------------
lr %>%
  group_by(id) %>%
  summarise(start    = first(date),
            end      = last(date),
            duration = round(difftime(last(date), first(date), units="days")),
            ndives   = n()) 
lr = lr %>% 
  filter(id != "37235",id != "22849b", id != "22850b", # remove HR tags
         id != "27262", id != "27262b", id != "93100", # remove HR tags
         id != "22849", id != "22853",
         id != "37227", id != "37228", id != "27261",  # according to geom_smooth
         id != "7617", id != "7618") %>%
  select(c(id,date,mean_dep,max_dep,daylength,month,sunrise,sunset)) 
         
unique(lr$id) # 6 ids
summary(lr)
table(lr$month)




#########################
# load HR dataset
#########################
hr <- readRDS("./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS") %>%
  mutate(month = format(date, "%b")) %>%
  filter(month != "Jan")  %>%
  select(c(id,date,mean_dep,max_dep,daylength,month,sunrise,sunset)) %>%
  filter(id != "27262")   # remove this id because tracked over only one month
hr %>%
  group_by(id) %>%
  summarise(start    = first(date),
            end      = last(date),
            duration = round(difftime(last(date), first(date), units="days")),
            ndives   = n()) 

unique(hr$id)                     # 4 ids
names(lr)
names(hr)
dat = rbind(lr, hr)
length(unique(dat$id))            # 10 ids
summary(dat$daylength)
dat = dat[!is.na(dat$daylength),] # 1193 obs
table(dat$id)
summary(dat)



#########################################################
# visualize relationship between daylength and mean_dep
#########################################################
ggplot(dat, aes(y=mean_dep, x=daylength)) +
  geom_point(size = 0.5) +
  geom_smooth() +
  facet_wrap(~id, ncol=5) +
  theme_tq()
unique(hr$id)
ggplot(dat, aes(y=mean_dep, x=daylength)) +
  geom_point(size = 0.5) +
  geom_smooth()





############################################
# pairwise correlations
############################################
cor.test(dat$mean_dep, dat$daylength)   # -0.59
cor.test(dat$max_dep, dat$daylength)    # -0.32

cor.test(hr$mean_dep, hr$daylength)   # -0.79
cor.test(hr$max_dep, hr$daylength)    # -0.40

cor.test(lr$mean_dep, lr$daylength)   # -0.53
cor.test(lr$max_dep, lr$daylength)    # -0.32


names(dat)
cor_dat = dat %>% 
  dplyr::select(-c(id,date,month,sunrise,sunset))
corr <- round(cor(cor_dat, use = "pairwise.complete.obs"), 1)
corr[is.na(corr)] <- 0
corr_p <- cor_pmat(cor_dat)
corr_p[is.na(corr_p)] <- 1

# display
ggcorrplot(
  corr,
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
data <- dat %>% dplyr::select(-c(id,date,month,sunrise,sunset)) 

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








#####################################
# GAM: mean of maxdepth vs daylength
#####################################
dat$id = as.factor(dat$id)
system.time({ m = gam(mean_dep ~ s(daylength, k=5) + 
                        s(id, bs = 're') +            # random intercept
                        s(daylength, id, bs = 're'),  # random slope
                      data = dat, method="REML") })
summary(m)                      # mean_dep: 63%
plot(m,pages=1, shade=T)
as.numeric(performance::r2(m))  # 0.61
saveRDS(m, "./RDATA/3.GAM/GAM_output_daylength_mean_dep_slope_intercept_all_tzCorr.rds")

# residuals: good fit 
#---------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")




# predictions at individual level
#----------------------------------
ind_pred_inter <- expand.grid(daylength = as.numeric(3:max(dat$daylength,na.rm=T)),
                              id        = unique(dat$id))
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
saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_daylength_mean_dep_slope_intercept_all_tzCorr.rds")

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
    daylength = as.numeric(3:max(dat$daylength,na.rm=T)),
    id = "population_level",
    stringsAsFactors = FALSE
  )
) %>%
  # sort by group_to_compare, time and id
  .[order(daylength, id),] %>%
  # retrieve population prediction
  .[, fit_pop := predict.gam(m,
                             # select the first individual by group_to_compare
                             ind_pred[, .SD[id == "22849b"]],
                             type = "response",
                             exclude = c("s(id)",
                                         "s(daylength,id)"))] %>%
  # add standard error at the population level
  .[, fit_pop_se := predict.gam(
    m,
    # select the first individual by group_to_compare
    ind_pred[, .SD[id == "22849b"]],
    type = "response",
    exclude = c("s(id)",
                "s(daylength,id)"),
    se.fit = T
  )$se.fit] %>%
  # trick to avoid calling twice this object in the console for display
  .[]
pop_pred = as_tibble(pop_pred)
saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_daylength_mean_dep_slope_intercept_All_tzCorr.rds")


# plot pop curve + SE
#----------------------
ggplot(pop_pred, aes(x = daylength, y = fit_pop)) +
  geom_ribbon(aes(ymin=fit_pop-fit_pop_se,ymax=fit_pop+fit_pop_se),
              alpha=0.3, fill="mediumseagreen") +
  geom_line(colour="mediumseagreen",lwd=1) +
  geom_line(data=ind_pred, aes(daylength, fit_ind, group=id), 
            colour="mediumseagreen", lwd=0.5, alpha=0.4) +
  ylim(0,120) +
  # geom_point(data=dat, aes(y=mean_dep, x=daylength), alpha=0.3, size=0.5) +
  labs(y = "Mean depth (m)", x = "Daylength (hours)") +
  theme_tq()









#################################
# GAM: max depth vs day length
#################################
system.time({ m = gam(max_dep ~ s(daylength, k=5) + 
                        s(id, bs = 're') +            # random intercept
                        s(daylength, id, bs = 're'),  # random slope
                      data = dat, method="REML") })
summary(m)   # max_dep: 37%
plot(m,pages=1, shade=T) 
saveRDS(m, "./RDATA/3.GAM/GAM_output_daylength_max_dep_slope_intercept_all_tzCorr.rds")

# residuals: ok fit but better with meandep
#-------------------------------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")



# predictions at individual level
#----------------------------------
ind_pred_inter <- expand.grid(daylength = as.numeric(3:max(dat$daylength,na.rm=T)),
                              id        = unique(dat$id))
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
saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_daylength_max_dep_slope_intercept_all_tzCorr.rds")

# plot: some negative depths for one animal!
ggplot(ind_pred, aes(x = daylength, y = fit_ind)) +
  geom_line(aes(colour=id),lwd=1) +
  # ylim(0,400) +
  geom_ribbon(aes(ymin=fit_ind-se_ind, 
                  ymax=fit_ind+se_ind, fill=id), alpha=0.2) +
  labs(y = "Mean depth (m)", x = "Day length (hours)", title="") +
  theme_tq()


#----------------------------------
# predictions at population level
#----------------------------------
pop_pred <- setDT(
  expand.grid(
    daylength = as.numeric(3:max(dat$daylength,na.rm=T)),
    id = "population_level",
    stringsAsFactors = FALSE
  )
) %>%
  # sort by group_to_compare, time and id
  .[order(daylength, id),] %>%
  # retrieve population prediction
  .[, fit_pop := predict.gam(m,
                             # select the first individual by group_to_compare
                             ind_pred[, .SD[id == "22849b"]],
                             type = "response",
                             exclude = c("s(id)",
                                         "s(daylength,id)"))] %>%
  # add standard error at the population level
  .[, fit_pop_se := predict.gam(
    m,
    # select the first individual by group_to_compare
    ind_pred[, .SD[id == "22849b"]],
    type = "response",
    exclude = c("s(id)",
                "s(daylength,id)"),
    se.fit = T
  )$se.fit] %>%
  # trick to avoid calling twice this object in the console for display
  .[]
pop_pred = as_tibble(pop_pred)
saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_daylength_max_dep_slope_intercept_All_tzCorr.rds")


# plot pop curve + SE
#----------------------
ggplot(pop_pred, aes(x = daylength, y = fit_pop)) +
  geom_ribbon(aes(ymin=fit_pop-fit_pop_se,ymax=fit_pop+fit_pop_se),
              alpha=0.3, fill="mediumseagreen") +
  ylim(0,400) +
  geom_line(colour="mediumseagreen",lwd=1) +
  geom_line(data=ind_pred, aes(daylength, fit_ind, group=id), 
            colour="mediumseagreen", lwd=0.5, alpha=0.4) +
  labs(y = "Mean depth (m)", x = "Day length (hours)") +
  theme_tq()




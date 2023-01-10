################################
#### modelling  diel pattern ###
####  low resolution dataset ###
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
daylength <- readRDS("./RDATA/1c.daylength_depth_LR_19ids.RDS")
daylength$month = as.numeric(substr(daylength$date, 6, 7))
unique(daylength$id) # 19 ids
daylength -> dat

# select individuals with long tracking dataset 
# at least 2 months, not only covering summer
#------------------------------------------------
dat %>%
  group_by(id) %>%
  dplyr::summarise(start    = first(date),
                   end      = last(date),
                   duration = round(difftime(last(date), first(date), units="days")),
                   ndives   = n()) 
dat = dat %>% 
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
dat$month = as.numeric(substr(dat$date, 6, 7))
unique(dat$id) # 8 remaining individuals




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

cor.test(daylength$mean_dep, daylength$daylength)
cor.test(daylength$median_dep, daylength$daylength)
cor.test(daylength$median_dep, daylength$month)

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
daylength$id = as.factor(daylength$id)
system.time({ m = gam(mean_dep ~ s(daylength, k=5) +  #s(day_depart, k=5) +
                        s(id, bs = 're') +            # random intercept
                        s(daylength, id, bs = 're'),  # random slope
                      data = daylength, method="REML") })
summary(m)                     # dev: 65%
plot(m,pages=1, shade=T)
as.numeric(performance::r2(m)) # 0.64
saveRDS(m, "./RDATA/3.GAM/GAM_LR_output_daylength_meandep_slope_intercept_8ids.rds")

# residuals: good fit
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
saveRDS(ind_pred, "./RDATA/3.GAM/Indiv-GAM_LR_daylength_meandep_slope_intercept_8ids.rds")

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
    daylength = as.numeric(0:max(daylength$daylength,na.rm=T)),
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
saveRDS(pop_pred, "./RDATA/3.GAM/Pop-GAM_LR_daylength_meandep_slope_intercept_8ids.rds")


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


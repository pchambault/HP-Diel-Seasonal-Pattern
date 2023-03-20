############################################
#### modelling salor elevation vs depth  ###
####          LR + HR datasets           ###
############################################


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
library(mgcViz)
library(rgl)


#########################
# HR dataset
#########################
hr <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") %>%
  select(c(id,dive,start,end,solar_alt,twilight,
           maxdep, meandep, dur, sunrise, sunset, period)) %>%
  mutate(month = format(start, "%b"),
         dur = dur / 60) %>%
  filter(month != "Jan")
unique(hr$id)  # 5 ids




############################################
# pairwise correlations
############################################
hr -> dat
names(dat)
cor_dat = dat %>% 
  dplyr::select(c(solar_alt, maxdep, meandep, dur))
corr <- round(cor(cor_dat, use = "pairwise.complete.obs"), 1)
corr[is.na(corr)] <- 0
corr_p <- cor_pmat(cor_dat)
corr_p[is.na(corr_p)] <- 1

cor.test(dat$meandep, dat$solar_alt)   # -0.37
cor.test(dat$maxdep, dat$solar_alt)    # -0.35

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








#####################################
# GAM: meandep and maxdep vs alt_deg
#####################################
dat$id    = as.factor(dat$id)
dat$month = as.factor(dat$month)
dat$month_num = as.numeric(substr(dat$start, 6, 7))
system.time({  # 26 sec
  m = gam(maxdep ~ s(solar_alt, k=4) +
            s(id, bs = 're') +          # random intercept
            s(solar_alt, id, bs = 're'),# random slope
          data = dat, method="REML")
})
summary(m)                     # meandep: 17%, maxdep: 15%
plot(m, pages=1, shade=T)

# saveRDS(m, "./RDATA/3.GAM/GAM_output_sunAltitude_meandep_intercept_HR.rds")
saveRDS(m, "./RDATA/3.GAM/GAM_output_sunAltitude_maxdep_intercept_HR.rds")

# residuals: bad fit for meandep
#-------------------------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")






#################################
# GAM: meandep vs alt_deg
#################################
dat$id    = as.factor(dat$id)
dat$month = as.factor(dat$month)
dat$month_num = as.numeric(substr(dat$start, 6, 7))
# system.time({  # 26 sec
#   m = gam(maxdep ~ s(solar_alt, k=4) + 
#             s(id, bs = 're') +          # random intercept
#             s(solar_alt, id, bs = 're'),# random slope
#           data = dat, method="REML") 
# })
system.time({  # 26 sec
  m = gam(maxdep ~ te(solar_alt,month_num) + twilight +
            s(id, bs = 're'),            # random intercept
          data = dat, method="REML") 
  })
summary(m)                     # meandep: %, maxdep: 18%
plot(m, pages=1, shade=T)

# https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html
b <- getViz(m)
plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
print(plot(b, allTerms = T), pages = 1) 
print(plot(b, select = 3), pages = 1) 
plotRGL(sm(b, 1), fix = c("z" = 0), residuals = TRUE)
check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

saveRDS(m, "./RDATA/4.GAM/GAM_output_sunAltitude_month_maxdep_intercept_HR.rds")

# residuals: bad fit for meandep
#-------------------------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")











##########################################
# plot dive profiles in July and Dec
# showing diel patterns
##########################################
hp <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") %>%
  mutate(date  = as.Date(start),
         month = format(date, "%b"),
         day   = as.numeric(format(date, "%e")))


id = hp %>% filter(id == "93100") 
id$month = factor(id$month, 
                  levels = c("Jul","Aug","Sep",
                             "Oct","Nov","Dec","Jan"))
ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour=period)) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), 
                   date_labels = "%d/%m", expand=c(0,0)) +
  scale_colour_manual(values = c("lightsalmon","navajowhite1",
                                 "#337882","dodgerblue3")) +
  theme_tq() +
  facet_wrap(~month, ncol=1, scales="free_x") +
  labs(y = "Maximum depth (m)", x = "", 
       title=paste0(unique(id$id)," (F)")) +
  theme(legend.position = "bottom",#c(0.05,0.9),
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
        strip.text = element_text(size=7, colour = "white",
                                  margin = margin(0.5,0.5,0.5,0.5, "mm")),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        plot.margin = unit(c(0.2,0.2,0.2,0.1), "cm"),
        panel.border = element_rect(size = 0.2),
        panel.spacing.x = unit(0.0, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






##################################################
# add bathy to plot: check if some benthic dives?
##################################################
library(raster)
bathy = raster("./ENV.DATA/GEBCO_Arctic2.nc")
bathy
bathy[bathy>=0] = NA
plot(bathy)
res(bathy)
hp$bathy = extract(bathy, hp[,c("lon","lat")])
summary(hp$bathy) # 8384 NA
hp$bathy = abs(hp$bathy)
hp = hp %>%
  mutate(diff_bathy = bathy - maxdep)
hist(hp$diff_bathy)

id = hp %>% 
  filter(id == "93100") %>%
  dplyr::select(id,start,dive,bathy,maxdep,month)
summary(id$bathy)
max_bathy = 400
id$month = factor(id$month, 
                  levels = c("Jul","Aug","Sep",
                             "Oct","Nov","Dec","Jan"))

ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep)) +
  # geom_line(aes(y=bathy, x=start), colour="red") +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(max_bathy,0),
                     breaks=c(50,100,200,300,600)) +
  geom_ribbon(data=id,
              aes(x=start,ymin=bathy,ymax=max_bathy),
              alpha=0.9,fill="grey") +
  geom_linerange(aes(ymin = 0, ymax = maxdep)) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), 
                   date_labels = "%d/%m", expand=c(0,0)) +
  scale_colour_manual(values = c("lightsalmon","navajowhite1",
                                 "#337882","dodgerblue3")) +
  theme_tq() +
  facet_wrap(~month, ncol=1, scales="free_x")




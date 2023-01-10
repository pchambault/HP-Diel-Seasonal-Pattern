########################################################
###### clustering to identify dive categories   ########
########################################################

library(FactoMineR)
library(factoextra)
library(missMDA)
library(ggfortify)
library(dplyr)
library(tidyquant)
library(ggplot2)
library(gridExtra)


######################################
# import data
######################################
dive <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS") %>%
  dplyr::select(id, dive, period, start, end, maxdep, meandep, 
         dur, des_rate, asc_rate, bot_dur)
names(dive)
dat = as.matrix(dive[,-c(1:5)]) # retain numeric variables

# center variables
dat = scale(dat, center = TRUE, scale = TRUE)
summary(dat)













###########################
# K-Means Cluster Analysis
###########################
k = 4

# impute missing values with PCA
#--------------------------------
res.comp <- imputePCA(dat, ncp = 2, graph = FALSE) 

# run cluster based on k clusters
#----------------------------------
system.time({  # 15 sec for k=4
  fit <- kmeans(na.omit(dat), centers = k,
              nstart = 50, iter.max = 15)     # k cluster solution
}) 
# fit <- kmeans(res.comp$completeObs, centers = k,
#               nstart = 50, iter.max = 15)   # k cluster solution
# 
# # get cluster means
# aggregate(res.comp$completeObs,by=list(fit$cluster),FUN=mean)
# 
# # find optimal number of clusters
# #---------------------------------
# fviz_nbclust(na.omit(dat), kmeans, method = "wss") # not for large dataset!


# visualize kmeans
#--------------------
fviz_cluster(kmeans(res.comp$completeObs, centers = k),
             geom = "point", data = res.comp$completeObs)

# append cluster assignment
#----------------------------
clust <- data.frame(na.omit(dive), "cluster" = fit$cluster) %>% 
  as_tibble() %>%
  mutate(cluster = as.factor(cluster))
names(clust)
clust = clust %>% arrange(id, start)
# clust$cluster = factor(clust$cluster, levels=c("5","2","4","1","3"))
# summary(clust)
# clust = clust %>% filter(bot_dur < 100)

clust = clust %>%
  mutate(month = format(start, "%b"),
         day_phase = case_when(period == "day"   ~ "day",
                               period == "night" ~ "night",
                               period == "dawn"  ~ "night",
                               period == "dusk"  ~ "night"))

saveRDS(clust, "./RDATA/4.Clustering/4.DiveCluster_5HP_4clusters.RDS")




#---------------------
# summary per cluster
#---------------------
## k= ##
clust %>%
  group_by(cluster) %>%
  summarise(maxdep    = mean(maxdep),            # in m
            meandep   = mean(meandep),           # in m
            dur       = mean(dur)/60,            # in min
            desc_rate = mean(des_rate, na.rm=T), # in m/s
            asc_rate  = mean(asc_rate, na.rm=T), # in m/s
            bot_dur   = mean(bot_dur, na.rm=T))  # in sec


# #---------------------------------------
# # find the optimal number of clusters
# # vector memory exhausted !!
# #---------------------------------------
# library(factoextra)
# library(cluster)
# library(NbClust)
# 
# # Elbow method
# fviz_nbclust(res.comp$completeObs, kmeans, method = "wss") +
#   geom_vline(xintercept = 2, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# # Silhouette method
# fviz_nbclust(res.comp$completeObs, kmeans, method = "silhouette")+
#   labs(subtitle = "Silhouette method")
# 
# # Gap statistic
# # nboot = 50 to keep the function speedy. 
# # recommended value: nboot= 500 for your analysis.
# # Use verbose = FALSE to hide computing progression.
# set.seed(123)
# fviz_nbclust(res.comp$completeObs, kmeans, nstart = 25,  
#              method = "gap_stat", nboot = 50)+
#   labs(subtitle = "Gap statistic method")










#######################################
# visually identify dive shape
# plot n dives for each dive type
#######################################
dive <- readRDS("./RDATA/1b.dive_5HP_calib_5m_zoc0.RDS")

# add column cluster to dive dataset (unsummarized)
#----------------------------------------------------
names(dive)
names(clust)
clust_select = clust %>% select(id, dive, cluster)
dive2  = dive %>%
  left_join(clust_select, by = c("dive","id")) %>%
  filter(!is.na(cluster))


# plot randomly 50 profiles per cluster over time
#---------------------------------------------------
# loop over each cluster 
system.time({  # 203 sec (4 min)
  for (i in unique(dive2$id)) { 
    id = dive2 %>% filter(id == i) %>%
      select(id, dive, depth, date, cluster)
    
    for (c in unique(dive2$cluster)) { 
      sub = id %>% filter(cluster == c) 
      n = sample(unique(sub$dive), 50, replace = T)
      
      # randomly select n dives
      for (j in n) { 
        select = sub %>% filter(dive == j)
        
        # plot dive profile over time
        ggplot(select, aes(y=-depth, x=date, colour=cluster)) +
          geom_line(linewidth=0.5) +
          ylim(-max(select$depth)-5,0) +
          theme_tq() +
          labs(y = "Maximum depth (m)", x = "Time",
               colour="Dive type", title = paste0("ID: ",select$id[1],", ",
                                                  substr(select$date[1],1,10),
                                                  ", Dive: #",unique(select$dive)),
               subtitle = paste0("start: ",substr(select$date[1],12,19),
                                 ", end: ",substr(select$date[nrow(select)],12,19)))
        
        ggsave(paste0("./FIGURES/Clustering/Dive_shape_4clusters/Cluster",c,
                      "/Profile_Cluster",c,"_",select$id[1],"_dive",
                      unique(select$dive),".png"),
               width=4.5,height=5,units="in",dpi=400)
      }
    }
  }
})
















##################
# Explo clusters
##################

# dive characteristics WITH outliers
#---------------------------------------
# x=reorder(cluster,-maxdep,na.rm = TRUE)
a = ggplot(clust, aes(x=cluster, y=-maxdep, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)",title="Max depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  # ylim(-365,0) +
  theme(legend.position = "none")

b = ggplot(clust, aes(x=cluster, y=-meandep, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)", title="Mean depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(-365,0) +
  theme(legend.position = "none")

c = ggplot(clust, aes(x=cluster, y=dur/60, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(min)", title="Dive duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

d = ggplot(clust, aes(x=cluster, y=des_rate, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Descent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

e = ggplot(clust, aes(x=cluster, y=asc_rate, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Ascent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

f = ggplot(clust, aes(x=cluster, y=bot_dur, fill=cluster)) +
  geom_boxplot() + 
  stat_summary(fun="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(sec)", title="Bottom duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

grid.arrange(a,b,c, d,e,f, ncol=3)

ggsave(paste0("./FIGURES/Clustering/Boxplot_dives_K-means_",
              k,"clusters.png"),
       grid.arrange(a,b,c,d,e,f, ncol=3),
       width=6,height=4.5,units="in",dpi=400)


# without outliers
#---------------------
a = ggplot(clust, aes(x=cluster,
                      y=-maxdep, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)",title="Max depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(-365,0) +
  theme(legend.position = "none")

b = ggplot(clust, aes(x=cluster, y=-meandep, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)", title="Mean depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(-365,0) +
  theme(legend.position = "none")

c = ggplot(clust, aes(x=cluster, y=dur/60, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(min)", title="Dive duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

d = ggplot(clust, aes(x=cluster, y=des_rate, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Descent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(0, 2) +
  theme(legend.position = "none")

e = ggplot(clust, aes(x=cluster, y=asc_rate, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Ascent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(0, 2) +
  theme(legend.position = "none")

f = ggplot(clust, aes(x=cluster, y=bot_dur, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) + 
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(sec)", title="Bottom duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

ggsave(paste0("./FIGURES/Clustering/Boxplot_dives_K-means_",
              k,"clusters_woOutliers.png"),
       grid.arrange(a,b,c,d,e,f, ncol=3),
       width=6,height=4.5,units="in",dpi=400)




#------------------------------
# maxdep vs cluster and period
#------------------------------
ggplot(clust, aes(x=reorder(cluster,-maxdep,na.rm = TRUE), 
                      y=-maxdep, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)",title="Max depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  facet_wrap(.~period, ncol=4) +
  ylim(-365,0) +
  theme(legend.position = "none")

ggplot(clust, aes(x=cluster, y=dur/60, fill=cluster)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(min)",title="Dive duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  facet_wrap(.~period, ncol=4) +
  theme(legend.position = "none")







#############################
# individual dive profiles
#############################

#-------------
i  = "93100"
id = clust %>% filter(id == i) 
id$month = factor(id$month, 
                  levels = c("Jul","Aug","Sep",
                             "Oct","Nov","Dec","Jan"))

ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour=cluster),
                 linewidth=0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values = c("lightsalmon","navajowhite1",
                                              "#337882","dodgerblue3")) +
  theme_tq() +
  facet_wrap(~month, ncol=1, scales="free_x") +
  labs(y = "Maximum depth (m)", x = "", 
       title=paste0(unique(id$id)," (F)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
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
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(paste0("./FIGURES/Clustering/K4/DivesProfile_K-means",
              k,"_",i,".png"),
       width=6,height=6,units="in",dpi=400)

# diel pattern over time
#-----------------------
ggplot(id, aes(y=maxdep, x=start)) +
  geom_linerange(aes(ymin = 0, ymax = maxdep, colour=day_phase),
                 linewidth=0.1) +
  scale_y_continuous(trans = "reverse", expand=c(0,0), 
                     limits=c(365,0),
                     breaks=c(50,100,200,300)) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), 
                   date_labels = "%d", expand=c(0,0)) +
  scale_colour_manual(values = c("navajowhite1","#337882")) +
  theme_tq() +
  facet_wrap(~month, ncol=1, scales="free_x") +
  labs(y = "Maximum depth (m)", x = "", 
       title=paste0(unique(id$id)," (F)")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.8, 'cm'),     #change legend key size
        legend.key.height = unit(0.3, 'cm'),   #change legend key height
        legend.key.width = unit(0.3, 'cm'),    #change legend key width
        legend.text = element_text(size=8),    #change legend text font size
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size=6, vjust = 0.5, hjust=0.5),
        axis.text.y  = element_text(size=5, hjust=0.5),
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
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3) ) )

ggsave(paste0("./FIGURES/Clustering/K4/DivesProfile_K-means",
              k,"_dayNight_",i,".png"),
       width=6,height=6,units="in",dpi=400)












# #########
# # PCA 
# #########
# system.time({ res.pca  <- PCA(res.comp$completeObs) })         # 71 sec
# gc()
# summary(res.pca)
# pca = get_pca(res.pca, element = c("var", "ind"))
# pca = get_pca_var(res.pca)
# head(pca)
# 
# 
#   
# 
# ############     
# ## HCPC: not made for large datasets!
# does not work individually neither!
# ############
# dat = dive %>% select(id, maxdep, meandep, dur,
#                       des_rate, asc_rate, bot_dur)
# dat = na.omit(dat)
# dat = dat %>% filter(bot_dur < 150) %>%
#   mutate(dur = dur / 60)
# sample_dive = sample_frac(dat, 0.2) # retain 20% of dataset
# 
# unique(dive$id)
# dat = dive %>% filter(id == "22849b") %>% select(-c(id,period))
# dat = na.omit(dat)
# system.time({ res.hcpc <- HCPC(sample_dive, graph = FALSE) }) #
#  
# # visualize the dendrogram
# fviz_dend(res.hcpc,
#           cex = 0.7,                     # Taille du text
#           palette = "jco",               # Palette de couleur ?ggpubr::ggpar
#           rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
#           rect_border = "jco",           # Couleur du rectangle
#           labels_track_height = 0.8)     # Augment l'espace pour le texte
# 
# 
# # visualize the clusters
# fviz_cluster(res.hcpc,
#              repel = TRUE,            # Evite le chevauchement des textes
#              show.clust.cent = TRUE,  # Montre le centre des clusters
#              palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
#              ggtheme = theme_minimal(),
#              main = "Factor map")
# 
# # Principal components + tree
# plot(res.hcpc, choice = "3D.map")
# 
# # look at the cluster characteristics
# head(res.hcpc$data.clust, 10)
# res.hcpc %>%
#   group_by(clust) %>%
#   summarise(mean_dep_start=mean(depth_start),
#             sd_dep_start=sd(depth_start),
#             mean_st=min(min_st))
# res.hcpc$desc.var$quanti
# 
# # extract the cluster data
# clust = res.hcpc$data.clust
# clust$st_magni = clust$max_st - clust$min_st








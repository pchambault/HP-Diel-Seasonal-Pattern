########################################################
# clustering to identify dive categories
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
dive <- readRDS("./RDATA/1c.diveSummary_5HP_calib_5m_zoc0.RDS")
names(dive)
dive = dive %>%
  select(id, period, maxdep, dur, meandep, des_rate, asc_rate, bot_dur)
dat = as.matrix(dive[,-c(1:3)])

# center variables
dat = scale(dat, center = TRUE, scale = TRUE)
summary(dat)





###########################
# K-Means Cluster Analysis
###########################
k = 5

# impute missing values with PCA
system.time({ res.comp <- imputePCA(dat, ncp = 5, graph = FALSE) }) # 50 sec
fit <- kmeans(na.omit(dat), centers = k,
              nstart = 50, iter.max = 15)   # k cluster solution
# fit <- kmeans(res.comp$completeObs, centers = k,
#               nstart = 50, iter.max = 15)   # k cluster solution

# get cluster means
aggregate(res.comp$completeObs,by=list(fit$cluster),FUN=mean)

# # visualize kmeans
# fviz_nbclust(na.omit(dat), kmeans, method = "wss")
# fviz_cluster(kmeans(res.comp$completeObs, centers = 2),
#              geom = "point", data = res.comp$completeObs)

# append cluster assignment
clust <- data.frame(dive, "cluster"=fit$cluster) %>% 
  as_tibble() %>%
  mutate(cluster = as.factor(cluster))
names(clust)

clust$cluster = factor(clust$cluster, levels=c("5","2","4","1","3"))
# clust$cluster = factor(clust$cluster, levels=c("2","1","3"))
summary(clust)
clust = clust %>% filter(bot_dur < 100)


#---------------------
# summary per cluster
#---------------------

## k=5 ##
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











###############
# Explo
###############

# dive characteristics WITH outliers
#---------------------------------------
a = ggplot(clust, aes(x=reorder(cluster,-maxdep,na.rm = TRUE), 
                      y=-maxdep, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)",title="Max depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(-365,0) +
  theme(legend.position = "none")

b = ggplot(clust, aes(x=cluster, y=-meandep, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m)", title="Mean depth") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  ylim(-365,0) +
  theme(legend.position = "none")

c = ggplot(clust, aes(x=cluster, y=dur/60, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(min)", title="Dive duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

d = ggplot(clust, aes(x=cluster, y=des_rate, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Descent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

e = ggplot(clust, aes(x=cluster, y=asc_rate, fill=cluster)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(m/s)", title="Ascent rate") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

f = ggplot(clust, aes(x=cluster, y=bot_dur, fill=cluster)) +
  geom_boxplot() + 
  stat_summary(fun.y="mean", colour="red", size=0.2) +
  labs(x="Clusters", y="(sec)", title="Bottom duration") +
  scale_fill_brewer(palette = "PiYG") +
  theme_tq()  +
  theme(legend.position = "none")

grid.arrange(a,b,c,d,e,f, ncol=3)

ggsave(paste0("./FIGURES/Clustering/Boxplot_dives_K-means_",
              k,"clusters.png"),
       grid.arrange(a,b,c,d,e,f, ncol=3),
       width=6,height=4.5,units="in",dpi=400)


# without outliers
#---------------------
a = ggplot(clust, aes(x=reorder(cluster,-maxdep,na.rm = TRUE), 
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




# #########
# # PCA 
# #########
# system.time({ res.pca  <- PCA(res.comp$completeObs) })         # 71 sec
# gc()
# summary(res.pca)
# pca = get_pca(res.pca, element = c("var", "ind"))
# pca = get_pca_var(res.pca)
# head(pca)


  


############     
## HCPC
############
# system.time({ res.hcpc <- HCPC(data[,-c(1:2)], graph = FALSE) }) #
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








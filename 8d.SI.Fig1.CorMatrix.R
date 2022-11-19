###########################################
####      SI FIGURES FOR HP PAPER #########
###########################################


library(GGally)
library(ggplot2)




#########################
# load LR dataset
#########################
lr <- readRDS("./RDATA/1c.daylight_LR.RDS")
lr$month = as.numeric(substr(lr$date, 6, 7))
unique(lr$id) # 19 ids

# select individuals with long tracking dataset 
# at least 2 months, not only covering summer
#------------------------------------------------
lr %>%
  group_by(id) %>%
  dplyr::summarise(start    = first(date),
                   end      = last(date),
                   duration = round(difftime(last(date), first(date), units="days")),
                   ndives   = n()) 
lr = lr %>% 
  filter(id != "22849", id != "22853",
         id != "7617", id != "7618",
         id != "24638",id != "37227", 
         id != "37235",id != "22849b", id != "22850b", 
         id != "27262", id != "27262b", id != "93100") # remove HR tags
length(unique(lr$id)) # 8 ids
summary(lr)




#########################
# load HR dataset
#########################
hr <- readRDS("./RDATA/4.daylight_HR.RDS")
hr$month = as.numeric(substr(hr$date, 6, 7))
length(unique(hr$id))  # 5 ids

dive = rbind(lr, hr)
length(unique(dive$id)) # 13 ids
dive = dive[!is.na(dive$daylight),] # 5042 obs








###########################################
# SI Fig 1: pairwise colinearity
###########################################
# Create data 
data <- dive %>% dplyr::select(-c(id, date)) %>%
  rename(daylength = daylight, 'mean depth' = mean_dep, 
         'median depth' = median_dep, 'max depth' = max_dep)

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
  theme_tq() +
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        strip.background = element_rect(fill = "steelblue4",
                                        colour="white"),
        strip.text = element_text(colour='white',size=7,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.text.x = element_text(size = 6, angle = 90, 
                                   vjust=0.5, hjust = 1),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 8, face="bold"),
        title = element_text(colour="black",size=9,face="bold"),
        plot.title=element_text(size=9, vjust=0, hjust=0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.2,0.3),"cm")) # t, r, b, l 

ggsave(filename=paste0("./FIGURES/PAPER/SI.Fig1.pdf"),
       width=5,height=5,units="in",dpi=400,family="ArialMT")



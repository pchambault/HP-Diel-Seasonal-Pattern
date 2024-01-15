###########################################
####     SI FIGURE 7: Cor matrix  #########
###########################################


library(GGally)
library(ggplot2)




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
         id != "22849", id != "22853",id != "37227",   # tracked < 1 month
         id != "7617", id != "7618") %>% # removed because different depth bin settings
  select(id,date,mean_dep,max_dep,daylength)
unique(lr$id) 






#########################
# load HR dataset
#########################
hr <- readRDS("./RDATA/0b.daylength_depth_HR_5ids_tzCorrected.RDS") %>%
  select(c(id,date,mean_dep,max_dep,daylength)) %>%
  filter(id != "27262")
length(unique(hr$id))   # 4 ids

names(lr)
names(hr)
dive = rbind(lr, hr)
length(unique(dive$id)) # 12 ids
dive = dive[!is.na(dive$daylength),] # 1230 obs








###########################################
# SI Fig 1: pairwise colinearity
###########################################
# Create data 
data <- dive %>% 
  dplyr::select(-c(id, date)) %>%
  rename('mean depth' = mean_dep, 'max depth' = max_dep)

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

ggsave(filename=paste0("./PAPER/5.SciRep/3.Review_Dec2023/SI/SI.Fig7.pdf"),
       width=5,height=5,units="in",dpi=400,family="ArialMT")



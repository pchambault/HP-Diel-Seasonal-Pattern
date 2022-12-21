##############################################
########## table summary / ID  ###############
##############################################

library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(viridis)
library(DT)
library(kableExtra)
library(data.table)
library(gtExtras)
library(scales)
library(gt)
source("gt_functions_Jojo.R")



#########################################
# dive depth summary
#########################################

# depth: low resolution
#------------------------
dep_lr <- readRDS("./RDATA/1c.depth_Binned_LR_19ids.RDS") %>%
  mutate(date  = as.Date(posix_local),
         hour  = substr(posix_local, 12, 13),
         month = format(date, "%b")) %>%
  dplyr::select(c(id,posix_local,date,hour,depth,month)) %>%
  filter(id != "7618",id != "7617") %>%
  mutate(dataset = "Low resolution")
dep_lr = dep_lr[order(dep_lr$date),]
dep_lr
unique(dep_lr$id)


# depth: high resolution
#-----------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dep_hr <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")  %>%
  mutate(date = as.Date(substr(start, 1, 10)),
         hour  = substr(start, 12, 13),
         month = format(date, "%b"),
         dataset = "Low resolution") %>%
  dplyr::rename(posix_local = start,
                depth = maxdep) %>%
  dplyr::select(c(id,posix_local,date,hour,depth,month,dataset)) %>%
  ungroup()
dep_hr = dep_hr[order(dep_hr$date),]
names(dep_hr)
names(dep_lr)

dep = rbind(dep_lr, dep_hr)
unique(dep$id) # 17 porpoises
dep$id[dep$id == "93100"]  = "93100*"
dep$id[dep$id == "27262b"] = "27262b*"
dep$id[dep$id == "22849b"] = "22849b*"
dep$id[dep$id == "22850b"] = "22850b*"
dep$id[dep$id == "27262"]  = "27262*"






#########################################
# dive duration summary
#########################################
# dur: low resolution
#------------------------
dur_lr <- readRDS("./RDATA/1c.duration_Binned_LR_19ids.RDS") %>%
  mutate(date  = as.Date(posix_local),
         hour  = substr(posix_local, 12, 13),
         month = format(date, "%b")) %>%
  dplyr::select(c(id,posix_local,date,hour,duration,month)) %>%
  filter(id != "7618",id != "7617") %>%
  dplyr::rename(dur = duration) %>%
  mutate(dataset = "Low resolution")
dur_lr = dur_lr[order(dur_lr$date),]
dur_lr
unique(dur_lr$id)


# dur: high resolution
#-----------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/HP-Diel-Seasonal-Pattern")
dur_hr <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")  %>%
  mutate(date = as.Date(substr(start, 1, 10)),
         hour  = substr(start, 12, 13),
         month = format(date, "%b"),
         dataset = "Low resolution",
         dur = dur / 60) %>%
  dplyr::rename(posix_local = start) %>%
  dplyr::select(c(id,posix_local,date,hour,dur,month,dataset)) %>%
  ungroup()
dur_hr = dur_hr[order(dur_hr$date),]
names(dur_hr)
names(dur_lr)

dur = rbind(dur_lr, dur_hr)
length(unique(dur$id)) # 17 porpoises
dur$id[dur$id == "93100"]  = "93100*"
dur$id[dur$id == "27262b"] = "27262b*"
dur$id[dur$id == "22849b"] = "22849b*"
dur$id[dur$id == "22850b"] = "22850b*"
dur$id[dur$id == "27262"]  = "27262*"
summary(dur)




####################
# monthly dives
####################
monthly = dep %>%
  group_by(id, month) %>%
  summarize(ntot           = n(),
            dives0_20m     = n_distinct(depth<=20),
            dives20_50m    = n_distinct(depth>20 & depth<=50),
            dives50_100m   = n_distinct(depth>50 & depth<=100),
            dives_sup100m  = n_distinct(depth>100)) %>%
  ungroup()
monthly





#############################
# track summary / ID
#############################
loc <- readRDS("./RDATA/1a.locations_filtered_17HP.rds") %>%
  mutate(date  = as.Date(posix_local),
         hour  = substr(posix_local, 12, 13),
         month = format(date, "%b")) %>%
  select(c(id,date,hour,lon,lat,month))
loc = loc[order(loc$date, loc$id),]
length(unique(loc$id)) # 17 porpoises
loc$id[loc$id == "93100"]  = "93100*"
loc$id[loc$id == "27262b"] = "27262b*"
loc$id[loc$id == "22849b"] = "22849b*"
loc$id[loc$id == "22850b"] = "22850b*"
loc$id[loc$id == "27262"]  = "27262*"


# tracking info
#----------------
sum_track = loc %>%
  group_by(id) %>%
  summarise(Start = first(date),
                   End   = last(date),
                   Duration = difftime(last(date), first(date), units = "days")) %>%
  arrange(Start) %>%
  mutate("Sex" = c("F","F","F","M","F","F","F","F","M","F",
                   "M","M","F","F","M","M","M"),
         "Length" = c(120,166,126,125,150,129,150,140,152,135,
                      120,149,125,123,130,140,145)) %>%
  # "Weight" = c(NA,NA,35,33,NA,47,43,43,NA,NA,
  #              NA,NA,NA,NA,NA,NA,NA)) %>%
  ungroup()
sum_track

# duration info
#----------------
sum_dur = dur %>%
  filter(dur < 8) %>%
  group_by(id) %>%
  summarise(Dive_dur = paste0(signif(mean(dur),3),
                              "+",signif(sd(dur),3),
                              " (",signif(max(dur),3),")")) %>%
  ungroup()
sum_dur


# depth info
#----------------
sum_dep = dep %>%
  group_by(id) %>%
  summarise(ndive  = n(),
            Dive_dep = paste0(signif(mean(depth),3),
                              "+",signif(sd(depth),3),
                              " (", signif(max(depth),3),")")) %>%
  ungroup()
sum_dep

# combine the 3 tables
#-------------------------
table = sum_track %>%
  left_join(sum_dep, by = "id") %>%
  left_join(sum_dur, by = "id")
table

webshot::install_phantomjs(force = TRUE)
table %>%
  kbl() %>% 
  kable_classic(full_width=F, html_font="times") %>%
  kable_styling(latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "condensed"),
                position = "center",
                font_size = 18) %>% 
  # kable_paper(full_width = T) %>%
  # column_spec(c(6:13), color = "red") %>%
  # column_spec(2, italic=T) %>%
  row_spec(0, bold=T)











#########################################
# create table with gt package (from Jojo)
#########################################
dep = dep %>%
  group_by(id) %>%
  mutate(day_departure = as.numeric(date - first(date)) + 1)
dur = dur %>%
  group_by(id) %>%
  mutate(day_departure = as.numeric(date - first(date)) + 1)


#-------------------------
# add depth proportions
# according to depth class
#-------------------------
# nrow(dive[dive$month=="Aug" & dive$id=="22849b*" & dive$depth<=20,])

## in Jul ##
dive_class = dep %>%
  filter(month == "Jul") %>%
  group_by(id) %>%
  summarise(ntot = n(),
            dives0_20m     = length(depth[depth<=20]),
            dives20_50m    = length(depth[depth>20 & depth<=50]),
            dives50_100m   = length(depth[depth>50 & depth<=100]),
            dives_deep100m = length(depth[depth>100])) %>%
  ungroup()
dive_class

dive_class = dive_class %>%
  pivot_longer(c(dives0_20m, dives20_50m,
                 dives50_100m, dives_deep100m),
               names_to = "depth_class", values_to = "n") %>%
  mutate(percent = round((n / ntot) * 100))
prop_jul = dive_class %>%
  group_by(id) %>%
  summarise(percent_dive_jul = list(percent))


## in Oct ##
dive_class = dep %>%
  filter(month == "Oct") %>%
  group_by(id) %>%
  summarize(ntot = n(),
                   dives0_20m     = length(depth[depth<=20]),
                   dives20_50m    = length(depth[depth>20 & depth<=50]),
                   dives50_100m   = length(depth[depth>50 & depth<=100]),
                   dives_deep100m = length(depth[depth>100])) %>%
  ungroup()

dive_class = dive_class %>%
  pivot_longer(c(dives0_20m, dives20_50m,
                 dives50_100m, dives_deep100m),
               names_to = "depth_class", values_to = "n") %>%
  mutate(percent = round((n / ntot) * 100))
prop_oct = dive_class %>%
  group_by(id) %>%
  summarise(percent_dive_oct = list(percent))



#-------------------
# prepare dataset
#------------------- 
# detach("package:plyr", unload = TRUE)
# join = loc %>%
#   left_join(dep, by = "id") %>%
#   left_join(dur, by = "id")
# names(join)

#----------------
table_loc = loc %>%
  group_by(id) %>%
  summarise(Start = first(date),
            End   = last(date),
            ndays = round(as.numeric(difftime(
              last(date), first(date),
              units = "days")))) %>%
  arrange(Start) %>%
  mutate("Sex" = c("F","F","F","M","F","F","F","F","M","F",
                   "M","M","F","F","M","M","M"),
         "Length" = c(120,166,126,125,150,129,150,140,152,135,
                      120,149,125,123,130,140,145)) %>%
  ungroup() 

# duration info
#----------------
table_dur = dur %>%
  filter(dur < 8) %>%
  group_by(id) %>%
  summarise(Dduration = paste0(signif(mean(dur),2),
                               "+",signif(sd(dur),2),
                               " (", signif(max(dur),2),")")) %>%
  ungroup()
table_dur

# depth info
#----------------
table_dep = dep %>%
  group_by(id) %>%
  summarise(ndive  = prettyNum(n(),
                               big.mark = ",",
                               scientific = FALSE),
            Maxdepth = paste0(round(mean(depth),1),
                              "+",round(sd(depth),1),
                              " (", round(max(depth),1),")")) %>%
  ungroup()
table_dep

table = table_loc %>%
  left_join(table_dep, by = "id") %>%
  left_join(table_dur, by = "id")
table
saveRDS(table, "./RDATA/6.Table_summary_17ids.RDS")


#-------------------
table1 = table %>%
  # add quantile 95 of dduration and max_depth
  left_join(
    .,
    dep %>%
      group_by(id, day_departure) %>%
      summarise(
        max_depth  = as.numeric(quantile(depth, 0.95, na.rm = T)),
        .groups = "drop"
      ) %>%
      group_by(id) %>%
      summarise(
        sparkline_maxdepth = list(
          data.frame(max_depth = max_depth,
                     day_departure = day_departure)),
        .groups = "drop"
      ),
    by = c("id")
  ) %>% 
  
  # add quantile 95 of dduration 
  left_join(
    .,
    dur %>%
      group_by(id, day_departure) %>%
      summarise(
        dduration  = as.numeric(quantile(dur, 0.95, na.rm = T)),
        .groups = "drop"
      ) %>%
      group_by(id) %>%
      summarise(
        sparkline_qt_dduration = list(
          data.frame(dduration = dduration,
                     day_departure = day_departure)),
        .groups = "drop"
      ),
    by = c("id")
  ) %>%
  
  
  # add percentage for depth class in July
  left_join(
    .,
    prop_jul,
    by = c("id")
  ) %>%
  # add percentage for depth class in Oct
  left_join(
    .,
    prop_oct,
    by = c("id")
  )
names(table1) 
View(table1)
table1$percent_dive_jul[1:8] = NA


# draw table
#-----------------
gt(table1) %>%
  # opt_row_striping() %>%
  tab_options(data_row.padding = px(3)) %>%
  # tab_spanner(
  #   label = "Length",
  #   columns = c(Length)
  # ) %>%
  gt_ggplot_sparkline(sparkline_maxdepth, "sparkline_maxdepth") %>%
  gt_ggplot_sparkline(sparkline_qt_dduration, "sparkline_qt_dduration") %>%
  
  tab_spanner(label = "Maximum depth (m)",
              columns = c(Maxdepth,
                          sparkline_maxdepth)) %>%
  tab_spanner(label = "Dive duration (min)",
              columns = c(Dduration,
                          sparkline_qt_dduration)) %>%
  tab_spanner(label = md("Dive proportions in July"),
              columns = c(percent_dive_jul)) %>%
  gt_plt_bar_stack_extra(
    percent_dive_jul,
    width   = 65,
    labels  = c("<20", "20-50", "50-100", ">100m"),
    palette = c("#000000", "#444444", "#888888", "#CCCCCC")
  ) %>%
  tab_spanner(label = md("Dive proportions in October"),
              columns = c(percent_dive_oct)) %>%
  gt_plt_bar_stack_extra(
    percent_dive_oct,
    width   = 65,
    labels  = c("<20", "20-50", "50-100", ">100m"),
    palette = c("#000000", "#444444", "#888888", "#CCCCCC")
  ) %>%
  
  cols_width(
    id ~ px(70),
    Start ~ px(100),
    End ~ px(100),
    Sex ~ px(50),
    Length ~ px(60),
    ndays ~ px(50),
    ndive ~ px(90),
    Maxdepth ~ px(130),
    sparkline_maxdepth ~ px(70),
    Dduration ~ px(110),
    sparkline_qt_dduration ~ px(70),
    everything() ~ px(270)
  )  %>%
  
  cols_align(align = "center") %>%
  cols_label(
    id = "ID",
    Sex = "Sex",
    Length = "Length (cm)",
    Start = "Start",
    End = "End",
    ndive = "#dives",
    ndays  = "#days",
    Maxdepth = "Mean±SD",
    sparkline_maxdepth = "Sparkline",
    Dduration = "Mean±SD",
    sparkline_qt_dduration = "Sparkline"
  ) 
  # tab_style(
  #   style = list(
  #     cell_fill(color = "azure3")
  #   ),
  #   locations = cells_body(
  #     # columns = vars(V1, V2), # not needed if coloring all columns
  #     rows = c(5,13,14,16,17))
  # ) 


# id 22849: not enough days of tracking for dur for sparkline
id = dur %>% filter(id == "22849")
id








####################################
# mean, sd, max depth for SUMMARY
####################################
lr <- readRDS("./RDATA/1c.dailyDepth_LR.RDS") %>%
  mutate(dateset = "low")
hr <- readRDS("./RDATA/1b.dailyDepth_HR.RDS") %>%
  mutate(dateset = "high")
names(hr)
names(lr)
dive = bind_rows(hr,lr)

mean(dive$maxdep) # 188 m
sd(dive$maxdep)   # 83 m
max(dive$maxdep)  # 450 m

# ndives
#-----------
table <- readRDS("./RDATA/6.Table_summary_17ids.RDS")
table$ndive = as.numeric(gsub("\\,", "", table$ndive))
sum(table$ndive)   # 411 448 dives
subset = table %>% filter(id == "93100*" | id == "27262b*" | id == "22849b*"
                          | id == "22850b*"| id == "27262*")
sum(subset$ndive)  # 401 438 dives










#########################################
# STATS: pairwise correlations
#########################################
dive <- readRDS("./RDATA/1b.diveSummary_5HP_calib_5m_zoc0.RDS")
names(dive)
cor_dat = dive %>% 
  dplyr::select(c(dur, maxdep, meandep, 
                  bot_dur, asc_dur, des_dur))
corr <- round(cor(cor_dat, use = "pairwise.complete.obs"), 1)
corr[is.na(corr)] <- 0
corr_p <- cor_pmat(cor_dat)
corr_p[is.na(corr_p)] <- 1

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






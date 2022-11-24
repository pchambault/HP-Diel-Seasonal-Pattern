###########################################
# relationship between max depth and size
###########################################
table <- readRDS("./RDATA/6.Table_summary_17ids.RDS")
table$id[table$id == "27262*"]  = "27262"
table$id[table$id == "22850b*"] = "22850b"
table$id[table$id == "22849b*"] = "22849b"
table$id[table$id == "27262b*"] = "27262b"
table$id[table$id == "93100*"]  = "93100"

lr <- readRDS("./RDATA/1c.dailyDepth_LR.RDS")
hr <- readRDS("./RDATA/1b.dailyDepth_HR.RDS")
hp = bind_rows(lr, hr) %>% filter(id != "7617", id != "7618")
length(unique(hp$id))

table2 = hp %>% 
  group_by(id) %>%
  summarise(daily_maxdep    = mean(maxdep),
            daily_mediandep = median(maxdep),
            daily_sddep     = sd(maxdep)) %>%
  left_join(table, by = "id")

# table = table %>% 
#   separate(Maxdepth, c('mean_sd', 'max_dep'), sep=" ") 
# table$max_dep = parse_number(table$max_dep)
kruskal.test(daily_maxdep~Sex, table2) # 0.6256
table2 %>% 
  group_by(Sex) %>% 
  summarise(n = n(),
            mean_maxdep   = mean(daily_maxdep),
            median_maxdep = median(daily_maxdep))









################################################
clust = clust %>% mutate(date = as.Date(start),
                         month = format(start, "%b"),
                         day   = as.numeric(round(difftime(start,
                                                           first(start), 
                                                           units = "days")))+1)
stat = clust %>%
  filter(month != "Jan") %>% 
  group_by(id, cluster, month) %>%
  summarise(ndives = n()) %>%
  ungroup()
stat$month = factor(stat$month, levels = c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))

stat2 = clust %>%
  filter(month != "Jan") %>%
  group_by(id, month) %>%
  summarize(ntot = n())

prop = stat %>%
  filter(month != "Jan") %>%
  left_join(stat2, by = c("id", "month")) %>%
  mutate(val = (ndives / ntot) * 100) %>%
  arrange(id, month)
prop$month = factor(prop$month, levels = c("Jul","Aug","Sep",
                                           "Oct","Nov","Dec"))


ggplot(prop, aes(y=val, x=month)) +
  geom_histogram(aes(fill=cluster), stat="identity",
                 position="fill") +
  facet_wrap(~id, ncol=5) +
  scale_fill_brewer(palette = 1, direction = 1) +
  theme_tq() +
  theme(axis.text  = element_text(size=6, hjust=0.5, angle=90))



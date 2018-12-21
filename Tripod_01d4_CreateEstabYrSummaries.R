### PLOT SUMMARIES INTO SITE SUMMARIES

# site shrub percent
shrub_temp <- tripod %>%
  group_by(., site) %>%
  summarize_at(c("shrub_all"),
               funs(mean(., na.rm=TRUE)))


# site DWD percent
dwd_temp <- tripod %>%
  group_by(., site) %>%
  summarize_at(c("dead_wood"),
               funs(mean(., na.rm=TRUE)))


# snag_LT10 count
snags_LT10_temp <- tripod %>%
  group_by(., site) %>%
  summarize_at(c("snags_LT10"),
               funs(mean(., na.rm=TRUE)))


# snag_1020 count
snags_1020_temp <- tripod %>%
  group_by(., site) %>%
  summarize_at(c("snags_1020"),
               funs(mean(., na.rm=TRUE)))


# snag_GT20 count
snags_GT20_temp <- tripod %>%
  group_by(., site) %>%
  summarize_at(c("snags_GT20"),
               funs(mean(., na.rm=TRUE)))


site_summary$shrub_perc <- shrub_temp$shrub_all
site_summary$dwd_perc <- dwd_temp$dead_wood
site_summary$snags_LT10 <- snags_LT10_temp$snags_LT10
site_summary$snags_1020 <- snags_1020_temp$snags_1020
site_summary$snags_GT20 <- snags_GT20_temp$snags_GT20


remove(shrub_temp); remove(dwd_temp);
remove(snags_LT10_temp); remove(snags_1020_temp); remove(snags_GT20_temp)



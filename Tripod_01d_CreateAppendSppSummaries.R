### SUMMARIES - ALL SPP
# spp density
spp_sum <- tripod %>%
  filter(! spp == "SEEDLING") %>%                    #*** NIXING SEEDLINGS***
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# all other columns are carried along as list (?). Nix 'em then add back on as variables
spp_sum <- spp_sum[,c("site", "tpha")]
spp_sum <- site_summary %>%
  right_join(spp_sum, by="site")

# spp height
spp_temp <- tripod %>% # again, all other columns carried along. Nix then add back in
  filter(! spp == "SEEDLING") %>%                    #*** NIXING SEEDLINGS***
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(spp_temp)[which(names(spp_temp) == "ttl_ht")] <- "avg_ht"
spp_temp <- spp_temp[,c("site", "avg_ht")]
spp_sum <- spp_temp %>%
  right_join(spp_sum, by="site")
remove(spp_temp)

# spp biomass proxy
spp_sum$biom_prox <- spp_sum$avg_ht * spp_sum$tpha


# quick grwth since 2012, estab yr
spp_grwth_temp <- tripod %>%
  filter(! spp == "SEEDLING") %>%                    #*** NIXING SEEDLINGS***
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
spp_grwth_temp$grwth_12_16 <- rowMeans(spp_grwth_temp[,c("grwth_2016",
                                                         "grwth_2015",
                                                         "grwth_2014",
                                                         "grwth_2013",
                                                         "grwth_2012")],
                                       na.rm=TRUE)
spp_sum <- spp_grwth_temp %>%
  right_join(spp_sum, by="site")

spp_sum$tpha <- as.integer(round(spp_sum$tpha))
spp_sum <- as.data.frame(spp_sum)
rownames(spp_sum) <- make.names(spp_sum$site, unique = TRUE)

remove(spp_grwth_temp)


#########################################################################
### SUMMARY - ABLA
# ABLA density
abla_sum <- tripod %>%
  subset(., spp == "ABLA") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#abla_sum <- abla_sum[,c("site", "tpha")]
abla_sum <- site_summary %>%
  right_join(abla_sum, by="site")

# ABLA height
abla_temp <- tripod %>%
  subset(., spp == "ABLA") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(abla_temp)[which(names(abla_temp) == "ttl_ht")] <- "avg_ht"
abla_temp <- abla_temp[,c("site", "avg_ht")]
abla_sum <- abla_temp %>%
  right_join(abla_sum, by="site")
remove(abla_temp)

# ABLA biomass proxy
abla_sum$biom_prox <- abla_sum$avg_ht * abla_sum$tpha

# ABLA 2012-2016 grwth, yr estab
abla_grwth_temp <- tripod %>%
  subset(., spp == "ABLA") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
abla_grwth_temp$grwth_12_16 <- rowMeans(abla_grwth_temp[,c("grwth_2016",
                                                           "grwth_2015",
                                                           "grwth_2014",
                                                           "grwth_2013",
                                                           "grwth_2012")],
                                        na.rm=TRUE)
abla_sum <- abla_grwth_temp %>%
  right_join(abla_sum, by="site")

abla_sum$tpha <- as.integer(round(abla_sum$tpha))
abla_sum <- as.data.frame(abla_sum)
rownames(abla_sum) <- make.names(abla_sum$site, unique = TRUE)


remove(abla_grwth_temp)





#########################################################################
### SUMMARY - LAOC
# laoc density
laoc_sum <- tripod %>%
  subset(., spp == "LAOC") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#laoc_sum <- laoc_sum[,c("site", "tpha")]
laoc_sum <- site_summary %>%
  right_join(laoc_sum, by="site")

# laoc height
laoc_temp <- tripod %>%
  subset(., spp == "LAOC") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(laoc_temp)[which(names(laoc_temp) == "ttl_ht")] <- "avg_ht"
laoc_temp <- laoc_temp[,c("site", "avg_ht")]
laoc_sum <- laoc_temp %>%
  right_join(laoc_sum, by="site")
remove(laoc_temp)

# laoc biomass proxy
laoc_sum$biom_prox <- laoc_sum$avg_ht * laoc_sum$tpha

# laoc 2012-2016 grwth, yr estab
laoc_grwth_temp <- tripod %>%
  subset(., spp == "LAOC") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
laoc_grwth_temp$grwth_12_16 <- rowMeans(laoc_grwth_temp[,c("grwth_2016",
                                                           "grwth_2015",
                                                           "grwth_2014",
                                                           "grwth_2013",
                                                           "grwth_2012")],
                                        na.rm=TRUE)
laoc_sum <- laoc_grwth_temp %>%
  right_join(laoc_sum, by="site")

laoc_sum$tpha <- as.integer(round(laoc_sum$tpha))
laoc_sum <- as.data.frame(laoc_sum)
rownames(laoc_sum) <- make.names(laoc_sum$site, unique = TRUE)


remove(laoc_grwth_temp)







#########################################################################
### SUMMARY - PICO
# PICO density
pico_sum <- tripod %>%
  subset(., spp == "PICO") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#pico_sum <- pico_sum[,c("site", "tpha")]
pico_sum <- site_summary %>%
  right_join(pico_sum, by="site")

# PICO height
pico_temp <- tripod %>%
  subset(., spp == "PICO") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(pico_temp)[which(names(pico_temp) == "ttl_ht")] <- "avg_ht"
pico_temp <- pico_temp[,c("site", "avg_ht")]
pico_sum <- pico_temp %>%
  right_join(pico_sum, by="site")
remove(pico_temp)

# PICO biomass proxy
pico_sum$biom_prox <- pico_sum$avg_ht * pico_sum$tpha

# PICO 2012-2016 grwth, yr estab
pico_grwth_temp <- tripod %>%
  subset(., spp == "PICO") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
pico_grwth_temp$grwth_12_16 <- rowMeans(pico_grwth_temp[,c("grwth_2016",
                                                         "grwth_2015",
                                                         "grwth_2014",
                                                         "grwth_2013",
                                                         "grwth_2012")],
                                       na.rm=TRUE)
pico_sum <- pico_grwth_temp %>%
  right_join(pico_sum, by="site")

pico_sum$tpha <- as.integer(round(pico_sum$tpha))
pico_sum <- as.data.frame(pico_sum)
rownames(pico_sum) <- make.names(pico_sum$site, unique = TRUE)


remove(pico_grwth_temp)






#########################################################################
### SUMMARY - PIEN
# pien density
pien_sum <- tripod %>%
  subset(., spp == "PIEN") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#pien_sum <- pien_sum[,c("site", "tpha")]
pien_sum <- site_summary %>%
  right_join(pien_sum, by="site")

# pien height
pien_temp <- tripod %>%
  subset(., spp == "PIEN") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(pien_temp)[which(names(pien_temp) == "ttl_ht")] <- "avg_ht"
pien_temp <- pien_temp[,c("site", "avg_ht")]
pien_sum <- pien_temp %>%
  right_join(pien_sum, by="site")
remove(pien_temp)

# pien biomass proxy
pien_sum$biom_prox <- pien_sum$avg_ht * pien_sum$tpha

# pien 2012-2016 grwth, yr estab
pien_grwth_temp <- tripod %>%
  subset(., spp == "PIEN") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
pien_grwth_temp$grwth_12_16 <- rowMeans(pien_grwth_temp[,c("grwth_2016",
                                                           "grwth_2015",
                                                           "grwth_2014",
                                                           "grwth_2013",
                                                           "grwth_2012")],
                                        na.rm=TRUE)
pien_sum <- pien_grwth_temp %>%
  right_join(pien_sum, by="site")

pien_sum$tpha <- as.integer(round(pien_sum$tpha))
pien_sum <- as.data.frame(pien_sum)
rownames(pien_sum) <- make.names(pien_sum$site, unique = TRUE)

remove(pien_grwth_temp)




#########################################################################
### SUMMARY - PIPO
# pipo density
pipo_sum <- tripod %>%
  subset(., spp == "PIPO") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#pipo_sum <- pipo_sum[,c("site", "tpha")]
pipo_sum <- site_summary %>%
  right_join(pipo_sum, by="site")

# pipo height
pipo_temp <- tripod %>%
  subset(., spp == "PIPO") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(pipo_temp)[which(names(pipo_temp) == "ttl_ht")] <- "avg_ht"
pipo_temp <- pipo_temp[,c("site", "avg_ht")]
pipo_sum <- pipo_temp %>%
  right_join(pipo_sum, by="site")
remove(pipo_temp)

# pipo biomass proxy
pipo_sum$biom_prox <- pipo_sum$avg_ht * pipo_sum$tpha


# PIPO 2012-2016 grwth, yr estab
pipo_grwth_temp <- tripod %>%
  subset(., spp == "PIPO") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
pipo_grwth_temp$grwth_12_16 <- rowMeans(pipo_grwth_temp[,c("grwth_2016",
                                                           "grwth_2015",
                                                           "grwth_2014",
                                                           "grwth_2013",
                                                           "grwth_2012")],
                                        na.rm=TRUE)
pipo_sum <- pipo_grwth_temp %>%
  right_join(pipo_sum, by="site")

pipo_sum$tpha <- as.integer(round(pipo_sum$tpha))
pipo_sum <- as.data.frame(pipo_sum)
rownames(pipo_sum) <- make.names(pipo_sum$site, unique = TRUE)

remove(pipo_grwth_temp)





#########################################################################
### SUMMARY - PSME
# psme density
psme_sum <- tripod %>%
  subset(., spp == "PSME") %>%
  group_by(., site) %>%
  summarize_at(c("tpha"), funs(sum(., na.rm=TRUE)))

# nix and re-add variables -- actually, not necessary. maybe b/c of above subset?
#psme_sum <- psme_sum[,c("site", "tpha")]
psme_sum <- site_summary %>%
  right_join(psme_sum, by="site")

# psme height
psme_temp <- tripod %>%
  subset(., spp == "PSME") %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE)))
colnames(psme_temp)[which(names(psme_temp) == "ttl_ht")] <- "avg_ht"
psme_temp <- psme_temp[,c("site", "avg_ht")]
psme_sum <- psme_temp %>%
  right_join(psme_sum, by="site")
remove(psme_temp)

# psme biomass proxy
psme_sum$biom_prox <- psme_sum$avg_ht * psme_sum$tpha

# PSME 2012-2016 grwth, yr estab
psme_grwth_temp <- tripod %>%
  subset(., spp == "PSME") %>%
  subset(., grwth_2016 > 0) %>%
  group_by(., site) %>%
  summarize_at(c("grwth_2016","grwth_2015",
                 "grwth_2014","grwth_2013",
                 "grwth_2012","grwth_2011",
                 "grwth_2010","grwth_2009",
                 "grwth_2008","grwth_2007",
                 # "grwth_2006",
                 "yrs_grwth_inclusive",
                 "estab_yr"),
               funs(mean(., na.rm=TRUE)))
psme_grwth_temp$grwth_12_16 <- rowMeans(psme_grwth_temp[,c("grwth_2016",
                                                           "grwth_2015",
                                                           "grwth_2014",
                                                           "grwth_2013",
                                                           "grwth_2012")],
                                        na.rm=TRUE)
psme_sum <- psme_grwth_temp %>%
  right_join(psme_sum, by="site")

psme_sum$tpha <- as.integer(round(psme_sum$tpha))
psme_sum <- as.data.frame(psme_sum)
rownames(psme_sum) <- make.names(psme_sum$site, unique = TRUE)

remove(psme_grwth_temp)
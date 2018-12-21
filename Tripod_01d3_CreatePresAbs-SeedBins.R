
# Change stupid capitalization of seed distances
abla_sum_all$abla_seed_dist <- abla_sum_all$ABLA_seed_dist; abla_sum_all$ABLA_seed_dist <- NULL
laoc_sum_all$laoc_seed_dist <- laoc_sum_all$LAOC_seed_dist; laoc_sum_all$LAOC_seed_dist <- NULL
pico_sum_all$pico_seed_dist <- pico_sum_all$PICO_seed_dist; pico_sum_all$PICO_seed_dist <- NULL
pien_sum_all$pien_seed_dist <- pien_sum_all$PIEN_seed_dist; pien_sum_all$PIEN_seed_dist <- NULL
pipo_sum_all$pipo_seed_dist <- pipo_sum_all$PIPO_seed_dist; pipo_sum_all$PIPO_seed_dist <- NULL
psme_sum_all$psme_seed_dist <- psme_sum_all$PSME_seed_dist; psme_sum_all$PSME_seed_dist <- NULL

abla_sum$abla_seed_dist <- abla_sum$ABLA_seed_dist; abla_sum$ABLA_seed_dist <- NULL
laoc_sum$laoc_seed_dist <- laoc_sum$LAOC_seed_dist; laoc_sum$LAOC_seed_dist <- NULL
pico_sum$pico_seed_dist <- pico_sum$PICO_seed_dist; pico_sum$PICO_seed_dist <- NULL
pien_sum$pien_seed_dist <- pien_sum$PIEN_seed_dist; pien_sum$PIEN_seed_dist  <- NULL
pipo_sum$pipo_seed_dist <- pipo_sum$PIPO_seed_dist; pipo_sum$PIPO_seed_dist  <- NULL
psme_sum$psme_seed_dist <- psme_sum$PSME_seed_dist; psme_sum$PSME_seed_dist  <- NULL

# New value needs to be an established factor level so convert factor site_air_seed to character.
pico_sum_all$site_air_seed <- as.character(pico_sum_all$site_air_seed)
pico_sum$site_air_seed <- as.character(pico_sum$site_air_seed)
pico_sum_all$site_air_seed[which(pico_sum_all$site_air_seed == "YES")] <- 1
pico_sum_all$site_air_seed[which(pico_sum_all$site_air_seed == "NO")] <- 0
pico_sum$site_air_seed[which(pico_sum$site_air_seed == "YES")] <- 1
pico_sum$site_air_seed[which(pico_sum$site_air_seed == "NO")] <- 0
# now convert to being a binary 1 present 0 not present number.
pico_sum_all$site_air_seed <- as.numeric(pico_sum_all$site_air_seed)
pico_sum$site_air_seed <- as.numeric(pico_sum$site_air_seed)


# Don't have dist to seed source for many spp, even those with tpha >0. Don't want these NAs dropped so use bins.
# Bin seed dist by intervals. Set any remaining NAs to 7 (b/c > 150 -- PSME is out there. Just didn't see it)
# Setting standard bin with max 
max(abla_sum_all$abla_seed_dist, na.rm=TRUE)
max(laoc_sum_all$laoc_seed_dist, na.rm=TRUE)
max(pico_sum_all$pico_seed_dist, na.rm=TRUE)
max(pien_sum_all$pien_seed_dist, na.rm=TRUE)
max(pipo_sum_all$pipo_seed_dist, na.rm=TRUE)
max(psme_sum_all$psme_seed_dist, na.rm=TRUE)

# Max is 300. Diff spp have diff maxes, but using 300 will 
vec <- c(0,25,50,75,100,125,150,175,200,225,250,275,300)
# Bin abla seed dist
abla_sum_all$abla_dist_bin <- findInterval(abla_sum_all$abla_seed_dist, vec=vec, rightmost.closed=TRUE)
abla_sum_all$abla_dist_bin[which(is.na(abla_sum_all$abla_seed_dist))] <- (max(abla_sum_all$abla_dist_bin, na.rm=TRUE)+1)

# Bin laoc seed dist
laoc_sum_all$laoc_dist_bin <- findInterval(laoc_sum_all$laoc_seed_dist, vec=vec, rightmost.closed = TRUE) 
laoc_sum_all$laoc_dist_bin[which(is.na(laoc_sum_all$laoc_seed_dist))] <- (max(laoc_sum_all$laoc_dist_bin, na.rm=TRUE)+1)

# Bin pico seed dist
pico_sum_all$pico_dist_bin <- findInterval(pico_sum_all$pico_seed_dist, vec=vec, rightmost.closed = TRUE) 
pico_sum_all$pico_dist_bin[which(is.na(pico_sum_all$pico_seed_dist))] <- (max(pico_sum_all$pico_dist_bin, na.rm=TRUE)+1)

# Bin pien seed dist
pien_sum_all$pien_dist_bin <- findInterval(pien_sum_all$pien_seed_dist, vec=vec, rightmost.closed = TRUE) 
pien_sum_all$pien_dist_bin[which(is.na(pien_sum_all$pien_seed_dist))] <- (max(pien_sum_all$pien_dist_bin, na.rm=TRUE)+1)

# Bin pipo seed dist
pipo_sum_all$pipo_dist_bin <- findInterval(pipo_sum_all$pipo_seed_dist, vec=vec, rightmost.closed = TRUE) 
pipo_sum_all$pipo_dist_bin[which(is.na(pipo_sum_all$pipo_seed_dist))] <- (max(pipo_sum_all$pipo_dist_bin, na.rm=TRUE)+1)

# Bin psme seed dist
psme_sum_all$psme_dist_bin <- findInterval(psme_sum_all$psme_seed_dist, vec=vec, rightmost.closed = TRUE) 
psme_sum_all$psme_dist_bin[which(is.na(psme_sum_all$psme_seed_dist ))] <- (max(psme_sum_all$psme_dist_bin, na.rm=TRUE)+1)

# Create summary table for presence-absence tests.
PAdata <- spp_sum %>%
  mutate(abla_tpha = abla_sum_all$tpha) %>%
  mutate(laoc_tpha = laoc_sum_all$tpha) %>%
  mutate(pico_tpha = pico_sum_all$tpha) %>%
  mutate(pien_tpha = pien_sum_all$tpha) %>%
  mutate(pipo_tpha = pipo_sum_all$tpha) %>%
  mutate(psme_tpha = psme_sum_all$tpha)

PAdata <- PAdata %>%
  mutate(abla_PA = ifelse(abla_tpha > 0, 1, 0)) %>%
  mutate(laoc_PA = ifelse(laoc_tpha > 0, 1, 0)) %>%
  mutate(pico_PA = ifelse(pico_tpha > 0, 1, 0)) %>%
  mutate(pien_PA = ifelse(pien_tpha > 0, 1, 0)) %>%
  mutate(pipo_PA = ifelse(pipo_tpha > 0, 1, 0)) %>%
  mutate(psme_PA = ifelse(psme_tpha > 0, 1, 0))

PAdata <- PAdata %>%
  mutate(abla_dist_bin = abla_sum_all$abla_dist_bin) %>%
  mutate(laoc_dist_bin = laoc_sum_all$laoc_dist_bin) %>%
  mutate(pico_dist_bin = pico_sum_all$pico_dist_bin) %>%
  mutate(pien_dist_bin = pien_sum_all$pien_dist_bin) %>%
  mutate(pipo_dist_bin = pipo_sum_all$pipo_dist_bin) %>%
  mutate(psme_dist_bin = psme_sum_all$psme_dist_bin)

PAdata$site_air_seed <- pico_sum_all$site_air_seed

remove(vec)

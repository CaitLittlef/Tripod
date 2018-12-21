## SIMPLE STATS

n <- nrow(PAdata)
attach(PAdata)
print("spp") ; min(tpha) ; max(tpha) ; mean(tpha) ; sd(tpha)/n 
print("abla") ; min(abla_tpha) ; max(abla_tpha) ; mean(abla_tpha) ; sd(abla_tpha)/n ; sum(abla_PA)
print("laoc") ; min(laoc_tpha) ; max(laoc_tpha) ; mean(laoc_tpha) ; sd(laoc_tpha)/n ; sum(laoc_PA)
print("pico") ; min(pico_tpha) ; max(pico_tpha) ; mean(pico_tpha) ; sd(pico_tpha)/n ; sum(pico_PA)
print("pien") ; min(pien_tpha) ; max(pien_tpha) ; mean(pien_tpha) ; sd(pien_tpha)/n ; sum(pien_PA)
print("pipo") ; min(pipo_tpha) ; max(pipo_tpha) ; mean(pipo_tpha) ; sd(pipo_tpha)/n ; sum(pipo_PA)
print("psme") ; min(psme_tpha) ; max(psme_tpha) ; mean(psme_tpha) ; sd(psme_tpha)/n ; sum(psme_PA)
detach(PAdata)

data <- tripod %>%
  filter(!spp == "SEEDLING") %>%
  dplyr::select(spp, starts_with("grwth"), -grwth_avg_Ngerm_N16, -grwth_avg_Ngerm_Y16) %>%
  gather(key = yr, value = grwth, -spp)
spp.summary <- data %>%
  filter(!is.na(grwth)) %>% # otherwise summary stats will all be NA
  select(-yr) %>% # don't need yr anymore
  group_by(spp) %>%
  summarize(min = min(grwth), max = max(grwth), mean = mean(grwth), se = sd(grwth)/n(),
            na.rm = TRUE)
all.summary <- data %>%
  filter(!is.na(grwth)) %>% # otherwise summary stats will all be NA
  select(-yr) %>% # don't need yr anymore
  summarize(min = min(grwth), max = max(grwth), mean = mean(grwth), se = sd(grwth)/n(),
            na.rm = TRUE)

#####################################################################################
## SHRUB STATS - PSME
data <- tripod %>%
  group_by(site) %>%
  summarize(max.alnus = max(alnus),
            max.cean = max(cean),
            max.nfix = (max(alnus)+max(cean)))

data <- psme_sum_all %>%
  left_join(data, by = "site") %>%
  dplyr::select(site, tpha, max.alnus, max.cean, max.nfix)

nfix <- data %>%
  count(max.nfix)

# 16 of 51 sites did NOT have N-fixing shrub present (altho at varying percent cover)

data <- data %>%
  filter(tpha > 0) %>%
  dplyr::select(tpha, max.nfix)

nfix <- data %>%
  count(max.nfix)

# only 7 of 33 sites WITH PMSE did NOT have N-fixing shrub present (altho at varying percent cover)

#####################################################################################
## SHRUB STATS - LAOC
data <- tripod %>%
  group_by(site) %>%
  summarize(max.shrub = sum(alnus, cean, populus, salix, shrub))

data <- laoc_sum_all %>%
  left_join(data, by = "site") %>%
  dplyr::select(site, tpha, laoc_seed_dist, max.shrub)

nfix <- data %>%
  count(max.nfix)



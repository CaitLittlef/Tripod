### Makes spp summary tables that keep all sites (and set tpha to zero)

abla_sum_all <- left_join(site_summary, abla_sum)
laoc_sum_all <- left_join(site_summary, laoc_sum)
pico_sum_all <- left_join(site_summary, pico_sum)
pien_sum_all <- left_join(site_summary, pien_sum)
pipo_sum_all <- left_join(site_summary, pipo_sum)
psme_sum_all <- left_join(site_summary, psme_sum)


abla_sum_all$tpha[is.na(abla_sum_all$tpha)] <- 0 ; as.numeric(abla_sum_all$tpha)
laoc_sum_all$tpha[is.na(laoc_sum_all$tpha)] <- 0 ; as.numeric(laoc_sum_all$tpha)
pico_sum_all$tpha[is.na(pico_sum_all$tpha)] <- 0 ; as.numeric(pico_sum_all$tpha)
pien_sum_all$tpha[is.na(pien_sum_all$tpha)] <- 0 ; as.numeric(pien_sum_all$tpha)
pipo_sum_all$tpha[is.na(pipo_sum_all$tpha)] <- 0 ; as.numeric(pipo_sum_all$tpha)
psme_sum_all$tpha[is.na(psme_sum_all$tpha)] <- 0 ; as.numeric(psme_sum_all$tpha)


# remove(abla_sum)
# remove(laoc_sum)
# remove(pico_sum)
# remove(pien_sum)
# remove(pipo_sum)
# remove(psme_sum)

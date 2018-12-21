# VARIABLE CLEAN-UP
# After exploring data (e.g., collinearity, influence of snags), nixing variables; sorta backwards:
# Should have weeded out useless variables prior to generating summaries. So it goes.
# Rember that _rng, _mean, _std are all based on 15m radius zone sampling 10m pixels

# GEN DROPS

gen_drop_list <- c("site_snag_ABLA", "site_snag_LAOC", "site_snag_PICO",
                  "site_snag_PIEN","site_snag_PIPO", "site_snag_PSME",
                  "site_snag_unk_or_Q", "site_air_seed",
                  "ABLA_seed_dist", 
                  "LAOC_seed_dist", 
                  "PICO_seed_dist",
                  "PIEN_seed_dist",
                  "PIPO_seed_dist",
                  "PSME_seed_dist",
                  "dem10", "asp10", "asp100","trasp10", "trasp100",
                  "srad10", "srad100", "slp10", "hli10",
                  "dem_rng", "dem_mean", #"dem_std",
                  "slp_rng", "slp_mean", #"slp_std",
                  "cti_rng", "cti_mean", #"cti_std",
                  "hli_rng", "hli_mean", #"hli_std",
                  "srad_rng", "srad_mean", "srad_std",
                  "trasp_rng", "trasp_mean", "trasp_std",
                  "snags_LT10", "snags_1020", "snags_GT20")


spp_sum <- spp_sum[,! names(spp_sum) %in% gen_drop_list]

abla_sum <- abla_sum[,! names(abla_sum) %in% gen_drop_list]
abla_temp <- site_summary[,c("site","ABLA_seed_dist")]
abla_sum <- left_join(abla_sum, abla_temp, by = "site")

laoc_sum <- laoc_sum[,! names(laoc_sum) %in% gen_drop_list]
laoc_temp <- site_summary[,c("site","LAOC_seed_dist")]
laoc_sum <- left_join(laoc_sum, laoc_temp, by = "site")

pico_sum <- pico_sum[,! names(pico_sum) %in% gen_drop_list]
pico_temp <- site_summary[,c("site","PICO_seed_dist","site_air_seed")]
pico_sum <- left_join(pico_sum, pico_temp, by = "site")

pien_sum <- pien_sum[,! names(pien_sum) %in% gen_drop_list]
pien_temp <- site_summary[,c("site","PIEN_seed_dist")]
pien_sum <- left_join(pien_sum, pien_temp, by = "site")

pipo_sum <- pipo_sum[,! names(pipo_sum) %in% gen_drop_list]
pipo_temp <- site_summary[,c("site","PIPO_seed_dist")]
pipo_sum <- left_join(pipo_sum, pipo_temp, by = "site")

psme_sum <- psme_sum[,! names(psme_sum) %in% gen_drop_list]
psme_temp <- site_summary[,c("site","PSME_seed_dist")]
psme_sum <- left_join(psme_sum, psme_temp, by = "site")


# # ACTIVATE IF WANT ALL SITES INCLUDED IN SPP-SPEIFIC SUMMARIES (Tripod_01d2_CreateAppendSppSummaries.R)
abla_sum_all <- abla_sum_all[,! names(abla_sum_all) %in% gen_drop_list]
abla_temp <- site_summary[,c("site","ABLA_seed_dist")]
abla_sum_all <- left_join(abla_sum_all, abla_temp, by = "site")

laoc_sum_all <- laoc_sum_all[,! names(laoc_sum_all) %in% gen_drop_list]
laoc_temp <- site_summary[,c("site","LAOC_seed_dist")]
laoc_sum_all <- left_join(laoc_sum_all, laoc_temp, by = "site")

pico_sum_all <- pico_sum_all[,! names(pico_sum_all) %in% gen_drop_list]
pico_temp <- site_summary[,c("site","PICO_seed_dist","site_air_seed")]
pico_sum_all <- left_join(pico_sum_all, pico_temp, by = "site")

pien_sum_all <- pien_sum_all[,! names(pien_sum_all) %in% gen_drop_list]
pien_temp <- site_summary[,c("site","PIEN_seed_dist")]
pien_sum_all <- left_join(pien_sum_all, pien_temp, by = "site")

pipo_sum_all <- pipo_sum_all[,! names(pipo_sum_all) %in% gen_drop_list]
pipo_temp <- site_summary[,c("site","PIPO_seed_dist")]
pipo_sum_all <- left_join(pipo_sum_all, pipo_temp, by = "site")

psme_sum_all <- psme_sum_all[,! names(psme_sum_all) %in% gen_drop_list]
psme_temp <- site_summary[,c("site","PSME_seed_dist")]
psme_sum_all <- left_join(psme_sum_all, psme_temp, by = "site")



# GROWTH DROPS
grwth_drop_list <- c("grwth_2016","grwth_2015",
                     "grwth_2014","grwth_2013",
                     "grwth_2012","grwth_2011",
                     "grwth_2010","grwth_2009",
                     "grwth_2008","grwth_2007",
                     "grwth_2006","yrs_grwth_inclusive",
                     "estab_yr", "grwth_12_16",
                     "avg_ht")


spp_sum <- spp_sum[,! names(spp_sum) %in% grwth_drop_list]

abla_sum <- abla_sum[,! names(abla_sum) %in% grwth_drop_list]
laoc_sum <- laoc_sum[,! names(laoc_sum) %in% grwth_drop_list]
pico_sum <- pico_sum[,! names(pico_sum) %in% grwth_drop_list]
pien_sum <- pien_sum[,! names(pien_sum) %in% grwth_drop_list]
pipo_sum <- pipo_sum[,! names(pipo_sum) %in% grwth_drop_list]
psme_sum <- psme_sum[,! names(psme_sum) %in% grwth_drop_list]

# # ACTIVATE IF WANT ALL SITES INCLUDED IN SPP-SPEIFIC SUMMARIES (Tripod_01d2_CreateAppendSppSummaries.R)
abla_sum_all <- abla_sum_all[,! names(abla_sum_all) %in% grwth_drop_list]
laoc_sum_all <- laoc_sum_all[,! names(laoc_sum_all) %in% grwth_drop_list]
pico_sum_all <- pico_sum_all[,! names(pico_sum_all) %in% grwth_drop_list]
pien_sum_all <- pien_sum_all[,! names(pien_sum_all) %in% grwth_drop_list]
pipo_sum_all <- pipo_sum_all[,! names(pipo_sum_all) %in% grwth_drop_list]
psme_sum_all <- psme_sum_all[,! names(psme_sum_all) %in% grwth_drop_list]


remove(gen_drop_list) ; remove(grwth_drop_list)
remove(abla_temp) ; remove(laoc_temp) ; remove(pico_temp) ; remove(pien_temp) ; remove(pipo_temp) ; remove(psme_temp)


# Test for correlation btwn all potential predictor variables.


attach(site_summary)
# dummy_df <- data.frame(trasp, hli, srad)
dummy_df <- data.frame(dem10, dem100, slp10, slp100, cti10, cti100, tpi100,
                       hli10, hli100, srad10, srad100, trasp10, trasp100,
                       dem_rng, dem_mean, dem_std,
                       slp_rng, slp_mean, slp_std,
                       cti_rng, cti_mean, cti_std,
                       hli_rng, hli_mean, hli_std,
                       srad_rng, srad_mean, srad_std,
                       trasp_rng, trasp_mean, trasp_std)
detach(site_summary)
(cor_dummy_df <- round(cor(dummy_df,  method = "spearman"),2))

currentDate <- Sys.Date()
csvFileName <- paste0("correlations_",currentDate,".csv")
write.csv(cor_dummy_df, csvFileName)

#          trasp  hli srad trasp100 hli100 srad100
# trasp     1.00 0.84 0.78     0.92   0.82    0.73
# hli       0.84 1.00 0.65     0.84   0.91    0.62
# srad      0.78 0.65 1.00     0.81   0.63    0.95
# trasp100  0.92 0.84 0.81     1.00   0.89    0.81
# hli100    0.82 0.91 0.63     0.89   1.00    0.64
# srad100   0.73 0.62 0.95     0.81   0.64    1.00



# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(dem, dem100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # totally correlated

# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(slp, slp100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # less correlated: 0.73

# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(hli, hli100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # highly correlated: 0.91

# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(srad, srad100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # highly correlated: 0.95

# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(trasp, trasp100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # highly correlated: 0.92

# Test for correlation btwn different resolutions of topo indices
attach(site_summary)
dummy_df <- data.frame(cti, cti100)
detach(site_summary)
cor_dummy_df <- round(cor(dummy_df, use = "na.or.complete", method = "spearman"),2)
cor_dummy_df # not very correlated: 0.34


remove(cor_dummy_df)
remove(dummy_df)


#could also look at:
pairs(~dem100 + slp100 + hli100 # gives scatterplot of all pair-wise combos of variables in the model
      + cti100 + cti
      + shrub_perc
      + min_live_seed_dist
      + hli100:cti100,
      data = spp_sum)


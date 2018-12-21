# ### Site Data
site_gen <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/SITES_GEN_160912.csv")

# topo data extracted by 15m radius zone; note b/c this results in some 500m2 & some 900m2 zones
topo_zone <-read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/TOPO_170118.csv")
colnames(topo_zone)[which(names(topo_zone) == "elev_rng")] <- "dem_rng"
colnames(topo_zone)[which(names(topo_zone) == "elev_mean")] <- "dem_mean"
colnames(topo_zone)[which(names(topo_zone) == "elev_std")] <- "dem_std"



# topo data extracted to pt with 10m and 100m pixels
topo <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/TOPO_170323.csv") #pt-based vs. prior v170118
topo$site[which(topo$site == "S-111-11")] <- "S-111-011" # dunno why this one doesn't have leading zero
tpi <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Topo/TPI_100m_bysite.csv")
levels(tpi$site) <- c(levels(tpi$site), "S-111-011") 
tpi$site[which(tpi$site == "S-111-11")] <- "S-111-011" # dunno why this one doesn't have leading zero
topo <- inner_join(topo, tpi, by = "site")
topo <- (topo[!(topo$site == "S-332-706"),])


topo$FID <- NULL
topo$site[which(topo$site == "S-111-11")] <- "S-111-011" # dunno why this one doesn't have leading zero

##for simplicity, keep 10m derived variables with easy name; those with 100 at end were geneated at 100m resolution
# colnames(topo)[which(names(topo) == "dem10")] <- "dem"
# colnames(topo)[which(names(topo) == "asp10")] <- "asp"
# colnames(topo)[which(names(topo) == "trasp10")] <- "trasp"
# colnames(topo)[which(names(topo) == "slp10")] <- "slp"
# colnames(topo)[which(names(topo) == "hli10")] <- "hli"
# colnames(topo)[which(names(topo) == "srad10")] <- "srad"
# colnames(topo)[which(names(topo) == "cti10")] <- "cti"

# topo$cti_cat <- cut(topo$cti, breaks=3,labels = c((paste("cti", 1:3, sep = "_"))))
# topo$cti100_cat <- cut(topo$cti100, breaks=3,labels = c((paste("cti100", 1:3, sep = "_"))))


### Plot Data
plot_cover <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/PLOT_COVER_160912.csv")

### Tree Data
juv <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/JUV_160912.csv")
#juv <- subset(juv, select = -c(X59,X60,X61,X62,X63,X64,X65,X66,X67,X68,X69,X70,X71)) #to nix dataless columns
#juv <- subset(juv[,1:58])
#juv <- subset(juv[1:2688,])


### Combining into one matrix
juv_plot <- juv %>%
  dplyr::select(-site, -plot, -quad, -record_expand_plot, -record_expand_site) %>%
  left_join(plot_cover , by = "site_plot")
juv_plot_site <- juv_plot %>%
  left_join(site_gen, by = "site")
tripod_orig <- juv_plot_site %>%
  left_join(topo, by = "site")
colnames(tripod_orig)
tripod <- tripod_orig # could use following for reordering [,c(54, 55, 1, 97:103, 82:96, 56:81, 2:53)]
colnames(tripod)
colnames(tripod)[which(names(tripod) == "air_seed")] <- "plot_air_seed"
# View(tripod)

# write_csv(tripod, "tripod.csv")
# Somehow, above tripod.csv had 2 sites eliminated. Version below has all sites still.
write_csv(tripod, "tripod_180131.csv")


### Site-level summary matrix for ease of reference/re-joining
site_summary <- site_gen %>%
  left_join(topo, by="site") %>%
  left_join(topo_zone, by="site")
write_csv(site_summary, "site_summary.csv")

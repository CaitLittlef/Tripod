### ACTUAL LOAD. N.b., v_180131 because had wrongly saved tripod.csv withOUT 2 sites
tripod <- read.csv("tripod_180131.csv")
tripod <- as.data.frame(tripod)
site_summary <- read.csv("site_summary.csv")
site_summary <-as.data.frame(site_summary)


### POST-LOAD TWEAKS (eventually I should incorporate into adjustments above...)
tripod$site <- as.factor(tripod$site)
tripod$spp <- as.factor(tripod$spp)
colnames(tripod)[which(names(tripod) == "trees_per_ha")] <- "tpha"

levels(tripod$site_air_seed) <- c("NO", "YES", "UNK", "0", "1")
tripod$site_air_seed[tripod$site_air_seed == "UNK"] <- 0
tripod$site_air_seed[is.na(tripod$site_air_seed)] <- 0
tripod$site_air_seed[tripod$site_air_seed == "NO"] <- 0
tripod$site_air_seed[tripod$site_air_seed == "YES"] <- 1


tripod$snags_LT10[tripod$snags_LT10 == "NR"] <- NA
tripod$snags_1020[tripod$snags_1020 == "NR"] <- NA
tripod$snags_GT20[tripod$snags_GT20 == "NR"] <- NA
tripod$snags_LT10 <- as.integer(tripod$snags_LT10)
tripod$snags_1020 <- as.integer(tripod$snags_1020)
tripod$snags_GT20 <- as.integer(tripod$snags_GT20)

site_summary$site <- as.factor(site_summary$site)
row.names(site_summary) <- make.names(site_summary$site, unique = TRUE) # coerces into syntacitically valid names, dash (-) not allowed 
row.names(site_summary)
site_summary$site_air_seed[site_summary$site_air_seed == "UNK"] <- "NO"
site_summary$site_air_seed[is.na(site_summary$site_air_seed)] <- "NO"


### 2 QUESTIONABLE SITES ### 
# --> S-312-428: realized post-sampling from harvest data and aerial imagery
# that distance to larch seed source had been taken to a regenerating clear-cut (1987).
# This clear-cut had all larch regen; I'm not confident in maturity of regenerating trees at that point.
# So, reset larch seed distance to NA (and reset site's minimum seed distance to next closest)
# --> S-221-211: realized post-sampling from harvest data that overstory removal had occurred in 1988.
# But minimum live seed distance was 62 and the site was right on the edge of what looks to be harvested
# (based on Goolge Earth time-lapse).
# --> I ran count analyses excluding and including these sites; results were NOT sensitive, so keep sites in.
# But do change the laoc seed dist below on S-312-428

# THIS DROPS THOSE TWO SITES:
# tripod <- (tripod[!(tripod$site == "S-312-428"),]) # Shot to non-mature clearcut
# tripod <- (tripod[!(tripod$site == "S-221-211"),]) # Violates no-harvest criterion
# 
# site_summary <- (site_summary[!(site_summary$site == "S-312-428"),]) # Shot to non-mature clearcut
# site_summary <- (site_summary[!(site_summary$site == "S-221-211"),]) # Violates no-harvest criterion

### ALTERNATIVE: CHANGE S-312-428 LAOC_seed_dist (currently 92) to NA.
# Also, re-set min_live_seed_dist to new minimum.
site_summary$ALBA_seed_dist[which(site_summary$site == "S-312-428")] 
site_summary$LAOC_seed_dist[which(site_summary$site == "S-312-428")]
site_summary$PICO_seed_dist[which(site_summary$site == "S-312-428")]
site_summary$PIEN_seed_dist[which(site_summary$site == "S-312-428")]
site_summary$PIPO_seed_dist[which(site_summary$site == "S-312-428")]
site_summary$PSME_seed_dist[which(site_summary$site == "S-312-428")]
# only PSME has value: 153

site_summary$LAOC_seed_dist[which(site_summary$site == "S-312-428")] <- NA
site_summary$min_live_seed_dist[which(site_summary$site == "S-312-428")] <- 153
tripod$LAOC_seed_dist[which(tripod$site == "S-312-428")] <- NA
tripod$min_live_seed_dist[which(tripod$site == "S-312-428")] <- 153

### FEB 2018: FOR INDIVIDS WITH 11 YRS OF GRWTH, I COMPUTED GRWTH_2006, WHICH IS BASICALLY GERMINATION YR GRWTH.
# I do not have this value for any other records -- only the ~31 or so for which I record 11 yrs of grwth.
# So, I'm dropping this column. The first growth metric (which may be grwth_2007 or later) reflects...
# grwtth that happened during the first yr of NOT being a germinant. Sprouted in 2006? Then grwth_2007...
# gives the distance btwn the budscar that caps 2006 grwth and the budscar that caps 2007 grwoth.

tripod$grwth_2006 <- NULL

### FEB 07 2018: QUITE SURE WE MIS-ID'ED PSME SEEDLINGS. 
# # Majority are at S-123-051, which upon inspiection of photos...
# # don't match PSME seedling in Franklin 1961 ID. See what happens when I change to UNKN.
# # Need to add new factor level first or else they'll get set as NAs
# levels(tripod$spp) <- c(levels(tripod$spp), "UNKN")
# # tripod$spp[which(tripod$site == "S-123-051" & tripod$spp == "PSME" & tripod$seedling == "YES")] <- "UNKN"
# # Alt if I want to convert ALL seedling PSME (alleged PSMEs) to UNKN
# tripod$spp[which(tripod$spp == "PSME" & tripod$seedling == "YES")] <- "UNKN"
# tripod[which(tripod$site == "S-123-051" & tripod$spp == "UNKN"),]

### FEB 12 2018: BRIAN HARVEY SAYS HE ALWAYS DROPS GERMINANTS. OK B/C MIDWAY THRU SUMMER, TOO.
# I could exclude SEEDLINGS = YES, but in setting spp to seedlings...
# ...I don't have to change all sp-specific code in subsequent summaries.
levels(tripod$spp) <- c(levels(tripod$spp), "SEEDLING")
tripod$spp[which(tripod$seedling == "YES")] <- "SEEDLING"

levels(tripod$spp)
# There's one record that has LAOC recorded with an extra space.
tripod$spp[which(tripod$spp == "LAOC ")] <- "LAOC"
tripod$spp <- droplevels(tripod$spp) # Drops that now-unused level
levels(tripod$spp)


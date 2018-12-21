library(dplyr)

### PRISM VIA WEST SIDE DROUGHT TRACKER DATA
climate <-  read.csv("ECascadesDiv_PRISM_all.csv")

# Rename month names with numbers (Jan = 01)
lu_month <-  read.csv("lu_month.csv",header = FALSE)
lu_month$V1 <- as.character(lu_month$V1)
lu_month$V1[2:13] <-
  substr(as.character(lu_month$V1[2:13]),
         start = 2,
         stop = 4) # to remove extra spaces
match(names(climate), lu_month$V1) # yup, index values are correct
# assign new names
names(climate) = lu_month$V2[match(names(climate), lu_month$V1)]
remove(lu_month)

# Add leading m to month
names(climate) <-
  c("year", paste0("m", names(climate[2:13])), "metric")

# Or, since I only have 5 climate variables, don't make such a mess and do them separately
wwdt_temp <- climate %>%
  filter(metric == "temp") %>%
  dplyr::select(-metric)
rownames(wwdt_temp) <- wwdt_temp$year
wwdt_temp$year <- NULL

wwdt_precip <- climate %>%
  filter(metric == "precip") %>%
  dplyr::select(-metric)
rownames(wwdt_precip) <- wwdt_precip$year
wwdt_precip$year <- NULL

wwdt_spei <- climate %>%
  filter(metric == "spei") %>%
  dplyr::select(-metric)
rownames(wwdt_spei) <- wwdt_spei$year
wwdt_spei$year <- NULL

# wwdt_spi <- climate %>%
#   filter(metric == "spi") %>%
#   dplyr::select(-metric)
# rownames(wwdt_spi) <- wwdt_spi$year
# wwdt_spi$year <- NULL

wwdt_pdsi <- climate %>%
  filter(metric == "pdsi") %>%
  dplyr::select(-metric)
rownames(wwdt_pdsi) <- wwdt_pdsi$year
wwdt_pdsi$year <- NULL




### PRISM VDP DIRECT
climate <-  read.csv("VPDmax.csv")
climate <- climate %>%
  dplyr::filter(Year < 2017) %>%
  dplyr::rename(year = Year, month = Month, VPDmax = vpdmax..hPa.)
climate$month <- climate$month %>%
  formatC(., width=2, format = "d", flag = "0")
climate$month <- paste0("m",climate$month)
vpdmax <- climate %>%
  spread(key = month, value = VPDmax)
rownames(vpdmax) <- vpdmax$year ; vpdmax$year <- NULL

#
# 
# 
# 
# ### nClimDiv
# climate <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/Tripod/Dir/nClimDiv.csv")
# 
# # Keep variables I want
# climate <- climate %>%
#   dplyr::select(year = Year, month = Month, precip = PCP, tavg = TAVG, tmax = TMAX, tmin = TMIN, pdsi = PDSI) %>%
#   filter(! year == "2017")
# 
# # Add leading zero to month then add leading m
# climate$month <- climate$month %>%
#   formatC(., width=2, format = "d", flag = "0")
# climate$month <- paste0("m",climate$month)
# 
# precip <- climate %>%
#   dplyr::select(year, month, precip) %>%
#   spread(key = month, value = precip)
# rownames(precip) <- precip$year ; precip$year <- NULL
# 
# tavg <- climate %>%
#   dplyr::select(year, month, tavg) %>%
#   spread(key = month, value = tavg)
# rownames(tavg) <- tavg$year ; tavg$year <- NULL
# 
# tmax <- climate %>%
#   dplyr::select(year, month, tmax) %>%
#   spread(key = month, value = tmax)
# rownames(tmax) <- tmax$year ; tmax$year <- NULL
# 
# tmin <- climate %>%
#   dplyr::select(year, month, tmin) %>%
#   spread(key = month, value = tmin)
# rownames(tmin) <- tmin$year ; tmin$year <- NULL
# 
# pdsi <- climate %>%
#   dplyr::select(year, month, pdsi) %>%
#   spread(key = month, value = pdsi)
# rownames(pdsi) <- pdsi$year ; pdsi$year <- NULL

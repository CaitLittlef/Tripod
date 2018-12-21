### Initial attempt (below) didn't work. See code adapted from Onset forum (line 56)
# Just load processed data here (which is result of code starting at line 56):
snowfree <- read.csv("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Tripod/Dir/HOBOs/snowfree.csv")


################################################# 
################ INITIAL ATTEMPT ################ 
################################################# 

# # Get the file names of all my HOBO .csvs
# temp <- list.files(pattern = "*.csv")
# # Loop through to read them, assigning names.
# # Useless almost-empty first row messed header assignment up
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], header = FALSE))
# 
# # Compile all csvs into a list; remove individual dataframes (the .csv objects)
# l.hobo <- setNames(lapply(ls(pattern=".csv"), function(x) get(x)), ls(pattern=".csv"))
# remove(list = ls(pattern = ".csv"))
# 
# # says, show me 4th element of my list, 1st row, 1st column
# l.hobo[[4]][1,1] # gives me stand-alone "﻿Plot Title:  10914606"
# 
# # Remove plot title (OK b/c HOBO serial # stored as .csv prefix & in temp & RH names)
# # Also removing row 2, which has variable name (date/time, temp, RH)
# # Keep only col 2, 3, 4 (VALUES of date/time, temp, RH)
# l.hobo2 <- lapply(l.hobo, function(x) x[-(1:2),2:4]) ; remove(l.hobo)
# 
# # Check
# l.hobo2[[4]][1,1] # First record of date/time is now in first row of that 4th list element
# 
# # Set variables as column names
# l.hobo3 <- lapply(l.hobo2, setNames, nm = c("date", "temp", "RH")) ; remove(l.hobo2)
# 
# # Check
# colnames(l.hobo3[[4]]) # gives date, temp, RH.
# 
# # Wrangle date/times (maybe drop times or convert to day of yr or integer that includes time
# # install.packages("chron")
# library(chron) # Appaently fails on a factor
# # Turn first column of each element into a character (not factor)
# # Recall lapply says to apply the following thing (here, explictly function) to each object
# # x[] says to keep as dataframe, and x says to return x within that loop
# l.hobo4<- lapply(l.hobo3, function(x) {
#   x[] <- lapply(x, as.character)
#   x
# })
# 
# # # Can I do that again but keep temp and RH as numeric? 
# # test <- lapply(l.hobo3, function(x) {
# #   x[] <- cbind(lapply(x[,1], as.character), lapply(x[,2:3], as.numeric))
# #   x
# # })
# # Nope. Dropped all temp, RH data.


########################################################## 
################ ADAPTED FROM ONSET FORUM ################ 
##########################################################

# # Requirements: .csv files named with the site name or serial number....
# read.format.hobo <- function(dataFile, skiprows,...)
# {
#   df4 <- read.csv(dataFile, header = TRUE, skip = skiprows, sep = ",")
#   nametemp <- sub(".*temp.*", "TempF", colnames(df4), ignore.case = TRUE)
#   colnames(df4) <- sub(".*RH.*", "RH", nametemp, ignore.case = TRUE)
#   nametemp <- sub(".*date.*", "DateTime", colnames(df4), ignore.case = TRUE)
#   colnames(df4) <- nametemp
#   df3<- df4[,c("DateTime", "TempF", "RH")]
#   nameSep<-unlist(strsplit(dataFile, "[_.]")) # unlist/split filename.
#   df1<- cbind(SN = paste(nameSep[1]), df3) # paste serial number into 1st col.
#   df<- na.omit(df1)
# }
# 
# # Apply this function ^ to all csvs and then dump into a single dataframe
# setwd("HOBOs/readout_data")
# Files <- Sys.glob("*.csv") # list all .csv files in working directory.
# out <- lapply(Files, FUN = read.format.hobo, skiprows = 1) # apply custom function to each file.
# library(plyr)
# hobos <- ldply(out, data.frame) # changes 'out' list to dataframe.
# # write.csv(hobos, "//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/Tripod/Dir/HOBOs/all_hobo.csv") # for safekeeping
# # hobos <- read.csv("//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/Tripod/Dir/HOBOs/all_hobo.csv")

# Extract date
# hobos$Date <- substr(as.character(output$DateTime), start = 1, stop = 8)
# hobos <- hobos %>%
#   dplyr::select(-DateTime) %>%
#   dplyr::select(SN, Date, TempF, RH)
# 
# # Count # snow-free days.
# # Select records w/ RH at 100%. I'll likely miss a few w/ snow, but inter-site comparison OK.
# # Snowpack can get below 32, esp. near surface (e.g., radiative loss to atmos). Set TempF <34
# # AND, HOBOs weren't at ground-level. So, likely missing some snow-covered days at beginning/end.
# # Some summer records still match this (RH=100, temp<34), so require 24 records/day.
# snobos <- hobos %>%
#   filter(TempF < 34, RH == 100)
# # Even temp grouping kicks out df attributes. Save 'em:
# attr <- attributes(snobos)
# snobos <- snobos %>%
#   dplyr::group_by(SN, Date) %>%
#   dplyr::mutate(nhours = n()) # count number of hrs that fit bill.
# attributes(snobos) <- attr ; remove(attr)
# # nhours hadn't been in attributes; rename
# snobos$nhours <- snobos[,5] ; snobos[,5] <- NULL
# snobos <- snobos %>%
#   filter(nhours == 24)
# snobos <- snobos %>%
#   group_by(SN) %>%
#   dplyr::summarise(snowdays=n_distinct(Date)) %>% # not specifying distinct includes each of 24 hrs/day * dates
#   dplyr::mutate(snowfree=365-snowdays)
# 
# # Grab look-up to associate sites with HOBO numbers. Recall don't have data for some (trashed)
# lu <- read.csv("//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/Tripod/Dir/HOBOs/lu_hobos.csv")
# lu$HOBO.sn<- as.factor(lu$HOBO.sn)
# snowfree <- right_join(lu, snobos, by = c("HOBO.sn" = "SN"))
# snowfree <- snowfree %>% dplyr::select(site, snowfree)
# # write.csv(snowfree, "//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/Tripod/Dir/HOBOs/snowfree.csv")
# 
# remove(hobos, snobos, lu)







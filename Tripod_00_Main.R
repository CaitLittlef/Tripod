
### WD
setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Tripod/Dir")
# setwd("D:/Shared/BackedUp/Caitlin/Tripod/Dir")


# ### PACKAGES
install.packages("readr")
install.packages("tidyverse")
# install.packages("dplyr")
#install.packages("vegan")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ggthemes")
# install.packages("RColorBrewer")
install.packages("ape")
install.packages("MASS")
install.packages("gstat")
install.packages("lattice")
install.packages("relaimpo")
# install.packages("quantreg")
install.packages("pscl")
# install.packages("countreg")
install.packages("pROC")
install.packages("ResourceSelection")
# install.packages("sjPlot")
install.packages("dunn.test")
install.packages("lmtest")



### LIBRARIES
library(readr)
library(tidyverse)
# library(dplyr)
#library(vegan)
library(ggplot2)
library(reshape2)
library(ggthemes)
# library(RColorBrewer)
library(ape)
library(MASS)
library(relaimpo)
library(gstat)
library(lattice)
# library(quantreg)
library(pscl)
# library(countreg)
library(pROC)
library(ResourceSelection)
# library(sjPlot)
library(dunn.test)
library(lmtest)




### SOURCE CODE

# DON'T USE. Created initial csvs, doesn't work well. Just join/tweak any data in later. 
# source("Tripod_01a_LoadInitialFix.R") 

# Loads csvs that merged initial csvs in Tripod_01a_LoadInitialFix.R
# I COULD drop certain sites in here (e.g., "S-312-428", "S-221-211")
# Or, could change offending datapt (e.g,. non-mature LAOC_seed_dist in S-312-428)
# This is also where I set any/all seedlings to spp = SEEDLING and then...
# ... exclude fom summaries in 01d codes. Long story short, saw waaay too many seedlings
# ... coded as PSME. Pics (S-123-051) suggest they should have been pine.
# Questionable to just chng those so I'm excluding all seedlings. (which Brian Harvey does)
# ^ THIS IS WHAT'S CURRENTLY ACTIVE.
source("Tripod_01b_LoadForReals.R") 

# Summarizes cover from plots to sites
source("Tripod_01c_CreateAppendPlotSummaries.R")

# Creates individual tables for each species.
# Obs exist ONLY for sites with that spp
# Can remove below.
source("Tripod_01d_CreateAppendSppSummaries.R")

# Joins all site info to spp tables.
# Obs exist for ALL sites, setting thp=0 for empty sites.
# Can remove below.
source("Tripod_01d2_CreateAppendSppSummaries.R")

# Based on var correlations (and other rationale), maintaining only certain predictor variables.
# Drop 'em and clean 'em here. Can turn on/off growth and additional topo (incl. zone std) variables
#source("Tripod_01e_VariableCollinearity.R") 
source("Tripod_01f_VarClean.R")  #CAN TURN ON-OFF GROWTH VARIABLES IN HERE! 


# OK, kinda out of order (new as of Jan 2018), but I want a presence-absence data-set...
# ...and don't want to miss with prior cleaning in above code _01f.
# This also changes the stupid ABLA_seed_dist to abla_seed_dist...
# And creates bins for distances to seed source so I don' thave NAs
source("Tripod_01d3_CreatePresAbs-SeedBins.R")

# Load climate data
source("Tripod_01g_ClimateData.v2.R")

# Load HOBO data. Processing code commented out, just load it.
source("Tripod_01h_HOBO.R")


# irrelevant -- cleaning done beforehand (Excel), already loaded above in 01_Load
#source("Tripod_02_Clean.R") 
#source("Tripod_03_Reload.R") 

# Load functions
source("Tripod_04_Functions.R")




# ## SHORT-CUTS
remove(abla_sum)
remove(laoc_sum)
remove(pico_sum)
remove(pien_sum)
remove(pipo_sum)
remove(psme_sum)
# #
remove(abla_sum_all)
remove(laoc_sum_all)
remove(pico_sum_all)
remove(pien_sum_all)
remove(pipo_sum_all)
remove(psme_sum_all)



###FIXME:
#"Tripod_01x_CreateSppSummaries.R" # to replace 01d with loop
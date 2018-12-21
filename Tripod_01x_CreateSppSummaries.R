### OPTIONAL INDIVID SPP TABLES
# Prior to this, I had been subsetting tripod each time to make independent summary table for each species.
# Here, I should only have to change summary code once and loop through each table (1 for each spp)
# In future, figure out way to not even have indepenmdent tables for each spp?

attach(tripod)
abla <- tripod[ which(spp == "ABLA"),]
laoc <- tripod[ which(spp == "LAOC"),]
pico <- tripod[ which(spp == "PICO"),]
pien <- tripod[ which(spp == "PIEN"),]
pipo <- tripod[ which(spp == "PIPO"),]
psme <- tripod[ which(spp == "PSME"),]
detach(tripod)


mylist <- list()
mylist[[1]] <- tripod 
mylist[[2]] <- abla  
mylist[[3]] <- laoc
mylist[[4]] <- pico 
mylist[[5]] <- pien 
mylist[[6]] <- pipo 
mylist[[7]] <- psme

# fixme

# for (i in 1:length(mylist)){
#   temp <- i %>%
#     group_by(i, site) %>%
#     summarize_at(c("tpha"), funs(sum(., na.rm=TRUE))) # thpa
#   temp <- temp[,c("site","tpha")]
#   temp <- site_summary %>%
#     right_join(temp, by="site") # joins to site summary data
#   temp2 <- i %>%
#     group_by(i, site) %>%
#     summarize_at(c("ttl_ht"), funs(mean(., na.rm=TRUE))) # height
#   colnames(temp)[which(names(temp) == "ttl_ht")] <- "avg_ht"
#   temp2 <- temp2[,c("site","avg_ht")]
#   temp <- temp2 %>%
#     right_join(temp, by="site")
#     remove(temp2)
# }



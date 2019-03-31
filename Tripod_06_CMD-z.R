## Extracting climatic water deficit z-scores for Tripod study area

# Grab date for saving files
currentDate <- Sys.Date()

# Set directory for climatic deficit z-scores
def.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/def_z"



################################################# STACK DEF, GET TRIPOD STUDY AREA
## Compile all def z-scores as raster into a list.
def.list <- lapply(list.files(def.dir, pattern = ".tif$", full.names = TRUE),
              raster) # applies FUN (here, raster) to all files; dumps into list; $=end
plot(def.list[[i]])

# What's Terraclim CRS? 
crs(def.list[[1]])
evi.crs <- paste0(st_crs(EVI)[2]) # [2] to drop EPSG code

# Create stack of all def-z rasters
rst = stack(def.list)
names(rst)



## Load Tripod study area
tripod <- st_read("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Tripod/Fire/TriPeri.shp")
plot(st_geometry(tripod), add =T)
class(tripod)

# What's Tripod crs?
st_crs(tripod)
tripod <- st_transform(tripod, st_crs(rst[[1]]))
identical(st_crs(tripod), st_crs(rst[[1]])) # but nb crs() won't be identical


#####################################
### CROP EVI DATA TO TRIPOD PERIMETER
rst.tri <- crop(rst, tripod) # geographic subset
rst.tri <- mask(rst.tri, tripod) # converts NA in y to NA in x.
rst.tri <- trim(rst.tri) # removes extraneous NAs
# ^ In this case, buffer was already so small, mask or trim didn't do anything new.

plot(rst.tri[[1]]) ; plot(st_geometry(tripod), add = TRUE)

## Alt: use box around Tripod for regional trends, not just perimeter.
tri.box <- st_make_grid(tripod, n = 1) # Creates new spatial object; n = 1 for no grid
st_crs(tri.box)
tri.box <- st_buffer(tri.box, 0.02)
tri.box <- as(tri.box, 'Spatial') # b/c crop doesn't play well with simple features
plot(tri.box) ; plot(tripod, add = TRUE)
rst.box <- crop(rst, tri.box)
rst.box <- mask(rst.box)
rst.box <- trim(rst.box)

###########################################
## GET AVERAGE DEFICIT VALUES WITHIN TRIPOD 
###########################################

temp <- NULL
cmd.avg <- NULL
loop.ready <- c(1:length(def.list)) # don't do length rst else get all cells
for(i in loop.ready){
  temp <-cellStats(rst.tri[[i]], mean)
  cmd.avg <- cbind(cmd.avg, temp)
  }
cmd.avg <- as.data.frame(t(cmd.avg)) # to make 1 column.
cmd.avg$yr <- as.character(NA)
cmd.avg$yr <- names(rst.tri)
cmd.avg <- cmd.avg %>% rename(avg = V1) %>% dplyr::select(yr, avg)
rownames(cmd.avg) <- NULL

# Check it. 
i <- 50
cellStats(rst.tri[[i]], mean) ; cmd.avg[i,2] ; plot(rst.tri[[i]]) ; plot(st_geometry(tripod), add = TRUE)

# separate by 5-9 and by 6-8
cmd.avg.5.9 <- cmd.avg[right(cmd.avg$yr,3)=="5.9",]
cmd.avg.5.9$yr <- mid(cmd.avg.5.9$yr, 5,4) %>% as.numeric()
cmd59 <- filter(cmd.avg.5.9, yr > 2006 & yr < 2017)
cmd.avg.6.8 <- cmd.avg[right(cmd.avg$yr,3)=="6.8",]
cmd.avg.6.8$yr <- mid(cmd.avg.6.8$yr, 5,4) %>% as.numeric()
cmd68 <- filter(cmd.avg.6.8, yr > 2006 & yr < 2017)

cmd59 %>% 
  filter(yr < 2012) %>%
  summarize(mean = mean(avg))
cmd59 %>%
  summarize(mean = mean(avg))


###########################################
## GET AVERAGE DEFICIT VALUES WITHIN BOX 
###########################################

temp <- NULL
cmd.avg.box <- NULL
loop.ready <- c(1:length(def.list)) # don't do length rst else get all cells
for(i in loop.ready){
  temp <-cellStats(rst.box[[i]], mean)
  cmd.avg.box <- cbind(cmd.avg.box, temp)
}
cmd.avg.box <- as.data.frame(t(cmd.avg.box)) # to make 1 column.
cmd.avg.box$yr <- as.character(NA)
cmd.avg.box$yr <- names(rst.box)
cmd.avg.box <- cmd.avg.box %>% rename(avg = V1) %>% dplyr::select(yr, avg)
rownames(cmd.avg.box) <- NULL

# Check it. 
i <- 5
cellStats(rst.box[[i]], mean) ; cmd.avg.box[i,2] ; plot(rst.box[[i]]) ; plot(tri.box, add = TRUE)

# separate by 5-9 and by 6-8
cmd.avg.box.5.9 <- cmd.avg.box[right(cmd.avg.box$yr,3)=="5.9",]
cmd.avg.box.5.9$yr <- mid(cmd.avg.box.5.9$yr, 5,4) %>% as.numeric()
cmdbox59 <- filter(cmd.avg.box.5.9, yr > 2006 & yr < 2017)
cmd.avg.box.6.8 <- cmd.avg.box[right(cmd.avg.box$yr,3)=="6.8",]
cmd.avg.box.6.8$yr <- mid(cmd.avg.box.6.8$yr, 5,4) %>% as.numeric()
cmdbox68 <- filter(cmd.avg.box.6.8, yr > 2006 & yr < 2017)






## Step 2 Get predictor data for aoi ####

### Prepares predictors (crops, masks) and puts all into stack for SDM


sourceProtection

setwd("")
dir()

library(sf)
options(sf_max.plot=1)
library(raster)

# sirgas for BR
sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # as in arcgis

# set temp path for raster somewhere where it won't be deleted after cluster clozses
# rasterOptions()
# rast_tmp <- "C:/CD_C/raster_tmp"
# rasterOptions(tmpdir = rast_tmp)
# rasterOptions()
# can wipe this after session

## Get occurrecne and aoi data
load("gis/occ_aoi.rdata")
rm(bl.sirgas)

# make rough wgs extent to crop with
# get wgs extended to crop
r <- raster(msk.aoi)
wgs.aoi <- extent(projectExtent(extend(r, 10), wgs))
wgs.aoi
rm(r)

### Get new predictors and old with proper extent

## 1 World clim ####
getwd()
plot(msk.aoi)
# writeRaster(msk.aoi, filename = "gis/r_sirg/msk_aoi.tif", datatype = "INT1U", overwrite = T)
# 

getwd()
# get files 
dir("../../GIS_info/Clim/wrldclm")
wc.fn <- list.files("../../GIS_info/Clim/wrldclm", pattern = "\\.tif$", full.names = T)

wc.stck <- stack(wc.fn)
proj4string(wc.stck)

# crop 
wc.stck <- crop(wc.stck, y = wgs.aoi)
wc.stck
#plot(wc.stck)

beginCluster()
wc.sir <- projectRaster(wc.stck, to = msk.aoi, method = "bilinear")
endCluster()

names(wc.sir)
names(wc.sir) <- gsub(" ", "0", sprintf("bio%02s", 1:19))

wc.sir$bio01
plot(wc.sir$bio01)

## mask to projected aoi
wc.sir <- mask(wc.sir, msk.aoi) # 
plot(wc.sir$bio01)

wc.sir$bio01

plot(wc.sir[[1:10]])
plot(wc.sir[[11:19]])

# check in memory
sapply(1:nlayers(wc.sir), function(x) inMemory(wc.sir[[x]]))

# # readinto memory
# wc.sir <- readAll(wc.sir)
wc.sir

# clean up
rm(wc.stck)
## SAVE as single file... 
writeRaster(wc.sir, filename = "gis/r_sir/wc.tif", bylayer = F)
wc.names <- names(wc.sir)

### 2. DHI #####
## Do same with DHI layers
## see https://www.sciencedirect.com/science/article/pii/S0034425718305625

## Downloaded from http://silvis.forest.wisc.edu/data/dhis/

dir("../../GIS_Info/cover/DHI", pattern = "\\.tif$")
dhi.fn <- list.files("../../GIS_Info/cover/DHI", pattern = "f\\.tif$", full.names = T) 
# just combined layers 2003-2014
dhi.fn

# can't stack them all as they have different extents... (for some reason...)
dhi.rList <- lapply(dhi.fn, stack) # use stack as these are multilayer rasters
dhi.rList

dhi.crp.rList <- lapply(dhi.rList, crop, y = wgs.aoi)

beginCluster()
dhi.sirg <- lapply(dhi.crp.rList, projectRaster, to = msk.aoi, method = "bilinear")
endCluster()

dhi.sirg.crp <- lapply(dhi.sirg, crop, y = msk.aoi)
dhi.sirg.msk <- lapply(dhi.sirg.crp, mask, mask = msk.aoi)
dhi.sir <- stack(dhi.sirg.msk)

dhi.sir
names(dhi.sir)
names(dhi.sir) <- unlist(lapply(c("evi", "gpp", "lai", "ndvi"), paste, c("cum", "min", "sea"), sep ="_"))
plot(dhi.sir)

sapply(1:nlayers(dhi.sir), function(x) inMemory(dhi.sir[[x]]))

writeRaster(dhi.sir, filename = "gis/r_sir/dhi.tif", bylayer = F)
dhi.names <- names(dhi.sir)

#clean
rm(dhi.sirg, dhi.crp.rList, dhi.fn, dhi.rList, dhi.sirg.crp, dhi.sirg.msk)



### 3. SRTM #####

# get raster SRTM data
cty <- c("PRY", "BRA", "ARG")
srtm <- list()

for(i in seq_along(cty)){
  
  tmp <- raster::getData("alt", country = cty[i], path = "gis/wgs", level = 1)
  srtm[[i]] <- tmp
  rm(tmp)
}

rm(cty, i)

srtm
lapply(srtm, plot)

plot(msk.aoi)
plot(st_geometry(cty0.sir), add = T)

# project each country
beginCluster()
srtm.sir <- lapply(srtm, projectRaster, to = msk.aoi, method = "bilinear")
endCluster()

lapply(srtm.sir, plot)

## merge (make sure NAs are not taken in first layer)
# add arguments
# OK - overlap is TRUE by default
srtm.mos <- do.call(merge, srtm.sir)

dem.sir <- crop(srtm.mos, msk.aoi)
dem.sir <- mask(dem.sir, msk.aoi)

plot(dem.sir, asp = 1)
plot(st_geometry(cty0.sir), add = T, col = NA)

terr.sir <- terrain(dem.sir, opt = c("slope", "aspect", "TPI", "TRI", "roughness"), units = "degrees")

terr.sir <- addLayer(terr.sir, dem.sir)
names(terr.sir)
names(terr.sir)[6] <- "dem"

terr.sir[[1]] # in memory

names(pred)
terr.sir <- dropLayer(pred, 7:37)

writeRaster(terr.sir, filename = "gis/r_sir/terr.tif", bylayer = F)
terr.names <- names(terr.sir)

# save all.
rm(srtm.mos, srtm, srtm.sir, dem.sir)

## 4. Create stack of all predictors ####

## Line them all up... 
compareRaster(terr.sir, dhi.sir, wc.sir, orig = T)

predStack <- stack(terr.sir, dhi.sir, wc.sir)
predStack

# save as single file
writeRaster(predStack, filename = "gis/r_sir/predStack.tif", bylayer = F, overwrite = T)
pred.names <- names(predStack)
pred.names
save(pred.names, wc.names, dhi.names, terr.names, file = "gis/r_sir/predNames.rdata")

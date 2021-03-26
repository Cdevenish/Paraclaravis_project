##  1. Occurrence points ####

## Get occurrence points, set up modelling area

setwd("")

# sirgas for BR
sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # as in arcgis

library(sf)
options(sf_max.plot=1)

## import points
pcl.all <- read.csv("occ/Paraclaravis.csv", stringsAsFactors = F)
str(pcl.all)

pcl <- subset(pcl.all, !is.na(Lat) | !is.na(Lon))
str(pcl)
summary(pcl)

plot(pcl$Lon, pcl$Lat, asp = 1)

# convert to sf
pcl.sf <- st_as_sf(pcl, coords = c("Lon", "Lat"), crs = wgs)

# write to shapefile
# st_write(pcl.sf, "gis/wgs/pcl.shp")

table(cut(pcl$Year, 8))
barplot(table(cut(pcl$Year, 8)))

getData("ISO3")

cty <- c("PRY", "BRA", "ARG")
cty1List <- list()
cty0List <- list()

for(i in seq_along(cty)){
  
  cty1.sp <- raster::getData("GADM", country = cty[i], path = "gis/wgs", level = 1)
  cty0.sp <- raster::getData("GADM", country = cty[i], path = "gis/wgs", level = 0)
  
  cty1 <- st_as_sf(cty1.sp)
  cty0 <- st_as_sf(cty0.sp)
  
  cty1 <- st_transform(cty1, wgs)
  cty0 <- st_transform(cty0, wgs)
  
  cty1List[[i]] <- cty1
  cty0List[[i]] <- cty0
  
  rm(cty1.sp, cty0.sp, cty1, cty0)
  
}

cty1 <- do.call(rbind, cty1List)
cty0 <- do.call(rbind, cty0List)

cty1.sir <- st_transform(cty1, sirgas)
cty0.sir <- st_transform(cty0, sirgas)

rm(cty1List, cty0List, i, cty1, cty0)

cty0

plot(st_geometry(cty0), col = NA)
plot(st_geometry(pcl.sf), col = "darkred", pch= 20, add = T)

plot(st_geometry(pcl.sf), col = "darkred", pch= 20, add = F)
plot(st_geometry(cty0), col = NA, add = T)


## Make sure all points are within ecoregion, country, etc
## convert all to sirgas (check UTM  cells if applicable)

pcl.sir <- st_transform(pcl.sf, sirgas)

# Make model area
# import BL shapefile
## Downloaded from IUCN - requires permission from Birdlife
bl <- st_read("gis/wgs/claravis_geof_BL.shp")
bl.sirgas <- st_transform(bl, sirgas)
# plot(st_geometry(bl), add = T)

# buffer around unioned points
pcl.buff <- st_buffer(st_convex_hull(st_union(pcl.sir)), dist = 50000)

plot(st_geometry(pcl.sir), col = "darkred", pch= 20, add = F)
plot(st_geometry(cty0.sir), col = NA, add = T)
plot(st_geometry(pcl.buff), col = NA, add = T)

plot(st_geometry(bl.sirgas), border = "darkgreen", add = T)

## add to BL extnte (union BL first)
aoi <- st_buffer(st_convex_hull(st_union(st_union(bl.sirgas), pcl.buff)), 5000)

## get extents form pcl.buff for aoi
aoi
plot(aoi, add = T, border = "blue")

st_bbox((aoi))

aoi.ext <- adjExt(st_bbox(aoi), d = 1000, outF = "Extent")

r <- raster(aoi.ext, crs = sirgas, res = 1000)
r

r[] <- 1

msk.aoi <- raster::mask(r, st_as_sf(aoi))
plot(msk.aoi)

save(pcl.sir, msk.aoi, cty1.sir, cty0.sir, bl.sirgas, file = "gis/occ_aoi.rdata")

## EOO in different time bands
load("gis/occ_aoi.rdata")

ch_all <- st_convex_hull(st_union(pcl.sir))

st_area(ch_all) / 1000000
# 834,642.8 # km2

ch_1990 <- st_convex_hull(st_union(subset(pcl.sir, Year >= 1990)))
st_area(ch_1990) /1000000
# 277,650 km2
sum(pcl.sir$Year >= 1990) # 14

ch_2005 <- st_convex_hull(st_union(subset(pcl.sir, Year >= 2005)))
st_area(ch_2005) / 1000000
# 95,045.97 km
sum(pcl.sir$Year >= 2005) # 6

st_write(ch_all, "gis/s_sir/ch_all.shp")
st_write(ch_2005, "gis/s_sir/ch_2005.shp", delete_layer = T)
st_write(ch_1990, "gis/s_sir/ch_1990.shp", delete_layer = T)


## Absence data... 
## from ebird_zeros_v3_2020_pcl.r
load("occ/ebird/ebird_absences.rdata") # ebd.zf, ebird.data, ebird.a
rm(ebd.zf)
# ebird.a is sf object of just PWGD absences

## cut to study areas
msk.aoi
plot(msk.aoi)
aoi.Ind <- raster::extract(msk.aoi, ebird.a)
table(aoi.Ind, useNA = "always")
head(aoi.Ind==1)

# filter just in study area
ebird.aoi <- ebird.a[!is.na(aoi.Ind),]

plot(ebird.aoi, add = T, pch = ".")

save(ebird.aoi, file = "ebirdAbsences_in_aoi.rdata")

st_write(ebird.aoi, "gis/s_sir/absences_aoi.shp", delete_layer = TRUE)

barplot(table(ebird.aoi$year), las = 2)

table(cut(ebird.aoi$year, breaks = c(0, 1990, 2000, 2010, 2020)))

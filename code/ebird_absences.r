
## Makes ebird absences across study area for extinction risk model


library(sf)
library(raster)
library(dplyr)

getwd()

sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# get ebird absences in binary model (from s3 script)
## This is ia shapefile of all the ebird complete checklist locations without the focal species, 
## within the thresholded area of the model
ebird.abs.bin <- st_read("gis/s_sir/ebird_abs_bin.shp", crs = sirgas)

ebird.abs.bin

# buffer and dissolve by year groups
summary(ebird.abs.bin)
table(cut(ebird.abs.bin$year, breaks = c(1972, 2005,2010,2015,2020)))
ebird.abs.bin$yr.grp <- cut(ebird.abs.bin$year, breaks = c(1972, 2005,2010,2015,2020), labels = c("<2005", "2010", "2015", "2020"))

table(ebird.abs.bin$yr.grp, useNA = "always")

# buff.grps <- ebird.abs.bin %>%
#   group_by(yr.grp)%>%
#   st_buffer(dist = 5000)%>%
#   st_union()
# 
# 
# buff.grps

ebird2010 <- subset(ebird.abs.bin, yr.grp == "2010")
ebird2015 <- subset(ebird.abs.bin, yr.grp == "2015")
ebird2020 <- subset(ebird.abs.bin, yr.grp == "2020")

ebird2010 <- st_union(st_buffer(ebird2010, dist = 5000))
ebird2015 <- st_union(st_buffer(ebird2015, dist = 5000))
ebird2020 <- st_union(st_buffer(ebird2020, dist = 5000))

plot(st_geometry(ebird.abs.bin), pch = 16)
plot(ebird2010, add = T, col = "red")
plot(ebird2015, add = T, col = "blue")
plot(ebird2020, add = T, col = "green")


# get binary modelled range (from s3_model script)
bin.range <- raster("gis/r_sir/models/modRes_habt_bin_mf.tif")
bin.range

plot(bin.range)
plot(ebird2020, add = T)
# 
# 
# # convert raster to polygon
# # convert 0 to NA
bin.range[bin.range == 0] <- NA
bin.range.poly <- rasterToPolygons(bin.range, dissolve = TRUE)

bin.range.poly
plot(bin.range.poly, add = T)
# 
bin.range.poly <- st_as_sf(bin.range.poly)

# intersect ebird absences with range for estimate of range surveyd 

## give range not in ebird2020
# diff2020 <- st_difference(bin.range.poly, ebird2020)
# 
# plot(st_geometry(bin.range.poly))
# plot(ebird2020, col = "blue", add = T)
# plot(diff2020, add = TRUE, col = "green")

# ebird dissolved buffers represent area surveyed as these absences are already only those inside modelled range

st_area(ebird2020) / st_area(bin.range.poly)
# 0.3226884 [1]
st_area(ebird2015) / st_area(bin.range.poly)
# 0.148272 [1]
st_area(ebird2010) / st_area(bin.range.poly)
# 0.068318 [1]

save(ebird2010, ebird2015, ebird2020, ebird.abs.bin, bin.range.poly, file = "gis/ebird_survey_data.rdata")
# load("gis/ebird_survey_data.rdata")
# rm(ebird2010, ebird2015, ebird2020)


ebird.abs.bin <- st_read("gis/s_sir/ebird_abs_bin.shp", crs = sirgas)

# make absences per year

res <- list()

range.area <- st_area(bin.range.poly) # 1.74489e+11 [m^2]
yrs <- 2005:2020

st_area(bin.range.poly) / 1000000
174489 # km2

for(i in yrs){
  
  tmp.yr <- subset(ebird.abs.bin, year == i)
  tmp.yr <- st_union(st_buffer(tmp.yr, dist = 5000))
  
  yr.res <- cbind(year = i, 
        area = st_area(tmp.yr),
        prop.total = st_area(tmp.yr) / range.area)
  
  res[[which(i == yrs)]] <- yr.res
    
}

yrRes <- do.call(rbind, res)

save(yrRes, ebird.abs.bin, bin.range.poly, file = "gis/ebird_absence_eps_prop.rdata")




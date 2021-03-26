## Step 3. Model ESM ###

## Creates SDM following method by breinman for mdoels with small numbers of  occurrence points


sourceProtection

setwd("")
dir()

library(sf)
options(sf_max.plot=1)
library(raster)
library(dismo)
library(ecospat)

# sirgas for BR (epgs 5880)
# EPSG:5880
# SIRGAS 2000 / Brazil Polyconic
# proj4.defs("EPSG:5880","+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs");

sirgas <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # as in arcgis

source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")

# Modified from code example for calculating an "Ensemble of Small Models" (ESM) in R - Breiman.. supp. materials

## get predictors

# to load in...
load("gis/r_sir/predNames.rdata") # From s2
# pred <- brick("gis/r_sir/predStack.tif")
# names(pred) <- pred.names
# rm(pred.names)
# pred

wc.sir <- brick("gis/r_sir/wc.tif")
names(wc.sir) <- wc.names

dhi.sir <- brick("gis/r_sir/dhi.tif")
names(dhi.sir) <- dhi.names

terr.sir <- brick("gis/r_sir/terr.tif")
names(terr.sir) <- terr.names

rm(wc.names, dhi.names, terr.names, pred.names)

## CHOOSE Predictors

names(terr.sir)
raster::pairs(terr.sir, maxpixels = 1000)

# BIO1 = Annual Mean Temperature - OUT (same as 5, 6,10,11)
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100) - OUT (similar to 4)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month 
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6) - OUT (same as 2)
# BIO8 = Mean Temperature of Wettest Quarter - OUT
# BIO9 = Mean Temperature of Driest Quarter - OUT
# BIO10 = Mean Temperature of Warmest Quarter - OUT (almost same as 11)
# BIO11 = Mean Temperature of Coldest Quarter - OUT (same as 5,6)
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month - OUT (same as 15,17)
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter - OUT (almost same as 13)
# BIO17 = Precipitation of Driest Quarter - OUT  (almost same as 14,15)
# BIO18 = Precipitation of Warmest Quarter - OUT
# BIO19 = Precipitation of Coldest Quarter - OUT

pairs(dropLayer(wc.sir, c(8,9,18,19)), maxpixels = 1000)
pairs(dropLayer(wc.sir, c(1,3,7,8,9,10,11,14,16,17,18,19)), maxpixels = 1000)#  method= doesn;t work here.. 
raster::pairs

pairs(dhi.sir, maxpixels = 1000)
pairs(dropLayer(dhi.sir,c(1,4,7,10:12)), maxpixels = 1000)
dhi.sir[[c(10,12)]]

plot(dhi.sir[[c(10,12)]])
plot(dhi.sir[[c(1,3)]])

writeRaster(dhi.sir[[c(10,12)]], filename = "gis/r_sir/dhi.tif", bylayer = T, suffix = 'names')
writeRaster(dhi.sir[[c(1,3)]], filename = "gis/r_sir/dhi.tif", bylayer = T, suffix = 'names')

## CLIMATE AND LAND COVER MODEL
pred <- stack(dropLayer(wc.sir, c(1,3,7,8,9,10,11,14,16,17,18,19)), terr.sir[[c("dem", "slope")]], dhi.sir[[c(1,3)]])
pred
names(pred)

## CLIMATE ONLY MODEL
# pred <- stack(dropLayer(wc.sir, c(1,3,7,8,9,10,11,14,16,17,18,19)), terr.sir[[c("dem", "slope")]])
# pred


## All correlations below 70

# check maxent
dismo::maxent()

## 2. Occurrence points ####

## get aoi and occurrence points (from s1)
load("gis/occ_aoi.rdata")
rm(bl.sirgas)
cty0.sir

## remove ercords for NA in presence records. Which ones are they?
tmp1 <- extract(pred, pcl.sir)
cc <- complete.cases(tmp1)
sum(!cc)
pcl.sir[!cc,] # 7 cases
tmp1[!cc,]

# st_write(pcl.sir[!cc,], "gis/s_sir/NA_occ_pts.shp")

## NA values correpond to some recods from urban ares, eg SAo paolo, curitiba, vitoria, rio de janeiro
pcl <- pcl.sir[cc,]
rm(cc)

## only for records >= 1990
head(pcl)
sum(pcl$Year >= 1990)
pcl <- pcl[pcl$Year >= 1990,]

## filter per 1 km grid cell
# source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/SDM_functions/filter_pts.r")
# pcl.f <- filter_pts(msk.aoi, pcl)

# don't filter... so few points
pcl.f <- pcl

# st_write(pcl, "gis/s_sir/pcl.shp")
# st_write(pcl.f, "gis/s_sir/pcl_f.shp")

## 3. background points ####


## do background for all
# same background data for all 
set.seed(99)
bg <- randomPoints(pred, 10000)
## check these points are within stck
tmp <- extract(pred, bg)
nrow(bg) - sum(complete.cases(tmp))

# remove outside points (why are they outside??)
bg <- bg[complete.cases(tmp),]
rm(tmp)

# save(pcl, pcl.f, bg, pred, file = "LC_CLIM_modelData.rdata")
# save(pcl, pcl.f, bg, pred, file = "CLIM_modelData.rdata")
rm(tmp1)

## SAVE model data

save(pcl, pcl.f, pred, bg, file = "final_mod_data.rdata")
load("final_mod_data.rdata")

### 4. Extract values ####

## presences:
env.occ <- extract(pred, pcl.f)

# bakcground env data
env.bg  <- extract(pred, bg)

# predictors
env  <- as.data.frame(rbind(env.occ, env.bg))
head(env)

# response coding
resp <- c(rep(1,nrow(env.occ)),rep(0,nrow(env.bg)))

### 5. Explore occ - preds ####

# boxplots of all
par(mfrow = c(3,4))
sapply(1:ncol(env), function(x) boxplot(env[,x] ~ resp, main = colnames(env)[x], xlab = "", ylab = ""))


## outliers
par(mfrow = c(1,1))
head(env.occ)
mDist <- mahalanobis(env.occ, colMeans(env.occ), cov(env.occ))
plot(mDist)
ind <- which(mDist > 20)

# eg compare with outliers on slope and bio6
# par(mfrow = c(2,3))
# colnames(env.occ)
# plot(env.occ[,"bio06"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])
# plot(env.occ[,"bio12"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])
# plot(env.occ[,"dem"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])
# plot(env.occ[,"slope"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])
# plot(env.occ[,"ndvi_cum"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])
# plot(env.occ[,"ndvi_sea"], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1])


par(mfrow = c(3,3))
# par(mfrow = c(3,4))
sapply(1:ncol(env.occ), function(x) plot(env.occ[,x], col = c("black", "red")[(1:nrow(env.occ) %in% ind)+1], 
                                         main = colnames(env.occ)[x], xlab = "", ylab = ""))

rm(ind, mDist)
par(mfrow = c(1,1))
plot(pred)

## check out mvoutlier package.. something similar.

rm(pcl, pcl.sir)

### 6. model starts here ####

# =================================================================================
# 10 runs using a random 50:50 split of data for training and testing ESM vs Maxent
# =================================================================================

# eval.test.st  <-  NULL    # for evaluation results of Maxent standard
eval.test.esm <-  NULL    # for evaluation results of ESM
w10List <- list()

nr <- 20

# i = 2

set.seed(2066)

for (i in 1:nr) {
  
  print(paste("Run",i))
  
  # split data for training and testing ()
  # mykfold <- kfold(resp, k=2, by=resp)
  # table(mykfold, resp)
  # 
  # sel.train  <- mykfold == 1
  # sel.test   <- mykfold != 1
  # resp.train <- resp[sel.train]
  # resp.test  <- resp[sel.test]
  # 
  ## VERSION with all bg points, and only splitting presence for each run.. 
  # split data for training and testing ()
  mykfold <- kfold(1:nrow(env.occ), k=4) # Split into 4, so roughly 3/4 for testing, and 11/12 for training
  
  sel.train <- c(mykfold != 1, rep(T, nrow(env.bg)))
  sel.test <- c(mykfold == 1, rep(T, nrow(env.bg)))
  
  resp.train <- resp[sel.train]
  resp.test  <- resp[sel.test]
  
  table(resp.train)
  table(resp.test)
  
  # standard Maxent
  # st <- maxent(x=env[sel.train,], p=resp[sel.train], args=c("-P","nohinge", "nothreshold", "noproduct"))                
  # pred.st <- predict(st, env)    
  # auc.test <- evaluate(p=pred.st[sel.test ][resp.test ==1], a=pred.st[sel.test ][resp.test ==0])@auc
  # boyce.test <- ecospat.boyce(pred.st[sel.test], pred.st[sel.test][resp.test==1], PEplot=F)# $Pearson.cor
  # eval.test.st <- rbind(eval.test.st, cbind(auc=auc.test,boyce=boyce.test))
  # 
  # # build ESM as weighted average of bivariate Maxent models
  weight.vec <- vector()                 # vector with weights of single bivariate models
  pred.biva.sum <- rep(0, length(resp))  # for ensemble prediction of bivariate models 
  #pred.biva.sum.stck <- rep(0, ncell(stack))
  
  # m = 2; n = 3
  
  weight.vec.m <- matrix(NA, nrow = (dim(env)[2]-1), ncol = dim(env)[2]) # col is no of predictors
  
  for(m in 1:(dim(env)[2]-1)){           # build all bivariate predictor combinations
    for(n in (m+1):dim(env)[2]){
      
      # calibrate bivariate maxent model
      me.biva <- maxent(x=env[sel.train,c(m,n)], p=resp.train, args=c("-P","nohinge", "nothreshold", "noproduct"))  
      
      # prediction
      pred.biva <- predict(me.biva, env)
      
      ## predict across whole raster
      #pred.biva.stck <- predict(me.biva, values(stack))
      
      # evaluate bivariate model and calculate its weight for ensemble prediction
      eval.train.biva <- evaluate(p=pred.biva[sel.train][resp.train==1], a=pred.biva[sel.train][resp.train==0])
      weight <- eval.train.biva@auc*2-1  # use Somers' D as weighting factor
      if(weight < 0) {weight <- 0}
      weight.vec <- c(weight.vec, weight)
      
      # collect all weights to do an average for final model weighting
      weight.vec.m[m,n] <- weight
      
      # build weighted sum of bivariate predictions
      pred.biva.sum <- pred.biva.sum + (pred.biva * weight)
      #pred.biva.sum.stck <- pred.biva.sum.stck + (pred.biva.stck * weight)
      
    }
  }
  
  w10List[[i]] <- weight.vec.m
  
  # calculate ESM prediction
  pred.esm <- pred.biva.sum/sum(weight.vec)
  #  pred.esm.stck <- pred.biva.sum.stck/sum(weight.vec)
  
  # evaluate ESM
  auc.test <- evaluate(p=pred.esm[sel.test ][resp.test ==1], a=pred.esm[sel.test][resp.test == 0])@auc
  # ecospat.boyce(pred.esm[sel.test],pred.esm[sel.test][resp.test==1], PEplot=T)
  boyce.test <- ecospat.boyce(fit = pred.esm[sel.test],obs = pred.esm[sel.test][resp.test==1], PEplot=F)$Spearman.cor     
  eval.test.esm <- rbind(eval.test.esm , cbind(auc=auc.test,boyce=boyce.test))
  #eval.test.esm <- rbind(eval.test.esm , cbind(auc=auc.test))
  #eval.test.esm <- c(eval.test.esm , auc.test)
  
} # end of nr runs of ESM

eval.test.esm
apply(eval.test.esm, 2, mean)


# save this below
# eval.test.esm

# weights for 10 runs of all bivariate combinations

# save this below
# w10List


# get average weight for each bivariate combination
#w.array <- do.call(abind, w10List)
w.array <- array(as.numeric(unlist(w10List)), dim=c(dim(env)[2]-1, dim(env)[2], nr)) # r, c, h
w.mn <- apply(w.array, c(1,2), mean)#*2-1 #  convert mean AUC to somer's D
w.mn

# counter
mn <- matrix(1:(nrow(w.mn)*ncol(w.mn)), nrow= nrow(w.mn), ncol = ncol(w.mn), byrow = T)

### do full model
pred.biva.sum.full <- rep(0, ncell(pred))

# m = 1; n = 2

for(m in 1:(dim(env)[2]-1)){           # build all bivariate predictor combinations
  for(n in (m+1):dim(env)[2]){
    
    # print counter
    print(paste("bivariate combination:", mn[m,n]))
    
    # calibrate bivariate maxent model
    me.biva.full <- maxent(x=env[,c(m,n)], p=resp, args=c("-P","nohinge", "nothreshold", "noproduct"))
    
    # prediction
    pred.biva.full <- dismo::predict(me.biva.full, values(pred))
    # throwing error in if (class(x) == "data.frame") 
    ## as it is "matrix" "array"...  it takes false, so it's ok, ie not a raster 
    #
    
    # build weighted sum of bivariate predictions
    pred.biva.sum.full <- pred.biva.sum.full + (pred.biva.full * w.mn[m,n])
    #pred.biva.sum.stck <- pred.biva.sum.stck + (pred.biva.stck * weight)
    
  }
}

# calculate ESM prediction
# Weighting for full model is 
pred.esm.full <- pred.biva.sum.full/sum(w.mn, na.rm = T)

modRes <- raster(pred)
modRes[] <- pred.esm.full
plot(modRes)
# dir.create("gis/r_sir/models")

writeRaster(modRes, filename = "gis/r_sir/models/modRes_habt.tif", datatype = "FLT4S")
# save this below
# modRes - is species prediction

# save(eval.test.esm, w10List, modRes, file = "mod_res_habt.rdata")

load("mod_res_habt.rdata")
# habt only split for testing/training on presence data (all bg used to train), climate (+ dem) + veg indices
# with k = 3,
# and 10 runs. 


## Get threshold
presVals <- raster::extract(modRes, pcl.f)
sort(presVals)

t.pres <- quantile(presVals, probs = 0.1, na.rm = T)
bin.pres <- reclassify(modRes, rcl = c(0, t.pres, 0, t.pres, 1, 1))

## do 3x3 filter
bin.pres.mf <- focal(bin.pres, w = matrix(1, nrow=3, ncol=3), fun = modal)
save(bin.pres, bin.pres.mf, file= "bin_mod.rdata")
writeRaster(bin.pres, filename = "gis/r_sir/models/modRes_habt_bin.tif", datatype = "INT1U", overwrite = T)
writeRaster(bin.pres.mf, filename = "gis/r_sir/models/modRes_habt_bin_mf.tif", datatype = "INT1U", overwrite = T)

bin.pres
# area of presence is 
sum(values(bin.pres) == 1, na.rm = T) #  181546 km
sum(values(bin.pres.mf) == 1, na.rm = T) #  174489 km


eval.test.esm

se <- function(x) sd(x)/length(x)
apply(eval.test.esm, 2, mean)
apply(eval.test.esm, 2, se)
apply(eval.test.esm, 2, sd)

## get absences within bin.pres (rom s1 occ)
load("ebirdAbsences_in_aoi.rdata")

absence.bin <- extract(bin.pres.mf, ebird.aoi)
table(absence.bin, useNA = "always")

## how many cells do they overlap?
absence.count <- raster::extract(bin.pres, ebird.aoi, cellnumbers = TRUE)
length(unique(absence.count)) # 26232

# as proportion of modelled range
26232/174489 # ~ 15%

ebird.abs.bin <- ebird.aoi[absence.bin == 1 & !is.na(absence.bin),]
st_write(ebird.abs.bin, "gis/s_sir/ebird_abs_bin.shp", delete_layer = T)

barplot(table(ebird.abs.bin$year), las = 2)

table(cut(ebird.abs.bin$year, breaks = c(0, 1990, 2000, 2010, 2020)), useNA = "always")
# (0,1.99e+03]    (1.99e+03,2e+03]    (2e+03,2.01e+03] (2.01e+03,2.02e+03]  <NA>
# 96                 337                1619               17972             0
96  + 337  +  1619+17972

337+1619+17972
sum(ebird.abs.bin$year >1990) 
# 19928 

sum(ebird.abs.bin$year >=1990) 
# 19961 including 1990

# square bracket includes value, ie 109 up to and inlcuing 1990

# cbind(1:10, cut(1:10, breaks = c(0,5,10)))


### Thompson method with Butchart data on paraclaravis


## COde run for paraclaravis with Butchart data - CD. 


## Sampling of the values, counting st dev then setting

setwd("C:/Users/55116479/Dropbox (Manchester Met)/R_online/Paraclaravis")

# passive surveys all years prior to 2005. - from Butchart et al 2018
eps.passive = c(0,	0.05)
pi.passive = c(0.10,	0.65)
pr.passive = c(0.40,	0.60)
pas.sur = c(eps.passive ,pi.passive,pr.passive)

pas.sur


# load ebird absences data
load("gis/ebird_absence_eps_prop.rdata")
yrRes <- data.frame(yrRes)
yrRes

pas.sur.tmp <- yrRes[,c("year", "prop.total")]

plot(yrRes[,c("year", "prop.total")])

## predict 5 years to 2030
lm1 <- lm(area ~ year, data = yrRes)
summary(lm1)
par(mfrow = c(2,2))
plot(lm1)
par(mfrow = c(1,1))

plot(yrRes$area ~ yrRes$year)
abline(lm1)

predict(lm1)

endYear <- 2030
prop21_30 <- predict(lm1, newdata = data.frame(year = 2021:2030))/1.74489e+11 # total area

pas.sur.tmp <- rbind(pas.sur.tmp, cbind(year = 2021:2030, prop.total = prop21_30))

signif(yrRes[,"prop.total"], 2)
floor(yrRes[,"prop.total"] * 10)/10
ceiling(yrRes[,"prop.total"] * 10)/10

# pas.sur.tmp
pas.sur05_20 <- cbind(year = pas.sur.tmp[,"year"],
                      eps_lower = floor(pas.sur.tmp[,"prop.total"] * 10)/10,
                      eps_upper = ceiling(pas.sur.tmp[,"prop.total"] * 10)/10,
                      pi_lower = rep(0.10,nrow(pas.sur.tmp)),
                      pi_upper = rep(0.65,nrow(pas.sur.tmp)),
                      pr_lower = rep(0.40,nrow(pas.sur.tmp)),
                      pr_upper = rep(0.60,nrow(pas.sur.tmp))
)

# rownames(pas.sur05_20) <- NULL
pas.sur05_20

# source functions
source("thompson_functions.r")

recordings <- read.csv("data/paraclaravis_All_data_records_sightings.csv", comment.char = "#")
surveys <- read.csv("data/paraclaravis_All_data_surveys.csv", comment.char = "#")

head(recordings)
str(recordings)
recordings[,1:3]

# check years are unique - one record per year, specimen trumps sighting
length(unique(recordings$year)) == nrow(recordings)


## subset to only specimens
unique(recordings$type)
recordings <- subset(recordings, type == "specimen")
recordings


#total number of years (including years without data)
years = seq(min(recordings[,'year'],pas.sur05_20[,'year']),endYear,by =1) # if no surveys

Total.years = length(years)

#add passive surveys to the surveys (for each year without record or dedicated survey)

# years pre 2005
years.lt2005 <- years[years < 2005]

pas.sur.years.lt05 <- years.lt2005[!years.lt2005 %in% recordings[,1]] #find years without either surveys or recordings
pas.surveys.lt05 = cbind(pas.sur.years.lt05, t(replicate(length(pas.sur.years.lt05), pas.sur)))

# years >= 2005 (with ebird surveys)
years.gte2005 <- years[years >= 2005]
pas.sur.years.gte05 <- years.gte2005[!years.gte2005 %in% recordings[,1]] #find years without
pas.surveys.gte05 <- pas.sur05_20[pas.sur05_20[,"year"] %in% pas.sur.years.gte05,]

colnames(pas.surveys.lt05) <- colnames(pas.surveys.gte05)

# colnames(pas.surveys) = names(surveys)[-8] # without notes column
surveys = as.data.frame(rbind(pas.surveys.lt05, pas.surveys.gte05))

surveys <- surveys[order(surveys[,"year"]),]
rownames(surveys) <- NULL
head(surveys)
surveys

rec.year = cbind(recordings[,'year'],1) #whether a recording was made this year
rec.year = rbind(rec.year,cbind(surveys[,'year'],0)) 
# 0 for all unsuccessful surveys (if survey is successful, then it's  a record)
rec.year = rec.year[order(rec.year[,1]),]

rec.year


nrow(rec.year) == (nrow(recordings) + nrow(surveys))
(nrow(recordings) + nrow(surveys)) == length(years)

PXt = px.mid()
print(cbind(years, PXt))


getwd()
save(PXt, file = "code/specimen.rdata")


# Plot the results -----------------------------
xx <- c(years, rev(years))


yysd=c( PXt[,1],rev(PXt[,3]))

yyint=c( PXt[,4],rev(PXt[,5]))

par(family="serif")
plot(
  years,PXt[,2], "l",ylim = c(0,1), xaxt = 'n',xaxs = "i", yaxs = "i" ,xlab =
    "Years", ylab = "P(X|t)"
)

polygon(xx, yyint, col = 'lightgrey', border = NA)
polygon(xx, yysd, col = 'darkgrey', border = NA)

lines(years,PXt[,2], "l")
axis(1, at = seq((min(years)%/%5+1) * 5, max(years), by = 5), las = 2)

#axis(1, at = seq(min(years), max(years), by = 5), las = 2)
#axis(1, at = c(min(years), max(years)), las = 2)

# Table of results -----------------------------
# PXt = cbind(years,PXt.min,PXt.mid,PXt.max)
PXt = cbind(years,PXt)
print (PXt)


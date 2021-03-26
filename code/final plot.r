####### Final plots

source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")

## Data from Thompson_method scripts
load("sightings.rdata")
years

dataX <- data.frame(years = years, PXt, Basis = "All records")
head(dataX)

load("specimen.rdata")

dataX <- rbind(dataX,data.frame(years = years, PXt, Basis = "Specimens"))
colnames(dataX) <- c("years", "sd_lwr", "PXt", "sd_upr", "PXt_min", "PXt_max", "Basis")

head(dataX)

w.xls(dataX)

library(ggplot2)

ggplot(dataX, aes(x = years, y = PXt, col= Basis))+
  geom_ribbon(aes(ymin=sd_lwr, ymax = sd_upr, fill = Basis), linetype = 0, alpha = 0.4)+
  geom_ribbon(aes(ymin=PXt_min, ymax = PXt_max, fill = Basis), linetype = 0, alpha = 0.2)+
  scale_color_manual(values = c("black", "darkred"))+
  scale_fill_manual(aesthetics = "fill", values = c("grey10", "grey50"))+
  geom_line(aes(linetype= Basis), size = 1)+
  ylim(c(0,1))+
  xlab("Years")+
  scale_x_continuous(breaks = seq(1810, 2030,10))+
  ylab("Probability that taxon is extant")+
  theme_light()+
  theme(legend.position = c(0.1,0.15))
ggsave("risk_2_on_1.png", width = 200, height = 150, units = "mm")


library(gstat)
library(sp)
library(spacetime)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(raster)
library(rgdal)
library(rgeos) 
library(geoR)

#Data
d <- data.temp.sub.trim

# Turn the data frame into a very long vector (by date and then by location)
values <- data.frame(temp = as.vector(as.matrix(t(d[,2:dim(d)[2]]))))
values$pcp <- as.vector(as.matrix(t(data.pcp24.sub.trim[,2:dim(data.pcp24.sub.trim)[2]])))

# Add the TIME in POSIX format
values$TIME <- rep(d$date, each=length(d)-1) #rep(sapply(d$date, as.POSIXct), each=length(d)-1)

# Add the corresponding lon and lat 
values$LON <- rep(sum_pcp24.sub$LON, times = dim(d)[1])
values$LAT <- rep(sum_pcp24.sub$LAT, times = dim(d)[1])

# Omit NAs
values <-na.omit(values)

# Pass Values to spval so Values will remain a dataframe 
spval <- values

# Create a Spatial Points Data Frame by specifying the coordinates
coordinates(spval)=~LON+LAT

#STILL WORKING ON THIS 
#i<-matrix(c(1:dim(values)[1],1:dim(values)[1]), nrow=dim(values)[1], ncol=2)
#sp = SpatialPoints(cbind(values$LAT, values$LON))
#STSDF(sp = sp, time = values$TIME, data = spval, index=i)

# SP Variogram - This takes some time to run (took me about 1 hour)
#temp.vgm <- variogram(temp~1, spval)

#Spatial Variogram
#6844 - Sept 26, 2009 Ondoy
d <- data.temp
sval <-data.frame(temp = t(data.temp[6844,2:dim(d)[2]]), 
                  pcp = t(data.pcp24[6844,2:dim(d)[2]]))
sval$LON <- sum_pcp24$LON
sval$LAT <- sum_pcp24$LAT
colnames(sval)<-c("temp", "pcp", "LON", "LAT")
sval <-na.omit(sval)

coordinates(sval)=~LON+LAT
sval.vgm.temp <-variogram(temp~1, sval)
plot(sval.vgm.temp, type = "b", col = "green")
sval.vgm.pcp<-variogram(pcp~1, sval)
plot(sval.vgm.pcp, type = "l", col = "blue")

# Fit - use Gaussian?
sval.fit <- fit.variogram(sval.vgm, model=vgm("Gau")) 

# Create a spatial grid where we want to krig
#lon_seq <- seq(bbox(sval)[1,1],bbox(sval)[1,2], by=.01)
#lat_seq <- seq(bbox(sval)[2,1],bbox(sval)[2,2], by=.01)
#sgrid<-merge(lat_seq,lon_seq)
sgrid<-data.frame(x = stat_loc$LAT,
                  y = stat_loc$LON)
plot(sgrid)
coordinates(sgrid)=~x+y

#Krig Precipitation
skrig.pcp <- krige(pcp~1, sval, sgrid, model=sval.fit)

skrig.pcp  %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()

#Krig Temperature
skrig.temp <- krige(temp~1, sval, sgrid, model=sval.fit)

skrig.temp  %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()


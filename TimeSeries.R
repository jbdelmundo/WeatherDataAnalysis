
setwd("R")
setwd("noaa")

source("loadAndCleanFile.R")


data <- loadAndCleanFile("Climate Data", start_date= "1990-01-01", end_date= "2015-06-21",na_threshold = 0.10)
data.temp <- data[["temp"]]
data.pcp <- data[["pcp"]]


#replace NAs by taking mean of each col
for(i in 1:ncol(data.temp)){
    data.temp[is.na(data.temp[,i]), i] <- mean(data.temp[,i], na.rm = TRUE)
}
for(i in 1:ncol(data.pcp)){
    data.pcp[is.na(data.pcp[,i]), i] <- mean(data.pcp[,i], na.rm = TRUE)
}

# Plot correlation matrix
require(lattice)
temp.cov <- cov(data.temp[-1])
temp.cor <- cor(data.temp[-1])
levelplot(temp.cor, scales=list( x=list(rot=90)))

pcp.cov2 <- cov(data.pcp[-1])
pcp.cor <- cor(data.pcp[-1])
levelplot(pcp.cor,scales=list(x=list(rot=90)))

d <- data.temp
test = as.POSIXct("2009-01-01")
print( class( test ) )
# [1] "POSIXct" "POSIXt"



mean.temp <- numeric
d <- d[-1]
for(i in 1:nrow(d) ) {
  mean.temp <- mean(as.numeric(d[i,]))     
}

library("TTR")
mean.temp <-apply(d,1,mean)
ema.temp <- EMA(mean.temp,n=30)
m <- cbind(date= as.POSIXct.Date(data.temp$date), temp=mean.temp, ema=ema.temp)
m




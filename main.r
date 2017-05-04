source("rfiles/installpackagesneeded.r")
source("rfiles/functions.R")

data.dir <- "C:/Users/clutariomark/Desktop/Climate Data"
setwd(data.dir)

data <- loadAndCleanFile("Climate Data",start_date="1991-01-01",end_date="2015-06-21",na_threshold=0.1)
data.pcp <- data[["pcp"]]
data.temp <- data[["temp"]]
data.loc <- data[["locations"]]


data.temp.mean <- replaceNAs(data.temp, data.loc)$data
data.pcp.mean <- replaceNAs(data.pcp, data.loc)$data
data.temp.mice <- replaceNAs(data.temp, data.loc, method="pmm")$data
data.pcp.mice <- replaceNAs(data.pcp, data.loc, method="pmm")$data

# read ph contour for plotting
ph_contour <- readShapePoly("shpfiles/PHL_Admin_Regions_Union.shp")
UTM51N <- CRS("+init=epsg:32651")
WGS84 <- CRS("+init=epsg:4326")
proj4string(ph_contour) <- WGS84
ph_contour.UTM51N <- spTransform(ph_contour, UTM51N)
ph_contour.UTM51N <- fortify(ph_contour.UTM51N, region="FID")

# plot heat maps
plotHeatMaps(data.pcp.mean, data.loc, ph_contour, c(0,300), from=as.Date("2015-01-01"), to=as.Date("2015-01-01"), save=T,prefix="rainfall_mean")
plotHeatMaps(data.pcp.mice, data.loc, ph_contour, c(0,300), from=as.Date("2015-01-01"), to=as.Date("2015-01-01"), save=T,prefix="rainfall_mice")
plotHeatMaps(data.temp.mice, data.loc, ph_contour, c(10,45), from=as.Date("2015-01-01"), to=as.Date("2015-01-01"), save=T,prefix="temperature")

plotByDate(data.pcp$date, data.pcp$ROMBLON)
plotByDate(data.pcp.mean$date, data.pcp.mean$SCIENCE_GARDEN)
plotByDate(data.pcp.mice$date, data.pcp.mice$ROMBLON)

plotByDate(data.temp$date, data.temp$ROMBLON)
plotByDate(data.temp.mean$date, data.temp.mean$ROMBLON)
plotByDate(data.temp.mice$date, data.temp.mice$ROMBLON)

plotByYear(data.pcp.mice, years=c("1991","2001","2011","2013"), ylim=c(0,40))

require(lattice)
t <-data.temp[-1]
temp.cov <- cov(data.temp[-1])
temp.cor <- cor(data.temp[-1])
pcp.cor <- cor(data.pcp[-1])
pcp.cor <- cov(data.pcp[-1])
levelplot(pcp.cor, scales=list( x=list(rot=90)))
levelplot(temp.cor, scales=list( x=list(rot=90)))

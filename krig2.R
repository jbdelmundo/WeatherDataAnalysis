library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos) 
library(dismo)
library(class)

#Data
d <- data.temp.sub.trim

# Turn the data frame into a very long vector (by date and then by location)
tp <- data.frame(temp = as.vector(as.matrix(t(d[,2:dim(d)[2]]))))
tp$pcp <- as.vector(as.matrix(t(data.pcp24.sub.trim[,2:dim(data.pcp24.sub.trim)[2]])))


d$date <- sapply(d$date, function(x) as.POSIXct(strptime(x, "%Y-%m-%d")))
tp$Time <- rep(d$date, each=length(d)-1)

# Typecast the longitude and latitudes as numeric
tp$Longitude <-as.numeric(rep(sum_pcp24.sub$LON, times = dim(d)[1]))
tp$Latitude <-as.numeric(rep(sum_pcp24.sub$LAT, times = dim(d)[1]))

# Omit NAs
tp <- na.omit(tp)

# Sample
subtp <- tp[sample(nrow(tp), 6000), ]

# Create a SpatialPointsDataFrame
coordinates(subtp)=~Longitude+Latitude

# Create SpatialPoints Obj
tpSP <- SpatialPoints(subtp@coords)

tpDF <- data.frame(temp=subtp$temp)
tpTM <- as.POSIXlt(as.numeric(subtp$Time), origin="1970-01-01")

#Variogram
tpVar<- variogram(temp~1, subtp)
tpFit<- fit.variogram(tpVar, vgm(c("Per", "Gau", "Exp")), fit.kappa = TRUE)

plot1 <- subtp 
     ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  +     ggtitle("Points with measurements")

Barangays.dir <- "C:/Users/gaagu/Documents/School/CS 297 Spatio-Temporal Analysis/Barangays"
setwd(Barangays.dir)

ph <- readOGR(".", "Barangays")
# map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))

# Create an empty raster.
grid <- raster(extent(ph))

# Choose its resolution. I will use 2 degrees of latitude and longitude.
res(grid) <- 2

proj4string(grid)<-proj4string(ph)

# Transform this raster into a polygon and you will have a grid, but without Brazil (or your own shapefile).
gridpolygon <- rasterToPolygons(grid)

# Intersect our grid with Brazil's shape (or your shapefile). R will need a considerable time to do that (~15 minutes in our example). Just let it work while you do other things, or drink a coffee, or whatever. Note that the time needed to finish this depends on the size (area) of your shape, and on your grid resolution. So, don't expect to intersect a 0.5 lat/long grid with a world shapefile in 5 minutes (=]).
ph.grid <- intersect(ph, gridpolygon)

# Plot the intersected shape to see if everything is fine.
plot(ph.grid)

# Let's try clustering 
clusters <- hclust(dist(t(d[,2:length(d)])))
png(filename="Cluster_dendogram.png")
plot(clusters)
dev.off()

clusters_pcp <- hclust(dist(t(data.pcp24.sub.trim[,2:length(data.pcp24.sub.trim)])))
png(filename="Cluster_pcp.png")
plot(clusters_pcp)
dev.off()

data.temppcp.sub.trim <- rbind(data.temp.sub.trim, data.pcp24.sub.trim)
clusters_temp_pcp <- hclust(dist(t(data.temppcp.sub.trim[sample(nrow(data.temppcp.sub.trim)),2:length(data.temppcp.sub.trim)])))

###############
tab <- data.frame(station = character(0),
                  month = character(0),
                  day = character(0),
                  pcp = numeric(0),
                  temp = numeric(0), stringsAsFactors=FALSE)

tab <- matrix(nrow = 393272, ncol = 5)
i = 0
# Split the data into station, month, day, pcp, temp
for(station in names(data.pcp24.sub.trim)[-1])
{
  for(row in seq(1:dim(data.pcp24.sub.trim)[1]))
  {
    i = i + 1
    tab[i,] <- c(as.character((station)),
                 format(data.pcp24.sub.trim$date[row][[1]], "%m"),
                 format(data.pcp24.sub.trim$date[row][[1]], "%d"),
                 data.pcp24.sub.trim[[station]][row],
                 data.temp.sub.trim[[station]][row]
                 )
  }
}  

# Split the data into station, month, day, pcp, temp
for(station in names(data.pcp24.sub.trim)[-1])
{
  for(row in seq(1001:2000))#dim(data.pcp24.sub.trim)[1]))
  {
    new_row <- c(as.character((station)),
                 format(data.pcp24.sub.trim$date[row][[1]], "%m"),
                 format(data.pcp24.sub.trim$date[row][[1]], "%d"),
                 data.pcp24.sub.trim[[station]][row],
                 data.temp.sub.trim[[station]][row]
    )
    
    tab[nrow(tab) + 1, ] <- new_row
    
  }
} 

# Split datasets into train and test
samp_size <- floor(0.75 * nrow(tab))

# Set the seed 
set.seed(2016)
train_index <- sample(seq_len(nrow(tab)), size = samp_size)

train <- tab[train_index, ]
test <- tab[-train_index, ]

train_labels <- tab[train_index, 1]
test_labels <- tab[-train_index, 1]

pred <- knn(train = train, test = test,cl = train_labels, k=6)

#Cluster


t_no_na <- data.temp
p_no_na <- data.pcp

# replace NAs by taking mean of each col
for(i in 1:ncol(t_no_na)){
  t_no_na[is.na(t_no_na[,i]), i] <- mean(t_no_na[,i], na.rm = TRUE)
}
for(i in 1:ncol(p_no_na)){
  p_no_na[is.na(p_no_na[,i]), i] <- mean(p_no_na[,i], na.rm = TRUE)
}


# Let's try clustering 
#Temp
clusters <- hclust(dist(t(d[,2:length(d)])))
png(filename="Cluster_dendogram.png")
plot(clusters)
dev.off()

#Precipitation
clusters_pcp <- hclust(dist(t(data.pcp24.sub.trim[,2:length(data.pcp24.sub.trim)])))
png(filename="Cluster_pcp.png")
plot(clusters_pcp)
dev.off()

#Both
data.temppcp.sub.trim <- rbind(data.temp.sub.trim, data.pcp24.sub.trim)
clusters_temp_pcp <- hclust(dist(t(data.temppcp.sub.trim[sample(nrow(data.temppcp.sub.trim)),2:length(data.temppcp.sub.trim)])))
png(filename="Cluster_temp_pcp.png")
plot(clusters_temp_pcp)
dev.off()

#Temp No NA
clusters_no_na <- hclust(dist(t(t_no_na[,2:length(t_no_na)])))
png(filename="Cluster_no_na_dendogram.png")
plot(clusters_no_na, asp = 2)
dev.off()

#PCP No NA
clust_pcp_no_na <- hclust(dist(t(p_no_na[,2:length(p_no_na)])))
png(filename="Clust_pcp_no_na_dendogram.png")
plot(clust_pcp_no_na, asp = 2)
dev.off()

#PCP No NA Binary
clust_pcp_no_na_bin <- hclust(dist(t(p_no_na[,2:length(p_no_na)]), method = "binary"))
png(filename="clust_pcp_no_na_bin_dendogram.png")
plot(clust_pcp_no_na_bin, asp = 2)
dev.off()

#Get only the trimmed station locations
stat_loc.sub.trim <- stat_loc[stat_loc$STATION.NAME %in% names(d), ]

#Shapefile
ph_contour <- readShapePoly("noaa_data/Regions.shp")
WGS84 <- CRS("+init=epsg:4326")
proj4string(ph_contour) <- WGS84
ph_contour.UTM51N <- spTransform(ph_contour, UTM51N)


clust_plot <- function(cluster)
{
  #Plot
  #Try several cuts 
  for(i in seq(1:15))
  {
    rcolors <- rainbow(i)
    clusterCut <- cutree(cluster, i)
    png(filename = paste(deparse(substitute(cluster)), i, ".png"))
    
    plot(ph_contour)
    for(j in seq(1:i))
    {
      points( stat_loc.sub.trim[stat_loc.sub.trim$STATION.NAME %in% names(clusterCut[clusterCut == j]), ]$LON,
              stat_loc.sub.trim[stat_loc.sub.trim$STATION.NAME %in% names(clusterCut[clusterCut == j]), ]$LAT, 
              col = rcolors[j], pch = 19, cex = 1)  
    }
    dev.off()
  }
}


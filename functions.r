

# Loads the CSV files from the script.dirs
# Parameters:
#       start_date and end_ date: dates where to trim the data
#       na_threhshold = remove stations that have na%  above this parameter 

loadAndCleanFile <- function(file.dir , start_date = "1973-01-01", end_date="2015-06-21", na_threshold = 0.6){
    setwd(file.dir)
    
    files <- list.files(pattern="*.csv")
    files <- files[which(files != "evi.csv")]
    files <- files[which(files != "NA.csv")]
    files <- files[which(files != "station_location.csv")]
    
    # Set-up Precipitation and Temperature data frames/matrix
    min_date <- as.Date(start_date, "%Y-%m-%d")
    max_date <- as.Date(end_date, "%Y-%m-%d")
    date_seq <- data.frame(date = seq(min_date, max_date, by=1))
    data.temp <- date_seq
    data.pcp24 <- date_seq
    
    for (file in files) {
        columnname <- strsplit(file, "[.]csv")[[1]][1]
        tmp_data <- read.csv(file, stringsAsFactors=F, header=T)
        tmp_data$date <- as.Date(tmp_data$date, "%Y-%m-%d")
        tmp_seq <- date_seq
        tmp_seq <- merge(tmp_seq, tmp_data, by.x="date", by.y="date", all.x=T)
        data.pcp24[columnname] <- tmp_seq$PCP24
        data.temp[columnname] <- tmp_seq$TEMP
    }
    
    # Replace space with underscores
    colnames(data.pcp24) <- gsub(" ", "_", colnames(data.pcp24))
    colnames(data.pcp24) <- gsub("[.]", "", colnames(data.pcp24))
    colnames(data.temp) <- gsub(" ", "_", colnames(data.temp))
    colnames(data.temp) <- gsub("[.]", "", colnames(data.temp))
    
    # Combine data of BASCO-BASCO_RADAR, BALER-BALER_RADAR, DAVAO-DAVAO_AIRPORT
    baler_vec.p <- vector(mode="numeric", length=nrow(data.pcp24))
    basco_vec.p <- vector(mode="numeric", length=nrow(data.pcp24))
    davao_vec.p <- vector(mode="numeric", length=nrow(data.pcp24))
    baler_vec.t <- vector(mode="numeric", length=nrow(data.pcp24))
    basco_vec.t <- vector(mode="numeric", length=nrow(data.pcp24))
    davao_vec.t <- vector(mode="numeric", length=nrow(data.pcp24))
    
    for (i in seq(1,nrow(data.pcp24))) {
        if (is.na(data.pcp24$BALER_RADAR[i])) {
            baler_vec.p[i] <- data.pcp24$BALER[i]
            baler_vec.t[i] <- data.temp$BALER[i]
        } else {
            baler_vec.p[i] <- data.pcp24$BALER_RADAR[i]
            baler_vec.t[i] <- data.temp$BALER_RADAR[i]
        }
        if (is.na(data.pcp24$BASCO_RADAR[i])) {
            basco_vec.p[i] <- data.pcp24$BASCO[i]
            basco_vec.t[i] <- data.temp$BASCO[i]
        } else {
            basco_vec.p[i] <- data.pcp24$BASCO_RADAR[i]
            basco_vec.t[i] <- data.temp$BASCO_RADAR[i]
        }
        if (is.na(data.pcp24$DAVAO[i])) {
            davao_vec.p[i] <- data.pcp24$DAVAO_AIRPORT[i]
            davao_vec.t[i] <- data.temp$DAVAO_AIRPORT[i]
        } else {
            davao_vec.p[i] <- data.pcp24$DAVAO[i]
            davao_vec.t[i] <- data.temp$DAVAO[i]
        }
    }
    
    data.pcp24$BASCO <- basco_vec.p
    data.pcp24$BALER <- baler_vec.p
    data.pcp24$DAVAO <- davao_vec.p
    data.temp$BASCO <- basco_vec.t
    data.temp$BALER <- baler_vec.t
    data.temp$DAVAO <- davao_vec.t
    
    data.pcp24 <- data.pcp24[,-which(colnames(data.pcp24) %in% c("BASCO_RADAR", "BALER_RADAR", "DAVAO_AIRPORT"))]
    data.temp <- data.temp[,-which(colnames(data.temp) %in% c("BASCO_RADAR", "BALER_RADAR", "DAVAO_AIRPORT"))]
    
    # Read station location csv file
    stat_loc <- read.csv("station_location.csv", stringsAsFactors=F, header=T)
    stat_loc$STATION.NAME <- gsub(" ", "_", stat_loc$STATION.NAME)
    stat_loc$STATION.NAME <- gsub("[.]", "", stat_loc$STATION.NAME)
    stat_loc <- stat_loc[stat_loc$STATION.NAME %in% colnames(data.pcp24),]
    stat_loc <- unique(stat_loc)
    
    # Sum NAs per column to see number of missing data per weather station
    # using precipitation data frame
    sum_pcp24 <- data.frame(cols=colnames(data.pcp24[,-1]))
    tmp_sum <- vector(mode="numeric")
    for (cols in colnames(data.pcp24[-1])) {
        tmp_sum <- c(tmp_sum,sum(is.na(data.pcp24[cols])))
    }
    sum_pcp24["NAs"] <- tmp_sum/nrow(data.pcp24[,-1])
    
    # merge sum_pcp24 anf stat_loc data frames
    sum_pcp24 <- merge(sum_pcp24, stat_loc, by.x="cols", by.y="STATION.NAME", all.x=T)
    
    # reorder data frame - descending number of NAs
    sum_pcp24 <- sum_pcp24[order(-sum_pcp24$NAs),]
    
    # Delete stations with more than 60% of observations missing
    col_idx <- as.character(sum_pcp24$cols[sum_pcp24$NAs < na_threshold])
    col_idx <- c("date",col_idx)
    data.pcp24.sub <- data.pcp24[,col_idx]
    data.temp.sub <- data.temp[,col_idx]
    
    sum_pcp24.sub <- sum_pcp24[sum_pcp24$cols %in% col_idx,]
    
    # Count NAs per row (determine NAs per day)
    row_nas_full <- apply(data.pcp24, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) )
    row_nas <- apply(data.pcp24.sub, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) )
    data.nas <- date_seq
    data.nas$row_nas_full <- row_nas_full
    data.nas$row_nas <- row_nas
    
    setwd("..")
    
    list(temp = data.temp.sub , pcp = data.pcp24.sub, locations = sum_pcp24.sub )
}

# Reproject coordinates from lat long to UTM (in meters)
# Parameters:
#       x = dataframe containing the lat long columns

reprojectCoords <- function(x) {
  y <- x
  UTM51N <- CRS("+init=epsg:32651")
  WGS84 <- CRS("+init=epsg:4326")
  coordinates(x) <- c("LON", "LAT")
  proj4string(x) <- WGS84
  x <- spTransform(x, UTM51N)
  y$X <- x@coords[,1]
  y$Y <- x@coords[,2]
  return(y)
}

# Plot interpolated heatmaps from the weather stations
# Parameters:
#       data = dataframe containing weather station data
#       data.loc = dataframe containing weather station locations/coordinates
#       ph_contour = SpatialPolygonsDataFrame needed for plotting outline of PH
#       limits = limits of plotted heatmap (z-axis)
#       from = start date (default = 1991-01-01)
#       to = end date (default = 2015-06-21)
#       by.days = days to skip (default = 1)
#       res = resolution of heatmap in meters (cannot be below 10000m)
#       save = logical T or F, T if you want to save the plot
#       prefix = string attached to filename (e.g. rainfall)

plotHeatMaps <- function(data,data.loc,ph_contour,limits,from="1991-01-01",to="2015-06-21",by.days=1,res=10000,save=F,prefix) {
  data.loc <- reprojectCoords(data.loc)
  if (res < 10000) {
    print("can't have res < 10000. will have memory problems")
  } else {
    from <- as.Date(from)
    to <- as.Date(to)
    min.date <- which(data$date == from)
    max.date <- which(data$date == to)
    plot.seq <- seq(min.date, max.date, by.days)
    for (i in plot.seq) {
      heatmap.res <- res
      heatmap.limits <- limits
      date.string <- as.character(data$date[i])
      data.heatmaps <- data.loc
      data.heatmaps$cols <- as.character(data.heatmaps$cols)
      data.heatmaps$date1 <- t(data[i,-1])
      
      coordinates(data.heatmaps) <- ~ X + Y
      x.range <- data.heatmaps@bbox[1,]
      y.range <- data.heatmaps@bbox[2,]
      grd <- expand.grid(x = seq(x.range[1], x.range[2], by=heatmap.res),
                         y = seq(y.range[1], y.range[2], by=heatmap.res))
      
      coordinates(grd) <- ~ x + y
      gridded(grd) <- TRUE
      
      krige1 <- krige(formula=data.heatmaps@data$date1~1,locations=data.heatmaps,
                      newdata=grd)
      
      krige1.output <- as.data.frame(krige1)
      
      krigeplot <- ggplot() + geom_tile(data = krige1.output, alpha = 0.8, aes(x = x, y = y, fill = var1.pred)) +
        geom_path(data = ph_contour.UTM51N, aes(long, lat, group = group), colour = "grey") +
        geom_point(data = as.data.frame(data.heatmaps@coords), aes(x = X, y = Y), shape = 15, 
                   colour = "red") + coord_fixed() +
        scale_fill_distiller(palette = "Spectral", name=prefix, limits=heatmap.limits) +
        xlim(x.range) + ylim(y.range) + ggtitle(date.string)
      
      if (save==F) {
        print(krigeplot)
      } else {
        filename <- paste0(prefix,"_",date.string, ".png")
        png(filename, bg="transparent")
        print(date.string)
        print(krigeplot)
        dev.off()  
      }
      return()
    }
  }
}


plotByDate <- function(date, y, from="1991-01-01",to="2015-06-21") {
  from <- as.Date(from)
  to <- as.Date(to)
  xlim <- c(from, to)
  print(xlim)
  plot(date, y, type="l", xlim=xlim, xlab="date")
}

# Replace NAs in data
# Parameters:
#       data = data frame containing data with missing observations
#       method = if "mean", replace missing observations with mean of station
#                if "pmm", replace missing observations using predictive mean
#                   matching between two nearest stations

replaceNAs <- function(data, data.loc, method="mean") {
  returnlist <- list(data=NULL,result=NULL)
  
  if (method=="mean") {
    for (i in 2:ncol(data)) {
      data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    }
    returnlist$data <- data
  }
  
  if (method=="pmm") {    
    # reproject coordinates from lat-long to mercator (meters)
    data.loc <- reprojectCoords(data.loc)
    rownames(data.loc) <- data.loc[,1]
    loc.cols <- which(names(data.loc) %in% c("X","Y"))
    data.loc.distmat <- as.matrix(dist(data.loc[,loc.cols]))
    
    # calculate distances between stations
    data.loc.nearest <- character(nrow(data.loc.distmat))
    for (i in seq(1,nrow(data.loc.distmat))) {
      data.loc.nearest[i] <- names(data.loc.distmat[order(data.loc.distmat[,i]),i][2])
    }
    names(data.loc.nearest) <- rownames(data.loc)
    
    stations <- names(data.loc.nearest)
    stations.nearest <- data.loc.nearest
    data.mice <- data.frame(date = data$date)
    mice.results <- list(length=length(stations))
    for (j in seq(1, length(stations))) {
      cur.stat <- stations[j]
      nxt.stat <- stations.nearest[j]
      mi.df <- data.frame(V1=data[cur.stat],V2=data[nxt.stat])
      colnames(mi.df) <- c(cur.stat, nxt.stat)
      mi <- mice(mi.df,m=5,maxit=1)
      mice.results[[cur.stat]] <- mi
      mi.complete <- complete(mi, sample(1:5,1))
      data.mice[cur.stat] <- mi.complete[cur.stat]
    }
    returnlist$data <- data.mice
    returnlist$result <- mice.results
  }
  return(returnlist)
}

# Plot data per year
# Parameters:
#       data = data frame containing data to be plotted
#       stations = if "all", gets mean of all stations per row
#                  else, plot each station separately
#       years = selected years to plot (default = 1991-2015 skip by 3 years)
#       mov_avg = set moving average number (default = 10)
#       ylim = set ylim of plot

plotByYear <- function(data, stations="all", years=as.character(seq(1991,2015,3)), mov_avg=10, ylim) {
  x <- format.Date(data$date, "%Y")
  y <- list()
  if (stations == "all") {
    y[[1]] <- rowMeans(data[,-1])
  } else {
    for (station in stations) {
      y[[station]] <- data[[station]]
    }
  }
  colors <- rainbow(length(years))
  for (i in seq(1,length(years))) {
    for (j in seq(1,length(y))) {
      year.idx <- which(x == years[i])
      y.filter <- filter(y[[j]][year.idx],rep(1/mov_avg,mov_avg))
      if (i == 1 & j == 1) {
        plot(y.filter, type="l", col=colors[i], ylab="", xlab="days", ylim=ylim)
      } else {
        lines(y.filter, col=colors[i], lty=j)
      }  
    }
    
  }
  legend("top", years, col=colors, lwd=1, seg.len=1, horiz=T, cex=0.5, bty="n")
  legend("bottom", stations, lty=seq(1,length(stations)), seg.len=1, horiz=T, cex=0.5, bty="n")
}
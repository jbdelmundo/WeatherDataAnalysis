

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
    
    # trim data based on date
    min_date_xlim <- as.Date("1990-01-01", "%Y-%m-%d")
    data.pcp24.sub.trim <- data.pcp24.sub[data.pcp24.sub$date >= min_date_xlim,]
    data.temp.sub.trim <- data.temp.sub[data.temp.sub$date >= min_date_xlim,]
    
    setwd("..")
    
    list(temp = data.temp.sub.trim , pcp = data.pcp24.sub.trim)
}



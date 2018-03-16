#######################################################################################################
## This file applies filtration to remove errors in the NYC taxi trip data.
## Different ways of filtering.

## Filter data

# at least 70 days should be kept.

rm(list=ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500")

f <- function(filename) {
  
  library(plyr)
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500")
  
  df <- read.csv(file=filename, stringsAsFactors = F)
  
  #convert time format
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$dropoff_datetime <- as.POSIXct(df$dropoff_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  #add date
  df$date <- as.Date(df$pickup_datetime)
  
  #remove 2013-11-3
  df <- subset(df, df$date != '2013-11-03')
  
  #for some vehicles, longitude and latitude are reverse
  for (i in 1:nrow(df)){
    
    if(df$pickup_longitude[i] > df$pickup_latitude[i]) {
      swap <- df$pickup_longitude[i]
      df$pickup_longitude[i] <- df$pickup_latitude[i]
      df$pickup_latitude[i] <- swap
    }
    
    if(df$dropoff_longitude[i] > df$dropoff_latitude[i]) {
      swap <- df$dropoff_longitude[i]
      df$dropoff_longitude[i] <- df$dropoff_latitude[i]
      df$dropoff_latitude[i] <- swap
    }
    
  }
  
  #remove dropoff-pickup=0
  df <- subset(df, difftime(df$dropoff_datetime, df$pickup_datetime, units = "mins") > 0)
  
  if (nrow(df) > 0){
    
  #remove trip_time=1min and trip_distance=0
  df <- subset(df, (df$trip_time_in_secs == 60 & df$trip_distance == 0) == FALSE)
  
  if (nrow(df) > 0){
  #numbering trip by date
  df$trip_number[1] <- 1
  if (nrow(df)>1) {
    for (i in 2:nrow(df)){
      if (df$date[i] == df$date[i-1]){
        df$trip_number[i] <- df$trip_number[i-1]+2
      } else {
        df$trip_number[i] <- 1
      }
    }
  }
  
  #prepare for remove whole day data
  freq_1 <- count(df$date)
  
  #filter level 1
  df <- subset(df, df$trip_time_in_secs<=10800 &
                 df$trip_distance<=100 & 
                 df$pickup_longitude>= -74.25 & df$pickup_longitude<= -73.5 &
                 df$pickup_latitude>=40.4 & df$pickup_latitude<=41.1 & 
                 df$dropoff_longitude>= -74.25 & df$dropoff_longitude<= -73.5 &
                 df$dropoff_latitude>=40.4 & df$dropoff_latitude<=41.1)
  
  #remove whole day
  if (nrow(df)!=0) {
    freq_2 <- count(df$date)
    freq_1 <- subset(freq_1, freq_1[,1] %in% freq_2[,1])
    temp <- cbind(freq_2, freq_1[,2])
    temp <- subset(temp, temp[,2]==temp[,3])
    if (nrow(temp)!=0){
      df <- subset(df, df$date %in% temp[,1])
    } else {
      df <- df[-c(1:nrow(df)),]
      setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_A")
      write.csv(df, file = filename, row.names = F)
    }
  } else {
    setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_A")
    write.csv(df, file = filename, row.names = F)
  }
  #last:write.csv
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_A")
  write.csv(df, file = filename, row.names = F)
  
  }
  }
  
  
}

parLapply(cl, filename, f)
stopCluster(cl)





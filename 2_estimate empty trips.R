#######################################################################################################
# This file estimates empty trips without customers on taxi.



# empty trip, gap time, dwell time, speed, etc

rm(list=ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F3")

f <- function(filename) {

  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F3")
  df <- read.csv(file=filename, stringsAsFactors = F)
  
  #convert time format
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$dropoff_datetime <- as.POSIXct(df$dropoff_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  #fix travel time
  df$trip_time_in_secs_fix <- as.numeric(difftime(df$dropoff_datetime, df$pickup_datetime, units = "secs"))
  
  #speed
  df$speed <- round(3600*df$trip_distance/df$trip_time_in_secs_fix, 2) #mph
  
  #empty trip distance
  #use least squares
  source("U:\\Desktop\\NYC Taxi Code\\earth.dist.R")
  for (i in 1:(nrow(df)-1)){
    df$empty_trip_distance[i] <- round(1.4413*earth.dist(df$pickup_longitude[i+1], df$pickup_latitude[i+1], 
                                                         df$dropoff_longitude[i], df$dropoff_latitude[i]) + 0.1383, 2)
  }
  df$empty_trip_distance[nrow(df)] <- 0
  
  #emtry trip speed
  for (i in 1:(nrow(df)-1)){
    df$empty_speed[i] <- round((df$speed[i]+df$speed[i+1])/2, 2)
  }
  df$empty_speed[nrow(df)] <- df$speed[nrow(df)] 
  
  #empty travel time
  df$empty_travel_time  <- round(3600*df$empty_trip_distance/df$empty_speed, 0)  # in secs
  
  #gap time
  for (i in 1:(nrow(df)-1)){
    df$gap_time[i] <- difftime(df$pickup_datetime[i+1], df$dropoff_datetime[i], units = "secs")
  }
  df$gap_time[nrow(df)] <- 0
  
  #dwell time
  df$dwell_time <- df$gap_time - df$empty_travel_time #in secs
  
  #dwell yes or no
  df$dwell_yes[df$dwell_time>1800] <- 1
  df$dwell_yes[df$dwell_time<=1800] <- 0
  
  #real dwell time
  df$dwell_time_yes <- df$dwell_time * df$dwell_yes
  
  #total distance
  df$total_distance <- df$trip_distance + df$empty_trip_distance
  
  
  #numbering occupied trip by date
  if(FALSE){
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
  
  #empty trip number
  df$empty_trip_number <- df$trip_number + 1}
  

  #write csv
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")
  write.csv(df, file = filename, row.names = F)
  
  
}

parLapply(cl, filename, f)
stopCluster(cl)



###########################################################################################################
# This file finds the nearest charging station.
# Use NYC existing charging station maps.
# If taxi run out of electricity during trips and no charger there, taxis stop and charge until full and keep going.



rm(list=ls())
library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

f <- function(filename) {
  
  NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")
  df <- read.csv(file=filename, stringsAsFactors = F)
  
  source("U:\\Desktop\\NYC Taxi Code\\earth.dist.R")
  
  for (i in 1:nrow(df)){
    if (df$dwell_yes[i] == 1){
      
      min.distance <- round(earth.dist(df$dropoff_longitude[i], df$dropoff_latitude[i], 
                                       NYC$Longitude[1], NYC$Latitude[1]), 4)
      
      df$min_distance_charge[i] <- round(min.distance, 2) #in miles
      
      df$charge_ID[i] <- NYC$ID[1]
      
      df$charge_level[i] <- NYC$Charge.Level[1]
      
      for (j in 2:nrow(NYC)){
        
        distance <- round(earth.dist(df$dropoff_longitude[i], df$dropoff_latitude[i], 
                                     NYC$Longitude[j], NYC$Latitude[j]), 4)
        
        if (distance < min.distance) {
          
          min.distance <- distance 
          df$min_distance_charge[i] <- round(min.distance, 2)
          
          df$charge_ID[i] <- NYC$ID[j]
          
          df$charge_level[i] <- NYC$Charge.Level[j]
        }
      }
      
    } else {
      df$min_distance_charge[i] <- NA
      
      df$charge_ID[i] <- NA
      
      df$charge_level[i] <- 0
    }
  }
  
  
  # do charge or do not charge
  df$charge_yes <- 0
  #can charge if within 0.5 miles
  df$charge_yes[df$min_distance_charge <= 0.5] <- 1
  
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F5")  
  write.csv(df, file = filename, row.names = F)
}

parLapply(cl, filename, f)
stopCluster(cl)



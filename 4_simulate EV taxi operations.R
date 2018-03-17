###########################################################################################################
# This file simulates the operations of electric taxis in NYC.
# Explore feasibility of replaceing gasoline taxis with electric taxis.
# Try to answer questions: How many occupied trips can be completed by electric taxis? 



rm(list=ls())
library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F5")

f <- function(filename) {
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F5")
  df <- read.csv(file=filename, stringsAsFactors = F)
  
  #time
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$dropoff_datetime <- as.POSIXct(df$dropoff_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  #set charging parameters
  setup_time <- 120 #sec
  #search_time <- 900 #sec
  df$charge_power[df$charge_level==0] <- 0 #kW
  df$charge_power[df$charge_level==1] <- 7 #kW
  df$charge_power[df$charge_level==2] <- 20 #kW
  df$charge_power[df$charge_level==3] <- 50 #kW
  elec_rate <- 0.3 #kWh/mile
  range_200 <- 200 #mile
  range_300 <- 300 #mile
  threshold <- 0.5 #charge when SOC is below threshold
  charge_to <- 1 #charge to 100% SOC
  #emergency_charge_power <- 20 #kW, used when emergency charge is needed
  emergency_charge_percent <- 0.1 #lower than 10%, resort to emergency charging
  
  #######################when range is 200 miles
  df$SOC_1[1] <- range_200
    
  df$SOC_2[1] <- round(df$SOC_1[1] - df$trip_distance[1], 2)
  
  if(df$SOC_2[1] >= range_200*emergency_charge_percent){
    
    if(df$SOC_2[1] < threshold*range_200 & df$charge_yes[1] == 1){
      df$SOC_plus[1] <- round(min(range_200, range_200-df$SOC_2[1], 
                                  (df$dwell_time_yes[1]-setup_time)/3600*df$charge_power[1]/elec_rate), 2)
    } else {df$SOC_plus[1] <- 0}
    
    df$flag[1] <- 1
    
  } else {
    
    start_time <- df$dropoff_datetime[1]
    start_SOC <- df$SOC_2[1]
    source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
    NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
    emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[1], df$dropoff_latitude[1])
    travel_SOC_charge <- emergency_charge_result[1]
    travel_time_charge <- emergency_charge_result[2]
    emergency_charge_power <- emergency_charge_result[3]
    end_SOC <- range_200*charge_to
    charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
    end_time <- start_time + travel_time_charge + charge_time + setup_time
    df$flag[1] <- 0
    
  }
    
  
  
  #set start end_time as 2012-12-31 23:00:00
  end_time <- as.POSIXct("2012-12-31 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  i <- 2 #set initial as 2
  while (i <= nrow(df)){
    
    if(df$pickup_datetime[i] >= end_time){
      
      if(df$pickup_datetime[i-1] > end_time){
        df$SOC_1[i] <- round(df$SOC_2[i-1] + df$SOC_plus[i-1] - df$empty_trip_distance[i-1], 2)
      } else {df$SOC_1[i] <- range_200*charge_to} #start from 100% SOC
      
      
        
      df$SOC_2[i] <- round(df$SOC_1[i] - df$trip_distance[i], 2)
      
      if(df$SOC_2[i] >= range_200*emergency_charge_percent){
        
        if(df$SOC_2[i] < threshold*range_200 & df$charge_yes[i] == 1){
          df$SOC_plus[i] <- round(min(range_200, range_200-df$SOC_2[i], 
                                      (df$dwell_time_yes[i]-setup_time)/3600*df$charge_power[i]/elec_rate), 2)
        } else {df$SOC_plus[i] <- 0}
        
        df$flag[i] <- 1
        
      } else if(df$SOC_2[i] >= 0) {
        
        start_time <- df$dropoff_datetime[i]
        start_SOC <- df$SOC_2[i]
        source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
        NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
        emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[i], df$dropoff_latitude[i])
        travel_SOC_charge <- emergency_charge_result[1]
        travel_time_charge <- emergency_charge_result[2]
        emergency_charge_power <- emergency_charge_result[3]
        end_SOC <- range_200*charge_to
        charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
        end_time <- start_time + travel_time_charge + charge_time + setup_time
        df$flag[i] <- 1
      } else {
        
        start_time <- df$dropoff_datetime[i-1]
        start_SOC <- df$SOC_2[i-1]
        source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
        NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
        emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[i-1], df$dropoff_latitude[i-1])
        travel_SOC_charge <- emergency_charge_result[1]
        travel_time_charge <- emergency_charge_result[2]
        emergency_charge_power <- emergency_charge_result[3]
        end_SOC <- range_200*charge_to
        charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
        end_time <- start_time + travel_time_charge + charge_time + setup_time
        df$flag[i] <- 0
        i <- i-1
      }
    } else {
      
      df$SOC_1[i] <- NA
      df$SOC_2[i] <- NA
      df$SOC_plus[i] <- NA
      df$flag[i] <- 0
    }
    
    i <- i+1
    
  }
  
  
  
  
  
  

  
  #######################when range is 300 miles
  df$SOC_3[1] <- range_300
  
  df$SOC_4[1] <- round(df$SOC_3[1] - df$trip_distance[1], 2)
  
  if(df$SOC_4[1] >= range_300*emergency_charge_percent){
    
    if(df$SOC_4[1] < threshold*range_300 & df$charge_yes[1] == 1){
      df$SOC_plus_3[1] <- round(min(range_300, range_300-df$SOC_4[1], 
                                  (df$dwell_time_yes[1]-setup_time)/3600*df$charge_power[1]/elec_rate), 2)
    } else {df$SOC_plus_3[1] <- 0}
    
    df$flag_3[1] <- 1
    
  } else {
    
    start_time <- df$dropoff_datetime[1]
    start_SOC <- df$SOC_4[1]
    source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
    NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
    emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[1], df$dropoff_latitude[1])
    travel_SOC_charge <- emergency_charge_result[1]
    travel_time_charge <- emergency_charge_result[2]
    emergency_charge_power <- emergency_charge_result[3]
    end_SOC <- range_300*charge_to
    charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
    end_time <- start_time + travel_time_charge + charge_time + setup_time
    df$flag_3[1] <- 0
    
  }
  
  
  
  #set start end_time as 2012-12-31 23:00:00
  end_time <- as.POSIXct("2012-12-31 23:00:00", format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  i <- 2 #set initial as 2
  while (i <= nrow(df)){
    
    if(df$pickup_datetime[i] >= end_time){
      
      if(df$pickup_datetime[i-1] > end_time){
        df$SOC_3[i] <- round(df$SOC_4[i-1] + df$SOC_plus_3[i-1] - df$empty_trip_distance[i-1], 2)
      } else {df$SOC_3[i] <- range_300*charge_to} #start from 80% SOC
      
      
      
      df$SOC_4[i] <- round(df$SOC_3[i] - df$trip_distance[i], 2)
      
      if(df$SOC_4[i] >= range_300*emergency_charge_percent){
        
        if(df$SOC_4[i] < threshold*range_300 & df$charge_yes[i] == 1){
          df$SOC_plus_3[i] <- round(min(range_300, range_300-df$SOC_4[i], 
                                      (df$dwell_time_yes[i]-setup_time)/3600*df$charge_power[i]/elec_rate), 2)
        } else {df$SOC_plus_3[i] <- 0}
        
        df$flag_3[i] <- 1
        
      } else if(df$SOC_4[i] >= 0) {
        
        start_time <- df$dropoff_datetime[i]
        start_SOC <- df$SOC_4[i]
        source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
        NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
        emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[i], df$dropoff_latitude[i])
        travel_SOC_charge <- emergency_charge_result[1]
        travel_time_charge <- emergency_charge_result[2]
        emergency_charge_power <- emergency_charge_result[3]
        end_SOC <- range_300*charge_to
        charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
        end_time <- start_time + travel_time_charge + charge_time + setup_time
        df$flag_3[i] <- 1
        
      } else {
        
        start_time <- df$dropoff_datetime[i-1]
        start_SOC <- df$SOC_4[i-1]
        source("U:\\Desktop\\NYC Taxi Code\\FindNearestChargeStation.R")
        NYC <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\NYC_alt_fuel_stations (Dec 22 2016).csv")
        emergency_charge_result <- FindNearestChargeStation(NYC, df$dropoff_longitude[i-1], df$dropoff_latitude[i-1])
        travel_SOC_charge <- emergency_charge_result[1]
        travel_time_charge <- emergency_charge_result[2]
        emergency_charge_power <- emergency_charge_result[3]
        end_SOC <- range_300*charge_to
        charge_time <- round((end_SOC - max((start_SOC - travel_SOC_charge), 0))*elec_rate/emergency_charge_power*3600, 0) #secs
        end_time <- start_time + travel_time_charge + charge_time + setup_time
        df$flag_3[i] <- 0
        i <- i-1
        
      }
      
      
    } else {
      
      df$SOC_3[i] <- NA
      df$SOC_4[i] <- NA
      df$SOC_plus_3[i] <- NA
      df$flag_3[i] <- 0
      
    }
    
    i <- i+1
    
  }
  
  # write csv
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6_Review")
  write.csv(df, file = filename, row.names = F)
}

parLapply(cl, filename, f)
stopCluster(cl)


